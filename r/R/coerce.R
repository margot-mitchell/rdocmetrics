# Input coercion and the RT unit contract.
#
# 1. A value that cannot be coerced is an error, so a measure is never computed
#    from fewer trials than the caller passed.
# 2. RT units are declared, not inferred from magnitudes: every entry point that
#    takes reaction times takes rt_unit and converts to seconds once.

RT_UNITS <- c("s", "ms")

# Plausible range for the largest reaction time passed in one call, per unit.
# The declared unit is authoritative; this catches the common 1000x mistake.
# Seconds declared as ms give a maximum around 1 (below 20), and ms declared as
# seconds a maximum in the hundreds or thousands (above 60). A cell whose
# largest RT happens to fall inside the other unit's range is not caught.
RT_PLAUSIBLE_WINDOW <- list(s = c(0.02, 60), ms = c(20, 60000))

as_numeric_vector <- function(x, name) {
  if (is.null(x)) {
    return(NULL)
  }
  if (is.data.frame(x)) {
    stop("`", name, "` must be a vector, not a data frame. This package takes ",
      "column vectors.",
      call. = FALSE
    )
  }
  if (is.numeric(x) || is.logical(x)) {
    return(as.numeric(x))
  }
  chr <- trimws(as.character(x))
  blank <- is.na(chr) | !nzchar(chr) | tolower(chr) %in% missing_tokens()
  out <- rep(NA_real_, length(chr))
  converted <- suppressWarnings(as.numeric(chr[!blank]))
  if (anyNA(converted)) {
    offender <- chr[!blank][which(is.na(converted))[1L]]
    stop("`", name, "` contains a value that is not numeric: ", sQuote(offender),
      ".",
      call. = FALSE
    )
  }
  out[!blank] <- converted
  out
}

# Correctness as 0/1, accepting the spellings exports use (1/0, true/false,
# t/f, yes/no). Any other value is an error.
as_correct_vector <- function(x, name = "correct") {
  if (is.null(x)) {
    return(NULL)
  }
  if (is.logical(x)) {
    return(as.integer(x))
  }
  if (is.numeric(x)) {
    bad <- x[!is.na(x) & !x %in% c(0, 1)]
    if (length(bad) > 0L) {
      stop("`", name, "` must be 0/1 or TRUE/FALSE; found ", bad[1L], ".",
        call. = FALSE
      )
    }
    return(as.integer(x))
  }
  chr <- tolower(trimws(as.character(x)))
  out <- rep(NA_integer_, length(chr))
  out[chr %in% c("1", "true", "t", "yes")] <- 1L
  out[chr %in% c("0", "false", "f", "no")] <- 0L
  blank <- is.na(chr) | !nzchar(chr) | chr %in% missing_tokens()
  unknown <- is.na(out) & !blank
  if (any(unknown)) {
    stop("`", name, "` contains an unrecognised truth value: ",
      sQuote(chr[which(unknown)[1L]]),
      ". Expected one of 1/0, true/false, t/f, yes/no.",
      call. = FALSE
    )
  }
  out
}

# Per-trial flags such as first_in_block: TRUE/FALSE, 1/0 or the spellings
# as_correct_vector() accepts. A missing or unrecognised value is an error.
as_flag_vector <- function(x, name) {
  if (is.null(x)) {
    return(NULL)
  }
  flags <- as_correct_vector(x, name)
  if (anyNA(flags)) {
    stop("`", name, "` has a missing value at position ", which(is.na(flags))[1L],
      ". Pass TRUE or FALSE for every trial.",
      call. = FALSE
    )
  }
  flags == 1L
}

check_lengths <- function(...) {
  args <- list(...)
  args <- args[!vapply(args, is.null, logical(1))]
  if (length(args) < 2L) {
    return(invisible(TRUE))
  }
  lens <- vapply(args, length, integer(1))
  if (length(unique(lens)) > 1L) {
    stop("All trial vectors must have the same length; got ",
      paste(sprintf("%s = %d", names(args), lens), collapse = ", "), ".",
      call. = FALSE
    )
  }
  invisible(TRUE)
}

# Reaction times in seconds, from `rt_unit` ("s" or "ms"). With plausible =
# TRUE the largest value is range-checked against the declared unit; pass
# FALSE for quantities that are legitimately tiny, such as a stop-signal delay
# near zero early in a staircase.
rt_to_seconds <- function(rt, rt_unit, plausible = TRUE) {
  if (missing(rt_unit) || is.null(rt_unit)) {
    stop("`rt_unit` is required: pass \"s\" or \"ms\".",
      call. = FALSE
    )
  }
  rt_unit <- as.character(rt_unit)[1L]
  if (!rt_unit %in% RT_UNITS) {
    stop("`rt_unit` must be one of ", paste(dQuote(RT_UNITS), collapse = ", "),
      "; got ", sQuote(rt_unit), ".",
      call. = FALSE
    )
  }
  rt <- as_numeric_vector(rt, "rt")
  observed <- rt[!is.na(rt)]
  if (length(observed) > 0L && isTRUE(plausible)) {
    check_no_negative_rt(observed)
    hi <- max(observed)
    window <- RT_PLAUSIBLE_WINDOW[[rt_unit]]
    too_large <- hi > window[[2L]]
    # A lone response cannot reveal a unit mistake by itself: one anticipatory
    # press of a millisecond or two is real data -- a no-go cell can hold a
    # single commission -- and a session-wide unit error still trips this on
    # every cell with two or more responses.
    too_small <- hi < window[[1L]] && length(observed) >= 2L
    if (too_small || too_large) {
      hint <- if (too_large && identical(rt_unit, "s")) {
        "The values look like milliseconds; pass rt_unit = \"ms\"."
      } else if (too_small && identical(rt_unit, "ms")) {
        "The values look like seconds; pass rt_unit = \"s\"."
      } else {
        "Check the values upstream."
      }
      stop("`rt` has a maximum of ", signif(hi, 4), ", which is implausible for ",
        "rt_unit = ", sQuote(rt_unit), " (expected a maximum between ",
        window[[1L]], " and ", window[[2L]], "). ", hint,
        call. = FALSE
      )
    }
  }
  check_no_negative_rt(rt)
  if (identical(rt_unit, "ms")) rt / 1000 else rt
}

# A unit check on the maximum stop-signal delay. Unlike RT, SSD has no lower
# bound to check in general: a staircase legitimately sits at or near zero
# early in a run. But a delay over 60 s is milliseconds declared as seconds,
# and a non-zero maximum under 2 ms -- shorter than one display frame -- is
# seconds declared as milliseconds.
SSD_PLAUSIBLE_MAX_S <- 60
SSD_MIN_NONZERO_MAX_MS <- 2

# `ssd_s` is already in seconds, from rt_to_seconds(plausible = FALSE).
check_ssd_unit <- function(ssd_s, rt_unit) {
  rt_unit <- as.character(rt_unit)[1L]
  observed <- ssd_s[is.finite(ssd_s)]
  if (length(observed) == 0L) {
    return(invisible(ssd_s))
  }
  hi <- max(observed)
  hi_declared <- if (identical(rt_unit, "ms")) hi * 1000 else hi
  if (hi > SSD_PLAUSIBLE_MAX_S) {
    hint <- if (identical(rt_unit, "s")) {
      "The values look like milliseconds; pass rt_unit = \"ms\"."
    } else {
      "Check the values upstream."
    }
    stop("`ssd` has a maximum of ", signif(hi_declared, 4), ", which is ",
      "implausible for rt_unit = ", sQuote(rt_unit), " (a stop-signal delay is ",
      "under ", SSD_PLAUSIBLE_MAX_S, " s). ", hint,
      call. = FALSE
    )
  }
  if (identical(rt_unit, "ms") && hi > 0 && hi_declared < SSD_MIN_NONZERO_MAX_MS) {
    stop("`ssd` has a maximum of ", signif(hi_declared, 4), ", which is ",
      "implausible for rt_unit = 'ms' (a non-zero stop-signal delay is at least ",
      SSD_MIN_NONZERO_MAX_MS, " ms). The values look like seconds; pass ",
      "rt_unit = \"s\".",
      call. = FALSE
    )
  }
  invisible(ssd_s)
}

# A negative RT is not a reaction time. Some exports code a missing response
# as -1; it has to become NA before it reaches a measure.
check_no_negative_rt <- function(values) {
  negative <- which(!is.na(values) & values < 0)
  if (length(negative) > 0L) {
    stop("`rt` contains a negative value (", values[[negative[1L]]], "). ",
      "Code a trial with no response as NA.",
      call. = FALSE
    )
  }
  invisible(values)
}
