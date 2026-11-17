# Stop-signal reaction time, integration (race-model) method.
# Verbruggen et al. (2019), https://doi.org/10.7554/eLife.46323
#
# This is a port of an earlier Python reference implementation. The golden
# fixtures in fixtures/ssrt/ were produced by that reference and are not
# regenerated from this code. They were produced with a 50 ms SSD floor applied
# to go trials too and omitted go RTs set to 1.0 s, so the tests pass those
# settings explicitly. The defaults here apply no SSD floor and set an omitted
# go RT to the slowest go response in the run.

KNOWN_SSRT_CONDITIONS <- c("go", "stop")

#' Stop-signal reaction time (integration method)
#'
#' @param condition Condition label per trial; only \code{"go"} and
#'   \code{"stop"} are recognised.
#' @param rt Reaction times.
#' @param ssd Stop-signal delays, on the same scale as \code{rt}.
#' @param rt_unit \code{"s"} or \code{"ms"}. Required.
#' @param trial_id Optional trial labels. When supplied, only
#'   \code{"test_trial"} rows are used, falling back to \code{"test_probe"}.
#'   Labels are matched exactly: one that differs from these only in case or
#'   surrounding spaces, such as \code{"Test_trial"}, is an error.
#' @param short_ssd_limit_s \code{NULL} (the default) applies no SSD floor. A
#'   number of seconds drops every trial whose SSD is below it -- go trials
#'   included, because a go trial carries the current staircase SSD, which
#'   removes those go RTs from the sorted distribution and shifts the order
#'   statistic.
#' @param max_go_rt_s Value substituted for omitted go RTs before sorting.
#'   \code{"session_max"} (the default) uses the slowest go response among the
#'   test trials passed, before any SSD floor, and returns \code{NA} if no go
#'   trial drew a response. A number of seconds substitutes that value, for
#'   example the task's go deadline.
#' @param drop_na_ssd If \code{FALSE} (default) trials with a missing SSD are
#'   kept. \code{TRUE} drops them. Only matters when an SSD floor is set.
#'
#' @return SSRT for one session \strong{in seconds}, or \code{NA_real_} when it
#'   is undefined: no go trials, no stop trials, no responses on any stop trial,
#'   or no usable rows.
#'
#' @details
#' Kept for compatibility with callers of earlier versions, so it returns
#' seconds. For one value per session in ms, use [ssrt()].
#'
#' Grouping by subject and session is the caller's responsibility.
#'
#' A condition label other than \code{"go"} or \code{"stop"} is an error.
#'
#' SSD and RT share \code{rt_unit}. An SSD maximum under 1\% of the largest RT,
#' or more than 10 times it, is an error, as is an SSD over 60 s.
#'
#' @examples
#' calculate_ssrt(
#'   condition = c(rep("go", 6), rep("stop", 4)),
#'   rt = c(400, 450, 500, 550, 600, NA, 420, NA, 480, NA),
#'   ssd = c(rep(200, 6), 200, 250, 150, 300),
#'   rt_unit = "ms"
#' )
#' @export
calculate_ssrt <- function(condition, rt, ssd, rt_unit, trial_id = NULL,
                           short_ssd_limit_s = NULL,
                           max_go_rt_s = "session_max",
                           drop_na_ssd = FALSE) {
  short_ssd_limit_s <- check_ssd_limit(short_ssd_limit_s)
  go_rt_fill <- check_max_go_rt(max_go_rt_s)
  labels <- as.character(condition)
  rt_s <- rt_to_seconds(rt, rt_unit)
  # SSD has no lower plausibility bound like RT's: a staircase legitimately
  # sits near zero early in a run, which is what the all_ssd_below_limit
  # fixture covers. Its scale is checked against the RTs below, then on its own.
  ssd_s <- rt_to_seconds(ssd, rt_unit, plausible = FALSE)
  check_lengths(condition = labels, rt = rt_s, ssd = ssd_s, trial_id = trial_id)
  if (length(labels) == 0L) {
    return(NA_real_)
  }
  check_ssd_scale(ssd_s, rt_s)
  check_ssd_unit(ssd_s, rt_unit)

  if (!is.null(trial_id)) {
    keep <- test_trial_mask(trial_id)
    if (is.null(keep)) {
      return(NA_real_)
    }
    labels <- labels[keep]
    rt_s <- rt_s[keep]
    ssd_s <- ssd_s[keep]
  }

  assert_known_conditions(labels)

  # The slowest go response in the run, taken before any SSD floor.
  if (is.na(go_rt_fill)) {
    responded_go <- rt_s[!is.na(labels) & labels == "go" & is.finite(rt_s)]
    if (length(responded_go) == 0L) {
      return(NA_real_)
    }
    go_rt_fill <- max(responded_go)
  }

  if (!is.null(short_ssd_limit_s)) {
    keep <- if (isTRUE(drop_na_ssd)) {
      !is.na(ssd_s) & ssd_s >= short_ssd_limit_s
    } else {
      is.na(ssd_s) | ssd_s >= short_ssd_limit_s
    }
    labels <- labels[keep]
    rt_s <- rt_s[keep]
    ssd_s <- ssd_s[keep]
  }

  is_go <- !is.na(labels) & labels == "go"
  is_stop <- !is.na(labels) & labels == "stop"
  if (!any(is_go) || !any(is_stop)) {
    return(NA_real_)
  }

  go_rts <- rt_s[is_go]
  go_rts[is.na(go_rts)] <- go_rt_fill

  stop_rt <- rt_s[is_stop]
  p_respond <- mean(!is.na(stop_rt))
  if (!isTRUE(p_respond > 0)) {
    return(NA_real_)
  }

  stop_ssd <- ssd_s[is_stop]
  if (!any(!is.na(stop_ssd))) {
    return(NA_real_)
  }
  avg_ssd <- mean(stop_ssd, na.rm = TRUE)
  if (!is.finite(avg_ssd)) {
    return(NA_real_)
  }

  # round() and numpy's rint() are both round-half-to-even, so the rank matches
  # the reference implementation exactly, including at .5 boundaries.
  nth_index <- as.integer(round(p_respond * length(go_rts))) - 1L
  nth_index <- max(0L, min(nth_index, length(go_rts) - 1L))
  nth_rt <- sort(go_rts)[[nth_index + 1L]]

  as.numeric(nth_rt - avg_ssd)
}

#' Stop-signal reaction time from a data frame
#'
#' A convenience wrapper for callers holding one session's trials as a frame.
#' The vector form is the primary API, because it is independent of any data
#' frame library and so cannot be perturbed by one.
#'
#' @param df One session's trials.
#' @param rt_unit \code{"s"} or \code{"ms"}. Required.
#' @param cols Named character vector mapping the inputs onto column names in
#'   \code{df}. Names left out keep their defaults: \code{condition},
#'   \code{rt}, \code{SSD} and \code{trial_id}.
#' @param ... Passed to \code{calculate_ssrt()}.
#' @return SSRT in seconds, or \code{NA_real_} when it is undefined. A frame
#'   missing a required column is an error.
#' @export
calculate_ssrt_from_frame <- function(df, rt_unit,
                                      cols = c(
                                        condition = "condition", rt = "rt",
                                        ssd = "SSD", trial_id = "trial_id"
                                      ),
                                      ...) {
  if (!is.data.frame(df)) {
    stop("`df` must be a data frame, not ", class(df)[1], ".", call. = FALSE)
  }
  # A partial mapping keeps the defaults for the names it leaves out.
  defaults <- c(condition = "condition", rt = "rt", ssd = "SSD", trial_id = "trial_id")
  cols <- c(cols, defaults[setdiff(names(defaults), names(cols))])
  if (nrow(df) == 0L) {
    return(NA_real_)
  }
  required <- c("condition", "rt", "ssd")
  missing_cols <- required[!cols[required] %in% names(df)]
  if (length(missing_cols) > 0L) {
    absent <- unname(cols[missing_cols])
    near <- names(df)[tolower(names(df)) %in% tolower(absent)]
    hint <- if (length(near) > 0L) {
      paste0(" Found ", paste(sQuote(near), collapse = ", "),
        "; map it with `cols`, e.g. cols = c(ssd = ", dQuote(near[1L], FALSE), ").")
    } else {
      ""
    }
    stop("`df` has no column ", paste(sQuote(absent), collapse = " or "), ".", hint,
      call. = FALSE
    )
  }
  trial_id <- if ("trial_id" %in% names(cols) && cols[["trial_id"]] %in% names(df)) {
    df[[cols[["trial_id"]]]]
  } else {
    NULL
  }
  calculate_ssrt(
    condition = df[[cols[["condition"]]]],
    rt = df[[cols[["rt"]]]],
    ssd = df[[cols[["ssd"]]]],
    rt_unit = rt_unit,
    trial_id = trial_id,
    ...
  )
}

# SSD and RT share rt_unit, so a mismatch shows in how their maxima compare.
# Over a run the staircase SSD reaches a sizeable fraction of the go RT, so an
# SSD maximum under 1% of the RT maximum means the two are on different scales.
# The staircase can climb past a fast responder's go RTs, but not tenfold: the
# stop signal would then arrive long after every response, and the staircase
# would come back down. Both maxima are in seconds.
check_ssd_scale <- function(ssd_s, rt_s) {
  max_ssd <- suppressWarnings(max(ssd_s, na.rm = TRUE))
  responded <- rt_s[is.finite(rt_s)]
  max_rt <- suppressWarnings(max(responded))
  if (!is.finite(max_ssd) || !is.finite(max_rt) || max_ssd <= 0) {
    return(invisible(TRUE))
  }
  if (max_ssd < 0.01 * max_rt) {
    stop("`ssd` has a maximum of ", signif(max_ssd, 3), " s, under 1% of the ",
      "largest RT (", signif(max_rt, 3), " s). SSD and RT must be in the same ",
      "unit, `rt_unit`.",
      call. = FALSE
    )
  }
  # As in rt_to_seconds(), a lone response is not enough to judge the scale.
  if (length(responded) >= 2L && max_ssd > 10 * max_rt) {
    stop("`ssd` has a maximum of ", signif(max_ssd, 3), " s, more than 10 ",
      "times the largest RT (", signif(max_rt, 3), " s). SSD and RT must be in ",
      "the same unit, `rt_unit`.",
      call. = FALSE
    )
  }
  invisible(TRUE)
}

# Mirror the reference's trial selection: prefer test_trial, fall back to
# test_probe, and give up when a trial_id column exists with neither label.
# Labels are matched exactly, so one that differs from these only in case or
# surrounding spaces is an error rather than a row left out.
# Returns NULL to mean "no usable rows".
test_trial_mask <- function(trial_id) {
  labels <- as.character(trial_id)
  wanted_labels <- c("test_trial", "test_probe")
  near <- !is.na(labels) & !labels %in% wanted_labels &
    tolower(trimws(labels)) %in% wanted_labels
  if (any(near)) {
    stop("`trial_id` has the label ", sQuote(labels[near][1L]), ", which ",
      "differs from ", dQuote(tolower(trimws(labels[near][1L])), FALSE),
      " only in case or spacing. Trial labels are matched exactly; relabel ",
      "upstream.",
      call. = FALSE
    )
  }
  for (wanted in wanted_labels) {
    if (any(labels == wanted, na.rm = TRUE)) {
      return(!is.na(labels) & labels == wanted)
    }
  }
  NULL
}

check_ssd_limit <- function(short_ssd_limit_s) {
  if (is.null(short_ssd_limit_s)) {
    return(NULL)
  }
  value <- suppressWarnings(as.numeric(short_ssd_limit_s))
  if (length(value) != 1L || is.na(value) || value < 0) {
    stop("`short_ssd_limit_s` must be NULL (no SSD floor) or a single ",
      "non-negative number of seconds.",
      call. = FALSE
    )
  }
  value
}

# The go-RT substitute in seconds, or NA to mean "the run's slowest go
# response", which can only be known once the trials are selected.
check_max_go_rt <- function(max_go_rt_s) {
  if (identical(max_go_rt_s, "session_max")) {
    return(NA_real_)
  }
  value <- suppressWarnings(as.numeric(max_go_rt_s))
  if (length(value) != 1L || is.na(value) || value <= 0) {
    stop("`max_go_rt_s` must be \"session_max\" or a single positive number ",
      "of seconds.",
      call. = FALSE
    )
  }
  value
}

assert_known_conditions <- function(condition) {
  blank <- which(is.na(condition) | !nzchar(trimws(condition)))
  if (length(blank) > 0L) {
    stop("Stop-signal `condition` has a missing label at position ", blank[1L],
      ". Label every trial \"go\" or \"stop\", or leave the trial out.",
      call. = FALSE
    )
  }
  observed <- unique(condition)
  unknown <- setdiff(observed, KNOWN_SSRT_CONDITIONS)
  if (length(unknown) > 0L) {
    stop(
      "Unrecognised stop-signal `condition` value(s): ",
      paste(sQuote(unknown), collapse = ", "),
      ". Expected only 'go' and 'stop'. Filter or relabel upstream.",
      call. = FALSE
    )
  }
  invisible(TRUE)
}
