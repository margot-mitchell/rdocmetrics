# Reading one task's trial table into the columns the measures use.
#
# The input is the trial-level export written by the preprocessing pipeline
# (one *_combined.csv per task): one row per trial, RT in milliseconds, and the
# trial tags already applied as a `usable` column. Trials are never dropped
# upstream: a tagged trial stays in the table with usable = FALSE, so that it
# keeps its place in the trial sequence.

# The value a measure takes when it is undefined.
NO_VALUE <- NA_real_

# Columns every function needs.
ID_COLUMNS <- c("subject_id", "session")

prepare_trials <- function(trials, task, columns, fn) {
  if (!is.data.frame(trials)) {
    stop("`trials` must be a data frame with one row per trial.", call. = FALSE)
  }
  missing <- setdiff(c(ID_COLUMNS, columns), names(trials))
  if (length(missing) > 0L) {
    stop(fn, "(): `trials` is missing column(s) ",
      paste(sQuote(missing), collapse = ", "), ".",
      call. = FALSE
    )
  }
  if ("trial_id" %in% names(trials)) {
    trials <- trials[!is.na(trials$trial_id) & trials$trial_id == "test_trial", ,
      drop = FALSE
    ]
  }

  out <- data.frame(
    subject_id = as.character(trials$subject_id),
    session = as.character(trials$session),
    stringsAsFactors = FALSE
  )
  if (anyNA(out$subject_id) || anyNA(out$session)) {
    stop(fn, "(): `subject_id` and `session` must be set on every trial.",
      call. = FALSE
    )
  }
  if ("condition" %in% columns) {
    out$condition <- as.character(trials$condition)
    if (anyNA(out$condition)) {
      stop(fn, "(): `condition` is missing on ", sum(is.na(out$condition)),
        " trial(s).",
        call. = FALSE
      )
    }
    if (task %in% RT_TASKS) check_conditions(out$condition, task)
  }
  if ("correct_trial" %in% columns) {
    out$correct <- as_correct_vector(trials$correct_trial, "correct_trial")
  }
  if ("rt" %in% columns) {
    # Validates the unit (a maximum that looks like seconds is an error) and
    # rejects negative values; the measures then work in milliseconds.
    out$rt <- 1000 * rt_to_seconds(trials$rt, rt_unit = "ms")
  }
  if ("trial_seq" %in% columns) {
    out$trial_seq <- as_numeric_vector(trials$trial_seq, "trial_seq")
  }
  if ("block_num" %in% columns) {
    out$block_num <- as_numeric_vector(trials$block_num, "block_num")
  }

  usable <- if ("usable" %in% names(trials)) {
    as_flag_vector(trials$usable, "usable")
  } else {
    rep(TRUE, nrow(out))
  }
  if ("condition" %in% columns && task %in% RT_TASKS) {
    usable <- usable & condition_fact(out$condition, task, "placeholder") != "yes"
  }
  out$usable <- usable
  out
}

# A trial gives a response when it has an RT; an RT of 0 counts.
responded <- function(rt) {
  is.finite(rt) & rt >= 0
}

# The sequential measures find neighbours by position, so every trial of a
# session has to be there, in order: trial_seq must run 1..n with no gaps.
check_full_sequence <- function(df, fn) {
  key <- paste(df$subject_id, df$session, sep = "\r")
  ok <- tapply(df$trial_seq, key, function(s) {
    !anyNA(s) && identical(as.numeric(sort(s)), as.numeric(seq_along(s)))
  })
  if (!all(ok)) {
    bad <- strsplit(names(ok)[!ok][1L], "\r", fixed = TRUE)[[1L]]
    stop(fn, "(): trial_seq does not run 1..n in ", sum(!ok),
      " session(s), for example subject ", bad[1L], " session ", bad[2L],
      ". Pass every trial and mark the ones to leave out with usable = FALSE.",
      call. = FALSE
    )
  }
  invisible(df)
}

# Sort into session order and add the row index of each trial's previous and
# next trial in the same session and block (NA at a block edge).
with_neighbours <- function(df) {
  df <- df[order(df$subject_id, df$session, df$trial_seq), , drop = FALSE]
  n <- nrow(df)
  same <- function(a, b) {
    df$subject_id[a] == df$subject_id[b] & df$session[a] == df$session[b] &
      !is.na(df$block_num[a]) & !is.na(df$block_num[b]) &
      df$block_num[a] == df$block_num[b]
  }
  idx <- seq_len(n)
  prev <- idx - 1L
  nxt <- idx + 1L
  prev[1L] <- NA_integer_
  nxt[n] <- NA_integer_
  if (n > 1L) {
    prev[-1L][!same(idx[-1L], idx[-n])] <- NA_integer_
    nxt[-n][!same(idx[-n], idx[-1L])] <- NA_integer_
  }
  df$prev <- prev
  df$next_ <- nxt
  rownames(df) <- NULL
  df
}

# A value of the neighbour row, or `missing` where there is none.
at <- function(x, index, missing = NA) {
  out <- x[index]
  out[is.na(index)] <- missing
  out
}

# One row per subject x session, in sorted order.
sessions_of <- function(df) {
  keys <- unique(df[, ID_COLUMNS, drop = FALSE])
  keys <- keys[order(keys$subject_id, keys$session), , drop = FALSE]
  rownames(keys) <- NULL
  keys
}

session_key <- function(df) paste(df$subject_id, df$session, sep = "\r")

# NaN (a mean of nothing) reads as NA.
nan_to_na <- function(x) {
  x[is.nan(x)] <- NA_real_
  x
}
