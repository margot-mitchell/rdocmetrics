#' Stop-signal reaction time per session
#'
#' SSRT by the integration method with replacement of go omissions (Verbruggen
#' et al., 2019), one value per subject and session. A missed go trial takes
#' the session's slowest go RT. Every stop-signal test trial is used,
#' including ones tagged unusable, and no trial is dropped for a short
#' stop-signal delay.
#'
#' The computation is [calculate_ssrt()]; this function runs it per session
#' and returns milliseconds.
#'
#' @param trials Stop-signal trials, with columns `subject_id`, `session`,
#'   `condition` (`"go"` or `"stop"`), `rt` (ms) and `SSD` (ms), and
#'   optionally `trial_id` (only `"test_trial"` rows are used).
#' @return A data frame with one row per subject and session: `task`,
#'   `subject_id`, `session` and `ssrt_ms`.
#' @references Verbruggen, F., Aron, A. R., Band, G. P. H., et al. (2019). A
#'   consensus guide to capturing the ability to inhibit actions and impulsive
#'   behaviors in the stop-signal task. *eLife, 8*, e46323.
#' @export
ssrt <- function(trials) {
  missing <- setdiff(c(ID_COLUMNS, "condition", "rt", "SSD"), names(trials))
  if (!is.data.frame(trials) || length(missing) > 0L) {
    stop("ssrt(): `trials` must be a data frame with column(s) ",
      paste(sQuote(missing), collapse = ", "), ".",
      call. = FALSE
    )
  }
  ids <- data.frame(
    subject_id = as.character(trials$subject_id),
    session = as.character(trials$session),
    stringsAsFactors = FALSE
  )
  sessions <- sessions_of(ids)
  groups <- split(seq_len(nrow(trials)), session_key(ids))
  value <- vapply(seq_len(nrow(sessions)), function(i) {
    df <- trials[groups[[paste(sessions$subject_id[i], sessions$session[i],
      sep = "\r"
    )]], , drop = FALSE]
    session_ssrt_ms(df)
  }, numeric(1))
  data.frame(
    task = rep("stop_signal", nrow(sessions)),
    sessions,
    ssrt_ms = value,
    stringsAsFactors = FALSE
  )
}

session_ssrt_ms <- function(df) {
  rt <- suppressWarnings(as.numeric(df$rt))
  is_test <- if ("trial_id" %in% names(df)) {
    !is.na(df$trial_id) & df$trial_id == "test_trial"
  } else {
    rep(TRUE, nrow(df))
  }
  go_rt <- rt[is_test & df$condition == "go" & !is.na(rt)]
  if (length(go_rt) == 0L) {
    return(NA_real_)
  }
  1000 * calculate_ssrt_from_frame(df,
    rt_unit = "ms", short_ssd_limit_s = 0,
    max_go_rt_s = max(go_rt) / 1000
  )
}

#' Span recall accuracy per session
#'
#' Recall accuracy irrespective of order on the 4x4 grid, one value per
#' subject and session, for simple span and operation span. Each recall trial
#' scores the share of its target cells the participant selected, in any
#' order; a trial with no selection scores 0. The session value is the mean
#' over recall trials. The response interval is the time from the start of
#' the response period to the last selection, divided by the number of
#' selections, averaged over trials.
#'
#' The per-trial scoring is [span_accuracy()] and [span_response_interval()].
#'
#' @param trials One span task's trials, with columns `subject_id`,
#'   `session`, `trial_id`, `response` and `spatial_sequence`, optionally
#'   `rt_each_spatial_location_response_grid` (cumulative ms), and for
#'   operation span `condition`. Recall trials are the `"test_trial"` rows,
#'   and in operation span those with condition `"span"`.
#' @param task `"simple_span"` or `"operation_span"`.
#' @return A data frame with one row per subject and session: `task`,
#'   `subject_id`, `session`, `n_trials`, `accuracy`, `err_rate` and
#'   `response_interval_ms` (`NA` without the response-time column).
#' @export
span_metrics <- function(trials, task) {
  check_task(task, SPAN_TASKS, "span_metrics")
  needed <- c(ID_COLUMNS, "trial_id", "response", "spatial_sequence",
    if (task == "operation_span") "condition")
  missing <- setdiff(needed, names(trials))
  if (!is.data.frame(trials) || length(missing) > 0L) {
    stop("span_metrics(): `trials` must be a data frame with column(s) ",
      paste(sQuote(missing), collapse = ", "), ".",
      call. = FALSE
    )
  }
  recall <- !is.na(trials$trial_id) & trials$trial_id == "test_trial"
  if (task == "operation_span") {
    recall <- recall & !is.na(trials$condition) & trials$condition == "span"
  }
  trials <- trials[recall, , drop = FALSE]
  ids <- data.frame(
    subject_id = as.character(trials$subject_id),
    session = as.character(trials$session),
    stringsAsFactors = FALSE
  )
  sessions <- sessions_of(ids)
  groups <- split(seq_len(nrow(trials)), session_key(ids))
  has_rt <- "rt_each_spatial_location_response_grid" %in% names(trials)
  rows <- lapply(seq_len(nrow(sessions)), function(i) {
    df <- trials[groups[[paste(sessions$subject_id[i], sessions$session[i],
      sep = "\r"
    )]], , drop = FALSE]
    acc <- span_accuracy(as.character(df$response), as.character(df$spatial_sequence),
      order = "irrespective"
    )
    interval <- if (has_rt) {
      1000 * span_response_interval(
        as.character(df$rt_each_spatial_location_response_grid),
        rt_unit = "ms"
      )
    } else {
      NA_real_
    }
    data.frame(
      n_trials = nrow(df),
      accuracy = acc,
      err_rate = if (is.finite(acc)) 1 - acc else NA_real_,
      response_interval_ms = interval
    )
  })
  out <- do.call(rbind, rows)
  if (is.null(out)) {
    out <- data.frame(n_trials = integer(), accuracy = numeric(),
      err_rate = numeric(), response_interval_ms = numeric())
  }
  data.frame(task = rep(task, nrow(sessions)), sessions, out,
    stringsAsFactors = FALSE
  )
}

#' Operation span minus simple span
#'
#' The difference in recall error rate between operation span and simple span,
#' per subject and session: how much the processing task costs recall.
#' `NA` in a session that lacks either task.
#'
#' @param operation,simple The output of [span_metrics()] for operation span
#'   and for simple span.
#' @return A data frame with columns `subject_id`, `session` and `err_rate`
#'   (operation minus simple).
#' @export
span_contrast <- function(operation, simple) {
  keep <- c(ID_COLUMNS, "err_rate")
  both <- merge(operation[, keep], simple[, keep],
    by = ID_COLUMNS, all = TRUE,
    suffixes = c("_operation", "_simple")
  )
  out <- data.frame(
    subject_id = both$subject_id,
    session = both$session,
    err_rate = both$err_rate_operation - both$err_rate_simple,
    stringsAsFactors = FALSE
  )
  out <- out[order(out$subject_id, out$session), , drop = FALSE]
  rownames(out) <- NULL
  out
}

#' Subject-level means across sessions
#'
#' Averages any of this package's per-session tables over each subject's
#' sessions, the way the study's reported subject values are formed: the mean
#' of the session values, `NA` for a subject with fewer than `min_sessions`
#' sessions of the task. A session counts for a task when any measure in
#' `x` has a value in it for that subject. Count columns (names starting
#' `n_`) are summed rather than averaged.
#'
#' The study's pipeline applies the 3-session minimum to every measure except
#' post-error slowing; pass `min_sessions = 1` for that table to match it.
#'
#' @param x The output of [condition_metrics()], [condition_contrasts()],
#'   [post_error_slowing()], [congruency_sequence_effect()],
#'   [post_stop_slowing()], [ssrt()], [span_metrics()] or [span_contrast()].
#' @param min_sessions Fewest sessions for a subject value.
#' @return One row per subject (and per task, condition or contrast where `x`
#'   has them), with `n_sessions` and the averaged measures.
#' @export
subject_means <- function(x, min_sessions = 3L) {
  if (!is.data.frame(x) || !all(ID_COLUMNS %in% names(x))) {
    stop("`x` must be one of this package's per-session tables.", call. = FALSE)
  }
  x$subject_id <- as.character(x$subject_id)
  x$session <- as.character(x$session)
  numeric_cols <- names(x)[vapply(x, is.numeric, logical(1))]
  counts <- numeric_cols[startsWith(numeric_cols, "n_")]
  measures <- setdiff(numeric_cols, counts)
  keys <- setdiff(names(x), c(numeric_cols, "session"))
  task_cols <- intersect(c("task", "subject_id"), names(x))

  has_value <- if (length(measures) > 0L) {
    rowSums(!is.na(x[, measures, drop = FALSE])) > 0L
  } else {
    rep(FALSE, nrow(x))
  }
  task_key <- do.call(paste, c(x[task_cols], sep = "\r"))
  n_sessions <- tapply(x$session[has_value], task_key[has_value], function(s) {
    length(unique(s))
  })

  group <- do.call(paste, c(x[keys], sep = "\r"))
  first <- !duplicated(group)
  out <- x[first, keys, drop = FALSE]
  idx <- split(seq_len(nrow(x)), factor(group, levels = group[first]))
  n <- as.integer(n_sessions[task_key[first]])
  n[is.na(n)] <- 0L
  out$n_sessions <- n
  for (col in counts) {
    out[[col]] <- vapply(idx, function(i) sum(x[[col]][i], na.rm = TRUE), numeric(1))
  }
  for (col in measures) {
    value <- vapply(idx, function(i) mean(x[[col]][i], na.rm = TRUE), numeric(1))
    value <- nan_to_na(value)
    value[n < min_sessions] <- NA_real_
    out[[col]] <- value
  }
  rownames(out) <- NULL
  out
}
