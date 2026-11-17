#' Accuracy, reaction time and omissions per condition
#'
#' One row per subject, session and condition for one task, from that task's
#' trial table. Only usable trials contribute; placeholder conditions (the
#' opening trials of a block in n-back and the task-switching tasks) are left
#' out. Pooled cells are added where the study reports them: n-back `1_back`
#' and `2_back`, and visual search `feature`.
#'
#' @section Definitions:
#' - A **response** is a trial with an RT; an RT of 0 counts.
#' - **Accuracy** is correct responses over responses, so an omission is
#'   neither right nor wrong. In go/no-go (both conditions) and on stop-signal
#'   stop trials it is correct trials over all trials instead, because there a
#'   withheld response is the outcome being scored: a missed go trial is an
#'   error, a withheld no-go or stop trial is correct.
#' - **`err_rate`** is `1 - accuracy`.
#' - **`rt_ms`** is the mean RT of correct responses, and `rt_var_ms2` their
#'   variance. On no-go trials it is the mean RT of all responses (commission
#'   errors).
#' - **`omission_rate`** is the share of usable trials with no response.
#'
#' @param trials One task's trials: a data frame with columns `subject_id`,
#'   `session`, `condition`, `correct_trial` and `rt` (milliseconds), and
#'   optionally `usable` (all trials are usable when it is absent) and
#'   `trial_id` (only `"test_trial"` rows are used).
#' @param task The task, one of `"ax_cpt"`, `"cued_task_switching"`,
#'   `"flanker"`, `"go_nogo"`, `"n_back"`, `"spatial_cueing"`,
#'   `"spatial_task_switching"`, `"stop_signal"`, `"stroop"`,
#'   `"visual_search"`.
#' @return A data frame with columns `task`, `subject_id`, `session`,
#'   `condition`, `n_trials`, `n_usable`, `n_responses`, `accuracy`,
#'   `err_rate`, `rt_ms`, `rt_var_ms2` and `omission_rate`.
#' @examples
#' trials <- read.csv(system.file("examples", "flanker_trials.csv",
#'   package = "rdocmetrics"
#' ))
#' condition_metrics(trials, "flanker")
#' @export
condition_metrics <- function(trials, task) {
  check_task(task, RT_TASKS, "condition_metrics")
  df <- prepare_trials(trials, task, c("condition", "correct_trial", "rt"),
    "condition_metrics"
  )
  df <- df[condition_fact(df$condition, task, "placeholder") != "yes", , drop = FALSE]

  conds <- task_conditions(task)
  pooled <- conds[nzchar(conds$pooled_from), , drop = FALSE]
  for (i in seq_len(nrow(pooled))) {
    sources <- strsplit(pooled$pooled_from[i], ";", fixed = TRUE)[[1L]]
    copy <- df[df$condition %in% sources, , drop = FALSE]
    copy$condition <- rep(pooled$condition[i], nrow(copy))
    df <- rbind(df, copy)
  }

  cells <- split(seq_len(nrow(df)), paste(df$subject_id, df$session, df$condition,
    sep = "\r"
  ), drop = TRUE)
  rows <- lapply(cells, function(i) cell_metrics(df[i, , drop = FALSE], task))
  out <- do.call(rbind, rows)
  if (is.null(out)) out <- empty_condition_metrics()
  out <- cbind(task = rep(task, nrow(out)), out, stringsAsFactors = FALSE)
  order_rows(out, conds$condition)
}

cell_metrics <- function(cell, task) {
  condition <- cell$condition[1L]
  use <- cell[cell$usable, , drop = FALSE]
  resp <- responded(use$rt)
  correct <- !is.na(use$correct) & use$correct == 1L

  accuracy <- if (identical(condition_fact(condition, task, "accuracy_over"), "all_trials")) {
    nan_to_na(mean(use$correct == 1L, na.rm = TRUE))
  } else if (sum(resp) > 0L) {
    sum(correct & resp) / sum(resp)
  } else {
    NA_real_
  }

  rt_trials <- if (identical(condition_fact(condition, task, "rt_from"), "all_responses")) {
    resp
  } else {
    resp & correct
  }
  x <- use$rt[rt_trials]

  data.frame(
    subject_id = cell$subject_id[1L],
    session = cell$session[1L],
    condition = condition,
    n_trials = nrow(cell),
    n_usable = nrow(use),
    n_responses = sum(resp),
    accuracy = accuracy,
    err_rate = 1 - accuracy,
    rt_ms = if (length(x) > 0L) mean(x) else NA_real_,
    rt_var_ms2 = if (length(x) >= 2L) stats::var(x) else NA_real_,
    omission_rate = if (nrow(use) > 0L) mean(!resp) else NA_real_,
    stringsAsFactors = FALSE
  )
}

empty_condition_metrics <- function() {
  data.frame(
    subject_id = character(), session = character(), condition = character(),
    n_trials = integer(), n_usable = integer(), n_responses = integer(),
    accuracy = numeric(), err_rate = numeric(), rt_ms = numeric(),
    rt_var_ms2 = numeric(), omission_rate = numeric(),
    stringsAsFactors = FALSE
  )
}

# Subjects and sessions in sorted order; conditions in table order.
order_rows <- function(out, levels) {
  key <- if ("condition" %in% names(out)) match(out$condition, levels) else 0L
  out <- out[order(out$subject_id, out$session, key), , drop = FALSE]
  rownames(out) <- NULL
  out
}

#' Contrasts between conditions
#'
#' The study's contrast scores, each one condition minus another (harder
#' minus easier, so a positive value means worse performance), computed within
#' each session from [condition_metrics()]:
#'
#' | Task | Contrast | Left − right |
#' |---|---|---|
#' | AX-CPT | `AY_BY`, `BX_BY` | AY − BY, BX − BY |
#' | Flanker, Stroop | `incongruent_congruent` | incongruent − congruent |
#' | N-back | `2back_1back` | 2-back − 1-back (pooled cells) |
#' | Visual search | `conjunction_24_conjunction_8`, `conjunction_24_feature_24` | |
#' | Cued and spatial task switching | `task_switch_cost` | task switch − task stay (cue switch) |
#' | | `cue_switch_cost` | cue switch − cue stay (task stay) |
#' | Spatial cueing | `invalid_doublecue`, `doublecue_valid`, `nocue_doublecue` | |
#'
#' A contrast is `NA` in a session where either condition has no value.
#'
#' @param metrics The output of [condition_metrics()], for one task or several.
#' @return A data frame with columns `task`, `subject_id`, `session`,
#'   `contrast`, `left`, `right`, `rt_ms` and `err_rate`: one row per
#'   subject, session and contrast of each task in `metrics`.
#' @examples
#' trials <- read.csv(system.file("examples", "flanker_trials.csv",
#'   package = "rdocmetrics"
#' ))
#' condition_contrasts(condition_metrics(trials, "flanker"))
#' @export
condition_contrasts <- function(metrics) {
  needed <- c("task", "subject_id", "session", "condition", "rt_ms", "err_rate")
  missing <- setdiff(needed, names(metrics))
  if (!is.data.frame(metrics) || length(missing) > 0L) {
    stop("`metrics` must be the output of condition_metrics(); missing column(s) ",
      paste(sQuote(missing), collapse = ", "), ".",
      call. = FALSE
    )
  }
  specs <- read_task_table("contrasts")
  specs <- specs[specs$task %in% unique(metrics$task), , drop = FALSE]
  key <- paste(metrics$task, metrics$subject_id, metrics$session, metrics$condition,
    sep = "\r"
  )
  rows <- lapply(seq_len(nrow(specs)), function(i) {
    s <- specs[i, ]
    m <- metrics[metrics$task == s$task, , drop = FALSE]
    sessions <- sessions_of(m)
    lookup <- function(condition, column) {
      metrics[[column]][match(paste(s$task, sessions$subject_id, sessions$session,
        condition,
        sep = "\r"
      ), key)]
    }
    data.frame(
      task = rep(s$task, nrow(sessions)),
      subject_id = sessions$subject_id,
      session = sessions$session,
      contrast = rep(s$contrast, nrow(sessions)),
      left = rep(s$left, nrow(sessions)),
      right = rep(s$right, nrow(sessions)),
      rt_ms = lookup(s$left, "rt_ms") - lookup(s$right, "rt_ms"),
      err_rate = lookup(s$left, "err_rate") - lookup(s$right, "err_rate"),
      stringsAsFactors = FALSE
    )
  })
  out <- do.call(rbind, rows)
  if (is.null(out)) {
    return(data.frame(
      task = character(), subject_id = character(), session = character(),
      contrast = character(), left = character(), right = character(),
      rt_ms = numeric(), err_rate = numeric(), stringsAsFactors = FALSE
    ))
  }
  out <- out[order(out$task, out$subject_id, out$session,
    match(out$contrast, specs$contrast)), , drop = FALSE]
  rownames(out) <- NULL
  out
}
