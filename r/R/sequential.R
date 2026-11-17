# Post-error slowing, the congruency sequence effect and post-stop slowing.
#
# All three look at a trial's neighbours: the trial just before and just after
# it in the same session and block, in trial_seq order. A trial that is not
# usable keeps its place, so it can't be a neighbour and the trials on either
# side of it are not adjacent. Nothing spans a block break.

SEQUENCE_COLUMNS <- c("condition", "correct_trial", "rt", "trial_seq", "block_num")

sequence_trials <- function(trials, task, fn) {
  df <- prepare_trials(trials, task, SEQUENCE_COLUMNS, fn)
  check_full_sequence(df, fn)
  df <- with_neighbours(df)
  df$responded <- responded(df$rt)
  df$correct_response <- df$responded & !is.na(df$correct) & df$correct == 1L
  df
}

per_session <- function(df, fun) {
  sessions <- sessions_of(df)
  groups <- split(seq_len(nrow(df)), session_key(df))
  rows <- lapply(seq_len(nrow(sessions)), function(i) {
    idx <- groups[[paste(sessions$subject_id[i], sessions$session[i], sep = "\r")]]
    cbind(sessions[i, , drop = FALSE], fun(idx), stringsAsFactors = FALSE)
  })
  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}

#' Post-error slowing
#'
#' Post-error slowing on correct-error-correct triplets (Dutilh et al., 2012),
#' one value per subject and session.
#'
#' An **error** is a usable incorrect trial with a response: a choice error, a
#' no-go response or a failed stop. A missed go trial is not one. An error
#' counts when the trials just before and after it, in its block, are usable;
#' in go/no-go and stop signal they must also be go trials. It forms a
#' **triplet** when both of those trials are correct responses. Post-error
#' slowing is the mean over triplets of the post-error minus the pre-error RT,
#' and is `NA` with fewer than `min_triplets` triplets.
#'
#' @inheritParams condition_metrics
#' @param trials One task's trials, with the columns [condition_metrics()]
#'   needs plus `trial_seq` (the trial's position in its session, 1 to n) and
#'   `block_num`. Pass every trial: mark the ones to leave out with
#'   `usable = FALSE` instead of dropping them.
#' @param min_triplets Fewest triplets for a value.
#' @return A data frame with one row per subject and session: `task`,
#'   `subject_id`, `session`, `n_errors` (counted errors), `n_triplets`,
#'   `pre_rt_ms`, `post_rt_ms` and `pes_ms`.
#' @references Dutilh, G., van Ravenzwaaij, D., Nieuwenhuis, S., van der Maas,
#'   H. L. J., Forstmann, B. U., & Wagenmakers, E.-J. (2012). How to measure
#'   post-error slowing: A confound and a simple solution. *Journal of
#'   Mathematical Psychology, 56*(3), 208–216.
#' @export
post_error_slowing <- function(trials, task, min_triplets = 2L) {
  check_task(task, RT_TASKS, "post_error_slowing")
  df <- sequence_trials(trials, task, "post_error_slowing")

  go_only <- task %in% c("go_nogo", "stop_signal")
  comparable <- df$usable & (!go_only | df$condition == "go")
  error <- df$usable & !is.na(df$correct) & df$correct == 0L & df$responded
  counted <- error & at(comparable, df$prev, FALSE) & at(comparable, df$next_, FALSE)
  triplet <- counted & at(df$correct_response, df$prev, FALSE) &
    at(df$correct_response, df$next_, FALSE)
  pre_rt <- at(df$rt, df$prev)
  post_rt <- at(df$rt, df$next_)

  out <- per_session(df, function(i) {
    t <- i[triplet[i]]
    n_triplets <- length(t)
    pre <- if (n_triplets > 0L) mean(pre_rt[t]) else NA_real_
    post <- if (n_triplets > 0L) mean(post_rt[t]) else NA_real_
    data.frame(
      n_errors = sum(counted[i]),
      n_triplets = n_triplets,
      pre_rt_ms = pre,
      post_rt_ms = post,
      pes_ms = if (n_triplets >= min_triplets) post - pre else NA_real_
    )
  })
  cbind(task = rep(task, nrow(out)), out, stringsAsFactors = FALSE)
}

#' Congruency sequence effect
#'
#' The congruency sequence effect on RT (Gratton et al., 1992; Egner, 2007),
#' one value per subject and session, for flanker and Stroop.
#'
#' A trial enters one of four cells by its own congruency and its previous
#' trial's: congruent after incongruent (cI), congruent after congruent (cC),
#' incongruent after incongruent (iI), incongruent after congruent (iC). It
#' needs to be usable, and its previous trial in the block usable and correct
#' (Braem et al., 2019). Each cell's value is the mean RT of its correct
#' responses, and the effect is `(cI - cC) - (iI - iC)`, `NA` when a cell is
#' empty.
#'
#' @inheritParams post_error_slowing
#' @param task `"flanker"` or `"stroop"`.
#' @return A data frame with one row per subject and session: `task`,
#'   `subject_id`, `session`, the four cell means `cI_ms`, `cC_ms`, `iI_ms`,
#'   `iC_ms`, their counts `n_cI`, `n_cC`, `n_iI`, `n_iC`, and `cse_ms`.
#' @references Braem, S., Bugg, J. M., Schmidt, J. R., Crump, M. J. C.,
#'   Weissman, D. H., Notebaert, W., & Egner, T. (2019). Measuring adaptive
#'   control in conflict tasks. *Trends in Cognitive Sciences, 23*(9),
#'   769–783.
#'
#'   Egner, T. (2007). Congruency sequence effects and cognitive control.
#'   *Cognitive, Affective, & Behavioral Neuroscience, 7*(4), 380–390.
#'
#'   Gratton, G., Coles, M. G. H., & Donchin, E. (1992). Optimizing the use of
#'   information: Strategic control of activation of responses. *Journal of
#'   Experimental Psychology: General, 121*(4), 480–506.
#' @export
congruency_sequence_effect <- function(trials, task) {
  check_task(task, c("flanker", "stroop"), "congruency_sequence_effect")
  df <- sequence_trials(trials, task, "congruency_sequence_effect")

  prev_condition <- at(df$condition, df$prev)
  prev_ok <- at(df$usable, df$prev, FALSE) &
    at(!is.na(df$correct) & df$correct == 1L, df$prev, FALSE)
  eligible <- df$usable & prev_ok & !is.na(prev_condition)
  cell <- rep(NA_character_, nrow(df))
  cell[eligible & df$condition == "congruent" & prev_condition == "incongruent"] <- "cI"
  cell[eligible & df$condition == "congruent" & prev_condition == "congruent"] <- "cC"
  cell[eligible & df$condition == "incongruent" & prev_condition == "incongruent"] <- "iI"
  cell[eligible & df$condition == "incongruent" & prev_condition == "congruent"] <- "iC"
  scored <- !is.na(cell) & df$correct_response

  out <- per_session(df, function(i) {
    m <- vapply(c("cI", "cC", "iI", "iC"), function(k) {
      j <- i[scored[i] & cell[i] == k]
      if (length(j) > 0L) mean(df$rt[j]) else NA_real_
    }, numeric(1))
    n <- vapply(c("cI", "cC", "iI", "iC"), function(k) sum(scored[i] & cell[i] == k), integer(1))
    data.frame(
      cI_ms = m[["cI"]], cC_ms = m[["cC"]], iI_ms = m[["iI"]], iC_ms = m[["iC"]],
      n_cI = n[["cI"]], n_cC = n[["cC"]], n_iI = n[["iI"]], n_iC = n[["iC"]],
      cse_ms = (m[["cI"]] - m[["cC"]]) - (m[["iI"]] - m[["iC"]])
    )
  })
  cbind(task = rep(task, nrow(out)), out, stringsAsFactors = FALSE)
}

#' Post-stop slowing
#'
#' Post-stop slowing in the stop-signal task (Bissett & Logan, 2011), one
#' value per subject and session.
#'
#' Every usable stop trial, successful or failed, can count. It counts when the
#' trial just before it is a usable go trial whose own previous trial, if it
#' has one in the block, is a correct go response, and the trial just after it
#' is a usable go trial. Those go trials form the pre-stop and post-stop cells.
#' Post-stop slowing is the mean RT of the post-stop cell's correct responses
#' minus the pre-stop cell's, `NA` unless each cell has at least `min_correct`
#' correct responses.
#'
#' @param trials Stop-signal trials, with the columns [post_error_slowing()]
#'   needs.
#' @param min_correct Fewest correct responses in each cell for a value.
#' @return A data frame with one row per subject and session: `task`,
#'   `subject_id`, `session`, `n_stops` (counted stop trials), `n_pre` and
#'   `n_post` (correct responses in each cell), `pre_rt_ms`, `post_rt_ms` and
#'   `pss_ms`.
#' @references Bissett, P. G., & Logan, G. D. (2011). Balancing cognitive
#'   demands: Control adjustments in the stop-signal paradigm. *Journal of
#'   Experimental Psychology: Learning, Memory, and Cognition, 37*(2),
#'   392–404.
#' @export
post_stop_slowing <- function(trials, min_correct = 4L) {
  task <- "stop_signal"
  df <- sequence_trials(trials, task, "post_stop_slowing")

  stop_trial <- df$usable & df$condition == "stop"
  go <- df$usable & df$condition == "go"
  correct_go <- go & df$correct_response
  pre_ok <- go & at(correct_go, df$prev, TRUE)
  counted <- stop_trial & at(pre_ok, df$prev, FALSE) & at(go, df$next_, FALSE)
  pre_cell <- at(counted, df$next_, FALSE)
  post_cell <- at(counted, df$prev, FALSE)

  out <- per_session(df, function(i) {
    pre <- i[pre_cell[i] & df$correct_response[i]]
    post <- i[post_cell[i] & df$correct_response[i]]
    pre_rt <- if (length(pre) > 0L) mean(df$rt[pre]) else NA_real_
    post_rt <- if (length(post) > 0L) mean(df$rt[post]) else NA_real_
    data.frame(
      n_stops = sum(counted[i]),
      n_pre = length(pre),
      n_post = length(post),
      pre_rt_ms = pre_rt,
      post_rt_ms = post_rt,
      pss_ms = if (length(pre) >= min_correct && length(post) >= min_correct) {
        post_rt - pre_rt
      } else {
        NA_real_
      }
    )
  })
  cbind(task = rep(task, nrow(out)), out, stringsAsFactors = FALSE)
}
