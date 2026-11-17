test_that("condition_metrics() reproduces the hand-checked cells", {
  expected <- read_case_expected("cells")
  expect_gt(nrow(expected), 0)
  for (i in seq_len(nrow(expected))) {
    row <- expected[i, ]
    m <- condition_metrics(read_case("cells", row$case), row$task)
    if (row$task == "flanker") {
      cell <- m[m$condition == "congruent", ]
      expect_equal(cell$accuracy, as.numeric(row$accuracy), info = row$case)
      expect_equal(cell$omission_rate, as.numeric(row$omission_rate), info = row$case)
      expect_equal(cell$rt_ms, as.numeric(row$rt_ms), info = row$case)
      expect_equal(cell$rt_var_ms2, as.numeric(row$rt_var_ms2), info = row$case)
    } else {
      go <- m[m$condition == "go", ]
      stop <- m[m$condition == "stop", ]
      expect_equal(if (nrow(go)) go$rt_ms else NA_real_, as.numeric(row$go_rt_ms),
        info = row$case
      )
      expect_equal(if (nrow(stop)) stop$accuracy else NA_real_,
        as.numeric(row$stop_accuracy),
        info = row$case
      )
    }
  }
})

test_that("accuracy is over responses, RT over correct responses, and unusable trials are left out", {
  trials <- trials_frame(
    condition = c(rep("congruent", 5), rep("incongruent", 2)),
    correct = c(1, 1, 0, 0, 1, 1, 1),
    rt = c(400, 500, 450, NA, 150, 600, 0),
    usable = c(TRUE, TRUE, TRUE, TRUE, FALSE, TRUE, TRUE)
  )
  m <- condition_metrics(trials, "flanker")
  con <- m[m$condition == "congruent", ]
  # Usable: 400, 500, 450 (wrong) and one omission. 2 of 3 responses correct.
  expect_equal(con$n_trials, 5L)
  expect_equal(con$n_usable, 4L)
  expect_equal(con$n_responses, 3L)
  expect_equal(con$accuracy, 2 / 3)
  expect_equal(con$err_rate, 1 / 3)
  # RT from the two correct responses: mean 450, variance 5000.
  expect_equal(con$rt_ms, 450)
  expect_equal(con$rt_var_ms2, 5000)
  expect_equal(con$omission_rate, 1 / 4)
  # An RT of 0 is a response: mean of 600 and 0.
  inc <- m[m$condition == "incongruent", ]
  expect_equal(inc$n_responses, 2L)
  expect_equal(inc$rt_ms, 300)
  expect_equal(inc$rt_var_ms2, var(c(600, 0)))
})

test_that("go/no-go accuracy is over all trials, and no-go RT is the commission errors' RT", {
  trials <- trials_frame(
    condition = c("go", "go", "nogo", "nogo", "nogo"),
    correct = c(1, 0, 1, 0, 0),
    rt = c(300, NA, NA, 350, 410)
  )
  m <- condition_metrics(trials, "go_nogo")
  go <- m[m$condition == "go", ]
  nogo <- m[m$condition == "nogo", ]
  # The missed go trial is an error: 1 of 2.
  expect_equal(go$accuracy, 0.5)
  expect_equal(go$rt_ms, 300)
  # One withheld no-go of three; RT is the mean of the two commissions.
  expect_equal(nogo$accuracy, 1 / 3)
  expect_equal(nogo$rt_ms, 380)
  expect_equal(nogo$omission_rate, 1 / 3)
})

test_that("stop accuracy is over all stop trials, and stop trials with no correct_trial are left out", {
  trials <- trials_frame(
    condition = c("go", "go", "stop", "stop", "stop"),
    correct = c(1, 1, 1, 0, 1),
    rt = c(400, 440, NA, 380, NA)
  )
  stop <- condition_metrics(trials, "stop_signal")
  expect_equal(stop$accuracy[stop$condition == "stop"], 2 / 3)
  # As in the modeling pipeline, a stop trial without a correct_trial value
  # is not counted at all, so a session of only such trials has no accuracy.
  unscored <- read_case("cells", "stop_typical")
  unscored$correct_trial[unscored$condition == "stop"] <- NA
  m <- condition_metrics(unscored, "stop_signal")
  expect_true(is.na(m$accuracy[m$condition == "stop"]))
})

test_that("placeholder conditions are dropped and pooled cells pool trials", {
  trials <- trials_frame(
    condition = c("na_1", "match_1", "match_1", "mismatch_1", "mismatch_1", "mismatch_1"),
    correct = c(1, 1, 0, 1, 1, 1),
    rt = c(450, 500, 600, 700, 800, 900)
  )
  m <- condition_metrics(trials, "n_back")
  expect_false("na_1" %in% m$condition)
  pooled <- m[m$condition == "1_back", ]
  # 4 of 5 responses correct, not the mean of the two cells (0.5 and 1).
  expect_equal(pooled$accuracy, 0.8)
  expect_equal(pooled$rt_ms, mean(c(500, 700, 800, 900)))
  expect_equal(pooled$n_trials, 5L)
})

test_that("each subject and session gets its own rows, in order", {
  trials <- rbind(
    trials_frame(c("congruent", "incongruent"), c(1, 1), c(400, 500), session = 2),
    trials_frame(c("congruent", "incongruent"), c(1, 0), c(420, 520), session = 1),
    trials_frame(c("congruent"), 1, 410, subject = "s002")
  )
  m <- condition_metrics(trials, "flanker")
  expect_equal(m$subject_id, c("s001", "s001", "s001", "s001", "s002"))
  expect_equal(m$session, c("1", "1", "2", "2", "1"))
  expect_equal(m$condition, c("congruent", "incongruent", "congruent", "incongruent", "congruent"))
  expect_equal(m$task, rep("flanker", 5))
})

test_that("only test trials are used", {
  trials <- trials_frame(c("congruent", "congruent"), c(1, 0), c(400, 900))
  trials$trial_id[2] <- "practice_trial"
  m <- condition_metrics(trials, "flanker")
  expect_equal(m$n_trials, 1L)
  expect_equal(m$accuracy, 1)
})

test_that("every time is in milliseconds", {
  trials <- read.csv(system.file("examples", "flanker_trials.csv", package = "rdocmetrics"))
  m <- condition_metrics(trials, "flanker")
  expect_true(all(m$rt_ms > 100 & m$rt_ms < 2000))
  time_cols <- grep("rt|ms", names(m), value = TRUE)
  expect_setequal(time_cols, c("rt_ms", "rt_var_ms2"))
})

test_that("input mistakes are errors that say what is wrong", {
  trials <- trials_frame(c("congruent", "incongruent"), c(1, 1), c(400, 500))
  expect_error(condition_metrics(trials, "flankers"), "does not take task")
  expect_error(condition_metrics(trials, "simple_span"), "span_metrics")
  expect_error(condition_metrics(trials[, -which(names(trials) == "rt")], "flanker"), "missing column")
  bad <- trials
  bad$condition[1] <- "Congruent"
  expect_error(condition_metrics(bad, "flanker"), "not declared")
  secs <- trials
  secs$rt <- c(0.4, 0.5)
  expect_error(condition_metrics(secs, "flanker"), "seconds")
  neg <- trials
  neg$rt[1] <- -1
  expect_error(condition_metrics(neg, "flanker"), "negative")
  na_usable <- trials
  na_usable$usable[1] <- NA
  expect_error(condition_metrics(na_usable, "flanker"), "usable")
})
