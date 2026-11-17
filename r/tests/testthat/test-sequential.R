# The cases in cases/pes, cases/cse and cases/pss were worked out by hand for
# earlier versions of this package, with RT in seconds. Their values are
# compared here in milliseconds. A case with more than one session was checked
# as the mean of its per-session values.

session_mean <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) > 0L) mean(x) else NA_real_
}

test_that("post_error_slowing() reproduces the hand-checked cases", {
  expected <- read_case_expected("pes")
  expect_gt(nrow(expected), 0)
  for (i in seq_len(nrow(expected))) {
    row <- expected[i, ]
    res <- post_error_slowing(read_case("pes", row$case), row$task,
      min_triplets = row$min_triplets
    )
    info <- paste(row$case, "min", row$min_triplets)
    expect_equal(session_mean(res$pes_ms), as.numeric(row$pes_ms), tolerance = 1e-9, info = info)
    expect_equal(sum(res$n_triplets), row$n_triplets, info = info)
    expect_equal(sum(res$n_errors), row$n_errors, info = info)
  }
})

test_that("congruency_sequence_effect() reproduces the hand-checked cases", {
  expected <- read_case_expected("cse")
  expect_gt(nrow(expected), 0)
  for (i in seq_len(nrow(expected))) {
    row <- expected[i, ]
    res <- congruency_sequence_effect(read_case("cse", row$case), "flanker")
    expect_equal(session_mean(res$cse_ms), as.numeric(row$cse_ms), tolerance = 1e-9,
      info = row$case
    )
  }
})

test_that("post_stop_slowing() reproduces the hand-checked cases", {
  expected <- read_case_expected("pss")
  expect_gt(nrow(expected), 0)
  for (i in seq_len(nrow(expected))) {
    row <- expected[i, ]
    res <- post_stop_slowing(read_case("pss", row$case), min_correct = row$min_correct)
    info <- paste(row$case, "min", row$min_correct)
    expect_equal(session_mean(res$pss_ms), as.numeric(row$pss_ms), tolerance = 1e-9, info = info)
    expect_equal(sum(res$n_pre), row$n_pre, info = info)
    expect_equal(sum(res$n_post), row$n_post, info = info)
  }
})

test_that("post-error slowing uses correct-error-correct triplets and needs two of them", {
  # Two triplets: 400 E 500 and 420 E 470. Post minus pre: (500 + 470)/2 - (400 + 420)/2.
  trials <- trials_frame(
    c(rep("congruent", 7)),
    c(1, 0, 1, 1, 0, 1, 1),
    c(400, 600, 500, 420, 610, 470, 480)
  )
  res <- post_error_slowing(trials, "flanker")
  expect_equal(res$n_errors, 2L)
  expect_equal(res$n_triplets, 2L)
  expect_equal(res$pes_ms, (500 + 470) / 2 - (400 + 420) / 2)
  # With one triplet it is not reported.
  one <- post_error_slowing(trials[1:4, ], "flanker")
  expect_equal(one$n_triplets, 1L)
  expect_true(is.na(one$pes_ms))
  expect_equal(post_error_slowing(trials[1:4, ], "flanker", min_triplets = 1)$pes_ms, 100)
})

test_that("an unusable trial, a block break or a missed go trial is not a neighbour or an error", {
  base <- trials_frame(rep("congruent", 3), c(1, 0, 1), c(400, 600, 500))
  unusable <- base
  unusable$usable[1] <- FALSE
  expect_equal(post_error_slowing(unusable, "flanker")$n_errors, 0L)
  split <- base
  split$block_num <- c(1, 2, 2)
  expect_equal(post_error_slowing(split, "flanker")$n_errors, 0L)
  # In go/no-go a missed go trial is not an error, and the neighbours of a
  # no-go commission must be go trials.
  gng <- trials_frame(c("go", "go", "go", "nogo", "go"), c(1, 0, 1, 0, 1),
    c(400, NA, 450, 300, 500))
  res <- post_error_slowing(gng, "go_nogo", min_triplets = 1)
  expect_equal(res$n_errors, 1L)
  expect_equal(res$pes_ms, 500 - 450)
  nogo_neighbour <- trials_frame(c("go", "nogo", "nogo", "go"), c(1, 0, 1, 1),
    c(400, 300, NA, 500))
  expect_equal(post_error_slowing(nogo_neighbour, "go_nogo")$n_errors, 0L)
})

test_that("the congruency sequence effect is (cI - cC) - (iI - iC) after correct trials", {
  # Pairs, previous -> current: C->I (iC) 520, I->C (cI) 450, C->C (cC) 430,
  # C->I (iC) 540, I->I (iI) 560.
  trials <- trials_frame(
    c("congruent", "incongruent", "congruent", "congruent", "incongruent", "incongruent"),
    rep(1, 6),
    c(400, 520, 450, 430, 540, 560)
  )
  res <- congruency_sequence_effect(trials, "flanker")
  expect_equal(c(res$cI_ms, res$cC_ms, res$iI_ms, res$iC_ms), c(450, 430, 560, 530))
  expect_equal(res$cse_ms, (450 - 430) - (560 - 530))
  # A trial after an error has no cell: the first iC pair goes, leaving 540.
  after_error <- trials
  after_error$correct_trial[1] <- 0
  expect_equal(congruency_sequence_effect(after_error, "flanker")$iC_ms, 540)
  expect_error(congruency_sequence_effect(trials, "n_back"), "does not take")
})

test_that("post-stop slowing compares the go trials around a counted stop", {
  # go 400, go 420 (pre: its previous is a correct go), stop, go 480 (post).
  trials <- trials_frame(c("go", "go", "stop", "go"), c(1, 1, 0, 1), c(400, 420, 350, 480))
  res <- post_stop_slowing(trials, min_correct = 1)
  expect_equal(res$n_stops, 1L)
  expect_equal(res$pss_ms, 480 - 420)
  # Four correct responses in each cell are needed by default.
  expect_true(is.na(post_stop_slowing(trials)$pss_ms))
  # A pre-stop trial that follows an error does not count.
  after_error <- trials
  after_error$correct_trial[1] <- 0
  expect_equal(post_stop_slowing(after_error, min_correct = 1)$n_stops, 0L)
  # The first trial of a block can be the pre-stop trial.
  opening <- trials_frame(c("go", "stop", "go"), c(1, 0, 1), c(420, 350, 480))
  expect_equal(post_stop_slowing(opening, min_correct = 1)$pss_ms, 60)
})

test_that("every trial of a session must be passed, in sequence", {
  trials <- trials_frame(rep("congruent", 3), c(1, 0, 1), c(400, 600, 500))
  gap <- trials[-2, ]
  expect_error(post_error_slowing(gap, "flanker"), "trial_seq does not run")
  shuffled <- trials[c(3, 1, 2), ]
  expect_equal(post_error_slowing(shuffled, "flanker", min_triplets = 1)$pes_ms, 100)
  expect_error(post_error_slowing(trials[, -which(names(trials) == "block_num")], "flanker"),
    "block_num")
})
