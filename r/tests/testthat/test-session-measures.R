stop_trials <- function(session = 1) {
  # Eight go trials (one missed) and four stop trials with SSD 250, two failed.
  go_rt <- c(300, 310, 320, 330, 340, 350, 900, NA)
  data.frame(
    subject_id = "s001", session = session, trial_id = "test_trial",
    condition = c(rep("go", 8), rep("stop", 4)),
    rt = c(go_rt, 430, 450, NA, NA),
    SSD = c(rep(200, 8), rep(250, 4)),
    correct_trial = c(rep(1, 7), 0, 0, 0, 1, 1),
    usable = TRUE,
    stringsAsFactors = FALSE
  )
}

test_that("ssrt() is the integration method with the session's slowest go RT for misses", {
  res <- ssrt(stop_trials())
  # p(respond | signal) = 2/4. The missed go takes 900, so the sorted go RTs
  # are 300 310 320 330 340 350 900 900, and the 4th (round(0.5 * 8)) is 330.
  # SSRT = 330 - mean stop SSD 250 = 80 ms.
  expect_equal(res$ssrt_ms, 80)
  expect_equal(names(res), c("task", "subject_id", "session", "ssrt_ms"))
})

test_that("ssrt() runs per session, ignores tags, and is NA with no answered go trial", {
  trials <- rbind(stop_trials(1), stop_trials(2))
  trials$usable[trials$session == 2] <- FALSE
  res <- ssrt(trials)
  expect_equal(res$session, c("1", "2"))
  expect_equal(res$ssrt_ms, c(80, 80))
  unanswered <- stop_trials()
  unanswered$rt[unanswered$condition == "go"] <- NA
  expect_true(is.na(ssrt(unanswered)$ssrt_ms))
  # It is calculate_ssrt_from_frame() with the session max, in ms.
  expect_equal(
    ssrt(stop_trials())$ssrt_ms,
    1000 * calculate_ssrt_from_frame(stop_trials(), rt_unit = "ms", max_go_rt_s = 0.9)
  )
})

span_trials <- function() {
  data.frame(
    subject_id = "s001",
    session = c(1, 1, 2, 1),
    trial_id = c("test_trial", "test_trial", "test_trial", "test_inter-stimulus"),
    condition = c("span", "span", "span", "operation"),
    response = c("[1, 2, 3]", "[7, 4]", "[]", NA),
    spatial_sequence = c("[1, 2, 3]", "[4, 5, 6, 7]", "[2, 9]", NA),
    rt_each_spatial_location_response_grid = c("[500, 1000, 1500]", "[800, 1600]", "[]", NA),
    stringsAsFactors = FALSE
  )
}

test_that("span_metrics() scores recall per session, in any order", {
  res <- span_metrics(span_trials(), "operation_span")
  s1 <- res[res$session == "1", ]
  # 3 of 3, then 2 of 4: mean 0.75.
  expect_equal(s1$n_trials, 2L)
  expect_equal(s1$accuracy, 0.75)
  expect_equal(s1$err_rate, 0.25)
  # 1500 / 3 and 1600 / 2: mean of 500 and 800.
  expect_equal(s1$response_interval_ms, 650)
  # A recall with no selection scores 0.
  expect_equal(res$accuracy[res$session == "2"], 0)
  # Simple span has no processing trials to leave out, and no condition column.
  simple <- span_trials()[1:3, ]
  simple$condition <- NULL
  expect_equal(span_metrics(simple, "simple_span")$accuracy, c(0.75, 0))
  expect_error(span_metrics(simple, "operation_span"), "condition")
  expect_error(span_metrics(simple, "flanker"), "does not take")
})

test_that("span_contrast() is operation span minus simple span, per session", {
  operation <- data.frame(subject_id = "s001", session = c("1", "2"), err_rate = c(0.4, 0.3))
  simple <- data.frame(subject_id = "s001", session = "1", err_rate = 0.25)
  res <- span_contrast(operation, simple)
  expect_equal(res$err_rate, c(0.15, NA))
})

test_that("subject_means() averages sessions and needs enough of them", {
  x <- data.frame(
    task = "flanker",
    subject_id = c("s001", "s001", "s001", "s002", "s002"),
    session = c("1", "2", "3", "1", "2"),
    condition = "congruent",
    n_trials = c(10L, 12L, 8L, 10L, 10L),
    rt_ms = c(400, 500, NA, 450, 470),
    accuracy = c(0.9, 0.8, 0.7, 1, 1),
    stringsAsFactors = FALSE
  )
  res <- subject_means(x)
  # s001 has three sessions with a value; RT averages the two it has.
  expect_equal(res$n_sessions, c(3L, 2L))
  expect_equal(res$rt_ms, c(450, NA))
  expect_equal(res$accuracy, c(0.8, NA))
  expect_equal(res$n_trials, c(30, 20))
  expect_equal(subject_means(x, min_sessions = 1)$rt_ms, c(450, 460))
  # A measure with no value in any session is NA, not NaN.
  x$rt_ms[x$subject_id == "s002"] <- NA
  expect_true(is.na(subject_means(x, min_sessions = 1)$rt_ms[2]))
  expect_false(is.nan(subject_means(x, min_sessions = 1)$rt_ms[2]))
})

test_that("subject_means() counts a task's sessions across all its rows", {
  m <- condition_metrics(rbind(
    trials_frame(c("congruent", "incongruent"), c(1, 1), c(400, 500), session = 1),
    trials_frame("congruent", 1, 420, session = 2),
    trials_frame("congruent", 1, 440, session = 3)
  ), "flanker")
  res <- subject_means(m)
  # Incongruent has one session, but the task has three, so it is reported.
  expect_equal(res$n_sessions, c(3L, 3L))
  expect_equal(res$rt_ms[res$condition == "incongruent"], 500)
})
