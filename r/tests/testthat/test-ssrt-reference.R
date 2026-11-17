# calculate_ssrt() and calculate_ssrt_from_frame(), kept unchanged from
# earlier versions. The reference cases are installed with the package
# (extdata/ssrt), because other pipelines' tests read them too.
ssrt_case <- function(name) read_installed_ssrt_case(name)
expected_table <- function() read_installed_ssrt_expected()

# The fixture expected values are in milliseconds, because they were produced
# by the Python reference implementation in tools/gen/_oracles/. They are not
# regenerated. calculate_ssrt() returns seconds, so the comparison divides.
#
# They were produced with a 50 ms SSD floor that applies to go trials too, and
# missed go trials imputed at 1.0 s, so every fixture comparison passes those
# settings explicitly. The defaults are tested at the end of this file.
LEGACY <- list(short_ssd_limit_s = 0.05, max_go_rt_s = 1.0)

ssrt_from_case <- function(case, ...) {
  args <- utils::modifyList(LEGACY, list(...))
  do.call(calculate_ssrt_from_frame, c(list(ssrt_case(case), rt_unit = "ms"), args))
}

test_that("calculate_ssrt matches the reference on every fixture", {
  expected <- expected_table()
  expect_gt(nrow(expected), 0)

  for (i in seq_len(nrow(expected))) {
    case <- expected$case[i]
    # A frame without a required column is an error since 0.4.0; the
    # reference returned NA. Tested separately below.
    if (case %in% "missing_ssd_column") next
    expect_equal(
      ssrt_from_case(case, drop_na_ssd = FALSE),
      expected$expected_drop_na_ssd_false[i] / 1000,
      tolerance = 1e-9,
      info = paste0(case, " (drop_na_ssd = FALSE)")
    )
    expect_equal(
      ssrt_from_case(case, drop_na_ssd = TRUE),
      expected$expected_drop_na_ssd_true[i] / 1000,
      tolerance = 1e-9,
      info = paste0(case, " (drop_na_ssd = TRUE)")
    )
  }
})

test_that("drop_na_ssd only matters when an SSD is actually missing", {
  expected <- expected_table()
  differing <- expected$case[
    !is.na(expected$expected_drop_na_ssd_true) &
      !is.na(expected$expected_drop_na_ssd_false) &
      abs(expected$expected_drop_na_ssd_true -
        expected$expected_drop_na_ssd_false) > 1e-9
  ]
  expect_identical(differing, "na_ssd_present")
})

test_that("the default is to keep trials with a missing SSD", {
  expect_equal(ssrt_from_case("na_ssd_present"),
    ssrt_from_case("na_ssd_present", drop_na_ssd = FALSE)
  )
  expect_false(isTRUE(all.equal(
    ssrt_from_case("na_ssd_present"),
    ssrt_from_case("na_ssd_present", drop_na_ssd = TRUE)
  )))
})

test_that("undefined cases return NA rather than erroring", {
  for (case in c(
    "p_respond_zero", "all_ssd_below_limit", "empty_frame",
    "no_stop_trials", "no_test_rows"
  )) {
    expect_true(is.na(ssrt_from_case(case)), info = case)
  }
})

test_that("unrecognised condition labels are an error, not a silent NA", {
  df <- ssrt_case("typical")
  df$condition[1] <- "nogo"
  expect_error(calculate_ssrt_from_frame(df, rt_unit = "ms"), "Unrecognised stop-signal")
  expect_error(calculate_ssrt_from_frame(df, rt_unit = "ms"), "nogo")
})

test_that("an SSD staircase near zero is valid data, not a unit error", {
  # The plausibility window applies to rt only. A staircase legitimately sits
  # below 50 ms early in a run, so range-checking SSD would reject these two
  # fixtures outright. They must return NA, not raise.
  expect_true(is.na(ssrt_from_case("all_ssd_below_limit")))
  expect_true(is.na(ssrt_from_case("ssd_below_limit_go_only")))
})

test_that("the SSD floor applies to go trials too, which is why this returns NA", {
  # This is the one part of the method the published measure table does not
  # mention. A go trial carries the current staircase SSD, so a go trial below
  # the floor is dropped from the sorted go distribution. In this fixture every
  # go trial is below it, leaving no go trials and therefore no SSRT -- even
  # though all three stop trials are perfectly usable.
  df <- ssrt_case("ssd_below_limit_go_only")
  expect_true(all(df$SSD[df$condition == "go"] < 50))
  expect_true(all(df$SSD[df$condition == "stop"] >= 50))
  expect_true(is.na(ssrt_from_case("ssd_below_limit_go_only")))

  # Lowering the floor below the staircase value recovers a finite SSRT, which
  # confirms the floor is the cause rather than anything about the stop trials.
  expect_false(is.na(ssrt_from_case("ssd_below_limit_go_only",
    short_ssd_limit_s = 0.005)))
  # And with no floor at all, the default, it is finite too.
  expect_false(is.na(calculate_ssrt_from_frame(df, rt_unit = "ms",
    max_go_rt_s = 1.0)))
})

test_that("a wrongly declared rt_unit is caught", {
  df <- ssrt_case("typical")
  expect_error(calculate_ssrt_from_frame(df, rt_unit = "s"), "look like milliseconds")
})

test_that("the vector API and the frame wrapper agree", {
  df <- ssrt_case("typical")
  expect_equal(
    calculate_ssrt(
      condition = df$condition, rt = df$rt, ssd = df$SSD,
      rt_unit = "ms", trial_id = df$trial_id
    ),
    calculate_ssrt_from_frame(df, rt_unit = "ms")
  )
  expect_equal(
    calculate_ssrt(
      condition = df$condition, rt = df$rt, ssd = df$SSD,
      rt_unit = "ms", trial_id = df$trial_id, short_ssd_limit_s = 0.05,
      max_go_rt_s = 1.0
    ),
    ssrt_from_case("typical")
  )
})

test_that("an implausibly fast but genuine millisecond session is accepted", {
  expect_false(is.na(calculate_ssrt(
    condition = c(rep("go", 4), rep("stop", 2)),
    rt = c(120, 140, 160, 180, 130, NA),
    ssd = rep(60, 6),
    rt_unit = "ms"
  )))
})

test_that("mismatched vector lengths are an error", {
  expect_error(
    calculate_ssrt(condition = c("go", "stop"), rt = c(400), ssd = c(200, 200),
      rt_unit = "ms"),
    "same length"
  )
})

test_that("the defaults: no SSD floor, session-max go RT", {
  # Worked by hand. Go RTs 400, 450, 500, 550, 600 and one miss; stop trials
  # 420, miss, 480, miss with SSDs 40, 250, 150, 300 ms.
  #
  # Default: the miss takes the slowest go response, 600. Sorted go RTs are
  # 400 450 500 550 600 600; p(respond) = 2/4, so k = round(0.5 x 6) - 1 = 2
  # and RTgo(k) = 500. No floor, so mean SSD = (40 + 250 + 150 + 300) / 4 = 185.
  # SSRT = 500 - 185 = 315 ms.
  args <- list(
    condition = c(rep("go", 6), rep("stop", 4)),
    rt = c(400, 450, 500, 550, 600, NA, 420, NA, 480, NA),
    ssd = c(rep(200, 6), 40, 250, 150, 300),
    rt_unit = "ms"
  )
  expect_equal(do.call(calculate_ssrt, args), 0.315, tolerance = 1e-12)

  # A 50 ms floor drops the 40 ms stop trial: p(respond) = 1/3, k = round(2) - 1
  # = 1, RTgo(k) = 450, mean SSD = 700 / 3. SSRT = 450 - 233.33 = 216.67 ms.
  expect_equal(do.call(calculate_ssrt, c(args, list(short_ssd_limit_s = 0.05))),
    0.45 - 0.7 / 3, tolerance = 1e-12)

  # A fixed 1.0 s imputation leaves the rank at 500 here, so the value matches
  # the default; it differs once the miss is sorted past the k-th position.
  expect_equal(do.call(calculate_ssrt, c(args, list(max_go_rt_s = 1.0))),
    0.315, tolerance = 1e-12)
})

test_that("session-max imputation changes the rank when misses are many", {
  # Go RTs 300, 320, 340 and three misses; stop trials miss, 330, 310, 290 with
  # SSD 100 throughout. p(respond) = 3/4, k = round(4.5) - 1 = 3 (half to even).
  # Session max: sorted 300, 320, 340, 340, 340, 340 -> RTgo(3) = 340 -> SSRT
  # 240 ms. Fixed 1.0 s: sorted 300, 320, 340, then three of 1000 -> RTgo(3) =
  # 1000 -> SSRT 900 ms.
  args <- list(
    condition = c(rep("go", 6), rep("stop", 4)),
    rt = c(300, 320, 340, NA, NA, NA, NA, 330, 310, 290),
    ssd = rep(100, 10),
    rt_unit = "ms"
  )
  expect_equal(do.call(calculate_ssrt, args), 0.24, tolerance = 1e-12)
  expect_equal(do.call(calculate_ssrt, c(args, list(max_go_rt_s = 1.0))), 0.9,
    tolerance = 1e-12)
})

test_that("session-max imputation is undefined when no go trial was answered", {
  expect_true(is.na(calculate_ssrt(
    condition = c("go", "go", "stop", "stop"), rt = c(NA, NA, 300, NA),
    ssd = c(200, 200, 200, 250), rt_unit = "ms"
  )))
  expect_error(calculate_ssrt(condition = c("go", "stop"), rt = c(400, 300),
    ssd = c(200, 200), rt_unit = "ms", max_go_rt_s = "max"), "session_max")
  expect_error(calculate_ssrt(condition = c("go", "stop"), rt = c(400, 300),
    ssd = c(200, 200), rt_unit = "ms", short_ssd_limit_s = -1), "non-negative")
})

test_that("a frame without a required column is an error that names it", {
  expect_error(ssrt_from_case("missing_ssd_column"), "SSD")
  df <- ssrt_case("typical")
  names(df)[names(df) == "SSD"] <- "ssd"
  expect_error(calculate_ssrt_from_frame(df, rt_unit = "ms"), "ssd")
})

test_that("an SSD on a different scale from RT is an error", {
  df <- ssrt_case("typical")
  df$SSD <- df$SSD / 1000  # seconds, while rt is in ms
  expect_error(calculate_ssrt_from_frame(df, rt_unit = "ms"), "SSD")
})

test_that("an SSD in milliseconds beside RTs in seconds is an error", {
  # RTs in seconds and SSDs in milliseconds, declared rt_unit = "s": the SSDs
  # read as hundreds of seconds and SSRT comes out hugely negative.
  args <- list(
    condition = c(rep("go", 6), rep("stop", 4)),
    rt = c(0.40, 0.45, 0.50, 0.55, 0.60, NA, 0.42, NA, 0.48, NA),
    ssd = c(rep(200, 6), 150, 200, 250, 300),
    rt_unit = "s"
  )
  expect_error(do.call(calculate_ssrt, args), "same unit")
  # Early in a run the staircase is short, but 50 "seconds" against a 0.6 s
  # slowest response is still far out of range.
  args$ssd <- c(rep(50, 6), 50, 40, 30, 50)
  expect_error(do.call(calculate_ssrt, args), "same unit")
})

test_that("an SSD a little above the slowest RT is accepted", {
  # A fast responder whose staircase climbed past their go RTs is valid data:
  # go 300-350 ms, SSDs up to 500 ms. p(respond) = 1/4, k = round(0.25 x 4) - 1
  # = 0, RTgo(0) = 0.30; mean SSD = 0.425 s, so SSRT = -0.125 s.
  expect_equal(calculate_ssrt(
    condition = c(rep("go", 4), rep("stop", 4)),
    rt = c(0.30, 0.31, 0.33, 0.35, 0.34, NA, NA, NA),
    ssd = c(rep(0.45, 4), 0.35, 0.40, 0.45, 0.50),
    rt_unit = "s"
  ), 0.30 - 0.425, tolerance = 1e-12)
})

test_that("a mis-cased or padded trial_id is an error, not a dropped row", {
  df <- ssrt_case("typical")
  df$trial_id[1] <- "Test_trial"
  expect_error(calculate_ssrt_from_frame(df, rt_unit = "ms"), "Test_trial")
  df <- ssrt_case("typical")
  df$trial_id[2] <- "test_trial "
  expect_error(calculate_ssrt_from_frame(df, rt_unit = "ms"), "test_trial ")
  # With every row mis-cased there is no exact match, which returned NA.
  df <- ssrt_case("typical")
  df$trial_id <- toupper(df$trial_id)
  expect_error(calculate_ssrt_from_frame(df, rt_unit = "ms"), "TEST_TRIAL")
  # Labels that are not test trials at all are still left out quietly.
  expect_equal(ssrt_from_case("mixed_trial_ids"),
    expected_table()$expected_drop_na_ssd_false[
      expected_table()$case == "mixed_trial_ids"] / 1000,
    tolerance = 1e-9)
})
