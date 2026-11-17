# The per-trial span scoring (span_accuracy(), span_response_interval(),
# parse_list()), kept unchanged from earlier versions. The cases and their
# expected values were worked out by hand; see cases/span/.

test_that("span scoring matches the hand-checked cases", {
  expected <- read_case_expected("span")
  expect_gt(nrow(expected), 0)

  for (i in seq_len(nrow(expected))) {
    row <- expected[i, ]
    trials <- read_case("span", row$case)
    info <- row$case

    n <- nrow(trials)
    response <- if (n == 0L) character(0) else as.character(trials$response)
    target <- if (n == 0L) character(0) else as.character(trials$spatial_sequence)
    rt_list <- if (n == 0L) character(0) else as.character(trials$rt_each_response)

    expect_equal(span_accuracy(response, target, "irrespective"),
      row$accuracy_irrespective, tolerance = 1e-9,
      info = paste("irrespective:", info))
    expect_equal(span_accuracy(response, target, "respective"),
      row$accuracy_respective, tolerance = 1e-9,
      info = paste("respective:", info))
    # The error rates were counted miss by miss, so this checks accuracy a
    # second way.
    expect_equal(1 - span_accuracy(response, target, "irrespective"),
      row$error_rate_irrespective, tolerance = 1e-9,
      info = paste("error_rate irrespective:", info))
    expect_equal(1 - span_accuracy(response, target, "respective"),
      row$error_rate_respective, tolerance = 1e-9,
      info = paste("error_rate respective:", info))
    interval <- span_response_interval(rt_list, rt_unit = "ms")
    expect_equal(if (is.na(interval)) NA_real_ else interval * 1000,
      row$response_interval_ms, tolerance = 1e-9,
      info = paste("response_interval:", info))
  }
})

test_that("the list parser strips BOTH brackets", {
  # The bug: sub() replaces only the first alternation match, so the leading
  # bracket consumed the substitution and the trailing one survived.
  expect_identical(parse_list("[1, 2, 3]"), c("1", "2", "3"))
  expect_identical(parse_list("[]"), character(0))
  expect_identical(parse_list(""), character(0))
  expect_identical(parse_list(NA), character(0))
  expect_identical(parse_list("[7]"), "7")
  # Every element must be numeric-coercible, i.e. no stray bracket survives.
  expect_false(anyNA(suppressWarnings(as.numeric(parse_list("[500, 900, 1500, 2400]")))))
})

test_that("REGRESSION: accuracy irrespective of order ignores order", {
  # Was 0.333 for a fully-correct reversed recall, under a measure whose name
  # says order is ignored. It only misfired when the last item was not recalled
  # last, so perfect trials looked fine and the bug hid.
  expect_equal(span_accuracy("[3, 2, 1]", "[1, 2, 3]"), 1)
  expect_equal(span_accuracy("[1, 2, 3]", "[1, 2, 3]"), 1)
  # And the order-sensitive variant still sees the difference: reversing a
  # three-element sequence leaves the middle element in place.
  expect_equal(span_accuracy("[3, 2, 1]", "[1, 2, 3]", order = "respective"),
    1 / 3)
})

test_that("REGRESSION: the denominator is deduplicated too", {
  # Was 0.667, because only the numerator was deduplicated, so a target
  # containing a repeated cell could never score 1.0.
  expect_equal(span_accuracy("[1, 2]", "[1, 2, 2]"), 1)
})

test_that("REGRESSION: the final response contributes to the interval", {
  # Was computed from the second-to-last timestamp, because the last element
  # parsed as "2400]" and coerced to NA.
  expect_equal(
    span_response_interval("[500, 900, 1500, 2400]", rt_unit = "ms"),
    0.6
  )
})

test_that("a trial presenting no target is skipped, not scored zero", {
  expect_true(is.na(span_accuracy("[1, 2, 3]", "[]")))
  # ... but an empty RESPONSE to a real target scores zero, which is correct.
  expect_equal(span_accuracy("[]", "[1, 2, 3]"), 0)
})

test_that("an invalid order argument is an error", {
  expect_error(
    span_accuracy("[1]", "[1]", order = "sideways"),
    "should be one of"
  )
})

test_that("span response times must be numeric and in the declared unit", {
  # Cumulative timestamps 500, 1000, 1500 ms: the last over the count, 0.5 s.
  expect_equal(span_response_interval("[500, 1000, 1500]", "ms"), 0.5)
  expect_error(span_response_interval("[500, x, 1500]", "ms"), "not numeric")
  expect_error(span_response_interval("[500, 1000, 1500]", "s"), "rt_unit")
})

test_that("quoted elements and missing-value tokens parse as a span response", {
  # Quoted cells are the same cells: 2 of 2 recalled.
  expect_equal(span_accuracy("['6', '5']", "[6, 5]"), 1)
  expect_equal(span_accuracy('["6","5"]', "[6, 5]"), 1)
  # "null", "None" and "NA" are empty responses, like "[]".
  expect_identical(lapply(c("null", "None", "NA", "[]"), parse_list), rep(list(character(0)), 4))
})

test_that("cells that parse as numbers are compared as numbers", {
  # "6.0" and "6" are the same cell.
  expect_equal(span_accuracy("[6.0, 5.0]", "[6, 5]"), 1)
  expect_equal(span_accuracy("[6.0, 5.0]", "[6, 5]", order = "respective"), 1)
  expect_equal(span_accuracy("[6, 5]", "['6.0', '05']"), 1)
  # Position still matters under "respective": 0 of 2 in place.
  expect_equal(span_accuracy("[5.0, 6.0]", "[6, 5]", order = "respective"), 0)
  # 6.5 is not 6: 1 of 2 recalled.
  expect_equal(span_accuracy("[6.5, 5]", "[6, 5]"), 0.5)
  # Cells that are not numbers are compared as text.
  expect_equal(span_accuracy("[a1, b2]", "[a1, b2]"), 1)
  expect_equal(span_accuracy("[A1, b2]", "[a1, b2]"), 0.5)
})
