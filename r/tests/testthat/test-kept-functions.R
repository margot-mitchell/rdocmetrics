# calculate_ssrt(), calculate_ssrt_from_frame(), span_accuracy(),
# span_response_interval() and parse_list() are kept exactly as in 0.3.0-0.5.2
# for the pipelines that call them. They take the same arguments and return
# seconds, and the SSRT reference cases stay installed where those pipelines'
# tests read them.

test_that("the kept functions take the same arguments as before", {
  expect_equal(names(formals(calculate_ssrt)), c(
    "condition", "rt", "ssd", "rt_unit", "trial_id", "short_ssd_limit_s",
    "max_go_rt_s", "drop_na_ssd"
  ))
  expect_equal(names(formals(calculate_ssrt_from_frame)), c("df", "rt_unit", "cols", "..."))
  expect_equal(names(formals(span_accuracy)), c("response", "target", "order"))
  expect_equal(names(formals(span_response_interval)), c("rt_list", "rt_unit"))
  expect_equal(names(formals(parse_list)), "x")
})

test_that("the kept functions return seconds", {
  expect_equal(span_response_interval("[500, 1000, 1500]", "ms"), 0.5)
  df <- read_installed_ssrt_case("typical")
  ssrt_s <- calculate_ssrt_from_frame(df, rt_unit = "ms")
  expect_true(ssrt_s > 0 && ssrt_s < 1)
})

test_that("the SSRT reference cases are installed", {
  expect_true(file.exists(file.path(ssrt_dir(), "expected.csv")))
  expect_gt(length(list.files(file.path(ssrt_dir(), "cases"), pattern = "\\.csv$")), 20)
})
