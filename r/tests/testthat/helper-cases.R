# Test cases live in cases/<family>/: one CSV of trials per case, and an
# expected.csv of values worked out by hand. The SSRT reference cases are the
# exception: they are installed with the package, in extdata/ssrt.

read_case <- function(family, case) {
  utils::read.csv(test_path("cases", family, paste0(case, ".csv")),
    na.strings = c("", "NA"), stringsAsFactors = FALSE
  )
}

read_case_expected <- function(family) {
  utils::read.csv(test_path("cases", family, "expected.csv"),
    na.strings = c("", "NA"), stringsAsFactors = FALSE
  )
}

ssrt_dir <- function() system.file("extdata", "ssrt", package = "rdocmetrics", mustWork = TRUE)

read_installed_ssrt_case <- function(case) {
  utils::read.csv(file.path(ssrt_dir(), "cases", paste0(case, ".csv")),
    na.strings = c("", "NA"), stringsAsFactors = FALSE
  )
}

read_installed_ssrt_expected <- function() {
  utils::read.csv(file.path(ssrt_dir(), "expected.csv"),
    na.strings = c("", "NA"), stringsAsFactors = FALSE
  )
}

# A small trial table in the export format.
trials_frame <- function(condition, correct, rt, session = 1, subject = "s001",
                         usable = TRUE, block = 1, trial_seq = NULL) {
  n <- length(condition)
  data.frame(
    subject_id = rep(subject, length.out = n),
    session = rep(session, length.out = n),
    trial_seq = if (is.null(trial_seq)) seq_len(n) else trial_seq,
    block_num = rep(block, length.out = n),
    trial_id = "test_trial",
    condition = condition,
    correct_trial = correct,
    rt = rt,
    usable = rep(usable, length.out = n),
    stringsAsFactors = FALSE
  )
}
