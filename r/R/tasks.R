# Task facts, from the CSV files in inst/tables:
#
# - task_conditions.csv: each task's conditions; which are placeholders (no
#   measure is reported for them); which cells pool other conditions; whether
#   accuracy is over all trials or over responses; whether RT comes from correct
#   responses or from all responses; and which trials may sit next to an event
#   in a sequential measure.
# - contrasts.csv: each contrast, as left minus right (harder minus easier).
# - missing_tokens.csv: the strings that mean "missing" in a character column.
#
# They are plain CSV so a reviewer can read a diff and see that a fact changed.

RT_TASKS <- c(
  "ax_cpt", "cued_task_switching", "flanker", "go_nogo", "n_back",
  "spatial_cueing", "spatial_task_switching", "stop_signal", "stroop",
  "visual_search"
)
SPAN_TASKS <- c("operation_span", "simple_span")

.task_tables <- new.env(parent = emptyenv())

read_task_table <- function(name) {
  if (is.null(.task_tables[[name]])) {
    path <- system.file("tables", paste0(name, ".csv"), package = "rdocmetrics")
    if (!nzchar(path)) {
      stop("rdocmetrics tables not found; the package is installed incorrectly.",
        call. = FALSE
      )
    }
    .task_tables[[name]] <- utils::read.csv(
      path,
      colClasses = "character",
      na.strings = character(0),
      check.names = FALSE,
      encoding = "UTF-8",
      stringsAsFactors = FALSE
    )
  }
  .task_tables[[name]]
}

# The tokens that mean "missing" in a character column, matched
# case-insensitively: pandas writes "<NA>", R writes "NA", JSON "null".
missing_tokens <- function() {
  tolower(read_task_table("missing_tokens")$token)
}

check_task <- function(task, allowed, fn) {
  if (!is.character(task) || length(task) != 1L || is.na(task)) {
    stop("`task` must be one task name, such as \"flanker\".", call. = FALSE)
  }
  if (!task %in% allowed) {
    hint <- if (task %in% SPAN_TASKS) " Use span_metrics() for the span tasks." else ""
    stop(fn, "() does not take task ", sQuote(task), ". Tasks: ",
      paste(allowed, collapse = ", "), ".", hint,
      call. = FALSE
    )
  }
  invisible(task)
}

# The task's rows of task_conditions.csv.
task_conditions <- function(task) {
  tab <- read_task_table("task_conditions")
  tab[tab$task == task, , drop = FALSE]
}

# Every condition label in the trials must be declared for the task. A
# mis-cased or padded label is an error rather than a silently empty cell.
check_conditions <- function(condition, task) {
  declared <- task_conditions(task)
  declared <- declared$condition[!nzchar(declared$pooled_from)]
  unknown <- setdiff(unique(condition), declared)
  if (length(unknown) > 0L) {
    stop("Condition ", sQuote(unknown[1L]), " is not declared for task ",
      sQuote(task), ". Declared conditions: ", paste(declared, collapse = ", "),
      ". Relabel upstream, or add the condition to inst/tables/task_conditions.csv.",
      call. = FALSE
    )
  }
  invisible(condition)
}

# Per-trial lookup of one task_conditions.csv column.
condition_fact <- function(condition, task, column) {
  tab <- task_conditions(task)
  tab[[column]][match(condition, tab$condition)]
}
