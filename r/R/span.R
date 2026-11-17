# Span (spatial working memory) scoring, per trial.
#
# span_metrics() (stop_span.R) uses these per session. span_accuracy() and
# span_response_interval() are also kept with their 0.3.0-0.5.2 interface,
# returning seconds, for callers that use them directly.
#
# The trial-level columns hold serialised lists -- "[6, 5, 3, 15]" -- one per
# trial, so these functions take character vectors of those strings. Each
# recall trial is scored against its own target, and a trial with no selection
# scores 0. Targets are deduplicated, so a target with a repeated cell can still
# score 1. Cells that parse as numbers are compared as numbers.

#' Parse a serialised list column
#'
#' Accepts \code{"[6, 5, 3]"}, quoted elements such as \code{"['6', '5']"} or
#' \code{'["6","5"]'}, and the empty forms \code{"[]"}, \code{""}, \code{NA}
#' and missing-value tokens such as \code{"null"} or \code{"None"}. Returns a
#' character vector of elements, empty for the empty forms.
#'
#' @param x A single serialised list string.
#' @return A character vector.
#' @examples
#' parse_list("[1, 2, 3]")
#' @export
parse_list <- function(x) {
  if (length(x) == 0L) {
    return(character(0))
  }
  if (is.list(x) && !is.data.frame(x)) {
    x <- x[[1L]]
  }
  s <- trimws(as.character(x)[[1L]])
  if (is.na(s) || !nzchar(s) || identical(s, "[]") || tolower(s) %in% missing_tokens()) {
    return(character(0))
  }
  # gsub, not sub: sub() replaces only the first alternation match, so the
  # leading bracket consumes the substitution and the trailing one survives.
  inner <- gsub("^\\[|\\]$", "", s)
  if (!nzchar(trimws(inner))) {
    return(character(0))
  }
  parts <- trimws(strsplit(inner, ",")[[1L]])
  # Quoted elements, as in "['6', '5']" or JSON's ["6","5"], are the same cells.
  parts <- gsub("^['\"]|['\"]$", "", parts)
  parts[nzchar(parts)]
}

# Cells that parse as decimal numbers are compared as numbers, so "6.0" and
# "06" are cell 6; any other cell is compared as text, exactly.
DECIMAL_PATTERN <- "^[+-]?([0-9]+[.]?[0-9]*|[.][0-9]+)([eE][+-]?[0-9]+)?$"

canonical_cells <- function(cells) {
  values <- suppressWarnings(as.numeric(cells))
  numeric <- grepl(DECIMAL_PATTERN, cells) & is.finite(values)
  cells[numeric] <- as.character(values[numeric])
  cells
}

# Per-trial accuracy, returning NA when the trial presented no target.
span_trial_accuracy <- function(response, target, order) {
  resp <- canonical_cells(parse_list(response))
  corr <- canonical_cells(parse_list(target))
  if (length(corr) == 0L) {
    return(NA_real_)
  }
  if (identical(order, "irrespective")) {
    # Deduplicate BOTH sides. Deduplicating only the numerator means a target
    # with a repeated cell can never score 1.0.
    return(length(intersect(unique(resp), unique(corr))) / length(unique(corr)))
  }
  n <- min(length(resp), length(corr))
  matches <- if (n > 0L) sum(resp[seq_len(n)] == corr[seq_len(n)]) else 0L
  matches / length(corr)
}

#' Span recall accuracy
#'
#' @param response Serialised response lists, one per trial.
#' @param target Serialised target sequences, one per trial.
#' @param order \code{"irrespective"} of presentation order (set overlap) or
#'   \code{"respective"} of it (position-wise match).
#' @return Mean per-trial accuracy across trials that presented a target, or
#'   \code{NA_real_} when no trial presented one.
#' @details Kept for compatibility with callers of earlier versions; for
#'   per-session values use [span_metrics()]. A trial presenting no target is skipped; an empty response to a
#'   real target scores zero. A cell that parses as a decimal number is compared
#'   as a number, so \code{"6.0"} matches \code{"6"}; any other cell is compared
#'   as text, exactly.
#' @examples
#' # Order genuinely ignored: a reversed recall is fully correct.
#' span_accuracy("[3, 2, 1]", "[1, 2, 3]")
#' @export
span_accuracy <- function(response, target,
                          order = c("irrespective", "respective")) {
  order <- match.arg(order)
  response <- as.character(response)
  target <- as.character(target)
  check_lengths(response = response, target = target)
  if (length(response) == 0L) {
    return(NO_VALUE)
  }
  per_trial <- vapply(
    seq_along(response),
    function(i) span_trial_accuracy(response[[i]], target[[i]], order),
    numeric(1)
  )
  per_trial <- per_trial[!is.na(per_trial)]
  if (length(per_trial) == 0L) {
    return(NO_VALUE)
  }
  mean(per_trial)
}

#' Mean inter-response interval within a span trial
#'
#' The response-time column holds cumulative timestamps from trial onset, one
#' per selected cell. The mean interval between selections is therefore the last
#' timestamp divided by the number of selections.
#'
#' @param rt_list Serialised cumulative response-time lists, one per trial.
#' @param rt_unit \code{"s"} or \code{"ms"}. Required.
#' @details Kept for compatibility with callers of earlier versions, so it
#'   returns seconds; [span_metrics()] reports the same interval in ms.
#' @return Mean inter-response interval in seconds, averaged across trials, or
#'   \code{NA} when no trial has a response. A non-numeric timestamp, or values
#'   implausible for \code{rt_unit}, are an error.
#' @export
span_response_interval <- function(rt_list, rt_unit) {
  rt_list <- as.character(rt_list)
  if (length(rt_list) == 0L) {
    return(NO_VALUE)
  }
  values <- lapply(rt_list, function(x) as_numeric_vector(parse_list(x), "rt_list"))
  # The timestamps are times from trial onset, so the unit check applies to
  # them; the per-trial intervals computed below are not checked again.
  rt_to_seconds(unlist(values), rt_unit)
  per_trial <- vapply(values, function(values) {
    values <- values[is.finite(values)]
    if (length(values) == 0L) {
      return(NA_real_)
    }
    values[[length(values)]] / length(values)
  }, numeric(1))
  per_trial <- per_trial[!is.na(per_trial)]
  if (length(per_trial) == 0L) {
    return(NO_VALUE)
  }
  # Convert after aggregating: the arithmetic is linear, so the order does not
  # matter, but this keeps a single conversion point.
  mean(rt_to_seconds(per_trial, rt_unit, plausible = FALSE))
}
