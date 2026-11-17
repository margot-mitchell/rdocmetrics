# rdocmetrics 0.6.0

Each function takes one task's trial table and returns one row per subject and
session, with times in milliseconds.

- `condition_metrics()`: accuracy, error rate, mean and variance of RT,
  omission rate and trial counts per condition, with the pooled n-back and
  visual-search cells.
- `condition_contrasts()`: the study's 14 contrasts, each harder minus easier.
- `post_error_slowing()`, `congruency_sequence_effect()` and
  `post_stop_slowing()`: the sequential measures.
- `ssrt()`: stop-signal reaction time by the integration method.
- `span_metrics()` and `span_contrast()`: span recall accuracy, error rate and
  response interval, and operation span minus simple span.
- `subject_means()`: any of these averaged over sessions, with a minimum
  number of sessions per subject (3 by default).

`calculate_ssrt()`, `calculate_ssrt_from_frame()`, `span_accuracy()`,
`span_response_interval()` and `parse_list()` work on vectors or a single
task's frame and return seconds.
