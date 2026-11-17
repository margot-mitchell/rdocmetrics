# rdocmetrics

Behavioral measures for the RDoC task battery, in R.

RDoC is the NIMH Research Domain Criteria framework. The battery is the Poldrack
Lab's set of 12 RDoC behavioral tasks, run in jsPsych; the task code is at
[poldracklab/expfactory-experiments-rdoc](https://github.com/poldracklab/expfactory-experiments-rdoc).
The tasks are AX-CPT, cued task switching, flanker, go/no-go, n-back, operation
span, simple span, spatial cueing, spatial task switching, stop signal, Stroop
and visual search.

Each function takes one task's trial table and returns one row per subject and
session: accuracy, reaction time and omissions per condition, contrasts between
conditions, post-error slowing, the congruency sequence effect, post-stop
slowing, stop-signal reaction time and span recall. Every time is in
milliseconds.

**Status: 0.6.0, under active development.** It is written for analyses of this
battery.

## Install

```r
remotes::install_github("margot-mitchell/rdocmetrics@v0.6.0", subdir = "r")
```

## Quick start

The package includes a small synthetic flanker dataset, two sessions of 24
trials, to try things on:

```r
library(rdocmetrics)

trials <- read.csv(system.file("examples", "flanker_trials.csv",
                               package = "rdocmetrics"))
m <- condition_metrics(trials, "flanker")
m[, c("session", "condition", "n_usable", "n_responses", "accuracy", "rt_ms", "omission_rate")]
#>   session   condition n_usable n_responses  accuracy    rt_ms omission_rate
#> 1       1   congruent       12          11 0.9090909 448.6000    0.08333333
#> 2       1 incongruent       10           9 0.7777778 530.1429    0.10000000
#> 3       2   congruent       12          12 0.9166667 451.3636    0.00000000
#> 4       2 incongruent       12          12 0.9166667 550.2727    0.00000000

condition_contrasts(m)[, c("session", "contrast", "rt_ms", "err_rate")]
#>   session              contrast    rt_ms  err_rate
#> 1       1 incongruent_congruent 81.54286 0.1313131
#> 2       2 incongruent_congruent 98.90909 0.0000000

post_error_slowing(trials, "flanker")[, c("session", "n_errors", "n_triplets", "pes_ms")]
#>   session n_errors n_triplets pes_ms
#> 1       1        3          1     NA
#> 2       2        2          2   56.5
```

## Input

One row per trial, in the format the preprocessing pipeline exports
(`*_combined.csv`, one file per task):

| Column | Used for |
|---|---|
| `subject_id`, `session` | Grouping. |
| `condition` | The task's condition label, e.g. `congruent`. Checked against [`r/inst/tables/task_conditions.csv`](r/inst/tables/task_conditions.csv). |
| `correct_trial` | 1 or 0 (TRUE/FALSE and similar spellings also work). |
| `rt` | Reaction time in ms. Missing for a trial with no response. |
| `usable` | Optional. FALSE for a trial tagged upstream (too fast, a placeholder). It stays in the table, keeping its place in the sequence, but enters no measure. |
| `trial_seq`, `block_num` | Trial order (1 to n in each session) and blocks, for the sequential measures. |
| `trial_id` | Only `test_trial` rows are used. |
| `SSD` | Stop-signal delay in ms, for SSRT. |
| `response`, `spatial_sequence`, `rt_each_spatial_location_response_grid` | Span recall, as serialised lists such as `"[6, 5, 3, 15]"`. |

## What it computes

| Function | One row per subject × session |
|---|---|
| `condition_metrics(trials, task)` | per condition: `accuracy`, `err_rate`, `rt_ms`, `rt_var_ms2`, `omission_rate`, and the trial counts |
| `condition_contrasts(metrics)` | the study's 14 contrasts, each harder minus easier |
| `post_error_slowing(trials, task)` | `pes_ms`, on correct-error-correct triplets |
| `congruency_sequence_effect(trials, task)` | `cse_ms`, for flanker and Stroop |
| `post_stop_slowing(trials)` | `pss_ms`, for stop signal |
| `ssrt(trials)` | `ssrt_ms`, by the integration method |
| `span_metrics(trials, task)` | recall `accuracy` and `err_rate`, `response_interval_ms` |
| `span_contrast(operation, simple)` | operation-span minus simple-span error rate |
| `subject_means(x, min_sessions = 3)` | any of the above averaged over sessions |

**Accuracy** is choice accuracy, i.e. omissions do not count towards accuracy, except in Go/Nogo (go and nogo) and on Stop Signal stop trials, where accuracy is over all trials.

**RT** is the mean of correct responses, except Go/Nogo trials in the nogo condition, where
it is the mean of the commission errors. 

**Pooled cells.** N-back `1_back` and `2_back`, and visual search `feature`,
pool the trials of their set-size or match conditions.

**Contrasts:** AX-CPT AY−BY and BX−BY; flanker and Stroop incongruent−congruent;
n-back 2-back − 1-back; visual search conjunction 24 − conjunction 8 and
conjunction 24 − feature 24; task-switch and cue-switch costs in both
switching tasks; spatial cueing invalid − double cue, double cue − valid and no
cue − double cue.

## Sequential measures

- **Post-error slowing** uses correct-error-correct triplets and requires at least 2 per session. Omissions never count as errors. In go/no-go and stop signal the
  trials either side of an error must be go trials. (All except Span tasks)
- **The congruency sequence effect** gives a trial a cell
  only when its previous trial is usable and correct, and needs a trial in each
  of the four cells. (Flanker and Stroop) 
- **Post-stop slowing** counts a stop trial whose neighbours are
  usable go trials, and takes RT from the correct ones; each side needs 4. (Stop Signal) 

A trial with `usable = FALSE` keeps its place in the sequence: it cannot be a
neighbour, and the trials on either side of it are not treated as adjacent.
So pass every test trial, rather than dropping rows. Nothing spans a block.

## SSRT

`ssrt()` uses the integration method with replacement of go omissions
(Verbruggen et al., 2019): a missed go trial takes the session's slowest go RT.
No trials are filtered out by RT or SSD. 

## Subject level

`subject_means()` averages session values over each subject's sessions, and
gives `NA` for a subject with fewer than `min_sessions` sessions of that task
(3 by default). The study's pipeline exempts post-error slowing from that
minimum; pass `min_sessions = 1` for it.

## Layout

```
r/            the package; nothing else here is installed
                R/ source, man/ (generated), tests/, inst/tables/ (task facts),
                inst/examples/, inst/extdata/ssrt/ (SSRT reference cases)
tools/        the identifier scanner and its pre-commit hook (Python)
```

## Tests

```sh
make test      # the test suite
make check     # R CMD check --as-cran
make scan      # identifier scan of the tracked tree
```

The tests are R. Their expected values were worked out by hand, and the cases
are in `r/tests/testthat/cases/`.

