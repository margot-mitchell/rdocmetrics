test_that("a contrast is the left cell minus the right, within each session", {
  trials <- rbind(
    trials_frame(c("congruent", "congruent", "incongruent", "incongruent"),
      c(1, 1, 1, 0), c(400, 500, 550, 600), session = 1),
    trials_frame(c("congruent", "congruent"), c(1, 1), c(420, 440), session = 2)
  )
  cc <- condition_contrasts(condition_metrics(trials, "flanker"))
  expect_equal(cc$contrast, c("incongruent_congruent", "incongruent_congruent"))
  # Session 1: RT 550 - 450 = 100; error rate 0.5 - 0.
  expect_equal(cc$rt_ms[1], 100)
  expect_equal(cc$err_rate[1], 0.5)
  # Session 2 has no incongruent trials, so no contrast.
  expect_true(is.na(cc$rt_ms[2]))
  expect_true(is.na(cc$err_rate[2]))
})

test_that("the contrast table has the study's 14 contrasts, harder minus easier", {
  specs <- read_task_table("contrasts")
  expect_equal(nrow(specs), 14L)
  expect_equal(
    paste(specs$task, specs$left, specs$right),
    c(
      "ax_cpt AY BY", "ax_cpt BX BY",
      "cued_task_switching task_switch_cue_switch task_stay_cue_switch",
      "cued_task_switching task_stay_cue_switch task_stay_cue_stay",
      "flanker incongruent congruent",
      "n_back 2_back 1_back",
      "spatial_cueing invalid doublecue", "spatial_cueing doublecue valid",
      "spatial_cueing nocue doublecue",
      "spatial_task_switching task_switch_cue_switch task_stay_cue_switch",
      "spatial_task_switching task_stay_cue_switch task_stay_cue_stay",
      "stroop incongruent congruent",
      "visual_search conjunction_24 conjunction_8",
      "visual_search conjunction_24 feature_24"
    )
  )
  # Every condition a contrast names is declared for its task.
  conds <- read_task_table("task_conditions")
  expect_true(all(paste(specs$task, specs$left) %in% paste(conds$task, conds$condition)))
  expect_true(all(paste(specs$task, specs$right) %in% paste(conds$task, conds$condition)))
})

test_that("n-back contrasts use the pooled cells, visual search the set-size cells", {
  trials <- trials_frame(
    c("match_1", "mismatch_1", "match_2", "mismatch_2", "feature_24"),
    c(1, 1, 1, 1, 1), c(500, 600, 700, 900, 400)
  )
  nb <- condition_contrasts(condition_metrics(trials[1:4, ], "n_back"))
  # 2-back pooled mean 800 minus 1-back pooled mean 550.
  expect_equal(nb$rt_ms, 250)
  vs <- trials_frame(c("feature_24", "feature_8", "conjunction_24"), c(1, 1, 1), c(500, 200, 900))
  vc <- condition_contrasts(condition_metrics(vs, "visual_search"))
  # conjunction_24 - feature_24 = 400, not minus the pooled feature cell (350).
  expect_equal(vc$rt_ms[vc$contrast == "conjunction_24_feature_24"], 400)
})

test_that("metrics for several tasks give each task's contrasts", {
  fl <- condition_metrics(trials_frame(c("congruent", "incongruent"), c(1, 1), c(400, 500)), "flanker")
  st <- condition_metrics(trials_frame(c("congruent", "incongruent"), c(1, 1), c(600, 750)), "stroop")
  cc <- condition_contrasts(rbind(fl, st))
  expect_equal(cc$task, c("flanker", "stroop"))
  expect_equal(cc$rt_ms, c(100, 150))
  expect_error(condition_contrasts(data.frame(x = 1)), "condition_metrics")
})
