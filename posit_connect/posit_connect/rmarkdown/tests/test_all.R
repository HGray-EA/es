###############################################################################
# Validation tests for all five ES decision trees
# Run from the es_framework folder:  Rscript tests/test_all.R
#
# A. Equivalence  - the editable model == original script code (all inputs,
#                   including missing values and the script's dummy data)
# B. Validity     - every valid input gives a whole number 1..5
# C. Known values - hand-worked cases
# D. Missing data - documented NA behaviour
# Property checks (monotonicity, jumps, missing-data advantage) are reported
# by run_all.R; they flag design issues rather than code errors.
###############################################################################

suppressPackageStartupMessages({ library(testthat); library(dplyr) })
source("R/es_original.R"); source("R/es_models.R")

dummy_df <- function(n = 1000, seed = 1) {
  set.seed(seed)
  vars <- c("r1","r3","r4","r5","r6","r9","r11","r12","r14","r19","r21","r24","r26","r27")
  df <- as.data.frame(setNames(lapply(vars, function(x) round(runif(n, 1, 5))), vars))
  df$r8  <- sample(c("Yes", "No"), n, replace = TRUE)
  df$r10 <- sample(c("No data", "Shaded", "Medium shade", "No shade"), n, replace = TRUE)
  df$r25 <- sample(c("Does Not Support Good", "Not assessed", "Supports Good", "High"), n, replace = TRUE)
  df$r28 <- sample(c("Red", "Grey", "Yellow", "Green"), n, replace = TRUE)
  # sprinkle missing values
  for (v in c("r4","r5","r6","r9","r11","r19","r21","r27")) df[[v]][sample(n, n / 10)] <- NA
  df
}
same <- function(a, b) identical(as.numeric(a), as.numeric(b))

for (k in names(ES)) {
  s <- ES[[k]]
  test_that(paste0(s$name, " A1: editable model == original on full grid"), {
    g <- es_grid(s); expect_true(same(s$model(g), s$original(g))) })
  test_that(paste0(s$name, " A2: ... including missing values"), {
    g <- es_grid_missing(s); expect_true(same(s$model(g), s$original(g))) })
  test_that(paste0(s$name, " A3: ... on script-style dummy data with NAs"), {
    d <- dummy_df(); expect_true(same(s$model(d), s$original(d))) })
  test_that(paste0(s$name, " B1: every valid input scores a whole number 1..5"), {
    y <- s$original(es_grid(s)); expect_false(anyNA(y)); expect_true(all(y %in% 1:5)) })
}

test_that("C: hand-worked cases", {
  expect_equal(rc_original(data.frame(r8 = "Yes", r11 = 3, r12 = 1, r14 = 3)), 4)
  expect_equal(hm_original(data.frame(r1 = 5, r4 = 5, r19 = 5, r21 = 5)), 5)
  expect_equal(hm_original(data.frame(r1 = 3, r4 = 5, r19 = 5, r21 = 5)), 4)   # R1 2-3 caps at 4
  expect_equal(hm_original(data.frame(r1 = 1, r4 = 5, r19 = 5, r21 = 5)), 1)   # R1 = 1 veto
  expect_equal(wf_original(data.frame(r5 = 3, r6 = 4, r25 = "High", r24 = 3, r28 = "Green")), 5)
  expect_equal(wf_original(data.frame(r5 = 5, r6 = 5, r25 = "Supports Good", r24 = 3, r28 = "Yellow")), 5)
  expect_equal(wq_original(data.frame(r3 = 4, r9 = 3, r10 = "Shaded", r11 = 3)), 4)
  expect_equal(ws_original(data.frame(r26 = 4, r27 = 1)), 2)
})

test_that("D: missing-data behaviour (documented)", {
  expect_equal(hm_original(data.frame(r1 = 2, r4 = 4, r19 = NA, r21 = 5)), 5)  # NA R19 -> R21, beats the cap of 4
  expect_true(is.na(hm_original(data.frame(r1 = NA, r4 = 4, r19 = 3, r21 = 3))))
  expect_equal(wf_original(data.frame(r5 = NA, r6 = 4, r25 = NA, r24 = 3, r28 = "Grey")), 4)
  expect_true(is.na(wf_original(data.frame(r5 = 3, r6 = 3, r25 = "High", r24 = NA, r28 = "Grey"))))
  expect_equal(wq_original(data.frame(r3 = NA, r9 = 4, r10 = "No data", r11 = NA)), 4)
  expect_equal(ws_original(data.frame(r26 = 3, r27 = NA)), 3)
  expect_true(is.na(rc_original(data.frame(r8 = NA, r11 = 3, r12 = 3, r14 = 3))))
})
