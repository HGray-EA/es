###############################################################################
# Original decision-tree code for all five ecosystem services.
# Each block is copied verbatim from the hackathon script and wrapped in a
# function that takes a data frame of indicators and returns the ES score.
# These are the reference ("truth") implementations for the validation tests.
###############################################################################

suppressPackageStartupMessages(library(dplyr))

# ============================== RECREATION ===================================
rc_original <- function(df) {
  rc_ind <- df %>% dplyr::select(r8, r11, r12, r14)
  rc_ind <- rc_ind %>%
    dplyr::mutate(r14r11rc = dplyr::case_when(
      is.na(r11) ~ r14,
      r14 == 1 ~ 1,
      r14 == 2 & r11 %in% c(1,2) ~ 1,
      r14 == 2 & r11 %in% c(3,4,5) ~ 2,
      r14 == 3 & r11 %in% c(1) ~ 1,
      r14 == 3 & r11 %in% c(2) ~ 2,
      r14 == 3 & r11 %in% c(3,4,5) ~ 3,
      r14 == 4 & r11 %in% c(1) ~ 2,
      r14 == 4 & r11 %in% c(2) ~ 3,
      r14 == 4 & r11 %in% c(3,4,5) ~ 4,
      r14 == 5 & r11 %in% c(1) ~ 3,
      r14 == 5 & r11 %in% c(2) ~ 4,
      r14 == 5 & r11 %in% c(3,4,5) ~ 5)) %>%
    dplyr::mutate(r12r11rc = dplyr::case_when(
      is.na(r11) ~ r12,
      r12 == 1 ~ 1,
      r12 == 2 & r11 %in% c(1) ~ 1,
      r12 == 2 & r11 %in% c(2,3,4,5) ~ 2,
      r12 == 3 & r11 %in% c(1) ~ 2,
      r12 == 3 & r11 %in% c(2,3,4,5) ~ 3,
      r12 == 4 & r11 %in% c(1) ~ 3,
      r12 == 4 & r11 %in% c(2,3,4,5) ~ 4,
      r12 == 5 & r11 %in% c(1) ~ 4,
      r12 == 5 & r11 %in% c(2,3,4,5) ~ 5))
  rc_ind <- rc_ind %>%
    dplyr::mutate(sub_score = dplyr::case_when(
      r14r11rc == 1 & r12r11rc %in% c(1) ~ 1,
      r14r11rc == 1 & r12r11rc %in% c(2,3) ~ 2,
      r14r11rc == 1 & r12r11rc %in% c(4,5) ~ 3,
      r14r11rc == 2 & r12r11rc %in% c(1,2) ~ 2,
      r14r11rc == 2 & r12r11rc %in% c(3,4) ~ 3,
      r14r11rc == 2 & r12r11rc %in% c(5) ~ 4,
      r14r11rc == 3 & r12r11rc %in% c(1) ~ 3,
      r14r11rc == 3 & r12r11rc %in% c(2,3) ~ 4,
      r14r11rc == 3 & r12r11rc %in% c(4,5) ~ 5,
      r14r11rc == 4 & r12r11rc %in% c(1,2) ~ 3,
      r14r11rc == 4 & r12r11rc %in% c(3,4) ~ 4,
      r14r11rc == 4 & r12r11rc %in% c(5) ~ 5,
      r14r11rc == 5 & r12r11rc %in% c(1) ~ 3,
      r14r11rc == 5 & r12r11rc %in% c(2,3) ~ 4,
      r14r11rc == 5 & r12r11rc %in% c(4,5) ~ 5))
  rc_ind <- rc_ind %>%
    dplyr::mutate(rc = dplyr::case_when(
      sub_score %in% c(1,5) ~ sub_score,
      sub_score %in% c(2,3,4) & r8 == "Yes" ~ sub_score + 1,
      sub_score %in% c(2,3,4) & r8 == "No" ~ sub_score))
  as.numeric(rc_ind$rc)
}

# ==================== HABITAT & POPULATION MAINTENANCE =======================
hm_original <- function(df) {
  hpm_ind <- df %>% dplyr::select(r1, r4, r19, r21)
  hpm_ind <- hpm_ind %>%
    dplyr::mutate(
      r4_orig = r4,
      r21_orig = r21,
      r4 = dplyr::coalesce(r4, r1),
      r21 = dplyr::coalesce(r21, r4),
      hm = dplyr::case_when(
        r1 %in% c(5,4) ~ dplyr::case_when(
          r4 %in% c(5)    &   is.na(r19)                              ~   r21,
          r4 %in% c(5)    &   r21 %in% c(5)      &   r19 %in% c(5,4)    ~   5,
          r4 %in% c(5)    &   r21 %in% c(5)      &   r19 %in% c(3)      ~   4,
          r4 %in% c(5)    &   r21 %in% c(5)      &   r19 %in% c(2,1)    ~   3,
          r4 %in% c(5)    &   r21 %in% c(4,3)    &   r19 %in% c(5,4,3)  ~   4,
          r4 %in% c(5)    &   r21 %in% c(4,3)    &   r19 %in% c(2,1)    ~   3,
          r4 %in% c(5)    &   r21 %in% c(2,1)    &   r19 %in% c(5,4,3)  ~   2,
          r4 %in% c(5)    &   r21 %in% c(2,1)    &   r19 %in% c(2,1)    ~   1,
          r4 %in% c(4,3)  &   is.na(r19)                              ~   r21,
          r4 %in% c(4,3)  &   r21 %in% c(5)      &   r19 %in% c(5,4,3)  ~   4,
          r4 %in% c(4,3)  &   r21 %in% c(5)      &   r19 %in% c(2,1)    ~   2,
          r4 %in% c(4,3)  &   r21 %in% c(4,3)    &   r19 %in% c(5,4,3)  ~   3,
          r4 %in% c(4,3)  &   r21 %in% c(4,3)    &   r19 %in% c(2,1)    ~   2,
          r4 %in% c(4,3)  &   r21 %in% c(2,1)    &   r19 %in% c(5,4,3)  ~   2,
          r4 %in% c(4,3)  &   r21 %in% c(2,1)    &   r19 %in% c(2,1)    ~   1,
          r4 %in% c(2,1)  &   is.na(r19)                              ~   r21,
          r4 %in% c(2,1)  &   r21 %in% c(5,4,3)  &   r19 %in% c(5,4,3)  ~   3,
          r4 %in% c(2,1)  &   r21 %in% c(5,4,3)  &   r19 %in% c(2,1)    ~   2,
          r4 %in% c(2,1)  &   r21 %in% c(2,1)    &   r19 %in% c(5,4,3)  ~   2,
          r4 %in% c(2,1)  &   r21 %in% c(2,1)    &   r19 %in% c(2,1)    ~   1,
          .default = NA
        ),
        r1 %in% c(3,2) ~ dplyr::case_when(
          r4 %in% c(5,4)  &   is.na(r19)                                ~   r21,
          r4 %in% c(5,4)  &   r21 %in% c(5,4)    &   r19 %in% c(5,4)    ~   4,
          r4 %in% c(5,4)  &   r21 %in% c(5,4)    &   r19 %in% c(3)      ~   3,
          r4 %in% c(5,4)  &   r21 %in% c(5,4)    &   r19 %in% c(2,1)    ~   2,
          r4 %in% c(5,4)  &   r21 %in% c(3)      &   r19 %in% c(5,4,3)  ~   3,
          r4 %in% c(5,4)  &   r21 %in% c(3)      &   r19 %in% c(2,1)    ~   2,
          r4 %in% c(5,4)  &   r21 %in% c(2,1)    &   r19 %in% c(5,4,3)  ~   2,
          r4 %in% c(5,4)  &   r21 %in% c(2,1)    &   r19 %in% c(2,1)    ~   1,
          r4 %in% c(3)    &   is.na(r19)                                ~   r21,
          r4 %in% c(3)    &   r21 %in% c(5,4,3)  &   r19 %in% c(5,4,3)  ~   3,
          r4 %in% c(3)    &   r21 %in% c(5,4,3)  &   r19 %in% c(1,2)    ~   2,
          r4 %in% c(3)    &   r21 %in% c(2,1)    &   r19 %in% c(5,4,3)  ~   2,
          r4 %in% c(3)    &   r21 %in% c(2,1)    &   r19 %in% c(2,1)    ~   1,
          r4 %in% c(2,1)  &   r21 %in% c(5,4,3)                         ~   2,
          r4 %in% c(2,1)  &   r21 %in% c(2,1)                           ~   1,
          .default = NA
        ),
        r1 %in% c(1) ~ 1
      )
    )
  as.numeric(hpm_ind$hm)
}

# ========================= WATER FLOW REGULATION =============================
wf_original <- function(df) {
  wf_ind <- df %>% dplyr::select(r5, r6, r25, r24, r28)
  wf_ind <- wf_ind %>%
    dplyr::mutate(
      wf_r6_calc = r6,
      wf_r25_calc = dplyr::case_when(
        is.na(r25) ~ 0,
        r25 == "Does Not Support Good" ~ -1,
        r25 == "Not assessed" ~ 0,
        r25 %in% c("Supports Good", "Not assessed") ~ 0,
        r25 == "High" ~ 1),
      wf_r28_calc = dplyr::case_when(
        r28 == "Red" ~ -1,
        r28 == "Grey" ~ 0,
        r28 == "Yellow" ~ 0,
        r28 == "Green" ~ 1))
  wf_ind <- wf_ind %>%
    dplyr::mutate(sub_score = dplyr::case_when(
      is.na(r5) ~ wf_r6_calc,
      is.na(wf_r6_calc) ~ r5,
      TRUE ~ floor((r5 + wf_r6_calc) / 2)))
  wf_ind$sub_score <- wf_ind$sub_score + wf_ind$wf_r25_calc
  wf_ind <- wf_ind %>%
    dplyr::mutate(sub_score = dplyr::case_when(
      is.na(sub_score) ~ NA,
      sub_score < 1 ~ 1,
      sub_score > 5 ~ 5,
      TRUE ~ sub_score))
  wf_ind <- dplyr::mutate(wf_ind, sub_score = dplyr::case_when(
    sub_score == 1 & r24 %in% c(1,2,3) ~ 1,
    sub_score == 1 & r24 %in% c(4) ~ 2,
    sub_score == 1 & r24 %in% c(5) ~ 3,
    sub_score == 2 & r24 %in% c(1,2) ~ 1,
    sub_score == 2 & r24 %in% c(3) ~ 2,
    sub_score == 2 & r24 %in% c(4) ~ 3,
    sub_score == 2 & r24 %in% c(5) ~ 4,
    sub_score == 3 & r24 %in% c(1) ~ 1,
    sub_score == 3 & r24 %in% c(2) ~ 2,
    sub_score == 3 & r24 %in% c(3) ~ 3,
    sub_score == 3 & r24 %in% c(4) ~ 4,
    sub_score == 3 & r24 %in% c(5) ~ 5,
    sub_score == 4 & r24 %in% c(1) ~ 2,
    sub_score == 4 & r24 %in% c(2) ~ 3,
    sub_score == 4 & r24 %in% c(3) ~ 4,
    sub_score == 4 & r24 %in% c(4,5) ~ 5,
    sub_score == 5 & r24 %in% c(1) ~ 2,
    sub_score == 5 & r24 %in% c(2) ~ 3,
    sub_score == 5 & r24 %in% c(3,4,5) ~ 5))
  wf_ind$sub_score <- wf_ind$sub_score + wf_ind$wf_r28_calc
  wf_ind <- wf_ind %>%
    dplyr::mutate(wf = dplyr::case_when(
      is.na(sub_score) ~ NA,
      sub_score < 1 ~ 1,
      sub_score > 5 ~ 5,
      TRUE ~ sub_score))
  as.numeric(wf_ind$wf)
}

# ======================= WATER QUALITY REGULATION ============================
wq_original <- function(df) {
  wq_ind <- df %>% dplyr::select(r3, r9, r10, r11)
  wq_ind <- wq_ind %>%
    dplyr::mutate(sub_score = dplyr::case_when(
      r3 == 0 | is.na(r3) ~ r9,
      is.na(r9) ~ r3,
      r3 == 1 ~ 1,
      r3 == 2 & r9 == 1 ~ 1,
      r3 == 2 & r9 == 2 ~ 2,
      r3 == 2 & r9 %in% c(3,4,5) ~ 3,
      r3 == 3 & r9 == 1 ~ 1,
      r3 == 3 & r9 == 2 ~ 2,
      r3 == 3 & r9 == 3 ~ 3,
      r3 == 3 & r9 %in% c(4,5) ~ 4,
      r3 == 4 & r9 %in% c(1,2) ~ 2,
      r3 == 4 & r9 == 3 ~ 3,
      r3 == 4 & r9 == 4 ~ 4,
      r3 == 4 & r9 == 5 ~ 5,
      r3 == 5 & r9 %in% c(1,2,3) ~ 3,
      r3 == 5 & r9 == 4 ~ 4,
      r3 == 5 & r9 == 5 ~ 5))
  wq_ind <- wq_ind %>%
    dplyr::mutate(wq_r10_calc = dplyr::case_when(
      r10 == "No data" ~ 0,
      r10 == "Shaded" ~ +1,
      r10 == "Medium shade" ~ 0,
      r10 == "No shade" ~ -1,
      TRUE ~ 0))
  wq_ind <- wq_ind %>%
    dplyr::mutate(sub_score = sub_score + wq_r10_calc) %>%
    dplyr::mutate(sub_score = dplyr::case_when(
      is.na(sub_score) ~ NA,
      sub_score > 5 ~ 5,
      sub_score < 1 ~ 1,
      TRUE ~ sub_score))
  wq_ind <- wq_ind %>%
    dplyr::mutate(wq = dplyr::case_when(
      is.na(r11) ~ sub_score,
      (sub_score == 1 & r11 %in% c(1,2)) |
        (sub_score == 2 & r11 %in% c(1)) ~ 1,
      (sub_score == 1 & r11 %in% c(3,4,5)) |
        (sub_score == 2 & r11 %in% c(2,3)) |
        (sub_score == 3 & r11 %in% c(1,2)) ~ 2,
      (sub_score == 2 & r11 %in% c(4,5)) |
        (sub_score == 3 & r11 %in% c(3)) |
        (sub_score == 4 & r11 %in% c(1,2)) ~ 3,
      (sub_score == 3 & r11 %in% c(4,5)) |
        (sub_score == 4 & r11 %in% c(3,4)) |
        (sub_score == 5 & r11 %in% c(1,2)) ~ 4,
      (sub_score == 4 & r11 %in% c(5)) |
        (sub_score == 5 & r11 %in% c(3,4,5)) ~ 5))
  as.numeric(wq_ind$wq)
}

# ============================== WATER SUPPLY =================================
ws_original <- function(df) {
  ws_ind <- df %>%
    dplyr::select(r26, r27_temp = r27) %>%
    dplyr::mutate(r27 = ifelse(grepl("No data", r27_temp), NA, r27_temp))
  ws_ind <- ws_ind %>%
    dplyr::mutate(ws = dplyr::case_when(
      is.na(r27) ~ r26,
      r27 %in% c(1) ~ r26 - 2,
      r27 %in% c(2,3) ~ r26 - 1,
      r27 %in% c(4,5) ~ r26))
  ws_ind <- ws_ind %>%
    dplyr::mutate(ws = dplyr::case_when(
      is.na(ws) ~ NA,
      ws < 1 ~ 1,
      ws > 5 ~ 5,
      TRUE ~ ws))
  as.numeric(ws_ind$ws)
}
