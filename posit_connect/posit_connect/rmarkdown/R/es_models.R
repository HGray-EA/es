###############################################################################
# Editable ("parametric") versions of the five decision trees, plus a spec
# for each ES describing its inputs.
#
# Each model is model(df, p = default_params(), steps = FALSE). Every rule
# value (lookup-table cells, +/- adjustments, leaf scores) lives in `p`, so a
# rule can be changed and its effect tested. tests/test_all.R checks that,
# with the default p, each model matches the *_original() code exactly.
###############################################################################

suppressPackageStartupMessages(library(dplyr))

clamp <- function(x, lo = 1, hi = 5) ifelse(is.na(x), NA_real_, pmin(hi, pmax(lo, x)))
lk <- function(tab, i, j) {
  out <- rep(NA_real_, length(i))
  ok <- !is.na(i) & !is.na(j) & i %in% 1:5 & j %in% 1:5
  out[ok] <- tab[cbind(i[ok], j[ok])]
  out
}
mat5 <- function(v, rn, cn) matrix(v, 5, byrow = TRUE,
  dimnames = setNames(list(as.character(1:5), as.character(1:5)), c(rn, cn)))
adj <- function(x, map, default = 0) {
  out <- unname(map[as.character(x)]); out[is.na(x)] <- default
  out[!is.na(x) & !(as.character(x) %in% names(map))] <- NA; as.numeric(out)
}

# =============================== RECREATION ==================================
rc_params <- function() list(
  T1 = mat5(c(1,1,1,1,1, 1,1,2,2,2, 1,2,3,3,3, 2,3,4,4,4, 3,4,5,5,5), "R14", "R11"),
  T2 = mat5(c(1,1,1,1,1, 1,2,2,2,2, 2,3,3,3,3, 3,4,4,4,4, 4,5,5,5,5), "R12", "R11"),
  T3 = mat5(c(1,2,2,3,3, 2,2,3,3,4, 3,4,4,5,5, 3,3,4,4,5, 3,4,4,5,5), "Step1", "Step2"),
  R8_bonus = c(Yes = 1))
rc_model <- function(df, p = rc_params(), steps = FALSE) {
  s1 <- ifelse(is.na(df$r11), df$r14, lk(p$T1, df$r14, df$r11))
  s2 <- ifelse(is.na(df$r11), df$r12, lk(p$T2, df$r12, df$r11))
  s3 <- lk(p$T3, s1, s2)
  y <- case_when(!(s3 %in% 2:4) ~ s3,
                 df$r8 == "Yes" ~ clamp(s3 + p$R8_bonus[["Yes"]]),
                 df$r8 == "No" ~ s3, TRUE ~ NA_real_)
  if (steps) return(data.frame(Step1 = s1, Step2 = s2, Step3 = s3, Final = y))
  y
}

# ==================== HABITAT & POPULATION MAINTENANCE =======================
# The nested case_when as an ordered rule table (first match wins).
# value = NA means "pass through R21" (the is.na(r19) rows).
hm_rule_table <- function() {
  r <- function(r1, r4, r21, r19, v) data.frame(r1 = r1, r4 = r4, r21 = r21, r19 = r19, value = v)
  rbind(
    r("4,5","5","*","NA",NA), r("4,5","5","5","4,5",5), r("4,5","5","5","3",4),
    r("4,5","5","5","1,2",3), r("4,5","5","3,4","3,4,5",4), r("4,5","5","3,4","1,2",3),
    r("4,5","5","1,2","3,4,5",2), r("4,5","5","1,2","1,2",1),
    r("4,5","3,4","*","NA",NA), r("4,5","3,4","5","3,4,5",4), r("4,5","3,4","5","1,2",2),
    r("4,5","3,4","3,4","3,4,5",3), r("4,5","3,4","3,4","1,2",2),
    r("4,5","3,4","1,2","3,4,5",2), r("4,5","3,4","1,2","1,2",1),
    r("4,5","1,2","*","NA",NA), r("4,5","1,2","3,4,5","3,4,5",3), r("4,5","1,2","3,4,5","1,2",2),
    r("4,5","1,2","1,2","3,4,5",2), r("4,5","1,2","1,2","1,2",1),
    r("2,3","4,5","*","NA",NA), r("2,3","4,5","4,5","4,5",4), r("2,3","4,5","4,5","3",3),
    r("2,3","4,5","4,5","1,2",2), r("2,3","4,5","3","3,4,5",3), r("2,3","4,5","3","1,2",2),
    r("2,3","4,5","1,2","3,4,5",2), r("2,3","4,5","1,2","1,2",1),
    r("2,3","3","*","NA",NA), r("2,3","3","3,4,5","3,4,5",3), r("2,3","3","3,4,5","1,2",2),
    r("2,3","3","1,2","3,4,5",2), r("2,3","3","1,2","1,2",1),
    r("2,3","1,2","3,4,5","*",2), r("2,3","1,2","1,2","*",1),
    r("1","*","*","*",1))
}
hm_params <- function() {
  rt <- hm_rule_table(); v <- rt$value[!is.na(rt$value)]
  names(v) <- with(rt[!is.na(rt$value), ], paste0("R1 ", r1, " | R4 ", r4, " | R21 ", r21, " | R19 ", r19))
  list(Leaves = v)
}
hm_model <- function(df, p = hm_params(), steps = FALSE) {
  rt <- hm_rule_table(); rt$value[!is.na(rt$value)] <- p$Leaves
  r1 <- df$r1; r4 <- coalesce(df$r4, r1); r21 <- coalesce(df$r21, r4); r19 <- df$r19
  inset <- function(x, s) if (s == "*") rep(TRUE, length(x)) else
    if (s == "NA") is.na(x) else x %in% as.numeric(strsplit(s, ",")[[1]])
  y <- rep(NA_real_, nrow(df)); done <- rep(FALSE, nrow(df))
  for (k in seq_len(nrow(rt))) {
    m <- !done & inset(r1, rt$r1[k]) & inset(r4, rt$r4[k]) & inset(r21, rt$r21[k]) & inset(r19, rt$r19[k])
    y[m] <- if (is.na(rt$value[k])) r21[m] else rt$value[k]; done <- done | m
  }
  if (steps) return(data.frame(Final = y))
  y
}

# ========================= WATER FLOW REGULATION =============================
wf_params <- function() list(
  R25_adj = c(`Does Not Support Good` = -1, `Not assessed` = 0, `Supports Good` = 0, High = 1),
  T4 = mat5(c(1,1,1,2,3, 1,1,2,3,4, 1,2,3,4,5, 2,3,4,5,5, 2,3,5,5,5), "Sub", "R24"),
  R28_adj = c(Red = -1, Grey = 0, Yellow = 0, Green = 1))
wf_model <- function(df, p = wf_params(), steps = FALSE) {
  s1 <- ifelse(is.na(df$r5), df$r6, ifelse(is.na(df$r6), df$r5, floor((df$r5 + df$r6) / 2)))
  s2 <- clamp(s1 + adj(df$r25, p$R25_adj, 0))
  s3 <- lk(p$T4, s2, df$r24)
  r28 <- unname(p$R28_adj[as.character(df$r28)]); r28[is.na(df$r28)] <- NA
  y <- clamp(s3 + r28)
  if (steps) return(data.frame(Connectivity = s1, PlusFlow = s2, PlusRunoff = s3, Final = y))
  y
}

# ======================= WATER QUALITY REGULATION ============================
wq_params <- function() list(
  T5 = mat5(c(1,1,1,1,1, 1,2,3,3,3, 1,2,3,4,4, 2,2,3,4,5, 3,3,3,4,5), "R3", "R9"),
  R10_adj = c(`No data` = 0, Shaded = 1, `Medium shade` = 0, `No shade` = -1),
  T6 = mat5(c(1,1,2,2,2, 1,2,2,3,3, 2,2,3,4,4, 3,3,4,4,5, 4,4,5,5,5), "Sub", "R11"))
wq_model <- function(df, p = wq_params(), steps = FALSE) {
  r3 <- df$r3; r9 <- df$r9
  s1 <- ifelse(is.na(r3) | r3 %in% 0, r9, ifelse(is.na(r9), r3, lk(p$T5, r3, r9)))
  a <- unname(p$R10_adj[as.character(df$r10)]); a[is.na(a)] <- 0
  s2 <- clamp(s1 + a)
  y <- ifelse(is.na(df$r11), s2, lk(p$T6, s2, df$r11))
  if (steps) return(data.frame(Riparian_x_LandUse = s1, PlusShade = s2, Final = y))
  y
}

# ============================== WATER SUPPLY =================================
ws_params <- function() list(R27_penalty = c(`1` = -2, `2` = -1, `3` = -1, `4` = 0, `5` = 0))
ws_model <- function(df, p = ws_params(), steps = FALSE) {
  pen <- unname(p$R27_penalty[as.character(df$r27)])
  y <- clamp(ifelse(is.na(df$r27), df$r26, df$r26 + pen))
  if (steps) return(data.frame(Final = y))
  y
}

# ================================ SPECS ======================================
# levels: values in the main grid, in "worse -> better" order (`order`).
# unordered: valid levels with no natural rank (in the grid, but not stepped).
# missing: how each input can be missing, compared to its worst level.
ES <- list(
  rc = list(name = "Recreation", original = rc_original, model = rc_model, params = rc_params,
    inputs = c(r8 = "R8 Natural beauty", r11 = "R11 Water quality", r12 = "R12 Local access", r14 = "R14 Rec. hotspots"),
    levels = list(r8 = c("No", "Yes"), r11 = 1:5, r12 = 1:5, r14 = 1:5),
    missing = list(r8 = NA, r11 = NA),
    hotspot = c(x = "r12", y = "r14", col = "r11", row = "r8")),
  hm = list(name = "Habitat & population maintenance", original = hm_original, model = hm_model, params = hm_params,
    inputs = c(r1 = "R1 RHQ reach index", r4 = "R4 Connectivity", r19 = "R19 WQ incl. pollutants", r21 = "R21 Hydrological condition"),
    levels = list(r1 = 1:5, r4 = 1:5, r19 = 1:5, r21 = 1:5),
    missing = list(r4 = NA, r19 = NA, r21 = NA),
    hotspot = c(x = "r19", y = "r21", col = "r4", row = "r1")),
  wf = list(name = "Water flow regulation", original = wf_original, model = wf_model, params = wf_params,
    inputs = c(r5 = "R5 Lateral connectivity", r6 = "R6 Vertical connectivity", r24 = "R24 Runoff infiltration",
               r25 = "R25 Flow naturalness", r28 = "R28 Abstraction licensing"),
    levels = list(r5 = 1:5, r6 = 1:5, r24 = 1:5,
                  r25 = c("Does Not Support Good", "Supports Good", "High"),
                  r28 = c("Red", "Yellow", "Green", "Grey")),
    unordered = list(r28 = "Grey"),
    missing = list(r5 = NA, r6 = NA, r24 = NA, r25 = "Not assessed"),
    hotspot = c(x = "r6", y = "r5", col = "r24", row = "r28")),
  wq = list(name = "Water quality regulation", original = wq_original, model = wq_model, params = wq_params,
    inputs = c(r3 = "R3 Riparian quality", r9 = "R9 Land use risk", r10 = "R10 River shade", r11 = "R11 Water quality"),
    levels = list(r3 = 1:5, r9 = 1:5, r10 = c("No shade", "Medium shade", "Shaded"), r11 = 1:5),
    missing = list(r3 = NA, r9 = NA, r10 = "No data", r11 = NA),
    hotspot = c(x = "r9", y = "r3", col = "r11", row = "r10")),
  ws = list(name = "Water supply", original = ws_original, model = ws_model, params = ws_params,
    inputs = c(r26 = "R26 Surface abstraction amount", r27 = "R27 Abstraction sustainability"),
    levels = list(r26 = 1:5, r27 = 1:5),
    missing = list(r27 = NA, r26 = NA),
    hotspot = c(x = "r27", y = "r26"))
)

es_grid <- function(spec) {
  g <- do.call(expand.grid, c(spec$levels, list(stringsAsFactors = FALSE, KEEP.OUT.ATTRS = FALSE)))
  g
}
# Grid that also includes each input's missing representation
es_grid_missing <- function(spec) {
  lv <- spec$levels
  for (v in names(spec$missing)) lv[[v]] <- c(lv[[v]], spec$missing[[v]])
  do.call(expand.grid, c(lv, list(stringsAsFactors = FALSE, KEEP.OUT.ATTRS = FALSE)))
}
# Ordered levels used for "one step better" moves
es_order <- function(spec, v) setdiff(spec$levels[[v]], spec$unordered[[v]])
