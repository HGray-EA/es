###############################################################################
# Generic sensitivity, property and comparison toolkit for any ES spec.
# Every function enumerates the full input grid, so results are exact.
# Optional `w`: list of per-level weights per input (e.g. real RSN frequencies).
###############################################################################

suppressPackageStartupMessages({ library(dplyr); library(tidyr) })

grid_weights <- function(g, w = NULL) {
  if (is.null(w)) return(rep(1 / nrow(g), nrow(g)))
  p <- rep(1, nrow(g))
  for (v in names(w)) { wv <- w[[v]]; p <- p * as.numeric(wv[as.character(g[[v]])]) }
  p[is.na(p)] <- 0; p / sum(p)
}
# Weights matching the script's dummy data: round(runif(n, 1, 5)) for 1-5 inputs
dummy_weights <- function(spec) {
  lv <- c(`1` = .125, `2` = .25, `3` = .25, `4` = .25, `5` = .125)
  lapply(spec$levels, function(l) if (is.numeric(l)) lv else setNames(rep(1 / length(l), length(l)), l))
}
wvar <- function(x, p) { m <- sum(p * x); sum(p * (x - m)^2) }
cond_mean <- function(y, p, key) { k <- interaction(key, drop = TRUE)
  m <- tapply(y * p, k, sum) / tapply(p, k, sum); as.numeric(m[k]) }

# ---- Sobol indices (first-order, total, pairwise) --------------------------
sobol_exact <- function(spec, g, y, w = NULL) {
  ins <- names(spec$inputs); p <- grid_weights(g, w); V <- wvar(y, p)
  S1 <- sapply(ins, function(v) wvar(cond_mean(y, p, g[v]), p) / V)
  ST <- sapply(ins, function(v) if (length(ins) == 1) 1 else
    1 - wvar(cond_mean(y, p, g[setdiff(ins, v)]), p) / V)
  pr <- if (length(ins) > 1) combn(ins, 2) else matrix(nrow = 2, ncol = 0)
  S2 <- apply(pr, 2, function(q) wvar(cond_mean(y, p, g[q]), p) / V - S1[q[1]] - S1[q[2]])
  list(indices = data.frame(input = ins, first_order = S1, total = ST, interaction = pmax(0, ST - S1)),
       pairs = data.frame(a = pr[1, ], b = pr[2, ], S2 = pmax(0, unlist(S2))))
}

# ---- One-step moves --------------------------------------------------------
step_move <- function(spec, g, v, dir = 1) {
  ord <- es_order(spec, v); pos <- match(as.character(g[[v]]), as.character(ord))
  np <- pos + dir; ok <- !is.na(pos) & np >= 1 & np <= length(ord)
  h <- g; h[[v]][ok] <- ord[np[ok]]
  list(ok = ok, h = h, from = as.character(ord[pos]), to = as.character(ord[np]))
}

flip_rates <- function(spec, f, g, w = NULL) {
  p_all <- grid_weights(g, w); y0 <- f(g)
  bind_rows(lapply(names(spec$inputs), function(v) {
    s <- step_move(spec, g, v, 1); d <- f(s$h[s$ok, ]) - y0[s$ok]
    data.frame(input = v, transition = paste0(s$from[s$ok], "->", s$to[s$ok]),
               step = match(s$from[s$ok], as.character(es_order(spec, v))), d = d, p = p_all[s$ok])
  })) %>% group_by(input, step, transition) %>%
    summarise(flip_rate = sum(p * (d != 0)) / sum(p), mean_change = sum(p * d) / sum(p),
              down_rate = sum(p * (d < 0)) / sum(p), big_jump_rate = sum(p * (abs(d) >= 2)) / sum(p),
              .groups = "drop") %>% arrange(input, step)
}

local_hotspots <- function(spec, f, g) {
  y0 <- f(g); n <- nrow(g)
  nn <- nc <- mj <- pv <- numeric(n)
  for (v in names(spec$inputs)) for (dir in c(-1, 1)) {
    s <- step_move(spec, g, v, dir); d <- rep(0, n)
    d[s$ok] <- f(s$h[s$ok, ]) - y0[s$ok]
    nn <- nn + s$ok; nc <- nc + (s$ok & d != 0); mj <- pmax(mj, abs(d))
    pv <- pv + (s$ok & dir == 1 & d < 0)
  }
  cbind(g, score = y0, neighbours = nn, n_change = nc, change_share = ifelse(nn > 0, nc / nn, 0),
        max_jump = mj, perverse = pv)
}

# Mean local sensitivity for every pair of inputs (averaged over the others)
pair_hotspots <- function(spec, hs) {
  ins <- names(spec$inputs); if (length(ins) < 2) return(data.frame())
  pr <- combn(ins, 2)
  bind_rows(apply(pr, 2, function(q) {
    hs %>% group_by(xv = as.character(.data[[q[1]]]), yv = as.character(.data[[q[2]]])) %>%
      summarise(sens = mean(change_share), score = mean(score), perverse = sum(perverse > 0),
                .groups = "drop") %>% mutate(a = q[1], b = q[2])
  }))
}

# ---- Rule criticality: nudge each rule value by +/-1 -----------------------
rule_criticality <- function(spec, g) {
  p0 <- spec$params(); y0 <- spec$model(g, p0); out <- list()
  for (pn in names(p0)) {
    P <- p0[[pn]]
    is_score <- is.matrix(P) || pn == "Leaves"
    for (k in seq_along(P)) {
      ch <- sapply(c(-1, 1), function(dl) {
        p1 <- p0; nv <- P[k] + dl; if (is_score) nv <- min(5, max(1, nv))
        if (nv == P[k]) return(0)
        p1[[pn]][k] <- nv; mean(spec$model(g, p1) != y0, na.rm = TRUE) })
      if (is.matrix(P)) { ij <- arrayInd(k, dim(P)); lab <- sprintf("%s[%s=%d, %s=%d]", pn,
        names(dimnames(P))[1], ij[1], names(dimnames(P))[2], ij[2]); r <- ij[1]; cc <- ij[2] }
      else { lab <- names(P)[k]; r <- k; cc <- NA }
      out[[length(out) + 1]] <- data.frame(param = pn, kind = ifelse(is.matrix(P), "matrix", "vector"),
        row = r, col = cc, label = lab, value = as.numeric(P[k]), change_share = max(ch))
    }
  }
  bind_rows(out)
}

# ---- Correlations of inputs, steps, final ----------------------------------
step_correlations <- function(spec, g) {
  st <- spec$model(g, steps = TRUE)
  num <- as.data.frame(lapply(names(spec$inputs), function(v) {
    ord <- es_order(spec, v); match(as.character(g[[v]]), as.character(ord)) }))
  names(num) <- toupper(names(spec$inputs))
  m <- cbind(num, st)
  list(names = names(m), r = suppressWarnings(cor(m, method = "spearman", use = "pairwise.complete.obs")))
}

score_distribution <- function(g, y, w = NULL) { p <- grid_weights(g, w); sapply(1:5, function(k) sum(p[y %in% k])) }

# ---- Property checks -------------------------------------------------------
property_checks <- function(spec, f) {
  g <- es_grid(spec); y <- f(g)
  viol <- list(); big <- 0; tot <- 0
  for (v in names(spec$inputs)) {
    s <- step_move(spec, g, v, 1); d <- f(s$h[s$ok, ]) - y[s$ok]
    tot <- tot + length(d); big <- big + sum(abs(d) >= 2)
    if (any(d < 0)) { b <- g[s$ok, ][d < 0, , drop = FALSE]
      viol[[v]] <- cbind(b, changed = v, step = paste0(s$from[s$ok][d < 0], "->", s$to[s$ok][d < 0]),
                         from = y[s$ok][d < 0], to = (y[s$ok] + d)[d < 0]) }
  }
  mv <- if (length(viol)) bind_rows(lapply(viol, function(x) { x[] <- lapply(x, as.character); x })) else data.frame()
  miss <- bind_rows(lapply(names(spec$missing), function(v) {
    gm <- g; gm[[v]] <- spec$missing[[v]]; gw <- g; gw[[v]] <- es_order(spec, v)[1]
    ym <- f(gm); yw <- f(gw)
    data.frame(input = v, missing_as = ifelse(is.na(spec$missing[[v]]), "NA", spec$missing[[v]]),
               result_na = mean(is.na(ym)), beats_worst = mean(ym > yw, na.rm = TRUE),
               beats_best = { gb <- g; gb[[v]] <- tail(es_order(spec, v), 1); mean(ym > f(gb), na.rm = TRUE) })
  }))
  na_pass <- miss$beats_worst[miss$result_na < 1]
  summary <- data.frame(
    property = c("Monotonic in every input", "No single step moves the score by 2+",
                 "Uses all five score levels", "Missing data never beats the worst value"),
    result = c(ifelse(nrow(mv) == 0, "PASS", sprintf("FLAG - %d violations", nrow(mv))),
               ifelse(big == 0, "PASS", sprintf("FLAG - %d of %d steps", big, tot)),
               ifelse(all(1:5 %in% y), "PASS", sprintf("FLAG - only %s reachable", paste(sort(unique(y)), collapse = ","))),
               ifelse(length(na_pass) == 0 || all(na_pass == 0), "PASS",
                      sprintf("FLAG - up to %.0f%% of cases", 100 * max(na_pass)))))
  list(summary = summary, monotonic_violations = mv, missing = miss, big_jumps = big, steps = tot)
}

# ---- Everything for one ES -------------------------------------------------
analyse_es <- function(spec) {
  g <- es_grid(spec); f <- spec$original; y <- f(g)
  hs <- local_hotspots(spec, f, g)
  list(name = spec$name, inputs = as.list(spec$inputs),
       levels = lapply(spec$levels, as.character), order = lapply(names(spec$levels), function(v) as.character(es_order(spec, v))),
       hotspot_layout = as.list(spec$hotspot),
       n = nrow(g),
       sobol = sobol_exact(spec, g, y), sobol_dummy = sobol_exact(spec, g, y, dummy_weights(spec)),
       flips = flip_rates(spec, f, g), hotspots = hs, pairs = pair_hotspots(spec, hs),
       rules = rule_criticality(spec, g), corr = step_correlations(spec, g),
       dist = score_distribution(g, y), dist_dummy = score_distribution(g, y, dummy_weights(spec)),
       properties = property_checks(spec, f))
}
