###############################################################################
# Full sensitivity analysis for all five ES decision trees
# Run from the es_framework folder:  Rscript run_all.R
# Outputs: outputs/<es>/*.csv, outputs/plots/*.png, outputs/results.json
###############################################################################

suppressPackageStartupMessages({ library(dplyr); library(tidyr); library(ggplot2); library(jsonlite) })
source("R/es_original.R"); source("R/es_models.R"); source("R/es_sensitivity.R")
dir.create("outputs/plots", recursive = TRUE, showWarnings = FALSE)

res <- list()
for (k in names(ES)) {
  cat("Analysing", ES[[k]]$name, "...\n")
  r <- analyse_es(ES[[k]]); res[[k]] <- r
  d <- file.path("outputs", k); dir.create(d, showWarnings = FALSE)
  write.csv(r$sobol$indices, file.path(d, "sobol_indices.csv"), row.names = FALSE)
  write.csv(r$sobol$pairs, file.path(d, "sobol_pairwise.csv"), row.names = FALSE)
  write.csv(r$flips, file.path(d, "flip_rates.csv"), row.names = FALSE)
  write.csv(r$hotspots, file.path(d, "local_hotspots.csv"), row.names = FALSE)
  write.csv(r$rules, file.path(d, "rule_criticality.csv"), row.names = FALSE)
  write.csv(r$properties$monotonic_violations, file.path(d, "monotonic_violations.csv"), row.names = FALSE)
  write.csv(r$properties$missing, file.path(d, "missing_data.csv"), row.names = FALSE)
}

# ---- Cross-ES comparison ----------------------------------------------------
ent <- function(p) { p <- p[p > 0]; -sum(p * log(p)) / log(5) }
compare <- bind_rows(lapply(names(res), function(k) { r <- res[[k]]; s <- r$sobol$indices
  data.frame(es = k, name = r$name, n_inputs = length(r$inputs), combinations = r$n,
    top_driver = s$input[which.max(s$total)], top_total = max(s$total),
    interaction_share = 1 - sum(s$first_order),
    mean_sensitivity = mean(r$hotspots$change_share),
    monotonic_violations = nrow(r$properties$monotonic_violations),
    big_jump_rate = r$properties$big_jumps / r$properties$steps,
    dead_steps = sum(r$flips$flip_rate == 0), total_steps = nrow(r$flips),
    extremes = r$dist[1] + r$dist[5], evenness = ent(r$dist), mean_score = sum((1:5) * r$dist)) }))
write.csv(compare, "outputs/es_comparison.csv", row.names = FALSE)

# Shared indicator R11 links Recreation and Water quality: joint correlation
gr <- es_grid(ES$rc); gq <- es_grid(ES$wq)
joint <- inner_join(mutate(gr, rc = rc_original(gr)), mutate(gq, wq = wq_original(gq)),
                    by = "r11", relationship = "many-to-many")
shared <- list(rho = cor(joint$rc, joint$wq, method = "spearman"),
               by_r11 = joint %>% group_by(r11) %>% summarise(rc = mean(rc), wq = mean(wq)))

write_json(list(es = res, compare = compare, shared_r11 = shared), "outputs/results.json",
           digits = 4, auto_unbox = TRUE, dataframe = "rows", na = "null")

# ---- Static plots (for slides / RStudio) ------------------------------------
sob <- bind_rows(lapply(names(res), function(k) mutate(res[[k]]$sobol$indices,
  es = res[[k]]$name, label = unlist(res[[k]]$inputs)[input])))
p <- ggplot(sob, aes(total, reorder(label, total))) + geom_col(fill = "#2a6fdb", width = .6) +
  geom_col(aes(first_order), fill = "#0b3d91", width = .6) + facet_wrap(~es, scales = "free_y", ncol = 2) +
  scale_x_continuous(labels = scales::percent) + labs(x = "Share of variance (dark = on its own, light = incl. interactions)", y = NULL,
  title = "Which indicators drive each ES score (Sobol indices)") + theme_minimal(base_size = 10)
ggsave("outputs/plots/sobol_all_es.png", p, width = 10, height = 7, dpi = 150)

for (k in names(res)) {
  pr <- res[[k]]$pairs; if (!nrow(pr)) next
  lab <- unlist(res[[k]]$inputs)
  pp <- ggplot(pr, aes(xv, yv, fill = sens)) + geom_tile(colour = "white") +
    geom_text(aes(label = round(100 * sens)), size = 2.6) +
    facet_wrap(~paste(lab[b], "(y) vs", lab[a], "(x)"), scales = "free") +
    scale_fill_gradient(low = "#eef3fb", high = "#0b3d91", labels = scales::percent, name = "Sensitivity") +
    labs(x = NULL, y = NULL, title = paste(res[[k]]$name, "- sensitivity hotspots for each pair of indicators")) +
    theme_minimal(base_size = 9) + theme(panel.grid = element_blank(), axis.text.x = element_text(angle = 30, hjust = 1))
  ggsave(sprintf("outputs/plots/hotspots_pairs_%s.png", k), pp, width = 11, height = 7, dpi = 140)
}

cat("\nDone.\n"); print(compare[, c("name","top_driver","top_total","interaction_share","monotonic_violations","big_jump_rate","dead_steps","extremes","mean_score")])

# ---- Interactive HTML report ------------------------------------------------
source("report/build_report.R"); build_report()
