rm(list = ls())

library(readxl)
library(dplyr)
library(tibble)
library(ggplot2)
library(growthcurver)
library(writexl)
library(vegan)

out_dir <- file.path(getwd(), "Growth_PCA_Outputs_0to24h")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
message("Saving all outputs to: ", out_dir)

file_path <- "$Data Paper 1 - Growth Avgs Across Media.xlsx"
sheet_names <- excel_sheets(file_path)

data_list <- lapply(sheet_names, read_excel, path = file_path) %>%
  setNames(sheet_names)

AUM <- data_list[["AUM"]]
M9  <- data_list[["M9"]]
LB  <- data_list[["LB"]]

time_points_full <- c(0, 2, 4, 6, 8, 10, 24)
time_points_use <- time_points_full

run_medium_pipeline <- function(df, medium_name, time_points_keep, out_dir) {
  message("Processing medium: ", medium_name)
  
  df_t <- as.data.frame(t(df))
  real_names <- as.character(df_t[1, ])
  
  blanks <- c("Blank_1", "Blank_2", "Blank_3", "Blank_4")
  keep_cols <- !(real_names %in% blanks)
  df_t <- df_t[, keep_cols, drop = FALSE]
  real_names <- real_names[keep_cols]
  
  df_t <- df_t[-1, , drop = FALSE]
  colnames(df_t) <- make.unique(real_names, sep = "_")
  
  df_t <- df_t %>%
    dplyr::slice(c(dplyr::n(), seq_len(dplyr::n() - 1)))
  
  plate_vals <- as.character(df_t[1, ])
  new_names <- paste0(colnames(df_t), "_", plate_vals)
  df_t <- df_t[-1, ]
  colnames(df_t) <- new_names
  
  n_keep <- length(time_points_keep)
  
  df_t <- df_t %>%
    dplyr::slice(1:n_keep) %>%
    mutate(time = time_points_keep) %>%
    relocate(time) %>%
    mutate(across(everything(), as.numeric))
  
  t_file <- file.path(
    out_dir,
    paste0(medium_name, "_t_0to24h.xlsx")
  )
  
  write_xlsx(df_t, t_file)
  
  gc_med <- SummarizeGrowthByPlate(df_t)
  print(head(gc_med))
  
  notes <- gc_med %>%
    filter(note != "")
  
  if (nrow(notes) > 0) {
    message("Wells with notes (", medium_name, "):")
    print(notes)
  } else {
    message("No wells with notes for ", medium_name)
  }
  
  hist_file <- file.path(
    out_dir,
    paste0(medium_name, "_sigma_histogram_0to24h.png")
  )
  
  png(hist_file, width = 1200, height = 900, res = 150)
  hist(
    gc_med$sigma,
    main = paste0(medium_name, ": Histogram of σ (0–24 h fits)"),
    xlab = "σ"
  )
  dev.off()
  
  pca_gc_med <- as_tibble(gc_med)
  
  metric_cols <- pca_gc_med %>%
    dplyr::select(k:sigma)
  
  pca_med <- prcomp(
    metric_cols,
    center = TRUE,
    scale. = TRUE
  )
  
  message("PCA summary for ", medium_name, " (0–24 h fits):")
  print(summary(pca_med))
  
  top_sigma <- gc_med %>%
    dplyr::top_n(5, sigma) %>%
    dplyr::arrange(desc(sigma))
  
  message("Top 5 by σ for ", medium_name, " (0–24 h fits):")
  print(top_sigma)
  
  pca_scores_med <- as_tibble(
    pca_med$x,
    .name_repair = "minimal"
  ) %>%
    mutate(sample = pca_gc_med$sample) %>%
    relocate(sample)
  
  pca_plot_df <- pca_scores_med %>%
    select(sample, PC1, PC2)
  
  p_pca_med <- ggplot(pca_plot_df, aes(PC1, PC2)) +
    geom_point(shape = 16, size = 2.2, alpha = 0.9) +
    geom_text(aes(label = sample), size = 3, vjust = -0.6) +
    ggtitle(
      paste(
        "PCA of growth metrics —",
        medium_name,
        "(0–24 h)"
      )
    ) +
    theme_bw()
  
  print(p_pca_med)
  
  pca_png <- file.path(
    out_dir,
    paste0(medium_name, "_PCA_growth_metrics_0to24h.png")
  )
  
  ggsave(
    pca_png,
    p_pca_med,
    width = 8,
    height = 6,
    dpi = 300
  )
  
  pca_loadings_med <- as_tibble(
    pca_med$rotation,
    rownames = "metric"
  )
  
  export_list <- list(
    df_t,
    gc_med,
    pca_scores_med,
    pca_loadings_med
  )
  
  names(export_list) <- c(
    paste0(medium_name, "_t_0to24h"),
    paste0("gc_", medium_name, "_0to24h"),
    paste0("PCA_scores_", medium_name, "_0to24h"),
    paste0("PCA_loadings_", medium_name, "_0to24h")
  )
  
  out_xlsx <- file.path(
    out_dir,
    paste0(medium_name, "_growth_PCA_outputs_0to24h.xlsx")
  )
  
  write_xlsx(export_list, out_xlsx)
  
  list(
    reshaped = df_t,
    gc = gc_med,
    pca = pca_med,
    scores = pca_scores_med,
    loadings = pca_loadings_med
  )
}

res_LB  <- run_medium_pipeline(LB, "LB", time_points_use, out_dir)
res_AUM <- run_medium_pipeline(AUM, "AUM", time_points_use, out_dir)
res_M9  <- run_medium_pipeline(M9, "M9", time_points_use, out_dir)

LB_t   <- res_LB$reshaped
gc_LB  <- res_LB$gc
pca_LB <- res_LB$pca

AUM_t   <- res_AUM$reshaped
gc_AUM  <- res_AUM$gc
pca_AUM <- res_AUM$pca

M9_t   <- res_M9$reshaped
gc_M9  <- res_M9$gc
pca_M9 <- res_M9$pca

scores_LB <- as_tibble(
  pca_LB$x,
  .name_repair = "minimal"
) %>%
  mutate(sample = gc_LB$sample) %>%
  relocate(sample)

scores_AUM <- as_tibble(
  pca_AUM$x,
  .name_repair = "minimal"
) %>%
  mutate(sample = gc_AUM$sample) %>%
  relocate(sample)

scores_M9 <- as_tibble(
  pca_M9$x,
  .name_repair = "minimal"
) %>%
  mutate(sample = gc_M9$sample) %>%
  relocate(sample)

all_scores <- bind_rows(
  scores_LB %>% transmute(medium = "LB", PC1, PC2),
  scores_M9 %>% transmute(medium = "M9", PC1, PC2),
  scores_AUM %>% transmute(medium = "AUM", PC1, PC2)
)

x_range <- range(all_scores$PC1, na.rm = TRUE)
y_range <- range(all_scores$PC2, na.rm = TRUE)

pad <- 0.1

x_lim <- c(
  x_range[1] - pad * diff(x_range),
  x_range[2] + pad * diff(x_range)
)

y_lim <- c(
  y_range[1] - pad * diff(y_range),
  y_range[2] + pad * diff(y_range)
)

p_LB <- ggplot(scores_LB, aes(PC1, PC2)) +
  geom_point(shape = 16, size = 2.2, alpha = 0.9) +
  geom_text(aes(label = sample), size = 3, vjust = -0.6) +
  xlim(x_lim) +
  ylim(y_lim) +
  ggtitle("PCA of growth metrics — LB (0–24 h)") +
  theme_bw()

p_AUM <- ggplot(scores_AUM, aes(PC1, PC2)) +
  geom_point(shape = 16, size = 2.2, alpha = 0.9) +
  geom_text(aes(label = sample), size = 3, vjust = -0.6) +
  xlim(x_lim) +
  ylim(y_lim) +
  ggtitle("PCA of growth metrics — AUM (0–24 h)") +
  theme_bw()

p_M9 <- ggplot(scores_M9, aes(PC1, PC2)) +
  geom_point(shape = 16, size = 2.2, alpha = 0.9) +
  geom_text(aes(label = sample), size = 3, vjust = -0.6) +
  xlim(x_lim) +
  ylim(y_lim) +
  ggtitle("PCA of growth metrics — M9 (0–24 h)") +
  theme_bw()

print(p_LB)
print(p_AUM)
print(p_M9)

ggsave(
  file.path(out_dir, "LB_PCA_growth_metrics_0to24h_fixedaxes.png"),
  p_LB,
  width = 8,
  height = 6,
  dpi = 300
)

ggsave(
  file.path(out_dir, "AUM_PCA_growth_metrics_0to24h_fixedaxes.png"),
  p_AUM,
  width = 8,
  height = 6,
  dpi = 300
)

ggsave(
  file.path(out_dir, "M9_PCA_growth_metrics_0to24h_fixedaxes.png"),
  p_M9,
  width = 8,
  height = 6,
  dpi = 300
)

medium_cols <- c(
  "LB" = "#CC5500",
  "M9" = "#C8A2C8",
  "AUM" = "#008080"
)

gc_LB_all  <- gc_LB %>% mutate(medium = "LB")
gc_AUM_all <- gc_AUM %>% mutate(medium = "AUM")
gc_M9_all  <- gc_M9 %>% mutate(medium = "M9")

gc_all <- bind_rows(
  gc_LB_all,
  gc_M9_all,
  gc_AUM_all
) %>%
  mutate(
    medium = factor(
      medium,
      levels = c("LB", "M9", "AUM")
    )
  )

metric_cols <- gc_all %>%
  dplyr::select(k:sigma)

pca_all <- prcomp(
  metric_cols,
  center = TRUE,
  scale. = TRUE
)

scores_all <- as_tibble(
  pca_all$x,
  .name_repair = "minimal"
) %>%
  mutate(
    sample = gc_all$sample,
    medium = gc_all$medium
  ) %>%
  relocate(sample, medium) %>%
  mutate(
    medium = factor(
      medium,
      levels = c("LB", "M9", "AUM")
    )
  )

fmt_p <- function(p) {
  if (is.na(p)) {
    return("p = NA")
  }
  
  if (p < 0.001) {
    "p < 0.001"
  } else {
    paste0(
      "p = ",
      format(
        round(p, 3),
        nsmall = 3,
        trim = TRUE
      )
    )
  }
}

metric_matrix <- as.matrix(metric_cols)

set.seed(123)

permanova_res <- adonis2(
  metric_matrix ~ medium,
  data = gc_all,
  method = "euclidean",
  permutations = 999
)

dist_mat <- dist(
  metric_matrix,
  method = "euclidean"
)

bd <- betadisper(
  dist_mat,
  gc_all$medium
)

bd_anova <- anova(bd)

kw_PC1 <- kruskal.test(
  PC1 ~ medium,
  data = scores_all
)

kw_PC2 <- kruskal.test(
  PC2 ~ medium,
  data = scores_all
)

stats_label <- paste0(
  "PERMANOVA (k–σ): R² = ",
  round(permanova_res$R2[1], 3),
  ", F = ",
  round(permanova_res$F[1], 2),
  ", ",
  fmt_p(permanova_res$`Pr(>F)`[1]),
  "\n",
  "Dispersion (betadisper): F = ",
  round(bd_anova$`F value`[1], 2),
  ", ",
  fmt_p(bd_anova$`Pr(>F)`[1]),
  "\n",
  "Kruskal–Wallis PC1: χ² = ",
  round(kw_PC1$statistic, 1),
  ", ",
  fmt_p(kw_PC1$p.value),
  "\n",
  "Kruskal–Wallis PC2: χ² = ",
  round(kw_PC2$statistic, 1),
  ", ",
  fmt_p(kw_PC2$p.value)
)

cat("\n===== GLOBAL STATS =====\n")

cat("\nPERMANOVA:\n")
print(permanova_res)

cat("\nBETADISPER:\n")
print(bd_anova)

cat("\nKRUSKAL–WALLIS PC1:\n")
print(kw_PC1)

cat("\nKRUSKAL–WALLIS PC2:\n")
print(kw_PC2)

hull_df <- scores_all %>%
  mutate(
    medium = factor(
      medium,
      levels = c("LB", "M9", "AUM")
    )
  ) %>%
  group_by(medium) %>%
  slice(chull(PC1, PC2)) %>%
  ungroup()

p_global <- ggplot(
  scores_all,
  aes(PC1, PC2, color = medium)
) +
  geom_polygon(
    data = hull_df,
    aes(fill = medium),
    color = NA,
    alpha = 0.12,
    show.legend = FALSE
  ) +
  stat_ellipse(
    aes(group = medium),
    type = "norm",
    level = 0.95,
    linewidth = 0.6,
    alpha = 0.7
  ) +
  geom_point(
    shape = 16,
    size = 2.2,
    alpha = 0.9
  ) +
  scale_color_manual(
    values = medium_cols,
    breaks = c("LB", "M9", "AUM"),
    name = "Medium"
  ) +
  scale_fill_manual(
    values = medium_cols,
    breaks = c("LB", "M9", "AUM")
  ) +
  theme_bw(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5)
  )

print(p_global)

ggsave(
  filename = file.path(
    out_dir,
    "$Supplementary Figure 2 Growthcurver Metrics at 24h by Medium.png"
  ),
  plot = p_global,
  width = 9,
  height = 6.5,
  dpi = 300
)

message("Done. All outputs saved in: ", out_dir)