rm(list = ls())

library(readxl)
library(dplyr)
library(tibble)
library(tidyr)
library(ggplot2)
library(growthcurver)
library(writexl)
library(vegan)

file_path <- "$Data Paper 1 - Growth Avgs Across Media.xlsx"
sheet_names <- excel_sheets(file_path)

data_list <- lapply(sheet_names, read_excel, path = file_path) %>%
  setNames(sheet_names)

LB  <- data_list[["LB"]]
M9  <- data_list[["M9"]]
AUM <- data_list[["AUM"]]

time_points_full <- c(0, 2, 4, 6, 8, 10, 24)
time_points_10 <- time_points_full[time_points_full <= 10]

run_medium_pipeline <- function(df, medium_name, time_points_keep) {
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
  
  t_file <- paste0(medium_name, "_t_0to10h.xlsx")
  write_xlsx(df_t, t_file)
  
  gc_med <- SummarizeGrowthByPlate(df_t)
  print(head(gc_med))
  
  notes <- gc_med %>% filter(note != "")
  
  if (nrow(notes) > 0) {
    message("Wells with notes (", medium_name, "):")
    print(notes)
  } else {
    message("No wells with notes for ", medium_name)
  }
  
  hist_file <- paste0(medium_name, "_sigma_histogram_0to10h.png")
  png(hist_file, width = 1200, height = 900, res = 150)
  hist(
    gc_med$sigma,
    main = paste0(medium_name, ": Histogram of σ (0–10 h fits)"),
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
  
  message("PCA summary for ", medium_name, " (0–10 h fits):")
  print(summary(pca_med))
  
  top_sigma <- gc_med %>%
    slice_max(order_by = sigma, n = 5) %>%
    arrange(desc(sigma))
  
  message("Top 5 by σ for ", medium_name, " (0–10 h fits):")
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
    ggtitle(paste("PCA of growth metrics —", medium_name, "(0–10 h)")) +
    theme_bw()
  
  print(p_pca_med)
  
  pca_png <- paste0(medium_name, "_PCA_growth_metrics_0to10h.png")
  ggsave(pca_png, p_pca_med, width = 8, height = 6, dpi = 300)
  
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
    paste0(medium_name, "_t_0to10h"),
    paste0("gc_", medium_name, "_0to10h"),
    paste0("PCA_scores_", medium_name, "_0to10h"),
    paste0("PCA_loadings_", medium_name, "_0to10h")
  )
  
  out_xlsx <- paste0(
    medium_name,
    "_growth_PCA_outputs_0to10h.xlsx"
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

res_LB  <- run_medium_pipeline(LB, "LB", time_points_10)
res_M9  <- run_medium_pipeline(M9, "M9", time_points_10)
res_AUM <- run_medium_pipeline(AUM, "AUM", time_points_10)

LB_t   <- res_LB$reshaped
gc_LB  <- res_LB$gc
pca_LB <- res_LB$pca

M9_t   <- res_M9$reshaped
gc_M9  <- res_M9$gc
pca_M9 <- res_M9$pca

AUM_t   <- res_AUM$reshaped
gc_AUM  <- res_AUM$gc
pca_AUM <- res_AUM$pca

medium_cols <- c(
  "LB" = "#CC5500",
  "M9" = "#C8A2C8",
  "AUM" = "#008080"
)

gc_LB_all  <- gc_LB %>% mutate(medium = "LB")
gc_M9_all  <- gc_M9 %>% mutate(medium = "M9")
gc_AUM_all <- gc_AUM %>% mutate(medium = "AUM")

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
  relocate(sample, medium)

metric_matrix <- as.matrix(metric_cols)

set.seed(123)

permanova_res <- adonis2(
  metric_matrix ~ medium,
  data = gc_all,
  method = "euclidean",
  permutations = 999
)

dist_mat <- dist(metric_matrix, method = "euclidean")
bd <- betadisper(dist_mat, gc_all$medium)
bd_anova <- anova(bd)

kw_PC1 <- kruskal.test(PC1 ~ medium, data = scores_all)
kw_PC2 <- kruskal.test(PC2 ~ medium, data = scores_all)

bf_norm_path <- "$Data Paper 1 - Normalized Biofilm Across Media.xlsx"
bf_norm_raw <- read_excel(bf_norm_path)

bf_norm_long <- bf_norm_raw %>%
  dplyr::select(
    Isolate = Strain,
    LB = `Normalized LB`,
    M9 = `Normalized M9`,
    AUM = `Normalized AUM`
  ) %>%
  pivot_longer(
    cols = c("LB", "M9", "AUM"),
    names_to = "medium",
    values_to = "biofilm_norm"
  ) %>%
  mutate(
    medium = factor(
      medium,
      levels = c("LB", "M9", "AUM")
    )
  )

bf_bin_path <- "Binary Biofilm.xlsx"
bf_bin_raw <- read_excel(bf_bin_path)

bf_bin_long <- bf_bin_raw %>%
  dplyr::select(
    Isolate = Strain,
    LB,
    M9,
    AUM
  ) %>%
  pivot_longer(
    cols = c("LB", "M9", "AUM"),
    names_to = "medium",
    values_to = "biofilm_bin"
  ) %>%
  mutate(
    medium = factor(
      medium,
      levels = c("LB", "M9", "AUM")
    ),
    biofilm_bin = as.character(biofilm_bin),
    biofilm_bin = case_when(
      biofilm_bin %in% c("0", "Low", "low") ~ "Low",
      biofilm_bin %in% c("1", "High", "high") ~ "High",
      TRUE ~ biofilm_bin
    ),
    biofilm_bin = factor(
      biofilm_bin,
      levels = c("Low", "High")
    )
  )

scores_all_bio <- scores_all %>%
  mutate(
    sample_key = sub("_.*$", "", sample)
  ) %>%
  left_join(
    bf_norm_long,
    by = c(
      "sample_key" = "Isolate",
      "medium" = "medium"
    )
  ) %>%
  left_join(
    bf_bin_long,
    by = c(
      "sample_key" = "Isolate",
      "medium" = "medium"
    )
  )

cat("\nMerged PCA + biofilm (first rows):\n")
print(head(scores_all_bio))

loadings_all <- as_tibble(
  pca_all$rotation,
  rownames = "metric"
)

scores_all_out <- scores_all

global_pca_export <- list(
  PC_scores = scores_all_out,
  PC_scores_with_bio = scores_all_bio,
  PC_loadings = loadings_all
)

write_xlsx(
  global_pca_export,
  path = "Global_growth_PCA_scores_loadings_0to10h.xlsx"
)

var_exp <- (
  pca_all$sdev^2 /
    sum(pca_all$sdev^2)
) * 100

pc1_lab <- paste0(
  "PC1 (",
  signif(var_exp[1], 3),
  "%)"
)

pc2_lab <- paste0(
  "PC2 (",
  signif(var_exp[2], 3),
  "%)"
)

df_bin <- scores_all_bio %>%
  filter(!is.na(biofilm_bin))

if (nlevels(df_bin$biofilm_bin) == 2) {
  w_PC1 <- wilcox.test(
    PC1 ~ biofilm_bin,
    data = df_bin
  )
  
  w_PC2 <- wilcox.test(
    PC2 ~ biofilm_bin,
    data = df_bin
  )
} else {
  w_PC1 <- kruskal.test(
    PC1 ~ biofilm_bin,
    data = df_bin
  )
  
  w_PC2 <- kruskal.test(
    PC2 ~ biofilm_bin,
    data = df_bin
  )
}

xr <- range(df_bin$PC1, na.rm = TRUE)
yr <- range(df_bin$PC2, na.rm = TRUE)

annot_x <- mean(xr)
annot_y <- yr[1] - 0.07 * diff(yr)

annot_x2 <- xr[1] - 0.12 * diff(xr)
annot_y2 <- mean(yr)

fmt_p <- function(p) {
  ifelse(
    p < 0.001,
    "< 0.001",
    sprintf("%.3f", p)
  )
}

txt_PC1 <- paste0(
  "Wilcoxon PC1 p = ",
  fmt_p(w_PC1$p.value)
)

txt_PC2 <- paste0(
  "Wilcoxon PC2 p = ",
  fmt_p(w_PC2$p.value)
)

p_pca_bio_bin <- ggplot() +
  geom_point(
    data = df_bin,
    aes(
      x = PC1,
      y = PC2,
      fill = medium,
      color = medium,
      alpha = biofilm_bin
    ),
    shape = 21,
    size = 3,
    stroke = 0
  ) +
  annotate(
    "text",
    x = annot_x,
    y = annot_y,
    label = txt_PC1,
    size = 4.2,
    hjust = 0.5
  ) +
  annotate(
    "text",
    x = annot_x2,
    y = annot_y2,
    label = txt_PC2,
    size = 4.2,
    angle = 90,
    vjust = 0.5
  ) +
  scale_fill_manual(
    values = medium_cols,
    name = "Medium"
  ) +
  scale_color_manual(
    values = medium_cols,
    guide = "none"
  ) +
  scale_alpha_manual(
    values = c(
      "Low" = 0.45,
      "High" = 1
    ),
    name = "Biofilm"
  ) +
  guides(
    fill = guide_legend(
      order = 1,
      override.aes = list(
        shape = 21,
        size = 3.5,
        alpha = 1,
        stroke = 0
      )
    ),
    alpha = guide_legend(
      order = 2,
      override.aes = list(
        shape = 21,
        size = 3.5,
        fill = "grey40",
        color = "grey40",
        stroke = 0
      )
    )
  ) +
  labs(
    x = pc1_lab,
    y = pc2_lab
  ) +
  theme_bw(base_size = 12) +
  theme(
    plot.title = element_text(hjust = 0.5),
    panel.border = element_rect(
      color = "black",
      linewidth = 0.7
    )
  )

print(p_pca_bio_bin)

ggsave(
  filename = "Global_PCA_growth_0to10h_with_binary_biofilm_Wilcoxon_on_figure_no_ellipses_fixed_legend.png",
  plot = p_pca_bio_bin,
  width = 8,
  height = 6,
  dpi = 300
)

df_cont <- scores_all_bio %>%
  filter(!is.na(biofilm_norm))

spearman_PC1 <- cor.test(
  df_cont$PC1,
  df_cont$biofilm_norm,
  method = "spearman",
  use = "complete.obs"
)

spearman_PC2 <- cor.test(
  df_cont$PC2,
  df_cont$biofilm_norm,
  method = "spearman",
  use = "complete.obs"
)

cat("\nSpearman PC1 vs normalized biofilm:\n")
print(spearman_PC1)

cat("\nSpearman PC2 vs normalized biofilm:\n")
print(spearman_PC2)

gc_all_bio <- gc_all %>%
  mutate(
    sample_key = sub("_.*$", "", sample)
  ) %>%
  left_join(
    bf_bin_long %>%
      dplyr::select(
        Isolate,
        medium,
        biofilm_bin
      ),
    by = c(
      "sample_key" = "Isolate",
      "medium" = "medium"
    )
  )

metric_cols_bio <- gc_all_bio %>%
  dplyr::select(k:sigma)

keep_perm <- complete.cases(
  metric_cols_bio,
  gc_all_bio$medium,
  gc_all_bio$biofilm_bin
)

metric_matrix_use <- as.matrix(
  metric_cols_bio[
    keep_perm,
    ,
    drop = FALSE
  ]
)

gc_use <- gc_all_bio[
  keep_perm,
  ,
  drop = FALSE
]

cat(
  "\nRows kept for PERMANOVA (medium + biofilm_bin):",
  nrow(gc_use),
  "/",
  nrow(gc_all_bio),
  "\n"
)

set.seed(123)

permanova_bio <- adonis2(
  metric_matrix_use ~ medium + biofilm_bin,
  data = gc_use,
  method = "euclidean",
  permutations = 999
)

cat("\nPERMANOVA: growth metrics ~ medium + biofilm_bin\n")
print(permanova_bio)

if ("biofilm_bin" %in% rownames(permanova_bio)) {
  perm_R2_bio <- permanova_bio["biofilm_bin", "R2"]
  perm_p_bio <- permanova_bio["biofilm_bin", "Pr(>F)"]
} else {
  perm_R2_bio <- NA
  perm_p_bio <- NA
}

sp_p_PC1 <- spearman_PC1$p.value
sp_p_PC2 <- spearman_PC2$p.value

p_PC1_bin <- w_PC1$p.value
p_PC2_bin <- w_PC2$p.value

stats_caption <- paste0(
  "PERMANOVA (biofilm_bin effect): R² = ",
  ifelse(
    is.na(perm_R2_bio),
    "NA",
    round(perm_R2_bio, 3)
  ),
  ", p = ",
  ifelse(
    is.na(perm_p_bio),
    "NA",
    signif(perm_p_bio, 3)
  ),
  " | Spearman PC1~biofilm_norm p = ",
  signif(sp_p_PC1, 3),
  ", PC2 p = ",
  signif(sp_p_PC2, 3),
  " | ",
  ifelse(
    nlevels(df_bin$biofilm_bin) == 2,
    "Wilcoxon",
    "KW"
  ),
  " PC1 p = ",
  signif(p_PC1_bin, 3),
  ", PC2 p = ",
  signif(p_PC2_bin, 3)
)

p_pca_bio_bin_stats <- p_pca_bio_bin +
  labs(caption = stats_caption) +
  theme(
    plot.caption = element_text(
      hjust = 0,
      size = 7
    )
  )

print(p_pca_bio_bin_stats)

ggsave(
  filename = "Global_PCA_growth_0to10h_with_binary_biofilm_and_stats_summary_below_figure.png",
  plot = p_pca_bio_bin_stats,
  width = 8,
  height = 6,
  dpi = 300
)