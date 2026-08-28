library(readxl)
library(dplyr)
library(tidyr)
library(rstatix)
library(ggpubr)
library(ggplot2)

options(scipen = 999)

file_path_q2 <- "$Data Paper 1 - Normalized Biofilm Across Media.xlsx"
df_q2 <- read_excel(file_path_q2)

df_q2 <- df_q2 %>%
  dplyr::select(
    Isolate = Strain,
    LB = `Normalized LB`,
    M9 = `Normalized M9`,
    AUM = `Normalized AUM`
  )

friedman_result <- friedman.test(
  as.matrix(df_q2[, c("LB", "M9", "AUM")])
)
print(friedman_result)

df_long_q2 <- df_q2 %>%
  pivot_longer(
    cols = c("LB", "M9", "AUM"),
    names_to = "Medium",
    values_to = "OD_avg"
  ) %>%
  mutate(
    Medium = factor(Medium, levels = c("LB", "M9", "AUM"))
  )

biofilm_med_iqr <- df_long_q2 %>%
  group_by(Medium) %>%
  summarise(
    n = sum(!is.na(OD_avg)),
    median = median(OD_avg, na.rm = TRUE),
    q1 = quantile(OD_avg, 0.25, na.rm = TRUE),
    q3 = quantile(OD_avg, 0.75, na.rm = TRUE),
    iqr = IQR(OD_avg, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    median = round(median, 3),
    q1 = round(q1, 3),
    q3 = round(q3, 3),
    iqr = round(iqr, 3),
    med_iqr_label = paste0(
      format(median, nsmall = 3),
      " (", format(q1, nsmall = 3), "–", format(q3, nsmall = 3), ")"
    )
  )

cat("\n==== Normalized biofilm: Median (IQR) by medium ====\n")
print(
  biofilm_med_iqr %>%
    select(Medium, n, median, q1, q3, iqr, med_iqr_label)
)

pairwise_pvals <- df_long_q2 %>%
  pairwise_wilcox_test(
    OD_avg ~ Medium,
    paired = TRUE,
    p.adjust.method = "holm"
  ) %>%
  mutate(
    p = as.numeric(p),
    p.adj = as.numeric(p.adj),
    p_raw_label = case_when(
      p < 0.001 ~ "< 0.001",
      TRUE ~ format(round(p, 3), nsmall = 3)
    ),
    p_adj_label = case_when(
      p.adj < 0.001 ~ "< 0.001",
      TRUE ~ format(round(p.adj, 3), nsmall = 3)
    )
  )

pairwise_pvals$label <- paste0(
  "p = ",
  pairwise_pvals$p_raw_label
)

max_y <- max(df_long_q2$OD_avg, na.rm = TRUE)

pairwise_pvals <- pairwise_pvals %>%
  arrange(group1, group2) %>%
  mutate(
    y.position = max_y * c(1.05, 1.15, 1.25)
  )

print(pairwise_pvals)

medium_cols <- c(
  "LB" = "#CC5500",
  "M9" = "#C8A2C8",
  "AUM" = "#008080"
)

p <- ggplot(
  df_long_q2,
  aes(x = Medium, y = OD_avg)
) +
  geom_violin(
    aes(fill = Medium),
    width = 0.6,
    alpha = 0.20,
    color = NA,
    trim = FALSE
  ) +
  geom_jitter(
    aes(color = Medium),
    position = position_jitter(width = 0.14),
    size = 2.0,
    alpha = 0.9
  ) +
  geom_boxplot(
    width = 0.35,
    fill = NA,
    color = "black",
    linewidth = 0.7,
    outlier.shape = NA
  ) +
  stat_summary(
    fun = mean,
    geom = "point",
    shape = 24,
    size = 2.8,
    color = "black",
    fill = "white"
  ) +
  scale_color_manual(
    values = medium_cols,
    breaks = c("LB", "M9", "AUM")
  ) +
  scale_fill_manual(
    values = medium_cols,
    breaks = c("LB", "M9", "AUM")
  ) +
  labs(
    y = "Normalized Biofilm",
    x = "Medium"
  ) +
  theme_bw(base_size = 14) +
  theme(
    legend.position = "none",
    plot.title = element_text(hjust = 0.5),
    panel.grid.major.x = element_blank()
  )

p_final <- p +
  stat_pvalue_manual(
    pairwise_pvals,
    label = "label",
    tip.length = 0.01
  )

print(p_final)

ggsave(
  filename = "Normalized_Biofilm_Violin_LB_M9_AUM.png",
  plot = p_final,
  width = 8,
  height = 6,
  dpi = 300
)
