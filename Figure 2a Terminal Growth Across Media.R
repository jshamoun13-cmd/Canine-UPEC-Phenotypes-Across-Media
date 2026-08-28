library(readxl)
library(dplyr)
library(tidyr)
library(rstatix)
library(ggpubr)
library(ggplot2)

options(scipen = 999)

file_path_growth <- "$Data Paper 1 - Growth Avgs Across Media.xlsx"

aum <- read_excel(file_path_growth, sheet = "AUM")
lb  <- read_excel(file_path_growth, sheet = "LB")
m9  <- read_excel(file_path_growth, sheet = "M9")

names(aum)[1] <- "Isolate"
names(lb)[1]  <- "Isolate"
names(m9)[1]  <- "Isolate"

make_wide_growth <- function(time_col) {
  time_col <- as.character(time_col)
  
  aum_sub <- aum %>% select(Isolate, AUM = all_of(time_col))
  lb_sub  <- lb  %>% select(Isolate, LB = all_of(time_col))
  m9_sub  <- m9  %>% select(Isolate, M9 = all_of(time_col))
  
  df_wide <- aum_sub %>%
    left_join(lb_sub, by = "Isolate") %>%
    left_join(m9_sub, by = "Isolate")
  
  return(df_wide)
}

df_10 <- make_wide_growth(10)
df_24 <- make_wide_growth(24)

medium_cols <- c(
  "LB"  = "#CC5500",
  "M9"  = "#C8A2C8",
  "AUM" = "#008080"
)

growth_stats_plot <- function(df_wide, time_label = "10 h") {
  
  df_long <- df_wide %>%
    pivot_longer(
      cols = c("LB", "M9", "AUM"),
      names_to = "Medium",
      values_to = "OD"
    ) %>%
    mutate(
      Medium = factor(Medium, levels = c("LB", "M9", "AUM")),
      IsBlank = Isolate %in% c("Blank_1", "Blank_2", "Blank_3", "Blank_4")
    )
  
  df_long_use <- df_long %>% filter(!IsBlank)
  
  med_iqr <- df_long_use %>%
    group_by(Medium) %>%
    summarise(
      n = sum(!is.na(OD)),
      median = median(OD, na.rm = TRUE),
      q1 = quantile(OD, 0.25, na.rm = TRUE),
      q3 = quantile(OD, 0.75, na.rm = TRUE),
      iqr = IQR(OD, na.rm = TRUE),
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
  
  cat("\n==== Median (IQR) by medium at", time_label, "====\n")
  print(med_iqr %>% select(Medium, n, median, q1, q3, iqr, med_iqr_label))
  
  df_wide_use <- df_long_use %>%
    select(Isolate, Medium, OD) %>%
    pivot_wider(names_from = Medium, values_from = OD) %>%
    select(Isolate, LB, M9, AUM)
  
  friedman_result <- friedman.test(
    as.matrix(df_wide_use[, c("LB", "M9", "AUM")])
  )
  
  cat("\n==== Friedman test for", time_label, "====\n")
  print(friedman_result)
  
  pairwise_pvals <- df_long_use %>%
    pairwise_wilcox_test(
      OD ~ Medium,
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
  
  pairwise_pvals$label <- paste0("p = ", pairwise_pvals$p_raw_label)
  
  max_y <- max(df_long_use$OD, na.rm = TRUE)
  
  pairwise_pvals <- pairwise_pvals %>%
    arrange(group1, group2) %>%
    mutate(y.position = max_y * c(1.05, 1.15, 1.25))
  
  cat("\n==== Pairwise Wilcoxon for", time_label, "====\n")
  print(pairwise_pvals)
  
  p <- ggplot(df_long_use, aes(x = Medium, y = OD)) +
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
      size = 2,
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
    scale_fill_manual(
      values = medium_cols,
      breaks = c("LB", "M9", "AUM")
    ) +
    scale_color_manual(
      values = medium_cols,
      breaks = c("LB", "M9", "AUM")
    ) +
    labs(
      y = bquote("OD"[600] ~ "(" * .(time_label) * ")"),
      x = "Medium"
    ) +
    theme_bw(base_size = 14) +
    theme(
      legend.position = "none",
      plot.title = element_text(hjust = 0.5),
      panel.grid.major.x = element_blank()
    ) +
    stat_pvalue_manual(
      pairwise_pvals,
      label = "label",
      tip.length = 0.01
    )
  
  return(p)
}

plot_growth_10 <- growth_stats_plot(df_10, time_label = "10 h")
plot_growth_24 <- growth_stats_plot(df_24, time_label = "24 h")

plot_growth_10
plot_growth_24