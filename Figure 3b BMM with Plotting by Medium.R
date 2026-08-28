library(brms)
library(tidyverse)
library(readxl)
library(posterior)
library(mclust)
library(ggplot2)
library(patchwork)

file_path <- "$Data Paper 1 - Normalized Biofilm Across Media.xlsx"
df <- read_excel(file_path)

df_long_clean <- df %>%
  select(
    Isolate = Strain,
    LB = `Normalized LB`,
    M9 = `Normalized M9`,
    AUM = `Normalized AUM`
  ) %>%
  pivot_longer(
    cols = c(LB, M9, AUM),
    names_to = "Medium",
    values_to = "OD_avg"
  ) %>%
  mutate(
    Medium = factor(Medium, levels = c("LB", "M9", "AUM"))
  )

head(df_long_clean)
table(df_long_clean$Medium)

cat("\n================ DATA SANITY CHECK ================\n")
cat("Total rows:", nrow(df_long_clean), "\n")
cat("Missing OD values:", sum(is.na(df_long_clean$OD_avg)), "\n")

print(
  df_long_clean %>%
    group_by(Medium) %>%
    summarise(
      n = n(),
      n_missing = sum(is.na(OD_avg)),
      min_OD = min(OD_avg, na.rm = TRUE),
      max_OD = max(OD_avg, na.rm = TRUE),
      mean_OD = mean(OD_avg, na.rm = TRUE),
      sd_OD = sd(OD_avg, na.rm = TRUE),
      .groups = "drop"
    )
)

run_bic_check <- function(data_medium, medium_name, G_range = 1:4) {
  x <- data_medium$OD_avg
  x <- x[is.finite(x)]
  
  cat("\nBIC mixture check for:", medium_name, "\n")
  cat("Sample size:", length(x), "\n")
  
  if (length(x) < 10) {
    warning(
      paste(
        "Too few observations in",
        medium_name,
        "for stable mixture modeling."
      )
    )
    return(NULL)
  }
  
  bic_fit <- Mclust(x, G = G_range)
  
  bic_df <- data.frame(
    n_components = bic_fit$G,
    model = bic_fit$modelName,
    selected_loglik = bic_fit$loglik
  )
  
  cat("Selected number of components:", bic_fit$G, "\n")
  cat("Selected model:", bic_fit$modelName, "\n")
  
  cat("\nComponent means:\n")
  print(round(bic_fit$parameters$mean, 4))
  
  cat("\nComponent variances:\n")
  print(round(bic_fit$parameters$variance$sigmasq, 4))
  
  cat("\nMixing proportions:\n")
  print(round(bic_fit$parameters$pro, 4))
  
  cat("\nClassification counts:\n")
  print(table(bic_fit$classification))
  
  plot(bic_fit, what = "BIC")
  plot(bic_fit, what = "classification")
  
  classified_df <- data_medium %>%
    filter(is.finite(OD_avg)) %>%
    mutate(
      bic_cluster = bic_fit$classification,
      bic_cluster = factor(
        bic_cluster,
        levels = sort(unique(bic_fit$classification)),
        labels = paste0(
          "Subpop_",
          sort(unique(bic_fit$classification))
        )
      )
    )
  
  list(
    fit = bic_fit,
    classified_data = classified_df,
    bic_summary = bic_df
  )
}

bic_LB <- run_bic_check(
  data_medium = df_long_clean %>% filter(Medium == "LB"),
  medium_name = "LB",
  G_range = 1:4
)

bic_M9 <- run_bic_check(
  data_medium = df_long_clean %>% filter(Medium == "M9"),
  medium_name = "M9",
  G_range = 1:4
)

bic_AUM <- run_bic_check(
  data_medium = df_long_clean %>% filter(Medium == "AUM"),
  medium_name = "AUM",
  G_range = 1:4
)

bic_classified_all <- bind_rows(
  bic_LB$classified_data,
  bic_M9$classified_data,
  bic_AUM$classified_data
) %>%
  mutate(
    Medium = factor(Medium, levels = c("LB", "M9", "AUM"))
  )

print(head(bic_classified_all))

write.csv(
  bic_classified_all,
  "BIC_classified_subpopulations_by_medium.csv",
  row.names = FALSE
)

run_mixture_model <- function(
    data_medium,
    prior_mu_shift = -0.1,
    iter = 8000,
    warmup = 4000) {
  
  scaled_obj <- scale(data_medium$OD_avg)
  data_medium$OD_scaled <- scaled_obj[, 1]
  mean_orig <- attr(scaled_obj, "scaled:center")
  sd_orig <- attr(scaled_obj, "scaled:scale")
  
  mix <- mixture(
    gaussian(),
    gaussian(),
    order = "mu"
  )
  
  priors <- c(
    set_prior(
      paste0("student_t(3, ", prior_mu_shift, ", 2.5)"),
      class = "Intercept",
      dpar = "mu1"
    ),
    set_prior(
      paste0("student_t(3, ", prior_mu_shift, ", 2.5)"),
      class = "Intercept",
      dpar = "mu2"
    ),
    set_prior(
      "student_t(3, 0, 2.5)",
      class = "sigma1",
      lb = 0
    ),
    set_prior(
      "student_t(3, 0, 2.5)",
      class = "sigma2",
      lb = 0
    ),
    set_prior(
      "dirichlet(1)",
      class = "theta"
    )
  )
  
  fit <- brm(
    bf(OD_scaled ~ 1),
    data = data_medium,
    family = mix,
    prior = priors,
    chains = 4,
    iter = iter,
    warmup = warmup,
    init = 0,
    control = list(
      adapt_delta = 0.99,
      max_treedepth = 15
    ),
    seed = 123
  )
  
  post <- as_draws_df(fit)
  
  mu1_bt <- post$b_mu1_Intercept * sd_orig + mean_orig
  mu2_bt <- post$b_mu2_Intercept * sd_orig + mean_orig
  sd1_bt <- post$sigma1 * sd_orig
  sd2_bt <- post$sigma2 * sd_orig
  theta1 <- post$theta1
  theta2 <- post$theta2
  
  compute_cutoff <- function(mu1, mu2, sd1, sd2, p1, p2) {
    f_diff <- function(x) {
      p1 * dnorm(x, mu1, sd1) -
        p2 * dnorm(x, mu2, sd2)
    }
    
    tryCatch(
      uniroot(
        f_diff,
        lower = min(mu1, mu2) - 3 * max(sd1, sd2),
        upper = max(mu1, mu2) + 3 * max(sd1, sd2)
      )$root,
      error = function(e) NA
    )
  }
  
  cutoff_samples <- purrr::pmap_dbl(
    list(
      mu1_bt,
      mu2_bt,
      sd1_bt,
      sd2_bt,
      theta1,
      theta2
    ),
    compute_cutoff
  )
  
  list(
    fit = fit,
    mean1 = mean(mu1_bt),
    mean2 = mean(mu2_bt),
    sd1 = mean(sd1_bt, na.rm = TRUE),
    sd2 = mean(sd2_bt, na.rm = TRUE),
    CI1 = quantile(mu1_bt, c(0.025, 0.975)),
    CI2 = quantile(mu2_bt, c(0.025, 0.975)),
    cutoff = tibble(
      mean = mean(cutoff_samples, na.rm = TRUE),
      lower = quantile(cutoff_samples, 0.025, na.rm = TRUE),
      upper = quantile(cutoff_samples, 0.975, na.rm = TRUE)
    )
  )
}

result_LB <- run_mixture_model(
  df_long_clean %>% filter(Medium == "LB"),
  prior_mu_shift = -0.3,
  iter = 20000,
  warmup = 10000
)

result_M9 <- run_mixture_model(
  df_long_clean %>% filter(Medium == "M9"),
  prior_mu_shift = -0.4,
  iter = 20000,
  warmup = 10000
)

result_AUM <- run_mixture_model(
  df_long_clean %>% filter(Medium == "AUM"),
  prior_mu_shift = -0.1
)

cat("===== LB =====\n")
print(result_LB$mean1)
print(result_LB$mean2)
print(result_LB$cutoff)

cat("===== M9 =====\n")
print(result_M9$mean1)
print(result_M9$mean2)
print(result_M9$cutoff)

cat("===== AUM =====\n")
print(result_AUM$mean1)
print(result_AUM$mean2)
print(result_AUM$cutoff)

x_range <- range(
  df_long_clean$OD_avg,
  na.rm = TRUE
)

get_ymax <- function(dat) {
  d <- density(
    dat$OD_avg,
    na.rm = TRUE,
    adjust = 1.1
  )
  
  max(d$y, na.rm = TRUE)
}

y_max <- max(
  get_ymax(
    df_long_clean %>%
      filter(Medium == "LB")
  ),
  get_ymax(
    df_long_clean %>%
      filter(Medium == "M9")
  ),
  get_ymax(
    df_long_clean %>%
      filter(Medium == "AUM")
  )
)

medium_cols <- c(
  "LB" = "#CC5500",
  "M9" = "#C8A2C8",
  "AUM" = "#008080"
)

make_density_plot <- function(
    medium_name,
    data_medium,
    cutoff_tbl,
    line_col,
    show_y = TRUE) {
  
  cutoff_mean <- cutoff_tbl$mean[1]
  y_lab <- if (show_y) "Density" else NULL
  
  dens <- density(
    data_medium$OD_avg,
    na.rm = TRUE,
    adjust = 1.1
  )
  
  dens_df <- tibble(
    x = dens$x,
    y = dens$y
  )
  
  dens_right <- dens_df %>%
    filter(x >= cutoff_mean)
  
  p <- ggplot() +
    geom_area(
      data = dens_df,
      aes(x = x, y = y),
      fill = line_col,
      alpha = 0.35,
      color = NA
    ) +
    geom_area(
      data = dens_right,
      aes(x = x, y = y),
      fill = line_col,
      alpha = 0.75,
      color = NA
    ) +
    geom_line(
      data = dens_df,
      aes(x = x, y = y),
      color = "black",
      linewidth = 0.5
    ) +
    geom_vline(
      xintercept = cutoff_mean,
      color = "black",
      linetype = "longdash",
      linewidth = 0.7
    ) +
    annotate(
      "text",
      x = cutoff_mean,
      y = 3,
      label = sprintf("%.2f", cutoff_mean),
      angle = 0,
      hjust = -0.2,
      vjust = 0.5,
      size = 4.2
    ) +
    coord_cartesian(
      xlim = x_range,
      ylim = c(0, y_max * 1.02),
      expand = FALSE
    ) +
    labs(
      title = medium_name,
      x = NULL,
      y = y_lab
    ) +
    theme_classic(base_size = 13) +
    theme(
      plot.title = element_text(
        hjust = 0.5,
        face = "plain",
        size = 13
      ),
      axis.title.x = element_text(
        size = 13,
        margin = margin(t = 4)
      ),
      axis.title.y = element_text(
        size = 13,
        margin = margin(r = 4)
      ),
      axis.text = element_text(
        size = 13,
        color = "black"
      ),
      panel.border = element_rect(
        color = "black",
        fill = NA,
        linewidth = 0.6
      ),
      plot.margin = margin(2, 6, 4, 6)
    )
  
  if (!show_y) {
    p <- p +
      theme(
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()
      )
  }
  
  return(p)
}

data_LB <- df_long_clean %>%
  filter(Medium == "LB")

data_M9 <- df_long_clean %>%
  filter(Medium == "M9")

data_AUM <- df_long_clean %>%
  filter(Medium == "AUM")

plot_LB <- make_density_plot(
  "LB",
  data_LB,
  result_LB$cutoff,
  medium_cols["LB"],
  show_y = TRUE
)

plot_M9 <- make_density_plot(
  "M9",
  data_M9,
  result_M9$cutoff,
  medium_cols["M9"],
  show_y = FALSE
)

plot_AUM <- make_density_plot(
  "AUM",
  data_AUM,
  result_AUM$cutoff,
  medium_cols["AUM"],
  show_y = FALSE
)

combined_plot <- (
  plot_LB |
    plot_M9 |
    plot_AUM
) +
  plot_annotation(
    title = "Normalized biofilm distributions and Bayesian mixture cutoffs"
  ) &
  theme(
    plot.title = element_text(hjust = 0.5)
  )

combined_plot <- combined_plot /
  patchwork::wrap_elements(
    ggplot() +
      annotate(
        "text",
        x = 0.5,
        y = 0.6,
        label = "Normalized biofilm",
        size = 4.5
      ) +
      theme_void()
  ) +
  plot_layout(
    heights = c(10, 0.5)
  )

print(combined_plot)

ggsave(
  filename = "$Figure 3b Biofilm Density Plots by Medium.png",
  plot = combined_plot,
  width = 12,
  height = 4.2,
  dpi = 600,
  bg = "white"
)
