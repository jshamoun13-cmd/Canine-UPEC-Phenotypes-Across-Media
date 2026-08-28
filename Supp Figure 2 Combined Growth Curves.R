rm(list = ls())

required_packages <- c(
  "readxl",
  "dplyr",
  "tidyr",
  "ggplot2",
  "writexl"
)

missing_packages <- required_packages[
  !required_packages %in% rownames(installed.packages())
]

if (length(missing_packages) > 0) {
  install.packages(missing_packages)
}

library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(writexl)

file_path <- "$Data Paper 1 - Growth Avgs Across Media.xlsx"

if (!file.exists(file_path)) {
  stop(
    paste0(
      "The Excel file was not found:\n",
      file_path,
      "\n\nUpdate 'file_path' near the top of the script."
    )
  )
}

out_dir <- file.path(
  dirname(file_path),
  "Overlapping_Growth_Curves_0to10h"
)

dir.create(
  out_dir,
  showWarnings = FALSE,
  recursive = TRUE
)

message("Output folder: ", out_dir)

time_points_10 <- c(0, 2, 4, 6, 8, 10)

blank_names <- c(
  "Blank_1",
  "Blank_2",
  "Blank_3",
  "Blank_4"
)

medium_colors <- c(
  "LB" = "#CC5500",
  "M9" = "#C8A2C8",
  "AUM" = "#008080"
)

sheet_names <- excel_sheets(file_path)
required_sheets <- c("LB", "M9", "AUM")
missing_sheets <- setdiff(required_sheets, sheet_names)

if (length(missing_sheets) > 0) {
  stop(
    paste0(
      "The following required sheets were not found in the workbook: ",
      paste(missing_sheets, collapse = ", "),
      "\n\nAvailable sheets are: ",
      paste(sheet_names, collapse = ", ")
    )
  )
}

message(
  "Required sheets found: ",
  paste(required_sheets, collapse = ", ")
)

LB_raw <- read_excel(
  path = file_path,
  sheet = "LB"
)

M9_raw <- read_excel(
  path = file_path,
  sheet = "M9"
)

AUM_raw <- read_excel(
  path = file_path,
  sheet = "AUM"
)

reshape_growth_sheet <- function(
    df,
    medium_name,
    time_points,
    blanks) {
  
  message("Reshaping ", medium_name, "...")
  
  df_transposed <- as.data.frame(
    t(df),
    stringsAsFactors = FALSE
  )
  
  original_names <- as.character(df_transposed[1, ])
  
  keep_columns <- !(original_names %in% blanks)
  
  df_transposed <- df_transposed[
    ,
    keep_columns,
    drop = FALSE
  ]
  
  original_names <- original_names[keep_columns]
  
  df_transposed <- df_transposed[
    -1,
    ,
    drop = FALSE
  ]
  
  colnames(df_transposed) <- make.unique(
    original_names,
    sep = "_"
  )
  
  df_transposed <- df_transposed %>%
    slice(
      c(
        n(),
        seq_len(n() - 1)
      )
    )
  
  plate_values <- as.character(df_transposed[1, ])
  
  new_names <- paste0(
    colnames(df_transposed),
    "_",
    plate_values
  )
  
  df_transposed <- df_transposed[
    -1,
    ,
    drop = FALSE
  ]
  
  colnames(df_transposed) <- new_names
  
  number_of_timepoints <- length(time_points)
  
  if (nrow(df_transposed) < number_of_timepoints) {
    stop(
      paste0(
        medium_name,
        " contains fewer rows than the expected number of 0–10-hour ",
        "timepoints."
      )
    )
  }
  
  df_transposed <- df_transposed %>%
    slice(seq_len(number_of_timepoints))
  
  df_wide <- df_transposed %>%
    mutate(time = time_points) %>%
    relocate(time) %>%
    mutate(
      across(
        everything(),
        ~ suppressWarnings(as.numeric(.x))
      )
    )
  
  df_long <- df_wide %>%
    pivot_longer(
      cols = -time,
      names_to = "sample",
      values_to = "OD600"
    ) %>%
    mutate(
      medium = medium_name,
      sample = as.character(sample),
      time = as.numeric(time),
      OD600 = as.numeric(OD600)
    ) %>%
    filter(
      !is.na(time),
      !is.na(OD600),
      time <= 10
    ) %>%
    arrange(
      sample,
      time
    )
  
  number_of_curves <- n_distinct(df_long$sample)
  
  message(
    "  ",
    medium_name,
    ": ",
    number_of_curves,
    " curves retained."
  )
  
  return(
    list(
      wide = df_wide,
      long = df_long,
      n_curves = number_of_curves
    )
  )
}

LB_data <- reshape_growth_sheet(
  df = LB_raw,
  medium_name = "LB",
  time_points = time_points_10,
  blanks = blank_names
)

M9_data <- reshape_growth_sheet(
  df = M9_raw,
  medium_name = "M9",
  time_points = time_points_10,
  blanks = blank_names
)

AUM_data <- reshape_growth_sheet(
  df = AUM_raw,
  medium_name = "AUM",
  time_points = time_points_10,
  blanks = blank_names
)

make_growth_curve_plot <- function(
    growth_data,
    medium_name,
    number_of_curves,
    medium_color,
    line_alpha = 0.25,
    line_width = 0.55,
    point_size = 0,
    y_limits = NULL) {
  
  p <- ggplot(
    growth_data,
    aes(
      x = time,
      y = OD600,
      group = sample
    )
  ) +
    geom_line(
      color = medium_color,
      alpha = line_alpha,
      linewidth = line_width,
      lineend = "round",
      na.rm = TRUE
    )
  
  if (point_size > 0) {
    p <- p +
      geom_point(
        color = medium_color,
        alpha = line_alpha,
        size = point_size,
        na.rm = TRUE
      )
  }
  
  p <- p +
    scale_x_continuous(
      breaks = c(0, 2, 4, 6, 8, 10),
      limits = c(0, 10),
      expand = expansion(mult = c(0.01, 0.02))
    ) +
    scale_y_continuous(
      breaks = seq(0, 1.5, by = 0.25)
    ) +
    labs(
      title = paste0(medium_name, " Growth Curves"),
      subtitle = paste0(" Isolate Trajectories 0-10 Hours"),
      x = "Time (Hours)",
      y = expression(OD[600])
    ) +
    theme_classic(base_size = 15) +
    theme(
      plot.title = element_text(
        hjust = 0.5,
        face = "bold",
        size = 17
      ),
      plot.subtitle = element_text(
        hjust = 0.5,
        size = 12
      ),
      axis.title.x = element_text(
        size = 15,
        margin = margin(t = 8)
      ),
      axis.title.y = element_text(
        size = 15,
        margin = margin(r = 8)
      ),
      axis.text = element_text(size = 12),
      axis.line = element_line(linewidth = 0.6),
      axis.ticks = element_line(linewidth = 0.6),
      panel.grid = element_blank(),
      plot.margin = margin(
        t = 12,
        r = 15,
        b = 10,
        l = 10
      )
    ) +
    coord_cartesian(
      xlim = c(0, 10),
      ylim = c(0, 1.75),
      expand = FALSE
    )
  
  return(p)
}

plot_LB <- make_growth_curve_plot(
  growth_data = LB_data$long,
  medium_name = "LB",
  number_of_curves = LB_data$n_curves,
  medium_color = medium_colors[["LB"]],
  line_alpha = 0.25,
  line_width = 0.55,
  point_size = 0
)

plot_M9 <- make_growth_curve_plot(
  growth_data = M9_data$long,
  medium_name = "M9",
  number_of_curves = M9_data$n_curves,
  medium_color = medium_colors[["M9"]],
  line_alpha = 0.25,
  line_width = 0.55,
  point_size = 0
)

plot_AUM <- make_growth_curve_plot(
  growth_data = AUM_data$long,
  medium_name = "AUM",
  number_of_curves = AUM_data$n_curves,
  medium_color = medium_colors[["AUM"]],
  line_alpha = 0.25,
  line_width = 0.55,
  point_size = 0
)

print(plot_LB)
print(plot_M9)
print(plot_AUM)

ggsave(
  filename = file.path(
    out_dir,
    "LB_all_growth_curves_0to10h.png"
  ),
  plot = plot_LB,
  width = 8,
  height = 6,
  units = "in",
  dpi = 300,
  bg = "white"
)

ggsave(
  filename = file.path(
    out_dir,
    "M9_all_growth_curves_0to10h.png"
  ),
  plot = plot_M9,
  width = 8,
  height = 6,
  units = "in",
  dpi = 300,
  bg = "white"
)

ggsave(
  filename = file.path(
    out_dir,
    "AUM_all_growth_curves_0to10h.png"
  ),
  plot = plot_AUM,
  width = 8,
  height = 6,
  units = "in",
  dpi = 300,
  bg = "white"
)

ggsave(
  filename = file.path(
    out_dir,
    "LB_all_growth_curves_0to10h.tiff"
  ),
  plot = plot_LB,
  width = 8,
  height = 6,
  units = "in",
  dpi = 600,
  compression = "lzw",
  bg = "white"
)

ggsave(
  filename = file.path(
    out_dir,
    "M9_all_growth_curves_0to10h.tiff"
  ),
  plot = plot_M9,
  width = 8,
  height = 6,
  units = "in",
  dpi = 600,
  compression = "lzw",
  bg = "white"
)

ggsave(
  filename = file.path(
    out_dir,
    "AUM_all_growth_curves_0to10h.tiff"
  ),
  plot = plot_AUM,
  width = 8,
  height = 6,
  units = "in",
  dpi = 600,
  compression = "lzw",
  bg = "white"
)

write_xlsx(
  list(
    LB_long = LB_data$long,
    M9_long = M9_data$long,
    AUM_long = AUM_data$long,
    LB_wide = LB_data$wide,
    M9_wide = M9_data$wide,
    AUM_wide = AUM_data$wide
  ),
  path = file.path(
    out_dir,
    "Growth_curve_plotting_data_0to10h.xlsx"
  )
)

message("Growth-curve plotting complete.")
message("Files saved in:")
message(out_dir)
message("LB curves plotted:  ", LB_data$n_curves)
message("M9 curves plotted:  ", M9_data$n_curves)
message("AUM curves plotted: ", AUM_data$n_curves)

prepare_log_growth_data <- function(growth_data, medium_name) {
  
  nonpositive_count <- sum(
    growth_data$OD600 <= 0,
    na.rm = TRUE
  )
  
  if (nonpositive_count > 0) {
    warning(
      medium_name,
      ": ",
      nonpositive_count,
      " OD600 value(s) were <= 0 and were excluded from the log10 figure."
    )
  }
  
  growth_data %>%
    filter(
      !is.na(OD600),
      OD600 > 0
    ) %>%
    mutate(
      log10_OD600 = log10(OD600)
    ) %>%
    arrange(
      sample,
      time
    )
}

LB_log_data <- prepare_log_growth_data(
  growth_data = LB_data$long,
  medium_name = "LB"
)

M9_log_data <- prepare_log_growth_data(
  growth_data = M9_data$long,
  medium_name = "M9"
)

AUM_log_data <- prepare_log_growth_data(
  growth_data = AUM_data$long,
  medium_name = "AUM"
)

log_y_limits <- log10(
  c(0.04, 1.75)
)

log_y_break_values <- c(
  0.01,
  0.02,
  0.05,
  0.10,
  0.20,
  0.50,
  1.00,
  1.50
)

log_y_breaks <- log10(
  log_y_break_values
)

log_y_labels <- c(
  "0.01",
  "0.02",
  "0.05",
  "0.10",
  "0.20",
  "0.50",
  "1.00",
  "1.50"
)

make_log_growth_curve_plot <- function(
    growth_data,
    medium_name,
    medium_color,
    line_alpha = 0.20,
    line_width = 0.50,
    point_size = 0) {
  
  number_of_curves <- n_distinct(
    growth_data$sample
  )
  
  p <- ggplot(
    growth_data,
    aes(
      x = time,
      y = log10_OD600,
      group = sample
    )
  ) +
    geom_line(
      color = medium_color,
      alpha = line_alpha,
      linewidth = line_width,
      lineend = "round",
      na.rm = TRUE
    )
  
  if (point_size > 0) {
    p <- p +
      geom_point(
        color = medium_color,
        alpha = line_alpha,
        size = point_size,
        na.rm = TRUE
      )
  }
  
  p <- p +
    scale_x_continuous(
      breaks = c(0, 2, 4, 6, 8, 10),
      limits = c(0, 10),
      expand = expansion(
        mult = c(0.01, 0.02)
      )
    ) +
    scale_y_continuous(
      breaks = log_y_breaks,
      labels = log_y_labels
    ) +
    coord_cartesian(
      xlim = c(0, 10),
      ylim = log_y_limits,
      expand = FALSE
    ) +
    labs(
      title = paste0(
        medium_name,
        " Growth Curves"
      ),
      subtitle = paste0(" Isolate Trajectories"),
      x = "Time (Hours)",
      y = expression(log[10](OD[600]))
    ) +
    theme_classic(
      base_size = 15
    ) +
    theme(
      plot.title = element_text(
        hjust = 0.5,
        face = "bold",
        size = 17
      ),
      plot.subtitle = element_text(
        hjust = 0.5,
        size = 12
      ),
      axis.title.x = element_text(
        size = 15,
        margin = margin(t = 8)
      ),
      axis.title.y = element_text(
        size = 15,
        margin = margin(r = 8)
      ),
      axis.text = element_text(
        size = 12
      ),
      axis.line = element_line(
        linewidth = 0.6
      ),
      axis.ticks = element_line(
        linewidth = 0.6
      ),
      panel.grid = element_blank(),
      plot.margin = margin(
        t = 12,
        r = 15,
        b = 10,
        l = 10
      )
    )
  
  return(p)
}

plot_LB_log <- make_log_growth_curve_plot(
  growth_data = LB_log_data,
  medium_name = "LB",
  medium_color = medium_colors[["LB"]],
  line_alpha = 0.20,
  line_width = 0.50,
  point_size = 0
)

plot_M9_log <- make_log_growth_curve_plot(
  growth_data = M9_log_data,
  medium_name = "M9",
  medium_color = medium_colors[["M9"]],
  line_alpha = 0.20,
  line_width = 0.50,
  point_size = 0
)

plot_AUM_log <- make_log_growth_curve_plot(
  growth_data = AUM_log_data,
  medium_name = "AUM",
  medium_color = medium_colors[["AUM"]],
  line_alpha = 0.20,
  line_width = 0.50,
  point_size = 0
)

print(plot_LB_log)
print(plot_M9_log)
print(plot_AUM_log)

ggsave(
  filename = file.path(
    out_dir,
    "LB_all_growth_curves_log10_OD600_0to10h.png"
  ),
  plot = plot_LB_log,
  width = 8,
  height = 6,
  units = "in",
  dpi = 300,
  bg = "white"
)

ggsave(
  filename = file.path(
    out_dir,
    "M9_all_growth_curves_log10_OD600_0to10h.png"
  ),
  plot = plot_M9_log,
  width = 8,
  height = 6,
  units = "in",
  dpi = 300,
  bg = "white"
)

ggsave(
  filename = file.path(
    out_dir,
    "AUM_all_growth_curves_log10_OD600_0to10h.png"
  ),
  plot = plot_AUM_log,
  width = 8,
  height = 6,
  units = "in",
  dpi = 300,
  bg = "white"
)

ggsave(
  filename = file.path(
    out_dir,
    "LB_all_growth_curves_log10_OD600_0to10h.tiff"
  ),
  plot = plot_LB_log,
  width = 8,
  height = 6,
  units = "in",
  dpi = 600,
  compression = "lzw",
  bg = "white"
)

ggsave(
  filename = file.path(
    out_dir,
    "M9_all_growth_curves_log10_OD600_0to10h.tiff"
  ),
  plot = plot_M9_log,
  width = 8,
  height = 6,
  units = "in",
  dpi = 600,
  compression = "lzw",
  bg = "white"
)

ggsave(
  filename = file.path(
    out_dir,
    "AUM_all_growth_curves_log10_OD600_0to10h.tiff"
  ),
  plot = plot_AUM_log,
  width = 8,
  height = 6,
  units = "in",
  dpi = 600,
  compression = "lzw",
  bg = "white"
)

write_xlsx(
  list(
    LB_log10 = LB_log_data,
    M9_log10 = M9_log_data,
    AUM_log10 = AUM_log_data
  ),
  path = file.path(
    out_dir,
    "Growth_curve_log10_plotting_data_0to10h.xlsx"
  )
)

message("Log10 growth-curve figures complete.")
message("Files saved in:")
message(out_dir)
message(
  "LB curves plotted:  ",
  n_distinct(LB_log_data$sample)
)
message(
  "M9 curves plotted:  ",
  n_distinct(M9_log_data$sample)
)
message(
  "AUM curves plotted: ",
  n_distinct(AUM_log_data$sample)
)