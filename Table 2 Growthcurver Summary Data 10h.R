pkgs <- c(
  "readxl",
  "dplyr",
  "tidyr",
  "stringr",
  "openxlsx",
  "officer",
  "flextable",
  "knitr"
)

to_install <- pkgs[
  !sapply(pkgs, requireNamespace, quietly = TRUE)
]

if (length(to_install) > 0) {
  install.packages(
    to_install,
    repos = "https://cloud.r-project.org"
  )
}

library(readxl)
library(dplyr)
library(tidyr)
library(stringr)
library(openxlsx)
library(officer)
library(flextable)
library(knitr)

file_path <- "#Combined Growthcurver Data 0-10 for JMP Upload.xlsx"
sheet_name <- "Sheet1"

if (!file.exists(file_path)) {
  stop(
    paste0(
      "Input file was not found:\n",
      normalizePath(file_path, mustWork = FALSE),
      "\n\nPlace the spreadsheet in the current working directory ",
      "or provide the complete file path."
    )
  )
}

df <- read_excel(
  path = file_path,
  sheet = sheet_name,
  col_names = TRUE
)

if (ncol(df) < 27) {
  stop(
    paste0(
      "The input spreadsheet contains only ",
      ncol(df),
      " columns. At least 27 columns are required."
    )
  )
}

id_cols <- c(
  AUM = 1,
  LB = 10,
  M9 = 19
)

var_cols <- list(
  AUM = 2:9,
  LB = 11:18,
  M9 = 20:27
)

gc_vars <- c(
  "k",
  "n0",
  "r",
  "t_mid",
  "t_gen",
  "auc_l",
  "auc_e",
  "sigma"
)

drop_controls <- TRUE

control_prefixes <- c(
  "CFT073",
  "11775",
  "25922"
)

control_pattern <- paste0(
  "^(",
  paste(control_prefixes, collapse = "|"),
  ")"
)

fmt_med_iqr <- function(x, digits = 3) {
  x <- suppressWarnings(as.numeric(x))
  x <- x[is.finite(x)]
  
  if (length(x) == 0) {
    return(NA_character_)
  }
  
  q <- stats::quantile(
    x,
    probs = c(0.25, 0.50, 0.75),
    na.rm = TRUE,
    names = FALSE,
    type = 7
  )
  
  paste0(
    formatC(q[2], format = "f", digits = digits),
    " [",
    formatC(q[1], format = "f", digits = digits),
    "–",
    formatC(q[3], format = "f", digits = digits),
    "]"
  )
}

make_long <- function(med) {
  tmp <- df[, c(id_cols[[med]], var_cols[[med]])]
  
  names(tmp) <- c(
    "ID",
    gc_vars
  )
  
  tmp <- tmp %>%
    mutate(
      ID = str_trim(as.character(ID))
    )
  
  for (v in gc_vars) {
    tmp[[v]] <- suppressWarnings(
      as.numeric(tmp[[v]])
    )
  }
  
  tmp <- tmp %>%
    filter(
      !is.na(ID),
      ID != ""
    )
  
  control_rows <- tmp %>%
    filter(
      str_detect(
        str_to_upper(ID),
        control_pattern
      )
    )
  
  if (drop_controls) {
    tmp <- tmp %>%
      filter(
        !str_detect(
          str_to_upper(ID),
          control_pattern
        )
      )
    
    message(
      med,
      ": excluded ",
      nrow(control_rows),
      " control rows; retained ",
      nrow(tmp),
      " rows."
    )
    
    if (nrow(control_rows) > 0) {
      message(
        med,
        " excluded IDs: ",
        paste(control_rows$ID, collapse = ", ")
      )
    }
  } else {
    message(
      med,
      ": control exclusion is disabled; retained ",
      nrow(tmp),
      " rows."
    )
  }
  
  tmp %>%
    pivot_longer(
      cols = all_of(gc_vars),
      names_to = "Variable",
      values_to = "Value"
    ) %>%
    mutate(
      Medium = med
    )
}

long <- bind_rows(
  make_long("AUM"),
  make_long("LB"),
  make_long("M9")
)

retained_counts <- long %>%
  distinct(Medium, ID) %>%
  count(
    Medium,
    name = "Unique_IDs_retained"
  )

print(retained_counts)

controls_remaining <- long %>%
  distinct(Medium, ID) %>%
  filter(
    str_detect(
      str_to_upper(ID),
      control_pattern
    )
  )

if (nrow(controls_remaining) > 0) {
  warning(
    "One or more control IDs remain after filtering."
  )
  
  print(controls_remaining)
} else {
  message(
    "Control-exclusion check passed: no CFT073, 11775, ",
    "or 25922 IDs remain in the bulk-analysis dataset."
  )
}

summ <- long %>%
  group_by(
    Variable,
    Medium
  ) %>%
  summarize(
    `Median [IQR]` = fmt_med_iqr(
      Value,
      digits = 3
    ),
    .groups = "drop"
  ) %>%
  mutate(
    Variable = factor(
      Variable,
      levels = gc_vars
    ),
    Medium = factor(
      Medium,
      levels = c("AUM", "LB", "M9")
    )
  ) %>%
  arrange(
    Variable,
    Medium
  ) %>%
  pivot_wider(
    names_from = Medium,
    values_from = `Median [IQR]`
  ) %>%
  arrange(
    Variable
  ) %>%
  mutate(
    Variable = as.character(Variable)
  )

print(
  summ,
  n = Inf
)

excel_output <- "Growthcurver_Median_IQR_by_Medium_0-10.xlsx"

openxlsx::write.xlsx(
  list(
    "Median_IQR_by_Medium" = summ,
    "Retained_ID_Counts" = retained_counts
  ),
  file = excel_output,
  overwrite = TRUE
)

word_output <- "Growthcurver_Median_IQR_by_Medium_0-10.docx"

ft <- flextable(summ) %>%
  theme_booktabs() %>%
  bold(
    part = "header"
  ) %>%
  align(
    align = "center",
    part = "all"
  ) %>%
  align(
    j = "Variable",
    align = "left",
    part = "body"
  ) %>%
  autofit()

doc <- read_docx() %>%
  body_add_par(
    "Growthcurver variables summarized as Median [IQR] by medium",
    style = "heading 1"
  ) %>%
  body_add_par(
    paste0(
      "Control strains CFT073, 11775, and 25922 were excluded ",
      "from the bulk analysis."
    )
  ) %>%
  body_add_flextable(ft)

print(
  doc,
  target = word_output
)

latex_output <- "Growthcurver_Median_IQR_by_Medium_0-10.tex"

latex_tbl <- knitr::kable(
  summ,
  format = "latex",
  booktabs = TRUE,
  caption = paste0(
    "Growthcurver variables summarized as median [IQR] by medium. ",
    "Control strains CFT073, 11775, and 25922 were excluded."
  )
)

writeLines(
  latex_tbl,
  con = latex_output
)

message(
  "\nDone. Files written:",
  "\n - ", excel_output,
  "\n - ", word_output,
  "\n - ", latex_output
)

