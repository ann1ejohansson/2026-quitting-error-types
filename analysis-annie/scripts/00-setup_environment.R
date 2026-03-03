# libraries ----
packages <- c(
  # file management
  "here",

  # data management
  "tidyverse",
  "data.table",
  "lubridate",
  "slider",
  "forcats",
  "dplyr",
  "tidyr",
  "kableExtra",
  "tidytext",
  "stringr",
  "zoo",
  "DescTools",

  # plotting tools
  "ggplot2",
  "cowplot",
  "ggpubr",
  "grid",
  "gridExtra",
  "RColorBrewer",

  # model fitting
  "msm",
  "lme4",
  "lmtest",
  "car",
  "merTools"
)

installed <- packages %in% rownames(installed.packages())

if (any(!installed)) {
  install.packages(packages[!installed])
}

invisible(lapply(packages, library, character.only = TRUE))


# set working dir ----
here::i_am("analysis-annie/scripts/00-setup_environment.R")

rm(installed, packages)

# folders
path_to_models <- here("analysis-annie/models")
path_to_figures <- here("analysis-annie/figures")
path_to_tables <- here("analysis-annie/tables")
