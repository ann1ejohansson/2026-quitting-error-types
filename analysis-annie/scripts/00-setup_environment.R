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
  "tidyr",
  "kableExtra",
  "tidytext",
  "stringr",
  "zoo",
  "DescTools",
  "dplyr",

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
  "merTools",

  # misc
  "conflicted"
)

installed <- packages %in% rownames(installed.packages())

if (any(!installed)) {
  install.packages(packages[!installed])
}

invisible(lapply(packages, library, character.only = TRUE))

## dplyr::select() conflicts with MASS::select()
conflict_prefer("select", "dplyr")


# set working dir ----
here::i_am("analysis-annie/scripts/00-setup_environment.R")

rm(installed, packages)

# directories
folders <- c("models", "figures", "tables")

# Loop through each folder
for (folder in folders) {
  # Get the full path to the folder
  folder_path <- here("analysis-annie", folder)

  # Check if the folder exists
  if (!dir.exists(folder_path)) {
    # Create the folder if it does not exist
    dir.create(folder_path)
    message(paste("Created dir:", folder))
  } else {
    message(paste("Dir already exists:", folder))
  }
}


# plotting tools ----
response_colors <- c(
  cor = "darkslategray",
  error = "firebrick2",
  qm = "goldenrod2",
  late = "darkslategray3"
)

main_color <- "darkgray"
comp_color <- "gray20"

binary_colors <- c("gray20", "darkgray")

plot_theme <- theme_bw(12) + theme(
  #panel.grid.major = element_line(color = "gray90"),
  #axis.ticks = element_line(color = "gray20"),
  axis.text = element_text(color = "gray20", size = 10),
  axis.title = element_text(color = "gray20", face = "bold", size = 14),
  plot.title = element_text(hjust = 0, size = 16, face = "bold"), # hjust = 0.5 for center aligned
  plot.subtitle = element_text(hjust = 0, size = 14),
  legend.position = "bottom",
  legend.title = element_text(size = 14, face = "bold"),
  legend.text = element_text(size = 14),
  strip.text = element_text(size = 12, color = "gray20"),
  strip.background = element_rect(fill = "gray80", color = "gray80"))

