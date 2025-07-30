### Master SCRIPT

# install_packages.R
packages_to_install <- c("tidyverse", "here", "sf", "readxl", "viridis", "knitr", "networkD3",  "edc", "rnaturalearth",
                         "ggspatial", "quarto")


# Check and install packages
for (package in packages_to_install) {
  if (!require(package, character.only = TRUE)) {
    install.packages(package, dependencies = TRUE)
    library(package, character.only = TRUE)
  }
}

library(here)

# Run this script to compute the results

### Loading functions

source(
  here(
    "depreciation_functions_24.R"
)
)

# Computes the results

source(
  here(
    "script_results.R"
  )
)

# Generates plots

source(
  here(
    "script_plots.R"
  )
)

# Renders all plots

quarto::quarto_render(here("docs", "your_report.qmd"))