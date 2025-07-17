
# Lodgic App - Required Packages Installation Script

required_packages <- c(
  "shiny",
  "shinyjs",
  "leaflet",
  "leaflet.extras",
  "plotly",
  "tmaptools",
  "readr",
  "readxl",
  "dplyr",
  "stringr",
  "ggplot2",
  "htmltools"
)

installed_packages <- rownames(installed.packages())
to_install <- setdiff(required_packages, installed_packages)

if (length(to_install)) {
  install.packages(to_install)
} else {
  message("All required packages are already installed.")
}
