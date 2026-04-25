# install_packages.R
# Run this script once before launching the app
# source("install_packages.R")

packages <- c("shiny", "tidyverse", "ggplot2", "plotly", "shinythemes", "DT")

for (pkg in packages) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg)
    library(pkg, character.only = TRUE)
  }
}

print("All packages installed successfully!")