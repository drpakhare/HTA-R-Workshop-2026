#!/bin/bash
# Build script for HTA-R-Workshop-2026 Quarto website
#
# Prerequisites:
#   - R (>= 4.3) installed
#   - RStudio or Quarto CLI installed
#   - Required R packages installed (run setup-guide.qmd first)
#
# Usage:
#   chmod +x build.sh
#   ./build.sh
#
# Or from RStudio: click "Build Website" in the Build pane

echo "=== HTA-R-Workshop-2026: Building website ==="
echo ""

# Check prerequisites
if ! command -v quarto &> /dev/null; then
    echo "ERROR: Quarto is not installed."
    echo "Download from: https://quarto.org/docs/get-started/"
    exit 1
fi

if ! command -v R &> /dev/null; then
    echo "ERROR: R is not installed."
    echo "Download from: https://cran.r-project.org/"
    exit 1
fi

echo "Quarto version: $(quarto --version)"
echo "R version: $(R --version | head -1)"
echo ""

# Install R packages if needed
echo "Checking R packages..."
R -e '
packages <- c("tidyverse", "ggplot2", "knitr", "DT", "shiny")
missing <- packages[!packages %in% installed.packages()[,"Package"]]
if (length(missing) > 0) {
  cat("Installing missing packages:", paste(missing, collapse=", "), "\n")
  install.packages(missing, repos="https://cran.r-project.org/")
} else {
  cat("All required packages installed.\n")
}
'

echo ""
echo "Building website..."
echo "NOTE: Exercise and solution .qmd files are excluded from rendering."
echo "      They are included as downloadable resources."
echo ""

# Render the website
quarto render

echo ""
echo "=== Build complete ==="
echo "Output: _site/"
echo "To preview: quarto preview"
echo "To deploy to GitHub Pages: quarto publish gh-pages"
