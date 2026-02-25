# R for HTA (Basics) — 3-Day Workshop

## Workshop Overview

This repository contains all materials for the **R for Health Technology Assessment (Basics)** workshop, designed for HTA analysts working in regional resource centres who are transitioning from Excel and TreeAge to R.

**Philosophy:** This workshop teaches *possibilities*, not programming. Participants learn what R enables for HTA, how to read and adapt existing code, and how to use available tools (including generative AI) to support their HTA workflow.

## Workshop Structure

### Day 1 — Why R, Getting Comfortable, and Decision Trees
- **Session 1:** The Case for R in HTA
- **Session 2:** R Orientation — Reading and Running Code
- **Session 3:** Diagnostic Decision Tree — Gestational Diabetes Screening
- **Session 4:** Therapeutic Decision Tree — Drug-Eluting vs Bare Metal Stents

### Day 2 — Markov Models, PSA, and Practical Tools
- **Session 5:** 4-State Markov Model — Chronic Kidney Disease Progression
- **Session 6:** Probabilistic Sensitivity Analysis
- **Session 7:** Using Generative AI for HTA Coding

### Day 3 — Partitioned Survival, Shiny Apps, and Next Steps
- **Session 8:** Partitioned Survival Model — Trastuzumab for HER2+ Breast Cancer
- **Session 9:** PSA Applied to the Partitioned Survival Model
- **Session 10:** Interactive Shiny App Templates
- **Session 11:** Wrap-Up and Next Steps

## Core HTA Examples

| Example | Model Type | Clinical Area | Data Sources |
|---|---|---|---|
| GDM Screening | Diagnostic Decision Tree | Gestational Diabetes Mellitus | Indian prevalence, OGTT/GCT accuracy, public hospital costs |
| DES vs BMS | Therapeutic Decision Tree | Coronary Artery Disease | Indian cardiology CEA literature, PMJAY procedure rates |
| CKD Progression | 4-State Markov Model | Chronic Kidney Disease | Indian nephrology studies, PMJAY dialysis costs |
| Trastuzumab | Partitioned Survival Model | HER2+ Breast Cancer | Tata Memorial data, Indian oncology CEA studies |

## Materials per Session

Each session includes:
- **Slides** (Quarto revealjs `.qmd`)
- **Explanatory module** (annotated `.qmd` rendering to HTML)
- **Exercise** (`.qmd` with tasks for participants)
- **Solution** (complete `.qmd` with answers)
- **Shiny app template** (for the four core HTA examples)

## How to Use This Repository

### For Participants
1. Install [R](https://cran.r-project.org/) and [RStudio](https://posit.co/download/rstudio-desktop/)
2. Clone or download this repository
3. Open the `.Rproj` file in RStudio
4. Navigate to the session folder and open the `.qmd` files
5. Follow along with the facilitator

### For Facilitators
- Session slides are in each session folder as `slides.qmd`
- Facilitator notes are in `resources/facilitator-notes.md`
- Ensure all packages listed in `resources/setup-guide.qmd` are installed before the workshop

## Required R Packages

```r
install.packages(c(
  "tidyverse",    # Data manipulation and visualization
  "heemod",       # Health economic modelling
  "survHE",       # Survival analysis for HE
  "flexsurv",     # Flexible survival modelling
  "BCEA",         # Bayesian cost-effectiveness analysis
  "shiny",        # Interactive web apps
  "dampack",      # Decision-analytic modelling
  "quarto",       # Document rendering
  "knitr",        # Tables and reporting
  "DT"            # Interactive tables
))
```

## Assumptions and Data Sources

All model parameters are sourced from published literature, preferencing Indian studies. Where Indian data is not available, the closest regional or global estimates are used with explicit disclaimers. Full references are provided in each module.

## Development Process

Initial drafts of these materials were generated with AI assistance (Claude, Anthropic), then reviewed, validated, and edited by the workshop team. The clinical reasoning, model structuring, parameter selection, and pedagogical design were guided by domain experts at RRC-HTA, AIIMS Bhopal. These materials are actively evolving — see the [About page](https://drpakhare.github.io/HTA-R-Workshop-2026/about.html) for full details.

## License

This work is licensed under [CC BY 4.0](https://creativecommons.org/licenses/by/4.0/). You are free to share and adapt these materials with attribution.

## Contact

For questions about workshop content or collaboration, please open an [issue](https://github.com/drpakhare/HTA-R-Workshop-2026/issues) on this repository.
