library(shiny)
library(ggplot2)

# Define UI
ui <- fluidPage(
  titlePanel("Breast Cancer Partitioned Survival Model"),

  sidebarLayout(
    sidebarPanel(
      h3("Treatment Effects (Hazard Ratios)"),

      sliderInput("hr_os", "HR for Overall Survival:",
                  min = 0.4, max = 1.0, value = 0.70, step = 0.05),

      sliderInput("hr_pfs", "HR for Progression-Free Survival:",
                  min = 0.3, max = 1.0, value = 0.50, step = 0.05),

      hr(),
      h3("Cost Parameters (₹)"),

      sliderInput("cost_trastuzumab_y1", "Trastuzumab cost, Year 1 (12 months):",
                  min = 100000, max = 600000, value = 420000, step = 50000),

      sliderInput("cost_trastuzumab_oy", "Trastuzumab cost, Other years (annual):",
                  min = 5000, max = 300000, value = 25000, step = 25000),

      sliderInput("cost_pd", "Annual cost, Progressive Disease state (₹):",
                  min = 50000, max = 300000, value = 180000, step = 25000),

      sliderInput("cost_ae", "Cost of Adverse Events (per patient):",
                  min = 10000, max = 100000, value = 50000, step = 10000),

      hr(),
      h3("Quality of Life (Utilities)"),

      sliderInput("util_pfs", "Utility in PFS state:",
                  min = 0.6, max = 1.0, value = 0.80, step = 0.05),

      sliderInput("util_pd", "Utility in PD state:",
                  min = 0.3, max = 0.8, value = 0.55, step = 0.05),

      hr(),
      h3("Survival Curve Shape (Weibull)"),

      sliderInput("os_gamma", "OS shape parameter (gamma):",
                  min = 0.8, max = 1.5, value = 1.15, step = 0.05),

      sliderInput("pfs_gamma", "PFS shape parameter (gamma):",
                  min = 0.8, max = 1.5, value = 1.05, step = 0.05),

      hr(),
      h3("Model Parameters"),

      sliderInput("time_horizon", "Time horizon (years):",
                  min = 10, max = 30, value = 20, step = 1),

      sliderInput("discount_rate", "Annual discount rate:",
                  min = 0.00, max = 0.10, value = 0.03, step = 0.01),

      hr(),
      h3("Cost-Effectiveness Threshold"),

      sliderInput("wtp_threshold", "WTP Threshold (₹/QALY):",
                  min = 50000, max = 500000, value = 170000, step = 10000),

      width = 3
    ),

    mainPanel(
      tabsetPanel(
        tabPanel("Survival Curves",
                 h3("Progression-Free and Overall Survival"),
                 plotOutput("survival_curves")),

        tabPanel("State Occupancy",
                 h3("Proportion in PFS and PD Over Time"),
                 plotOutput("state_occupancy")),

        tabPanel("Results Table",
                 h3("Cost-Effectiveness Analysis"),
                 tableOutput("results_table"),
                 br(),
                 h4("Incremental Analysis"),
                 tableOutput("icer_table")),

        tabPanel("Distribution Comparison",
                 h3("Weibull vs Exponential Survival Fitting"),
                 plotOutput("distribution_comparison"))
      )
    )
  )
)

# Define server logic
server <- function(input, output) {

  # Reactive calculation of PSM outcomes
  calc_psm <- reactive({

    # Parameters
    hr_os <- input$hr_os
    hr_pfs <- input$hr_pfs

    cost_tras_y1 <- input$cost_trastuzumab_y1
    cost_tras_oy <- input$cost_trastuzumab_oy
    cost_pd <- input$cost_pd
    cost_ae <- input$cost_ae

    util_pfs <- input$util_pfs
    util_pd <- input$util_pd

    horizon <- input$time_horizon
    discount <- input$discount_rate

    # Weibull shape parameters
    os_gamma <- input$os_gamma
    pfs_gamma <- input$pfs_gamma

    # Baseline survival parameters (Weibull model)
    # These represent OS and PFS for control arm
    lambda_os_control <- 0.045  # Baseline hazard for OS in control
    lambda_pfs_control <- 0.12  # Baseline hazard for PFS in control

    # Treatment arm: adjusted by HRs (proportional hazards)
    lambda_os_treatment <- lambda_os_control * hr_os
    lambda_pfs_treatment <- lambda_pfs_control * hr_pfs

    # Calculate survival probabilities at each time point using Weibull
    # S(t) = exp(-lambda * t^gamma)
    time_points <- 0:horizon
    s_pfs_control <- exp(-lambda_pfs_control * time_points ^ pfs_gamma)
    s_pfs_treatment <- exp(-lambda_pfs_treatment * time_points ^ pfs_gamma)

    s_os_control <- exp(-lambda_os_control * time_points ^ os_gamma)
    s_os_treatment <- exp(-lambda_os_treatment * time_points ^ os_gamma)

    # Partitioned survival: state occupancy
    # PFS state: alive and progression-free
    # PD state: progressed (alive but progressed)
    # Dead: deceased

    # Simplified: PD occupancy = (OS - PFS)
    pfs_occ_control <- s_pfs_control
    pd_occ_control <- pmax(s_os_control - s_pfs_control, 0)
    dead_occ_control <- 1 - s_os_control

    pfs_occ_treatment <- s_pfs_treatment
    pd_occ_treatment <- pmax(s_os_treatment - s_pfs_treatment, 0)
    dead_occ_treatment <- 1 - s_os_treatment

    # Cost calculations
    # Control arm
    cost_control <- sum(
      pfs_occ_control * 0 / (1 + discount) ^ time_points,  # PFS: no drug cost
      pd_occ_control * cost_pd / (1 + discount) ^ time_points
    )

    # Treatment arm: drug costs in PFS state
    cost_treatment <- sum(
      pfs_occ_treatment * cost_tras_y1 * (time_points == 1) / (1 + discount) ^ time_points,  # Year 1
      pfs_occ_treatment * cost_tras_oy * (time_points > 1) / (1 + discount) ^ time_points,   # Other years
      pfs_occ_treatment * cost_ae / (1 + discount) ^ time_points,  # AE costs in PFS
      pd_occ_treatment * cost_pd / (1 + discount) ^ time_points
    )

    # QALY calculations
    qaly_control <- sum(
      pfs_occ_control * 1.0 / (1 + discount) ^ time_points,
      pd_occ_control * util_pd / (1 + discount) ^ time_points
    )

    qaly_treatment <- sum(
      pfs_occ_treatment * util_pfs / (1 + discount) ^ time_points,
      pd_occ_treatment * util_pd / (1 + discount) ^ time_points
    )

    list(
      time_points = time_points,
      s_pfs_control = s_pfs_control,
      s_pfs_treatment = s_pfs_treatment,
      s_os_control = s_os_control,
      s_os_treatment = s_os_treatment,
      pfs_occ_control = pfs_occ_control,
      pfs_occ_treatment = pfs_occ_treatment,
      pd_occ_control = pd_occ_control,
      pd_occ_treatment = pd_occ_treatment,
      cost_control = cost_control,
      cost_treatment = cost_treatment,
      qaly_control = qaly_control,
      qaly_treatment = qaly_treatment,
      lambda_os_control = lambda_os_control,
      lambda_os_treatment = lambda_os_treatment,
      lambda_pfs_control = lambda_pfs_control,
      lambda_pfs_treatment = lambda_pfs_treatment,
      os_gamma = os_gamma,
      pfs_gamma = pfs_gamma
    )
  })

  output$survival_curves <- renderPlot({
    psm_results <- calc_psm()

    survival_df <- data.frame(
      Time = rep(psm_results$time_points, 4),
      Strategy = rep(c("Control - PFS", "Treatment - PFS", "Control - OS", "Treatment - OS"),
                    each = length(psm_results$time_points)),
      Survival = c(
        psm_results$s_pfs_control,
        psm_results$s_pfs_treatment,
        psm_results$s_os_control,
        psm_results$s_os_treatment
      )
    )

    ggplot(survival_df, aes(x = Time, y = Survival, color = Strategy, linetype = Strategy)) +
      geom_line(size = 1.2) +
      scale_color_manual(values = c(
        "Control - PFS" = "#2E86AB",
        "Treatment - PFS" = "#1B998B",
        "Control - OS" = "#A23B72",
        "Treatment - OS" = "#F18F01"
      )) +
      scale_linetype_manual(values = c(
        "Control - PFS" = "dashed",
        "Treatment - PFS" = "dashed",
        "Control - OS" = "solid",
        "Treatment - OS" = "solid"
      )) +
      scale_y_continuous(labels = scales::percent) +
      labs(title = "Progression-Free and Overall Survival Curves",
           x = "Time (years)",
           y = "Survival Probability",
           color = "Strategy",
           linetype = "Strategy") +
      theme_minimal() +
      theme(
        plot.title = element_text(face = "bold", size = 14),
        legend.position = "bottom",
        panel.grid.major = element_line(color = "lightgray"),
        text = element_text(size = 11)
      )
  })

  output$state_occupancy <- renderPlot({
    psm_results <- calc_psm()

    occupancy_df <- data.frame(
      Time = rep(psm_results$time_points, 2),
      State = rep(c("PFS", "PD"), each = length(psm_results$time_points)),
      Occupancy_Control = c(
        psm_results$pfs_occ_control,
        psm_results$pd_occ_control
      ),
      Occupancy_Treatment = c(
        psm_results$pfs_occ_treatment,
        psm_results$pd_occ_treatment
      )
    )

    # Plot treatment strategy
    occ_treatment_df <- data.frame(
      Time = rep(psm_results$time_points, 2),
      State = rep(c("PFS", "PD"), each = length(psm_results$time_points)),
      Occupancy = c(psm_results$pfs_occ_treatment, psm_results$pd_occ_treatment)
    )

    occ_treatment_df$State <- factor(occ_treatment_df$State, levels = c("PFS", "PD"))

    ggplot(occ_treatment_df, aes(x = Time, y = Occupancy, fill = State)) +
      geom_area(alpha = 0.7, position = "stack") +
      scale_fill_manual(values = c("PFS" = "#2E86AB", "PD" = "#F18F01")) +
      scale_y_continuous(labels = scales::percent) +
      labs(title = "State Occupancy Over Time (Treatment Strategy)",
           x = "Time (years)",
           y = "Proportion of Cohort",
           fill = "Health State") +
      theme_minimal() +
      theme(
        plot.title = element_text(face = "bold", size = 14),
        legend.position = "bottom",
        panel.grid.major = element_line(color = "lightgray"),
        text = element_text(size = 11)
      )
  })

  output$results_table <- renderTable({
    psm_results <- calc_psm()

    results_df <- data.frame(
      Strategy = c("Control", "Treatment"),
      Total_Cost = c(
        paste("₹", format(round(psm_results$cost_control, 0), big.mark = ",")),
        paste("₹", format(round(psm_results$cost_treatment, 0), big.mark = ","))
      ),
      Total_QALYs = c(
        format(round(psm_results$qaly_control, 2), nsmall = 2),
        format(round(psm_results$qaly_treatment, 2), nsmall = 2)
      ),
      stringsAsFactors = FALSE
    )

    results_df
  }, striped = TRUE, hover = TRUE, bordered = TRUE)

  output$icer_table <- renderTable({
    psm_results <- calc_psm()
    wtp <- input$wtp_threshold

    inc_cost <- psm_results$cost_treatment - psm_results$cost_control
    inc_qaly <- psm_results$qaly_treatment - psm_results$qaly_control

    if (inc_qaly != 0) {
      icer <- inc_cost / inc_qaly
      icer_text <- paste("₹", format(round(icer, 0), big.mark = ","))
    } else {
      icer_text <- "Undefined"
    }

    # Calculate NMB
    nmb <- wtp * inc_qaly - inc_cost

    icer_df <- data.frame(
      Comparison = "Treatment vs Control",
      Incremental_Cost = paste("₹", format(round(inc_cost, 0), big.mark = ",")),
      Incremental_QALYs = format(round(inc_qaly, 2), nsmall = 2),
      ICER = icer_text,
      NMB = paste("₹", format(round(nmb, 0), big.mark = ",")),
      stringsAsFactors = FALSE
    )

    if (inc_cost < 0 & inc_qaly > 0) {
      icer_df$Interpretation <- "DOMINANT (cheaper + better)"
    } else if (inc_cost > 0 & inc_qaly < 0) {
      icer_df$Interpretation <- "DOMINATED (costlier + worse)"
    } else if (inc_cost > 0 & inc_qaly > 0) {
      icer_val <- inc_cost / inc_qaly
      if (icer_val < wtp) {
        icer_df$Interpretation <- "Cost-effective"
      } else {
        icer_df$Interpretation <- "Not cost-effective"
      }
    } else {
      icer_df$Interpretation <- "Trade-off (cheaper but worse)"
    }

    icer_df
  }, striped = TRUE, hover = TRUE, bordered = TRUE)

  output$distribution_comparison <- renderPlot({
    psm_results <- calc_psm()

    # Compare Weibull, Exponential, and Log-logistic distributions
    # For OS curve
    time_pts <- psm_results$time_points
    s_os_control <- psm_results$s_os_control
    lambda <- psm_results$lambda_os_control
    gamma <- psm_results$os_gamma

    # Exponential: S(t) = exp(-lambda*t)
    s_exponential <- exp(-lambda * time_pts)

    # Weibull: S(t) = exp(-lambda * t^gamma)
    s_weibull <- exp(-lambda * time_pts ^ gamma)

    # Log-logistic: S(t) = 1 / (1 + (lambda * t)^gamma)
    s_loglogistic <- 1 / (1 + (lambda * time_pts) ^ gamma)

    dist_df <- data.frame(
      Time = rep(time_pts, 4),
      Survival = c(s_os_control, s_exponential, s_weibull, s_loglogistic),
      Distribution = rep(c("Baseline (Weibull)", "Exponential", "Weibull", "Log-logistic"),
                        each = length(time_pts))
    )

    ggplot(dist_df, aes(x = Time, y = Survival, color = Distribution, linetype = Distribution)) +
      geom_line(size = 1.2) +
      scale_color_manual(values = c(
        "Baseline (Weibull)" = "#2E86AB",
        "Exponential" = "#F18F01",
        "Weibull" = "#A23B72",
        "Log-logistic" = "#1B998B"
      )) +
      scale_linetype_manual(values = c(
        "Baseline (Weibull)" = "solid",
        "Exponential" = "dashed",
        "Weibull" = "dotted",
        "Log-logistic" = "twodash"
      )) +
      scale_y_continuous(labels = scales::percent) +
      labs(title = "Distribution Comparison: Exponential, Weibull, and Log-logistic",
           subtitle = "Comparison of OS survival distributions",
           x = "Time (years)",
           y = "Overall Survival Probability",
           color = "Distribution",
           linetype = "Distribution") +
      theme_minimal() +
      theme(
        plot.title = element_text(face = "bold", size = 14),
        legend.position = "bottom",
        panel.grid.major = element_line(color = "lightgray"),
        text = element_text(size = 11)
      )
  })
}

shinyApp(ui = ui, server = server)
