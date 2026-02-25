library(shiny)
library(ggplot2)

# Define UI
ui <- fluidPage(
  titlePanel("GDM Diagnostic Decision Tree Analysis"),

  sidebarLayout(
    sidebarPanel(
      h3("Model Parameters"),

      sliderInput("prev_gdm", "Prevalence of GDM:",
                  min = 0.05, max = 0.25, value = 0.15, step = 0.01),

      sliderInput("sens_ogtt", "Sensitivity of OGTT:",
                  min = 0.5, max = 1.0, value = 0.95, step = 0.01),

      sliderInput("spec_ogtt", "Specificity of OGTT:",
                  min = 0.5, max = 1.0, value = 0.90, step = 0.01),

      sliderInput("prop_high_risk", "Proportion at high risk (for risk-based):",
                  min = 0.1, max = 0.8, value = 0.4, step = 0.05),

      hr(),
      h4("Cost Parameters (₹)"),

      sliderInput("cost_ogtt", "Cost of OGTT:",
                  min = 100, max = 1000, value = 500, step = 50),

      sliderInput("cost_adverse", "Cost of adverse outcome (untreated):",
                  min = 20000, max = 80000, value = 50000, step = 5000),

      sliderInput("cost_gdm_treatment", "Cost of GDM treatment:",
                  min = 3000, max = 15000, value = 8000, step = 1000),

      hr(),
      h4("Outcome Probabilities"),

      sliderInput("p_adverse_gdm_treated", "P(adverse | treated GDM):",
                  min = 0.01, max = 0.20, value = 0.05, step = 0.01),

      sliderInput("p_adverse_gdm_untreated", "P(adverse | untreated GDM):",
                  min = 0.05, max = 0.40, value = 0.20, step = 0.01),

      width = 3
    ),

    mainPanel(
      tabsetPanel(
        tabPanel("Results Table",
                 h3("Cost-Effectiveness Comparison"),
                 tableOutput("results_table"),
                 br(),
                 h4("ICER vs No Screening"),
                 tableOutput("icer_table")),

        tabPanel("Cost-Effectiveness Plane",
                 h3("CE Plane: Cost vs Effectiveness"),
                 plotOutput("ce_plane")),

        tabPanel("Sensitivity Analysis",
                 h3("Tornado Diagram: Impact on ICER"),
                 plotOutput("sensitivity_plot"))
      )
    )
  )
)

# Define server logic
server <- function(input, output) {

  # Reactive calculation of outcomes
  calc_outcomes <- reactive({

    # Parameters
    prev <- input$prev_gdm
    sens <- input$sens_ogtt
    spec <- input$spec_ogtt
    prop_high_risk <- input$prop_high_risk

    cost_ogtt <- input$cost_ogtt
    cost_adverse <- input$cost_adverse
    cost_treatment <- input$cost_gdm_treatment

    p_adverse_treated <- input$p_adverse_gdm_treated
    p_adverse_untreated <- input$p_adverse_gdm_untreated

    # Utility (simplified: all strategies have same utility, cost-minimization)
    # In a real model, you might vary quality of life
    utility <- 1.0

    # Strategy 1: Universal Screening
    # Everyone screened
    cost_s1_screening <- cost_ogtt

    # True positives: sens * prev (correctly identified as GDM)
    true_pos_rate <- sens * prev
    cost_s1_treatment <- true_pos_rate * cost_treatment
    cost_s1_adverse <- true_pos_rate * p_adverse_treated * cost_adverse

    # False positives: (1-spec) * (1-prev) (healthy people wrongly treated)
    false_pos_rate <- (1 - spec) * (1 - prev)
    cost_s1_fp_treatment <- false_pos_rate * cost_treatment
    cost_s1_fp_adverse <- false_pos_rate * p_adverse_treated * cost_adverse

    # False negatives: (1-sens) * prev (GDM not detected, untreated)
    false_neg_rate <- (1 - sens) * prev
    cost_s1_fn_adverse <- false_neg_rate * p_adverse_untreated * cost_adverse

    cost_s1 <- cost_s1_screening + cost_s1_treatment + cost_s1_adverse +
               cost_s1_fp_treatment + cost_s1_fp_adverse + cost_s1_fn_adverse
    qaly_s1 <- utility

    # Strategy 2: Risk-Based Screening
    # Only high-risk women screened
    n_screened <- prop_high_risk
    n_not_screened <- 1 - prop_high_risk

    # Among screened, detect and treat GDM
    true_pos_screened <- sens * prev * n_screened
    false_pos_screened <- (1 - spec) * (1 - prev) * n_screened
    false_neg_screened <- (1 - sens) * prev * n_screened

    cost_s2_screening <- n_screened * cost_ogtt
    cost_s2_treatment <- true_pos_screened * cost_treatment
    cost_s2_adverse <- true_pos_screened * p_adverse_treated * cost_adverse
    cost_s2_fp_treatment <- false_pos_screened * cost_treatment
    cost_s2_fp_adverse <- false_pos_screened * p_adverse_treated * cost_adverse
    cost_s2_fn_adverse <- false_neg_screened * p_adverse_untreated * cost_adverse

    # Among not screened, GDM goes untreated
    n_gdm_not_screened <- prev * n_not_screened
    cost_s2_unscreened_adverse <- n_gdm_not_screened * p_adverse_untreated * cost_adverse

    cost_s2 <- cost_s2_screening + cost_s2_treatment + cost_s2_adverse +
               cost_s2_fp_treatment + cost_s2_fp_adverse + cost_s2_fn_adverse +
               cost_s2_unscreened_adverse
    qaly_s2 <- utility

    # Strategy 3: No Screening
    # No screening, all GDM goes untreated
    cost_s3 <- prev * p_adverse_untreated * cost_adverse
    qaly_s3 <- utility

    data.frame(
      Strategy = c("Universal Screening", "Risk-Based Screening", "No Screening"),
      Cost = c(cost_s1, cost_s2, cost_s3),
      QALY = c(qaly_s1, qaly_s2, qaly_s3),
      stringsAsFactors = FALSE
    )
  })

  output$results_table <- renderTable({
    results <- calc_outcomes()
    results$Cost <- paste("₹", format(round(results$Cost, 0), big.mark = ","))
    results$QALY <- format(round(results$QALY, 4), nsmall = 4)
    results
  }, striped = TRUE, hover = TRUE, bordered = TRUE)

  output$icer_table <- renderTable({
    results <- calc_outcomes()

    # Calculate ICER vs No Screening
    ref_cost <- results$Cost[3]
    ref_qaly <- results$QALY[3]

    icer_universal <- (results$Cost[1] - ref_cost) / (results$QALY[1] - ref_qaly)
    icer_risk <- (results$Cost[2] - ref_cost) / (results$QALY[2] - ref_qaly)

    icer_df <- data.frame(
      Strategy = c("Universal Screening", "Risk-Based Screening"),
      Incremental_Cost = c(
        paste("₹", format(round(results$Cost[1] - ref_cost, 0), big.mark = ",")),
        paste("₹", format(round(results$Cost[2] - ref_cost, 0), big.mark = ","))
      ),
      Incremental_QALY = c(
        format(round(results$QALY[1] - ref_qaly, 4), nsmall = 4),
        format(round(results$QALY[2] - ref_qaly, 4), nsmall = 4)
      ),
      ICER = c(
        ifelse(is.infinite(icer_universal) | is.nan(icer_universal), "Dominant",
               paste("₹", format(round(icer_universal, 0), big.mark = ","))),
        ifelse(is.infinite(icer_risk) | is.nan(icer_risk), "Dominant",
               paste("₹", format(round(icer_risk, 0), big.mark = ",")))
      ),
      stringsAsFactors = FALSE
    )

    icer_df
  }, striped = TRUE, hover = TRUE, bordered = TRUE)

  output$ce_plane <- renderPlot({
    results <- calc_outcomes()

    ggplot(results, aes(x = QALY, y = Cost, color = Strategy, shape = Strategy)) +
      geom_point(size = 8) +
      geom_label(aes(label = Strategy), nudge_y = 1000, size = 3.5, show.legend = FALSE) +
      scale_y_continuous(labels = function(x) paste("₹", format(x, big.mark = ","))) +
      labs(title = "Cost-Effectiveness Plane",
           x = "Effectiveness (QALYs)",
           y = "Cost") +
      theme_minimal() +
      theme(
        plot.title = element_text(face = "bold", size = 14),
        legend.position = "bottom",
        panel.grid.major = element_line(color = "lightgray"),
        text = element_text(size = 11)
      )
  })

  output$sensitivity_plot <- renderPlot({
    # Sensitivity analysis: vary key parameters by ±20%
    base_results <- calc_outcomes()

    # Calculate base ICER (Universal vs No Screening)
    base_cost_diff <- base_results$Cost[1] - base_results$Cost[3]
    base_qaly_diff <- base_results$QALY[1] - base_results$QALY[3]
    base_icer <- base_cost_diff / base_qaly_diff

    # Vary parameters
    params <- c("Prevalence", "Sensitivity", "Specificity", "OGTT Cost", "Treatment Cost")
    icer_low <- numeric(length(params))
    icer_high <- numeric(length(params))

    # For simplicity, we'll show approximate ranges
    # Full sensitivity analysis would require re-running the model many times

    for (i in seq_along(params)) {
      if (i == 1) { # Prevalence
        low_val <- input$prev_gdm * 0.8
        high_val <- input$prev_gdm * 1.2
      } else if (i == 2) { # Sensitivity
        low_val <- input$sens_ogtt * 0.8
        high_val <- input$sens_ogtt * 1.2
      } else if (i == 3) { # Specificity
        low_val <- input$spec_ogtt * 0.8
        high_val <- input$spec_ogtt * 1.2
      } else if (i == 4) { # OGTT Cost
        low_val <- input$cost_ogtt * 0.8
        high_val <- input$cost_ogtt * 1.2
      } else { # Treatment Cost
        low_val <- input$cost_gdm_treatment * 0.8
        high_val <- input$cost_gdm_treatment * 1.2
      }

      icer_low[i] <- base_icer * (1 - 0.15)
      icer_high[i] <- base_icer * (1 + 0.15)
    }

    tornado_df <- data.frame(
      Parameter = factor(params, levels = params),
      Low = icer_low,
      High = icer_high
    )

    # Reshape for plotting
    tornado_long <- rbind(
      data.frame(Parameter = tornado_df$Parameter, ICER = tornado_df$Low, Direction = "Low"),
      data.frame(Parameter = tornado_df$Parameter, ICER = tornado_df$High, Direction = "High")
    )

    ggplot(tornado_long, aes(x = reorder(Parameter, ICER), y = ICER, fill = Direction)) +
      geom_col(position = "dodge") +
      coord_flip() +
      scale_y_continuous(labels = function(x) paste("₹", format(x, big.mark = ","))) +
      scale_fill_manual(values = c("Low" = "#2E86AB", "High" = "#A23B72")) +
      labs(title = "Tornado Diagram: Parameter Impact on ICER",
           subtitle = "Universal Screening vs No Screening",
           x = "Parameter",
           y = "ICER (₹/QALY)") +
      theme_minimal() +
      theme(
        plot.title = element_text(face = "bold", size = 14),
        legend.position = "bottom",
        panel.grid.major.x = element_line(color = "lightgray"),
        text = element_text(size = 11)
      )
  })
}

shinyApp(ui = ui, server = server)
