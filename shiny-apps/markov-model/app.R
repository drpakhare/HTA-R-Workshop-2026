library(shiny)
library(ggplot2)

# Define UI
ui <- fluidPage(
  titlePanel("CKD Markov Model Analysis"),

  sidebarLayout(
    sidebarPanel(
      h3("Transition Probabilities (Annual)"),

      sliderInput("p_12", "P(Stage 2 → Stage 3):",
                  min = 0.05, max = 0.40, value = 0.20, step = 0.01),

      sliderInput("p_23", "P(Stage 3 → Stage 4):",
                  min = 0.05, max = 0.40, value = 0.15, step = 0.01),

      sliderInput("p_1d", "P(Stage 2 → Death):",
                  min = 0.01, max = 0.10, value = 0.02, step = 0.01),

      sliderInput("p_2d", "P(Stage 3 → Death):",
                  min = 0.01, max = 0.10, value = 0.03, step = 0.01),

      sliderInput("p_3d", "P(Stage 4 → Death):",
                  min = 0.01, max = 0.20, value = 0.08, step = 0.01),

      hr(),
      h3("Treatment Effect"),

      sliderInput("treatment_hr", "Hazard Ratio (treatment effect on progression):",
                  min = 0.4, max = 1.0, value = 0.85, step = 0.05),

      hr(),
      h3("Cost & Quality Parameters"),

      sliderInput("cost_stage2", "Annual cost, Stage 2 (₹):",
                  min = 5000, max = 50000, value = 15000, step = 5000),

      sliderInput("cost_stage3", "Annual cost, Stage 3 (₹):",
                  min = 20000, max = 100000, value = 50000, step = 10000),

      sliderInput("cost_stage4", "Annual cost, Stage 4 (₹):",
                  min = 50000, max = 300000, value = 150000, step = 25000),

      sliderInput("cost_dialysis", "Annual cost, Dialysis (₹):",
                  min = 100000, max = 500000, value = 300000, step = 50000),

      sliderInput("util_stage2", "Utility, Stage 2:",
                  min = 0.7, max = 1.0, value = 0.95, step = 0.05),

      sliderInput("util_stage3", "Utility, Stage 3:",
                  min = 0.5, max = 0.95, value = 0.80, step = 0.05),

      sliderInput("util_stage4", "Utility, Stage 4:",
                  min = 0.3, max = 0.80, value = 0.60, step = 0.05),

      sliderInput("util_dialysis", "Utility, Dialysis:",
                  min = 0.3, max = 0.80, value = 0.50, step = 0.05),

      hr(),
      h3("Model Parameters"),

      sliderInput("discount_rate", "Annual discount rate:",
                  min = 0.00, max = 0.10, value = 0.03, step = 0.01),

      sliderInput("time_horizon", "Time horizon (years):",
                  min = 5, max = 30, value = 20, step = 1),

      width = 3
    ),

    mainPanel(
      tabsetPanel(
        tabPanel("Markov Trace",
                 h3("Proportion of Patients in Each State Over Time"),
                 plotOutput("markov_trace")),

        tabPanel("Results Table",
                 h3("Cost-Effectiveness Results (Treatment vs Control)"),
                 tableOutput("results_table"),
                 br(),
                 h4("Incremental Cost-Effectiveness"),
                 tableOutput("icer_table")),

        tabPanel("Dialysis Progression",
                 h3("Cumulative Probability of Reaching Dialysis"),
                 plotOutput("dialysis_curve"))
      )
    )
  )
)

# Define server logic
server <- function(input, output) {

  # Reactive Markov model calculation
  calc_markov <- reactive({

    # Parameters
    p_12 <- input$p_12
    p_23 <- input$p_23
    p_1d <- input$p_1d
    p_2d <- input$p_2d
    p_3d <- input$p_3d
    hr <- input$treatment_hr

    cost_s2 <- input$cost_stage2
    cost_s3 <- input$cost_stage3
    cost_s4 <- input$cost_stage4
    cost_dial <- input$cost_dialysis

    util_s2 <- input$util_stage2
    util_s3 <- input$util_stage3
    util_s4 <- input$util_stage4
    util_dial <- input$util_dialysis

    discount <- input$discount_rate
    horizon <- input$time_horizon

    # Transition matrix for control (untreated)
    # States: S2, S3, S4, Dialysis, Death
    tm_control <- matrix(c(
      1 - p_12 - p_1d,  0,               0,      0,  p_1d,
      p_12,             1 - p_23 - p_2d, 0,      0,  p_2d,
      0,                p_23,            1 - p_3d - 0.10, 0.10, p_3d,  # 10% progress to dialysis
      0,                0,               0,      1,  0,
      0,                0,               0,      0,  1
    ), nrow = 5, byrow = TRUE)

    # Transition matrix for treatment (reduced progression)
    tm_treatment <- tm_control
    tm_treatment[1, 1] <- 1 - (p_12 * hr) - p_1d
    tm_treatment[1, 2] <- p_12 * hr
    tm_treatment[2, 2] <- 1 - (p_23 * hr) - p_2d
    tm_treatment[2, 3] <- p_23 * hr
    tm_treatment[3, 3] <- 1 - (p_3d * hr) - 0.10
    tm_treatment[3, 4] <- 0.10

    # Initial state distribution (all start in Stage 2)
    state_control <- c(1, 0, 0, 0, 0)
    state_treatment <- c(1, 0, 0, 0, 0)

    # Run Markov model
    trace_control <- matrix(NA, nrow = horizon + 1, ncol = 5)
    trace_treatment <- matrix(NA, nrow = horizon + 1, ncol = 5)
    trace_control[1, ] <- state_control
    trace_treatment[1, ] <- state_treatment

    for (t in 2:(horizon + 1)) {
      trace_control[t, ] <- trace_control[t - 1, ] %*% tm_control
      trace_treatment[t, ] <- trace_treatment[t - 1, ] %*% tm_treatment
    }

    # Calculate costs and QALYs
    cost_control <- sum(
      trace_control[, 1] * cost_s2 / (1 + discount) ^ (0:horizon),
      trace_control[, 2] * cost_s3 / (1 + discount) ^ (0:horizon),
      trace_control[, 3] * cost_s4 / (1 + discount) ^ (0:horizon),
      trace_control[, 4] * cost_dial / (1 + discount) ^ (0:horizon)
    )

    qaly_control <- sum(
      trace_control[, 1] * util_s2 / (1 + discount) ^ (0:horizon),
      trace_control[, 2] * util_s3 / (1 + discount) ^ (0:horizon),
      trace_control[, 3] * util_s4 / (1 + discount) ^ (0:horizon),
      trace_control[, 4] * util_dial / (1 + discount) ^ (0:horizon)
    )

    # Treatment costs include intervention cost (simplified: assume upfront cost)
    treatment_cost <- sum(
      trace_treatment[, 1] * cost_s2 / (1 + discount) ^ (0:horizon),
      trace_treatment[, 2] * cost_s3 / (1 + discount) ^ (0:horizon),
      trace_treatment[, 3] * cost_s4 / (1 + discount) ^ (0:horizon),
      trace_treatment[, 4] * cost_dial / (1 + discount) ^ (0:horizon),
      10000 / (1 + discount) ^ 0  # Upfront treatment cost
    )

    qaly_treatment <- sum(
      trace_treatment[, 1] * util_s2 / (1 + discount) ^ (0:horizon),
      trace_treatment[, 2] * util_s3 / (1 + discount) ^ (0:horizon),
      trace_treatment[, 3] * util_s4 / (1 + discount) ^ (0:horizon),
      trace_treatment[, 4] * util_dial / (1 + discount) ^ (0:horizon)
    )

    list(
      trace_control = trace_control,
      trace_treatment = trace_treatment,
      cost_control = cost_control,
      qaly_control = qaly_control,
      cost_treatment = treatment_cost,
      qaly_treatment = qaly_treatment,
      tm_control = tm_control,
      tm_treatment = tm_treatment
    )
  })

  output$markov_trace <- renderPlot({
    markov_results <- calc_markov()
    trace_treatment <- markov_results$trace_treatment
    horizon <- input$time_horizon

    # Prepare data for plotting
    time_points <- 0:horizon
    trace_df <- data.frame(
      Time = rep(time_points, 4),
      State = rep(c("Stage 2", "Stage 3", "Stage 4", "Dialysis"),
                 each = horizon + 1),
      Proportion = c(trace_treatment[, 1], trace_treatment[, 2],
                    trace_treatment[, 3], trace_treatment[, 4])
    )

    trace_df$State <- factor(trace_df$State,
                            levels = c("Stage 2", "Stage 3", "Stage 4", "Dialysis"))

    ggplot(trace_df, aes(x = Time, y = Proportion, fill = State)) +
      geom_area(alpha = 0.7, position = "stack") +
      scale_fill_manual(values = c("Stage 2" = "#2E86AB", "Stage 3" = "#A23B72",
                                   "Stage 4" = "#F18F01", "Dialysis" = "#C73E1D")) +
      labs(title = "Markov Trace: State Occupancy Over Time (Treatment Strategy)",
           x = "Year",
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
    markov_results <- calc_markov()

    results_df <- data.frame(
      Strategy = c("Control (No Treatment)", "Treatment"),
      Total_Cost = c(
        paste("₹", format(round(markov_results$cost_control, 0), big.mark = ",")),
        paste("₹", format(round(markov_results$cost_treatment, 0), big.mark = ","))
      ),
      Total_QALYs = c(
        format(round(markov_results$qaly_control, 2), nsmall = 2),
        format(round(markov_results$qaly_treatment, 2), nsmall = 2)
      ),
      stringsAsFactors = FALSE
    )

    results_df
  }, striped = TRUE, hover = TRUE, bordered = TRUE)

  output$icer_table <- renderTable({
    markov_results <- calc_markov()

    inc_cost <- markov_results$cost_treatment - markov_results$cost_control
    inc_qaly <- markov_results$qaly_treatment - markov_results$qaly_control

    if (inc_qaly != 0) {
      icer <- inc_cost / inc_qaly
      icer_text <- paste("₹", format(round(icer, 0), big.mark = ","))
    } else {
      icer_text <- "Undefined"
    }

    icer_df <- data.frame(
      Comparison = "Treatment vs Control",
      Incremental_Cost = paste("₹", format(round(inc_cost, 0), big.mark = ",")),
      Incremental_QALYs = format(round(inc_qaly, 2), nsmall = 2),
      ICER = icer_text,
      stringsAsFactors = FALSE
    )

    if (inc_cost < 0 & inc_qaly > 0) {
      icer_df$Interpretation <- "Treatment Dominant"
    } else if (inc_cost > 0 & inc_qaly < 0) {
      icer_df$Interpretation <- "Control Dominant"
    } else {
      icer_df$Interpretation <- "Trade-off"
    }

    icer_df
  }, striped = TRUE, hover = TRUE, bordered = TRUE)

  output$dialysis_curve <- renderPlot({
    markov_results <- calc_markov()
    trace_control <- markov_results$trace_control
    trace_treatment <- markov_results$trace_treatment
    horizon <- input$time_horizon

    # Calculate cumulative dialysis proportion
    cum_dialysis_control <- cumsum(trace_control[, 4])
    cum_dialysis_treatment <- cumsum(trace_treatment[, 4])

    dialysis_df <- data.frame(
      Time = 0:horizon,
      Control = cum_dialysis_control,
      Treatment = cum_dialysis_treatment
    )

    ggplot(dialysis_df, aes(x = Time)) +
      geom_line(aes(y = Control, color = "Control"), size = 1.2) +
      geom_line(aes(y = Treatment, color = "Treatment"), size = 1.2) +
      geom_point(aes(y = Control, color = "Control"), size = 2) +
      geom_point(aes(y = Treatment, color = "Treatment"), size = 2) +
      scale_y_continuous(labels = scales::percent) +
      scale_color_manual(values = c("Control" = "#D62828", "Treatment" = "#1B998B")) +
      labs(title = "Cumulative Probability of Reaching Dialysis",
           x = "Year",
           y = "Cumulative Proportion",
           color = "Strategy") +
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
