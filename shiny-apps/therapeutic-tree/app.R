library(shiny)
library(ggplot2)

# Define UI
ui <- fluidPage(
  titlePanel("DES vs BMS Stent Decision Tree Analysis"),

  sidebarLayout(
    sidebarPanel(
      h3("Stent Cost Parameters (₹)"),

      sliderInput("cost_des_stent", "Cost of DES stent:",
                  min = 10000, max = 80000, value = 50000, step = 5000),

      sliderInput("cost_bms_stent", "Cost of BMS stent:",
                  min = 5000, max = 30000, value = 15000, step = 2000),

      hr(),
      h3("Clinical Event Probabilities"),

      sliderInput("p_restenosis_des", "P(Restenosis | DES):",
                  min = 0.01, max = 0.20, value = 0.08, step = 0.01),

      sliderInput("p_restenosis_bms", "P(Restenosis | BMS):",
                  min = 0.05, max = 0.40, value = 0.20, step = 0.01),

      sliderInput("p_mace_des", "P(MACE | DES):",
                  min = 0.05, max = 0.25, value = 0.12, step = 0.01),

      sliderInput("p_mace_bms", "P(MACE | BMS):",
                  min = 0.10, max = 0.35, value = 0.18, step = 0.01),

      hr(),
      h3("Additional Costs (₹)"),

      sliderInput("cost_repeat_revasc", "Cost of repeat revascularization:",
                  min = 10000, max = 100000, value = 50000, step = 5000),

      sliderInput("cost_mace", "Cost of MACE (hospitalization, intervention):",
                  min = 50000, max = 500000, value = 200000, step = 25000),

      width = 3
    ),

    mainPanel(
      tabsetPanel(
        tabPanel("Results Comparison",
                 h3("Cost-Effectiveness Results"),
                 tableOutput("results_table"),
                 br(),
                 h4("Incremental Analysis"),
                 tableOutput("incremental_table")),

        tabPanel("Cost Breakdown",
                 h3("Detailed Cost Components"),
                 plotOutput("cost_breakdown")),

        tabPanel("Threshold Analysis",
                 h3("Break-even Analysis: Cost of DES"),
                 plotOutput("threshold_plot"))
      )
    )
  )
)

# Define server logic
server <- function(input, output) {

  # Reactive calculation of outcomes
  calc_outcomes <- reactive({

    # Stent costs
    cost_des <- input$cost_des_stent
    cost_bms <- input$cost_bms_stent

    # Event probabilities
    p_restenosis_des <- input$p_restenosis_des
    p_restenosis_bms <- input$p_restenosis_bms
    p_mace_des <- input$p_mace_des
    p_mace_bms <- input$p_mace_bms

    # Additional costs
    cost_repeat_revasc <- input$cost_repeat_revasc
    cost_mace <- input$cost_mace

    # Utilities (simplified: uniform across strategies)
    # In a full model, MACE and restenosis would reduce utility
    utility <- 1.0

    # DES Strategy
    cost_des_strategy <- cost_des +
                        p_restenosis_des * cost_repeat_revasc +
                        p_mace_des * cost_mace
    qaly_des <- utility * (1 - 0.05 * p_mace_des) # Small utility decrement for MACE

    # BMS Strategy
    cost_bms_strategy <- cost_bms +
                        p_restenosis_bms * cost_repeat_revasc +
                        p_mace_bms * cost_mace
    qaly_bms <- utility * (1 - 0.05 * p_mace_bms)

    data.frame(
      Strategy = c("DES", "BMS"),
      Stent_Cost = c(cost_des, cost_bms),
      Restenosis_Cost = c(p_restenosis_des * cost_repeat_revasc,
                         p_restenosis_bms * cost_repeat_revasc),
      MACE_Cost = c(p_mace_des * cost_mace,
                   p_mace_bms * cost_mace),
      Total_Cost = c(cost_des_strategy, cost_bms_strategy),
      QALY = c(qaly_des, qaly_bms),
      stringsAsFactors = FALSE
    )
  })

  output$results_table <- renderTable({
    results <- calc_outcomes()
    results_display <- data.frame(
      Strategy = results$Strategy,
      Total_Cost = paste("₹", format(round(results$Total_Cost, 0), big.mark = ",")),
      QALY = format(round(results$QALY, 4), nsmall = 4),
      stringsAsFactors = FALSE
    )
    results_display
  }, striped = TRUE, hover = TRUE, bordered = TRUE)

  output$incremental_table <- renderTable({
    results <- calc_outcomes()

    # Calculate incremental cost-effectiveness (DES vs BMS)
    inc_cost <- results$Total_Cost[1] - results$Total_Cost[2]
    inc_qaly <- results$QALY[1] - results$QALY[2]

    if (inc_qaly != 0) {
      icer <- inc_cost / inc_qaly
      icer_text <- paste("₹", format(round(icer, 0), big.mark = ","))
    } else {
      icer_text <- "Undefined (equal effectiveness)"
    }

    inc_df <- data.frame(
      Comparison = "DES vs BMS",
      Incremental_Cost = paste("₹", format(round(inc_cost, 0), big.mark = ",")),
      Incremental_QALY = format(round(inc_qaly, 4), nsmall = 4),
      ICER = icer_text,
      stringsAsFactors = FALSE
    )

    if (inc_cost < 0 & inc_qaly > 0) {
      inc_df$Interpretation <- "DES Dominant"
    } else if (inc_cost > 0 & inc_qaly < 0) {
      inc_df$Interpretation <- "BMS Dominant"
    } else {
      inc_df$Interpretation <- "Trade-off"
    }

    inc_df
  }, striped = TRUE, hover = TRUE, bordered = TRUE)

  output$cost_breakdown <- renderPlot({
    results <- calc_outcomes()

    # Prepare data for stacked bar chart
    cost_data <- data.frame(
      Strategy = c("DES", "DES", "DES", "BMS", "BMS", "BMS"),
      Component = c("Stent", "Restenosis", "MACE", "Stent", "Restenosis", "MACE"),
      Cost = c(results$Stent_Cost[1],
               results$Restenosis_Cost[1],
               results$MACE_Cost[1],
               results$Stent_Cost[2],
               results$Restenosis_Cost[2],
               results$MACE_Cost[2])
    )

    cost_data$Component <- factor(cost_data$Component,
                                  levels = c("Stent", "Restenosis", "MACE"))

    ggplot(cost_data, aes(x = Strategy, y = Cost, fill = Component)) +
      geom_col(position = "stack") +
      scale_y_continuous(labels = function(x) paste("₹", format(x, big.mark = ","))) +
      scale_fill_manual(values = c("Stent" = "#1B998B", "Restenosis" = "#F77F00", "MACE" = "#D62828")) +
      labs(title = "Cost Breakdown by Component",
           x = "Stent Type",
           y = "Cost (₹)") +
      theme_minimal() +
      theme(
        plot.title = element_text(face = "bold", size = 14),
        legend.position = "right",
        panel.grid.major.y = element_line(color = "lightgray"),
        text = element_text(size = 11)
      )
  })

  output$threshold_plot <- renderPlot({
    # Threshold analysis: at what DES cost are the two strategies equivalent?
    # This is a simplified break-even analysis

    bms_results <- calc_outcomes()
    bms_total <- bms_results$Total_Cost[2]

    # Restenosis and MACE costs are the same
    restenosis_mace_cost_des <- input$p_restenosis_des * input$cost_repeat_revasc +
                                input$p_mace_des * input$cost_mace

    # Break-even DES stent cost
    breakeven_des_cost <- bms_total - restenosis_mace_cost_des

    # Create a range of DES costs
    des_cost_range <- seq(5000, 100000, by = 2500)

    des_total_costs <- des_cost_range + restenosis_mace_cost_des
    bms_cost_constant <- rep(bms_total, length(des_cost_range))

    threshold_df <- data.frame(
      DES_Stent_Cost = des_cost_range,
      DES_Total = des_total_costs,
      BMS_Total = bms_cost_constant
    )

    ggplot(threshold_df, aes(x = DES_Stent_Cost)) +
      geom_line(aes(y = DES_Total, color = "DES Strategy"), size = 1) +
      geom_line(aes(y = BMS_Total, color = "BMS Strategy"), size = 1) +
      geom_vline(xintercept = breakeven_des_cost, linetype = "dashed", color = "gray50") +
      geom_label(aes(x = breakeven_des_cost, y = bms_total + 50000),
                 label = paste("Break-even:\n₹", format(round(breakeven_des_cost, 0), big.mark = ",")),
                 size = 3.5) +
      scale_x_continuous(labels = function(x) paste("₹", format(x, big.mark = ","))) +
      scale_y_continuous(labels = function(x) paste("₹", format(x, big.mark = ","))) +
      scale_color_manual(values = c("DES Strategy" = "#2E86AB", "BMS Strategy" = "#A23B72")) +
      labs(title = "Break-Even Analysis: Cost of DES Stent",
           subtitle = paste("DES becomes cost-equivalent to BMS at ₹",
                           format(round(breakeven_des_cost, 0), big.mark = ",")),
           x = "DES Stent Cost",
           y = "Total Cost per Patient",
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
