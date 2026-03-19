library(shiny)
library(ggplot2)

# Define UI
ui <- fluidPage(
  titlePanel("DES vs BMS Stent Decision Tree Analysis"),

  sidebarLayout(
    sidebarPanel(
      h3("Stent Cost Parameters (₹)"),

      sliderInput("cost_des_stent", "Cost of DES stent:",
                  min = 10000, max = 80000, value = 38933, step = 5000),

      sliderInput("cost_bms_stent", "Cost of BMS stent:",
                  min = 5000, max = 30000, value = 10693, step = 2000),

      hr(),
      h3("Clinical Event Probabilities"),

      sliderInput("p_restenosis_des", "P(Restenosis | DES):",
                  min = 0.01, max = 0.20, value = 0.08, step = 0.01),

      sliderInput("p_restenosis_bms", "P(Restenosis | BMS):",
                  min = 0.05, max = 0.40, value = 0.20, step = 0.01),

      sliderInput("p_mace_des", "P(MACE | DES):",
                  min = 0.05, max = 0.25, value = 0.08, step = 0.01),

      sliderInput("p_mace_bms", "P(MACE | BMS):",
                  min = 0.10, max = 0.35, value = 0.15, step = 0.01),

      hr(),
      h3("Procedure and Additional Costs (₹)"),

      sliderInput("cost_pci_procedure", "PCI procedure cost:",
                  min = 50000, max = 200000, value = 120000, step = 10000),

      sliderInput("cost_dapt_des", "DAPT cost, DES (annual):",
                  min = 2000, max = 20000, value = 8000, step = 1000),

      sliderInput("cost_dapt_bms", "DAPT cost, BMS (annual):",
                  min = 1000, max = 10000, value = 3000, step = 500),

      sliderInput("cost_followup", "Annual follow-up cost:",
                  min = 1000, max = 20000, value = 5000, step = 1000),

      hr(),
      h3("Event Costs (₹)"),

      sliderInput("cost_repeat_revasc", "Cost of repeat revascularization:",
                  min = 10000, max = 100000, value = 50000, step = 5000),

      sliderInput("cost_mace", "Cost of MACE (hospitalization, intervention):",
                  min = 50000, max = 500000, value = 200000, step = 25000),

      hr(),
      h3("Cost-Effectiveness Threshold"),

      sliderInput("wtp_threshold", "WTP Threshold (₹/QALY):",
                  min = 50000, max = 500000, value = 170000, step = 10000),

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

    # Procedure and maintenance costs
    cost_pci <- input$cost_pci_procedure
    cost_dapt_des <- input$cost_dapt_des
    cost_dapt_bms <- input$cost_dapt_bms
    cost_followup <- input$cost_followup

    # Event costs
    cost_repeat_revasc <- input$cost_repeat_revasc
    cost_mace <- input$cost_mace

    # Utilities (simplified: uniform across strategies)
    # In a full model, MACE and restenosis would reduce utility
    utility <- 1.0

    # DES Strategy (includes PCI procedure, stent, DAPT, follow-up, and event costs)
    cost_des_strategy <- cost_pci +
                        cost_des +
                        cost_dapt_des +
                        cost_followup +
                        p_restenosis_des * cost_repeat_revasc +
                        p_mace_des * cost_mace
    qaly_des <- utility * (1 - 0.05 * p_mace_des) # Small utility decrement for MACE

    # BMS Strategy (includes PCI procedure, stent, DAPT, follow-up, and event costs)
    cost_bms_strategy <- cost_pci +
                        cost_bms +
                        cost_dapt_bms +
                        cost_followup +
                        p_restenosis_bms * cost_repeat_revasc +
                        p_mace_bms * cost_mace
    qaly_bms <- utility * (1 - 0.05 * p_mace_bms)

    data.frame(
      Strategy = c("DES", "BMS"),
      PCI_Cost = c(cost_pci, cost_pci),
      Stent_Cost = c(cost_des, cost_bms),
      DAPT_Cost = c(cost_dapt_des, cost_dapt_bms),
      Followup_Cost = c(cost_followup, cost_followup),
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
    wtp <- input$wtp_threshold

    # Calculate incremental cost-effectiveness (DES vs BMS)
    inc_cost <- results$Total_Cost[1] - results$Total_Cost[2]
    inc_qaly <- results$QALY[1] - results$QALY[2]

    if (inc_qaly != 0) {
      icer <- inc_cost / inc_qaly
      icer_text <- paste("₹", format(round(icer, 0), big.mark = ","))
    } else {
      icer_text <- "Undefined (equal effectiveness)"
    }

    # Calculate NMB
    nmb <- wtp * inc_qaly - inc_cost

    inc_df <- data.frame(
      Comparison = "DES vs BMS",
      Incremental_Cost = paste("₹", format(round(inc_cost, 0), big.mark = ",")),
      Incremental_QALY = format(round(inc_qaly, 4), nsmall = 4),
      ICER = icer_text,
      NMB = paste("₹", format(round(nmb, 0), big.mark = ",")),
      stringsAsFactors = FALSE
    )

    if (inc_cost < 0 & inc_qaly > 0) {
      inc_df$Interpretation <- "DOMINANT (DES cheaper + better)"
    } else if (inc_cost > 0 & inc_qaly < 0) {
      inc_df$Interpretation <- "DOMINATED (DES costlier + worse)"
    } else if (inc_cost > 0 & inc_qaly > 0) {
      icer_val <- inc_cost / inc_qaly
      if (icer_val < wtp) {
        inc_df$Interpretation <- "Cost-effective"
      } else {
        inc_df$Interpretation <- "Not cost-effective"
      }
    } else {
      inc_df$Interpretation <- "Trade-off (cheaper but worse)"
    }

    inc_df
  }, striped = TRUE, hover = TRUE, bordered = TRUE)

  output$cost_breakdown <- renderPlot({
    results <- calc_outcomes()

    # Prepare data for stacked bar chart with all cost components
    cost_data <- data.frame(
      Strategy = c("DES", "DES", "DES", "DES", "DES", "DES", "DES",
                   "BMS", "BMS", "BMS", "BMS", "BMS", "BMS", "BMS"),
      Component = c("PCI Procedure", "Stent", "DAPT", "Follow-up", "Restenosis", "MACE", "Margin",
                    "PCI Procedure", "Stent", "DAPT", "Follow-up", "Restenosis", "MACE", "Margin"),
      Cost = c(results$PCI_Cost[1],
               results$Stent_Cost[1],
               results$DAPT_Cost[1],
               results$Followup_Cost[1],
               results$Restenosis_Cost[1],
               results$MACE_Cost[1],
               pmax(0, results$Total_Cost[1] - (results$PCI_Cost[1] + results$Stent_Cost[1] +
                                                 results$DAPT_Cost[1] + results$Followup_Cost[1] +
                                                 results$Restenosis_Cost[1] + results$MACE_Cost[1])),
               results$PCI_Cost[2],
               results$Stent_Cost[2],
               results$DAPT_Cost[2],
               results$Followup_Cost[2],
               results$Restenosis_Cost[2],
               results$MACE_Cost[2],
               pmax(0, results$Total_Cost[2] - (results$PCI_Cost[2] + results$Stent_Cost[2] +
                                                 results$DAPT_Cost[2] + results$Followup_Cost[2] +
                                                 results$Restenosis_Cost[2] + results$MACE_Cost[2])))
    )

    cost_data$Component <- factor(cost_data$Component,
                                  levels = c("PCI Procedure", "Stent", "DAPT", "Follow-up", "Restenosis", "MACE", "Margin"))

    ggplot(cost_data, aes(x = Strategy, y = Cost, fill = Component)) +
      geom_col(position = "stack") +
      scale_y_continuous(labels = function(x) paste("₹", format(x, big.mark = ","))) +
      scale_fill_manual(values = c("PCI Procedure" = "#2E86AB",
                                    "Stent" = "#1B998B",
                                    "DAPT" = "#06A77D",
                                    "Follow-up" = "#A23B72",
                                    "Restenosis" = "#F77F00",
                                    "MACE" = "#D62828",
                                    "Margin" = "#E8E8E8")) +
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
    # Break-even: at what DES stent price do total strategy costs equal?
    # DES total = PCI + stent_price + DAPT_DES + followup + resten_DES + mace_DES
    # BMS total is fixed from current parameters

    results <- calc_outcomes()
    bms_total <- results$Total_Cost[2]

    # DES non-stent costs (everything except the stent itself)
    des_fixed <- input$cost_pci_procedure +
                 input$cost_dapt_des +
                 input$cost_followup +
                 input$p_restenosis_des * input$cost_repeat_revasc +
                 input$p_mace_des * input$cost_mace

    # Break-even: des_fixed + stent_price = bms_total
    breakeven_price <- bms_total - des_fixed
    current_price <- input$cost_des_stent

    # Sweep DES stent price
    price_range <- seq(0, 150000, by = 1000)
    des_totals <- des_fixed + price_range

    plot_df <- data.frame(
      DES_Price = rep(price_range, 2),
      Total_Cost = c(des_totals, rep(bms_total, length(price_range))),
      Strategy = rep(c("DES (varies with stent price)", "BMS (fixed)"), each = length(price_range))
    )

    ggplot(plot_df, aes(x = DES_Price, y = Total_Cost, colour = Strategy)) +
      geom_line(linewidth = 1.2) +
      # Break-even point
      geom_vline(xintercept = breakeven_price, linetype = "dashed", colour = "grey40") +
      geom_point(data = data.frame(x = breakeven_price, y = bms_total),
                 aes(x = x, y = y), colour = "black", size = 4, inherit.aes = FALSE) +
      annotate("label", x = breakeven_price, y = bms_total + 15000,
               label = paste0("Break-even\n", intToUtf8(8377),
                              format(round(breakeven_price), big.mark = ",")),
               size = 3.5, fill = "white") +
      # Current NPPA price
      geom_vline(xintercept = current_price, linetype = "dotted", colour = "#e15759") +
      annotate("text", x = current_price, y = max(des_totals) * 0.95,
               label = paste0("NPPA 2025\n", intToUtf8(8377),
                              format(current_price, big.mark = ",")),
               colour = "#e15759", size = 3, hjust = -0.1) +
      scale_colour_manual(values = c("DES (varies with stent price)" = "#4e79a7",
                                      "BMS (fixed)" = "#e15759")) +
      scale_x_continuous(labels = function(x) paste0(intToUtf8(8377), format(x, big.mark = ","))) +
      scale_y_continuous(labels = function(x) paste0(intToUtf8(8377), format(x, big.mark = ","))) +
      labs(title = "Break-Even Analysis: DES Stent Price",
           subtitle = ifelse(current_price < breakeven_price,
                             paste0("Current price (", intToUtf8(8377), format(current_price, big.mark = ","),
                                    ") is BELOW break-even \u2192 DES saves money"),
                             paste0("Current price (", intToUtf8(8377), format(current_price, big.mark = ","),
                                    ") is ABOVE break-even \u2192 DES costs more")),
           x = "DES Stent Price", y = "Total Strategy Cost per Patient", colour = "") +
      theme_minimal() +
      theme(plot.title = element_text(face = "bold"),
            legend.position = "bottom")
  })
}

shinyApp(ui = ui, server = server)
