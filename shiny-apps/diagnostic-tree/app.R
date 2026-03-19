library(shiny)
library(ggplot2)

ui <- fluidPage(
  titlePanel("GDM Diagnostic Decision Tree — Interactive Analysis"),

  sidebarLayout(
    sidebarPanel(
      h4("Epidemiology"),
      sliderInput("prev_gdm", "GDM Prevalence:",
                  min = 0.05, max = 0.25, value = 0.12, step = 0.01),
      sliderInput("prop_high_risk", "Proportion classified high-risk:",
                  min = 0.2, max = 0.9, value = 0.60, step = 0.05),
      sliderInput("prev_gdm_hr", "GDM prevalence in high-risk:",
                  min = 0.05, max = 0.35, value = 0.18, step = 0.01),
      sliderInput("prev_gdm_lr", "GDM prevalence in low-risk:",
                  min = 0.01, max = 0.10, value = 0.03, step = 0.01),

      hr(), h4("Test Accuracy"),
      sliderInput("sens_ogtt", "OGTT Sensitivity:",
                  min = 0.60, max = 1.0, value = 0.90, step = 0.01),
      sliderInput("spec_ogtt", "OGTT Specificity:",
                  min = 0.60, max = 1.0, value = 0.85, step = 0.01),
      sliderInput("detect_clinical", "Clinical detection rate (no screening):",
                  min = 0.10, max = 0.50, value = 0.25, step = 0.05),

      hr(), h4("Costs (₹)"),
      sliderInput("cost_ogtt", "OGTT cost:", min = 100, max = 1000, value = 250, step = 50),
      sliderInput("cost_risk_assess", "Risk assessment cost:", min = 0, max = 200, value = 50, step = 10),
      sliderInput("cost_gdm_treatment", "GDM treatment:", min = 2000, max = 20000, value = 8000, step = 1000),
      sliderInput("cost_adverse", "Adverse outcome:", min = 10000, max = 100000, value = 45000, step = 5000),
      sliderInput("cost_no_adverse", "Normal delivery:", min = 1000, max = 15000, value = 5000, step = 1000),

      hr(), h4("Outcome Probabilities"),
      sliderInput("p_adv_treated", "P(adverse | treated GDM):", min = 0.02, max = 0.25, value = 0.10, step = 0.01),
      sliderInput("p_adv_untreated", "P(adverse | untreated GDM):", min = 0.10, max = 0.60, value = 0.35, step = 0.01),
      sliderInput("p_adv_no_gdm", "P(adverse | no GDM):", min = 0.01, max = 0.15, value = 0.05, step = 0.01),

      hr(), h4("Utilities"),
      sliderInput("u_adverse", "Utility: adverse outcome:", min = 0.3, max = 0.8, value = 0.65, step = 0.01),
      sliderInput("u_no_adverse", "Utility: no adverse:", min = 0.7, max = 1.0, value = 0.90, step = 0.01),
      sliderInput("u_gdm_treated", "Utility: managed GDM:", min = 0.6, max = 0.95, value = 0.82, step = 0.01),

      hr(), h4("Decision Threshold"),
      sliderInput("wtp", "WTP (₹/QALY):", min = 50000, max = 500000, value = 170000, step = 10000),

      width = 3
    ),

    mainPanel(
      tabsetPanel(
        tabPanel("Results",
                 h3("Cost-Effectiveness Comparison"),
                 tableOutput("results_table"),
                 br(),
                 h4("Incremental Analysis (vs No Screening)"),
                 tableOutput("icer_table"),
                 br(),
                 h4("Net Monetary Benefit Ranking"),
                 tableOutput("nmb_table")),
        tabPanel("CE Plane", plotOutput("ce_plane", height = "500px")),
        tabPanel("Sensitivity Analysis", plotOutput("tornado_plot", height = "500px"))
      )
    )
  )
)

server <- function(input, output) {

  # Core model function — mirrors the Session 3 QMD logic exactly
  run_model <- function(prev, sens, spec, prop_hr, prev_hr, prev_lr, detect_clin,
                        cost_ogtt, cost_risk, cost_treat, cost_adv, cost_noadv,
                        p_adv_tr, p_adv_untr, p_adv_no,
                        u_adv, u_noadv, u_gdm_tr) {

    # ---- UNIVERSAL SCREENING ----
    n_gdm <- prev; n_no_gdm <- 1 - prev
    tp <- n_gdm * sens
    fn <- n_gdm * (1 - sens)
    fp <- n_no_gdm * (1 - spec)
    tn <- n_no_gdm * spec

    cost_univ <- cost_ogtt +
      (tp + fp) * cost_treat +
      (tp * p_adv_tr + fn * p_adv_untr + fp * p_adv_no + tn * p_adv_no) * cost_adv +
      (tp * (1 - p_adv_tr) + fn * (1 - p_adv_untr) + fp * (1 - p_adv_no) + tn * (1 - p_adv_no)) * cost_noadv

    qaly_univ <- tp * ((1 - p_adv_tr) * u_gdm_tr + p_adv_tr * u_adv) +
      fn * ((1 - p_adv_untr) * u_noadv + p_adv_untr * u_adv) +
      fp * ((1 - p_adv_no) * u_gdm_tr + p_adv_no * u_adv) +
      tn * ((1 - p_adv_no) * u_noadv + p_adv_no * u_adv)

    # ---- RISK-BASED SCREENING ----
    n_hr <- prop_hr; n_lr <- 1 - prop_hr
    # High-risk arm (screened)
    n_gdm_hr <- n_hr * prev_hr; n_no_gdm_hr <- n_hr * (1 - prev_hr)
    tp_hr <- n_gdm_hr * sens
    fn_hr <- n_gdm_hr * (1 - sens)
    fp_hr <- n_no_gdm_hr * (1 - spec)
    tn_hr <- n_no_gdm_hr * spec
    # Low-risk arm (not screened)
    n_gdm_lr <- n_lr * prev_lr; n_no_gdm_lr <- n_lr * (1 - prev_lr)

    cost_risk_strat <- n_hr * cost_ogtt + cost_risk +
      (tp_hr + fp_hr) * cost_treat +
      (tp_hr * p_adv_tr + fn_hr * p_adv_untr + fp_hr * p_adv_no + tn_hr * p_adv_no +
         n_gdm_lr * p_adv_untr + n_no_gdm_lr * p_adv_no) * cost_adv +
      (tp_hr * (1 - p_adv_tr) + fn_hr * (1 - p_adv_untr) + fp_hr * (1 - p_adv_no) + tn_hr * (1 - p_adv_no) +
         n_gdm_lr * (1 - p_adv_untr) + n_no_gdm_lr * (1 - p_adv_no)) * cost_noadv

    qaly_risk_strat <- tp_hr * ((1 - p_adv_tr) * u_gdm_tr + p_adv_tr * u_adv) +
      fn_hr * ((1 - p_adv_untr) * u_noadv + p_adv_untr * u_adv) +
      fp_hr * ((1 - p_adv_no) * u_gdm_tr + p_adv_no * u_adv) +
      tn_hr * ((1 - p_adv_no) * u_noadv + p_adv_no * u_adv) +
      n_gdm_lr * ((1 - p_adv_untr) * u_noadv + p_adv_untr * u_adv) +
      n_no_gdm_lr * ((1 - p_adv_no) * u_noadv + p_adv_no * u_adv)

    # ---- NO SCREENING ----
    det <- prev * detect_clin
    undet <- prev * (1 - detect_clin)
    no_gdm_ns <- 1 - prev

    cost_none <- det * cost_treat +
      (det * p_adv_tr + undet * p_adv_untr + no_gdm_ns * p_adv_no) * cost_adv +
      (det * (1 - p_adv_tr) + undet * (1 - p_adv_untr) + no_gdm_ns * (1 - p_adv_no)) * cost_noadv

    qaly_none <- det * ((1 - p_adv_tr) * u_gdm_tr + p_adv_tr * u_adv) +
      undet * ((1 - p_adv_untr) * u_noadv + p_adv_untr * u_adv) +
      no_gdm_ns * ((1 - p_adv_no) * u_noadv + p_adv_no * u_adv)

    data.frame(
      Strategy = c("Universal Screening", "Risk-Based Screening", "No Screening"),
      Cost = c(cost_univ, cost_risk_strat, cost_none),
      QALY = c(qaly_univ, qaly_risk_strat, qaly_none),
      stringsAsFactors = FALSE
    )
  }

  # Reactive: run with current slider values
  calc_outcomes <- reactive({
    run_model(input$prev_gdm, input$sens_ogtt, input$spec_ogtt,
              input$prop_high_risk, input$prev_gdm_hr, input$prev_gdm_lr,
              input$detect_clinical,
              input$cost_ogtt, input$cost_risk_assess, input$cost_gdm_treatment,
              input$cost_adverse, input$cost_no_adverse,
              input$p_adv_treated, input$p_adv_untreated, input$p_adv_no_gdm,
              input$u_adverse, input$u_no_adverse, input$u_gdm_treated)
  })

  # Results table
  output$results_table <- renderTable({
    r <- calc_outcomes()
    data.frame(
      Strategy = r$Strategy,
      `Cost per Woman (₹)` = paste0("₹", format(round(r$Cost), big.mark = ",")),
      `QALYs per Woman` = format(round(r$QALY, 4), nsmall = 4),
      check.names = FALSE, stringsAsFactors = FALSE
    )
  }, striped = TRUE, hover = TRUE, bordered = TRUE)

  # ICER table
  output$icer_table <- renderTable({
    r <- calc_outcomes()
    wtp <- input$wtp

    interpret <- function(dc, dq, wtp) {
      if (dc < 0 & dq > 0) return("DOMINANT")
      if (dc > 0 & dq < 0) return("DOMINATED")
      if (abs(dq) < 1e-8) return("No QALY difference")
      icer <- dc / dq
      if (icer < wtp) return("Cost-effective") else return("Not cost-effective")
    }

    comparisons <- data.frame(
      Comparison = c("Universal vs None", "Risk-Based vs None"),
      `Incr Cost (₹)` = c(
        paste0("₹", format(round(r$Cost[1] - r$Cost[3]), big.mark = ",")),
        paste0("₹", format(round(r$Cost[2] - r$Cost[3]), big.mark = ","))
      ),
      `Incr QALYs` = c(
        format(round(r$QALY[1] - r$QALY[3], 4), nsmall = 4),
        format(round(r$QALY[2] - r$QALY[3], 4), nsmall = 4)
      ),
      ICER = c(
        ifelse(abs(r$QALY[1] - r$QALY[3]) < 1e-8, "—",
               paste0("₹", format(round((r$Cost[1] - r$Cost[3]) / (r$QALY[1] - r$QALY[3])), big.mark = ","))),
        ifelse(abs(r$QALY[2] - r$QALY[3]) < 1e-8, "—",
               paste0("₹", format(round((r$Cost[2] - r$Cost[3]) / (r$QALY[2] - r$QALY[3])), big.mark = ",")))
      ),
      Conclusion = c(
        interpret(r$Cost[1] - r$Cost[3], r$QALY[1] - r$QALY[3], wtp),
        interpret(r$Cost[2] - r$Cost[3], r$QALY[2] - r$QALY[3], wtp)
      ),
      check.names = FALSE, stringsAsFactors = FALSE
    )
    comparisons
  }, striped = TRUE, hover = TRUE, bordered = TRUE)

  # NMB table
  output$nmb_table <- renderTable({
    r <- calc_outcomes()
    wtp <- input$wtp
    nmb <- wtp * r$QALY - r$Cost
    rank_order <- rank(-nmb)

    data.frame(
      Strategy = r$Strategy,
      `NMB (₹)` = paste0("₹", format(round(nmb), big.mark = ",")),
      Rank = rank_order,
      Recommendation = ifelse(rank_order == 1, "★ OPTIMAL", ""),
      check.names = FALSE, stringsAsFactors = FALSE
    )
  }, striped = TRUE, hover = TRUE, bordered = TRUE)

  # CE Plane
  output$ce_plane <- renderPlot({
    r <- calc_outcomes()
    wtp <- input$wtp

    # Plot all 3 strategies on cost-QALY plane
    ggplot(r, aes(x = QALY, y = Cost, colour = Strategy)) +
      geom_point(size = 6) +
      geom_text(aes(label = Strategy), vjust = -1.2, size = 3.5, show.legend = FALSE) +
      geom_abline(intercept = r$Cost[3] - wtp * r$QALY[3], slope = wtp,
                  linetype = "dashed", colour = "red", linewidth = 0.5) +
      annotate("text", x = max(r$QALY), y = r$Cost[3] + wtp * (max(r$QALY) - r$QALY[3]),
               label = paste0("WTP = ₹", format(wtp, big.mark = ",")),
               colour = "red", hjust = 1, vjust = -0.5, size = 3) +
      scale_colour_manual(values = c("Universal Screening" = "#59a14f",
                                      "Risk-Based Screening" = "#f28e2b",
                                      "No Screening" = "#e15759")) +
      scale_y_continuous(labels = function(x) paste0("₹", format(round(x), big.mark = ","))) +
      labs(title = "Cost-Effectiveness Plane",
           subtitle = "Each point = one screening strategy (per woman)",
           x = "QALYs per Woman", y = "Cost per Woman (₹)") +
      theme_minimal() +
      theme(legend.position = "bottom", plot.title = element_text(face = "bold"))
  })

  # REAL Tornado Diagram — actually re-runs the model
  output$tornado_plot <- renderPlot({
    # Base case
    base <- calc_outcomes()
    wtp <- input$wtp
    base_nmb <- wtp * (base$QALY[1] - base$QALY[3]) - (base$Cost[1] - base$Cost[3])

    # Parameters to vary ±20%
    param_list <- list(
      list(name = "GDM Prevalence", lo = input$prev_gdm * 0.8, hi = input$prev_gdm * 1.2,
           arg = "prev_gdm"),
      list(name = "OGTT Sensitivity", lo = max(input$sens_ogtt * 0.8, 0.5), hi = min(input$sens_ogtt * 1.2, 1),
           arg = "sens_ogtt"),
      list(name = "OGTT Specificity", lo = max(input$spec_ogtt * 0.8, 0.5), hi = min(input$spec_ogtt * 1.2, 1),
           arg = "spec_ogtt"),
      list(name = "OGTT Cost", lo = input$cost_ogtt * 0.5, hi = input$cost_ogtt * 2,
           arg = "cost_ogtt"),
      list(name = "Treatment Cost", lo = input$cost_gdm_treatment * 0.5, hi = input$cost_gdm_treatment * 2,
           arg = "cost_gdm_treatment"),
      list(name = "Adverse Cost", lo = input$cost_adverse * 0.5, hi = input$cost_adverse * 2,
           arg = "cost_adverse"),
      list(name = "P(adverse|treated)", lo = input$p_adv_treated * 0.5, hi = min(input$p_adv_treated * 2, 0.5),
           arg = "p_adv_treated"),
      list(name = "P(adverse|untreated)", lo = input$p_adv_untreated * 0.5, hi = min(input$p_adv_untreated * 2, 0.9),
           arg = "p_adv_untreated")
    )

    # Base args
    base_args <- list(
      prev = input$prev_gdm, sens = input$sens_ogtt, spec = input$spec_ogtt,
      prop_hr = input$prop_high_risk, prev_hr = input$prev_gdm_hr, prev_lr = input$prev_gdm_lr,
      detect_clin = input$detect_clinical,
      cost_ogtt = input$cost_ogtt, cost_risk = input$cost_risk_assess,
      cost_treat = input$cost_gdm_treatment,
      cost_adv = input$cost_adverse, cost_noadv = input$cost_no_adverse,
      p_adv_tr = input$p_adv_treated, p_adv_untr = input$p_adv_untreated,
      p_adv_no = input$p_adv_no_gdm,
      u_adv = input$u_adverse, u_noadv = input$u_no_adverse, u_gdm_tr = input$u_gdm_treated
    )

    arg_map <- c(prev_gdm = "prev", sens_ogtt = "sens", spec_ogtt = "spec",
                 cost_ogtt = "cost_ogtt", cost_gdm_treatment = "cost_treat",
                 cost_adverse = "cost_adv", p_adv_treated = "p_adv_tr",
                 p_adv_untreated = "p_adv_untr")

    nmb_lo <- nmb_hi <- numeric(length(param_list))

    for (i in seq_along(param_list)) {
      p <- param_list[[i]]
      mapped <- arg_map[p$arg]

      # Low
      args_lo <- base_args
      args_lo[[mapped]] <- p$lo
      r_lo <- do.call(run_model, args_lo)
      nmb_lo[i] <- wtp * (r_lo$QALY[1] - r_lo$QALY[3]) - (r_lo$Cost[1] - r_lo$Cost[3])

      # High
      args_hi <- base_args
      args_hi[[mapped]] <- p$hi
      r_hi <- do.call(run_model, args_hi)
      nmb_hi[i] <- wtp * (r_hi$QALY[1] - r_hi$QALY[3]) - (r_hi$Cost[1] - r_hi$Cost[3])
    }

    tornado_df <- data.frame(
      Parameter = sapply(param_list, `[[`, "name"),
      Low = nmb_lo - base_nmb,
      High = nmb_hi - base_nmb,
      Swing = abs(nmb_hi - nmb_lo),
      stringsAsFactors = FALSE
    )
    tornado_df <- tornado_df[order(tornado_df$Swing), ]
    tornado_df$Parameter <- factor(tornado_df$Parameter, levels = tornado_df$Parameter)

    ggplot(tornado_df) +
      geom_segment(aes(x = Low, xend = High, y = Parameter, yend = Parameter),
                   linewidth = 8, colour = "#4e79a7", alpha = 0.8) +
      geom_vline(xintercept = 0, linetype = "dashed", colour = "red") +
      scale_x_continuous(labels = function(x) paste0("₹", format(round(x), big.mark = ","))) +
      labs(title = "Tornado Diagram: Incremental NMB (Universal vs No Screening)",
           subtitle = paste0("Base NMB = ₹", format(round(base_nmb), big.mark = ","),
                             " | WTP = ₹", format(wtp, big.mark = ",")),
           x = "Change in Incremental NMB (₹)", y = "") +
      theme_minimal() +
      theme(plot.title = element_text(face = "bold"),
            axis.text.y = element_text(size = 11))
  })
}

shinyApp(ui = ui, server = server)
