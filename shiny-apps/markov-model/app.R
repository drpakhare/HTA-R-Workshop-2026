library(shiny)
library(ggplot2)
library(tidyr)

# ============================================================
# CKD 4-State Markov Cohort Model — Interactive Shiny App
# Matches Session 5 QMD parameters exactly
# ============================================================

ui <- fluidPage(
  titlePanel("CKD 4-State Markov Cohort Model"),
  sidebarLayout(
    sidebarPanel(
      h4("Transition Probabilities (Standard Care)"),
      sliderInput("p_12", "Early → Moderate (p₁₂):", min = 0.02, max = 0.25, value = 0.10, step = 0.01),
      sliderInput("p_1d", "Early → Death (p₁d):", min = 0.005, max = 0.10, value = 0.02, step = 0.005),
      sliderInput("p_23", "Moderate → Advanced (p₂₃):", min = 0.02, max = 0.25, value = 0.12, step = 0.01),
      sliderInput("p_2d", "Moderate → Death (p₂d):", min = 0.01, max = 0.15, value = 0.04, step = 0.01),
      sliderInput("p_3d", "Advanced → Death (p₃d):", min = 0.05, max = 0.30, value = 0.15, step = 0.01),

      hr(), h4("Treatment Effect"),
      sliderInput("hr", "Hazard Ratio (ACE-inhibitor):", min = 0.30, max = 1.00, value = 0.66, step = 0.02),

      hr(), h4("Costs (₹/year)"),
      sliderInput("cost_early", "Early CKD:", min = 5000, max = 30000, value = 12000, step = 1000),
      sliderInput("cost_moderate", "Moderate CKD:", min = 20000, max = 100000, value = 45000, step = 5000),
      sliderInput("cost_advanced", "Advanced/Dialysis:", min = 100000, max = 800000, value = 350000, step = 25000),
      sliderInput("cost_ace", "ACE-inhibitor (annual):", min = 2000, max = 15000, value = 6000, step = 1000),
      sliderInput("cost_screen", "Screening (one-time):", min = 100, max = 2000, value = 500, step = 100),

      hr(), h4("Utilities"),
      sliderInput("u_early", "Early CKD:", min = 0.5, max = 1.0, value = 0.85, step = 0.01),
      sliderInput("u_moderate", "Moderate CKD:", min = 0.4, max = 0.9, value = 0.72, step = 0.01),
      sliderInput("u_advanced", "Advanced/Dialysis:", min = 0.2, max = 0.7, value = 0.55, step = 0.01),

      hr(), h4("Model Settings"),
      sliderInput("horizon", "Time horizon (years):", min = 5, max = 40, value = 20, step = 1),
      sliderInput("discount", "Discount rate:", min = 0, max = 0.10, value = 0.03, step = 0.005),
      sliderInput("wtp", "WTP threshold (₹/QALY):", min = 50000, max = 500000, value = 170000, step = 10000),
      sliderInput("n_cohort", "Cohort size:", min = 1000, max = 50000, value = 10000, step = 1000),

      hr(),
      actionButton("run_psa", "Run PSA (1,000 iterations)", class = "btn-primary btn-block"),
      width = 3
    ),

    mainPanel(
      tabsetPanel(
        tabPanel("Base Case",
                 h3("Cost-Effectiveness Results"),
                 tableOutput("results_table"),
                 br(), tableOutput("icer_table"),
                 br(), tableOutput("nmb_table")),
        tabPanel("Markov Trace", plotOutput("trace_plot", height = "450px"),
                 br(), plotOutput("dialysis_plot", height = "350px")),
        tabPanel("Sensitivity", plotOutput("tornado_plot", height = "500px")),
        tabPanel("PSA",
                 conditionalPanel("output.psa_done",
                   h4("PSA Summary"), tableOutput("psa_summary"),
                   plotOutput("ce_plane", height = "400px"),
                   plotOutput("ceac_plot", height = "400px")
                 ),
                 conditionalPanel("!output.psa_done",
                   h4("Click 'Run PSA' in the sidebar to generate results.")
                 ))
      )
    )
  )
)

server <- function(input, output) {

  # ---- Core Markov engine (mirrors Session 5 QMD exactly) ----
  run_markov <- function(p_12, p_1d, p_23, p_2d, p_3d, hr,
                         cost_early, cost_moderate, cost_advanced, cost_ace, cost_screen,
                         u_early, u_moderate, u_advanced,
                         horizon, disc_rate, n_cohort) {

    p_12_int <- 1 - (1 - p_12)^hr
    p_23_int <- 1 - (1 - p_23)^hr

    tm_std <- matrix(c(
      1-p_12-p_1d, p_12, 0, p_1d,
      0, 1-p_23-p_2d, p_23, p_2d,
      0, 0, 1-p_3d, p_3d,
      0, 0, 0, 1), nrow = 4, byrow = TRUE)

    tm_int <- matrix(c(
      1-p_12_int-p_1d, p_12_int, 0, p_1d,
      0, 1-p_23_int-p_2d, p_23_int, p_2d,
      0, 0, 1-p_3d, p_3d,
      0, 0, 0, 1), nrow = 4, byrow = TRUE)

    costs_std <- c(cost_early, cost_moderate, cost_advanced, 0)
    costs_int <- c(cost_early + cost_ace, cost_moderate + cost_ace, cost_advanced, 0)
    utils <- c(u_early, u_moderate, u_advanced, 0)

    trace_s <- matrix(0, nrow = horizon + 1, ncol = 4)
    trace_i <- matrix(0, nrow = horizon + 1, ncol = 4)
    trace_s[1, 1] <- n_cohort; trace_i[1, 1] <- n_cohort

    for (t in 1:horizon) {
      trace_s[t+1, ] <- trace_s[t, ] %*% tm_std
      trace_i[t+1, ] <- trace_i[t, ] %*% tm_int
    }

    disc <- 1 / (1 + disc_rate)^(0:(horizon - 1))

    tc_s <- 0; tq_s <- 0; tc_i <- 0; tq_i <- 0
    for (t in 1:horizon) {
      hcc_s <- (trace_s[t, ] + trace_s[t+1, ]) / 2
      hcc_i <- (trace_i[t, ] + trace_i[t+1, ]) / 2
      tc_s <- tc_s + sum(hcc_s * costs_std) * disc[t]
      tq_s <- tq_s + sum(hcc_s * utils) * disc[t]
      tc_i <- tc_i + sum(hcc_i * costs_int) * disc[t]
      tq_i <- tq_i + sum(hcc_i * utils) * disc[t]
    }
    tc_i <- tc_i + n_cohort * cost_screen

    list(cost_std = tc_s, qaly_std = tq_s, cost_int = tc_i, qaly_int = tq_i,
         trace_std = trace_s, trace_int = trace_i)
  }

  # ---- Reactive base case ----
  base <- reactive({
    run_markov(input$p_12, input$p_1d, input$p_23, input$p_2d, input$p_3d, input$hr,
               input$cost_early, input$cost_moderate, input$cost_advanced,
               input$cost_ace, input$cost_screen,
               input$u_early, input$u_moderate, input$u_advanced,
               input$horizon, input$discount, input$n_cohort)
  })

  # ---- Results table ----
  output$results_table <- renderTable({
    b <- base(); n <- input$n_cohort
    data.frame(
      Strategy = c("Standard Care", "Screening + ACE-inhibitor"),
      `Cost/Patient` = c(
        paste0(intToUtf8(8377), format(round(b$cost_std / n), big.mark = ",")),
        paste0(intToUtf8(8377), format(round(b$cost_int / n), big.mark = ","))),
      `QALYs/Patient` = c(round(b$qaly_std / n, 4), round(b$qaly_int / n, 4)),
      check.names = FALSE, stringsAsFactors = FALSE)
  }, striped = TRUE, bordered = TRUE)

  output$icer_table <- renderTable({
    b <- base(); n <- input$n_cohort; wtp <- input$wtp
    dc <- (b$cost_int - b$cost_std) / n
    dq <- (b$qaly_int - b$qaly_std) / n
    icer_val <- if (abs(dq) > 1e-8) dc / dq else NA
    interp <- if (dc < 0 & dq > 0) "DOMINANT"
              else if (dc > 0 & dq < 0) "DOMINATED"
              else if (!is.na(icer_val) && icer_val < wtp) "Cost-effective"
              else "Not cost-effective"
    nmb <- wtp * dq - dc
    rupee <- intToUtf8(8377)
    data.frame(
      Metric = c("Incremental Cost", "Incremental QALYs", "ICER", "Incremental NMB", "Conclusion"),
      Value = c(paste0(rupee, format(round(dc), big.mark = ",")),
                round(dq, 4),
                ifelse(is.na(icer_val), "\u2014", paste0(rupee, format(round(icer_val), big.mark = ","))),
                paste0(rupee, format(round(nmb), big.mark = ",")),
                interp),
      check.names = FALSE, stringsAsFactors = FALSE)
  }, striped = TRUE, bordered = TRUE)

  output$nmb_table <- renderTable({
    b <- base(); n <- input$n_cohort; wtp <- input$wtp; rupee <- intToUtf8(8377)
    nmb_s <- wtp * b$qaly_std / n - b$cost_std / n
    nmb_i <- wtp * b$qaly_int / n - b$cost_int / n
    data.frame(
      Strategy = c("Standard Care", "Intervention"),
      `NMB/Patient` = c(paste0(rupee, format(round(nmb_s), big.mark = ",")),
                        paste0(rupee, format(round(nmb_i), big.mark = ","))),
      Optimal = c(ifelse(nmb_s >= nmb_i, "\u2605", ""), ifelse(nmb_i > nmb_s, "\u2605", "")),
      check.names = FALSE, stringsAsFactors = FALSE)
  }, striped = TRUE, bordered = TRUE)

  # ---- Markov Trace ----
  output$trace_plot <- renderPlot({
    b <- base(); n <- input$n_cohort; h <- input$horizon
    states <- c("Early CKD", "Moderate CKD", "Advanced/Dialysis", "Death")
    df_s <- as.data.frame(b$trace_std / n); names(df_s) <- states
    df_s$Cycle <- 0:h; df_s$Strategy <- "Standard Care"
    df_i <- as.data.frame(b$trace_int / n); names(df_i) <- states
    df_i$Cycle <- 0:h; df_i$Strategy <- "Intervention"
    df <- rbind(df_s, df_i)
    df_long <- pivot_longer(df, cols = all_of(states), names_to = "State", values_to = "Proportion")
    df_long$State <- factor(df_long$State, levels = states)

    ggplot(df_long, aes(x = Cycle, y = Proportion, fill = State)) +
      geom_area(alpha = 0.8) + facet_wrap(~Strategy) +
      scale_fill_manual(values = c("#59a14f", "#f28e2b", "#e15759", "#76b7b2")) +
      labs(x = "Year", y = "Proportion", title = "Markov Trace: CKD Progression") +
      theme_minimal() + theme(legend.position = "bottom")
  })

  output$dialysis_plot <- renderPlot({
    b <- base(); n <- input$n_cohort; h <- input$horizon
    df <- data.frame(
      Year = rep(0:h, 2),
      Pct = c(b$trace_std[, 3] / n * 100, b$trace_int[, 3] / n * 100),
      Strategy = rep(c("Standard Care", "Intervention"), each = h + 1))
    ggplot(df, aes(x = Year, y = Pct, colour = Strategy)) +
      geom_line(linewidth = 1.2) +
      scale_colour_manual(values = c("#e15759", "#4e79a7")) +
      labs(x = "Year", y = "% on Dialysis", title = "Patients Reaching Advanced CKD / Dialysis") +
      theme_minimal() + theme(legend.position = "bottom")
  })

  # ---- Tornado (real DSA using NMB) ----
  output$tornado_plot <- renderPlot({
    b <- base(); n <- input$n_cohort; wtp <- input$wtp
    dc0 <- (b$cost_int - b$cost_std) / n
    dq0 <- (b$qaly_int - b$qaly_std) / n
    base_nmb <- wtp * dq0 - dc0

    params <- list(
      list(name = "p12 (Early-Mod)", arg = "p_12", lo = input$p_12 * 0.5, hi = min(input$p_12 * 1.5, 0.3)),
      list(name = "p23 (Mod-Adv)", arg = "p_23", lo = input$p_23 * 0.5, hi = min(input$p_23 * 1.5, 0.3)),
      list(name = "p3d (Adv-Death)", arg = "p_3d", lo = input$p_3d * 0.5, hi = min(input$p_3d * 2, 0.4)),
      list(name = "Hazard Ratio", arg = "hr", lo = max(input$hr - 0.15, 0.3), hi = min(input$hr + 0.15, 1.0)),
      list(name = "Cost: Advanced", arg = "cost_advanced", lo = input$cost_advanced * 0.5, hi = input$cost_advanced * 2),
      list(name = "Cost: ACE-inhibitor", arg = "cost_ace", lo = input$cost_ace * 0.5, hi = input$cost_ace * 2),
      list(name = "Utility: Early", arg = "u_early", lo = max(input$u_early - 0.1, 0.5), hi = min(input$u_early + 0.1, 1)),
      list(name = "Discount Rate", arg = "disc_rate", lo = 0, hi = 0.05)
    )

    base_args <- list(p_12 = input$p_12, p_1d = input$p_1d, p_23 = input$p_23,
                      p_2d = input$p_2d, p_3d = input$p_3d, hr = input$hr,
                      cost_early = input$cost_early, cost_moderate = input$cost_moderate,
                      cost_advanced = input$cost_advanced, cost_ace = input$cost_ace,
                      cost_screen = input$cost_screen,
                      u_early = input$u_early, u_moderate = input$u_moderate,
                      u_advanced = input$u_advanced,
                      horizon = input$horizon, disc_rate = input$discount, n_cohort = input$n_cohort)

    nmb_lo <- nmb_hi <- numeric(length(params))
    for (i in seq_along(params)) {
      p <- params[[i]]
      a_lo <- base_args; a_lo[[p$arg]] <- p$lo
      r_lo <- do.call(run_markov, a_lo)
      nmb_lo[i] <- wtp * (r_lo$qaly_int - r_lo$qaly_std) / n - (r_lo$cost_int - r_lo$cost_std) / n

      a_hi <- base_args; a_hi[[p$arg]] <- p$hi
      r_hi <- do.call(run_markov, a_hi)
      nmb_hi[i] <- wtp * (r_hi$qaly_int - r_hi$qaly_std) / n - (r_hi$cost_int - r_hi$cost_std) / n
    }

    tdf <- data.frame(
      Parameter = sapply(params, `[[`, "name"),
      Low = nmb_lo - base_nmb, High = nmb_hi - base_nmb,
      Swing = abs(nmb_hi - nmb_lo), stringsAsFactors = FALSE)
    tdf <- tdf[order(tdf$Swing), ]
    tdf$Parameter <- factor(tdf$Parameter, levels = tdf$Parameter)

    rupee <- intToUtf8(8377)
    ggplot(tdf) +
      geom_segment(aes(x = Low, xend = High, y = Parameter, yend = Parameter),
                   linewidth = 8, colour = "#4e79a7", alpha = 0.8) +
      geom_vline(xintercept = 0, linetype = "dashed", colour = "red") +
      scale_x_continuous(labels = function(x) paste0(rupee, format(round(x), big.mark = ","))) +
      labs(title = "Tornado: Change in Incremental NMB",
           subtitle = paste0("Base NMB = ", rupee, format(round(base_nmb), big.mark = ",")),
           x = paste0("Change in NMB (", rupee, ")"), y = "") +
      theme_minimal() + theme(axis.text.y = element_text(size = 11))
  })

  # ---- PSA (1,000 iterations) ----
  psa_data <- reactiveVal(NULL)
  output$psa_done <- reactive({ !is.null(psa_data()) })
  outputOptions(output, "psa_done", suspendWhenHidden = FALSE)

  observeEvent(input$run_psa, {
    n <- input$n_cohort; wtp <- input$wtp
    n_iter <- 1000
    results <- data.frame(inc_cost = numeric(n_iter), inc_qaly = numeric(n_iter))

    withProgress(message = "Running PSA...", value = 0, {
      for (i in 1:n_iter) {
        s_p12 <- rbeta(1, input$p_12 * 100, (1 - input$p_12) * 100)
        s_p23 <- rbeta(1, input$p_23 * 100, (1 - input$p_23) * 100)
        s_p3d <- rbeta(1, input$p_3d * 100, (1 - input$p_3d) * 100)
        s_p1d <- rbeta(1, input$p_1d * 200, (1 - input$p_1d) * 200)
        s_p2d <- rbeta(1, input$p_2d * 200, (1 - input$p_2d) * 200)
        s_hr <- exp(rnorm(1, log(input$hr), 0.15))
        s_ce <- rgamma(1, shape = (input$cost_early / 3000)^2,
                        rate = (input$cost_early / 3000)^2 / input$cost_early)
        s_cm <- rgamma(1, shape = (input$cost_moderate / 10000)^2,
                        rate = (input$cost_moderate / 10000)^2 / input$cost_moderate)
        s_ca <- rgamma(1, shape = (input$cost_advanced / 70000)^2,
                        rate = (input$cost_advanced / 70000)^2 / input$cost_advanced)
        s_ue <- rbeta(1, input$u_early * 100, (1 - input$u_early) * 100)
        s_um <- rbeta(1, input$u_moderate * 100, (1 - input$u_moderate) * 100)
        s_ua <- rbeta(1, input$u_advanced * 100, (1 - input$u_advanced) * 100)

        r <- run_markov(s_p12, s_p1d, s_p23, s_p2d, s_p3d, s_hr,
                        s_ce, s_cm, s_ca, input$cost_ace, input$cost_screen,
                        s_ue, s_um, s_ua,
                        input$horizon, input$discount, n)
        results$inc_cost[i] <- (r$cost_int - r$cost_std) / n
        results$inc_qaly[i] <- (r$qaly_int - r$qaly_std) / n

        if (i %% 100 == 0) setProgress(i / n_iter)
      }
    })

    results$inc_nmb <- wtp * results$inc_qaly - results$inc_cost
    psa_data(results)
  })

  output$psa_summary <- renderTable({
    r <- psa_data(); if (is.null(r)) return()
    rupee <- intToUtf8(8377)
    data.frame(
      Metric = c("Mean Incr Cost/patient", "Mean Incr QALYs/patient",
                  "Mean Incr NMB", "P(cost-effective)", "P(dominant)"),
      Value = c(
        paste0(rupee, format(round(mean(r$inc_cost)), big.mark = ",")),
        round(mean(r$inc_qaly), 4),
        paste0(rupee, format(round(mean(r$inc_nmb)), big.mark = ",")),
        paste0(round(mean(r$inc_nmb > 0) * 100, 1), "%"),
        paste0(round(mean(r$inc_cost < 0 & r$inc_qaly > 0) * 100, 1), "%")),
      check.names = FALSE, stringsAsFactors = FALSE)
  }, striped = TRUE, bordered = TRUE)

  output$ce_plane <- renderPlot({
    r <- psa_data(); if (is.null(r)) return()
    wtp <- input$wtp; rupee <- intToUtf8(8377)
    r$ce <- r$inc_nmb > 0

    # Axis ranges symmetric around 0 for quadrant visibility
    x_lim <- max(abs(r$inc_qaly), na.rm = TRUE) * 1.1
    y_lim <- max(abs(r$inc_cost), na.rm = TRUE) * 1.1

    ggplot(r, aes(x = inc_qaly, y = inc_cost, colour = ce)) +
      geom_hline(yintercept = 0, colour = "grey40", linewidth = 0.5) +
      geom_vline(xintercept = 0, colour = "grey40", linewidth = 0.5) +
      geom_point(alpha = 0.3, size = 1.5) +
      geom_abline(intercept = 0, slope = wtp, linetype = "dashed", colour = "red", linewidth = 0.8) +
      # Quadrant labels
      annotate("text", x = x_lim * 0.7, y = -y_lim * 0.85, label = "DOMINANT\n(cheaper + better)",
               colour = "#2ecc71", size = 3.5, fontface = "bold") +
      annotate("text", x = -x_lim * 0.7, y = y_lim * 0.85, label = "DOMINATED\n(costlier + worse)",
               colour = "#e74c3c", size = 3.5, fontface = "bold") +
      annotate("text", x = x_lim * 0.7, y = y_lim * 0.85, label = "NE quadrant\n(costlier + better)",
               colour = "grey50", size = 3, fontface = "italic") +
      annotate("text", x = -x_lim * 0.7, y = -y_lim * 0.85, label = "SW quadrant\n(cheaper + worse)",
               colour = "grey50", size = 3, fontface = "italic") +
      scale_colour_manual(values = c("TRUE" = "#2ecc71", "FALSE" = "#e74c3c"),
                          labels = c("TRUE" = "Cost-effective", "FALSE" = "Not CE"), name = "") +
      coord_cartesian(xlim = c(-x_lim, x_lim), ylim = c(-y_lim, y_lim)) +
      labs(x = "Incremental QALYs", y = paste0("Incremental Cost (", rupee, ")"),
           title = "Cost-Effectiveness Plane (1,000 PSA iterations)") +
      theme_minimal() + theme(legend.position = "bottom")
  })

  output$ceac_plot <- renderPlot({
    r <- psa_data(); if (is.null(r)) return()
    wtp_range <- seq(0, 500000, by = 10000)
    pce <- sapply(wtp_range, function(w) mean(w * r$inc_qaly - r$inc_cost > 0))
    df <- data.frame(WTP = wtp_range, PCE = pce)
    rupee <- intToUtf8(8377)
    ggplot(df, aes(x = WTP, y = PCE)) +
      geom_line(linewidth = 1.2, colour = "#3498db") +
      geom_ribbon(aes(ymin = 0, ymax = PCE), alpha = 0.15, fill = "#3498db") +
      geom_vline(xintercept = input$wtp, linetype = "dashed", colour = "red") +
      scale_x_continuous(labels = function(x) paste0(rupee, format(x / 1000, big.mark = ","), "K")) +
      scale_y_continuous(limits = c(0, 1), labels = scales::percent) +
      labs(x = paste0("WTP (", rupee, "/QALY)"), y = "P(Cost-Effective)",
           title = "Cost-Effectiveness Acceptability Curve") +
      theme_minimal()
  })
}

shinyApp(ui = ui, server = server)
