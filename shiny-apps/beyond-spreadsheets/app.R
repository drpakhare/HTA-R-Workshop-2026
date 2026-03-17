# ============================================================================
# Beyond Spreadsheets — Why R for HTA
# A live showcase app for Session 1 of the R for HTA (Basics) Workshop
# RRC-HTA, AIIMS Bhopal | 2026
# ============================================================================

library(shiny)
library(bslib)
library(ggplot2)
library(DT)
library(DiagrammeR)

# ── Colour palette (high-contrast, projector-safe) ────────────────────────
pal <- list(
  teal      = "#0D7377",
  coral     = "#C81D4E",
  amber     = "#D97706",
  green     = "#15803D",
  sky       = "#0369A1",
  slate     = "#1E293B",
  muted     = "#64748B",
  bg_light  = "#F8FAFC",
  card_bg   = "#FFFFFF"
)

# ── Projector-optimised theme for ggplot2 ─────────────────────────────────
theme_projector <- function(base_size = 20) {
  theme_minimal(base_size = base_size) %+replace%
    theme(
      plot.title       = element_text(face = "bold", size = base_size * 1.3,
                                      color = "#0f172a", margin = margin(b = 12)),
      plot.subtitle    = element_text(size = base_size * 0.85, color = "#475569",
                                      margin = margin(b = 16)),
      axis.title       = element_text(face = "bold", size = base_size * 0.9,
                                      color = "#334155"),
      axis.text        = element_text(size = base_size * 0.8, color = "#334155"),
      legend.text      = element_text(size = base_size * 0.8),
      legend.title     = element_text(size = base_size * 0.85, face = "bold"),
      legend.position  = "top",
      legend.key.size  = unit(1.2, "lines"),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(color = "#E2E8F0", linewidth = 0.4),
      plot.margin      = margin(20, 20, 20, 20)
    )
}

# ── Custom CSS — projector & accessibility optimised ──────────────────────
app_css <- "
  /* ── Global typography ── */
  body {
    font-size: 18px !important;
    line-height: 1.7 !important;
    color: #1e293b !important;
  }
  h1 { font-size: 2.4rem !important; font-weight: 800 !important; }
  h2 { font-size: 2.0rem !important; font-weight: 700 !important; }
  h3 { font-size: 1.7rem !important; font-weight: 700 !important; }
  h4 { font-size: 1.5rem !important; font-weight: 700 !important; }
  h5 { font-size: 1.25rem !important; font-weight: 600 !important; }
  p, li, label, .form-label {
    font-size: 18px !important;
    line-height: 1.7 !important;
  }

  /* ── Navbar ── */
  .navbar {
    padding: 14px 24px !important;
    border-bottom: 3px solid #0D7377 !important;
    box-shadow: 0 2px 8px rgba(0,0,0,0.06) !important;
  }
  .navbar .nav-link {
    font-size: 17px !important;
    font-weight: 600 !important;
    padding: 10px 18px !important;
    color: #334155 !important;
  }
  .navbar .nav-link.active {
    color: #0D7377 !important;
    border-bottom: 3px solid #0D7377 !important;
  }

  /* ── Code blocks: LIGHT theme for projector ── */
  .code-display {
    background: #f1f5f9;
    color: #1e293b;
    padding: 24px 28px;
    border-radius: 10px;
    font-family: 'Fira Code', 'Consolas', 'Courier New', monospace;
    font-size: 20px;
    line-height: 1.8;
    white-space: pre-wrap;
    border-left: 6px solid #0D7377;
    margin: 20px 0;
  }
  .code-display .kw  { color: #7C3AED; font-weight: 600; }
  .code-display .fn  { color: #0369A1; font-weight: 600; }
  .code-display .num { color: #B45309; font-weight: 600; }
  .code-display .str { color: #15803D; }
  .code-display .cmt { color: #64748b; font-style: italic; }

  /* ── Metric cards ── */
  .metric-card {
    background: linear-gradient(135deg, #f0fdfa 0%, #f8fafc 100%);
    border: 2px solid #CBD5E1;
    border-radius: 14px;
    padding: 28px 20px;
    text-align: center;
    margin-bottom: 20px;
  }
  .metric-value {
    font-size: 36px;
    font-weight: 800;
    color: #0D7377;
    line-height: 1.2;
  }
  .metric-label {
    font-size: 15px;
    font-weight: 600;
    color: #475569;
    margin-top: 8px;
    text-transform: uppercase;
    letter-spacing: 0.5px;
  }

  /* ── Callout boxes ── */
  .insight-box {
    background: #FFFBEB;
    border-left: 6px solid #D97706;
    padding: 20px 24px;
    border-radius: 0 10px 10px 0;
    margin: 24px 0;
    font-size: 18px;
  }
  .danger-box {
    background: #FEF2F2;
    border-left: 6px solid #C81D4E;
    padding: 20px 24px;
    border-radius: 0 10px 10px 0;
    margin: 24px 0;
    font-size: 18px;
  }
  .success-box {
    background: #F0FDF4;
    border-left: 6px solid #15803D;
    padding: 20px 24px;
    border-radius: 0 10px 10px 0;
    margin: 24px 0;
    font-size: 18px;
  }

  /* ── Story timeline ── */
  .story-step {
    padding: 24px 28px;
    margin-bottom: 20px;
    border-radius: 10px;
    border: 2px solid #E2E8F0;
  }
  .story-step.active-step {
    border-color: #0D7377;
    background: #f0fdfa;
    box-shadow: 0 4px 12px rgba(13,115,119,0.12);
  }
  .timeline-dot {
    width: 18px; height: 18px;
    border-radius: 50%;
    display: inline-block;
    margin-right: 10px;
    vertical-align: middle;
  }

  /* ── App header ── */
  .app-title {
    font-size: 28px;
    font-weight: 800;
    color: #0f172a;
    letter-spacing: -0.5px;
  }
  .app-subtitle {
    font-size: 15px;
    color: #64748b;
    margin-top: 2px;
    font-weight: 500;
  }

  /* ── Section spacer ── */
  .section-spacer { height: 36px; }

  /* ── Cards ── */
  .card {
    border: 1px solid #E2E8F0 !important;
    box-shadow: 0 2px 8px rgba(0,0,0,0.04) !important;
    margin-bottom: 24px !important;
    border-radius: 12px !important;
  }
  .card-header {
    padding: 16px 24px !important;
    font-size: 20px !important;
  }
  .card-body {
    padding: 20px 24px !important;
  }

  /* ── Tables (DT) — force visibility ── */
  .dataTables_wrapper {
    width: 100% !important;
    overflow-x: auto !important;
  }
  table.dataTable {
    font-size: 17px !important;
    width: 100% !important;
  }
  table.dataTable thead th {
    font-size: 17px !important;
    font-weight: 700 !important;
    padding: 14px 16px !important;
    background: #F1F5F9 !important;
    color: #1E293B !important;
    border-bottom: 3px solid #0D7377 !important;
  }
  table.dataTable tbody td {
    padding: 12px 16px !important;
    font-size: 17px !important;
  }
  table.dataTable tbody tr:nth-child(odd) {
    background: #F8FAFC !important;
  }
  table.dataTable tbody tr:hover {
    background: #E0F2FE !important;
  }

  /* ── Comparison table ── */
  .comparison-table td, .comparison-table th {
    padding: 16px 20px !important;
    font-size: 18px !important;
    vertical-align: top;
  }
  .comparison-table thead th {
    font-size: 20px !important;
    border-bottom: 3px solid #0D7377 !important;
    background: #F8FAFC;
  }
  .comparison-table tbody tr:nth-child(odd) {
    background: #F8FAFC;
  }

  /* ── Sidebar — ultra-compact for maximum visibility ── */
  .bslib-sidebar-layout > .sidebar {
    overflow-y: auto !important;
    padding: 8px 10px !important;
  }
  .sidebar .form-group {
    margin-bottom: 3px !important;
  }
  .sidebar .form-group label,
  .sidebar .control-label {
    font-size: 11px !important;
    font-weight: 600 !important;
    color: #334155 !important;
    margin-bottom: 1px !important;
    line-height: 1.2 !important;
  }
  .sidebar p {
    font-size: 11px !important;
    line-height: 1.3 !important;
    margin-bottom: 4px !important;
  }
  .sidebar .btn {
    font-size: 13px !important;
    padding: 7px 12px !important;
    font-weight: 700 !important;
  }
  .sidebar h6 {
    font-size: 10px !important;
    text-transform: uppercase;
    letter-spacing: 0.8px;
    color: #64748b !important;
    margin-top: 6px !important;
    margin-bottom: 2px !important;
  }
  .sidebar .btn-download {
    margin-top: 4px;
    font-size: 11px !important;
    padding: 4px 10px !important;
  }
  .sidebar hr {
    margin: 4px 0 !important;
  }
  .sidebar .irs { margin-bottom: -2px !important; }
  .sidebar .irs--shiny .irs-min,
  .sidebar .irs--shiny .irs-max {
    font-size: 9px !important;
  }
  .sidebar .irs--shiny .irs-single,
  .sidebar .irs--shiny .irs-from,
  .sidebar .irs--shiny .irs-to {
    font-size: 10px !important;
  }
  .sidebar .sidebar-title {
    font-size: 12px !important;
  }
  .sidebar .insight-box,
  .sidebar .success-box {
    font-size: 11px !important;
    padding: 6px 8px !important;
    margin: 4px 0 !important;
  }
  .sidebar .shiny-input-container {
    margin-bottom: 2px !important;
  }

  /* ── Slider inputs ── */
  .irs--shiny .irs-bar { background: #0D7377; }
  .irs--shiny .irs-handle { border-color: #0D7377; }
  .irs--shiny .irs-single {
    font-size: 13px;
    font-weight: 600;
    background: #0D7377;
  }

  /* ── TP Matrix display ── */
  .tp-matrix {
    font-family: 'Fira Code', monospace;
    font-size: 16px;
    width: 100%;
    border-collapse: collapse;
    margin: 12px 0;
  }
  .tp-matrix th, .tp-matrix td {
    padding: 10px 14px;
    text-align: center;
    border: 1px solid #CBD5E1;
  }
  .tp-matrix th {
    background: #F1F5F9;
    font-weight: 700;
    color: #334155;
    font-size: 14px;
  }
  .tp-matrix td {
    color: #1E293B;
    font-weight: 600;
  }
  .tp-matrix .diag { background: #F0FDFA; }
  .tp-matrix .tp-val { color: #0D7377; font-weight: 800; }

  /* ── Radio buttons bigger targets ── */
  .radio label, .checkbox label {
    font-size: 17px !important;
    padding: 6px 0 !important;
    line-height: 1.6 !important;
  }
  .radio input[type='radio'],
  .checkbox input[type='checkbox'] {
    transform: scale(1.3);
    margin-right: 8px;
  }

  /* ── Tab content scroll ── */
  .tab-pane { padding-bottom: 60px; }
"

# ============================================================================
# UI
# ============================================================================

ui <- page_navbar(
  title = div(
    span("Beyond Spreadsheets", class = "app-title"),
    div("Why R for Health Technology Assessment", class = "app-subtitle")
  ),
  theme = bs_theme(
    version = 5,
    bootswatch = "flatly",
    primary = pal$teal,
    "navbar-bg" = "#FFFFFF",
    "navbar-light-color" = pal$slate,
    base_font = font_google("Inter"),
    code_font = font_google("Fira Code"),
    "border-radius" = "10px"
  ),
  bg = "white",
  header = tags$head(
    tags$style(HTML(app_css)),
    tags$link(
      rel = "stylesheet",
      href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/6.4.0/css/all.min.css"
    )
  ),

  # ══════════════════════════════════════════════════════════════════════════
  # TAB 1: The Reinhart-Rogoff Story  (full-width — few controls)
  # ══════════════════════════════════════════════════════════════════════════
  nav_panel(
    title = "The Spreadsheet That Shook Nations",
    icon = icon("triangle-exclamation"),

    div(style = "max-width: 1200px; margin: 0 auto; padding: 32px 24px;",

      # ── Ch 1: Context ──
      h2(icon("book-open", style = "color: #0D7377;"),
         " A Cautionary Tale from Austerity Economics",
         style = "margin-bottom: 28px;"),

      fluidRow(
        column(4, div(class = "story-step active-step",
          h4(span(class = "timeline-dot", style = "background: #C81D4E;"), "2008"),
          h5("Financial Crisis", style = "color: #D97706;"),
          p("The worst global recession since the 1930s. Banks failed,
             unemployment soared, governments borrowed massively to
             stimulate their economies.")
        )),
        column(4, div(class = "story-step",
          h4(span(class = "timeline-dot", style = "background: #D97706;"), "The Debate"),
          h5("Stimulus vs. Austerity", style = "color: #D97706;"),
          p("Should governments keep spending to boost growth — or slash
             spending to reduce dangerous debt levels? The debate was fierce.")
        )),
        column(4, div(class = "story-step",
          h4(span(class = "timeline-dot", style = "background: #0D7377;"), "The Question"),
          h5("Does debt kill growth?", style = "color: #D97706;"),
          p("Policymakers desperately wanted data. At what point does
             government debt become so large that it chokes off growth?")
        ))
      ),

      # ── Ch 2: The Paper ──
      div(class = "section-spacer"),
      div(class = "story-step active-step",
        h3(span(class = "timeline-dot", style = "background: #0D7377;"),
           "2010 — \"Growth in a Time of Debt\""),
        fluidRow(
          column(5,
            h5("Carmen Reinhart & Kenneth Rogoff",
               style = "color: #D97706; margin-bottom: 4px;"),
            p("Harvard University, American Economic Review",
              style = "color: #64748b; font-size: 16px;"),
            p("Countries whose government debt exceeds 90% of GDP experience
               dramatically lower economic growth — averaging just",
              tags$strong("-0.1% per year.")),
            p(tags$strong("Cited 3,000+ times."),
              style = "color: #C81D4E; font-size: 20px;")
          ),
          column(7,
            # Claimed GDP growth chart
            plotOutput("rr_claimed_chart", height = "260px")
          )
        )
      ),

      # ── Ch 3: Policy impact + quote ──
      div(class = "section-spacer"),
      div(style = "background: #F8FAFC; border-left: 6px solid #D97706;
                    border-radius: 0 12px 12px 0; padding: 24px 28px; margin: 20px 0;",
        p(tags$em(style = "font-size: 20px;",
          "\"The recent research of economists Carmen Reinhart and Kenneth Rogoff
           confirms that high levels of government debt consistently lead to
           lower economic growth.\""),
          style = "margin-bottom: 8px;"),
        p("— Paul Ryan, U.S. House Budget Committee, 2013 Budget Proposal",
          style = "color: #64748b; font-size: 16px; margin: 0;")
      ),

      fluidRow(
        column(3, div(class = "story-step",
          h5("European Commission", style = "color: #D97706;"),
          p("Imposed austerity on Greece, Spain, Portugal, Ireland as
             condition of bailouts.")
        )),
        column(3, div(class = "story-step",
          h5("UK Treasury", style = "color: #D97706;"),
          p("Chancellor Osborne used it to defend sweeping welfare and
             public service cuts.")
        )),
        column(3, div(class = "story-step",
          h5("IMF", style = "color: #D97706;"),
          p("Cited in country reports recommending fiscal consolidation
             across debt-heavy nations.")
        )),
        column(3, div(class = "story-step",
          h5("U.S. Congress", style = "color: #D97706;"),
          p("Informed Republican budget plans targeting trillions in
             spending reductions.")
        ))
      ),

      # ── Ch 4: Thomas Herndon ──
      div(class = "section-spacer"),
      div(class = "story-step",
        h3(span(class = "timeline-dot", style = "background: #C81D4E;"),
           "2013 — A Grad Student Asks for the Spreadsheet"),
        fluidRow(
          column(3,
            div(class = "metric-card",
              div(class = "metric-value", style = "font-size: 56px;", "28"),
              div(class = "metric-label", "years old"),
              div(style = "margin-top: 8px; font-size: 16px; color: #475569;",
                  "Thomas Herndon"),
              div(style = "font-size: 14px; color: #64748b;",
                  "PhD Student, UMass Amherst")
            )
          ),
          column(9,
            tags$table(style = "width: 100%; font-size: 18px; border-collapse: separate;
                                border-spacing: 0 12px;",
              tags$tr(
                tags$td(style = "width: 50px; color: #D97706; font-weight: 800;
                                 font-size: 24px; vertical-align: top;", "01"),
                tags$td(tags$strong("The assignment — ", style = "color: #D97706;"),
                        "Professor asked students to replicate a famous economics paper.
                         He chose Reinhart-Rogoff.")
              ),
              tags$tr(
                tags$td(style = "width: 50px; color: #D97706; font-weight: 800;
                                 font-size: 24px; vertical-align: top;", "02"),
                tags$td(tags$strong("The mismatch — ", style = "color: #D97706;"),
                        "His results didn't match. He ran the numbers again. And again.
                         Still wrong.")
              ),
              tags$tr(
                tags$td(style = "width: 50px; color: #D97706; font-weight: 800;
                                 font-size: 24px; vertical-align: top;", "03"),
                tags$td(tags$strong("The email — ", style = "color: #D97706;"),
                        "He emailed Reinhart and Rogoff directly and politely asked for
                         their original Excel file.")
              ),
              tags$tr(
                tags$td(style = "width: 50px; color: #D97706; font-weight: 800;
                                 font-size: 24px; vertical-align: top;", "04"),
                tags$td(tags$strong("The moment — ", style = "color: #D97706;"),
                        "They sent it. What he found inside would become one of the
                         most famous discoveries in economics.")
              )
            )
          )
        )
      ),

      # ── Ch 5: Three errors ──
      div(class = "section-spacer"),
      h3(icon("magnifying-glass", style = "color: #0D7377;"),
         " Three Errors. One Catastrophic Result."),

      radioButtons("rr_error", NULL, inline = TRUE,
        choices = c(
          "The Coding Mistake" = "exclude",
          "Unconventional Weighting" = "omit",
          "Selective Exclusion" = "weight"
        ),
        selected = "exclude"
      ),
      uiOutput("rr_error_detail"),

      # ── Ch 6: The Number That Changed Everything ──
      div(class = "section-spacer"),
      h3(icon("arrows-left-right", style = "color: #0D7377;"),
         " The Number That Changed Everything"),
      fluidRow(
        column(5,
          div(style = "background: #FEF2F2; border: 3px solid #C81D4E;
                        border-radius: 12px; padding: 28px; text-align: center;",
            p("Original Claim", style = "color: #C81D4E; font-weight: 700;
                                         font-size: 16px; text-transform: uppercase;
                                         letter-spacing: 1px; margin-bottom: 8px;"),
            p("Growth above 90% debt:", style = "color: #475569; margin-bottom: 4px;"),
            div(style = "font-size: 64px; font-weight: 800; color: #C81D4E;
                         line-height: 1;", "-0.1%"),
            p("per year average", style = "color: #64748b; margin-top: 8px;"),
            p(tags$em("Debt above 90% causes contraction"),
              style = "color: #C81D4E; margin: 0; font-size: 16px;")
          )
        ),
        column(2,
          div(style = "display: flex; align-items: center; justify-content: center;
                        height: 100%; min-height: 200px;",
            span(style = "font-size: 36px; font-weight: 800; color: #64748b;", "vs."))
        ),
        column(5,
          div(style = "background: #F0FDF4; border: 3px solid #15803D;
                        border-radius: 12px; padding: 28px; text-align: center;",
            p("Corrected Result", style = "color: #15803D; font-weight: 700;
                                           font-size: 16px; text-transform: uppercase;
                                           letter-spacing: 1px; margin-bottom: 8px;"),
            p("Growth above 90% debt:", style = "color: #475569; margin-bottom: 4px;"),
            div(style = "font-size: 64px; font-weight: 800; color: #15803D;
                         line-height: 1;", "+2.2%"),
            p("per year average", style = "color: #64748b; margin-top: 8px;"),
            p(tags$em("Slower growth — but not economic collapse"),
              style = "color: #15803D; margin: 0; font-size: 16px;")
          )
        )
      ),

      # ── Ch 7: Interactive demo ──
      div(class = "section-spacer"),
      h3(icon("flask", style = "color: #0D7377;"),
         " Try It Yourself"),
      p("Toggle countries in and out to see how the conclusion changes.",
        style = "color: #475569; font-size: 19px; margin-bottom: 24px;"),

      fluidRow(
        column(3,
          card(
            card_header(h5("Select Countries"), class = "bg-light"),
            card_body(
              checkboxGroupInput("rr_countries", NULL,
                choices = c(
                  "Australia" = "AUS", "Austria" = "AUT",
                  "Belgium" = "BEL", "Canada" = "CAN",
                  "Denmark" = "DNK", "Greece" = "GRC",
                  "Ireland" = "IRL", "Italy" = "ITA",
                  "Japan" = "JPN", "New Zealand" = "NZL",
                  "Norway" = "NOR", "Sweden" = "SWE",
                  "UK" = "GBR", "USA" = "USA"
                ),
                selected = c("AUS","AUT","BEL","CAN","DNK","GRC",
                             "IRL","ITA","JPN","NZL","NOR","SWE","GBR","USA")
              ),
              div(class = "insight-box",
                icon("lightbulb", style = "color: #D97706;"),
                tags$strong(" Try deselecting New Zealand"),
                " — that's what the Excel error did."
              )
            )
          )
        ),
        column(9,
          plotOutput("rr_plot", height = "550px"),
          uiOutput("rr_verdict")
        )
      ),

      # ── Five Lessons ──
      div(class = "section-spacer"),
      h3(icon("graduation-cap", style = "color: #0D7377;"),
         " Five Lessons from Spreadsheet-Gate"),
      fluidRow(
        column(4, div(class = "danger-box",
          h5("01 — Share your data", style = "color: #C81D4E;"),
          p("Research that shapes policy must be verifiable.
             Journals now require raw data and code.")
        )),
        column(4, div(class = "danger-box",
          h5("02 — Excel is fragile", style = "color: #C81D4E;"),
          p("An off-by-one range error became the most expensive
             typo in economic history.")
        )),
        column(4, div(class = "danger-box",
          h5("03 — Replicate before acting", style = "color: #C81D4E;"),
          p("No policy affecting millions should rest on a single
             unreviewed spreadsheet.")
        ))
      ),
      fluidRow(
        column(6, div(class = "insight-box",
          h5("04 — Power amplifies errors", style = "color: #D97706;"),
          p("Institutions adopt what confirms their priors —
             and resist correction fiercely.")
        )),
        column(6, div(class = "success-box",
          h5("05 — The grad student matters", style = "color: #15803D;"),
          p("Thomas Herndon did what no senior economist had
             bothered to do. It took 3 years.")
        ))
      ),

      div(class = "section-spacer"),
      div(class = "success-box", style = "padding: 24px 28px;",
        h4(icon("shield-halved", style = "color: #15803D;"),
           " What R Would Have Prevented", style = "margin: 0 0 12px 0;"),
        tags$ul(style = "font-size: 18px; margin: 0;",
          tags$li(tags$strong("Transparent code:"), " Every step visible — ",
                  tags$code("mean(df$growth)"), " always uses ALL rows"),
          tags$li(tags$strong("Reproducible:"), " set.seed() + script = same result every time"),
          tags$li(tags$strong("Version-controlled:"), " Git tracks every change to every line"),
          tags$li(tags$strong("Peer-reviewable:"), " Anyone can run the same script and verify")
        )
      )
    )
  ),

  # ══════════════════════════════════════════════════════════════════════════
  # TAB 2: PSA Demo  (sidebar: sliders left, output right)
  # ══════════════════════════════════════════════════════════════════════════
  nav_panel(
    title = "PSA Demo",
    icon = icon("dice"),
    layout_sidebar(
      sidebar = sidebar(
        title = "Simulation Parameters",
        width = 340, open = TRUE,
        sliderInput("psa_n", "Number of simulations",
                    min = 100, max = 50000, value = 10000, step = 100),
        numericInput("psa_seed", "Random seed", value = 42, min = 1, max = 99999),
        hr(),
        h6("Treatment A (Standard)"),
        sliderInput("psa_cost_a", "Mean cost (Rs.)", 50000, 200000, 100000, 5000),
        sliderInput("psa_qaly_a", "Mean QALYs", 1.0, 10.0, 5.0, 0.1),
        hr(),
        h6("Treatment B (New)"),
        sliderInput("psa_cost_b", "Mean cost (Rs.)", 50000, 300000, 120000, 5000),
        sliderInput("psa_qaly_b", "Mean QALYs", 1.0, 12.0, 6.5, 0.1),
        hr(),
        sliderInput("psa_cv", "Coefficient of variation", 0.05, 0.50, 0.20, 0.01),
        sliderInput("psa_wtp", "WTP threshold (Rs./QALY)",
                    min = 50000, max = 500000, value = 250000, step = 10000),
        actionButton("psa_run", "Run PSA",
                     icon = icon("play"),
                     class = "btn-primary w-100"),
        hr(),
        downloadButton("psa_download", "Download All Simulations (CSV)",
                       class = "btn-outline-primary w-100 btn-download")
      ),

      # ── Main panel: scrollable outputs ──
      div(
        # Code showcase
        h3(icon("terminal", style = "color: #0D7377;"),
           " The R Challenge"),
        p("In Excel, a probabilistic sensitivity analysis needs hundreds of rows of
           formulas. In R:", style = "color: #475569;"),
        div(class = "code-display",
          HTML(paste0(
            '<span class="cmt"># The entire PSA</span>\n',
            '<span class="fn">set.seed</span>(<span class="num">42</span>)\n',
            'delta_cost <span class="kw">&lt;-</span> <span class="fn">rnorm</span>(',
            '<span class="num">10000</span>, <span class="num">20000</span>, ',
            '<span class="num">5000</span>)\n',
            'delta_qaly <span class="kw">&lt;-</span> <span class="fn">rnorm</span>(',
            '<span class="num">10000</span>, <span class="num">1.5</span>, ',
            '<span class="num">0.4</span>)\n',
            '<span class="cmt"># 10,000 simulations. Done.</span>'
          ))
        ),
        div(class = "insight-box",
          icon("lightbulb", style = "color: #D97706;"),
          tags$strong(" Change the seed, rerun"),
          " — every result is traceable. Try that in a 10,000-row spreadsheet."
        ),

        # Summary metrics
        div(class = "section-spacer"),
        uiOutput("psa_metrics"),

        # CE Plane
        div(class = "section-spacer"),
        h3(icon("braille", style = "color: #0D7377;"),
           " Cost-Effectiveness Plane"),
        plotOutput("psa_ceplane", height = "600px"),

        # Simulation table
        div(class = "section-spacer"),
        h3(icon("table", style = "color: #0D7377;"),
           " Simulation Table"),
        div(style = "width: 100%; overflow-x: auto;",
          DTOutput("psa_table", width = "100%")
        ),

        # CEAC preview
        div(class = "section-spacer"),
        h3(icon("chart-line", style = "color: #0D7377;"),
           " CEAC Preview"),
        plotOutput("psa_ceac_mini", height = "500px")
      )
    )
  ),

  # ══════════════════════════════════════════════════════════════════════════
  # TAB 3: Markov Model  (sidebar: sliders left, output right)
  # ══════════════════════════════════════════════════════════════════════════
  nav_panel(
    title = "Markov Model",
    icon = icon("diagram-project"),
    layout_sidebar(
      sidebar = sidebar(
        title = "Model Parameters",
        width = 340, open = TRUE,
        h6("Transition Probabilities"),
        sliderInput("mk_p_prog", "Healthy -> Sick", 0.01, 0.40, 0.10, 0.01),
        sliderInput("mk_p_death_h", "Healthy -> Dead", 0.00, 0.10, 0.01, 0.005),
        uiOutput("mk_p_stay_h_display"),
        hr(),
        sliderInput("mk_p_death_s", "Sick -> Dead", 0.01, 0.40, 0.05, 0.01),
        uiOutput("mk_p_stay_s_display"),
        hr(),
        h6("Treatment Effect"),
        sliderInput("mk_hr", "Hazard ratio on progression",
                    0.30, 1.00, 0.70, 0.05),
        hr(),
        h6("Costs (Annual, Rs.)"),
        sliderInput("mk_cost_h", "Healthy", 0, 50000, 5000, 1000),
        sliderInput("mk_cost_s", "Sick", 10000, 200000, 50000, 5000),
        sliderInput("mk_cost_trt", "Treatment", 0, 100000, 20000, 5000),
        hr(),
        h6("Utilities"),
        sliderInput("mk_util_h", "Healthy", 0.70, 1.00, 0.95, 0.01),
        sliderInput("mk_util_s", "Sick", 0.30, 0.90, 0.60, 0.01),
        hr(),
        sliderInput("mk_horizon", "Time horizon (years)", 5, 40, 20, 1),
        sliderInput("mk_discount", "Discount rate", 0.00, 0.08, 0.03, 0.01),
        actionButton("mk_run", "Run Model",
                     icon = icon("play"),
                     class = "btn-primary w-100"),
        hr(),
        downloadButton("mk_download", "Download Trace Table (CSV)",
                       class = "btn-outline-primary w-100 btn-download")
      ),

      # ── Main panel ──
      div(
        h3(icon("terminal", style = "color: #0D7377;"),
           " The R Code"),
        p("A full cohort Markov model — readable, auditable, one screen:",
          style = "color: #475569;"),
        div(class = "code-display",
          HTML(paste0(
            '<span class="cmt"># States: Healthy, Sick, Dead</span>\n',
            'P <span class="kw">&lt;-</span> <span class="fn">matrix</span>(<span class="fn">c</span>(\n',
            '  <span class="num">0.89</span>, <span class="num">0.10</span>, <span class="num">0.01</span>,\n',
            '  <span class="num">0.00</span>, <span class="num">0.95</span>, <span class="num">0.05</span>,\n',
            '  <span class="num">0.00</span>, <span class="num">0.00</span>, <span class="num">1.00</span>\n',
            '), <span class="kw">nrow</span> = <span class="num">3</span>, <span class="kw">byrow</span> = <span class="num">TRUE</span>)\n\n',
            'trace <span class="kw">&lt;-</span> <span class="fn">matrix</span>(<span class="num">0</span>, <span class="kw">nrow</span> = <span class="num">21</span>, <span class="kw">ncol</span> = <span class="num">3</span>)\n',
            'trace[<span class="num">1</span>, ] <span class="kw">&lt;-</span> <span class="fn">c</span>(<span class="num">1000</span>, <span class="num">0</span>, <span class="num">0</span>)\n\n',
            '<span class="kw">for</span> (i <span class="kw">in</span> <span class="num">2</span>:<span class="num">21</span>) {\n',
            '  trace[i, ] <span class="kw">&lt;-</span> trace[i<span class="num">-1</span>, ] <span class="fn">%*%</span> P\n',
            '}\n',
            '<span class="cmt"># Full cohort simulation in one line of logic.</span>'
          ))
        ),
        div(class = "insight-box",
          icon("lightbulb", style = "color: #D97706;"),
          " The entire model logic: ",
          tags$code("trace[i,] <- trace[i-1,] %*% P",
                    style = "font-size: 18px; font-weight: 700;"),
          ". Try dragging formulas across 20 years x 3 states in Excel."
        ),

        # Model diagram
        div(class = "section-spacer"),
        h3(icon("circle-nodes", style = "color: #0D7377;"),
           " Model Structure"),
        p("Unidirectional progressive disease model — patients move from",
          tags$strong("Healthy \u2192 Sick \u2192 Dead."),
          "No recovery from Sick to Healthy (e.g. CKD, progressive cancer).",
          style = "color: #475569; font-size: 16px;"),
        grVizOutput("mk_diagram", height = "580px"),

        # Transition probability matrix
        div(class = "section-spacer"),
        h3(icon("table-cells", style = "color: #0D7377;"),
           " Transition Probability Matrix"),
        uiOutput("mk_tp_matrix"),

        # Markov trace (draggable slider)
        div(class = "section-spacer"),
        h3(icon("chart-area", style = "color: #0D7377;"),
           " Markov Trace"),
        p("Drag the slider to watch the cohort flow through health states.",
          style = "color: #475569;"),
        sliderInput("mk_cycle_view", "Show up to cycle:",
                    min = 1, max = 40, value = 20, step = 1, width = "100%"),
        plotOutput("mk_trace_plot", height = "550px"),

        # Trace table
        div(class = "section-spacer"),
        h3(icon("table", style = "color: #0D7377;"),
           " Trace Table — Key Cycles"),
        div(style = "width: 100%; overflow-x: auto;",
          DTOutput("mk_trace_table", width = "100%")
        ),

        # Results
        div(class = "section-spacer"),
        h3(icon("calculator", style = "color: #0D7377;"),
           " Cost-Effectiveness Results"),
        uiOutput("mk_results")
      )
    )
  ),

  # ══════════════════════════════════════════════════════════════════════════
  # TAB 4: CEAC Explorer  (sidebar)
  # ══════════════════════════════════════════════════════════════════════════
  nav_panel(
    title = "CEAC Explorer",
    icon = icon("chart-line"),
    layout_sidebar(
      sidebar = sidebar(
        title = "CEAC Parameters",
        width = 340, open = TRUE,
        div(class = "insight-box", style = "margin-top: 0;",
          icon("info-circle", style = "color: #0369A1;"),
          tags$strong(" Uses PSA results"),
          " — run PSA tab first."
        ),
        sliderInput("ceac_wtp_max", "Max WTP (Rs./QALY)",
                    50000, 1000000, 500000, 10000),
        sliderInput("ceac_wtp_step", "WTP step size (Rs.)",
                    5000, 50000, 10000, 5000),
        hr(),
        sliderInput("ceac_wtp_marker", "Your WTP threshold (Rs./QALY)",
                    50000, 1000000, 250000, 10000),
        div(class = "success-box", style = "font-size: 15px;",
          icon("flag", style = "color: #15803D;"),
          " India's suggested threshold: 1x GDP per capita = Rs. 2,50,000/QALY"
        )
      ),
      div(
        h3(icon("chart-line", style = "color: #0D7377;"),
           " Cost-Effectiveness Acceptability Curve"),
        p("The CEAC answers: ", tags$em("\"At my WTP, what is the probability
           the new treatment is cost-effective?\""),
          style = "color: #475569; font-size: 19px;"),
        plotOutput("ceac_plot", height = "600px"),

        div(class = "section-spacer"),
        uiOutput("ceac_annotation"),

        div(class = "section-spacer"),
        div(class = "insight-box",
          icon("hand-pointer", style = "color: #D97706;"),
          tags$strong(" Move the WTP slider"),
          " and watch the probability change in real-time.
           This is what R enables — interactive, explorable decision support
           that no static spreadsheet can match."
        )
      )
    )
  ),

  # ══════════════════════════════════════════════════════════════════════════
  # TAB 5: R vs Excel  (full width)
  # ══════════════════════════════════════════════════════════════════════════
  nav_panel(
    title = "R vs Excel",
    icon = icon("scale-balanced"),
    div(style = "max-width: 1200px; margin: 0 auto; padding: 32px 24px;",

      h2(icon("scale-balanced", style = "color: #0D7377;"),
         " The Comparison",
         style = "margin-bottom: 32px;"),

      tags$table(
        class = "table table-hover comparison-table",
        style = "width: 100%;",
        tags$thead(
          tags$tr(
            tags$th("Criterion", style = "width: 22%;"),
            tags$th(icon("file-excel"), " Excel", style = "width: 39%; color: #15803D;"),
            tags$th(icon("r-project"), " R", style = "width: 39%; color: #0D7377;")
          )
        ),
        tags$tbody(
          tags$tr(
            tags$td(tags$strong("Reproducibility")),
            tags$td("Cell references hidden; copy-paste errors common"),
            tags$td(tags$strong("Script = complete audit trail"))
          ),
          tags$tr(
            tags$td(tags$strong("PSA (10,000 sims)")),
            tags$td("Thousands of rows, slow, crash-prone"),
            tags$td(tags$strong("3 lines of R, < 1 second"))
          ),
          tags$tr(
            tags$td(tags$strong("Markov Models")),
            tags$td("Drag formulas across states x cycles"),
            tags$td(tags$strong("Matrix multiplication: 1 line"))
          ),
          tags$tr(
            tags$td(tags$strong("Sensitivity Analysis")),
            tags$td("Manual data tables, limited dimensions"),
            tags$td(tags$strong("Loops + parallel computing"))
          ),
          tags$tr(
            tags$td(tags$strong("Visualization")),
            tags$td("Basic charts, limited customization"),
            tags$td(tags$strong("ggplot2, plotly, Shiny dashboards"))
          ),
          tags$tr(
            tags$td(tags$strong("Collaboration")),
            tags$td("Email attachments, version confusion"),
            tags$td(tags$strong("Git + GitHub, branching, code review"))
          ),
          tags$tr(
            tags$td(tags$strong("Cost")),
            tags$td("Licence fee (Rs. 5,000-15,000/year)"),
            tags$td(tags$strong("Free & open-source, forever"))
          ),
          tags$tr(
            tags$td(tags$strong("Regulatory Acceptance")),
            tags$td("Common but scrutinized for errors"),
            tags$td(tags$strong("NICE, HTAIn, WHO — all accept R"))
          )
        )
      ),

      div(class = "section-spacer"),
      div(class = "success-box", style = "padding: 28px 32px;",
        h3(icon("graduation-cap", style = "color: #15803D;"),
           " Workshop Promise",
           style = "margin: 0 0 12px 0; color: #15803D;"),
        p("Over the next 3 days, you will build every model type shown in this app —
           from scratch, in R. No prior R experience required.",
          style = "margin: 0; font-size: 22px; font-weight: 500;")
      )
    )
  )
)

# ============================================================================
# SERVER
# ============================================================================

server <- function(input, output, session) {

  # ── Reinhart-Rogoff ─────────────────────────────────────────────────────

  rr_data <- reactive({
    data.frame(
      code = c("AUS","AUT","BEL","CAN","DNK","GRC","IRL","ITA",
               "JPN","NZL","NOR","SWE","GBR","USA"),
      country = c("Australia","Austria","Belgium","Canada","Denmark",
                   "Greece","Ireland","Italy","Japan","New Zealand",
                   "Norway","Sweden","UK","USA"),
      growth_above90 = c(3.8, 1.1, 2.6, 2.0, 2.5,
                         2.9, 2.4, 0.8, 0.5, 7.7,
                         3.4, 2.2, 1.5, 2.1),
      growth_below90 = c(3.1, 3.3, 3.6, 3.0, 3.0,
                         4.0, 4.1, 4.2, 4.5, 3.5,
                         4.0, 3.5, 2.4, 3.4),
      stringsAsFactors = FALSE
    )
  })

  # ── R-R Claimed GDP Growth Chart ──
  output$rr_claimed_chart <- renderPlot({
    claimed <- data.frame(
      Debt = factor(c("Below 30%", "30–60%", "60–90%", "Above 90%"),
                    levels = c("Above 90%", "60–90%", "30–60%", "Below 30%")),
      Growth = c(4.1, 2.9, 3.4, -0.1)
    )
    ggplot(claimed, aes(x = Debt, y = Growth, fill = Growth < 0)) +
      geom_col(width = 0.65, show.legend = FALSE) +
      geom_text(aes(label = paste0(Growth, "%"),
                     hjust = ifelse(Growth < 0, 1.2, -0.2)),
                size = 6, fontface = "bold",
                color = ifelse(claimed$Growth < 0, "#C81D4E", "#1e293b")) +
      scale_fill_manual(values = c("FALSE" = "#0D7377", "TRUE" = "#C81D4E")) +
      coord_flip() +
      labs(title = "Claimed: Average GDP Growth by Debt Level",
           x = NULL, y = "Real GDP Growth (%)") +
      theme_projector(base_size = 16) +
      theme(
        axis.text.y = element_text(size = 14, face = "bold"),
        plot.title = element_text(size = 16, face = "bold"),
        panel.grid.major.y = element_blank()
      )
  })

  output$rr_error_detail <- renderUI({
    err <- input$rr_error
    if (err == "exclude") {
      div(class = "danger-box",
        h4("Error 1: Excel Row Exclusion", style = "color: #C81D4E; margin-bottom: 12px;"),
        p("The AVERAGE formula covered rows 30-44 instead of 30-49.",
          tags$strong("Five countries were simply left out."),
          "A drag-select error — the kind Excel makes invisible."),
        div(class = "code-display",
          HTML(paste0(
            '<span class="cmt"># In R, this can\'t happen:</span>\n',
            '<span class="fn">mean</span>(df$growth)  ',
            '<span class="cmt"># always uses ALL rows</span>'
          ))
        )
      )
    } else if (err == "omit") {
      div(class = "danger-box",
        h4("Error 2: Selective Data Omission", style = "color: #C81D4E; margin-bottom: 12px;"),
        p("New Zealand had debt > 90% GDP with",
          tags$strong("growth of 7.7%"),
          "— among the highest. It was excluded."),
        p("Including it changes the average from",
          tags$strong("-0.1% to +2.2%"),
          "— flipping the conclusion.")
      )
    } else {
      div(class = "danger-box",
        h4("Error 3: Unconventional Weighting", style = "color: #C81D4E; margin-bottom: 12px;"),
        p("Each", tags$em("country"), "got equal weight regardless of years in
           the high-debt category. New Zealand's 1 year = UK's 19 years."),
        p("In R, weighting is",
          tags$strong("explicit in code"),
          "— not buried in a formula pattern.")
      )
    }
  })

  output$rr_plot <- renderPlot({
    df <- rr_data()
    selected <- df[df$code %in% input$rr_countries, ]
    if (nrow(selected) == 0) return(NULL)

    avg_above <- mean(selected$growth_above90)
    plot_df <- data.frame(
      country = rep(selected$country, 2),
      category = rep(c("Debt > 90% GDP", "Debt < 90% GDP"), each = nrow(selected)),
      growth = c(selected$growth_above90, selected$growth_below90)
    )

    ggplot(plot_df, aes(x = reorder(country, growth), y = growth, fill = category)) +
      geom_col(position = position_dodge(width = 0.8), width = 0.7, alpha = 0.9) +
      geom_hline(yintercept = avg_above, linetype = "dashed", color = pal$coral,
                 linewidth = 1.5) +
      annotate("text", x = 0.8, y = avg_above + 0.3,
               label = paste0("Mean (>90%): ", round(avg_above, 1), "%"),
               hjust = 0, color = pal$coral, fontface = "bold", size = 6.5) +
      scale_fill_manual(values = c("Debt > 90% GDP" = pal$coral,
                                   "Debt < 90% GDP" = pal$teal)) +
      coord_flip() +
      labs(x = NULL, y = "Average GDP Growth (%)", fill = NULL,
           title = "GDP Growth by Debt Level",
           subtitle = paste0(nrow(selected), " of 14 countries selected")) +
      theme_projector(base_size = 20) +
      theme(panel.grid.major.y = element_blank())
  })

  output$rr_verdict <- renderUI({
    df <- rr_data()
    selected <- df[df$code %in% input$rr_countries, ]
    if (nrow(selected) == 0) return(NULL)
    avg <- mean(selected$growth_above90)
    n_excluded <- sum(!(df$code %in% input$rr_countries))

    if (avg < 1.0) {
      div(class = "danger-box", style = "font-size: 20px;",
        icon("triangle-exclamation"),
        tags$strong(paste0(" Average growth with debt >90%: ", round(avg, 1), "%")),
        " — Supports the austerity narrative.",
        if (n_excluded > 0) paste0(" (", n_excluded, " countries excluded)")
      )
    } else {
      div(class = "success-box", style = "font-size: 20px;",
        icon("check-circle"),
        tags$strong(paste0(" Average growth with debt >90%: ", round(avg, 1), "%")),
        " — The 'cliff' at 90% disappears."
      )
    }
  })

  # ── PSA logic ───────────────────────────────────────────────────────────

  psa_results <- eventReactive(input$psa_run, {
    set.seed(input$psa_seed)
    n <- input$psa_n
    cv <- input$psa_cv
    cost_a <- rnorm(n, input$psa_cost_a, input$psa_cost_a * cv)
    cost_b <- rnorm(n, input$psa_cost_b, input$psa_cost_b * cv)
    qaly_a <- rnorm(n, input$psa_qaly_a, input$psa_qaly_a * cv)
    qaly_b <- rnorm(n, input$psa_qaly_b, input$psa_qaly_b * cv)
    delta_cost <- cost_b - cost_a
    delta_qaly <- qaly_b - qaly_a
    icer <- delta_cost / delta_qaly

    data.frame(
      Sim = 1:n,
      Cost_A = round(cost_a, 0), Cost_B = round(cost_b, 0),
      QALY_A = round(qaly_a, 3), QALY_B = round(qaly_b, 3),
      Delta_Cost = round(delta_cost, 0),
      Delta_QALY = round(delta_qaly, 3),
      ICER = round(icer, 0)
    )
  }, ignoreNULL = FALSE)

  output$psa_ceplane <- renderPlot({
    df <- psa_results()
    if (is.null(df)) return(NULL)
    wtp <- input$psa_wtp
    df$CE <- ifelse(df$Delta_Cost / df$Delta_QALY < wtp & df$Delta_QALY > 0,
                    "Cost-effective", "Not cost-effective")
    df$CE[df$Delta_Cost < 0 & df$Delta_QALY > 0] <- "Dominant"
    ce_cols <- c("Cost-effective" = pal$teal, "Not cost-effective" = pal$coral,
                 "Dominant" = pal$green)

    # Ensure all four quadrants are always visible with symmetric axes
    x_abs <- max(abs(df$Delta_QALY), 0.01) * 1.25
    y_abs <- max(abs(df$Delta_Cost), 1) * 1.25

    ggplot(df, aes(x = Delta_QALY, y = Delta_Cost)) +
      # Quadrant shading for visual clarity
      annotate("rect", xmin = 0, xmax = x_abs, ymin = 0, ymax = y_abs,
               fill = "#F8FAFC", alpha = 0.5) +
      annotate("rect", xmin = -x_abs, xmax = 0, ymin = 0, ymax = y_abs,
               fill = "#FEF2F2", alpha = 0.3) +
      annotate("rect", xmin = 0, xmax = x_abs, ymin = -y_abs, ymax = 0,
               fill = "#F0FDF4", alpha = 0.3) +
      annotate("rect", xmin = -x_abs, xmax = 0, ymin = -y_abs, ymax = 0,
               fill = "#FFFBEB", alpha = 0.3) +
      # Axes and WTP line
      geom_hline(yintercept = 0, color = "#475569", linewidth = 0.8) +
      geom_vline(xintercept = 0, color = "#475569", linewidth = 0.8) +
      geom_abline(slope = wtp, intercept = 0, linetype = "dashed",
                  color = pal$amber, linewidth = 1.5) +
      geom_point(aes(color = CE), alpha = 0.3, size = 2.5) +
      scale_color_manual(values = ce_cols) +
      # Quadrant labels — always positioned in each corner
      annotate("text", x = x_abs * 0.55, y = y_abs * 0.85,
               label = "NE: More effective,\nmore costly", color = "#475569",
               size = 5.5, fontface = "italic") +
      annotate("text", x = x_abs * 0.55, y = -y_abs * 0.85,
               label = "SE: Dominant\n(better & cheaper)", color = pal$green,
               size = 5.5, fontface = "bold") +
      annotate("text", x = -x_abs * 0.55, y = y_abs * 0.85,
               label = "NW: Dominated\n(worse & costlier)", color = pal$coral,
               size = 5.5, fontface = "bold") +
      annotate("text", x = -x_abs * 0.55, y = -y_abs * 0.85,
               label = "SW: Less effective,\nless costly", color = "#64748B",
               size = 5.5, fontface = "italic") +
      # WTP label
      annotate("label", x = x_abs * 0.7,
               y = min(wtp * x_abs * 0.7, y_abs * 0.7),
               label = paste0("WTP = \u20B9", format(wtp, big.mark = ",")),
               fill = "white", color = pal$amber, fontface = "bold", size = 5.5,
               label.size = 0.8) +
      scale_x_continuous(limits = c(-x_abs, x_abs), expand = c(0, 0)) +
      scale_y_continuous(limits = c(-y_abs, y_abs), expand = c(0, 0)) +
      labs(x = "\u0394 QALYs (Treatment \u2212 Control)",
           y = "\u0394 Cost (Treatment \u2212 Control, \u20B9)",
           title = "Cost-Effectiveness Plane",
           subtitle = paste0(format(nrow(df), big.mark = ","),
                            " iterations  |  seed = ", input$psa_seed),
           color = NULL) +
      theme_projector(base_size = 20) +
      theme(legend.text = element_text(size = 16),
            legend.key.size = unit(1.5, "lines"))
  })

  output$psa_table <- renderDT({
    df <- psa_results()
    if (is.null(df)) return(NULL)
    display_df <- df[1:min(50, nrow(df)),
                     c("Sim", "Delta_Cost", "Delta_QALY", "ICER")]
    names(display_df) <- c("Sim #", "Delta Cost (Rs.)", "Delta QALYs", "ICER (Rs./QALY)")

    datatable(display_df,
              options = list(
                pageLength = 10,
                dom = 'tip',
                scrollX = FALSE,
                autoWidth = TRUE,
                columnDefs = list(list(className = 'dt-center', targets = '_all'))
              ),
              rownames = FALSE,
              class = "stripe hover",
              style = "bootstrap5") %>%
      formatCurrency(columns = c("Delta Cost (Rs.)", "ICER (Rs./QALY)"),
                     currency = "Rs. ", digits = 0) %>%
      formatRound(columns = "Delta QALYs", digits = 3)
  })

  output$psa_ceac_mini <- renderPlot({
    df <- psa_results()
    if (is.null(df)) return(NULL)
    wtp_range <- seq(0, input$psa_wtp * 2, length.out = 50)
    prob_ce <- sapply(wtp_range, function(w) {
      mean(df$Delta_Cost / df$Delta_QALY < w & df$Delta_QALY > 0 |
           (df$Delta_Cost < 0 & df$Delta_QALY > 0))
    })
    ceac_df <- data.frame(WTP = wtp_range, Prob_CE = prob_ce)

    ggplot(ceac_df, aes(x = WTP, y = Prob_CE)) +
      geom_line(color = pal$teal, linewidth = 2.5) +
      geom_vline(xintercept = input$psa_wtp, linetype = "dashed",
                 color = pal$coral, linewidth = 1.2) +
      geom_hline(yintercept = 0.5, linetype = "dotted", color = "#94A3B8",
                 linewidth = 0.8) +
      scale_x_continuous(labels = function(x) paste0("Rs.", x / 1000, "K")) +
      scale_y_continuous(labels = scales::percent, limits = c(0, 1)) +
      annotate("label", x = input$psa_wtp, y = 0.08,
               label = paste0("WTP = Rs. ", format(input$psa_wtp, big.mark = ",")),
               fill = "white", color = pal$coral, fontface = "bold", size = 5.5,
               label.size = 0.8) +
      labs(x = "Willingness-to-Pay (Rs./QALY)",
           y = "Probability Cost-Effective",
           title = "CEAC Preview") +
      theme_projector(base_size = 20)
  })

  output$psa_metrics <- renderUI({
    df <- psa_results()
    if (is.null(df)) return(NULL)
    wtp <- input$psa_wtp
    prop_ce <- mean(df$Delta_Cost / df$Delta_QALY < wtp & df$Delta_QALY > 0 |
                    (df$Delta_Cost < 0 & df$Delta_QALY > 0))
    mean_icer <- mean(df$ICER[is.finite(df$ICER)])
    prop_dominant <- mean(df$Delta_Cost < 0 & df$Delta_QALY > 0)

    fluidRow(
      column(3, div(class = "metric-card",
        div(class = "metric-value", paste0(round(prop_ce * 100, 1), "%")),
        div(class = "metric-label", "Prob. Cost-Effective")
      )),
      column(3, div(class = "metric-card",
        div(class = "metric-value",
            paste0("Rs.", format(round(mean_icer), big.mark = ","))),
        div(class = "metric-label", "Mean ICER")
      )),
      column(3, div(class = "metric-card",
        div(class = "metric-value", paste0(round(prop_dominant * 100, 1), "%")),
        div(class = "metric-label", "Dominant")
      )),
      column(3, div(class = "metric-card",
        div(class = "metric-value", format(nrow(df), big.mark = ",")),
        div(class = "metric-label", "Simulations")
      ))
    )
  })

  # PSA download
  output$psa_download <- downloadHandler(
    filename = function() { paste0("psa_simulations_seed", input$psa_seed, ".csv") },
    content = function(file) {
      df <- psa_results()
      if (!is.null(df)) write.csv(df, file, row.names = FALSE)
    }
  )

  # ── Markov model logic ─────────────────────────────────────────────────

  # Auto-computed stay probabilities
  mk_p_stay_h <- reactive({
    val <- 1 - input$mk_p_prog - input$mk_p_death_h
    max(0, round(val, 3))
  })

  mk_p_stay_s <- reactive({
    val <- 1 - input$mk_p_death_s
    max(0, round(val, 3))
  })

  # Display auto-adjusted stay probabilities
  output$mk_p_stay_h_display <- renderUI({
    val <- mk_p_stay_h()
    col <- if (val < 0.01) "#C81D4E" else "#15803D"
    div(style = paste0("padding: 8px 12px; background: #F8FAFC; border-radius: 8px;
                        border: 2px solid ", col, "; margin-top: 6px;"),
      span(style = paste0("font-size: 15px; font-weight: 700; color: ", col, ";"),
           paste0("P(Healthy -> Healthy) = ", val)),
      if (val < 0.01) span(style = "color: #C81D4E; font-size: 13px;",
                           " (Warning: too low!)")
    )
  })

  output$mk_p_stay_s_display <- renderUI({
    val <- mk_p_stay_s()
    col <- if (val < 0.01) "#C81D4E" else "#15803D"
    div(style = paste0("padding: 8px 12px; background: #F8FAFC; border-radius: 8px;
                        border: 2px solid ", col, "; margin-top: 6px;"),
      span(style = paste0("font-size: 15px; font-weight: 700; color: ", col, ";"),
           paste0("P(Sick -> Sick) = ", val)),
      if (val < 0.01) span(style = "color: #C81D4E; font-size: 13px;",
                           " (Warning: too low!)")
    )
  })

  # Transition probability matrix display
  output$mk_tp_matrix <- renderUI({
    p_prog <- input$mk_p_prog
    p_dh   <- input$mk_p_death_h
    p_ds   <- input$mk_p_death_s
    p_hh   <- mk_p_stay_h()
    p_ss   <- mk_p_stay_s()
    hr     <- input$mk_hr

    make_row <- function(from, vals, diag_idx) {
      cells <- lapply(seq_along(vals), function(i) {
        cls <- if (i == diag_idx) "diag" else ""
        cls <- if (vals[i] > 0 && i != diag_idx) paste(cls, "tp-val") else cls
        tags$td(class = cls, sprintf("%.3f", vals[i]))
      })
      do.call(tags$tr, c(list(tags$td(tags$strong(from))), cells))
    }

    div(
      h5("Control Arm", style = "color: #475569; margin-bottom: 8px;"),
      tags$table(class = "tp-matrix",
        tags$thead(tags$tr(
          tags$th("From \\ To"), tags$th("Healthy"), tags$th("Sick"), tags$th("Dead")
        )),
        tags$tbody(
          make_row("Healthy", c(p_hh, p_prog, p_dh), 1),
          make_row("Sick",    c(0, p_ss, p_ds), 2),
          make_row("Dead",    c(0, 0, 1), 3)
        )
      ),
      div(style = "height: 16px;"),
      h5("Treatment Arm", style = "color: #475569; margin-bottom: 8px;"),
      p(style = "font-size: 15px; color: #64748b;",
        paste0("HR = ", input$mk_hr, " applied to Healthy -> Sick")),
      tags$table(class = "tp-matrix",
        tags$thead(tags$tr(
          tags$th("From \\ To"), tags$th("Healthy"), tags$th("Sick"), tags$th("Dead")
        )),
        tags$tbody(
          make_row("Healthy", c(max(0, 1 - p_prog * hr - p_dh),
                                p_prog * hr, p_dh), 1),
          make_row("Sick",    c(0, p_ss, p_ds), 2),
          make_row("Dead",    c(0, 0, 1), 3)
        )
      )
    )
  })

  # Model state-transition diagram (DiagrammeR) — triangular layout
  output$mk_diagram <- renderGrViz({
    p_prog <- input$mk_p_prog
    p_dh   <- input$mk_p_death_h
    p_ds   <- input$mk_p_death_s
    p_hh   <- mk_p_stay_h()
    p_ss   <- mk_p_stay_s()

    grViz(sprintf("
      digraph MarkovModel {
        graph [rankdir=TB, bgcolor='white', fontname='Helvetica',
               pad=0.3, nodesep=1.5, ranksep=1.0, dpi=62, margin=0.2,
               label='State-Transition Diagram\\n(Unidirectional Progressive Model)',
               labelloc=t, fontsize=16, fontcolor='#475569']

        node [fontname='Helvetica', style='filled',
              penwidth=3, fixedsize=true]
        edge [fontname='Helvetica', penwidth=2.5, arrowsize=1.1]

        # ── Rank: Healthy and Sick side by side ──
        { rank=same;
          Healthy [label='Healthy\\n(H)', fillcolor='#CCFBF1', color='#0D7377',
                   fontcolor='#0D7377', shape=circle, fontsize=18,
                   width=1.8, height=1.8]
          Sick    [label='Sick\\n(S)', fillcolor='#FEF3C7', color='#D97706',
                   fontcolor='#92400E', shape=circle, fontsize=18,
                   width=1.8, height=1.8]
        }

        # ── Dead below ──
        Dead [label='Dead\\n(Absorbing)', fillcolor='#F1F5F9', color='#64748B',
              fontcolor='#475569', shape=doublecircle, fontsize=16,
              width=1.8, height=1.8]

        # ── Disease progression ──
        Healthy -> Sick [label=<<B>  %.3f  </B>>,
                         color='#D97706', fontcolor='#92400E', fontsize=16,
                         penwidth=3]

        # ── Mortality ──
        Healthy -> Dead [label=<<I>  %.4f  </I>>,
                         color='#94A3B8', fontcolor='#64748B', fontsize=13,
                         style=dashed]
        Sick -> Dead    [label=<<B>  %.3f  </B>>,
                         color='#C81D4E', fontcolor='#C81D4E', fontsize=16,
                         penwidth=3]

        # ── Self-loops (staying in state) ──
        Healthy -> Healthy [label=<<I>Stay: %.3f</I>>,
                            color='#0D7377', fontcolor='#0D7377', fontsize=13]
        Sick -> Sick       [label=<<I>Stay: %.3f</I>>,
                            color='#D97706', fontcolor='#92400E', fontsize=13]
      }
    ", p_prog, p_dh, p_ds, p_hh, p_ss))
  })

  mk_results <- eventReactive(input$mk_run, {
    p_prog <- input$mk_p_prog
    p_dh <- input$mk_p_death_h
    p_ds <- input$mk_p_death_s
    hr <- input$mk_hr
    horizon <- input$mk_horizon
    disc <- input$mk_discount

    P_control <- matrix(c(
      mk_p_stay_h(), p_prog,       p_dh,
      0,             1 - p_ds,     p_ds,
      0,             0,            1
    ), nrow = 3, byrow = TRUE)

    P_treat <- matrix(c(
      max(0, 1 - p_prog * hr - p_dh), p_prog * hr, p_dh,
      0,                               1 - p_ds,    p_ds,
      0,                               0,           1
    ), nrow = 3, byrow = TRUE)

    cohort <- 1000
    trace_c <- trace_t <- matrix(0, nrow = horizon + 1, ncol = 3)
    trace_c[1, ] <- trace_t[1, ] <- c(cohort, 0, 0)
    for (i in 2:(horizon + 1)) {
      trace_c[i, ] <- trace_c[i - 1, ] %*% P_control
      trace_t[i, ] <- trace_t[i - 1, ] %*% P_treat
    }
    colnames(trace_c) <- colnames(trace_t) <- c("Healthy", "Sick", "Dead")

    disc_vec <- 1 / (1 + disc) ^ (0:horizon)
    costs_c <- sum((trace_c[, 1] * input$mk_cost_h +
                     trace_c[, 2] * input$mk_cost_s) * disc_vec)
    costs_t <- sum((trace_t[, 1] * (input$mk_cost_h + input$mk_cost_trt) +
                     trace_t[, 2] * input$mk_cost_s) * disc_vec)
    qalys_c <- sum((trace_c[, 1] * input$mk_util_h +
                     trace_c[, 2] * input$mk_util_s) * disc_vec) / cohort
    qalys_t <- sum((trace_t[, 1] * input$mk_util_h +
                     trace_t[, 2] * input$mk_util_s) * disc_vec) / cohort

    list(trace_c = trace_c, trace_t = trace_t,
         cost_c = costs_c, cost_t = costs_t,
         qaly_c = qalys_c, qaly_t = qalys_t,
         horizon = horizon)
  }, ignoreNULL = FALSE)

  observeEvent(input$mk_horizon, {
    updateSliderInput(session, "mk_cycle_view", max = input$mk_horizon,
                      value = min(input$mk_cycle_view, input$mk_horizon))
  })

  output$mk_trace_plot <- renderPlot({
    res <- mk_results()
    if (is.null(res)) return(NULL)
    horizon <- res$horizon
    cycles_show <- min(input$mk_cycle_view, horizon)
    if (cycles_show < 1) return(NULL)

    # Build data for BOTH arms with line traces (not stacked area)
    tc <- res$trace_c[1:(cycles_show + 1), , drop = FALSE]
    tt <- res$trace_t[1:(cycles_show + 1), , drop = FALSE]

    df <- data.frame(
      Cycle = rep(0:cycles_show, 6),
      State = rep(c("Healthy", "Sick", "Dead"), each = cycles_show + 1, times = 2),
      Arm   = rep(c("Control", "Treatment"), each = 3 * (cycles_show + 1)),
      Count = c(tc[, 1], tc[, 2], tc[, 3],
                tt[, 1], tt[, 2], tt[, 3])
    )
    df$State <- factor(df$State, levels = c("Healthy", "Sick", "Dead"))

    state_cols <- c("Healthy" = pal$teal, "Sick" = pal$amber, "Dead" = "#94A3B8")

    ggplot(df, aes(x = Cycle, y = Count, color = State, linetype = Arm)) +
      geom_line(linewidth = 1.8, alpha = 0.9) +
      geom_point(size = 2.2, alpha = 0.7) +
      scale_color_manual(values = state_cols) +
      scale_linetype_manual(values = c("Control" = "dashed", "Treatment" = "solid")) +
      scale_x_continuous(limits = c(0, horizon),
                         breaks = seq(0, horizon, by = max(1, horizon %/% 10))) +
      scale_y_continuous(limits = c(0, 1000), breaks = seq(0, 1000, 200)) +
      labs(x = "Cycle (year)", y = "Cohort (n = 1,000)",
           title = paste0("Markov Trace — Cycle 0 to ", cycles_show),
           subtitle = "Solid = Treatment | Dashed = Control",
           color = "State", linetype = "Arm") +
      theme_projector(base_size = 20) +
      theme(legend.box = "horizontal",
            legend.text = element_text(size = 15))
  })

  output$mk_trace_table <- renderDT({
    res <- mk_results()
    if (is.null(res)) return(NULL)
    tr <- res$trace_t
    show_cycles <- c(0, 1, 2, 3, 5, 8, 10, 15, 20, 25, 30, 35, 40)
    show_cycles <- show_cycles[show_cycles <= res$horizon]

    tbl <- data.frame(
      Cycle = show_cycles,
      Healthy = round(tr[show_cycles + 1, 1], 1),
      Sick = round(tr[show_cycles + 1, 2], 1),
      Dead = round(tr[show_cycles + 1, 3], 1)
    )

    datatable(tbl,
              options = list(
                dom = 't',
                pageLength = 20,
                scrollX = FALSE,
                autoWidth = TRUE,
                columnDefs = list(list(className = 'dt-center', targets = '_all'))
              ),
              rownames = FALSE,
              class = "stripe hover",
              style = "bootstrap5")
  })

  output$mk_results <- renderUI({
    res <- mk_results()
    if (is.null(res)) return(NULL)
    inc_cost <- res$cost_t - res$cost_c
    inc_qaly <- res$qaly_t - res$qaly_c
    icer <- if (inc_qaly != 0) inc_cost / inc_qaly else NA

    interp <- if (!is.na(icer)) {
      if (inc_cost < 0 & inc_qaly > 0) "Dominant (better & cheaper)"
      else if (inc_cost > 0 & inc_qaly < 0) "Dominated (worse & costlier)"
      else if (icer < 250000) "Cost-effective at Rs. 2.5L/QALY"
      else "Not cost-effective at Rs. 2.5L/QALY"
    } else "Undefined"

    fluidRow(
      column(4, div(class = "metric-card",
        div(class = "metric-label", "CONTROL"),
        div(class = "metric-value", style = "font-size: 28px;",
            paste0("Rs.", format(round(res$cost_c / 1000), big.mark = ","), "K")),
        div(class = "metric-label", paste0(round(res$qaly_c, 2), " QALYs"))
      )),
      column(4, div(class = "metric-card",
        div(class = "metric-label", "TREATMENT"),
        div(class = "metric-value", style = "font-size: 28px;",
            paste0("Rs.", format(round(res$cost_t / 1000), big.mark = ","), "K")),
        div(class = "metric-label", paste0(round(res$qaly_t, 2), " QALYs"))
      )),
      column(4, div(class = "metric-card",
            style = if (grepl("Dominant|Cost-effective", interp))
              "border-color: #15803D; background: #F0FDF4;"
            else "border-color: #C81D4E; background: #FEF2F2;",
        div(class = "metric-label", "ICER (Rs./QALY)"),
        div(class = "metric-value", style = "font-size: 32px;",
          if (!is.na(icer)) paste0("Rs.", format(round(icer), big.mark = ","))
          else "---"
        ),
        div(style = "margin-top: 8px; font-size: 18px; font-weight: 700;",
          if (grepl("Dominant|Cost-effective", interp))
            span(style = "color: #15803D;", icon("check-circle"), " ", interp)
          else
            span(style = "color: #C81D4E;", icon("times-circle"), " ", interp)
        )
      ))
    )
  })

  # Markov download
  output$mk_download <- downloadHandler(
    filename = function() { "markov_trace.csv" },
    content = function(file) {
      res <- mk_results()
      if (!is.null(res)) {
        tr <- res$trace_t
        tbl <- data.frame(
          Cycle = 0:res$horizon,
          Healthy = round(tr[, 1], 2),
          Sick = round(tr[, 2], 2),
          Dead = round(tr[, 3], 2)
        )
        write.csv(tbl, file, row.names = FALSE)
      }
    }
  )

  # ── CEAC Explorer ───────────────────────────────────────────────────────

  output$ceac_plot <- renderPlot({
    df <- psa_results()
    if (is.null(df)) return(NULL)
    wtp_range <- seq(0, input$ceac_wtp_max, by = input$ceac_wtp_step)
    prob_ce <- sapply(wtp_range, function(w) {
      mean(df$Delta_Cost / df$Delta_QALY < w & df$Delta_QALY > 0 |
           (df$Delta_Cost < 0 & df$Delta_QALY > 0))
    })
    ceac_df <- data.frame(WTP = wtp_range, Prob_CE = prob_ce)
    marker_wtp <- input$ceac_wtp_marker
    marker_prob <- approx(ceac_df$WTP, ceac_df$Prob_CE, xout = marker_wtp)$y

    ggplot(ceac_df, aes(x = WTP, y = Prob_CE)) +
      geom_ribbon(aes(ymin = 0, ymax = Prob_CE), fill = pal$teal, alpha = 0.12) +
      geom_line(color = pal$teal, linewidth = 3) +
      geom_vline(xintercept = marker_wtp, linetype = "dashed",
                 color = pal$coral, linewidth = 1.5) +
      geom_point(data = data.frame(x = marker_wtp, y = marker_prob),
                 aes(x = x, y = y), color = pal$coral, size = 7) +
      geom_hline(yintercept = 0.5, linetype = "dotted", color = "#94A3B8",
                 linewidth = 0.8) +
      annotate("label", x = marker_wtp, y = min(1, marker_prob + 0.08),
               label = paste0(round(marker_prob * 100, 1), "% CE\nat Rs. ",
                              format(marker_wtp, big.mark = ",")),
               fill = "white", color = pal$coral, fontface = "bold", size = 6,
               label.size = 1, label.padding = unit(0.6, "lines")) +
      scale_x_continuous(labels = function(x) paste0("Rs.", x / 1000, "K"),
                         expand = c(0.02, 0)) +
      scale_y_continuous(labels = scales::percent, limits = c(0, 1),
                         expand = c(0.02, 0)) +
      labs(x = "Willingness-to-Pay Threshold (Rs./QALY)",
           y = "Probability Cost-Effective",
           title = "Cost-Effectiveness Acceptability Curve",
           subtitle = paste0("Based on ", format(nrow(df), big.mark = ","),
                            " PSA simulations")) +
      theme_projector(base_size = 20)
  })

  output$ceac_annotation <- renderUI({
    df <- psa_results()
    if (is.null(df)) return(NULL)
    marker_wtp <- input$ceac_wtp_marker
    prob <- mean(df$Delta_Cost / df$Delta_QALY < marker_wtp & df$Delta_QALY > 0 |
                 (df$Delta_Cost < 0 & df$Delta_QALY > 0))
    if (prob >= 0.9) {
      div(class = "success-box", style = "font-size: 20px;",
        icon("check-circle"),
        tags$strong(paste0(" ", round(prob * 100, 1),
                          "% probability of being cost-effective.")),
        " Strong evidence to recommend at this WTP."
      )
    } else if (prob >= 0.5) {
      div(class = "insight-box", style = "font-size: 20px;",
        icon("scale-balanced"),
        tags$strong(paste0(" ", round(prob * 100, 1),
                          "% probability of being cost-effective.")),
        " More likely than not, but uncertainty remains."
      )
    } else {
      div(class = "danger-box", style = "font-size: 20px;",
        icon("triangle-exclamation"),
        tags$strong(paste0(" Only ", round(prob * 100, 1),
                          "% probability of being cost-effective.")),
        " Standard treatment likely preferable at this WTP."
      )
    }
  })
}

# ============================================================================
shinyApp(ui, server)
