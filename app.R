# ==============================================================================
# Labour Market Briefing Dashboard - Shiny App
# ==============================================================================
# GOV.UK Design System styled UI with automatic date detection
# ==============================================================================

suppressPackageStartupMessages({
  library(shiny)
  library(shinythemes)
  library(DBI)
  library(RPostgres)
  library(dplyr)
  library(lubridate)
})

# ==============================================================================
# UI
# ==============================================================================

ui <- fluidPage(
  theme = shinytheme("flatly"),

  # GOV.UK CSS styling
  tags$head(
    tags$style(HTML("
      @import url('https://fonts.googleapis.com/css2?family=Source+Sans+Pro:wght@400;600;700&display=swap');

      * {
        box-sizing: border-box;
      }

      body {
        font-family: 'Source Sans Pro', Arial, sans-serif;
        font-size: 19px;
        line-height: 1.5;
        color: #0b0c0c;
        background: #f3f2f1;
        margin: 0;
        padding: 0;
      }

      .container-fluid {
        max-width: 960px;
        margin: 0 auto;
        padding: 0;
      }

      /* GOV.UK Header */
      .govuk-header {
        background: #0b0c0c;
        padding: 10px 0;
        border-bottom: 10px solid #1d70b8;
      }

      .govuk-header-content {
        max-width: 960px;
        margin: 0 auto;
        padding: 10px 30px;
      }

      .govuk-header-logo {
        font-size: 30px;
        font-weight: 700;
        color: #ffffff;
      }

      /* Phase Banner */
      .govuk-phase-banner {
        background: #ffffff;
        border-bottom: 1px solid #b1b4b6;
        padding: 10px 30px;
      }

      .govuk-phase-banner-content {
        max-width: 960px;
        margin: 0 auto;
        display: flex;
        align-items: center;
        gap: 15px;
        font-size: 16px;
      }

      .govuk-phase-tag {
        background: #1d70b8;
        color: #ffffff;
        padding: 4px 8px;
        font-weight: 700;
        font-size: 14px;
        text-transform: uppercase;
        letter-spacing: 1px;
      }

      /* Main Content */
      .govuk-main {
        background: #ffffff;
        padding: 40px 30px 60px 30px;
        min-height: calc(100vh - 200px);
      }

      .govuk-main-inner {
        max-width: 900px;
      }

      /* Typography */
      .govuk-heading-xl {
        font-size: 48px;
        font-weight: 700;
        line-height: 1.1;
        margin: 0 0 30px 0;
        color: #0b0c0c;
      }

      .govuk-heading-l {
        font-size: 36px;
        font-weight: 700;
        line-height: 1.1;
        margin: 50px 0 20px 0;
        color: #0b0c0c;
      }

      .govuk-heading-m {
        font-size: 24px;
        font-weight: 700;
        line-height: 1.2;
        margin: 30px 0 15px 0;
        color: #0b0c0c;
      }

      .govuk-body {
        font-size: 19px;
        margin: 0 0 20px 0;
        color: #0b0c0c;
      }

      .govuk-body-s {
        font-size: 16px;
        color: #505a5f;
      }

      .govuk-caption-xl {
        font-size: 27px;
        font-weight: 400;
        color: #505a5f;
        margin-bottom: 5px;
        display: block;
      }

      /* Buttons */
      .govuk-button {
        font-family: inherit;
        font-size: 19px;
        font-weight: 600;
        line-height: 1;
        padding: 12px 16px 10px;
        border: none;
        border-radius: 0;
        cursor: pointer;
        display: inline-flex;
        align-items: center;
        gap: 10px;
        text-decoration: none;
        margin-right: 15px;
        margin-bottom: 15px;
        min-width: 200px;
        justify-content: center;
      }

      .govuk-button:focus {
        outline: 3px solid #ffdd00;
        outline-offset: 0;
        background: #ffdd00;
        box-shadow: 0 2px 0 #0b0c0c;
        color: #0b0c0c;
      }

      .govuk-button--primary {
        background: #00703c;
        color: #ffffff;
        box-shadow: 0 2px 0 #002d18;
      }

      .govuk-button--primary:hover {
        background: #005a30;
        color: #ffffff;
      }

      .govuk-button--secondary {
        background: #f3f2f1;
        color: #0b0c0c;
        box-shadow: 0 2px 0 #505a5f;
      }

      .govuk-button--secondary:hover {
        background: #dbdad9;
        color: #0b0c0c;
      }

      /* Button Group */
      .govuk-button-group {
        margin: 30px 0;
        display: flex;
        flex-wrap: wrap;
      }

      /* Inset Text */
      .govuk-inset-text {
        border-left: 10px solid #b1b4b6;
        padding: 15px 20px;
        margin: 30px 0;
      }

      .govuk-inset-text--success {
        border-color: #00703c;
        background: #e6f4ed;
      }

      .govuk-inset-text--error {
        border-color: #d4351c;
        background: #fbeae5;
      }

      .govuk-inset-text--info {
        border-color: #1d70b8;
        background: #e8f1f8;
      }

      /* Panel */
      .govuk-panel {
        background: #f3f2f1;
        padding: 35px;
        margin: 30px 0;
      }

      .govuk-panel__title {
        font-size: 27px;
        font-weight: 700;
        margin: 0 0 20px 0;
      }

      /* Tables */
      .govuk-table {
        width: 100%;
        border-collapse: collapse;
        margin: 30px 0;
        font-size: 18px;
      }

      .govuk-table__header,
      .govuk-table__cell {
        padding: 15px 20px 15px 0;
        border-bottom: 1px solid #b1b4b6;
        text-align: left;
        vertical-align: top;
      }

      .govuk-table__header {
        font-weight: 700;
      }

      .govuk-table__header--numeric,
      .govuk-table__cell--numeric {
        text-align: right;
      }

      .govuk-table__row:hover {
        background: #f3f2f1;
      }

      .govuk-positive {
        color: #00703c;
        font-weight: 600;
      }

      .govuk-negative {
        color: #d4351c;
        font-weight: 600;
      }

      /* Lists */
      .govuk-list {
        margin: 20px 0;
        padding: 0;
        list-style: none;
      }

      .govuk-list--number {
        counter-reset: item;
        padding-left: 0;
      }

      .govuk-list--number > li {
        counter-increment: item;
        margin-bottom: 15px;
        padding-left: 40px;
        position: relative;
      }

      .govuk-list--number > li::before {
        content: counter(item) '.';
        position: absolute;
        left: 0;
        font-weight: 700;
        color: #0b0c0c;
      }

      /* Section Break */
      .govuk-section-break {
        margin: 0;
        border: 0;
      }

      .govuk-section-break--xl {
        margin-top: 50px;
        margin-bottom: 50px;
      }

      .govuk-section-break--visible {
        border-bottom: 1px solid #b1b4b6;
      }

      /* Footer */
      .govuk-footer {
        background: #f3f2f1;
        border-top: 1px solid #b1b4b6;
        padding: 25px 30px;
        font-size: 16px;
        color: #505a5f;
      }

      .govuk-footer-content {
        max-width: 960px;
        margin: 0 auto;
      }

      /* Document Preview - matches DBoutput.docx */
      .doc-preview {
        background: #ffffff;
        border: 1px solid #d0d0d0;
        margin: 30px 0;
        font-family: Arial, sans-serif;
      }

      .doc-header {
        padding: 20px 25px;
        border-bottom: 1px solid #d0d0d0;
      }

      .doc-header__title {
        font-size: 22px;
        font-weight: 700;
        color: #00285F;
        margin: 0 0 8px 0;
      }

      .doc-header__title a {
        color: #1d70b8;
        text-decoration: underline;
      }

      .doc-header__subtitle {
        font-size: 14px;
        color: #00285F;
        margin: 0;
      }

      .doc-takeaways {
        background: #00285F;
        color: #ffffff;
        padding: 20px 25px;
        margin: 0;
      }

      .doc-takeaways__title {
        font-size: 17px;
        font-weight: 700;
        margin: 0 0 15px 0;
        color: #ffffff;
      }

      .doc-takeaways__list {
        margin: 0;
        padding-left: 20px;
        list-style-type: disc;
      }

      .doc-takeaways__list li {
        margin-bottom: 12px;
        font-size: 14px;
        line-height: 1.5;
        color: #ffffff;
      }

      .doc-section {
        padding: 20px 25px;
        border-bottom: 1px solid #d0d0d0;
      }

      .doc-section:last-child {
        border-bottom: none;
      }

      .doc-section__title {
        font-size: 16px;
        font-weight: 700;
        color: #00285F;
        margin: 0 0 15px 0;
        padding-bottom: 8px;
        border-bottom: 2px solid #00285F;
      }

      .doc-topten {
        margin: 0;
        padding: 0;
        list-style: none;
        counter-reset: topten;
      }

      .doc-topten li {
        counter-increment: topten;
        margin-bottom: 15px;
        padding-left: 8px;
        font-size: 14px;
        line-height: 1.6;
        color: #0b0c0c;
      }

      .doc-topten li::before {
        content: counter(topten) '. ';
        font-weight: 700;
        color: #00285F;
      }

      .doc-table {
        width: 100%;
        border-collapse: collapse;
        font-size: 13px;
      }

      .doc-table th {
        background: #00285F;
        color: #ffffff;
        padding: 10px 12px;
        text-align: center;
        font-weight: 600;
        border: 1px solid #00285F;
      }

      .doc-table th:first-child {
        text-align: left;
      }

      .doc-table td {
        padding: 8px 12px;
        border: 1px solid #d0d0d0;
        text-align: center;
      }

      .doc-table td:first-child {
        text-align: left;
        font-weight: 500;
      }

      .doc-table tr:nth-child(even) {
        background: #f8f8f8;
      }

      .doc-positive { color: #00703c; }
      .doc-negative { color: #d4351c; }

      .doc-footer {
        padding: 15px 25px;
        font-size: 12px;
        color: #505a5f;
        border-top: 1px solid #d0d0d0;
        background: #f8f8f8;
      }

      /* Shiny Overrides */
      .shiny-notification {
        border-radius: 0;
        border: none;
        border-left: 5px solid #1d70b8;
        font-family: inherit;
      }

      .progress {
        height: 10px;
        border-radius: 0;
        background: #b1b4b6;
      }

      .progress-bar {
        background: #00703c;
        border-radius: 0;
      }

      @media (max-width: 640px) {
        .govuk-heading-xl { font-size: 32px; }
        .govuk-heading-l { font-size: 27px; }
        .govuk-heading-m { font-size: 21px; }
        body { font-size: 16px; }
        .govuk-button { width: 100%; margin-right: 0; }
      }
    "))
  ),

  # GOV.UK Header
  div(class = "govuk-header",
    div(class = "govuk-header-content",
      span(class = "govuk-header-logo", "Labour Market Statistics")
    )
  ),

  # Phase Banner
  div(class = "govuk-phase-banner",
    div(class = "govuk-phase-banner-content",
      span(class = "govuk-phase-tag", "LIVE"),
      span(textOutput("release_label", inline = TRUE))
    )
  ),

  # Main Content
  div(class = "govuk-main",
    div(class = "govuk-main-inner",

      # Page Title
      h1(class = "govuk-heading-xl",
        span(class = "govuk-caption-xl", "Statistical Briefing"),
        "Labour Market Statistics Briefing"
      ),

      p(class = "govuk-body",
        "Generate and preview labour market statistics briefings. Data is sourced from the latest ONS releases."
      ),

      # Preview Section
      h2(class = "govuk-heading-l", "Preview"),
      p(class = "govuk-body-s", "Review the data before generating documents"),

      div(class = "govuk-button-group",
        actionButton("preview_word",
                     tagList(icon("file-lines"), " Document Preview"),
                     class = "govuk-button govuk-button--secondary")
      ),

      hr(class = "govuk-section-break govuk-section-break--xl govuk-section-break--visible"),

      # Generate Section
      h2(class = "govuk-heading-l", "Generate Reports"),
      p(class = "govuk-body-s", "Download final documents"),

      div(class = "govuk-button-group",
        downloadButton("download_word",
                       tagList(icon("file-word"), " Download Word Document"),
                       class = "govuk-button govuk-button--primary"),
        downloadButton("download_excel",
                       tagList(icon("file-excel"), " Download Excel Workbook"),
                       class = "govuk-button govuk-button--primary")
      ),

      # Status message
      uiOutput("status_message"),

      # Preview output area
      uiOutput("preview_output")
    )
  ),

  # Footer
  div(class = "govuk-footer",
    div(class = "govuk-footer-content",
      "Data sourced from ONS Labour Market Statistics"
    )
  )
)

# ==============================================================================
# SERVER
# ==============================================================================

server <- function(input, output, session) {

  # Paths relative to project root
  config_path       <- "utils/config.R"
  calculations_path <- "utils/calculations.R"
  word_script_path  <- "utils/word_output.R"
  excel_script_path <- "sheets/excel_audit.R"
  summary_path      <- "sheets/summary.R"
  top_ten_path      <- "sheets/top_ten_stats.R"
  template_path     <- "utils/DB.docx"

  # Reactive values
  status <- reactiveVal(list(type = "info", message = "Ready to generate reports"))
  cached_env <- reactiveVal(NULL)

  # Get briefing label for filenames
  get_briefing_label <- reactive({
    env <- cached_env()
    if (!is.null(env) && exists("briefing_release_label", envir = env)) {
      get("briefing_release_label", envir = env)
    } else {
      format(Sys.Date(), "%B %Y")
    }
  })

  # Get the release label dynamically on startup
  output$release_label <- renderText({
    tryCatch({
      env <- new.env()
      source(calculations_path, local = env)
      cached_env(env)
      if (exists("briefing_release_label", envir = env)) {
        paste("Data release:", get("briefing_release_label", envir = env))
      } else {
        paste("Data release:", format(Sys.Date(), "%B %Y"))
      }
    }, error = function(e) {
      paste("Data release:", format(Sys.Date(), "%B %Y"))
    })
  })

  # Status message output
  output$status_message <- renderUI({
    s <- status()
    if (!is.null(s$message)) {
      css_class <- switch(s$type,
        "success" = "govuk-inset-text govuk-inset-text--success",
        "error" = "govuk-inset-text govuk-inset-text--error",
        "govuk-inset-text govuk-inset-text--info"
      )
      div(class = css_class, s$message)
    }
  })

  # Preview Word Document
  observeEvent(input$preview_word, {
    withProgress(message = "Generating Document Preview", value = 0, {
      tryCatch({
        incProgress(0.1, detail = "Loading calculations...")
        env <- new.env()
        source(calculations_path, local = env)
        cached_env(env)

        incProgress(0.3, detail = "Loading summary generator...")
        source(summary_path, local = env)

        incProgress(0.4, detail = "Loading top ten generator...")
        source(top_ten_path, local = env)

        incProgress(0.5, detail = "Generating summary...")
        summary <- env$generate_summary()

        incProgress(0.6, detail = "Generating top 10...")
        top10 <- env$generate_top_ten()

        incProgress(0.8, detail = "Building document preview...")

        briefing_label <- if (exists("briefing_release_label", envir = env)) {
          get("briefing_release_label", envir = env)
        } else {
          format(Sys.Date(), "%B %Y")
        }

        lfs_label <- if (exists("lfs_period_label", envir = env)) {
          get("lfs_period_label", envir = env)
        } else {
          ""
        }

        # Helper functions for dashboard
        gv <- function(name) {
          if (exists(name, envir = env)) get(name, envir = env) else NA_real_
        }

        fmt_num <- function(x, digits = 0) {
          if (is.na(x)) return("-")
          format(round(x, digits), big.mark = ",", nsmall = digits)
        }

        fmt_pct <- function(x) {
          if (is.na(x)) return("-")
          paste0(format(round(x, 1), nsmall = 1), "%")
        }

        fmt_chg <- function(x, divisor = 1, digits = 0, suffix = "") {
          if (is.na(x)) return("-")
          val <- x / divisor
          sign <- if (val > 0) "+" else ""
          paste0(sign, format(round(val, digits), big.mark = ",", nsmall = digits), suffix)
        }

        color_class <- function(x, invert = FALSE) {
          if (is.na(x) || x == 0) return("")
          positive <- x > 0
          if (invert) positive <- !positive
          if (positive) "doc-positive" else "doc-negative"
        }

        incProgress(0.9, detail = "Rendering preview...")

        output$preview_output <- renderUI({
          div(class = "doc-preview",

            # Header - Labour Market Overview: Month YYYY
            div(class = "doc-header",
              h2(class = "doc-header__title",
                "Labour Market Overview: ",
                tags$a(href = "#", briefing_label)
              ),
              p(class = "doc-header__subtitle",
                paste(format(Sys.Date(), "%d %B %Y"), "- Employment Rights Directorate")
              )
            ),

            # Dark blue takeaways box
            div(class = "doc-takeaways",
              h3(class = "doc-takeaways__title", "The key takeaways are:"),
              tags$ul(class = "doc-takeaways__list",
                lapply(1:6, function(i) {
                  line <- summary[[paste0("line", i)]]
                  if (!is.null(line) && nchar(line) > 0) {
                    tags$li(line)
                  }
                })
              )
            ),

            # Top Ten Stats section
            div(class = "doc-section",
              h3(class = "doc-section__title", "Top Ten Stats"),
              tags$ol(class = "doc-topten",
                lapply(1:10, function(i) {
                  line <- top10[[paste0("line", i)]]
                  if (!is.null(line) && nchar(line) > 0) {
                    tags$li(line)
                  }
                })
              )
            ),

            # Dashboard Table
            div(class = "doc-section",
              h3(class = "doc-section__title", paste("Dashboard -", lfs_label)),
              tags$table(class = "doc-table",
                tags$thead(
                  tags$tr(
                    tags$th(""),
                    tags$th("Current"),
                    tags$th("QoQ"),
                    tags$th("YoY"),
                    tags$th("vs COVID"),
                    tags$th("vs Election")
                  )
                ),
                tags$tbody(
                  tags$tr(
                    tags$td("Employment 16+ (000s)"),
                    tags$td(fmt_num(gv("emp16_cur") / 1000)),
                    tags$td(class = color_class(gv("emp16_dq")), fmt_chg(gv("emp16_dq"), 1000)),
                    tags$td(class = color_class(gv("emp16_dy")), fmt_chg(gv("emp16_dy"), 1000)),
                    tags$td(class = color_class(gv("emp16_dc")), fmt_chg(gv("emp16_dc"), 1000)),
                    tags$td(class = color_class(gv("emp16_de")), fmt_chg(gv("emp16_de"), 1000))
                  ),
                  tags$tr(
                    tags$td("Employment rate (16-64)"),
                    tags$td(fmt_pct(gv("emp_rt_cur"))),
                    tags$td(class = color_class(gv("emp_rt_dq")), fmt_chg(gv("emp_rt_dq"), 1, 1, "pp")),
                    tags$td(class = color_class(gv("emp_rt_dy")), fmt_chg(gv("emp_rt_dy"), 1, 1, "pp")),
                    tags$td(class = color_class(gv("emp_rt_dc")), fmt_chg(gv("emp_rt_dc"), 1, 1, "pp")),
                    tags$td(class = color_class(gv("emp_rt_de")), fmt_chg(gv("emp_rt_de"), 1, 1, "pp"))
                  ),
                  tags$tr(
                    tags$td("Unemployment 16+ (000s)"),
                    tags$td(fmt_num(gv("unemp16_cur") / 1000)),
                    tags$td(class = color_class(gv("unemp16_dq"), TRUE), fmt_chg(gv("unemp16_dq"), 1000)),
                    tags$td(class = color_class(gv("unemp16_dy"), TRUE), fmt_chg(gv("unemp16_dy"), 1000)),
                    tags$td(class = color_class(gv("unemp16_dc"), TRUE), fmt_chg(gv("unemp16_dc"), 1000)),
                    tags$td(class = color_class(gv("unemp16_de"), TRUE), fmt_chg(gv("unemp16_de"), 1000))
                  ),
                  tags$tr(
                    tags$td("Unemployment rate (16+)"),
                    tags$td(fmt_pct(gv("unemp_rt_cur"))),
                    tags$td(class = color_class(gv("unemp_rt_dq"), TRUE), fmt_chg(gv("unemp_rt_dq"), 1, 1, "pp")),
                    tags$td(class = color_class(gv("unemp_rt_dy"), TRUE), fmt_chg(gv("unemp_rt_dy"), 1, 1, "pp")),
                    tags$td(class = color_class(gv("unemp_rt_dc"), TRUE), fmt_chg(gv("unemp_rt_dc"), 1, 1, "pp")),
                    tags$td(class = color_class(gv("unemp_rt_de"), TRUE), fmt_chg(gv("unemp_rt_de"), 1, 1, "pp"))
                  ),
                  tags$tr(
                    tags$td("Inactivity rate (16-64)"),
                    tags$td(fmt_pct(gv("inact_rt_cur"))),
                    tags$td(class = color_class(gv("inact_rt_dq"), TRUE), fmt_chg(gv("inact_rt_dq"), 1, 1, "pp")),
                    tags$td(class = color_class(gv("inact_rt_dy"), TRUE), fmt_chg(gv("inact_rt_dy"), 1, 1, "pp")),
                    tags$td(class = color_class(gv("inact_rt_dc"), TRUE), fmt_chg(gv("inact_rt_dc"), 1, 1, "pp")),
                    tags$td(class = color_class(gv("inact_rt_de"), TRUE), fmt_chg(gv("inact_rt_de"), 1, 1, "pp"))
                  ),
                  tags$tr(
                    tags$td("Vacancies (000s)"),
                    tags$td(fmt_num(gv("vac_cur"))),
                    tags$td(fmt_chg(gv("vac_dq"))),
                    tags$td(fmt_chg(gv("vac_dy"))),
                    tags$td(fmt_chg(gv("vac_dc"))),
                    tags$td(fmt_chg(gv("vac_de")))
                  ),
                  tags$tr(
                    tags$td("Payroll employees (000s)"),
                    tags$td(fmt_num(gv("payroll_cur"))),
                    tags$td(class = color_class(gv("payroll_dq")), fmt_chg(gv("payroll_dq"))),
                    tags$td(class = color_class(gv("payroll_dy")), fmt_chg(gv("payroll_dy"))),
                    tags$td(class = color_class(gv("payroll_dc")), fmt_chg(gv("payroll_dc"))),
                    tags$td(class = color_class(gv("payroll_de")), fmt_chg(gv("payroll_de")))
                  ),
                  tags$tr(
                    tags$td("Annual average wages (total pay)"),
                    tags$td(fmt_pct(gv("latest_wages"))),
                    tags$td(fmt_chg(gv("wages_change_q"), 1, 0, "pp")),
                    tags$td(fmt_chg(gv("wages_change_y"), 1, 0, "pp")),
                    tags$td("-"),
                    tags$td("-")
                  ),
                  tags$tr(
                    tags$td("Annual average wages CPI adjusted"),
                    tags$td(fmt_pct(gv("latest_wages_cpi"))),
                    tags$td(fmt_chg(gv("wages_cpi_change_q"), 1, 0, "pp")),
                    tags$td(fmt_chg(gv("wages_cpi_change_y"), 1, 0, "pp")),
                    tags$td("-"),
                    tags$td("-")
                  )
                )
              )
            ),

            # Footer
            div(class = "doc-footer",
              paste("Generated:", format(Sys.Date(), "%d %B %Y"), "| Source: ONS Labour Market Statistics")
            )
          )
        })

        incProgress(1.0, detail = "Complete!")
        status(list(type = "success", message = "Document preview generated successfully"))
      }, error = function(e) {
        status(list(type = "error", message = paste("Error:", e$message)))
        output$preview_output <- renderUI(NULL)
      })
    })
  })

  # Download Word Document
  output$download_word <- downloadHandler(
    filename = function() {
      label <- get_briefing_label()
      paste0("Labour Market Stats Briefing - ", label, ".docx")
    },
    content = function(file) {
      withProgress(message = "Generating Word Document", value = 0, {
        tryCatch({
          incProgress(0.1, detail = "Loading word_output.R...")
          source(word_script_path)

          incProgress(0.3, detail = "Running calculations...")

          incProgress(0.5, detail = "Building document...")
          generate_word_output(
            template_path = template_path,
            output_path = file,
            calculations_path = calculations_path,
            config_path = config_path,
            summary_path = summary_path,
            top_ten_path = top_ten_path,
            verbose = TRUE
          )

          incProgress(1.0, detail = "Complete!")
          status(list(type = "success", message = "Word document generated successfully!"))
        }, error = function(e) {
          status(list(type = "error", message = paste("Error:", e$message)))
          stop(e)
        })
      })
    }
  )

  # Download Excel Workbook
  output$download_excel <- downloadHandler(
    filename = function() {
      label <- get_briefing_label()
      parsed <- tryCatch({
        d <- as.Date(paste0("01 ", label), format = "%d %B %Y")
        format(d, "%B %y")
      }, error = function(e) {
        format(Sys.Date(), "%B %y")
      })
      paste0(parsed, " LM Stats.xlsx")
    },
    content = function(file) {
      withProgress(message = "Generating Excel Workbook", value = 0, {
        tryCatch({
          incProgress(0.1, detail = "Loading excel_audit.R...")
          source(excel_script_path)

          incProgress(0.3, detail = "Running calculations...")

          incProgress(0.5, detail = "Building Dashboard sheet...")

          incProgress(0.7, detail = "Building data sheets...")

          create_audit_workbook(
            output_path = file,
            calculations_path = calculations_path,
            config_path = config_path,
            verbose = TRUE
          )

          incProgress(1.0, detail = "Complete!")
          status(list(type = "success", message = "Excel workbook generated successfully!"))
        }, error = function(e) {
          status(list(type = "error", message = paste("Error:", e$message)))
          stop(e)
        })
      })
    }
  )
}

# ==============================================================================
# RUN APP
# ==============================================================================

shinyApp(ui = ui, server = server)
