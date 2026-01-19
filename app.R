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

      /* Document Preview */
      .govuk-document-preview {
        background: #ffffff;
        border: 1px solid #b1b4b6;
        padding: 40px 50px;
        margin: 30px 0;
      }

      .govuk-document-preview__header {
        border-bottom: 4px solid #0b0c0c;
        padding-bottom: 20px;
        margin-bottom: 30px;
      }

      .govuk-document-preview__title {
        font-size: 32px;
        font-weight: 700;
        margin: 0;
      }

      .govuk-document-preview__subtitle {
        font-size: 19px;
        color: #505a5f;
        margin: 10px 0 0 0;
      }

      .govuk-document-preview__section {
        margin: 35px 0;
      }

      .govuk-document-preview__section-title {
        font-size: 24px;
        font-weight: 700;
        margin: 0 0 15px 0;
        padding-bottom: 10px;
        border-bottom: 2px solid #b1b4b6;
      }

      .govuk-summary-box {
        background: #f3f2f1;
        padding: 20px 25px;
        border-left: 5px solid #1d70b8;
        margin: 20px 0;
      }

      .govuk-summary-box p {
        margin: 10px 0;
        font-size: 18px;
      }

      .govuk-document-preview__footer {
        margin-top: 40px;
        padding-top: 20px;
        border-top: 1px solid #b1b4b6;
        font-size: 14px;
        color: #505a5f;
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

        gv <- function(name) {
          if (exists(name, envir = env)) get(name, envir = env) else NA_real_
        }

        fmt_k <- function(x) {
          if (is.na(x)) return("-")
          format(round(x / 1000), big.mark = ",")
        }
        fmt_pct <- function(x) {
          if (is.na(x)) return("-")
          paste0(format(round(x, 1), nsmall = 1), "%")
        }
        fmt_chg <- function(x, is_pct = FALSE) {
          if (is.na(x)) return("-")
          sign <- if (x > 0) "+" else ""
          if (is_pct) {
            paste0(sign, format(round(x, 1), nsmall = 1), "pp")
          } else {
            paste0(sign, format(round(x / 1000), big.mark = ","))
          }
        }
        color_class <- function(x, invert = FALSE) {
          if (is.na(x) || x == 0) return("")
          positive <- x > 0
          if (invert) positive <- !positive
          if (positive) "govuk-positive" else "govuk-negative"
        }

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

        incProgress(0.9, detail = "Rendering preview...")

        output$preview_output <- renderUI({
          div(class = "govuk-document-preview",

            div(class = "govuk-document-preview__header",
              h2(class = "govuk-document-preview__title", "Labour Market Statistics Briefing"),
              p(class = "govuk-document-preview__subtitle", briefing_label)
            ),

            div(class = "govuk-document-preview__section",
              h3(class = "govuk-document-preview__section-title", "Executive Summary"),
              div(class = "govuk-summary-box",
                lapply(1:6, function(i) {
                  line <- summary[[paste0("line", i)]]
                  if (!is.null(line) && nchar(line) > 0) {
                    p(line)
                  }
                })
              )
            ),

            div(class = "govuk-document-preview__section",
              h3(class = "govuk-document-preview__section-title", "Top 10 Statistics"),
              tags$ol(class = "govuk-list govuk-list--number",
                lapply(1:10, function(i) {
                  line <- top10[[paste0("line", i)]]
                  if (!is.null(line) && nchar(line) > 0) {
                    tags$li(line)
                  }
                })
              )
            ),

            div(class = "govuk-document-preview__section",
              h3(class = "govuk-document-preview__section-title", paste("Key Metrics -", lfs_label)),
              tags$table(class = "govuk-table",
                tags$thead(
                  tags$tr(
                    tags$th(class = "govuk-table__header", "Metric"),
                    tags$th(class = "govuk-table__header govuk-table__header--numeric", "Current"),
                    tags$th(class = "govuk-table__header govuk-table__header--numeric", "QoQ"),
                    tags$th(class = "govuk-table__header govuk-table__header--numeric", "YoY")
                  )
                ),
                tags$tbody(
                  tags$tr(class = "govuk-table__row",
                    tags$td(class = "govuk-table__cell", "Employment (000s)"),
                    tags$td(class = "govuk-table__cell govuk-table__cell--numeric", fmt_k(gv("emp16_cur"))),
                    tags$td(class = paste("govuk-table__cell govuk-table__cell--numeric", color_class(gv("emp16_dq"))), fmt_chg(gv("emp16_dq"))),
                    tags$td(class = paste("govuk-table__cell govuk-table__cell--numeric", color_class(gv("emp16_dy"))), fmt_chg(gv("emp16_dy")))
                  ),
                  tags$tr(class = "govuk-table__row",
                    tags$td(class = "govuk-table__cell", "Employment Rate"),
                    tags$td(class = "govuk-table__cell govuk-table__cell--numeric", fmt_pct(gv("emp_rt_cur"))),
                    tags$td(class = paste("govuk-table__cell govuk-table__cell--numeric", color_class(gv("emp_rt_dq"))), fmt_chg(gv("emp_rt_dq"), TRUE)),
                    tags$td(class = paste("govuk-table__cell govuk-table__cell--numeric", color_class(gv("emp_rt_dy"))), fmt_chg(gv("emp_rt_dy"), TRUE))
                  ),
                  tags$tr(class = "govuk-table__row",
                    tags$td(class = "govuk-table__cell", "Unemployment (000s)"),
                    tags$td(class = "govuk-table__cell govuk-table__cell--numeric", fmt_k(gv("unemp16_cur"))),
                    tags$td(class = paste("govuk-table__cell govuk-table__cell--numeric", color_class(gv("unemp16_dq"), TRUE)), fmt_chg(gv("unemp16_dq"))),
                    tags$td(class = paste("govuk-table__cell govuk-table__cell--numeric", color_class(gv("unemp16_dy"), TRUE)), fmt_chg(gv("unemp16_dy")))
                  ),
                  tags$tr(class = "govuk-table__row",
                    tags$td(class = "govuk-table__cell", "Unemployment Rate"),
                    tags$td(class = "govuk-table__cell govuk-table__cell--numeric", fmt_pct(gv("unemp_rt_cur"))),
                    tags$td(class = paste("govuk-table__cell govuk-table__cell--numeric", color_class(gv("unemp_rt_dq"), TRUE)), fmt_chg(gv("unemp_rt_dq"), TRUE)),
                    tags$td(class = paste("govuk-table__cell govuk-table__cell--numeric", color_class(gv("unemp_rt_dy"), TRUE)), fmt_chg(gv("unemp_rt_dy"), TRUE))
                  ),
                  tags$tr(class = "govuk-table__row",
                    tags$td(class = "govuk-table__cell", "Inactivity (000s)"),
                    tags$td(class = "govuk-table__cell govuk-table__cell--numeric", fmt_k(gv("inact_cur"))),
                    tags$td(class = paste("govuk-table__cell govuk-table__cell--numeric", color_class(gv("inact_dq"), TRUE)), fmt_chg(gv("inact_dq"))),
                    tags$td(class = paste("govuk-table__cell govuk-table__cell--numeric", color_class(gv("inact_dy"), TRUE)), fmt_chg(gv("inact_dy")))
                  ),
                  tags$tr(class = "govuk-table__row",
                    tags$td(class = "govuk-table__cell", "Inactivity Rate"),
                    tags$td(class = "govuk-table__cell govuk-table__cell--numeric", fmt_pct(gv("inact_rt_cur"))),
                    tags$td(class = paste("govuk-table__cell govuk-table__cell--numeric", color_class(gv("inact_rt_dq"), TRUE)), fmt_chg(gv("inact_rt_dq"), TRUE)),
                    tags$td(class = paste("govuk-table__cell govuk-table__cell--numeric", color_class(gv("inact_rt_dy"), TRUE)), fmt_chg(gv("inact_rt_dy"), TRUE))
                  ),
                  tags$tr(class = "govuk-table__row",
                    tags$td(class = "govuk-table__cell", "Vacancies (000s)"),
                    tags$td(class = "govuk-table__cell govuk-table__cell--numeric", format(round(gv("vac_cur")), big.mark = ",")),
                    tags$td(class = "govuk-table__cell govuk-table__cell--numeric", format(round(gv("vac_dq")), big.mark = ",")),
                    tags$td(class = "govuk-table__cell govuk-table__cell--numeric", format(round(gv("vac_dy")), big.mark = ","))
                  ),
                  tags$tr(class = "govuk-table__row",
                    tags$td(class = "govuk-table__cell", "Payroll (000s)"),
                    tags$td(class = "govuk-table__cell govuk-table__cell--numeric", format(round(gv("payroll_cur")), big.mark = ",")),
                    tags$td(class = paste("govuk-table__cell govuk-table__cell--numeric", color_class(gv("payroll_dq"))), paste0(if(gv("payroll_dq") > 0) "+" else "", round(gv("payroll_dq")))),
                    tags$td(class = paste("govuk-table__cell govuk-table__cell--numeric", color_class(gv("payroll_dy"))), paste0(if(gv("payroll_dy") > 0) "+" else "", round(gv("payroll_dy"))))
                  )
                )
              )
            ),

            div(class = "govuk-document-preview__footer",
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
