# ==============================================================================
# Labour Market Briefing Dashboard - Shiny App
# ==============================================================================
# Premium UI with automatic date detection from database
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

  # Premium CSS styling
 tags$head(
    tags$style(HTML("
      @import url('https://fonts.googleapis.com/css2?family=Inter:wght@300;400;500;600;700&display=swap');

      body {
        font-family: 'Inter', -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, sans-serif;
        background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
        min-height: 100vh;
        margin: 0;
        padding: 20px;
      }

      .container-fluid {
        max-width: 800px;
        margin: 0 auto;
      }

      .main-card {
        background: rgba(255, 255, 255, 0.95);
        backdrop-filter: blur(20px);
        border-radius: 24px;
        box-shadow: 0 25px 50px -12px rgba(0, 0, 0, 0.25);
        padding: 0;
        overflow: hidden;
      }

      .main-header {
        background: linear-gradient(135deg, #1e3a5f 0%, #2d5a87 50%, #1e3a5f 100%);
        padding: 50px 40px;
        text-align: center;
        position: relative;
        overflow: hidden;
      }

      .main-header::before {
        content: '';
        position: absolute;
        top: -50%;
        left: -50%;
        width: 200%;
        height: 200%;
        background: radial-gradient(circle, rgba(255,255,255,0.1) 0%, transparent 60%);
        animation: shimmer 15s infinite linear;
      }

      @keyframes shimmer {
        0% { transform: rotate(0deg); }
        100% { transform: rotate(360deg); }
      }

      .main-header h1 {
        font-size: 2.8em;
        font-weight: 700;
        color: white;
        margin: 0 0 12px 0;
        letter-spacing: -0.5px;
        position: relative;
        text-shadow: 0 2px 4px rgba(0,0,0,0.2);
      }

      .main-header .release-label {
        font-size: 1.1em;
        color: rgba(255, 255, 255, 0.9);
        font-weight: 400;
        position: relative;
        background: rgba(255,255,255,0.15);
        padding: 8px 20px;
        border-radius: 20px;
        display: inline-block;
      }

      .content-section {
        padding: 40px;
      }

      .section-title {
        font-size: 0.75em;
        font-weight: 600;
        text-transform: uppercase;
        letter-spacing: 2px;
        color: #64748b;
        margin-bottom: 20px;
        text-align: center;
      }

      .btn-grid {
        display: grid;
        grid-template-columns: 1fr 1fr;
        gap: 16px;
        margin-bottom: 32px;
      }

      .btn-custom {
        padding: 20px 24px;
        font-size: 0.95em;
        font-weight: 600;
        border-radius: 16px;
        border: none;
        cursor: pointer;
        transition: all 0.3s cubic-bezier(0.4, 0, 0.2, 1);
        display: flex;
        align-items: center;
        justify-content: center;
        gap: 10px;
        text-decoration: none;
        width: 100%;
      }

      .btn-preview {
        background: linear-gradient(135deg, #f8fafc 0%, #e2e8f0 100%);
        color: #334155;
        border: 2px solid #e2e8f0;
      }

      .btn-preview:hover {
        background: linear-gradient(135deg, #e2e8f0 0%, #cbd5e1 100%);
        transform: translateY(-2px);
        box-shadow: 0 10px 20px -5px rgba(0, 0, 0, 0.1);
        color: #1e293b;
      }

      .btn-generate {
        background: linear-gradient(135deg, #1e3a5f 0%, #2d5a87 100%);
        color: white;
        border: none;
      }

      .btn-generate:hover {
        background: linear-gradient(135deg, #2d5a87 0%, #3b7cb8 100%);
        transform: translateY(-2px);
        box-shadow: 0 10px 25px -5px rgba(30, 58, 95, 0.4);
        color: white;
      }

      .btn-custom:active {
        transform: translateY(0);
      }

      .divider {
        height: 1px;
        background: linear-gradient(90deg, transparent, #e2e8f0, transparent);
        margin: 8px 0 32px 0;
      }

      .status-message {
        text-align: center;
        padding: 16px 24px;
        margin: 24px 40px;
        border-radius: 12px;
        font-weight: 500;
        font-size: 0.9em;
      }

      .status-success {
        background: linear-gradient(135deg, #ecfdf5 0%, #d1fae5 100%);
        color: #065f46;
        border: 1px solid #a7f3d0;
      }

      .status-error {
        background: linear-gradient(135deg, #fef2f2 0%, #fee2e2 100%);
        color: #991b1b;
        border: 1px solid #fecaca;
      }

      .status-info {
        background: linear-gradient(135deg, #eff6ff 0%, #dbeafe 100%);
        color: #1e40af;
        border: 1px solid #bfdbfe;
      }

      .preview-output {
        margin: 24px 40px 40px 40px;
        padding: 28px;
        background: linear-gradient(135deg, #f8fafc 0%, #f1f5f9 100%);
        border-radius: 16px;
        border: 1px solid #e2e8f0;
      }

      .preview-output h4 {
        color: #1e3a5f;
        margin: 0 0 20px 0;
        padding-bottom: 12px;
        border-bottom: 2px solid #1e3a5f;
        font-weight: 600;
        font-size: 1.1em;
      }

      .preview-output ol {
        margin: 0;
        padding-left: 24px;
      }

      .preview-output li {
        padding: 8px 0;
        color: #334155;
        line-height: 1.6;
      }

      .preview-output table {
        width: 100%;
        border-collapse: separate;
        border-spacing: 0;
        font-size: 0.85em;
      }

      .preview-output th {
        background: #1e3a5f;
        color: white;
        padding: 12px 16px;
        text-align: left;
        font-weight: 600;
      }

      .preview-output th:first-child {
        border-radius: 8px 0 0 0;
      }

      .preview-output th:last-child {
        border-radius: 0 8px 0 0;
      }

      .preview-output td {
        padding: 10px 16px;
        border-bottom: 1px solid #e2e8f0;
        color: #334155;
      }

      .preview-output tr:last-child td:first-child {
        border-radius: 0 0 0 8px;
      }

      .preview-output tr:last-child td:last-child {
        border-radius: 0 0 8px 0;
      }

      .preview-output tr:hover td {
        background: rgba(30, 58, 95, 0.05);
      }

      .shiny-notification {
        border-radius: 12px;
        border: none;
        box-shadow: 0 10px 40px -10px rgba(0, 0, 0, 0.2);
      }

      .progress {
        height: 8px;
        border-radius: 4px;
        background: #e2e8f0;
      }

      .progress-bar {
        background: linear-gradient(90deg, #1e3a5f, #2d5a87);
        border-radius: 4px;
      }

      .footer-text {
        text-align: center;
        padding: 20px 40px 30px 40px;
        color: #94a3b8;
        font-size: 0.8em;
      }
    "))
  ),

  # Main card container
  div(class = "main-card",

    # Header
    div(class = "main-header",
      h1("Labour Market Briefing"),
      div(class = "release-label",
        textOutput("release_label", inline = TRUE)
      )
    ),

    # Content
    div(class = "content-section",

      # Preview section
      div(class = "section-title", "Preview"),
      div(class = "btn-grid",
        actionButton("preview_top10",
                     tagList(icon("list-ol"), "Top 10 Statistics"),
                     class = "btn-custom btn-preview"),
        actionButton("preview_dashboard",
                     tagList(icon("table-columns"), "Dashboard Metrics"),
                     class = "btn-custom btn-preview")
      ),

      div(class = "divider"),

      # Generate section
      div(class = "section-title", "Generate Reports"),
      div(class = "btn-grid",
        downloadButton("download_word",
                       tagList(icon("file-word"), "Word Document"),
                       class = "btn-custom btn-generate"),
        downloadButton("download_excel",
                       tagList(icon("file-excel"), "Excel Workbook"),
                       class = "btn-custom btn-generate")
      )
    ),

    # Status message
    uiOutput("status_message"),

    # Preview output area
    uiOutput("preview_output"),

    # Footer
    div(class = "footer-text",
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
        paste("Latest briefing released", get("briefing_release_label", envir = env))
      } else {
        paste("Latest briefing released", format(Sys.Date(), "%B %Y"))
      }
    }, error = function(e) {
      paste("Latest briefing released", format(Sys.Date(), "%B %Y"))
    })
  })

  # Status message output
  output$status_message <- renderUI({
    s <- status()
    if (!is.null(s$message)) {
      div(class = paste("status-message", paste0("status-", s$type)),
          s$message)
    }
  })

  # Preview Top 10
  observeEvent(input$preview_top10, {
    withProgress(message = "Loading Top 10 Statistics", value = 0, {
      tryCatch({
        incProgress(0.2, detail = "Loading calculations...")
        env <- new.env()
        source(calculations_path, local = env)
        cached_env(env)

        incProgress(0.5, detail = "Loading top ten generator...")
        source(top_ten_path, local = env)

        incProgress(0.7, detail = "Generating statistics...")
        top10 <- env$generate_top_ten()

        incProgress(0.9, detail = "Rendering preview...")
        output$preview_output <- renderUI({
          div(class = "preview-output",
            h4("Top 10 Statistics"),
            tags$ol(
              lapply(1:10, function(i) {
                line <- top10[[paste0("line", i)]]
                if (!is.null(line) && nchar(line) > 0) {
                  tags$li(line)
                }
              })
            )
          )
        })

        incProgress(1.0, detail = "Complete!")
        status(list(type = "success", message = "Top 10 preview generated successfully"))
      }, error = function(e) {
        status(list(type = "error", message = paste("Error:", e$message)))
        output$preview_output <- renderUI(NULL)
      })
    })
  })

  # Preview Dashboard
  observeEvent(input$preview_dashboard, {
    withProgress(message = "Loading Dashboard Metrics", value = 0, {
      tryCatch({
        incProgress(0.2, detail = "Loading calculations...")
        env <- new.env()
        source(calculations_path, local = env)
        cached_env(env)

        incProgress(0.5, detail = "Processing metrics...")

        # Helper to get value or NA
        gv <- function(name) {
          if (exists(name, envir = env)) get(name, envir = env) else NA_real_
        }

        incProgress(0.7, detail = "Building table...")

        # Build dashboard table
        dashboard_data <- data.frame(
          Metric = c(
            "Employment (000s)",
            "Employment Rate (%)",
            "Unemployment (000s)",
            "Unemployment Rate (%)",
            "Inactivity (000s)",
            "Inactivity 50-64 (000s)",
            "Inactivity Rate (%)",
            "Inactivity Rate 50-64 (%)",
            "Vacancies (000s)",
            "Payroll (000s)",
            "Wages Nominal (%)",
            "Wages CPI (%)"
          ),
          Current = c(
            round(gv("emp16_cur") / 1000, 0),
            round(gv("emp_rt_cur"), 1),
            round(gv("unemp16_cur") / 1000, 0),
            round(gv("unemp_rt_cur"), 1),
            round(gv("inact_cur") / 1000, 0),
            round(gv("inact5064_cur") / 1000, 0),
            round(gv("inact_rt_cur"), 1),
            round(gv("inact5064_rt_cur"), 1),
            round(gv("vac_cur"), 0),
            round(gv("payroll_cur"), 0),
            round(gv("latest_wages"), 1),
            round(gv("latest_wages_cpi"), 1)
          ),
          `QoQ` = c(
            round(gv("emp16_dq") / 1000, 0),
            round(gv("emp_rt_dq"), 2),
            round(gv("unemp16_dq") / 1000, 0),
            round(gv("unemp_rt_dq"), 2),
            round(gv("inact_dq") / 1000, 0),
            round(gv("inact5064_dq") / 1000, 0),
            round(gv("inact_rt_dq"), 2),
            round(gv("inact5064_rt_dq"), 2),
            round(gv("vac_dq"), 0),
            round(gv("payroll_dq"), 0),
            round(gv("wages_change_q"), 0),
            round(gv("wages_cpi_change_q"), 0)
          ),
          `YoY` = c(
            round(gv("emp16_dy") / 1000, 0),
            round(gv("emp_rt_dy"), 2),
            round(gv("unemp16_dy") / 1000, 0),
            round(gv("unemp_rt_dy"), 2),
            round(gv("inact_dy") / 1000, 0),
            round(gv("inact5064_dy") / 1000, 0),
            round(gv("inact_rt_dy"), 2),
            round(gv("inact5064_rt_dy"), 2),
            round(gv("vac_dy"), 0),
            round(gv("payroll_dy"), 0),
            round(gv("wages_change_y"), 0),
            round(gv("wages_cpi_change_y"), 0)
          ),
          stringsAsFactors = FALSE,
          check.names = FALSE
        )

        lfs_label <- if (exists("lfs_period_label", envir = env)) {
          get("lfs_period_label", envir = env)
        } else {
          ""
        }

        incProgress(0.9, detail = "Rendering preview...")

        output$preview_output <- renderUI({
          div(class = "preview-output",
            h4(paste("Dashboard Metrics", "-", lfs_label)),
            tableOutput("dashboard_table")
          )
        })

        output$dashboard_table <- renderTable({
          dashboard_data
        }, striped = TRUE, hover = TRUE, bordered = TRUE)

        incProgress(1.0, detail = "Complete!")
        status(list(type = "success", message = "Dashboard preview generated successfully"))
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
      # Parse to get "Month YY" format
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
