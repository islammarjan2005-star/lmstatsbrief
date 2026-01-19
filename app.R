# ==============================================================================
# Labour Market Briefing Dashboard - Shiny App
# ==============================================================================
# Clean, redesigned UI with automatic date detection from database
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

  # Custom CSS for cleaner look
  tags$head(
    tags$style(HTML("
      .main-header {
        text-align: center;
        padding: 40px 20px 20px 20px;
        background: linear-gradient(135deg, #0c275c 0%, #1a4a8c 100%);
        color: white;
        margin: -15px -15px 30px -15px;
        border-radius: 0 0 10px 10px;
      }
      .main-header h1 {
        font-size: 2.5em;
        font-weight: bold;
        margin-bottom: 10px;
      }
      .main-header .release-label {
        font-size: 1.2em;
        opacity: 0.9;
        font-style: italic;
      }
      .btn-block-custom {
        width: 100%;
        padding: 20px;
        font-size: 1.1em;
        margin-bottom: 15px;
        border-radius: 8px;
      }
      .btn-preview {
        background-color: #3498db;
        border-color: #2980b9;
        color: white;
      }
      .btn-preview:hover {
        background-color: #2980b9;
        border-color: #1f6aa5;
        color: white;
      }
      .btn-generate {
        background-color: #27ae60;
        border-color: #1e8449;
        color: white;
      }
      .btn-generate:hover {
        background-color: #1e8449;
        border-color: #196f3d;
        color: white;
      }
      .button-section {
        max-width: 500px;
        margin: 0 auto;
        padding: 20px;
      }
      .preview-output {
        margin-top: 30px;
        padding: 20px;
        background-color: #f8f9fa;
        border-radius: 10px;
        border: 1px solid #dee2e6;
      }
      .preview-output h4 {
        color: #0c275c;
        margin-bottom: 15px;
        border-bottom: 2px solid #0c275c;
        padding-bottom: 10px;
      }
      .status-message {
        text-align: center;
        padding: 15px;
        margin-top: 20px;
        border-radius: 8px;
        max-width: 500px;
        margin-left: auto;
        margin-right: auto;
      }
      .status-success {
        background-color: #d4edda;
        color: #155724;
        border: 1px solid #c3e6cb;
      }
      .status-error {
        background-color: #f8d7da;
        color: #721c24;
        border: 1px solid #f5c6cb;
      }
      .status-info {
        background-color: #cce5ff;
        color: #004085;
        border: 1px solid #b8daff;
      }
    "))
  ),

  # Main header
  div(class = "main-header",
    h1("Labour Market Briefing"),
    div(class = "release-label",
      textOutput("release_label", inline = TRUE)
    )
  ),

  # Button section
  div(class = "button-section",
    h4("Preview", style = "text-align: center; color: #2c3e50; margin-bottom: 20px;"),
    actionButton("preview_top10", "Preview Top 10",
                 class = "btn btn-preview btn-block-custom",
                 icon = icon("list-ol")),
    actionButton("preview_dashboard", "Preview Dashboard",
                 class = "btn btn-preview btn-block-custom",
                 icon = icon("chart-bar")),

    hr(style = "margin: 30px 0;"),

    h4("Generate", style = "text-align: center; color: #2c3e50; margin-bottom: 20px;"),
    downloadButton("download_word", "Generate Word Document",
                   class = "btn btn-generate btn-block-custom"),
    downloadButton("download_excel", "Generate Excel Workbook",
                   class = "btn btn-generate btn-block-custom")
  ),

  # Status message
  uiOutput("status_message"),

  # Preview output area
  uiOutput("preview_output")
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
  status <- reactiveVal(list(type = "info", message = "Ready. Click a button to begin."))

  # Get the release label dynamically on startup
  output$release_label <- renderText({
    tryCatch({
      env <- new.env()
      source(calculations_path, local = env)
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
    status(list(type = "info", message = "Generating Top 10 preview..."))

    tryCatch({
      env <- new.env()
      source(calculations_path, local = env)
      source(top_ten_path, local = env)

      top10 <- env$generate_top_ten()

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

      status(list(type = "success", message = "Top 10 preview generated successfully."))
    }, error = function(e) {
      status(list(type = "error", message = paste("Error generating Top 10:", e$message)))
      output$preview_output <- renderUI(NULL)
    })
  })

  # Preview Dashboard
  observeEvent(input$preview_dashboard, {
    status(list(type = "info", message = "Generating Dashboard preview..."))

    tryCatch({
      env <- new.env()
      source(calculations_path, local = env)

      # Helper to get value or NA
      gv <- function(name) {
        if (exists(name, envir = env)) get(name, envir = env) else NA_real_
      }

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
        `Change QoQ` = c(
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
        `Change YoY` = c(
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

      output$preview_output <- renderUI({
        div(class = "preview-output",
          h4(paste("Dashboard Preview -", lfs_label)),
          tableOutput("dashboard_table")
        )
      })

      output$dashboard_table <- renderTable({
        dashboard_data
      }, striped = TRUE, hover = TRUE, bordered = TRUE)

      status(list(type = "success", message = "Dashboard preview generated successfully."))
    }, error = function(e) {
      status(list(type = "error", message = paste("Error generating Dashboard:", e$message)))
      output$preview_output <- renderUI(NULL)
    })
  })

  # Download Word Document
  output$download_word <- downloadHandler(
    filename = function() {
      paste0("Labour_Market_Briefing_", format(Sys.Date(), "%Y%m%d"), ".docx")
    },
    content = function(file) {
      status(list(type = "info", message = "Generating Word document..."))

      tryCatch({
        source(word_script_path)
        generate_word_output(
          template_path = template_path,
          output_path = file,
          calculations_path = calculations_path,
          config_path = config_path,
          summary_path = summary_path,
          top_ten_path = top_ten_path,
          verbose = TRUE
        )
        status(list(type = "success", message = "Word document generated successfully!"))
      }, error = function(e) {
        status(list(type = "error", message = paste("Error generating Word:", e$message)))
        stop(e)
      })
    }
  )

  # Download Excel Workbook
  output$download_excel <- downloadHandler(
    filename = function() {
      paste0("Labour_Market_Stats_", format(Sys.Date(), "%Y%m%d"), ".xlsx")
    },
    content = function(file) {
      status(list(type = "info", message = "Generating Excel workbook..."))

      tryCatch({
        source(excel_script_path)
        create_audit_workbook(
          output_path = file,
          calculations_path = calculations_path,
          config_path = config_path,
          verbose = TRUE
        )
        status(list(type = "success", message = "Excel workbook generated successfully!"))
      }, error = function(e) {
        status(list(type = "error", message = paste("Error generating Excel:", e$message)))
        stop(e)
      })
    }
  )
}

# ==============================================================================
# RUN APP
# ==============================================================================

shinyApp(ui = ui, server = server)
