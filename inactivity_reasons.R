# ==============================================================================
# INACTIVITY REASONS MODULE - A01 Sheet 11
# ==============================================================================
# Table: ons.PLACEHOLDER_INACTIVITY_REASON_TABLE
# Time period format: "Jul-Sep 2025"
# ==============================================================================

# ------------------------------------------------------------------------------
# CONFIG
# ------------------------------------------------------------------------------

INACT_REASON_CODES <- list(
  STUDENT        = "LF63",
  FAMILY_HOME    = "LF65",
  TEMP_SICK      = "LF67",
  LONG_TERM_SICK = "LF69",
  DISCOURAGED    = "LFL8",
  RETIRED        = "LF6B",
  OTHER          = "LF6D"
)

# Friendly names for narrative
INACT_REASON_NAMES <- list(
  LF63 = "students",
  LF65 = "those looking after family or home",
  LF67 = "temporary sickness",
  LF69 = "long-term sickness",
  LFL8 = "discouraged workers",
  LF6B = "retirees",
  LF6D = "other reasons"
)

# ------------------------------------------------------------------------------
# FETCH
# ------------------------------------------------------------------------------

fetch_inactivity_reasons <- function() {
  conn <- DBI::dbConnect(RPostgres::Postgres())

  tryCatch({
    query <- 'SELECT time_period, dataset_indentifier_code, value
              FROM \"ons\".\"labour_market__inactivity\"'
    result <- DBI::dbGetQuery(conn, query)
    tibble::as_tibble(result)
  },
  error = function(e) {
    warning("Failed to fetch inactivity reasons: ", e$message)
    tibble::tibble(
      time_period = character(),
      dataset_indentifier_code = character(),
      value = numeric()
    )
  },
  finally = {
    DBI::dbDisconnect(conn)
  })
}

# ------------------------------------------------------------------------------
# HELPER - Parse LFS label to get end date
# ------------------------------------------------------------------------------

parse_lfs_label_to_end_date_inact <- function(label) {
  # Parse "Jul-Sep 2025" -> 2025-09-01 (end month)
  month_map <- c(jan=1,feb=2,mar=3,apr=4,may=5,jun=6,jul=7,aug=8,sep=9,oct=10,nov=11,dec=12)

  matches <- regmatches(label, gregexpr("[A-Za-z]{3}", label))[[1]]
  year <- regmatches(label, gregexpr("[0-9]{4}", label))[[1]]

  if (length(matches) >= 2 && length(year) >= 1) {
    end_month <- month_map[tolower(matches[2])]
    yr <- as.integer(year[1])
    if (!is.na(end_month) && !is.na(yr)) {
      return(as.Date(sprintf("%04d-%02d-01", yr, end_month)))
    }
  }
  NA
}

# ------------------------------------------------------------------------------
# FIND LATEST PERIOD
# ------------------------------------------------------------------------------

find_latest_inactivity_period <- function(pg_data) {
  # Use student code as reference
  inact_data <- pg_data %>%
    filter(dataset_indentifier_code == INACT_REASON_CODES$STUDENT) %>%
    mutate(parsed_date = sapply(time_period, parse_lfs_label_to_end_date_inact)) %>%
    mutate(parsed_date = as.Date(parsed_date, origin = "1970-01-01")) %>%
    filter(!is.na(parsed_date)) %>%
    arrange(desc(parsed_date))

  if (nrow(inact_data) == 0) return(NULL)

  inact_data$parsed_date[1]
}

# ------------------------------------------------------------------------------
# COMPUTE
# ------------------------------------------------------------------------------

compute_inactivity_reasons <- function(pg_data) {
  # Find latest period dynamically
  end_cur <- find_latest_inactivity_period(pg_data)

  if (is.null(end_cur)) {
    reasons <- c("LF63", "LF65", "LF67", "LF69", "LFL8", "LF6B", "LF6D")
    results <- list()
    for (code in reasons) {
      results[[code]] <- list(cur = NA_real_, dy = NA_real_, dc = NA_real_)
    }
    results$labels <- list(cur = "", y = "", covid = "Dec-Feb 2020")
    results$end <- NA
    return(results)
  }

  end_y <- end_cur %m-% months(12)

  lab_cur <- make_lfs_label(end_cur)
  lab_y <- make_lfs_label(end_y)
  lab_covid <- "Dec-Feb 2020"

  reasons <- c("LF63", "LF65", "LF67", "LF69", "LFL8", "LF6B", "LF6D")

  results <- list()

  for (code in reasons) {
    cur <- val_by_code(pg_data, code, lab_cur)
    val_y <- val_by_code(pg_data, code, lab_y)
    val_covid <- val_by_code(pg_data, code, lab_covid)

    dy <- if (!is.na(cur) && !is.na(val_y)) cur - val_y else NA_real_
    dc <- if (!is.na(cur) && !is.na(val_covid)) cur - val_covid else NA_real_

    results[[code]] <- list(
      cur = cur,
      dy = dy,
      dc = dc
    )
  }

  results$labels <- list(
    cur = lab_cur,
    y = lab_y,
    covid = lab_covid
  )

  results$end <- end_cur

  results
}

# ------------------------------------------------------------------------------
# FIND TOP DRIVERS
# ------------------------------------------------------------------------------

find_top_n_drivers <- function(inact_data, comparison = "dc", n = 2) {
  reasons <- c("LF63", "LF65", "LF67", "LF69", "LFL8", "LF6B", "LF6D")

  changes <- sapply(reasons, function(code) {
    inact_data[[code]][[comparison]]
  })

  df <- data.frame(
    code = reasons,
    name = sapply(reasons, function(x) INACT_REASON_NAMES[[x]]),
    change = changes,
    stringsAsFactors = FALSE
  )

  df <- df[order(-abs(df$change)), ]
  head(df, n)
}

generate_inactivity_driver_text <- function(inact_data) {
  top_drivers <- find_top_n_drivers(inact_data, "dc", 2)

  if (nrow(top_drivers) == 0 || all(is.na(top_drivers$change))) {
    return("changes across various reasons")
  }

  increases <- top_drivers[top_drivers$change > 0, ]

  if (nrow(increases) == 0) {
    return("decreases across various reasons")
  } else if (nrow(increases) == 1) {
    return(paste0("increases in ", increases$name[1]))
  } else {
    return(paste0("increases in ", increases$name[1], " and ", increases$name[2]))
  }
}

# ------------------------------------------------------------------------------
# CALCULATE INACTIVITY REASONS
# ------------------------------------------------------------------------------

calculate_inactivity_reasons <- function() {
  pg_data <- fetch_inactivity_reasons()
  compute_inactivity_reasons(pg_data)
}
