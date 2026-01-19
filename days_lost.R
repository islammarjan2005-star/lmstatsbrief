
# DAYS LOST MODULE - A01 Sheet 18 (Working Days Lost)



# CONFIG


DAYS_LOST_CODE <- "BBFW"


# FETCH


fetch_days_lost <- function() {
  conn <- DBI::dbConnect(RPostgres::Postgres())

  tryCatch({
    query <- 'SELECT time_period, dataset_indentifier_code, value
FROM "ons"."labour_market__disputes"'
    result <- DBI::dbGetQuery(conn, query)
    tibble::as_tibble(result)
  },
  error = function(e) {
    warning("Failed to fetch days lost: ", e$message)
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


# FIND LATEST PERIOD


find_latest_days_lost_period <- function(pg_data) {
  # Time periods are like "August 2025" or "August 2025 [p]"
  days_data <- pg_data %>%
    filter(dataset_indentifier_code == DAYS_LOST_CODE) %>%
    mutate(
      # Extract just the month year part
      clean_period = gsub("\\s*\\[.*\\]\\s*$", "", time_period),
      parsed_date = as.Date(paste0("01 ", clean_period), format = "%d %B %Y")
    ) %>%
    filter(!is.na(parsed_date)) %>%
    arrange(desc(parsed_date))

  if (nrow(days_data) == 0) return(NULL)

  list(
    anchor = days_data$parsed_date[1],
    label = days_data$time_period[1]
  )
}


# COMPUTE


compute_days_lost <- function(pg_data) {
  # Find latest period dynamically
  latest <- find_latest_days_lost_period(pg_data)

  if (is.null(latest)) {
    return(list(cur = NA_real_, label = "", anchor = NA))
  }

  anchor <- latest$anchor
  lab_cur <- make_payroll_label(anchor)

  # use startsWith to handle [p] or [r] suffixes
  match_row <- pg_data %>%
    filter(
      dataset_indentifier_code == DAYS_LOST_CODE,
      startsWith(time_period, lab_cur)
    )

  if (nrow(match_row) == 0) {
    cur <- NA_real_
  } else {
    cur <- suppressWarnings(as.numeric(match_row$value[1]))
  }

  list(
    cur = cur,
    label = lab_cur,
    anchor = anchor
  )
}


# CALCULATE DAYS LOST


calculate_days_lost <- function() {
  pg_data <- fetch_days_lost()
  compute_days_lost(pg_data)
}
