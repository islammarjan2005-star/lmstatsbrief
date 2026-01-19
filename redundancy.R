
# REDUNDANCY MODULE - A01 Sheet 10

#CONFIG


REDUND_CODE <- "BEIR"


# FETCH


fetch_redundancy <- function() {
  conn <- DBI::dbConnect(RPostgres::Postgres())

  tryCatch({
    query <- 'SELECT time_period, dataset_indentifier_code, value
FROM "ons"."labour_market__redundancies"'
    result <- DBI::dbGetQuery(conn, query)
    tibble::as_tibble(result)
  },
  error = function(e) {
    warning("Failed to fetch redundancy: ", e$message)
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


# HELPER - Parse LFS label to get end date

parse_lfs_label_to_end_date_redund <- function(label) {
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


# FIND LATEST PERIOD

find_latest_redundancy_period <- function(pg_data) {
  redund_data <- pg_data %>%
    filter(dataset_indentifier_code == REDUND_CODE) %>%
    mutate(parsed_date = sapply(time_period, parse_lfs_label_to_end_date_redund)) %>%
    mutate(parsed_date = as.Date(parsed_date, origin = "1970-01-01")) %>%
    filter(!is.na(parsed_date)) %>%
    arrange(desc(parsed_date))

  if (nrow(redund_data) == 0) return(NULL)

  redund_data$parsed_date[1]
}


# COMPUTE


compute_redundancy <- function(pg_data,
                               covid_label = COVID_LFS_LABEL,
                               election_label = ELECTION_LABEL) {
  # Find latest period dynamically
  end_cur <- find_latest_redundancy_period(pg_data)

  if (is.null(end_cur)) {
    return(list(cur = NA_real_, dq = NA_real_, dy = NA_real_,
                dc = NA_real_, de = NA_real_, end = NA))
  }

  end_q <- end_cur %m-% months(3)
  end_y <- end_cur %m-% months(12)

  lab_cur <- make_lfs_label(end_cur)
  lab_q <- make_lfs_label(end_q)
  lab_y <- make_lfs_label(end_y)

  cur <- val_by_code(pg_data, REDUND_CODE, lab_cur)
  val_q <- val_by_code(pg_data, REDUND_CODE, lab_q)
  val_y <- val_by_code(pg_data, REDUND_CODE, lab_y)
  val_c <- val_by_code(pg_data, REDUND_CODE, covid_label)
  val_e <- val_by_code(pg_data, REDUND_CODE, election_label)

  dq <- if (!is.na(cur) && !is.na(val_q)) cur - val_q else NA_real_
  dy <- if (!is.na(cur) && !is.na(val_y)) cur - val_y else NA_real_
  dc <- if (!is.na(cur) && !is.na(val_c)) cur - val_c else NA_real_
  de <- if (!is.na(cur) && !is.na(val_e)) cur - val_e else NA_real_

  list(cur = cur, dq = dq, dy = dy, dc = dc, de = de, end = end_cur)
}


# CALCULATE REDUNDANCY


calculate_redundancy <- function() {
  pg_data <- fetch_redundancy()
  compute_redundancy(pg_data)
}
