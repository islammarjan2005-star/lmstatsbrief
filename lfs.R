# LFS MODULE - A01 Sheet 2 (Employment, Unemployment, Inactivity)

# CONFIG


LFS_CODES <- list(
  EMP16 = "MGRZ",
  EMP_RT = "LF24",
  UNEMP16 = "MGSC",
  UNEMP_RT = "MGSX",
  INACT = "LF2M",
  INACT_RT = "LF2S",
  INACT5064 = "LF2A",
  INACT5064_RT = "LF2W"
)


# FETCH


fetch_lfs <- function() {
  conn <- DBI::dbConnect(RPostgres::Postgres())

  tryCatch({
    query <- 'SELECT time_period, dataset_indentifier_code, value
FROM "ons"."labour_market__age_group"'
    result <- DBI::dbGetQuery(conn, query)
    tibble::as_tibble(result)
  },
  error = function(e) {
    warning("Failed to fetch LFS data: ", e$message)
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

parse_lfs_label_to_end_date <- function(label) {
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

find_latest_lfs_period <- function(pg_data, code) {
  lfs_data <- pg_data %>%
    filter(dataset_indentifier_code == code) %>%
    mutate(parsed_date = sapply(time_period, parse_lfs_label_to_end_date)) %>%
    mutate(parsed_date = as.Date(parsed_date, origin = "1970-01-01")) %>%
    filter(!is.na(parsed_date)) %>%
    arrange(desc(parsed_date))

  if (nrow(lfs_data) == 0) return(NULL)

  list(
    end_date = lfs_data$parsed_date[1],
    label = trimws(lfs_data$time_period[1])
  )
}


# COMPUTE


compute_lfs_metric <- function(pg_data,
                               end_cur,
                               code,
                               covid_label = COVID_LFS_LABEL,
                               election_label = ELECTION_LABEL) {

  end_q <- end_cur %m-% months(3)
  end_y <- end_cur %m-% months(12)

  lab_cur <- make_lfs_label(end_cur)
  lab_q <- make_lfs_label(end_q)
  lab_y <- make_lfs_label(end_y)

  cur <- val_by_code(pg_data, code, lab_cur)
  val_q <- val_by_code(pg_data, code, lab_q)
  val_y <- val_by_code(pg_data, code, lab_y)
  val_c <- val_by_code(pg_data, code, covid_label)
  val_e <- val_by_code(pg_data, code, election_label)

  dq <- if (!is.na(cur) && !is.na(val_q)) cur - val_q else NA_real_
  dy <- if (!is.na(cur) && !is.na(val_y)) cur - val_y else NA_real_
  dc <- if (!is.na(cur) && !is.na(val_c)) cur - val_c else NA_real_
  de <- if (!is.na(cur) && !is.na(val_e)) cur - val_e else NA_real_

  list(cur = cur, dq = dq, dy = dy, dc = dc, de = de, end = end_cur)
}


# CALCULATE ALL LFS METRICS


calculate_lfs <- function() {
  pg_data <- fetch_lfs()

  # Find the latest period dynamically from the database
  latest <- find_latest_lfs_period(pg_data, LFS_CODES$EMP16)

  if (is.null(latest)) {
    # Return empty structure if no data
    empty <- list(cur = NA_real_, dq = NA_real_, dy = NA_real_,
                  dc = NA_real_, de = NA_real_, end = NA)
    return(list(
      emp16 = empty, emp_rt = empty, unemp16 = empty, unemp_rt = empty,
      inact = empty, inact_rt = empty, inact5064 = empty, inact5064_rt = empty
    ))
  }

  end_cur <- latest$end_date

  list(
    emp16 = compute_lfs_metric(pg_data, end_cur, LFS_CODES$EMP16),
    emp_rt = compute_lfs_metric(pg_data, end_cur, LFS_CODES$EMP_RT),
    unemp16 = compute_lfs_metric(pg_data, end_cur, LFS_CODES$UNEMP16),
    unemp_rt = compute_lfs_metric(pg_data, end_cur, LFS_CODES$UNEMP_RT),
    inact = compute_lfs_metric(pg_data, end_cur, LFS_CODES$INACT),
    inact_rt = compute_lfs_metric(pg_data, end_cur, LFS_CODES$INACT_RT),
    inact5064 = compute_lfs_metric(pg_data, end_cur, LFS_CODES$INACT5064),
    inact5064_rt = compute_lfs_metric(pg_data, end_cur, LFS_CODES$INACT5064_RT)
  )
}
