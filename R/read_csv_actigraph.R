#' Read accelerometer data from an *.csv file
#'
#' Read ActiGraph accelerometer data from a database stored in an CSV file. Return a tsibble.
#' @param file Full path to a csv file to read.
#' @param tz Time zone for the output data, converted from UTC (the default).
#' @param preamble Keep csv file preamble as an attribute?
#' @return A \code{tsibble} (\code{tbl_ts}) of accelerometer data with at least two columns: timestamp and axis1.
#' @export
read_csv_actigraph <- function(file, tz = "UTC", preamble = FALSE) {
  csv_data_raw <- read_csv_actigraph_raw_(file, tz)

  acc_data <-
    csv_data_raw$data %>%
    tsibble::as_tsibble(index = timestamp)
  preamble <- csv_data_raw$preamble
  preamble_raw <- csv_data_raw$preamble_raw

  check_data_integrity(acc_data)

  if (length(preamble) > 0) {
    check_agddata_epochlength(acc_data, preamble)
    check_agddata_starttime(acc_data, preamble)
  }

  if (is_gapful(acc_data)) {
    epochlength_guess <-
      difftime(acc_data$timestamp[2], acc_data$timestamp[1], units = "secs") %>%
      {round(. / 5) * 5} %>%
      as.integer()
    acc_data <-
      csv_data_raw$data %>%
      mutate(timestamp =
               lubridate::round_date(timestamp,
                                     lubridate::seconds(epochlength_guess))) %>%
      tsibble::as_tsibble(index = timestamp)
    check_data_gaps(acc_data)
  }


  if (preamble) {
    attr(acc_data, "preamble") <- preamble
    attr(acc_data, "preamble_raw") <- preamble_raw
  }

  acc_data
}

read_csv_actigraph_raw_ <- function(file, tz = "UTC") {
  first_15 <- readr::read_lines(file, n_max = 15)

  preamble_locations <- which(stringr::str_detect(first_15, "\\-{3,}"))

  if (length(preamble_locations) == 2) {
    preamble_raw <- first_15[preamble_locations[1]:preamble_locations[2]]
    skip <- preamble_locations[2]
  } else if (length(preamble_locations) == 1) {
    preamble_raw <- first_15[1:preamble_locations[1]]
    skip <- preamble_locations[1]
  } else if (length(preamble_locations) == 0) {
    preamble_raw <- character(0)
    skip <- 0
  }

  preamble <- preamble_parser_(preamble_raw, tz = tz)

  header <- !stringr::str_detect(first_15[skip + 1], "^[,\\d]")

  if (header) {
    data <-
      suppressMessages(readr::read_csv(file, skip = skip)) %>%
      rename_all(tolower) %>%
      rename_at(vars(starts_with("activity")),
                ~stringr::str_c("axis", seq_len(length(.)))) %>%
      mutate(timestamp = lubridate::mdy_hms(str_c(date, time, sep = " "), tz = tz)) %>%
      mutate_at(vars(starts_with("axis")), as.integer) %>%
      select(timestamp, num_range("axis", 1:3))
  } else {
    data <-
      suppressMessages(readr::read_csv(file, col_names = FALSE, skip = skip)) %>%
      rename_all(~stringr::str_replace(., "^X", "axis")) %>%
      mutate(timestamp = preamble$startdatetime + lubridate::seconds((0:(n() - 1L)) * preamble$epochlength)) %>%
      select(timestamp, num_range("axis", 1:3))
  }

  list(data = data, preamble = preamble, preamble_raw = preamble_raw)
}

preamble_parser_ <- function(preamble_raw, tz = "UTC") {

  if (length(preamble_raw) == 0) {
    return(tibble())
  }

  start_time <-
    preamble_raw %>%
    stringr::str_subset(stringr::regex("start time", ignore_case = TRUE)) %>%
    stringr::str_extract("\\d{2}:\\d{2}:\\d{2}")

  start_date <-
    preamble_raw %>%
    stringr::str_subset(stringr::regex("start date", ignore_case = TRUE)) %>%
    stringr::str_extract("\\d{1,2}/\\d{1,2}/\\d{4}")

  startdatetime <- lubridate::mdy_hms(stringr::str_c(start_date, start_time, sep = " "), tz = tz)

  epochlength <-
    preamble_raw %>%
    stringr::str_subset(stringr::regex("epoch|cycle period", ignore_case = TRUE)) %>%
    stringr::str_extract("\\d{2}:\\d{2}:\\d{2}") %>%
    hms::as_hms() %>%
    as.integer(units = "seconds")

  tibble(startdatetime = startdatetime,
         epochlength = epochlength)
}
