#' Read accelerometer data from an *.csv file
#'
#' Read ActiGraph accelerometer data from a database stored in an CSV file. Return a tsibble.
#' @param file Full path to a csv file to read.
#' @param tz Time zone for the output data, converted from UTC (the default).
#' @param preamble Keep csv file preamble as an attribute?
#' @return A \code{tsibble} (\code{tbl_ts}) of accelerometer data with at least two columns: timestamp and axis1.
#' @export
read_csv_actigraph <- function(file, tz = "UTC", include_preamble = FALSE) {
  data_raw <- read_csv_actigraph_raw_(file, tz)
  acc_data <- tsibble::as_tsibble(data_raw$data, index = timestamp)
  preamble <- data_raw$preamble
  preamble_raw <- data_raw$preamble_raw

  # if (correct) {
  #   acc_data <- correct_acc_data_gaps_(acc_data)
  #
  #   if (nrow(csv_preamble) == 1) {
  #     acc_data <-
  #       correct_acc_starttime_(acc_data,
  #                              csv_preamble$epochlength,
  #                              csv_preamble$startdatetime)
  #   }
  # }

  check_data_integrity(acc_data)
  check_data_gaps(acc_data)

  if (nrow(preamble) > 0) {
    check_agddata_epochlength(acc_data, preamble)
    check_agddata_starttime(acc_data, preamble)
  }

  if (include_preamble) {
    attr(acc_data, "preamble") <- preamble
    attr(acc_data, "preamble_raw") <- preamble_raw
  }

  acc_data
}

read_csv_actigraph_raw_ <- function(file, tz = "UTC") {

  preamble_list <- process_preamble_(file, tz = tz)

  preamble_preamble <- preamble_list$preamble

  skip <- preamble_list$skip

  stopifnot(nrow(preamble) %in% 0:1)

  header <- !stringr::str_detect(readr::read_lines(file = file, skip = skip, n_max = 1), "^[,\\d]")

  data <-
    if (header) {
      read_csv_actigraph_raw_header_(file, skip, preamble, tz)
    } else {
      read_csv_actigraph_raw_noheader_(file, skip, preamble, tz)
    }

  list(data = data,
       preamble = preamble_preamble,
       preamble_raw = preamble_list$preamble_raw)
}

process_preamble_ <- function(file, tz = "UTC") {
  first_15 <- data.table::fread(file = file, sep = "\n", nrows = 15, header = FALSE)[[1]]
  # readr::read_lines(file, n_max = 15)

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

  if (length(preamble_raw) == 0) {
    return(dplyr::tibble())
  }

  start_time <-
    preamble_raw %>%
    stringr::str_subset(stringr::regex("start time", ignore_case = TRUE)) %>%
    stringr::str_extract("\\d{2}:\\d{2}:\\d{2}")

  start_date <-
    preamble_raw %>%
    stringr::str_subset(stringr::regex("start date", ignore_case = TRUE)) %>%
    stringr::str_extract("\\d{1,2}/\\d{1,2}/\\d{4}")

  startdatetime <-
    stringr::str_c(start_date, start_time, sep = " ") %>%
    lubridate::mdy_hms(tz = tz)

  epochlength <-
    preamble_raw %>%
    stringr::str_subset(stringr::regex("epoch|cycle period", ignore_case = TRUE)) %>%
    stringr::str_extract("\\d{2}:\\d{2}:\\d{2}") %>%
    hms::as_hms() %>%
    as.integer(units = "seconds")

  preamble <- dplyr::tibble(startdatetime = startdatetime, epochlength = epochlength)

  stopifnot(nrow(preamble) == 1)

  list(preamble = preamble, skip = skip, preamble_raw = preamble_raw)
}

read_csv_actigraph_raw_header_ <- function(file, skip, preamble, tz) {
  data_raw <-
    data.table::fread(file = file, sep = ",", skip = skip) %>%
    dplyr::as_tibble() %>%
    dplyr::rename_all(tolower) %>%
    dplyr::rename_at(dplyr::vars(dplyr::starts_with("activity")),
                     ~stringr::str_c("axis", seq_len(length(.)))) %>%
    mutate(timestamp =
             suppressWarnings(lubridate::mdy_hms(stringr::str_c(date, time, sep = " "), tz = tz))) %>%
    dplyr::mutate_at(dplyr::vars(dplyr::starts_with("axis")), as.integer) %>%
    dplyr::select(timestamp, dplyr::num_range("axis", 1:3))

  if (nrow(preamble) == 1) {
    starttime <- preamble$startdatetime
    epochlength <- preamble$epochlength
  } else {
    starttime <- data_raw$timestamp[[1]]
    epochlength <-
      difftime(data_raw$timestamp[[2]],
               data_raw$timestamp[[1]],
               units = "secs") %>%
      {round(. / 5) * 5} %>%
      as.integer()
  }

  data_raw %>%
    dplyr::mutate(timestamp =
                    {lubridate::with_tz(starttime, tzone = "UTC") +
                        lubridate::seconds((0:(dplyr::n() - 1L)) * epochlength)} %>%
                    lubridate::with_tz(tzone = tz))
}

read_csv_actigraph_raw_noheader_ <- function(file, skip, preamble, tz) {
  data.table::fread(file = file, sep = ",", header = FALSE, skip = skip) %>%
    dplyr::as_tibble() %>%
    dplyr::rename_all(~stringr::str_replace(., "^V", "axis")) %>%
    dplyr::mutate(timestamp =
                    {lubridate::with_tz(preamble$startdatetime, tzone = "UTC") +
                        lubridate::seconds((0:(dplyr::n() - 1L)) * preamble$epochlength)} %>%
                    lubridate::with_tz(tzone = tz)) %>%
    dplyr::select(timestamp, dplyr::num_range("axis", 1:3))
}

correct_acc_data_gaps_ <- function(acc_data) {
  # if there are gaps, then maybe the timings are bad
  if (has_any_gaps(acc_data)) {
    message("Data contains gaps, applying correction.")

    epochlength_guess <-
      difftime(acc_data$timestamp[[2]],
               acc_data$timestamp[[1]],
               units = "secs") %>%
      {round(. / 5) * 5} %>%
      as.integer()

    acc_data <-
      acc_data %>%
      dplyr::mutate(timestamp = lubridate::round_date(timestamp, lubridate::seconds(epochlength_guess))) %>%
      tsibble::as_tsibble(index = timestamp)
  }
  acc_data
}

correct_acc_starttime_ <- function(acc_data, epochlength, starttime) {
  # if there are gaps, then maybe the timings are bad
  if (has_starttime_mismatch(acc_data, starttime)) {
    message("Data possesses mismatched start time, applying correction.")

    timestamp_error <-
      difftime(acc_data$timestamp[[1]],
               starttime,
               units = "secs")

    if (timestamp_error == epochlength & timestamp_error <= 60) {
      acc_data <-
        acc_data %>%
        dplyr::mutate(timestamp = starttime + lubridate::seconds((0:(dplyr::n() - 1L)) * epochlength)) %>%
        tsibble::as_tsibble(index = timestamp)
    } else {
      stop("Cannot correct start time mismatch.")
    }
  }
  acc_data
}