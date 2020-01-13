read_csv_actigraph <- function(file, tz = "UTC", preamble = FALSE) {
  csv_data_raw <- read_csv_actigraph_raw_(file, tz)

  acc_data <- tsibble::as_tsibble(csv_data_raw$data, index = timestamp)
  preamble <- csv_data_raw$preamble

  check_data_integrity(acc_data)
  check_data_gaps(acc_data)

  if (preamble) {
    attr(acc_data, "preamble") <- preamble
  }

  acc_data
}

read_csv_actigraph_raw_ <- function(file, tz = "UTC") {
  first_15 <- read_lines(file, n_max = 15)

  preamble_locations <- which(str_detect(first_15, "\\-{3,}"))

  if (length(preamble_locations) == 2) {
    preamble <- first_15[preamble_locations[1]:preamble_locations[2]]
    skip <- preamble_locations[2]
  } else if (length(preamble_locations) == 1) {
    preamble <- first_15[1:preamble_locations[1]]
    skip <- preamble_locations[1]
  } else if (length(preamble_locations) == 0) {
    preamble <- character(0)
    skip <- 0
  }

  data <-
    suppressMessages(read_csv(file, skip = skip)) %>%
    rename_all(tolower) %>%
    transmute(timestamp = mdy_hms(str_c(date, time, sep = " "), tz = tz),
              activity,
              activity_hz = `activity (horizontal)`,
              steps) %>%
    mutate_at(vars(activity:steps), as.integer)

  list(data = data, preamble = preamble)
}

