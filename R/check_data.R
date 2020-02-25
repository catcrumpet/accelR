is_starty_bad <- function(acc_data, starttime) {
  dplyr::first(acc_data$timestamp) != starttime
}

# chk_d_start_
check_agddata_starttime <- function(agd_data, agd_settings) {
  if (is_starty_bad(agd_data,
                    agd_settings$startdatetime[[1]])) {
    stop("Data start time does not match setting start time.")
  }
  invisible(TRUE)
}

is_stoppy_bad <- function(acc_data, epochlength, stoptime) {
  (dplyr::last(acc_data$timestamp) + lubridate::seconds(epochlength)) != stoptime
}

# chk_d_stop_
check_agddata_stoptime <- function(agd_data, agd_settings) {
  if (is_stoppy_bad(agd_data,
                    get_epochlength(agd_data),
                    agd_settings$stopdatetime[[1]])) {
    stop("Data stop time does not match setting stop time.")
  }
  invisible(TRUE)
}

is_gappy_bad <- function(acc_data) {
  any(tsibble::has_gaps(acc_data)$.gaps)
}

# chk_d_gaps_
check_data_gaps <- function(acc_data) {
  if (is_gappy_bad(acc_data)) {
    stop("Data contains gaps in observations.")
  }
  invisible(TRUE)
}

check_data_integrity <- function(acc_data) {
  if (!tsibble::is_regular(acc_data)) {
    stop("Data is not regular.")
  }
  if (!tsibble::is_ordered(acc_data)) {
    stop("Data is not ordered.")
  }
  if (tsibble::is_duplicated(acc_data, index = !!index(acc_data))) {
    stop("Data contains duplicated observations.")
  }
  invisible(TRUE)
}

# chk_d_epoch_
check_agddata_epochlength <- function(agd_data, agd_settings) {
  if (get_epochlength(agd_data) != agd_settings$epochlength[[1]]) {
    stop("Epoch length does not match data interval.")
  }
  if (60L %% get_epochlength(agd_data)) {
    stop("Epochs should be exact divisors of 60.")
  }
  invisible(TRUE)
}

check_agddata_epochcount <- function(agd_data, agd_settings) {
  if (nrow(agd_data) != agd_settings$epochcount[[1]]) {
    stop("Actual epoch count does not match recorded epoch count.")
  }
  invisible(TRUE)
}

# chk_d_miss_
check_data_missing <- function(acc_data, variable) {
  if (anyNA(acc_data[[variable]])) {
    stop("Data contains missing observations on ", variable, ".")
  }
  invisible(TRUE)
}

check_x_missing <- function(x) {
  stopifnot(!anyNA(x))
  invisible(TRUE)
}

check_data_presummary <- function(acc_data) {
  check_data_gaps(acc_data)
  check_data_missing(acc_data, "timestamp")
  check_data_missing(acc_data, "counts")
  check_data_missing(acc_data, "pa")
  check_data_missing(acc_data, "invalid")

  stopifnot(is.logical(acc_data$invalid))
}