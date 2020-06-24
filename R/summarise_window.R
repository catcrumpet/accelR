# windows are sorted
#' @export
summarise_window <- function(acc_data,
                             anchor_times,
                             windows,
                             counts = axis1,
                             pa = pa,
                             valid = valid) {

  stopifnot(lubridate::is.POSIXct(anchor_times))
  # msg = "Anchor times must be a vector of POSIXct objects."

  epochlength <- get_epochlength(acc_data)

  parameter_tb <-
    tidyr::expand_grid(anchor_time = anchor_times,
                       purrr::map_dfr(windows, convert_window_)) %>%
    dplyr::mutate(window_start = anchor_time + lubridate::minutes(window_left),
                  window_stop = anchor_time + lubridate::minutes(window_right))

  # TODO: compare to group by timestamp and pre-subset using the largest window
  # span within timestamp group

  std_data_subset <-
    standardize_data_(acc_data,
                      !!rlang::enquo(counts),
                      !!rlang::enquo(pa),
                      !!rlang::enquo(valid)) %>%
    dplyr::filter(timestamp >= min(parameter_tb$window_start),
                  timestamp < max(parameter_tb$window_stop))

  parameter_tb %>%
    dplyr::bind_cols(purrr::map2_dfr(.$window_start,
                                     .$window_stop,
                                     ~summarise_window_(std_data_subset, .x, .y, epochlength)))
}

summarise_window_ <- function(std_data, window_start, window_stop, epochlength) {
  std_data %>%
    dplyr::filter(timestamp >= !!window_start, timestamp < !!window_stop) %>%
    summarise_chunk_(epochlength)
}

convert_window_ <- function(window) {
  assertthat::assert_that(is.numeric(window) & length(window) %in% 1:2,
                          msg = "Window must be a numeric vector of length 1 or 2.")

  if (length(window) == 1) {
    win_vec <- sort(c(window, 0L))
    # win_len <- as.integer(abs(window))
  } else if (length(window) == 2) {
    win_vec <- sort(window)
  }

  assertthat::assert_that(win_vec[1] < win_vec[2],
                          msg = "Window boundaries are misspecified.")

  win_len <- as.integer(win_vec[2] - win_vec[1])

  dplyr::tibble(window_left = win_vec[1],
                window_right = win_vec[2],
                window_length = win_len)
}