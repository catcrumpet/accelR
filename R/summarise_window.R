#' @export
summarise_window <- function(acc_data,
                             counts = axis1, pa = pa, valid = !nonwear,
                             anchor_time,
                             window) {

  stopifnot(lubridate::is.POSIXct(anchor_time) & length(anchor_time) == 1)
  # msg = "Anchor time must be a POSIXct object of length 1."

  win_list <- convert_window_(window)

  # create look_vec: length(ISOdatetime()) == 2
  time_boundaries <- anchor_time + lubridate::minutes(win_list$win_vec)

  output <-
    tibble(anchor_time = anchor_time,
           window_start = win_list$win_vec[[1]],
           window_stop = win_list$win_vec[[2]],
           window_length = win_list$win_len[[1]],
           time_start = time_boundaries[[1]],
           time_stop = time_boundaries[[2]])

  .data_subset <-
    acc_data %>%
    filter(timestamp >= time_boundaries[[1]], timestamp < time_boundaries[[2]])

  if (nrow(data) > 0) {
    window_summary <-
      standardize_data_(.data_subset,
                        !!enquo(counts),
                        !!enquo(pa),
                        !!enquo(valid)) %>%
      summarise_chunk_(get_epochlength(.data_subset))

    output <- bind_cols(output, window_summary)
  }

  output
}

convert_window_ <- function(window) {
  assert_that(is.numeric(window) & length(window) %in% 1:2,
              msg = "Window must be a numeric vector of length 1 or 2.")

  if (length(window) == 1) {
    win_len <- as.integer(abs(window))
    if (window < 0) {
      win_vec <- c(window, 0L)
    } else if (window > 0) {
      win_vec <- c(0L, window)
    }
  } else if (length(window) == 2) {
    stopifnot(window[[2]] > window[[1]])
    win_vec <- window
    win_len <- as.integer(win_vec[2] - win_vec[1])
  }

  stopifnot(win_len > 0)

  list(win_vec = win_vec, win_len = win_len)
}
