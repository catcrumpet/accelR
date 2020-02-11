#' @export
summarise_window <- function(acc_data,
                             anchor_times,
                             windows,
                             counts = axis1,
                             pa = pa,
                             valid = valid) {

  stopifnot(lubridate::is.POSIXct(anchor_times))
  # msg = "Anchor times must be a vector of POSIXct objects."

  standardized_data <-
    standardize_data_(acc_data,
                      !!enquo(counts),
                      !!enquo(pa),
                      !!enquo(valid))

  epochlength <- get_epochlength(acc_data)

  parameter_table <-
    expand_grid(anchor_time = anchor_times,
                map_dfr(windows, convert_window_)) %>%
    mutate(window_start = anchor_time + lubridate::minutes(window_left),
           window_stop = anchor_time + lubridate::minutes(window_right))

  parameter_table %>%
    bind_cols(map2(.$window_start,
                   .$window_stop,
                   function(time_start, time_stop) {
                     standardized_data %>%
                       filter(timestamp >= time_start,
                              timestamp < time_stop) %>%
                       summarise_chunk_(epochlength)
                   }))
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

  tibble(window_left = win_vec[1],
         window_right = win_vec[2],
         window_length = win_len)
}
