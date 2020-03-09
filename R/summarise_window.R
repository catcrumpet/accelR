#' @export
summarise_window <- function(acc_data,
                             anchor_times,
                             windows,
                             counts = axis1,
                             pa = pa,
                             valid = valid) {

  stopifnot(is.POSIXct(anchor_times))
  # msg = "Anchor times must be a vector of POSIXct objects."

  epochlength <- get_epochlength(acc_data)

  parameter_tb <-
    tidyr::expand_grid(anchor_time = anchor_times,
                       map_dfr(windows, convert_window_)) %>%
    mutate(window_start = anchor_time + minutes(window_left),
           window_stop = anchor_time + minutes(window_right))

  std_ldt_subset <-
    standardize_data_(acc_data,
                      !!enquo(counts),
                      !!enquo(pa),
                      !!enquo(valid),
                      data_table = TRUE) %>%
    lazy_dt(key_by = timestamp) %>%
    filter(timestamp >= min(parameter_tb$window_start),
           timestamp < max(parameter_tb$window_stop)) %>%
    compute()

  parameter_tb %>%
    bind_cols(map2_dfr(.$window_start,
                       .$window_stop,
                       ~summarise_window_(std_ldt_subset, .x, .y, epochlength)))
}

summarise_window_ <- function(std_ldt, window_start, window_stop, epochlength) {
  std_ldt %>%
    filter(timestamp >= !!window_start, timestamp < !!window_stop) %>%
    as_tibble() %>%
    summarise_chunk_(epochlength)
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

if (moomoo) {
  convert_window_ <- memoise::memoise(convert_window_)
}