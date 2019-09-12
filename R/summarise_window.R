#' @export
summarise_window <- function(acc_data,
                             anchor_time,
                             window,
                             use_magnitude = FALSE) {
  epoch_len <- get_epochlength(acc_data)

  counts_var <- "axis1"
  if (use_magnitude) {
    acc_data <- use_magnitude_(acc_data)
    counts_var <- name_acc_type_(acc_data, "magnitude")
    assert_that(length(counts_var) == 1,
                msg = "More than one extant magnitude column.")
  }

  pa_var <- name_acc_type_(acc_data, "pa")
  if (length(pa_var) == 0) {
    stop("PA category column is missing.")
  } else if (length(pa_var) > 1) {
    stop("More than 1 PA category column.")
  }

  nonvalid_var <- name_acc_type_(acc_data, c("nonwear", "nonvalid"))
  if (length(nonvalid_var) == 0) {
    nonvalid <- rep(FALSE, nrow(acc_data))
  } else {
    nonvalid <- pmap_lgl(acc_data[, nonvalid_var], ~any(...))
  }

  assert_that(lubridate::is.POSIXct(anchor_time) & length(anchor_time) == 1,
              msg = "Anchor time must be a POSIXct object of length 1.")

  win_list <- convert_window_(window)

  # create look_vec: length(ISOdatetime()) == 2
  time_boundaries <- anchor_time + lubridate::minutes(win_list$win_vec)

  window_summary <-
    acc_data %>%
    as_tibble() %>%
    select(timestamp, counts = !!counts_var, pa = !!pa_var) %>%
    mutate(valid = !nonvalid) %>%
    filter(timestamp >= time_boundaries[[1]], timestamp < time_boundaries[[2]]) %>%
    with(summarise_chunk_(timestamp, counts, pa, valid, epoch_len))

  tibble(anchor_time = anchor_time,
         window_start = win_list$win_vec[[1]],
         window_stop = win_list$win_vec[[2]],
         window_length = win_list$win_len[[1]],
         time_start = time_boundaries[[1]],
         time_stop = time_boundaries[[2]]) %>%
    bind_cols(window_summary) %>%
    set_acc_attr_("summary_window",
                  use_magnitude = use_magnitude)
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
    assert_that(window[[2]] > window[[1]])
    win_vec <- window
    win_len <- as.integer(win_vec[2] - win_vec[1])
  }

  assert_that(win_len > 0)

  list(win_vec = win_vec, win_len = win_len)
}

summarise_window_fast_ <- function(data, epoch_len, anchor_time, window_left, window_right) {
  data %>%
    filter(timestamp >= (anchor_time + lubridate::minutes(window_left)),
           timestamp < (anchor_time + lubridate::minutes(window_right))) %>%
    with(summarise_chunk_(timestamp, counts, pa, valid, epoch_len))
}
