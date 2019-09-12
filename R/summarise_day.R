#' @export
summarise_day <- function(acc_data, use_magnitude = FALSE) {

  epoch_len <- get_epochlength(acc_data)
  epochs_min <- 60L / epoch_len

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

  acc_data %>%
    as_tibble() %>%
    select(timestamp, counts = !!counts_var, pa = !!pa_var) %>%
    mutate(date = lubridate::date(timestamp)) %>%
    mutate(valid = !nonvalid) %>%
    group_by(date) %>%
    do(summarise_chunk_(.$timestamp, .$counts, .$pa, .$valid, !!epoch_len)) %>%
    ungroup() %>%
    set_acc_attr_("summary_day", use_magnitude = use_magnitude)
}
