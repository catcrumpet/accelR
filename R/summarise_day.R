#' @export
summarise_day <- function(acc_data, use_magnitude = FALSE) {
  epoch_len <- get_epochlength(acc_data)

  make_data_(acc_data, use_magnitude) %>%
    mutate(date = lubridate::date(timestamp)) %>%
    group_by(date) %>%
    do(summarise_chunk_(data = ., !!epoch_len)) %>%
    ungroup() %>%
    set_acc_attr_("summary_day", use_magnitude = use_magnitude)
}
