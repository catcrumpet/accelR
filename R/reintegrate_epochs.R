#' @export
reintegrate_epochs <- function(acc_data, new_epoch_len, collapse_function = sum) {
  stopifnot(60 %% get_epochlength(acc_data) == 0)
  stopifnot(60 %% new_epoch_len == 0)
  stopifnot(new_epoch_len %% get_epochlength(acc_data) == 0)
  check_data_integrity(acc_data)
  check_data_gaps(acc_data)

  integration_factor <- as.integer(new_epoch_len / get_epochlength(acc_data))

  start_timestamp <- first(acc_data$timestamp)

  output_data <-
    acc_data %>%
    as_tibble() %>%
    mutate(timestamp =
               lubridate::floor_date(timestamp,
                                     lubridate::seconds(new_epoch_len))) %>%
    group_by(timestamp) %>%
    {full_join(ungroup(summarise_if(., is.numeric, collapse_function)),
               ungroup(summarise(., n = n())),
               by = "timestamp")} %>%
    ungroup() %>%
    filter(n == integration_factor) %>%
    select(-n) %>%
    tsibble::as_tsibble(index = timestamp)

  check_data_integrity(output_data)
  check_data_gaps(output_data)

  output_data
}
