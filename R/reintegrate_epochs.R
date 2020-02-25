#' @export
reintegrate_epochs <- function(acc_data, new_epochlength, collapse_function = sum) {
  epochlength <- get_epochlength(acc_data)

  stopifnot(60 %% epochlength == 0)
  stopifnot(60 %% new_epochlength == 0)
  stopifnot(new_epochlength %% epochlength == 0)
  check_data_integrity(acc_data)
  check_data_gaps(acc_data)

  integration_factor <- as.integer(new_epochlength / epochlength)

  output_data <-
    acc_data %>%
    mutate(timestamp = floor_date(timestamp, seconds(new_epochlength))) %>%
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
