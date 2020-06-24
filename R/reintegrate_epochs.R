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
    dplyr::mutate(timestamp = lubridate::floor_date(timestamp, lubridate::seconds(new_epochlength))) %>%
    dplyr::group_by(timestamp) %>%
    {dplyr::full_join(dplyr::ungroup(dplyr::summarise_if(., is.numeric, collapse_function)),
                      dplyr::ungroup(dplyr::summarise(., n = dplyr::n())),
                      by = "timestamp")} %>%
    dplyr::ungroup() %>%
    dplyr::filter(n == integration_factor) %>%
    dplyr::select(-n) %>%
    tsibble::as_tsibble(index = timestamp)

  check_data_integrity(output_data)
  check_data_gaps(output_data)

  output_data
}
