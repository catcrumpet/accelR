#' @export
summarise_day <- function(acc_data,
                          acc_values = "axis1",
                          pa = "pa",
                          invalid = "nonwear") {

  make_data_(acc_data, acc_values, pa, invalid) %>%
    mutate(date = lubridate::date(timestamp)) %>%
    group_by(date) %>%
    do(summarise_chunk_(data = ., get_epochlength(acc_data))) %>%
    ungroup()
}
