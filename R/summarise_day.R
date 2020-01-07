#' @export
summarise_day <- function(acc_data, counts = axis1, pa = pa, valid = !nonwear) {

  standardize_data_(acc_data,
                    !!enquo(counts),
                    !!enquo(pa),
                    !!enquo(valid)) %>%
    mutate(date = lubridate::date(timestamp)) %>%
    group_by(date) %>%
    do(summarise_chunk_(data = ., get_epochlength(acc_data))) %>%
    ungroup()
}
