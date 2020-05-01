#' @export
summarise_day <- function(acc_data, counts = axis1, pa = pa, valid = !nonwear) {

  epochlength <- get_epochlength(acc_data)

  standardize_data_(acc_data,
                    !!enquo(counts),
                    !!enquo(pa),
                    !!enquo(valid)) %>%
    #                 data_table = TRUE) %>%
    # lazy_dt(key_by = timestamp) %>%
    mutate(date = lubridate::date(timestamp)) %>%
    group_by(date) %>%
    group_modify(~summarise_chunk_(., epochlength = epochlength))
    # as_tibble()
}
