#' @export
summarise_day <- function(acc_data, counts = axis1, pa = pa, valid = valid) {

  epochlength <- get_epochlength(acc_data)

  standardize_data_(acc_data,
                    !!rlang::enquo(counts),
                    !!rlang::enquo(pa),
                    !!rlang::enquo(valid)) %>%
    dplyr::mutate(date = lubridate::date(timestamp)) %>%
    dplyr::group_by(date) %>%
    dplyr::group_modify(~summarise_chunk_(., epochlength = epochlength)) %>%
    dplyr::ungroup()
}
