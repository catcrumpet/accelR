summarise_chunk_ <- function(std_data, epochlength) {
  epochs_min <- 60L / epochlength

  if (nrow(std_data) == 0) {
    dplyr::tibble(timestamp_start = lubridate::as_datetime(NA),
                  timestamp_stop = lubridate::as_datetime(NA),

                  epochlength = epochlength,

                  total_e = 0L,
                  total_m = 0,
                  total_c = 0L,

                  valid_e = 0L,
                  valid_m = 0,
                  valid_c = 0L)
  } else {
    output_total <-
      std_data %>%
      dplyr::summarise(timestamp_start = min(timestamp),
                timestamp_stop = max(timestamp) + lubridate::seconds(epochlength),

                epochlength = epochlength,

                total_e = dplyr::n(),
                total_m = dplyr::n() / epochs_min,
                total_c = sum(counts),

                valid_e = sum(valid),
                valid_m = sum(valid) / epochs_min,
                valid_c = sum(counts[valid]))

    stopifnot(with(output_total, valid_m <= total_m))
    stopifnot(with(output_total, valid_c <= total_c))

    if (output_total$valid_m > 0) {
      output_pa_min <-
        std_data %>%
        dplyr::count(pa, valid, .drop = FALSE) %>%
        dplyr::mutate(n = n / epochs_min) %>%
        tidyr::spread(pa, n) %>%
        dplyr::rename_at(dplyr::vars(-valid), ~stringr::str_c(., "m", sep = "_")) %>%
        dplyr::mutate_at(dplyr::vars(-valid), ~tidyr::replace_na(., 0)) %>%
        dplyr::filter(valid) %>%
        dplyr::select(-valid)

      output_pa_count <-
        std_data %>%
        dplyr::group_by(valid, pa, .drop = FALSE) %>%
        dplyr::summarise(counts = sum(counts)) %>%
        dplyr::ungroup() %>%
        tidyr::spread(pa, counts) %>%
        dplyr::rename_at(dplyr::vars(-valid), ~stringr::str_c(., "c", sep = "_")) %>%
        dplyr::mutate_at(dplyr::vars(-valid), ~tidyr::replace_na(., 0)) %>%
        dplyr::filter(valid) %>%
        dplyr::select(-valid)

      stopifnot(all.equal(output_total$valid_m, rowSums(output_pa_min)))
      stopifnot(all.equal(output_total$valid_c, rowSums(output_pa_count)))

      dplyr::bind_cols(output_total, output_pa_min, output_pa_count)
    } else {
      output_total
    }
  }
}
