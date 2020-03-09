summarise_chunk_ <- function(std_tb, epochlength) {
  epochs_min <- 60L / epochlength

  if (nrow(std_tb) == 0) {
    tibble(timestamp_start = lubridate::as_datetime(NA),
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
      std_tb %>%
      summarise(timestamp_start = min(timestamp),
                timestamp_stop = max(timestamp) + seconds(epochlength),

                epochlength = epochlength,

                total_e = n(),
                total_m = n() / epochs_min,
                total_c = sum(counts),

                valid_e = sum(valid),
                valid_m = sum(valid) / epochs_min,
                valid_c = sum(counts[valid]))

    stopifnot(with(output_total, valid_m <= total_m))
    stopifnot(with(output_total, valid_c <= total_c))

    if (output_total$valid_m > 0) {
      output_pa_min <-
        std_tb %>%
        count(pa, valid, .drop = FALSE) %>%
        mutate(n = n / epochs_min) %>%
        tidyr::spread(pa, n) %>%
        rename_at(vars(-valid), ~str_c(., "m", sep = "_")) %>%
        mutate_at(vars(-valid), ~tidyr::replace_na(., 0)) %>%
        filter(valid) %>%
        select(-valid)

      output_pa_count <-
        std_tb %>%
        group_by(valid, pa, .drop = FALSE) %>%
        summarise(counts = sum(counts)) %>%
        ungroup() %>%
        tidyr::spread(pa, counts) %>%
        rename_at(vars(-valid), ~str_c(., "c", sep = "_")) %>%
        mutate_at(vars(-valid), ~tidyr::replace_na(., 0)) %>%
        filter(valid) %>%
        select(-valid)

      stopifnot(all.equal(output_total$valid_m, rowSums(output_pa_min)))
      stopifnot(all.equal(output_total$valid_c, rowSums(output_pa_count)))

      bind_cols(output_total, output_pa_min, output_pa_count)
    } else {
      output_total
    }
  }
}
