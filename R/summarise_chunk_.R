summarise_chunk_ <- function(timestamp, counts, pa, valid, epoch_len) {
  epochs_min <- 60L / epoch_len

  data <-
    tibble(timestamp = timestamp,
           counts = counts,
           pa = pa,
           valid = valid)

  output_total <-
    data %>%
    summarise(
      timestamp_min = min(timestamp),
      timestamp_max = max(timestamp),

      epoch_length = !!epoch_len,
      total_e = n(),
      total_m = n() / epochs_min,
      total_c = sum(counts),

      valid_e = sum(valid),
      valid_m = sum(valid) / epochs_min,
      valid_c = sum(counts[valid]))

  assert_that(with(output_total, valid_m <= total_m))
  assert_that(with(output_total, valid_c <= total_c))

  if (output_total$valid_m > 0) {
    output_pa_min <-
      data %>%
      count(pa, valid, .drop = FALSE) %>%
      ungroup() %>%
      mutate(n = n / epochs_min) %>%
      tidyr::spread(pa, n) %>%
      rename_at(vars(-valid), ~stringr::str_c(., "m", sep = "_")) %>%
      mutate_at(vars(-valid), ~ replace_na(., 0)) %>%
      filter(valid) %>%
      select(-valid)

    output_pa_count <-
      data %>%
      group_by(valid, pa, .drop = FALSE) %>%
      summarise(counts = sum(counts)) %>%
      ungroup() %>%
      tidyr::spread(pa, counts) %>%
      rename_at(vars(-valid), ~stringr::str_c(., "c", sep = "_")) %>%
      mutate_at(vars(-valid), ~ replace_na(., 0)) %>%
      filter(valid) %>%
      select(-valid)

    assert_that(all.equal(output_total$valid_m, rowSums(output_pa_min)))
    assert_that(all.equal(output_total$valid_c, rowSums(output_pa_count)))

    bind_cols(output_total, output_pa_min, output_pa_count)
  } else {
    output_total
  }
}
