summarise_chunk_ <- function(data, epoch_len) {
  epochs_min <- 60L / epoch_len

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
      mutate_at(vars(-valid), ~tidyr::replace_na(., 0)) %>%
      filter(valid) %>%
      select(-valid)

    output_pa_count <-
      data %>%
      group_by(valid, pa, .drop = FALSE) %>%
      summarise(counts = sum(counts)) %>%
      ungroup() %>%
      tidyr::spread(pa, counts) %>%
      rename_at(vars(-valid), ~stringr::str_c(., "c", sep = "_")) %>%
      mutate_at(vars(-valid), ~tidyr::replace_na(., 0)) %>%
      filter(valid) %>%
      select(-valid)

    assert_that(all.equal(output_total$valid_m, rowSums(output_pa_min)))
    assert_that(all.equal(output_total$valid_c, rowSums(output_pa_count)))

    bind_cols(output_total, output_pa_min, output_pa_count)
  } else {
    output_total
  }
}

make_data_ <- function(acc_data, use_magnitude) {

  pa_var <- name_acc_type_(acc_data, "pa")
  if (length(pa_var) == 0) {
    stop("PA category column is missing.")
  } else if (length(pa_var) > 1) {
    stop("More than 1 PA category column.")
  }

  if (attr(acc_data[[pa_var]], "acc")$parameters$use_magnitude & !use_magnitude) {
    warning("PA column relied on magnitude column. Using magnitude.")
    use_magnitude <- TRUE
  }

  counts_var <- "axis1"
  if (use_magnitude) {
    acc_data <- use_magnitude_(acc_data)
    counts_var <- name_acc_type_(acc_data, "magnitude")
    assert_that(length(counts_var) == 1,
                msg = "More than one extant magnitude column.")
  }

  valid_var <- name_acc_type_(acc_data, c("valid"))
  if (length(valid_var) == 0) {
    valid <- calculate_nonvalid_(acc_data)
  }

  acc_data %>%
    tibble::as_tibble() %>%
    select(timestamp, counts = !!counts_var, pa = !!pa_var) %>%
    mutate(valid = !!valid)
}