#' @export

add_nonwear_troiano <- function(acc_data, nonwear = "nonwear", ...) {
  add_period_(acc_data,
              identify_nonwear_troiano(acc_data, ...),
              nonwear)
}

#' @export

identify_nonwear_troiano <- function(acc_data,
                                     activity_threshold = 0,
                                     min_period_len = 60,
                                     max_nonzero_count = Inf,
                                     spike_tolerance = 2,
                                     spike_stoplevel = 100,
                                     use_magnitude = FALSE,
                                     endat_nnz_seq = TRUE) {

  check_data_epochlength(acc_data)
  check_data_gaps(acc_data)
  check_data_missing(acc_data, "axis1")

  if (use_magnitude) {
    for (i in tidyselect::vars_select(names(acc_data), starts_with("axis"), -axis1)) {
      check_data_missing(acc_data, i)
    }
  }

  epoch_len <- get_epochlength(acc_data)
  epochs_min <- 60L / epoch_len

  data <-
    acc_data %>%
    tsibble::as_tibble() %>%
    transmute(timestamp,
              count = if (use_magnitude) calc_mag_(.) else as.numeric(axis1))

  troiano_args <-
    list(data = data,
         epoch_len = epoch_len,
         activity_threshold = activity_threshold / epochs_min,
         min_period_len = min_period_len * epochs_min,
         max_nonzero_count = max_nonzero_count / epochs_min,
         spike_tolerance = spike_tolerance * epochs_min,
         spike_stoplevel = spike_stoplevel / epochs_min,
         use_magnitude = use_magnitude)

  if (endat_nnz_seq) {
    purrr::lift(id_nw_troiano_seq_)(troiano_args)
  } else {
    purrr::lift(id_nw_troiano_nonseq_)(troiano_args)
  }
}

id_nw_troiano_seq_ <- function(data,
                               epoch_len,
                               activity_threshold,
                               min_period_len,
                               max_nonzero_count,
                               spike_tolerance,
                               spike_stoplevel,
                               use_magnitude) {

  data %>%
    mutate(wear = if_else(count <= activity_threshold | count > max_nonzero_count, 0L, 1L),
           wear = if_else(count > spike_stoplevel, 2L, wear)) %>%
    group_by(rleid = data.table::rleidv(wear)) %>%
    summarise(wear = first(wear),
              .from = first(timestamp),
              .n = n()) %>%
    mutate(wear = if_else(wear == 1L &
                            lead(wear, default = 1L) == 0L &
                            .n <= spike_tolerance,
                          NA_integer_, wear),
           # Since `na.locf` can't impute leading NAs, fill in those with 1s
           wear = if_else(row_number() == 1 & is.na(wear), 1L, wear),
           # Fill in NAs with the most recent zero/nonzero wear state
           wear = zoo::na.locf(wear)) %>%
    group_by(rleid = data.table::rleidv(wear)) %>%
    summarise(wear = first(wear),
              .from = first(.from),
              .n = sum(.n)) %>%
    filter(wear == 0L, .n >= min_period_len) %>%
    mutate(.to = .from + lubridate::seconds(.n * epoch_len)) %>%
    select(.from, .to, .n)
}

id_nw_troiano_nonseq_ <- function(data,
                                  epoch_len,
                                  activity_threshold,
                                  min_period_len,
                                  max_nonzero_count,
                                  spike_tolerance,
                                  spike_stoplevel,
                                  use_magnitude) {

  data %>%
    transmute(count = if_else(count > max_nonzero_count, 0, count),
              .n = wle(count, activity_threshold, spike_tolerance, spike_stoplevel)) %>%
    filter(.n >= min_period_len) %>%
    rename(.from = timestamp) %>%
    select(.from, .n) %>%
    mutate(.to = .from + lubridate::seconds(.n * epoch_len)) %>%
    mutate(a = lubridate::time_length(.from - first(.from), "second"),
           b = lubridate::time_length(.to - first(.from), "second")) %>%
    # Remove periods which overlap with previous periods
    filter(overlap(a, b)) %>%
    select(.from, .to, .n)
}