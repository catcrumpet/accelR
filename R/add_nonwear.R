#' @export
add_nonwear_troiano <- function(acc_data, nonwear = "nonwear", ...) {
  args_list <- list2(...)

  if (has_name(args_list, "use_magnitude")) {
    if (args_list$use_magnitude) {
      acc_data <- use_magnitude_(acc_data)
    }
  }

  nonwear_periods <- count_nonwear_troiano(acc_data, ...)

  acc_data %>%
    add_period_(nonwear_periods, nonwear) %>%
    {
      .[[nonwear]] <- set_attr_(.[[nonwear]], "acc", attr(nonwear_periods, "acc"))
      .
    }
}

#' @export
count_nonwear_troiano <- function(acc_data,
                                  activity_threshold = 0,
                                  min_period_len = 60,
                                  max_nonzero_count = Inf,
                                  spike_tolerance = 2,
                                  spike_stoplevel = 100,
                                  use_magnitude = FALSE,
                                  endat_nnz_seq = TRUE) {

  check_data_epochlength(acc_data)
  check_data_gaps(acc_data)

  if (use_magnitude) {
    acc_data <- use_magnitude_(acc_data)
    mag_var <- name_acc_type_(acc_data, "magnitude")
    assert_that(length(mag_var) == 1,
                msg = "More than one extant magnitude column.")
    check_data_missing(acc_data, mag_var)
    counts <- acc_data[[mag_var]]
  } else {
    check_data_missing(acc_data, "axis1")
    counts <- acc_data[["axis1"]]
  }

  troiano_args <-
    list(timestamp = acc_data$timestamp,
         counts = counts,
         epoch_len = get_epochlength(acc_data),
         activity_threshold = activity_threshold,
         min_period_len = min_period_len,
         max_nonzero_count = max_nonzero_count,
         spike_tolerance = spike_tolerance,
         spike_stoplevel = spike_stoplevel)

  if (endat_nnz_seq) {
    output <- purrr::lift(count_nonwear_troiano_seq_)(troiano_args)
  } else {
    output <- purrr::lift(count_nonwear_troiano_nonseq_)(troiano_args)
  }

  set_acc_attr_(output, "nonwear", !!!troiano_args[-(1:2)])
}

count_nonwear_troiano_seq_ <- function(timestamp,
                                       counts,
                                       epoch_len,
                                       activity_threshold,
                                       min_period_len,
                                       max_nonzero_count,
                                       spike_tolerance,
                                       spike_stoplevel) {
  epochs_min <- 60L / epoch_len
  activity_threshold <- activity_threshold / epochs_min
  min_period_len <- min_period_len * epochs_min
  max_nonzero_count <- max_nonzero_count / epochs_min
  spike_tolerance <- spike_tolerance * epochs_min
  spike_stoplevel <- spike_stoplevel / epochs_min

  tibble(timestamp = timestamp,
         wear =
           case_when(counts <= activity_threshold | counts > max_nonzero_count ~ 0L,
                     counts > spike_stoplevel ~ 2L,
                     TRUE ~ 1L)) %>%
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

count_nonwear_troiano_nonseq_ <- function(timestamp,
                                          counts,
                                          activity_threshold,
                                          min_period_len,
                                          max_nonzero_count,
                                          spike_tolerance,
                                          spike_stoplevel) {
  epochs_min <- 60L / epoch_len
  activity_threshold <- activity_threshold / epochs_min
  min_period_len <- min_period_len * epochs_min
  max_nonzero_count <- max_nonzero_count / epochs_min
  spike_tolerance <- spike_tolerance * epochs_min
  spike_stoplevel <- spike_stoplevel / epochs_min

  tibble(timestamp = timestamp,
         counts = if_else(counts > max_nonzero_count, 0, counts),
         .n = wle(counts, activity_threshold, spike_tolerance, spike_stoplevel)) %>%
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