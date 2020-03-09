#' Categorize nonwear
#' @param counts Numeric vector of accelerometer values.
#' @param epochlength Epoch length in seconds.
#' @export
categorize_nonwear <- function(counts, epochlength, algorithm = nonwear_troiano, ...) {
  algorithm(counts, epochlength, ...)
}

#' @export
nonwear_troiano <- function(counts,
                            epochlength,
                            activity_threshold = 0,
                            min_nonwear_mins = 60,
                            max_nonzero_count = Inf,
                            spike_tolerance_mins = 2,
                            spike_stoplevel = 100,
                            endat_nnz_seq = TRUE) {
  epochs_min <- 60L / epochlength

  troiano_args <-
    list(counts = counts,
         activity_threshold = activity_threshold / epochs_min,
         min_period_len = min_nonwear_mins * epochs_min,
         max_nonzero_count = max_nonzero_count / epochs_min,
         spike_tolerance = spike_tolerance_mins * epochs_min,
         spike_stoplevel = spike_stoplevel / epochs_min)

  if (endat_nnz_seq) {
    purrr::lift(count_nonwear_troiano_seq_)(troiano_args)
  } else {
    purrr::lift(count_nonwear_troiano_nonseq_)(troiano_args)
  }
}

#' @export
nonwear_troiano_modified <- function(counts,
                                     epochlength,
                                     activity_threshold = 0,
                                     min_period_len = 60,
                                     max_nonzero_count = Inf,
                                     spike_tolerance = 0,
                                     spike_stoplevel = 100,
                                     endat_nnz_seq = TRUE) {
  epochs_min <- 60L / epochlength

  troiano_args <-
    list(counts = counts,
         activity_threshold = activity_threshold / epochs_min,
         min_period_len = min_period_len * epochs_min,
         max_nonzero_count = max_nonzero_count / epochs_min,
         spike_tolerance = spike_tolerance * epochs_min,
         spike_stoplevel = spike_stoplevel / epochs_min)

  if (endat_nnz_seq) {
    purrr::lift(count_nonwear_troiano_seq_)(troiano_args)
  } else {
    purrr::lift(count_nonwear_troiano_nonseq_)(troiano_args)
  }
}

count_nonwear_troiano_seq_ <- function(counts,
                                       activity_threshold,
                                       min_period_len,
                                       max_nonzero_count,
                                       spike_tolerance,
                                       spike_stoplevel) {

  check_x_missing(counts)

  counts_seq <- seq_along(counts)

  data.table(row = counts_seq,
             counts = counts) %>%
    lazy_dt(immutable = FALSE) %>%
    mutate(wear =
             case_when(counts > spike_stoplevel ~ 2L,
                       counts <= activity_threshold | counts > max_nonzero_count ~ 0L,
                       TRUE ~ 1L)) %>%
    group_by(rleid = rleidv(wear)) %>%
    summarise(wear = data.table::first(wear),
              .from = data.table::first(row),
              .n = n()) %>%
    mutate(wear =
             case_when(row_number() == 1 & is.na(wear) ~ 1L,
                       wear == 1 & lead(wear, default = 1L) == 0L & .n <= spike_tolerance ~ NA_integer_,
                       TRUE ~ wear) %>%
             zoo::na.locf() %>%
             as.integer()) %>%
    group_by(rleid = rleidv(wear)) %>%
    summarise(wear = data.table::first(wear),
              .from = data.table::first(.from),
              .n = sum(.n)) %>%
    filter(wear == 0L, .n >= min_period_len) %>%
    transmute(.from, .to = .from + .n - 1L) %>%
    as.data.table() %>%
    pmap(~..1:..2) %>%
    unlist() %>%
    {counts_seq %in% .}
}

count_nonwear_troiano_nonseq_ <- function(counts,
                                          activity_threshold,
                                          min_period_len,
                                          max_nonzero_count,
                                          spike_tolerance,
                                          spike_stoplevel) {
  check_x_missing(counts)

  counts_seq <- seq_along(counts)

  tibble(row = counts_seq,
         counts = if_else(counts > max_nonzero_count, 0, counts),
         .n = wle(counts, activity_threshold, spike_tolerance, spike_stoplevel)) %>%
    lazy_dt(immutable = FALSE) %>%
    filter(.n >= min_period_len) %>%
    rename(.from = row) %>%
    select(.from, .n) %>%
    mutate(a = .from - dplyr::first(.from),
           b = .to - dplyr::first(.from)) %>%
    filter(overlap(a, b)) %>%
    select(.from, .to, .n) %>%
    as.data.table() %>%
    pmap(~..1:..2) %>%
    unlist() %>%
    {counts_seq %in% .}
}

#' Convenience wrapper for \code{categorize_nonwear_troiano}
#' @export
add_nonwear <- function(acc_data,
                        counts,
                        algorithm = nonwear_troiano,
                        ...,
                        nonwear = "nonwear") {
  epochlength <- get_epochlength(acc_data)

  acc_data %>%
    mutate(!!enquo(nonwear) :=
             categorize_nonwear(!!enquo(counts),
                                epochlength = !!epochlength,
                                algorithm = !!algorithm,
                                ...))

}