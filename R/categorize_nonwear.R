#' Categorize nonwear
#' @param counts Numeric vector of accelerometer values.
#' @param epoch_len Epoch length in seconds.
#' @export
categorize_nonwear <- function(counts, epoch_len, method = nonwear_troiano, ...) {
  method(counts, epoch_len, ...)
}

#' @export
nonwear_troiano <- function(counts,
                            epoch_len,
                            activity_threshold = 0,
                            min_period_len = 60,
                            max_nonzero_count = Inf,
                            spike_tolerance = 2,
                            spike_stoplevel = 100,
                            endat_nnz_seq = TRUE) {
  epochs_min <- 60L / epoch_len

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

#' @export
nonwear_troiano_modified <- function(counts, epoch_len, endat_nnz_seq = TRUE) {
  epochs_min <- 60L / epoch_len

  troiano_args <-
    list(counts = counts,
         activity_threshold = 0,
         min_period_len = 60 * epochs_min,
         max_nonzero_count = Inf,
         spike_tolerance = 0,
         spike_stoplevel = 0)

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

  acc_sequence <- seq_along(counts)

  tibble(row = acc_sequence,
         counts = counts) %>%
    mutate(wear =
             case_when(counts > spike_stoplevel ~ 2L,
                       counts <= activity_threshold | counts > max_nonzero_count ~ 0L,
                       TRUE ~ 1L)) %>%
    group_by(rleid = data.table::rleidv(wear)) %>%
    summarise(wear = first(wear),
              .from = first(row),
              .n = n()) %>%
    mutate(wear =
             case_when(row_number() == 1 & is.na(wear) ~ 1L,
                       wear == 1 & lead(wear, default = 1L) == 0L & .n <= spike_tolerance ~ NA_integer_,
                       TRUE ~ wear) %>%
             zoo::na.locf() %>%
             as.integer()) %>%
    group_by(rleid = data.table::rleidv(wear)) %>%
    summarise(wear = first(wear),
              .from = first(.from),
              .n = sum(.n)) %>%
    filter(wear == 0L, .n >= min_period_len) %>%
    transmute(.from, .to = .from + .n - 1L) %>%
    pmap(~..1:..2) %>%
    unlist() %>%
    {acc_sequence %in% .}
}

count_nonwear_troiano_nonseq_ <- function(counts,
                                          activity_threshold,
                                          min_period_len,
                                          max_nonzero_count,
                                          spike_tolerance,
                                          spike_stoplevel) {
  check_x_missing(counts)

  acc_sequence <- seq_along(counts)

  tibble(row = acc_sequence,
         counts = if_else(counts > max_nonzero_count, 0, counts),
         .n = wle(counts, activity_threshold, spike_tolerance, spike_stoplevel)) %>%
    filter(.n >= min_period_len) %>%
    rename(.from = row) %>%
    select(.from, .n) %>%
    mutate(a = .from - first(.from),
           b = .to - first(.from)) %>%
    filter(overlap(a, b)) %>%
    select(.from, .to, .n) %>%
    pmap(~..1:..2) %>%
    unlist() %>%
    {acc_sequence %in% .}
}

#' Convenience wrapper for \code{categorize_nonwear_troiano}
#' @export
add_nonwear <- function(acc_data,
                        counts,
                        method = nonwear_troiano,
                        ...,
                        nonwear = "nonwear") {
  epoch_len <- get_epochlength(acc_data)

  acc_data %>%
    mutate(!!enquo(nonwear) :=
             categorize_nonwear(!!enquo(counts),
                                epoch_len = !!epoch_len,
                                method = !!method,
                                ...))

}