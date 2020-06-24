standardize_data_ <- function(acc_data, counts = axis1, pa = pa, valid = valid) {

  std_data <-
    acc_data %>%
    dplyr::as_tibble() %>%
    dplyr::select(timestamp,
                  counts = !!rlang::enquo(counts),
                  pa = !!rlang::enquo(pa),
                  valid = !!rlang::enquo(valid))

  if (with(std_data, is.character(counts) | is.character(pa) | is.character(valid))) {
    stop("The resulting counts, pa, or valid columns are character types. Did you mean to use unquoted arguments?")
  }

  if (!is.numeric(std_data$counts)) {
    stop("Resulting counts column is not a numeric type.")
  }

  if (!is.factor(std_data$pa)) {
    stop("Resulting pa column is not a factor.")
  }

  if (!is.logical(std_data$valid)) {
    stop("Resulting valid column is not a logical type.")
  }

  std_data
}
