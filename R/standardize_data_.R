standardize_data_ <- function(acc_data, counts = axis1, pa = pa, valid = valid, data_table = FALSE) {

  std_tb <-
    acc_data %>%
    as_tibble() %>%
    transmute(timestamp,
              counts = !!enquo(counts),
              pa = !!enquo(pa),
              valid = !!enquo(valid))

  if (with(std_tb, is.character(counts) | is.character(pa) | is.character(valid))) {
    stop("The resulting counts, pa, or valid columns are character types. Did you mean to use unquoted arguments?")
  }

  if (!is.numeric(std_tb$counts)) {
    stop("Resulting counts column is not a numeric type.")
  }

  if (!is.factor(std_tb$pa)) {
    stop("Resulting pa column is not a factor.")
  }

  if (!is.logical(std_tb$valid)) {
    stop("Resulting valid column is not a logical type.")
  }

  if (data_table) {
    as.data.table(std_tb)
  } else {
    std_tb
  }
}
