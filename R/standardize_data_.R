standardize_data_ <- function(acc_data, counts = axis1, pa = pa, valid = valid, data_table = FALSE) {

  std_tb <-
    acc_data %>%
    as_tibble() %>%
    transmute(timestamp,
              counts = !!enquo(counts),
              pa = !!enquo(pa),
              valid = !!enquo(valid))

  if (data_table) {
    as.data.table(std_tb)
  } else {
    std_tb
  }
}
