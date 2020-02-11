standardize_data_ <- function(acc_data, counts, pa, valid, data_table = FALSE) {
  acc_data %>%
    as_tibble() %>%
    transmute(timestamp,
              counts = !!enquo(counts),
              pa = !!enquo(pa),
              valid = !!enquo(valid)) %>%
    {
      if (data_table) {
        data.table::setkey(data.table::as.data.table(.), timestamp)
      } else {
        .
      }
    }
}