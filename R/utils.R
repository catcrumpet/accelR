ext_setting_ <- function(agd_data, setting) {
  attributes(agd_data)$settings[[setting]]
}

calc_mag_ <- function(acc_data) {
  acc_data %>%
    tsibble::as_tibble() %>%
    select(starts_with("axis")) %>%
    purrr::pmap_dbl(function(...) sqrt(sum(c(...) ^ 2)))
}

add_mag_ <- function(acc_data, magnitude = "magnitude") {
  acc_data %>%
    mutate(!!magnitude := calc_mag_(.))
}

set_attr_ <- function(x, which, value) {
  `attr<-`(x, which, value)
}
