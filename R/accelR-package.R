#' accelR
#'
#' @name accelR
#' @docType package
#' @useDynLib accelR, .registration = TRUE
#' @rawNamespace import(data.table, except = c(first, between, last, key))
#' @import dtplyr
#' @rawNamespace import(dplyr, except = c(id))
#' @import tsibble
#' @importFrom assertthat assert_that has_attr
#' @importFrom lubridate is.POSIXct
#' @importFrom lubridate mdy_hms ymd_hms
#' @importFrom lubridate minutes seconds
#' @importFrom lubridate floor_date round_date with_tz
#' @importFrom purrr map map_chr map_dbl map_dfr map2 map2_dfr pmap
#' @importFrom rlang list2 !! !!! has_name enquo enquos
#' @importFrom stats quantile
#' @importFrom stringr str_c str_detect str_extract str_length str_replace str_subset regex
#' @importFrom tibble tibble as_tibble enframe
NULL

.datatable.aware <- TRUE

moomoo <- TRUE

globalVariables(".")