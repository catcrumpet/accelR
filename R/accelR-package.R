#' accelR
#'
#' @name accelR
#' @docType package
#' @useDynLib accelR, .registration = TRUE
#' @importFrom assertthat assert_that has_attr has_name
#' @importFrom tibble tibble
#' @importFrom tsibble tsibble
#' @importFrom dplyr select select_at select_if
#' @importFrom dplyr rename rename_at rename_if rename_all
#' @importFrom dplyr filter filter_at filter_if filter_all
#' @importFrom dplyr mutate mutate_at mutate_if mutate_all
#' @importFrom dplyr transmute transmute_at transmute_if
#' @importFrom dplyr summarise summarise_at summarise_if
#' @importFrom dplyr arrange distinct group_by
#' @importFrom dplyr tbl pull collect
#' @importFrom dplyr vars starts_with ends_with matches
#' @importFrom dplyr n first last row_number lag lead if_else case_when na_if
#' @importFrom magrittr %>%
#' @importFrom rlang !!
NULL

globalVariables(".")