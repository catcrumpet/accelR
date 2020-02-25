format_v.POSIXct <- function(x) {
  format.POSIXct(x, format = "%Y-%m-%d %H:%M:%S", usetz = TRUE)
}

str_pad_0_adj <- function(x) {
  stringr::str_pad(x, max(str_length(x)), "left", "0")
}