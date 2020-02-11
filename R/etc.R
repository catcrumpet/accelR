format_v.POSIXct <- function(x) {
  format.POSIXct(x, format = "%Y-%m-%d %H:%M:%S", usetz = TRUE)
}