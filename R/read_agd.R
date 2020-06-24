#' Read accelerometer data from an *.agd file
#'
#' Read ActiGraph accelerometer data from a database stored in an AGD file. Return a tsibble.
#' @param file Full path to an agd file to read.
#' @param tz Time zone for the output data, converted from UTC (the default).
#' @param settings Keep agd settings as an attribute?
#' @return A \code{tsibble} (\code{tbl_ts}) of accelerometer data with at least two columns: timestamp and axis1.
#' @export
read_agd <- function(file, tz = "UTC", settings = FALSE) {

    agd_data_raw <- read_agd_raw_(file, tz)

    assertthat::assert_that(nrow(agd_data_raw$settings) == 1)

    acc_data <- tsibble::as_tsibble(agd_data_raw$data, index = timestamp)
    agd_settings <- agd_data_raw$settings

    check_data_integrity(acc_data)
    check_agddata_epochlength(acc_data, agd_settings)
    check_agddata_epochcount(acc_data, agd_settings, warning = TRUE)
    check_agddata_starttime(acc_data, agd_settings)
    check_agddata_stoptime(acc_data, agd_settings, warning = TRUE)
    check_data_gaps(acc_data)

    if (settings) {
        attr(acc_data, "agd_settings") <- agd_settings
    }

    acc_data
}

read_agd_raw_ <- function(file, tz = "UTC") {
    assertthat::assert_that(file.exists(file))

    db <- DBI::dbConnect(RSQLite::SQLite(), dbname = file)
    on.exit(DBI::dbDisconnect(db))

    settings <-
        db %>%
        dplyr::tbl("settings") %>%
        dplyr::select(settingName, settingValue) %>%
        dplyr::collect() %>%
        dplyr::distinct() %>%
        tidyr::spread(settingName, settingValue, convert = TRUE) %>%
        dplyr::mutate_at(dplyr::vars(dplyr::any_of(c("height", "mass", "age")),
                              dplyr::matches("dateOfBirth"),
                              dplyr::ends_with("time"),
                              dplyr::ends_with("date")), ~dplyr::na_if(., 0)) %>%
        dplyr::mutate_at(dplyr::vars(dplyr::any_of("sex")),
                         ~dplyr::if_else(. %in% "Undefined", NA_character_, .)) %>%
        dplyr::mutate_at(dplyr::vars(dplyr::any_of("finished")), as.logical) %>%
        dplyr::mutate_at(dplyr::vars(dplyr::any_of(c("height", "mass", "age"))), as.numeric) %>%
        dplyr::mutate_at(dplyr::vars(dplyr::any_of(c("customsleepparameters", "notes"))), as.character) %>%
        dplyr::mutate_at(dplyr::vars(dplyr::any_of(c("dateOfBirth")),
                                     dplyr::ends_with("time"),
                                     dplyr::ends_with("date")),
                         ~convert_agd_time_(., tz = tz)) %>%
        dplyr::select(dplyr::pull(dplyr::tbl(db, "settings"), settingName)) %>%
        dplyr::rename_all(~tolower(stringr::str_replace_all(., "\\s", "_"))) %>%
        dplyr::mutate_at(dplyr::vars(dplyr::starts_with("epoch")), as.integer)

    raw_data <-
        db %>%
        dplyr::tbl("data") %>%
        dplyr::collect() %>%
        dplyr::rename_all(tolower)

    # There is a very specific bug in lubridate where it breaks at the DST
    # boundary. The way to get around is to first convert the time to UTC, do
    # the time operations, and then convert it back to the proper time zone.

    # This took a day to figure out. DO NOT FORGET THIS.

    baseline_time_raw <- dplyr::first(raw_data$datatimestamp)
    # convert to UTC time
    baseline_time_utc <- convert_agd_time_(baseline_time_raw, tz = "UTC")

    data <-
        raw_data %>%
        dplyr::rename(timestamp = datatimestamp) %>%
        dplyr::mutate(timestamp = (timestamp - baseline_time_raw) / 1e+07) %>%
        dplyr::mutate_if(is.numeric, as.integer) %>%
        # first calculate timestamps using UTC time
        dplyr::mutate(timestamp = baseline_time_utc + lubridate::seconds(timestamp),
                      # then convert to the desired timezone
                      timestamp = lubridate::with_tz(timestamp, tzone = tz))

    list(data = data, settings = settings)
}

convert_agd_time_ <- function(x, tz) {
    # anytime::anytime(time / 1e+07 - 62135596800)
    as.POSIXct(x / 1e7, origin = "0001-01-01 00:00:00", tz = tz)
}
