#' Read accelerometer data from an *.agd file
#'
#' Read ActiGraph accelerometer data from a database stored in an AGD file. Return a tsibble.
#' @param file Full path to an agd file to read.
#' @param tz Time zone for the output data, converted from UTC (the default).
#' @return A \code{tsibble} (\code{tbl_ts}) of accelerometer data with at least two columns: timestamp and axis1.
#' ActiGraph settings are stored as a tibble in the settings attribute.
#' @export
read_agd <- function(file, tz = "UTC") {

    agd_data_raw <- read_agd_raw_(file, tz)

    acc_data <-
        agd_data_raw$data %>%
        tsibble::as_tsibble(index = timestamp) %>%
        set_attr_("type", "actigraph") %>%
        set_attr_("settings", agd_data_raw$settings)

    assert_that(tsibble::is_regular(acc_data),
                tsibble::is_ordered(acc_data),
                !tsibble::is_duplicated(acc_data, index = timestamp))
    check_data_starttime(acc_data)
    check_data_stoptime(acc_data)
    check_data_gaps(acc_data)
    check_data_epochlength(acc_data)

    acc_data
}

read_agd_raw_ <- function(file, tz = "UTC") {
    assert_that(file.exists(file))

    db <- DBI::dbConnect(RSQLite::SQLite(), dbname = file)
    on.exit(DBI::dbDisconnect(db))

    settings <-
        db %>%
        tbl("settings") %>%
        select(settingName, settingValue) %>%
        collect() %>%
        tidyr::spread(settingName, settingValue, convert = TRUE) %>%
        mutate_at(vars(height, mass, age,
                       matches("dateOfBirth"),
                       ends_with("time"),
                       ends_with("date")), ~na_if(., 0)) %>%
        mutate(sex = if_else(sex %in% "Undefined", NA_character_, sex)) %>%
        mutate_at(vars(finished), as.logical) %>%
        mutate_at(vars(height, mass, age), as.numeric) %>%
        mutate_at(vars(customsleepparameters, notes), as.character) %>%
        mutate_at(vars(matches("dateOfBirth"),
                       ends_with("time"),
                       ends_with("date")),
                  ~time_convert_(., tz = tz)) %>%
        select(pull(tbl(db, "settings"), settingName)) %>%
        rename_all(~tolower(stringr::str_replace_all(., "\\s", "_"))) %>%
        mutate_at(vars(starts_with("epoch")), as.integer)

    assert_that(nrow(settings) == 1)

    data <-
        db %>%
        tbl("data") %>%
        collect() %>%
        rename_all(tolower) %>%
        rename(timestamp = datatimestamp) %>%
        mutate(timestamp = time_convert_(timestamp, tz = tz)) %>%
        mutate_if(is.numeric, as.integer)

    list(data = data, settings = settings)
}

time_convert_ <- function(time, tz) {
    anytime::anytime(time / 1e+07 - 62135596800, tz = tz)
    # as.POSIXct(time / 1e7, origin = "0001-01-01 00:00:00", tz = tz)
}
