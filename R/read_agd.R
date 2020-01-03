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

    assert_that(nrow(agd_data_raw$settings) == 1)

    agd_data <- tsibble::as_tsibble(agd_data_raw$data, index = timestamp)
    agd_settings <- agd_data_raw$settings

    assert_that(tsibble::is_regular(agd_data),
                tsibble::is_ordered(agd_data),
                !tsibble::is_duplicated(agd_data, index = timestamp))



    check_agddata_epochlength(agd_data, agd_settings)
    check_agddata_epochcount(agd_data, agd_settings)
    check_agddata_starttime(agd_data, agd_settings)
    check_agddata_stoptime(agd_data, agd_settings)
    check_data_gaps(agd_data)


    if (settings) {
        attr(agd_data, "agd_settings") <- agd_settings
    }

    agd_data
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
                  ~convert_time_agd_(., tz = tz)) %>%
        select(pull(tbl(db, "settings"), settingName)) %>%
        rename_all(~tolower(stringr::str_replace_all(., "\\s", "_"))) %>%
        mutate_at(vars(starts_with("epoch")), as.integer)

    data <-
        db %>%
        tbl("data") %>%
        collect() %>%
        rename_all(tolower) %>%
        rename(timestamp = datatimestamp) %>%
        mutate(timestamp = convert_time_agd_(timestamp, tz = tz)) %>%
        mutate_if(is.numeric, as.integer)

    list(data = data, settings = settings)
}

convert_time_agd_ <- function(time, tz) {
    anytime::anytime(time / 1e+07 - 62135596800, tz = tz)
    # as.POSIXct(time / 1e7, origin = "0001-01-01 00:00:00", tz = tz)
}