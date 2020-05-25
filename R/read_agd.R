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
    assert_that(file.exists(file))

    db <- DBI::dbConnect(RSQLite::SQLite(), dbname = file)
    on.exit(DBI::dbDisconnect(db))

    settings <-
        db %>%
        tbl("settings") %>%
        select(settingName, settingValue) %>%
        collect() %>%
        distinct() %>%
        tidyr::spread(settingName, settingValue, convert = TRUE) %>%
        mutate_at(vars(any_of(c("height", "mass", "age")),
                       matches("dateOfBirth"),
                       ends_with("time"),
                       ends_with("date")), ~na_if(., 0)) %>%
        mutate_at(vars(any_of("sex")), ~if_else(. %in% "Undefined", NA_character_, .)) %>%
        mutate_at(vars(any_of("finished")), as.logical) %>%
        mutate_at(vars(any_of(c("height", "mass", "age"))), as.numeric) %>%
        mutate_at(vars(any_of(c("customsleepparameters", "notes"))), as.character) %>%
        mutate_at(vars(any_of(c("dateOfBirth")),
                       ends_with("time"),
                       ends_with("date")),
                  ~convert_time_(., tz = tz)) %>%
        select(pull(tbl(db, "settings"), settingName)) %>%
        rename_all(~tolower(stringr::str_replace_all(., "\\s", "_"))) %>%
        mutate_at(vars(starts_with("epoch")), as.integer)

    raw_data <-
        db %>%
        tbl("data") %>%
        collect() %>%
        rename_all(tolower)

    baseline_time_raw <- raw_data$datatimestamp[[1]]
    baseline_time <- convert_time_(baseline_time_raw, tz)

    data <-
        raw_data %>%
        rename(timestamp = datatimestamp) %>%
        mutate(timestamp = (timestamp - baseline_time_raw) / 1e+07) %>%
        mutate_if(is.numeric, as.integer) %>%
        mutate(timestamp = baseline_time + seconds(timestamp))

    list(data = data, settings = settings)
}

convert_time_ <- function(time, tz) {
    # anytime::anytime(time / 1e+07 - 62135596800)
    as.POSIXct(time / 1e7, origin = "0001-01-01 00:00:00", tz = "UTC") %>%
        lubridate::force_tz(tzone = tz)
}