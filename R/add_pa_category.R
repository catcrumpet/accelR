#' Add PA category to accelerometer data
#'
#' Adds PA category as a new column in accelerometer data. Returns original
#' data with appended column(s).
#' @param acc_data Accelerometer tsibble object.
#' @param age Age in years as a numeric value.
#' @param cut_params Cut parameters, default is Troiano cut parameters.
#' @param use_magnitude Should magnitude be used instead of axis1 (the default)?
#' @param add_age Should the age value be appended as a separate column to the
#' output? Can accept a string value, which will be used as the new age column
#' name.
#' @param pa The name for the new column containing PA category values.
#' @return The original accelerometer data with an appended column for physical
#' activity category and optionally another appended column for age.
#' ActiGraph settings are stored as a tibble in the settings attribute.
#' @export
add_pa_category <- function(acc_data,
                            age = NA,
                            cut_params = pacuts_troiano,
                            use_magnitude = FALSE,
                            add_age = FALSE,
                            pa = "pa") {
  if (is.na(age) & is.na(get_setting(acc_data, "age"))) {
    stop("Valid age must be provided.")
  } else if (!is.na(get_setting(acc_data, "age"))) {
    message("Using age provided in settings.")
    age <- get_setting(acc_data, "age")
  }

  if (is.character(add_age)) {
    age_varname <- add_age
    add_age <- TRUE
  } else if (add_age) {
    age_varname <- "age"
  }

  if (use_magnitude) {
    acc_data <- use_magnitude_(acc_data)
    counts_var <- name_acc_type_(acc_data, "magnitude")
    assert_that(length(counts_var) == 1,
                msg = "More than one extant magnitude column.")
  } else {
    counts_var <- "axis1"
  }

  pa_args <-
    list(counts = acc_data[[counts_var]],
         epoch_len = get_epochlength(acc_data),
         age = age,
         cut_params = cut_params(age))

  acc_data %>%
    {
      if (add_age) mutate_acc_(., !!age_varname := !!age) else .
    } %>%
    mutate_acc_(!!pa :=
                  categorize_pa(!!!pa_args) %>%
                  set_acc_attr_("pa", !!!pa_args[-1]))
}

#' Calculate PA category
#'
#' Calculates PA category given count data and other paramters. Returns a
#' vector of values.
#' @param x Numeric vector of counts.
#' @param epoch_len Epoch length in seconds.
#' @param age Age in years as a numeric value.
#' @param cut_params Cut parameters, default is Troiano cut parameters.
#' @return An ordinal vector of the same length as \code{x}.
#' @export
categorize_pa <- function(counts, epoch_len, age, cut_params = pacuts_troiano(age)) {
  purrr::lift(cut)(c(x = list(counts * (60L / epoch_len)), cut_params))
}

#' Cut parameters for Troiano
#'
#' Generates cut parameters based on Troiano values given an age.
#' @param age Age in years as a numeric value.
#' @return A list of parameters for the cut function.
#' @export
pacuts_troiano <- function(age) {
  case_when(age == 6 ~ c(1400, 3758),
            age == 7 ~ c(1515, 3947),
            age == 8 ~ c(1638, 4147),
            age == 9 ~ c(1770, 4360),
            age == 10 ~ c(1910, 4588),
            age == 11 ~ c(2059, 4832),
            age == 12 ~ c(2220, 5094),
            age == 13 ~ c(2393, 5375),
            age == 14 ~ c(2580, 5679),
            age == 15 ~ c(2781, 6007),
            age == 16 ~ c(3000, 6363),
            age == 17 ~ c(3239, 6751),
            age >= 18 ~ c(2020, 5999)) %>%
    {c(0, 100, ., 16000, Inf)} %>%
    {
      list(name = "troiano",
           breaks = .,
           labels = c("sed", "lig", "mod", "vig", "ext"),
           include.lowest = TRUE,
           right = FALSE,
           ordered_result = TRUE)
    }
}