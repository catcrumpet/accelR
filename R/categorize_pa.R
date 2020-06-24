#' Categorize accelerometer epochs into PA categories
#'
#' Calculates PA category given count data and other paramters. Returns a
#' vector of values.
#' @param counts Numeric vector of accelerometer values.
#' @param epochlength Epoch length in seconds.
#' @param age Age in years as a numeric value.
#' @param cut_params Cut parameters, default is Troiano cut parameters.
#' @return An ordinal vector of the same length as \code{x}.
#' @export
categorize_pa <- function(counts, epochlength, age, cut_params = pa_params_troiano) {
  purrr::lift(cut)(c(x = list(counts * (60L / epochlength)), cut_params(age)))
}

#' Cut parameters for Troiano
#'
#' Generates cut parameters based on Troiano values given an age.
#' @param age Age in years as a numeric value.
#' @return A list of parameters for the cut function.
#' @export
pa_params_troiano <- function(age) {
  stopifnot(is.numeric(age))

  if (age < 6) {
    stop("Cut parameters not designed for under 6 years of age")
  }

  dplyr::case_when(age == 6 ~ c(1400, 3758),
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
    list(breaks = .,
         labels = c("sed", "lig", "mod", "vig", "ext"),
         include.lowest = TRUE,
         right = FALSE,
         ordered_result = TRUE)
}

#' Cut parameters for Matthews
#'
#' Generates cut parameters based on Matthews values for adults.
#' @param age Age in years as a numeric value.
#' @return A list of parameters for the cut function.
#' @export
pa_params_matthews <- function(age = 18) {
  stopifnot(is.numeric(age))

  if (age < 18) {
    warning("Cut parameters not designed for under 18 years of age")
  }

  list(breaks = c(0, 100, 760, 5725, 16000, Inf),
       labels = c("sed", "lig", "mod", "vig", "ext"),
       include.lowest = TRUE,
       right = FALSE,
       ordered_result = TRUE)
}

#' Add PA categories to accelerometer data
#'
#' Convenience wrapper for \code{categorize_pa}
#' @export
add_pa <- function(acc_data, counts, age, cut_params = pa_params_troiano, pa = "pa") {
  pa_vals <- categorize_pa(dplyr::pull(acc_data, !!rlang::enquo(counts)),
                           epochlength = get_epochlength(acc_data),
                           age = age,
                           cut_params = cut_params)

  dplyr::mutate(acc_data, !!rlang::enquo(pa) := pa_vals)
}