#' @export

add_pa <- function(acc_data,
                   age = NA,
                   cut_params = pacuts_troiano,
                   use_magnitude = FALSE,
                   add_age = FALSE,
                   pa = "pa") {
  if (is.na(age) & is.na(get_setting(acc_data, "age"))) {
    stop("Valid age must be provided.")
  } else if (!is.na(get_setting(acc_data, "age"))) {
    age <- get_setting(acc_data, "age")
  }

  if (is.character(add_age)) {
    age_var <- add_age
    add_age <- TRUE
  } else if (add_age) {
    age_var <- "age"
  }

  acc_data %>%
    {
      if (add_age) mutate_acc_(., !!age_var := !!age) else .
    } %>%
    mutate_acc_(!!pa := add_pa_(data = if (!!use_magnitude) calc_mag_(.) else axis1,
                                epoch_len = attr(., "epochlength"),
                                pacuts = cut_params(as.integer(!!age))))
}

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
    list(breaks = .,
         labels = c("sed", "lig", "mod", "vig", "ext"),
         include.lowest = TRUE,
         right = FALSE,
         ordered_result = TRUE)
}

add_pa_ <- function(data, epoch_len, pacuts) {
  purrr::lift(cut)(c(x = list(data * (60L / epoch_len)), pacuts))
}