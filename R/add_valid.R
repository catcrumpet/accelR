#' @export
add_valid <- function(acc_data, expression, valid = "valid") {
  if (missing(expression)) {
    mutate_acc_(acc_data,
                !!valid :=
                  !calculate_nonvalid_(acc_data) %>%
                  set_acc_attr_("valid",
                                components = name_acc_type_(acc_data,
                                                            c("nonwear", "nonvalid"))))
  } else {
    rlang::enquo(expression) %>%
      {
          set_acc_attr_(rlang::eval_tidy(., data = acc_data),
                        "nonvalid",
                        expression = as.character(rlang::get_expr(.)))
      } %>%
      {
        assert_that(is.logical(.),
                    msg = "Expression must evaluate to a logical vector.")
        mutate_acc_(acc_data, !!valid := .)
      }
  }
}

calculate_nonvalid_ <- function(acc_data) {
  nonvalid_var <- name_acc_type_(acc_data, c("nonwear", "nonvalid"))
  if (length(nonvalid_var) == 0) {
    message("No extant nonwear/nonvalid columns in data.")
    rep(FALSE, nrow(acc_data))
  } else {
    purrr::pmap_lgl(acc_data[, nonvalid_var], ~any(...))
  }
}

#' @export
mutate_acc_nonvalid <- function(acc_data, ...) {
  rlang::enquos(...) %>%
    purrr::map2(as.character(rlang::get_expr(.)),
                ~rlang::eval_tidy(.x, data = acc_data) %>%
                  set_acc_attr_("nonvalid", expression = .y)) %>%
    {
      assert_that(all(purrr::map_lgl(., is.logical)),
                  msg = "Nonvalid expressions must evaluate to a logical vector.")
      mutate_acc_(acc_data, !!!.)
    }
}

