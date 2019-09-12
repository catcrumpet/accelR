#' @export
add_valid <- function(acc_data, valid = "valid") {
  nonvalid_var <- name_acc_type_(acc_data, c("nonwear", "nonvalid"))

  acc_data %>%
    mutate_acc_(!!valid :=
                  !calculate_nonvalid_(acc_data) %>%
                  set_acc_attr_("valid", components = nonvalid_var))
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
  expr_list <- enquos(...)

  expr_names <- as.character(get_expr(expr_list))

  assert_that(all(!names(expr_list) %in% names(acc_data)),
              msg = "Variables already exist in original data.")

  output <- map(expr_list, eval_tidy, data = acc_data)

  assert_that(all(map_lgl(output, is.logical)),
              msg = "Nonvalid expressions must evaluate to a logical vector.")

  output %>%
    map2_dfc(expr_names,
             ~set_acc_attr_(.x,
                            "nonvalid",
                            expression = .y)) %>%
    {bind_cols(acc_data, .)}
}
