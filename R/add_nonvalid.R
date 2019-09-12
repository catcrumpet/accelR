#' @export
add_nonvalid <- function(acc_data, ...) {
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
