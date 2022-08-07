#' format_nicely2
#'
#' Formats a list of moderated meta3 objects into a data.frame which can be used as a table in a LaTex document
#' @param x a meta_list object
#' @param round a scalar.
#' @param transf a function. If provided will transform effects and confidence intervals.
#' @param effect_name a string. If provided, will rename Estimate column with string provided.
#' @param transf_name a character string. If provided, will name the transformed column.
#' @param hide_insig a bool.
#' @param escape_pc a bool. If TRUE, \% symbols will be escaped in header, captions and notes.
#' @param p_digits a scalar. The number of digits to round p to.
#' @param leading_zero a bool. If TRUE, p-values will have leading zeros
#' @param ci_sep separator for confidence intervals
#' @param include_i2 A bool, should i2 be included next to baseline?
#' @export format_nicely
#' @import data.table

format_nicely2 = function(x,
                          round = 2,
                          transf = function(x){x},
                          effect_name = NULL,
                          transf_name = NULL,
                          hide_insig = TRUE,
                          escape_pc = FALSE,
                          p_digits = 3,
                          leading_zero = FALSE,
                          ci_sep = ", ",
                          include_i2 = FALSE) {


}

# x0 <- meta3(drink_yi, drink_vi)

