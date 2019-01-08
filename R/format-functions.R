#' format_nicely
#'
#' @param x a meta_ninja
#' @param round a scalar.
#' @param transform a function. If provided will transform effects and confidence intervals.
#' @param t_name a character string. If provided, will name the transformed column.
#' @export format_nicely
#' @importFrom dplyr select
#' @importFrom papertools glue_bracket digits

#examples
# round = 2; transform = logit2prob; t_name = "Pr (95% CI)"
format_nicely = function(x,
                         round = 2,
                         transform = NULL,
                         t_name = NULL) {
  if (!"meta_ninja" %in% class(x)) {
    stop(
      "'format_nicely' only works with objects of class meta_ninja. See Fn meta3_moderation",
      call. = F
    )
  }
  args = as.list(match.call())
  df = x$table
  df$SE = papertools::digits(df$SE, round)

  if (!is.null(transform)) {
    if (is.null(t_name)) {
      t_name = deparse(args$transform)
    }
    df$extra = papertools::glue_bracket(transform(df$estimate),
                                        transform(df$lbound),
                                        transform(df$ubound),
                                        round = round)
    df$extra[df$extra == papertools::glue_bracket(NA, NA, NA)] = NA
    df = df %>% dplyr::select(
      indent = moderation,
      Moderator= model.name,
      k,
      n,
      extra,
      Estimate = estimate,
      SE,
      I2,
      R2 = R2_2,
      "ANOVA p-value" = `anova p-value`
    )
    df$Estimate = papertools::digits(df$Estimate, round)
    names(df)[names(df) == "extra"] = t_name
  } else{
    df$estimate = papertools::glue_bracket(df$estimate, df$lbound, df$ubound, round = round)
    df$estimate[df$estimate == papertools::glue_bracket(NA, NA, NA)] = NA
    df = df %>% dplyr::select(
      indent = moderation,
      Moderator = model.name,
      k,
      n,
      Estimate = estimate,
      SE,
      I2,
      R2 = R2_2,
      `ANOVA p-value` = `anova p-value`
    )
  }
  df$indent = duplicated(df$indent)

  df$`ANOVA p-value` = ifelse(df$`ANOVA p-value` < 0.001,
                              "0.001",
                              papertools::digits(df$`ANOVA p-value`, 3))
  df$I2 = digits(df$I2, round)
  df$R2 = digits(df$R2, round)
  df[is.na(df)] = "-"
  df[df == "NA"] = "-"
  df$k = as.character(df$k)
  df$n = as.character(df$n)
  return(df)
}
