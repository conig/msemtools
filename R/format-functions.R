#' format_nicely
#'
#' @param x a meta-ninja
#' @param round a scalar.
#' @param transform a function. If provided will transform effects and confidence intervals.
#' @param t_name a character string. If provided, will name the transformed column.
#' @export format_nicely
#' @importFrom dplyr select
#' @importFrom papertools glue_bracket digits

#examples
# round = 2; transform = papertools::logit2prob; t_name = "Pr (95% CI)"
format_nicely = function(x, round = 2, transform = NULL, t_name = NULL){
  args = as.list(match.call())
  df = x$table
  df$SE = papertools::digits(df$SE, round)

  if(!is.null(transform)){
    if(is.null(t_name)){
    t_name = deparse(args$transform)
    }

  df$extra = papertools::glue_bracket(transform(df$estimate), transform(df$lbound), transform(df$ubound), round = round)
  df$extra[df$extra == papertools::glue_bracket(NA,NA,NA)] = NA
  df = df %>% dplyr::select(
    indent = moderation, model.name, k, n, extra,estimate, SE, I2, R2 = R2_2, `anova p-value`
  )
  names(df)[names(df) == "extra"] = t_name

  }else{
  df$estimate = papertools::glue_bracket(df$estimate, df$lbound, df$ubound, round = round)
  df$estimate[df$estimate == papertools::glue_bracket(NA,NA,NA)] = NA
  df = df %>% dplyr::select(
    indent = moderation, model.name, k, n,estimate, SE, I2, R2 = R2_2, `anova p-value`
  )

    }

  df$indent = duplicated(df$indent)

  df %>% dplyr::select(estimate = estimate)
  df$`anova p-value` = ifelse(df$`anova p-value` < 0.001, "0.001", papertools::digits(df$`anova p-value`, 3))
return(df)
}
