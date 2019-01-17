#' format_nicely
#'
#' @param x a meta_ninja
#' @param round a scalar.
#' @param transform a function. If provided will transform effects and confidence intervals.
#' @param effect.name a string. If provided, will rename Estimate column with string provided.
#' @param t.name a character string. If provided, will name the transformed column.
#' @param hide.insig a bool.
#' @export format_nicely
#' @importFrom dplyr select rename
#' @importFrom papertools glue_bracket digits

#examples
#round = 2; transform = NULL; t.name = "Pr (95% CI)"; hide.insig = F
format_nicely = function(x,
                         round = 2,
                         effect.name = NULL,
                         transform = NULL,
                         t.name = NULL,
                         hide.insig = T) {
  if (!"meta_ninja" %in% class(x)) {
    stop(
      "'format_nicely' only works with objects of class meta_ninja. See Fn meta3_moderation",
      call. = F
    )
  }
  args = as.list(match.call())
  base_table = x$table
  df = base_table
  df$SE = papertools::digits(as.numeric(as.character(df$SE)), round)
  df$"Slope (95% CI)" = NA
  df$"Slope SE" = NA

  if (hide.insig) { #this code chunk could be improved, was painful to write.
    mods = unique(df$moderation)
    for (i in seq_along(mods)) {
      #message(i)

      ps = df %>%
        dplyr::filter(moderation == mods[i]) %>%
        dplyr::select("anova p-value")
      if (any(!is.na(ps))) {
        p =  ps %>%
          max(na.rm = T)
        if (p >= 0.05) {
          rows = which(!(df$moderation == mods[i] &
                           duplicated(df$moderation)))
          df = df[rows, ] #remove factor levels from the current moderation
          base_table = base_table[rows,]


        }
      }
    }
  }




  if (!is.null(transform)) {
    if (is.null(t.name)) {
      t.name = deparse(args$transform)
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
      "Slope (95% CI)",
      "Slope SE",
      I2,
      R2 = R2_2,
      "ANOVA p-value" = `anova p-value`
    )
    df$Estimate = papertools::digits(df$Estimate, round)
    names(df)[names(df) == "extra"] = t.name
  } else{
    df$estimate = papertools::glue_bracket(df$estimate, df$lbound, df$ubound, round = round)
    df$estimate[df$estimate == papertools::glue_bracket(NA, NA, NA)] = NA
    df = df %>% dplyr::select(
      indent = moderation,
      moderation,
      Moderator = model.name,
      k,
      n,
      Estimate = estimate,
      SE,
      "Slope (95% CI)",
      "Slope SE",
      I2,
      R2 = R2_2,
      `ANOVA p-value` = `anova p-value`
    )
  }

  if(any(!is.na(base_table$slope))){
    df$`Slope (95% CI)` = papertools::glue_bracket(base_table$slope, base_table$slope_lbound, base_table$slope_ubound, round = round)
    df$`Slope SE` = papertools::digits(base_table$slope_se, round)
    df$`Slope (95% CI)`[df$`Slope (95% CI)` == papertools::glue_bracket(NA, NA, NA)] = NA
  }else{
      df$`Slope (95% CI)` = NULL
      df$`Slope SE` = NULL
    }

  df$indent = duplicated(df$indent)

  df$`ANOVA p-value` = lapply(df$`ANOVA p-value`, function(i) { #add in sig stars and round p-value
    if (is.na(i)) {
      return(NA)
    } else{
      if (i < 0.001) {
        return("< 0.001*")
      } else{
        if (i < 0.05) {
          return(paste0(papertools::digits(i, 3), "*"))
        } else {
          return(papertools::digits(i, 3))
        }
      }
    }
  }) %>% unlist

  df$I2 = digits(df$I2, round)
  df$R2 = digits(df$R2, round)
  df[is.na(df)] = "-"
  df[df == "NA"] = "-"
  df$k = as.character(df$k)
  df$n = as.character(df$n)
  df = df %>%
    rename("p-value" = "ANOVA p-value")

  if(!is.null(effect.name)){
    names(df)[names(df) == "Estimate"] = effect.name
  }else{
    if(is.null(transform)){
    names(df)[names(df) == "Estimate"] = "Estimate (95% CI)"
    }
  }
  return(df)

}

#' to_apa
#'
#' @param x a pretty_ninja
#' @param caption a character.
#' @param note a charcter.
#' @param escape a bool
#' @param ... additional functions can be deliverd to papaja::apa_table
#' @importFrom papaja apa_table
#' @importFrom dplyr %>%
#' @export to_apa

to_apa = function(x, caption, note, escape = F, ...){
  if("meta_ninja" %in% class(x)){
    x = format_nicely(x)
  }

  names(x)[names(x) %in% c("I2","R2")] = c("I^2^", "R^2^")

  indents = x$indent
  x = x[-1]

  papaja::apa_table(x,
                    caption = caption,
                    note = note,
                    stub_indents = list(indents),
                    escape = F,...
  )
}
