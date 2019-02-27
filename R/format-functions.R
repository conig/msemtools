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
#round = 2; transform = NULL; t.name = "Pr (95% CI)"; hide.insig = T
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
  args = as.list(match.call()) #I capture the call here, to give t_name a name if necessary.
  base_table = x$table
  df = base_table
  df$SE = papertools::digits(as.numeric(as.character(df$SE)), round)
  df$"Slope (95% CI)" = NA
  df$"Slope SE" = NA

  if (hide.insig) { #this code chunk could be improved, was painful to write.
    mods = unique(df$moderation) #what are the mods?
    for (i in seq_along(mods)) {
      #message(i)

      ps = df %>%
        dplyr::filter(moderation == mods[i]) %>%
        dplyr::select("anova p-value")
      if (any(!is.na(ps))) {
        p =  ps %>%
          max(na.rm = T)
        if (p >= 0.05) {
          rows = which(!(df$moderation == mods[i] & #rows to keep
                           duplicated(df$moderation)))
          df = df[rows, ] #remove factor levels from the current moderation
          base_table = base_table[rows,]


        }
      }else { #all ps were NAs
        rows = which(!(df$moderation == mods[i] & duplicated(df$moderation))) #rows to keep
        df = df[rows,]
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
      I2_2,
      I2_3,
      R2_2,
      R2_3 = R2_3,
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
      I2_2,
      I2_3,
      R2_2,
      R2_3 = R2_3,
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
      if (i < 0.01) {
        return("< 0.01*")
      } else{
        if (i < 0.05) {
          return(paste0(papertools::digits(i, 2), "*"))
        } else {
          return(papertools::digits(i, 2))
        }
      }
    }
  }) %>% unlist

  df$Moderator[1] = paste0(df$Moderator[1], " (","I^2^~(2;3)~: ",digits(df$I2_2[1], round),"; ",digits(df$I2_3[1],round),")")
  df$I2_2 = NULL
  df$I2_3 = NULL
  df$R2_2 = digits(df$R2_2, round)
  df$R2_3 = digits(df$R2_3, round)
  df[is.na(df)] = "-"
  df[df == "NA"] = "-"
  df$k = as.character(df$k)
  df$n = as.character(df$n)
  df = df %>%
    rename("p" = "ANOVA p-value")

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
#' @param escape a bool. Sent to papaja::apa_table, defaults to false.
#' @param escape.pc a bool. If True, \% symbols will be escaped in header, captions and notes.
#' @param docx a bool. If True, formats table superscripts for docx, otherwise for pdf.
#' @param ... additional functions can be deliverd to papaja::apa_table
#' @importFrom papaja apa_table
#' @importFrom dplyr %>%
#' @export to_apa

to_apa = function(x, caption, note,escape = F, escape.pc = T,docx = T, ...){
  if("meta_ninja" %in% class(x)){
    x = format_nicely(x)
  }
if(docx){
  names(x)[names(x) %in% c("R2_2","R2_3")] = c("R^2^~(2)~","R^2^~(3)~")
  #x$Moderator = gsub("\\(I2_2 =","(I^2^~(2)~ =",x$Moderator)
  #x$Moderator = gsub("I2_3 =","I^2^~(3)~",x$Moderator)

}else{
  names(x)[names(x) %in% c("I2","R2")] = c("I\\textsuperscript{2}", "R\\textsuperscript{2}")
}

  if (escape.pc) {
    names(x) = gsub("\\%", "\\\\%", names(x))
    caption = gsub("\\%", "\\\\%", caption)
    note = gsub("\\%", "\\\\%", note)
  }

  indents = x$indent
  x = x[-1]

  papaja::apa_table(x,
                    caption = caption,
                    note = note,
                    stub_indents = list(indents),
                    escape = escape,...
  )
}

default_note = function(){
  note = "k = number of studies; n = numbers of effect sizes; Estimate = population average; SE = standard error;  I^2^~(2,3)~ = Heterogeneity at level two and three, respectively; R^2^~(2)~ = the proportion of within-cluster heterogeneity explained by the covariate; R^2^~(3)~ = the proportion of between-cluster heterogeneity explained by the covariate; p-value = ANOVA p-value; * indicates p < 0.05"
  return(note)
}



#describing models

#' describe_baseline
#' @param obj a meta_ninja
#' @importFrom dplyr %>%

describe_q = function(obj){
  meta_ninja = get(obj)
  q_info = meta_ninja$models$Baseline %>%
    summary %>%
    .$Q.stat
  if(q_info$pval < 0.05){
    starting_message = "Inspecting the Q statistic revealed significant heterogeneity"
  } else{
    starting_message = "Inspecting the Q statistic did not reveal significant heterogeneity"
  }
  q = paste0("`r summary(",obj,"$models$Baseline)$Q.stat$Q %>% papertools::digits(2)`")
  df = paste0("`r summary(",obj,"$models$Baseline)$Q.stat$Q.df`")
  p = paste0("`r summary(",obj,"$models$Baseline)$Q.stat$pval %>% papertools::round_p(2)`")
  stats_text = paste0(" (Q(df = ",df,") = ",q, ", *p* = ", p,").")
  paste0(starting_message, stats_text)
  }



#describing models

#' describe_baseline
#' @param obj a meta_ninja
#' @importFrom dplyr %>%

describe_baseline = function(obj) {
  studies = paste0("`r ", obj, "$table$k[1] %>% papertools::as_word(T)`")
  effects = paste0("`r ", obj, "$table$n[1] %>% papertools::as_word(F)`")
  pooled = paste0(
    "`r papertools::glue_bracket(",
    obj,
    "$table$estimate[1],",
    obj,
    "$table$lbound[1],",
    obj,
    "$table$ubound[1])`"
  )
  i2_2 = paste0("`r ",
                obj,
                "$table$I2_2[1] %>% '*'(100) %>% papertools::digits(2)`")
  i2_3 = paste0("`r ",
                obj,
                "$table$I2_3[1] %>% '*'(100) %>% papertools::digits(2)`")

  message = paste0(
    studies,
    " studies (",
    effects,
    " effects) presented data which could be pooled. The estimated population average and 95% Wald CI were ",
    pooled,
    ". The heterogeneity at level 2 was ",
    i2_2,
    "%. The heterogeneity at level 3 was ",
    i2_3,
    "%."
  )
  return(message)
}

#' describe_moderators
#' @param obj a meta_ninja
#' @importFrom Hmisc capitalize
#' @importFrom dplyr filter %>%

describe_moderators = function(obj){
  base_text = "The covariates which significantly moderated the baseline model were"
  x = get(obj, envir = globalenv())
  sig_mods_table = x$table %>%
    filter(!type %in% c("Baseline", "factor level") &
             `anova p-value` < 0.05)
  if(nrow(sig_mods_table) > 0){
  sig_mods = sig_mods_table %>% select(model.name) %>% unlist %>% tolower %>%  paste(collapse = "', '")
  sig_mods = paste0("'", sig_mods, "'") %>%
    gsub(",(?!.*,)", " and", ., perl = T)
  first_phrase = paste0(base_text, " ", sig_mods, ".")
  }else{
    first_phrase = "No covariates were found to be significant moderators of the baseline model."
  }


  list_text = lapply(seq_along(sig_mods_table$model.name), function(i) {
    mod_name = sig_mods_table$model.name[i] %>% tolower %>% Hmisc::capitalize()
    R2_code = paste0(
      "`r ",
      obj,
      "$table %>% filter(model.name == ",
      "'",
      sig_mods_table$model.name[i],
      "'",
      ") %>% select(R2_2) %>% '*'(100) %>% papertools::digits(2)`"
    )
    R2_3_code = paste0(
      "`r ",
      obj,
      "$table %>% filter(model.name == ",
      "'",
      sig_mods_table$model.name[i],
      "'",
      ") %>% select(R2_3) %>% '*'(100) %>% papertools::digits(2)`"
    )

    paste0(
      mod_name,
      " explained ",
      R2_code,
      "% of heterogeneity within clusters, and ",
      R2_3_code,
      "% between clusters."
    )
  })

  moderation_text = paste(list_text, collapse = " ")

  final_text = paste0(first_phrase, " ", moderation_text)
  return(final_text)
}

#' describe_all_mods
#' @param obj the charcater name of an object
#' @export describe_all_mods

describe_all_mods = function(obj) {
  obj = match.call()
  obj = obj$obj %>% deparse

  x = get(obj, envir = globalenv())
  models = x$table %>%
    filter(type == "factor") %>%
    select(model.name) %>% unlist %>%
    as.character

  lapply(models, function(i) {
    anova_string = paste0("anova(",
                          obj,
                          "$models$",
                          "'",
                          i,
                          "'",
                          ", ",obj,"$models$Baseline)")

    diffll = paste0("`r ",
                    anova_string,
                    "$diffLL[2] %>% papertools::digits(2)`")
    df =  paste0("`r ", anova_string, "$diffdf[2]", "`")
    pval = paste0("`r ", anova_string, "$p[2] %>% papertools::round_p(2)", "`")
    real_p = eval(parse(text = paste0(anova_string, "$p[2]")))



    if (real_p < 0.05) {
      eval_message = " moderated the baseline model"
    } else{
      eval_message = " did not moderate the baseline model "
    }

    paste0(
      i %>% Hmisc::capitalize(),
      eval_message,
      "($\\bigtriangleup$$\\chi$^2^(",
      df,
      ") = ",
      diffll,
      ", *p* = ",
      pval,
      ")."
    )

  }) %>% paste(collapse = " ")


}


