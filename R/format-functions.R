#' format_nicely
#'
#' @param x a meta_ninja
#' @param round a scalar.
#' @param transform a function. If provided will transform effects and confidence intervals.
#' @param effect.name a string. If provided, will rename Estimate column with string provided.
#' @param t.name a character string. If provided, will name the transformed column.
#' @param hide.insig a bool.
#' @param escape.pc a bool. If TRUE, \% symbols will be escaped in header, captions and notes.
#' @param p_digits a scalar. The number of digits to round p to.
#' @param leading.zero a bool. If TRUE, p-values will have leading zeros
#' @export format_nicely
#' @importFrom dplyr select rename
#' @importFrom papertools glue_bracket digits

#examples
#round = 2; transform = papertools::logit2prob; t.name = "Pr (95% CI)"; hide.insig = T
format_nicely = function(x,
                         round = 2,
                         effect.name = NULL,
                         transform = NULL,
                         t.name = NULL,
                         hide.insig = T,
                         escape.pc = F,
                         p_digits = 3,
                         leading.zero = FALSE) {
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

  # apply transformation
  if (!is.null(transform)) {
    if (is.null(t.name)) {
      t.name = deparse(args$transform)
    }
    df$extra = lapply(seq_along(unlist(df[,1])),function(i){
      papertools::glue_bracket(transform(df[i,]$estimate),
                               transform(df[i,]$lbound),
                               transform(df[i,]$ubound),
                               round = round , brackets = c("[","]"))

    }) %>% unlist
    df$extra[df$extra == papertools::glue_bracket(NA, NA, NA,brackets = c("[","]"))] = NA
    df = df %>% dplyr::select(
      moderation,
      Moderator= model.name,
      k,
      n,
      extra,
      Estimate = estimate,
      SE,
      I2_2,
      I2_3,
      R2_2,
      R2_3,
      "ANOVA p-value" = `anova p-value`
    )
    df$Estimate = papertools::digits(df$Estimate, round)
    names(df)[names(df) == "extra"] = t.name
  } else{
    df$estimate = lapply(seq_along(unlist(df[,1])),function(i){
      papertools::glue_bracket(transform(df[i,]$estimate),
                               transform(df[i,]$lbound),
                               transform(df[i,]$ubound),
                               round = round,
                               brackets = c("[","]"))

    }) %>% unlist
    df$estimate[df$estimate == papertools::glue_bracket(NA, NA, NA,brackets = c("[","]"))] = NA
    df = df %>% dplyr::select(
      moderation,
      Moderator = model.name,
      k,
      n,
      Estimate = estimate,
      SE,
      I2_2,
      I2_3,
      R2_2,
      R2_3 = R2_3,
      `ANOVA p-value` = `anova p-value`
    )
  }

  df$indent_ = duplicated(df$moderation)

  df$`ANOVA p-value` = df$`ANOVA p-value` %>%
    papertools::round_p(p_digits, stars= 0.05, leading.zero = F)

  df$Moderator[1] = paste0(df$Moderator[1], " (","$I^2_{(2;3)}$: ",papertools::digits(df$I2_2[1], round),"; ",papertools::digits(df$I2_3[1],round),")")
  df$I2_2 = NULL
  df$I2_3 = NULL
  df$R2_2 = papertools::digits(df$R2_2, round)
  df$R2_3 = papertools::digits(df$R2_3, round)
  df[is.na(df)] = "-"
  df[df == "NA"] = "-"
  df$k = as.character(df$k)
  df$n = as.character(df$n)
  df$moderation = NULL

  if(!is.null(effect.name)){
    names(df)[names(df) == "Estimate"] = effect.name
  }else{
    if(is.null(transform)){
    names(df)[names(df) == "Estimate"] = "Estimate (95% CI)"
    }
  }

  df = df %>%
    rename("$p$" = "ANOVA p-value",
           "$SE$" = SE,
           "$R^2_{(2)}$" = R2_2,
           "$R^2_{(3)}$" = R2_3)

  if (escape.pc) {
    names(df) = gsub("\\%", "\\\\%", names(df))
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

to_apa = function(x, caption, note,escape = F,
                  escape.pc = F,docx = T, ...){
  if("meta_ninja" %in% class(x)){
    x = format_nicely(x)
  }
# if(docx){
#   names(x)[names(x) %in% c("R2_2","R2_3")] = c("R^2^~(2)~","R^2^~(3)~")
#   #x$Moderator = gsub("\\(I2_2 =","(I^2^~(2)~ =",x$Moderator)
#   #x$Moderator = gsub("I2_3 =","I^2^~(3)~",x$Moderator)
#
# }else{
#   names(x)[names(x) %in% c("I2","R2")] = c("I\\textsuperscript{2}", "R\\textsuperscript{2}")
# }
  if (escape.pc) {
  names(x) = gsub("\\%", "\\\\%", names(x))
  caption = gsub("\\%", "\\\\%", caption)
  note = gsub("\\%", "\\\\%", note)
  }


  indents = x$indent_
  x$indent_ = NULL

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

describe_moderators = function(obj) {
  x = get(obj, envir = globalenv())
  sig_mods_table = x$table %>%
    filter(!type %in% c("Baseline", "factor level") &
             `anova p-value` < 0.05)
  if (nrow(sig_mods_table) > 0) {
  base_text = "The covariates which significantly moderated the baseline model were"
    list_text = lapply(seq_along(sig_mods_table$model.name), function(i) {
      mod_name = sig_mods_table$model.name[i] %>% tolower %>%  paste0("'", ., "'")
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

     paste0(mod_name, "(R^2^~(2)~ = ", R2_code, "%; R^2^~(3)~ = ", R2_3_code,"%)")
    }) %>% paste(collapse = ", ") %>% gsub(",(?!.*,)", ", and", ., perl = T)
    final_text = paste0(base_text, " ", list_text,".")
  }  else{
  final_text = "No covariates were found to be significant moderators of the baseline model."

  }
return(final_text)
}

#' describe_all_mods
#' @param obj the character names of an object

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

#' get_val
#'
#' Returns a value for models contained in meta_ninja objects
#'
#' @param x a meta_ninja object
#' @param value the value to extract
#' @param m the moderator to extract
#' @param digits the number of digits
#' @param transform a function to transform returned values.
#' @importFrom dplyr %>%
#' @export get_val

get_val = function(x, value, m = NULL, digits = Inf, transform = NULL){

  call = match.call()
  #return(call)

  x = eval(call$x)

  if(is.null(call$value)) call$value = names(x$table)

  if(class(x) != "meta_ninja") stop("get_model_value() only works for objects of class meta_ninja")

  output = x$table
  mods = get_moderators(x)

  if(!is.null(call$m)){
    if(!as.character(call$m) %in% mods$moderator) stop("moderator could not be found in the model table")

    output = output %>%
      filter(moderation == call$m)
    }

  values = tidyselect::vars_select(names(output), {{value}})

  output = c(output[1, values])
  if(length(unlist(output)) == 1) output <- as.numeric(unname(output))

  if(!is.null(transform)){
    output = transform(output)
  }

  if(!is.infinite(digits)){
  output = papertools::digits(output, n = digits)
  }

  return(output)

}

#' get_moderators
#'
#' return moderators and significe status
#' @importFrom dplyr %>%
#' @param meta_ninja a meta_ninja object
#' @param p p value with which to assess significance

get_moderators = function(meta_ninja, p = 0.05){
  call = match.call()

  m = meta_ninja$table %>%
    dplyr::select(moderation,R2_2,R2_3, `anova p-value`) %>%
    .[-1,] %>%
    filter(!duplicated(moderation)) %>%
    rename(moderator = moderation)

  m$sig = m$`anova p-value` < p

  return(m)
}




