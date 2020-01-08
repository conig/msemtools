


.onAttach <- function(libname, pkgname) {
  m = get_haiku()
  packageStartupMessage(
#     "A ninja's swift gaze...
# Dark stones, shrouded in concern...
# leaves fall silently.
#                             [-_-]~"
    m
  )
}
#Define classes
setClass(
  "meta_ninja",
  representation(
    models = "list",
    table = "list",
    cluster = "character",
    covariates = "list",
    calls = "list",
    data = "tbl",
    removed_moderators = "list"
  )
)

#' meta_ninja methods
#' @param x object to print
#' @param ... additional arguments. Not currently used.
#' @importFrom dplyr select %>% mutate
#' @export
print.meta_ninja = function(x, ...) {

  "Moderation results:\n" %>%
    crayon::underline() %>%
    cat

  cat("\n")

  "I2(2): " %>%
    paste0(papyr::digits(x$table$I2_2[1]*100,1), "%") %>%
    cat()

  cat("\n")

  "I2(3): " %>%
    paste0(papyr::digits(x$table$I2_3[1]*100,1), "%") %>%
    cat()

  cat("\n")

  out = x$table %>%
    select(moderation, k, n,R2_2,R2_3,`p-value` = "anova p-value", type, Mx_status) %>%
    filter(type == "moderator") %>% data.frame
  out[,4:5] = round(out[,4:5],2)
  out$p.value = papyr::round_p(out$p.value, stars = 0.05) %>%
    lapply(function(i){
      if(!grepl("\\*",i)){
        i = paste0(i," ") #if no star, add a space to keep things nicely lined up
      }
      return(i)
    }) %>% unlist

  problem_models = out$moderation[!out$Mx_status %in% c(0,1)]

  out = out %>%
    select(-type, - Mx_status) %>%
    utils::capture.output()

  header = out[1]
  width = max(nchar(out))
  bar = paste(rep(crayon::silver("-"), width),collapse = "")
  text = gsub("\\*", crayon::yellow("*"),out[-1])

  tab = paste(c(bar,
        header,
        bar,
        text,
        bar), collapse = "\n")

  cat(tab)
  cat("\n")

  if(length(problem_models) > 0){
    mx_message = paste0("Did not converge: " ,paste(problem_models, collapse = ", "),".") %>%
      crayon::red()
  } else {
    mx_message = crayon::cyan("All models converged.")
  }

  cat(mx_message)

  removed_moderators = names(x$removed_moderators)[x$removed_moderators]

  cat("\n\n")
  if(length(removed_moderators) > 0){
    removed_moderator_message = paste0(length(removed_moderators) %>% papyr::as_word(T),
                                       " moderator(s) were removed due to no variance:\n",
                                       paste(removed_moderators, collapse = ", "),".") %>%
      crayon::red()
    cat(removed_moderator_message)
  }

}

#' removed_mod_note
#'
#' Generates text to describe the removal of moderators
#' @param x a meta_ninja model
#' @param capitalise_first A bool. Set to true if you want to capitalise the first word
#' @param case one of "default", "lower" or "sentence"
#' @export removed_mod_note
#' @importFrom dplyr %>%

#TODO integrate with print metaninja
removed_mod_note = function(x,
                            capitalise_first = T,
                            case = c("default", "lower", "sentence")) {
  case = case[1]
  removed_moderators = names(x$removed_moderators)[x$removed_moderators]

  removed_n = length(removed_moderators)

  #number of moderators as a word
  number_w = paste0(length(removed_moderators) %>% papyr::as_word(capitalise_first))

  #get moderator word grammar
  if (removed_n > 1) {
    mods_w <- "moderators were"

  } else{
    mods_w <- "moderator was"
  }

  moderators_sentence = removed_moderators %>%
    paste(collapse = ", ") %>%
    gsub(",([^,]*)$", ", and\\1", .)


  out =  glue::glue("{number_w} {mods_w} removed due to no variance: {moderators_sentence}") %>% as.character

  if (!case %in% c("lower", "default", "sentence"))
    stop("case must be one of 'default', 'lower' or 'sentence'")

  if (case == "lower") {
    out <- stringr::str_to_lower(out)
  }

  if (case == "sentence") {
    out <- stringr::str_to_sentence(out)
  }

  if (removed_n == 0) {
    return("")
  } else{
    return(out)
  }

}



#' meta_ninja plot method
#' @param x model to print
#' @param y not used.
#' @param ... additional arguments passed to ninjaForest.
#' @export
plot.meta_ninja = function(x, y, ...) {
  forest_plot(x, ...)
}

#' meta_ninja summary method
#' @param object model to summarise
#' @param ... additional arguments passed to format_nicely
#' @export
summary.meta_ninja = function(object, ...) {
  out = format_nicely(object, ...)
  out$Moderator[1] = "Baseline"
  out$Moderator = ifelse(out$indent_, paste0('__',out$Moderator), out$Moderator)
  out = out %>%
    select(- indent_)

}

#' meta_ninja as.data.frame method
#' @param x an R object
#' @param row.names NULL or a character vector
#' @param optional Rubbish
#' @param ... extra arguments
#' @export
as.data.frame.meta_ninja = function(x,
                                    row.names = NULL,
                                    optional = NULL,
                                    ...) {
  data.frame(x$table, row.names = row.names)
}

#' meta_ninja as.character method
#' @param x a meta_ninja
#' @param ... extra arguments
#' @export
as.character.meta_ninja = function(x, ...) {
  obj = match.call()
  obj = obj$x %>% deparse
  q_desc = describe_q(obj)
  baseline_desc = describe_baseline(obj)
  moderator_desc = describe_moderators(obj)
  output = paste(q_desc,baseline_desc, moderator_desc, collapse = " ")
  return(output)
}

#' coef.meta_ninja
#' @param object the meta_ninja
#' @param ... specific moderators to extract
#' @importFrom stats coef
#' @export

coef.meta_ninja = function(object,...){
  model = object
  # summary ---------------------------
  tab = model$table
  tab = tidyr::fill(tab, `anova p-value`)
  tab = tab[model$table$type != "moderator",]
  tab$setting = "Pooled"
  tab$order = seq_along(tab$moderation)

  mods = c("Baseline",tidyselect::vars_select(unique(tab$moderation),...))

  tab = tab[tab$moderation %in% mods, ]

  tab$model.name[tab$year == 1] = "Pooled estimate"

  summary_final = dplyr::select(
    tab,
    moderation,
    cluster = model.name,
    k,
    SE,
    n,
    est = estimate,
    lower = lbound,
    upper = ubound,
    type = type,
    setting = setting,
    model_p = `anova p-value`
  )

  eff_final = dplyr::select(get_effects(model$models[[1]]),
                      moderation,
                      cluster,
                      k,
                      n,
                      est,
                      SE,
                      lower,
                      upper,
                      type,
                      setting
  )
  eff_final$model_p = NA

  out = data.frame(rbind(summary_final, eff_final))
  out$order = seq_along(out$moderation)
  rownames(out) = NULL
return(out)
}

get_effects = function(model, baseline = FALSE) {
  eff = model$data
  eff$SE = sqrt(eff$v)
  eff$lower = eff$y - 1.96 * eff$SE
  eff$upper = eff$y + 1.96 * eff$SE
  eff$est = eff$y
  eff$moderation = NA
  eff$k = NA
  eff$n = 1
  eff$type = "effect size"
  eff$setting = "Effect sizes"
  eff = eff[, c("moderation",
                "cluster",
                "k",
                "n",
                "est",
                "SE",
                "lower",
                "upper",
                "type",
                "setting")]

  if (baseline) {
    b_mod <- extractData(model)
    b_mod$moderation = "Baseline"
    b_mod$cluster = "Baseline"
    b_mod$type = "Baseline"
    b_mod$setting = "Pooled"
    b_mod = dplyr::select(
      b_mod,
      moderation,
      cluster,
      k,
      n,
      est = estimate,
      SE,
      lower = lbound,
      upper = ubound,
      type,
      setting
    )
    eff = data.frame(rbind(b_mod, eff))
  }


  return(eff)

}


#Define global variables
utils::globalVariables(
  c(
    ".",
    "x",
    "I2_2",
    "I2_3",
    "R2_2",
    "R2_3",
    "SE",
    "anova",
    "estimate",
    "k",
    "n" ,
    "lbound",
    "ubound",
    "slope",
    "slope_ubound",
    "result",
    "model.name",
    "moderation",
    "slope_lbound",
    "x_internal",
    "y_internal",
    "v_internal",
    "cluster_internal",
    "representation",
    "y",
    "v",
    "type",
    "se",
    "lower",
    "upper",
    "setting",
    "est",
    "aes",
    "Mx_status",
    "extra",
    "anova p-value",
    "slope_se",
    "level",
    "name",
    "stde",
    "anova.p.value",
    "original_x",
    "predictor_matricies",
    "indent_",
    "cluster",
    "mod_data",
    "Q",
    "Q_df",
    "Q_p",
    "value",
    "sig"
  )
)

#' haiku print
#' @param x object to print
#' @param ... additional arguments. Not currently used.
#' @export
print.haiku = function(x, ...) {
cat(x)
}
