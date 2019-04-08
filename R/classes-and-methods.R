


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

  "Moderation results:\n\n" %>%
    crayon::underline() %>%
    cat

  "I2(2): " %>%
    paste0(papertools::digits(x$table$I2_2[1]*100,1), "%") %>%
    cat()

  cat("\n")

  "I2(3): " %>%
    paste0(papertools::digits(x$table$I2_3[1]*100,1), "%") %>%
    cat()

  cat("\n")
  cat("\n")

  out = x$table %>%
    select(moderation, k, n,R2_2,R2_3,`p-value` = "anova p-value", type, Mx_status) %>%
    filter(type == "moderator") %>% data.frame
  out[,4:5] = round(out[,4:5],2)
  out$p.value = papertools::round_p(out$p.value, stars = 0.05)

  problem_models = out$moderation[!out$Mx_status %in% c(0,1)]

  out = out %>%
    select(-type, - Mx_status)

  print(out)

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
    removed_moderator_message = paste0(length(removed_moderators) %>% papertools::as_word(T),
                                       " moderators were removed due to having no variance:\n",
                                       paste(removed_moderators, collapse = ", "),".")
    cat(removed_moderator_message)
  }

}

#' meta_ninja plot method
#' @param x model to print
#' @param y not used.
#' @param ... additional arguments passed to ninjaForest.
#' @export
plot.meta_ninja = function(x, y, ...) {
  ninjaForest(x, ...)
}

#' meta_ninja summary method
#' @param object model to summarise
#' @param ... additional arguments passed to format_nicely
#' @export
summary.meta_ninja = function(object, ...) {
  out = format_nicely(object, ...) %>%
    select(-indent_)
  out$Moderator[1] = "Baseline"
  print(out, n = 100)
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
    "predictor_matricies"
  )
)
