.onAttach <- function(libname, pkgname) {
  packageStartupMessage(
    "A ninja's swift gaze...
Dark stones, shrouded in concern...
leaves fall silently.
                            [-_-]~"
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
    data = "tbl"
  )
)

#' meta_ninja methods
#' @param x object to print
#' @param ... additional arguments. Not currently used.
#' @importFrom dplyr select %>% mutate
#' @export
print.meta_ninja = function(x, ...) {
  x = x$table %>%
    select(moderation, model.name, k, n, estimate, lbound, ubound, I2_2, I2_3, slope, slope_lbound, slope_ubound, R2_2, "anova p-value")
  print(x)
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
  format_nicely(object, ...)
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
  baseline_desc = describe_baseline(obj)
  moderator_desc = describe_moderators(obj)
  output = paste(baseline_desc, moderator_desc, collapse = " ")
  return(output)
}


#Define global variables
utils::globalVariables(
  c(
    ".",
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
    "level"
  )
)
