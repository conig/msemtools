# library(msemtools)
#
#
# m <- meta3(drink_yi, drink_vi, study_id, data = conigrave20)
#
#
# m1 = mlm(m, ~character_matrix(Age))
# m2 = mlm(m , ~character_matrix(Age)-1)

meta_matrix <- function(formula, data, intercept = FALSE, remove_varname = TRUE){
  matrix_call <- list(formula = str2lang(formula),
                      data = data,
                      na.action = na.pass)
  model_frame <- do.call(model.frame, matrix_call)

  matrx <- model.matrix(eval(parse(text = formula)), data = model_frame)
  if(!intercept & any(grepl("(Intercept)",colnames(matrx)))){
    matrx <- subset(matrx, select = -`(Intercept)`)
  }

  matrx
}

#' mlm
#'
#' Meta regression for meta3 objects
#' @param m meta3 object
#' @param formula formula passed to model.matrix
#' @param model.name String. Name your model (optional)
#' @export

mlm <- function(m, formula, model.name = NULL){
  formula = as.character(formula)[as.character(formula) != "~"]
  if (!grepl("^~", formula)) {
    formula <- paste0("~ ", formula)
  }

 call = m$call
  dat = call$data

  matrx_call <- call("meta_matrix", formula = formula, data = dat)
  matrx <- eval(call("meta_matrix", formula = formula, data = dat, intercept = TRUE))

  includes_intercept = any(grepl("\\(Intercept\\)",colnames(matrx)))

  if(!includes_intercept){
    call$intercept.constraints = 0
  }

  call$x = matrx_call
  call$model.name = model.name

  m_out <- eval(call)
  status <- m_out$mx.fit$output$status[[1]]
  if(!status %in% c(0,1)){
    m_out <- try_even_harder(m_out)
  }

  out <- list(model = m_out,
        anova = anova(m_out, m),
       parameter_names = colnames(matrx)[!colnames(matrx) %in% "(Intercept)"])
  class(out) <- "metalm"
  out

  }

#' print.metalm
#'
#' Print function for metalm

print.metalm = function(x){
  print(x$model)
}

#' summary.metalm
#'
#' Summary function for metalm
#' @export

summary.metalm = function(x, rename = TRUE){
 out <- summary(x$model)
 if(rename){
 rownames(out$coefficients)[grepl("Slope", rownames(out$coefficients))] = x$parameter_names
 }

out <- list(summary = out,
      anova = x$anova)

class(out) <- "mlm_summary"
out

}

#' print.mlm_summary
#'
#' Print function for mlm_summary
#' @export

print.mlm_summary = function(x){

  print(x$summary)

  cat("\n")
  cat(crayon::underline("ANOVA results"))
  cat("\n")
  x$anova$p <- round_p(x$anova$p)
  print(x$anova)
}

`%~%` = function(lhs, rhs){

  lhs <- eval(substitute(lhs))
  rhs <- deparse(substitute(rhs))
  rhs = gsub("\\~","", rhs)

  mlm(m = lhs, formula = rhs)

}
