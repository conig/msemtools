# library(msemtools)
#
#
# m <- meta3(drink_yi, drink_vi, study_id, data = conigrave20)
#
#
# m1 = mlm(m, ~character_matrix(Age))
# m2 = mlm(m , ~character_matrix(Age)-1)

mlm <- function(m, expr){
  expr = as.character(expr)
  expr = expr[expr != "~"]
  expr <- paste0("~ ", expr)

 call = m$call
  dat = call$data

  matrix_call <- list(formula = str2lang(expr),
       data = dat,
       na.action = na.pass)
  model_frame <- do.call(model.frame, matrix_call)
  matrx <- model.matrix(eval(parse(text = expr)), data = model_frame)
  includes_intercept = any(grepl("\\(Intercept\\)",colnames(matrx)))

  if(!includes_intercept){
    call$intercept.constraints = 0
  }else{
  matrx <- subset(matrx, select = -`(Intercept)`)
  }
  na.omit(rowSums(matrx))

  call$x = str2lang("matrx")

  m_out <- eval(call)

  out <- list(model = m_out,
       parameter_names = colnames(matrx),
       x = matrx)
  class(out) <- "metalm"
  out

}

print.metalm = function(x){
  print(x$model)
}

summary.metalm = function(x){
 out <- summary(x$model)
 rownames(out$coefficients)[grepl("Slope", rownames(out$coefficients))] = x$parameter_names
 out
}

`%~%` = function(lhs, rhs){

  lhs <- eval(substitute(lhs))
  rhs <- deparse(substitute(rhs))
  rhs = gsub("\\~","", rhs)

  mlm(m = lhs, expr = rhs)

}
