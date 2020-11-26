#' PET_PEESE
#'
#' Calculates a PET-PEESEE adjusted estimate
#' @param x a metaSEM or meta_ninja model
#' @param transf function with which to transform results
#' @param digits number of digits for rounding
#' @param alpha the alpha threshold used in PET-PEESE. Defaults to 0.1 (10% as per Stanley 2017)
#' @param type Should PET-PEESE, PET or PEESE be used?
#' @details PET-PEESE uses meta-regression in order to adjust estimates for publication bias. When there is publication bias the sampling variance becomes correlated with effect size. By using the standard error (PET) or the sampling variance (PEESE) as moderators in a meta-regression, estimates (the intercept) can be made which partial out the correlation between effect size and variance. PET-PEESE first tests whether the variance component is a significant (p < 0.05) predictor of effect size. If it is, PEESE is used. Otherwise, PET is used.

PET_PEESE = function(m, transf = NULL, digits = 2, alpha = .1, type = c("PET-PEESE","PET","PEESE"), pattern = "{est} [95% CI {lower}, {upper}]"){
  if(is(m, "meta_ninja")){
  m <- m$models$Baseline
  }


 if(!is(m, "meta3")){
   stop("This function will only work with meta3 or meta_ninja objects as inputs")
 }

  type = type[1]
  types = c("PET-PEESE","PET","PEESE")

  if(!type %in% c("PET-PEESE","PET","PEESE")){
    stop("type must be one of: '", paste(types, collapse = "', '"), "'.")
  }
  call = m$call
  pet_call = call
  pet_call$x = str2lang(glue::glue("sqrt({call$v})"))

  peese_call = call
  peese_call$x = str2lang(glue::glue("{call$v}"))

  if(type == "PET"){
    adj_m <- eval(pet_call)
  }
  if(type == "PEESE"){
    adj_m <- eval(peese_call)
  }

  if(type == "PET-PEESE") {
    adj_m <- eval(pet_call)
    pet_p <- extractData(adj_m)$slope_p
    if (pet_p < alpha) {
      adj_m <- eval(peese_call)
    }
  }

  if(is.null(transf)){
    transf = function(x) x
  }

  results = extractData(adj_m)
  est <- papyr::digits(transf(results$estimate), digits)
  lower <- papyr::digits(transf(results$lbound),digits)
  upper <- papyr::digits(transf(results$ubound),digits)

  result = glue::glue("{est} [95% CI {lower}, {upper}]")
  result

}

#' prop_stronger
#'
#' Wrapper for MetaUtility prop_stronger

prop_stronger = function(m, q, transf = NULL, tail = "above",
                       R = 500, method = "calibrated"){

  requireNamespace(package = "MetaUtility", quietly = TRUE)

  if(is(m, "meta_ninja")){
    m <- m$models[[1]]
  }
  if(is.null(transf)) transf <- function(x) x

  info = extractData(m)
  est = transf(info$estimate)
  se = transf(info$SE)
  t2 = info$t2 + info$t2_3

  dat = tibble::tibble(eval(m$call$data))[, c(as.character(m$call$y), as.character(m$call$v))]
  names(dat) = c("yi","vi")
  dat$yi = transf(dat$yi)
  dat$vi = transf(dat$vi)
  dat <- na.omit(dat)
  prop_call <- list(
    q = q,
    M = est,
    se.M = se,
    #t2 = t2,
    dat = dat,
    tail = tail,
    estimate.method = method,
    ci.method = method,
    R = R
  )

  out <- suppressMessages(do.call(MetaUtility::prop_stronger, prop_call))

  out <- out[,c("est","se","lo","hi","bt.mn")]
  out$formatted = glue::glue("{papyr::digits(out$est * 100,1)}% [95% CI {papyr::digits(out$lo * 100,1)}, {papyr::digits(out$hi * 100,1)}]")
  out
}

