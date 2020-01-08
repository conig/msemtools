#reports



report_q = function(x, rmarkdown = FALSE, digits = 2){
call = match.call()
envir = sys.parent()
if(class(x) != "name") x <- call$x
 stat_q = rmarkdown_wrap(glue::glue('get_val({x}, "Q", digits = {digits})'), rmarkdown = rmarkdown, envir = envir)
 stat_df = rmarkdown_wrap(glue::glue('get_val({x}, "Q_df", digits = 0)'), rmarkdown = rmarkdown, envir = envir)
 stat_p = rmarkdown_wrap(glue::glue('papyr::round_p(get_val({x}, "Q_p"),3)'), rmarkdown = rmarkdown, envir = envir)

 if(stat_p < 0.05){
   mess = glue::glue("Inspecting the Q statistic revealed significant heterogeneity $Q$({stat_df}) = {stat_q}, $p$ {stat_p}.")
 }else{
 mess = glue::glue("Evidence for heterogeneity was not found $Q$({stat_df}) = {stat_q}, $p$ = {stat_p}.")
 }

 return(mess)

}

report_n = function(x, rmarkdown = FALSE){
  call = match.call()
  envir = sys.parent()
  #return(call)
  if(class(x) != "name") x <- call$x
  stat_k = rmarkdown_wrap(glue::glue('papyr::as_word(get_val({x}, k, digits = 0), T)'), rmarkdown = rmarkdown, envir = envir)
  stat_n = rmarkdown_wrap(glue::glue('get_val({x}, n, digits = 0)'), rmarkdown = rmarkdown, envir = envir)

    mess = glue::glue("{stat_k} studies ({stat_n} effects) reported data which could be pooled.")

  return(mess)

}

report_baseline = function(x, rmarkdown = FALSE, digits = 2, transform = NULL){
  call = match.call()
  envir = sys.parent()
  if(class(x) != "name") x <- call$x
  if(class(transform) != "call") transform <- call$transform

  stat_est = rmarkdown_wrap(glue::glue('get_val({x}, estimate, digits = {digits}, transform = {deparse(transform)})'), rmarkdown = rmarkdown, envir = envir)
  stat_lower = rmarkdown_wrap(glue::glue('get_val({x}, lbound, digits = {digits}, transform = {deparse(transform)})'), rmarkdown = rmarkdown, envir = envir)
  stat_upper = rmarkdown_wrap(glue::glue('get_val({x}, ubound, digits = {digits}, transform = {deparse(transform)})'), rmarkdown = rmarkdown, envir = envir)
  mess = glue::glue("The pooled effect size was {stat_est} [95% CI {stat_lower}, {stat_upper}].")
  return(mess)
}

report_i2 = function(x, rmarkdown = FALSE, digits = 2){
  call = match.call()
  envir = sys.parent()
  if(class(x) != "name") x <- call$x
  stat_i2_2 = rmarkdown_wrap(glue::glue('papyr::digits(get_val({x}, I2_2) * 100)'), rmarkdown = rmarkdown, envir = envir)
  stat_i2_3 = rmarkdown_wrap(glue::glue('papyr::digits(get_val({x}, I2_3) *100)'), rmarkdown = rmarkdown, envir = envir)
  mess = glue::glue("The heterogeneity at level 2 was {stat_i2_2}%. The heterogeneity at level 3 was {stat_i2_3}%.")
  return(mess)
}

report_moderators = function(x, rmarkdown = FALSE, digits = 2){
  call = match.call()
  #return(call)
  if(class(x) != "name") x <- call$x
  dat = eval(x)
  mods = data.table::data.table(get_moderators(dat))
  mods = mods[mods$sig == T,]
  envir = sys.parent()

  if(nrow(mods) == 0){
    return("No covariate was found to be a significant moderator of the baseline model.")
    }

  mods$r2_2.md = rmarkdown_wrap(glue::glue("papyr::digits(get_val({x}, R2_2, '{mods$moderator}')*100,{digits})"),rmarkdown, envir=envir)
  mods$r2_3.md = rmarkdown_wrap(glue::glue("papyr::digits(get_val({x}, R2_3, '{mods$moderator}')*100,{digits})"),rmarkdown, envir=envir)

  if (nrow(mods) == 1) {
    sent_mods = glue::glue(
      "'<<mods$moderator>>' ($R^2_{(2)}$ = <<mods$r2_2.md>>%; $R^2_{(3)}$ = <<mods$r2_3.md>>%)",
      .open = "<<",
      .close = ">>"
    )
  } else{
    sent_mods = papyr::c_sentence(
      glue::glue(
        "'<<mods$moderator>>' ($R^2_{(2)}$ = <<mods$r2_2.md>>%; $R^2_{(3)}$ = <<mods$r2_3.md>>%)",
        .open = "<<",
        .close = ">>"
      )
    )
  }

  if(nrow(mods) ==1){
    mess = glue::glue("The covariate which significantly moderated the baseline model was {sent_mods}.")
  }


  if(nrow(mods) > 1){
  mess = glue::glue("The covariates which significantly moderated the baseline model were {sent_mods}.")
  }
return(mess)

}

#' report
#'
#' Constructs written reports about the contents of models
#' @param meta_ninja the meta_ninja object
#' @param ... things to report. One o
#' @param rmarkdown return results in rmarkdown?
#' @param digits the number of digits to return
#' @param transform you can supply a function to transform baseline pooled estimates
#' @export report

report = function(meta_ninja,..., rmarkdown = T, digits = 2, transform = NULL){
call = match.call()

elip = sapply(substitute(list(...)),deparse)[-1]
options = c("n","q","baseline","i2","moderators")
filt = tidyselect::vars_select(options, elip)
if(length(filt) == 0) filt = options

mess = list(
  n = report_n(call$meta_ninja, rmarkdown = rmarkdown),
  q = report_q(call$meta_ninja, rmarkdown = rmarkdown, digits = 2),
  baseline = report_baseline(
    call$meta_ninja,
    rmarkdown = rmarkdown,
    digits = 2,
    transform = call$transform
  ),
  i2 = report_i2(call$meta_ninja, rmarkdown = rmarkdown, digits = 2),
  moderators = report_moderators(call$meta_ninja, rmarkdown = rmarkdown, digits = 2)
)

mess = paste(mess[filt], collapse = " ")


if(rmarkdown){
  return(cat(mess))
}else{
  return(mess)
}

}



rmarkdown_wrap = function(code, rmarkdown = FALSE, envir = sys.parent()){
call = match.call()

  if(!rmarkdown){

    if(length(code) >1){

      return(unlist(lapply(seq_along(code), function(i) eval(parse(text = code[[i]]), envir = envir))))
    }else{
    return(eval(parse(text = code), envir = envir))
    }

  }else{
    return(glue::glue('`r {code}`'))

  }

}
