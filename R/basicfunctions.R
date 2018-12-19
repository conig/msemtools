#' extractData
#'
#' Grabs pertinent data from a metaSEM model
#' @param model a numeric.
#' @importFrom dplyr %>%
#' @importFrom tibble rownames_to_column as_tibble
#' @export extractData

extractData = function(model){
  summary = summary(model)
  coef = summary$coefficients %>% data.frame %>% rownames_to_column
  names(coef) = c("row","est","stde","lbound","ubound","z","p")
  names = c("name","k","n","estimate","SE","lbound","ubound","t2","t2p","I2","t2_3","t2_3p",
            "I2_3","Q","Q_p","slope","slope_lbound","slope_ubound","R2_2","R2_3","Mx_status")
  result = data.frame(matrix(ncol = length(names),nrow = 1))
  names(result) = names
  result$k = summary$no.studies
  result$n = summary$obsStat
  result$estimate = coef[coef$row == "Intercept","est"]
  result$SE = coef[coef$row == "Intercept","stde"]
  result$lbound = coef[coef$row == "Intercept","lbound"]
  result$ubound = coef[coef$row == "Intercept","ubound"]
  result$t2 = coef[coef$row == "Tau2_2","est"]
  result$t2p = coef[coef$row == "Tau2_2","p"]
  result$t2_3 = coef[coef$row == "Tau2_3","est"]
  result$t2_3p = coef[coef$row == "Tau2_3","p"]
  result$Q = summary$Q.stat$Q
  result$Q_p = summary$Q.stat$pval
  if(!all(is.na(summary$I2.values))){
  result$I2 = summary$I2.values[1,2]
  result$I2_3 = summary$I2.values[2,2]
  }else{
    result$I2 = NA
    result$I2_3 = NA
  }

  if(!all(is.na(summary$R2.values))){
    result$R2_2 = summary$R2.values[3,1]
    result$R2_3 = summary$R2.values[3,2]
  }
  if(!is.na(coef[coef$row == "Tau2_2","p"])){
    if(coef[coef$row == "Tau2_2","p"] < 0.05){
      result$t2 = paste(result$t2, "*",sep = "")
    }}
  if(!is.na(coef[coef$row == "Tau2_3","p"])){
    if(coef[coef$row == "Tau2_3","p"] < 0.05){
      result$t2_3 = paste(result$t2_3, "*",sep = "")
    }}
  slopes = sum(grepl("Slope",coef$row))

  if(slopes > 0 & slopes < 2){
    result$slope = coef[coef$row == "Slope_1","est"]
    result$slope_lbound = coef[coef$row == "Slope_1","lbound"]
    result$slope_ubound = coef[coef$row == "Slope_1","ubound"]

  }else{
    result$slope = NA
    result$slope_lbound = NA
    result$slope_ubound = NA
  }

 if(is.null(summary$call$model.name)){
   result$name = NA
 }else{
   result$name = as.character(summary$call$model.name)
 }
result$Mx_stats = summary$Mx.status1

return(as_tibble(result))
}

#' string_meta3
#'
#' Allows three-level meta-analyses to be specified with strings
#' @param y a string. The name of the effect size variable
#' @param v a string. The name of the sampling variance variable
#' @param cluster a string. The name of the cluster variable
#' @param x a string.
#' @param data an object. data object
#' @param model.name a string. The name of the model
#' @importFrom metaSEM meta3 rerun
#'
#' @export string_meta3

#data = f ; y = "drink_yi"; v = "drink_vi";factor = "gender_cat"; names = NULL; cluster = "study_id"; x = "females_p"; model.name = "test"

string_meta3 = function(y, v, cluster, x = NULL, data, model.name = NA) {

  if (is.null(x)) {
    model = metaSEM::meta3(
      y = eval(parse(text = y)),
      v = eval(parse(text = v)),
      cluster = eval(parse(text = cluster)),
      data = data
    )
  } else{
    model = metaSEM::meta3(
      y = eval(parse(text = y)),
      v = eval(parse(text = v)),
      x = eval(parse(text = x)),
      cluster = eval(parse(text = cluster)),
      data = data
    )
  }

  if(!summary(model)$Mx.status %in% c(0,1)){
    model = rerun(model)
  }

  model$call$model.name = model.name

  return(model)
}

#' meta3_by_factor
#'
#' Creates summary statistics for three level meta-analyses across a factor.
#' @param y a string. The name of the effect size variable
#' @param v a string. The name of the sampling variance variable
#' @param cluster a string. The name of the cluster variable
#' @param factor a string.
#' @param data an object The name of the data object
#' @param names character vector. If given, default names will be replaced.
#' @importFrom metaSEM meta3
#' @importFrom dplyr filter
#' @importFrom tibble as_tibble
#'
#' @export meta3_by_factor


meta3_by_factor = function(y, v, cluster, factor, data, names = NULL) {
  #prepare factors
  . = NULL
  factor_levels = levels(data[, factor])

  model_list = lapply(factor_levels, function(x) {
    temp_data = data %>% filter(data[, factor] == x)
    output = string_meta3(
      data = temp_data,
      y = y,
      v = v,
      cluster = cluster,
      model.name = x
    )
    output
  })

  results = model_list %>% lapply(extractData) %>%
    do.call("rbind", .)

  if (!is.null(names)) {
    if (length(names) != length(result$model)) {
      stop(
        paste0(
          "You provided ",
          length(names),
          " name(s). There are ",
          length(result$model),
          " levels: ",
          paste(result$model, collapse = ", "),
          "."
        )
      )
    } else{
      result$model = names
    }
  }
  results %>% as_tibble
}

