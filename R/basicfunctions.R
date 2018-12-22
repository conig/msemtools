#' extractData
#'
#' Grabs pertinent data from a metaSEM model
#' @param model a numeric.
#' @importFrom dplyr %>%
#' @importFrom tibble rownames_to_column as_tibble
#' @export extractData

extractData = function(model){

  safe_add = function(x) ifelse(length(x) >0, x,NA)

  summary = summary(model)
  coef = summary$coefficients %>% data.frame %>% rownames_to_column
  names(coef) = c("row","est","stde","lbound","ubound","z","p")
  names = c("model.name","k","n","estimate","SE","lbound","ubound","t2","t2p","I2","t2_3","t2_3p",
            "I2_3","Q","Q_p","slope","slope_lbound","slope_ubound","slope_p","R2_2","R2_3","Mx_status")
  result = data.frame(matrix(ncol = length(names),nrow = 1))
  names(result) = names
  result$k = summary$no.studies
  result$n = summary$obsStat

  result$estimate = safe_add(coef[coef$row == "Intercept","est"])
  result$SE = coef[coef$row == "Intercept","stde"] %>%
    safe_add
  result$lbound = coef[coef$row == "Intercept","lbound"] %>% safe_add
  result$ubound = coef[coef$row == "Intercept","ubound"] %>% safe_add
  result$t2 = coef[coef$row == "Tau2_2","est"] %>% safe_add
  result$t2p = coef[coef$row == "Tau2_2","p"] %>% safe_add
  result$t2_3 = coef[coef$row == "Tau2_3","est"]%>% safe_add
  result$t2_3p = coef[coef$row == "Tau2_3","p"]%>% safe_add
  result$Q = summary$Q.stat$Q%>% safe_add
  result$Q_p = summary$Q.stat$pval %>% safe_add
  if(!all(is.na(summary$I2.values))){
  result$I2 = summary$I2.values[1,2] %>% safe_add
  result$I2_3 = summary$I2.values[2,2] %>% safe_add
  }else{
    result$I2 = NA
    result$I2_3 = NA
  }

  if(!all(is.na(summary$R2.values))){
    result$R2_2 = summary$R2.values[3,1] %>% safe_add
    result$R2_3 = summary$R2.values[3,2] %>% safe_add
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
    result$slope = coef[coef$row == "Slope_1","est"] %>% safe_add
    result$slope_lbound = coef[coef$row == "Slope_1","lbound"] %>% safe_add
    result$slope_ubound = coef[coef$row == "Slope_1","ubound"] %>% safe_add
    result$slope_p = coef[coef$row == "Slope_1","p"] %>% safe_add

  }else{
    result$slope = NA
    result$slope_lbound = NA
    result$slope_ubound = NA
    result$slope_p = NA
  }

 if(is.null(summary$call$model.name)){
   result$name = NA
 }else{
   result$model.name = as.character(summary$call$model.name)
 }
result$Mx_stats = summary$Mx.status1

return(as_tibble(result))
}

#' extractSlopes
#'
#' Grabs slope coefficient data
#'
#' @param model a model.
#' @export extractSlopes

extractSlopes = function(model){
  summary = summary(model)
  coef = summary$coefficients %>% data.frame %>% rownames_to_column
  names(coef) = c("row","est","stde","lbound","ubound","z","p")
  coef[grepl("Slope",coef$row),]
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
#' @param intercept.constraints a numeric. Set the value of the intercept
#' @importFrom metaSEM meta3 rerun
#'
#' @export string_meta3


string_meta3 = function(y, v, cluster, x = NULL, data, model.name = NA,intercept.constraints = NULL) {

  if (is.null(x)) {
    model = metaSEM::meta3(
      y = eval(parse(text = y)),
      v = eval(parse(text = v)),
      cluster = eval(parse(text = cluster)),
      data = data,
      intercept.constraints = intercept.constraints
    )
  } else{
    model = metaSEM::meta3(
      y = eval(parse(text = y)),
      v = eval(parse(text = v)),
      x = eval(parse(text = x)),
      cluster = eval(parse(text = cluster)),
      data = data,
      intercept.constraints = intercept.constraints
    )
  }

  if(!summary(model)$Mx.status %in% c(0,1)){
    model = metaSEM::rerun(model)
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
#' @param output.models a bool. If true, models are provided in a list.
#' @importFrom metaSEM meta3
#' @importFrom dplyr filter %>%
#' @importFrom tibble as_tibble
#'
#' @export meta3_by_factor


meta3_by_factor = function(y, v, cluster, factor, data, names = NULL, output.models = F) {
  #prepare factors
  . = NULL
  factor_levels = levels(data[, factor])

  model_list = lapply(factor_levels, function(fc) {
    #message(fc)
    temp_data = data %>% filter(data[, factor] == fc)
    output = string_meta3(
      data = temp_data,
      y = y,
      v = v,
      x = NULL,
      cluster = cluster,
      model.name = fc,
      intercept.constraints = NULL
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
      results$model.name = names
    }
  }

  if(output.models){
  return(list(table = as_tibble(results), models = model_list))
  } else {
    return(as_tibble(results))
  }

}


#' meta3_moderation
#'
#' This function streamlines the moderation process. Moderation strategy varies based on whether a numeric, or factoral variable is supplied.
#' @param y a string. The name of yi
#' @param v a string. The name of vi
#' @param cluster a string. The name of the clustering variable e.g. 'study id'.
#' @param moderators a charcater vector. The names of the variable used for moderation
#' @param data an object.
#' @param base a meta3 model. Defaults to NULL. The baseline model is automatically calculated, but can also be supplied with this argument.
#' @param names a vector of strings. If supplied, the models can be renamed.
#' @importFrom metaSEM meta3
#' @importFrom dplyr filter %>% add_row
#' @importFrom tibble as_tibble
#' @importFrom stats anova
#' @export meta3_moderation

#y = "drink_yi"; v = "drink_vi"; cluster = "study_id"; moderators = c("gender_cat","females_p"); data = f; base = NULL; names = NULL
# msemtools::meta3_moderation("drink_yi","drink_vi","study_id","gender_cat",data = f)
#x = msemtools::meta3_moderation("drink_yi","drink_vi","study_id",c("gender_cat"),data = f)
meta3_moderation = function(y,
                            v,
                            cluster,
                            moderators,
                            data,
                            base = NULL,
                            names = NULL) {

  if (is.null(base)) {

    base = string_meta3(
      data = data,
      y = y,
      v = v,
      x = NULL,
      cluster = cluster,
      model.name = "baseline",
      intercept.constraints = NULL
    )

  }

  get_vars = function(x){
    x %>% dplyr::select(
      moderation,
      model.name,
      k,
      n,
      estimate,
      lbound,
      ubound,
      I2,
      I2_3,
      slope,
      slope_lbound,
      slope_ubound,
      R2_2,
      R2_3
    )
  }

  base_table = base %>% extractData %>%
    dplyr::mutate(moderation = "baseline") %>%
    get_vars %>%
    dplyr::mutate("anova p-value" = NA) %>%
    list

  amazing_result = lapply(moderators, function(x) {
    mod = data[, x]
    if (!(is.numeric(mod) | is.factor(mod))) {
      stop(
        paste0(
          "'",
          x,
          "' is a ",
          class(mod),
          " variable. Moderators must be numeric or factor variables."
        )
      ,call.=F)
    }

    if (is.numeric(mod)) {
      message("numeric")
      temp_model = string_meta3(
        y = y,
        v = v,
        cluster = cluster,
        x = x,
        data = data,
        model.name = x
      )
      temp_anova = anova(temp_model, base)
      temp_table = extractData(temp_model) %>%
        dplyr::mutate(moderation = x) %>%
        get_vars %>%
        dplyr::mutate("anova p-value" = temp_anova$p[2])
      return(temp_table)
    }


    if (is.factor(mod)) {
      message("Factor")
      levels = levels(mod) #we remove the first level as it becomes the reference group
      cat_x = lapply(levels, function(y) {
        ifelse(mod == y, 1, 0)
      }) %>%
        do.call("cbind", .)
      colnames(cat_x) = levels

      ##sting meta won't take cat_x due to its environment.
      #so we will mix up this approach.
      data$y_internal = data[,y]
      data$v_internal = data[,v]
      data$cluster_internal = data[,cluster]

      temp_model = metaSEM::meta3(
        y = y_internal,
        v = v_internal,
        cluster = cluster_internal,
        x = cat_x,
        data = data,
        model.name = "temp_name",
        intercept.constraints = 0
      )
      temp_model$call$model.name = x
      temp_anova = anova(temp_model, base)
      temp_slopes = temp_model %>% extractSlopes
      temp_slopes$row = colnames(cat_x)
      temp_by_cat = meta3_by_factor(
        y = y,
        v = v,
        cluster = cluster,
        factor = x,
        data = data
      )
      temp_table = extractData(temp_model) %>%
        dplyr::mutate(moderation = x) %>%
        get_vars %>%
        dplyr::mutate("anova p-value" = temp_anova$p[2])
      if (nrow(temp_by_cat) != nrow(temp_slopes))
        warning(paste0("rows, "))
      for (i in seq_len(nrow(temp_slopes))) {
        temp_table = temp_table %>%
          add_row(
            moderation = x,
            model.name = temp_slopes$row[i],
            #add a space to indent line.
            k = temp_by_cat$k[i],
            n = temp_by_cat$n[i],
            estimate = temp_slopes$est[i],
            lbound = temp_slopes$lbound[i],
            ubound = temp_slopes$ubound[i],
          )
      }
      return(temp_table)
    }


  })

  amazing_result = append(base_table, amazing_result) %>%
    do.call("rbind", .)

  return(amazing_result)
}

utils::globalVariables(c(".","I2","I2_3","R2_2","R2_3","anova","estimate",
                         "k","n" ,"lbound","ubound","slope",
                         "slope_ubound","result","model.name",
                         "moderation","slope_lbound","y_internal",
                         "v_internal","cluster_internal"))
