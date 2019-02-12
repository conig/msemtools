#' extractData
#'
#' Grabs pertinent data from a metaSEM model
#' @param model a numeric.
#' @param model.name a model name.
#' @importFrom dplyr %>%
#' @importFrom tibble rownames_to_column as_tibble


extractData = function(model, model.name = NULL) {
  names = c(
    "model.name",
    "k",
    "n",
    "estimate",
    "SE",
    "lbound",
    "ubound",
    "t2",
    "t2p",
    "I2_2",
    "t2_3",
    "t2_3p",
    "I2_3",
    "Q",
    "Q_p",
    "slope",
    "slope_se",
    "slope_lbound",
    "slope_ubound",
    "slope_p",
    "R2_2",
    "R2_3",
    "Mx_status"
  )
  result = data.frame(matrix(ncol = length(names), nrow = 1))
  names(result) = names

  if (is.data.frame(model)) {
    result[1, ] = c(as.character(model$model.name),
                    model$k,
                    model$n,
                    rep(NA, length(names) - 3))
    return(result)
  }

  safe_add = function(x)
    ifelse(length(x) > 0, x, NA)

  summary = summary(model)
  coef = summary$coefficients %>% data.frame %>% rownames_to_column
  names(coef) = c("row", "est", "stde", "lbound", "ubound", "z", "p")
  result$k = summary$no.studies
  result$n = summary$obsStat

  result$estimate = safe_add(coef[coef$row == "Intercept", "est"])
  result$SE = coef[coef$row == "Intercept", "stde"] %>%
    safe_add
  result$lbound = coef[coef$row == "Intercept", "lbound"] %>% safe_add
  result$ubound = coef[coef$row == "Intercept", "ubound"] %>% safe_add
  result$t2 = coef[coef$row == "Tau2_2", "est"] %>% safe_add
  result$t2p = coef[coef$row == "Tau2_2", "p"] %>% safe_add
  result$t2_3 = coef[coef$row == "Tau2_3", "est"] %>% safe_add
  result$t2_3p = coef[coef$row == "Tau2_3", "p"] %>% safe_add
  result$Q = summary$Q.stat$Q %>% safe_add
  result$Q_p = summary$Q.stat$pval %>% safe_add
  if (!all(is.na(summary$I2.values))) {
    result$I2_2 = summary$I2.values[1, 2] %>% safe_add
    result$I2_3 = summary$I2.values[2, 2] %>% safe_add
  } else{
    result$I2_2 = NA
    result$I2_3 = NA
  }

  if (!all(is.na(summary$R2.values))) {
    result$R2_2 = summary$R2.values[3, 1] %>% safe_add
    result$R2_3 = summary$R2.values[3, 2] %>% safe_add
  }
  if (!is.na(coef[coef$row == "Tau2_2", "p"])) {
    if (coef[coef$row == "Tau2_2", "p"] < 0.05) {
      result$t2 = paste(result$t2, "*", sep = "")
    }
  }
  if (!is.na(result$t2_3)) {
    if (!is.na(coef[coef$row == "Tau2_3", "p"])) {
      if (coef[coef$row == "Tau2_3", "p"] < 0.05) {
        result$t2_3 = paste(result$t2_3, "*", sep = "")
      }
    }
  }
  slopes = sum(grepl("Slope", coef$row))

  if (slopes > 0 & slopes < 2) {
    result$slope = coef[coef$row == "Slope_1", "est"] %>% safe_add
    result$slope_se = coef[coef$row == "Slope_1", "stde"] %>% safe_add
    result$slope_lbound = coef[coef$row == "Slope_1", "lbound"] %>% safe_add
    result$slope_ubound = coef[coef$row == "Slope_1", "ubound"] %>% safe_add
    result$slope_p = coef[coef$row == "Slope_1", "p"] %>% safe_add

  } else{
    result$slope = NA
    result$slope_lbound = NA
    result$slope_ubound = NA
    result$slope_p = NA
  }

  if (is.null(summary$call$model.name)) {
    result$name = NA
  } else{
    result$model.name = as.character(summary$call$model.name)
  }
  result$Mx_status = summary$Mx.status1

  return(as_tibble(result))
}

#' extractSlopes
#'
#' Grabs slope coefficient data
#'
#' @param model a model.

extractSlopes = function(model) {
  summary = summary(model)
  coef = summary$coefficients %>% data.frame %>% rownames_to_column
  names(coef) = c("row", "est", "stde", "lbound", "ubound", "z", "p")
  coef[grepl("Slope", coef$row), ]
}

#' meta3_moderation
#'
#' This function streamlines the moderation process. Moderation strategy varies based on whether a numeric, or factoral variable is supplied.
#' @param call a call list
#' @param moderators a vector of moderators
#' @importFrom metaSEM meta3
#' @importFrom dplyr filter %>% add_row
#' @importFrom tibble as_tibble
#' @importFrom stats anova na.omit
#' @importFrom methods setClass representation
#' @export meta3_moderation

#call = get_call(model0)

meta3_moderation = function(call,moderators) {

  #--------------------------------------- create base
  base = do.call(meta3,call) %>%
    try_even_harder() %>%
    fix_call
  base$call$model.name = "Baseline"
  #--------------------------------------

  get_vars = function(x) {
    x %>% dplyr::select(
      moderation,
      model.name,
      k,
      n,
      estimate,
      SE,
      lbound,
      ubound,
      I2_2,
      I2_3,
      slope,
      slope_se,
      slope_lbound,
      slope_ubound,
      R2_2,
      R2_3,
      Mx_status
    )
  }

  # --------------------------------------------

  base_table = base %>% extractData %>%
    dplyr::mutate(moderation = "Baseline") %>%
    get_vars %>%
    dplyr::mutate("anova p-value" = NA,
                  type = "Baseline") %>%
    list
  base_intercept = base_table[[1]]$estimate
  model_list = list(base)
  table_list = base_table
  covariate_list = list(NULL)

  data = base$call$data %>%
    as.character %>%
    get %>%
    data.frame

  y = base$call$y %>%
    as.character
  v = base$call$v %>%
    as.character
  cluster = base$call$cluster %>%
    as.character

  amazing_result = lapply(moderators, function(x) {
    #----------------- start moderation
    #message(x)
    temp_data = data[, c(y, v, cluster, make.names(x))]
    omitted = temp_data %>% na.omit
    temp_data$x_internal = temp_data[, make.names(x)]

    #so we will mix up this approach.

    mod = temp_data[, make.names(x)] #create moderator variable
    if (!(is.numeric(mod) | is.factor(mod))) {
      stop(
        paste0(
          "'",
          x,
          "' is a ",
          class(mod),
          " variable. Moderators must be numeric or factor variables."
        )
        ,
        call. = F
      )
    }
    #if the moderator is numeric ---------------------------------
    if (is.numeric(mod)) {
      #message("numeric")
      temp_data[, make.names(x)] = scale(temp_data[, make.names(x)], scale = F)
      temp_call = call
      temp_call$x = as.name(x)
      temp_call$intercept.constraints = base_intercept

      temp_model = do.call(meta3,temp_call) %>% fix_call %>% try_even_harder()
      temp_model$call$model.name = x
      other_call = temp_call
      other_call$intercept.constraints = call$intercept.constraints

      unconstrained = do.call(meta3,other_call) %>% fix_call %>% try_even_harder()

      temp_anova = anova(temp_model, base)
      temp_table = extractData(temp_model) %>%
        dplyr::mutate(moderation = x) %>%
        get_vars %>%
        dplyr::mutate("anova p-value" = anova(unconstrained, base)$p[2],
                      type = "numeric")
      return(list(table = temp_table, model = temp_model, covariates = temp_data[,make.names(x)]))
    }

    #if the moderator is a factor -----------------------------------------
    if (is.factor(mod)) {
      temp_data[, make.names(x)] = droplevels(temp_data[, make.names(x)])
      omitted[, make.names(x)] = droplevels(omitted [, make.names(x)])
      keep_levels = levels(omitted[,make.names(x)])
      mod = droplevels(mod) #get rid of empty levels
      levels = keep_levels

      cat_x = lapply(levels, function(y) {
        ifelse(mod == y, 1, 0)
      }) %>%
        do.call("cbind", .)
      colnames(cat_x) = levels

      ##sting meta won't take cat_x due to its environment.

      temp_call = call

      temp_call$x = substitute(cat_x)
      temp_call$intercept.constraints = 0

      temp_model = do.call(meta3,temp_call) %>%
        fix_call() %>%
        try_even_harder()

      temp_model$call$model.name = x
      temp_anova = anova(temp_model, base)
      temp_slopes = temp_model %>% extractSlopes
      temp_slopes$row = colnames(cat_x)
      temp_by_cat = count_levels(
        #this just gets k and n. need to make faster
        y = y,
        v = v,
        cluster = cluster,
        factor = make.names(x),
        data = temp_data
      )
      temp_table = extractData(temp_model) %>%
        dplyr::mutate(moderation = x) %>%
        get_vars %>%
        dplyr::mutate("anova p-value" = temp_anova$p[2],
                      type = "factor")
      if (nrow(temp_by_cat) != nrow(temp_slopes)) {
        warning(paste0("rows are not equal "))
      }
      for (i in seq_len(nrow(temp_slopes))) {
        temp_table = temp_table %>%
          add_row(
            moderation = x,
            model.name = temp_slopes$row[i],
            #add a space to indent line.
            k = temp_by_cat$k[i],
            n = temp_by_cat$n[i],
            estimate = temp_slopes$est[i],
            SE = temp_slopes$stde[i],
            lbound = temp_slopes$lbound[i],
            ubound = temp_slopes$ubound[i],
            type = "factor level"
          )
      }
      return(list(table = temp_table, model = temp_model, covariates = cat_x))
    }
    #got past it

  })
  amazing_tables = lapply(amazing_result, function(x)
    x$table)
  amazing_models = lapply(amazing_result, function(x)
    x$model)
  amazing_covariates = lapply(amazing_result, function(x)
    x$covariates)

  final_models = append(model_list, amazing_models)
  model_names = unlist(lapply(final_models, function(x)
    x$call$model.name))
  names(final_models) = model_names

  final_tables = append(table_list, amazing_tables)
  names(final_tables) = model_names

  final_covariates = append(covariate_list,amazing_covariates)
  names(final_covariates) = model_names
  merged_table = final_tables %>%
    do.call("rbind", .)

  merged_table[merged_table$type == "factor", c("slope", "slope_ubound", "slope_lbound")] = NA #gets rid of error when factors have only one level.

  out = list(
    models = final_models,
    table = merged_table,
    cluster = cluster,
    covariates = final_covariates,
    data = as_tibble(data)
  )
  class(out) = c("meta_ninja")

  #Flag to the users if models have a mx status of greater than one (which indicates issues)
  model_names = out$table$model.name
  model_scores = out$table$Mx_status
  model_names = paste0(model_names, "[", model_scores, "]")
  problem_models =  model_names[which(model_scores > 1)]


  if (length(problem_models) > 0) {
    warning(
      paste0(
        "The following models had Mx status greater that one which indicates potential issues: '",
        paste(problem_models, collapse = "', '"),
        "'."
      ),
      call. = F
    )
  }

  return(out)
}

#' get_call
#'
#' This function is used to extract calls from meta3 models
#' @param model meta 3 model
#' @importFrom dplyr %>%


get_call = function(model) {
  call = model$call[-1] %>%
    as.list
  return(call)
}

#' moderate
#'
#' This is a wrapper to perform meta3 moderations with. The original data file must be in the environment.
#' @param model A meta3 model. The original data file must be available in the environment, with the same name.
#' @param ... moderators, entered as objects
#' @param moderators a character vector. A vector of moderator names may be supplied.
#' @importFrom dplyr %>%
#' @importFrom Conigrave check_names
#' @export moderate

moderate = function(model, ..., moderators = NULL) {
  if (!identical(class(model), c("meta", "meta3"))) {
    stop("moderate only works  for meta3 objects")
  }
  mods = c()
  call = get_call(model)

  if (call$data == ".") {
    stop(
      "moderate grabs the data.frame based on it's name as stored in the metaSEM model call. You've used a pipe (%>%) to specify the model which records the data's name as '.' which cannot be accessed from the global environment. This breaks moderate, please specify the data name in the model explicitly."
    )
  }
  mods = substitute(list(...))[-1] %>%
    sapply(deparse)

  if (!is.null(moderators)) {
    mods = append(mods, moderators) %>% unlist
  }
  data = model$call$data %>%as.character() %>%  get
  Conigrave::check_names(mods, data)

  #return(moderators)
  meta3_moderation(call,
                    moderators = mods)

}

fix_call = function(model){
  model$call[1] = call("meta3")
  model
}
