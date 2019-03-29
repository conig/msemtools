#TODO commands in data field should be evaluated, cannot assume data will be assigned to an object before operating on.

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


#' factor_to_matrix
#'
#' Converts a factor to a predictor matrix
#' @param factor a factor.
#' @param col_names a vector of colnames. If none given, will use factor levels.
#' @importFrom dplyr %>%
#' @export factor_to_matrix

factor_to_matrix = function(factor, col_names = NULL) {
  if (!is.factor(factor)) {
    stop("factor_to_matrix must be supplied a factor", call. = FALSE)
  }
  levels = levels(factor)
  result = lapply(levels, function(y) {
    ifelse(factor == y, 1, 0)
  }) %>%
    do.call("cbind", .)
  if (is.null(col_names)) {
    colnames(result) = levels
  } else{
    colnames(result) = col_names
  }
  return(result)
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
    temp_data = data[, c(make.names(y), make.names(v), cluster, make.names(x))]
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
      keep_levels = omitted[,make.names(x)] %>% droplevels %>%
        levels
      mod = factor(mod, levels = keep_levels) #get rid of empty levels
      levels = keep_levels
      temp_data[,make.names(x)] = factor(temp_data[,make.names(x)], levels = keep_levels)

      cat_x = temp_data[,make.names(x)] %>%
        factor_to_matrix()

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
 #return(call)
  #return(mods)
  meta3_moderation(call,
                    moderators = mods)

}

fix_call = function(model){
  model$call[1] = call("meta3")
  model
}

#' split_to_matrix
#'
#' creates a predictor matrix when cells can contain multiple tags
#' @param x a character vector
#' @param pattern pattern provided to str_split
#' @importFrom stringr str_split
#' @importFrom dplyr %>%
#' @export split_to_matrix

split_to_matrix = function(x, pattern = ",") {

  split = x %>%
    stringr::str_split(pattern) %>% #split based on pattern
    lapply(., trimws) #remove whitespace

  contents = split %>%
    unlist %>%
    unique

  out = lapply(contents, function(c) {
    lapply(split, function(s) {
      as.numeric(c %in% s)
    }) %>% unlist
  }) %>% do.call("cbind", .)

  colnames(out) = contents

  matrix_levels = levels(droplevels(x))
  levels_length_same <- length(matrix_levels) == ncol(out)

  if(levels_length_same){
  out = out[,matrix_levels] #reorder matrix if possible
  }

  return(out)
}

#' get_args
#'
#' Derrives arguments from elipses
#' @param ... elipises
#' @importFrom dplyr %>%

get_args <- function(...) {
  vars = substitute(list(...))[-1] %>%
    sapply(deparse) %>%
    as.list
  vars
}

#' prepare_args
#'
#' Takes a list of arguments, puts contents in names when there are none
#' @param args some args

prepare_args = function(args){
  vars = args
  if(is.null(names(vars))){
    names(vars) = rep("", length(vars))
  }
  names = names(vars)
  names[names == ""] = vars[names == ""] %>%
    unlist

  # for(i in seq_along(vars)){
  #   if(names(vars)[i]== ""){
  #     vars[[i]] = NA
  #   }
  #
  #   vars[[i]] = as.name(vars[[i]])
  #
  # }
  names(vars) = names
  vars
}

#' get_k_n
#'
#' extracts the numbers of k and n from a predictor matrix
#' @param model a model
#' @importFrom dplyr %>%

get_k_n = function(model){
  if(model$call$model.name == "Baseline"){
    info = extractData(model)
    k = info$k
    n = info$n
    data.frame(name = "baseline",
               k = k,
               n = n)
  }else{
    data = eval(parse(text = model$call$data))
    suppressMessages(attach(data))
    pred_matrix = eval(parse(text = deparse(model$call$x))) %>%
      as.matrix
    suppressMessages(detach(data))
    lapply(seq_along(pred_matrix[1,]), function(i){
      clus = as.numeric(unlist(data[,deparse(model$call$cluster)]))
      mini_matrix = pred_matrix[,i] %>%
        cbind(moderators = ., cluster = clus) %>%
        na.omit
    data.frame(name = "slope",k = length(unique(mini_matrix[,2])),
         n = nrow(mini_matrix))
    }) %>%
      do.call(rbind,.)
  }
}


#' moderate2
#'
#' This is a wrapper to perform meta3 moderations with. The original data file must be in the environment.
#' @param model A meta3 model. The original data file must be available in the environment, with the same name.
#' @param ... moderators, entered as objects
#' @param moderators a character vector. A vector of moderator names may be supplied.
#' @param binary_intercept a numeric. Constrain the intercept for matricies with binary elements
#' @param continuous_intercept a numeric. Constrain the intercept for matricies with continuous elements
#' @importFrom dplyr %>%
#' @importFrom Conigrave check_names
#' @return a meta_ninja \(until I rename it\)
#' @details
#' moderate simplifies moderation analyses by taking the call from a meta3, and then using it to generate
#' subsequent moderation models. A few rules are used to do this.
#' 1. If a continuous variable is used a predictor, then an intercept is recorded
#' 2. If binary variables are included, then intercepts are forced to be zero, these binary variables become the intercepts.

# model = model0; moderators = NULL
# args = get_args()
moderate2 = function(model, ..., moderators = NULL,binary_intercept = 0, continuous_intercept = NULL) {
  if (!identical(class(model), c("meta", "meta3"))) {
    stop("moderate only works  for meta3 objects")
  }
  mods = c()
  call = get_call(model)
  args = get_args(...)

  if (call$data == ".") {
    stop(
      "moderate grabs the data.frame based on it's name as stored in the metaSEM model call. You've used a pipe (%>%) to specify the model which records the data's name as '.' which cannot be accessed from the global environment. This breaks moderate, please specify the data name in the model explicitly."
    )
  }

  if (!is.null(moderators)) {
    mods = as.list(moderators) %>%
      append(args)
    mods = append(args, moderators) %>% unlist
  }else{
    mods = args
  }
    mods = mods %>%
      prepare_args

  data = model$call$data %>%as.character() %>% parse(text = .) %>% eval
  #Conigrave::check_names(mods, data)
  #return(call)
  #return(mods)

  #return(list(call = call, moderators = mods))

  meta3_ninja(call,
                   moderators = mods,
              binary_intercept = binary_intercept,
              continuous_intercept = continuous_intercept)
}

#' is_binary
#'
#' This function tests whether a matrix column is binary
#' @param x a matrix

is_binary = function(x){
  apply(as.matrix(x),2,function(y) { all(y %in% 0:1) })
}

#' add_function
#'
#' wraps text in a function
#' @param x character
#' @param f as character

add_function = function(x, f = NULL){
  if(!is.null(f)){
    x = paste0(f,"(",x,")")
  }
}

#' character_call
#'
#' converts character to a call object
#' @param x a character
#' @return a call

character_call = function(x){
  eval(parse(text = paste0("quote(",x,")")))
}

#' extractSlopes2
#'
#' Grabs slope coefficient data
#'
#' @param model a model.

extractSlopes2 = function(model) {
  summary = summary(model)
  coef = summary$coefficients %>% data.frame %>% rownames_to_column
  names(coef) = c("row", "est", "stde", "lbound", "ubound", "z", "p")
  intercept = coef[grepl("Intercept", coef$row),]
  slopes = coef[grepl("Slope", coef$row),]
  rbind(intercept,slopes)
}

#' extract_colnames
#'
#' Tries to determine column names of matricies
#' @param model the model to extract from
#' @param n the number of names to extract
#' @param data data
#' @importFrom dplyr %>%
#' @importFrom stringr str_split

extract_colnames = function(model, n, data) {
  names = deparse(model$call$x) #just give the names of the input
  names = gsub("[\\(\\)]", "", regmatches(names, gregexpr("\\(.*?\\)", names))[[1]]) %>%
    str_split(pattern = ",") %>%
    unlist %>%
    trimws %>%
    .[1] # keep first argument only

  if (length(names) != n) {
    #if names isn't equal to n
    suppressMessages(attach(data))
    obj = eval(parse(text = deparse(model$call$x))) #
    names = colnames(obj)
    suppressMessages(detach(data))
  }

  if (length(names) != n) {
    names = as.character(model$call$x)[-1]
  }
  if (length(names) != n) {
    #get multiple names out of function
    names = deparse(obj)
    names = gsub("[\\(\\)]", "", regmatches(names, gregexpr("\\(.*?\\)", names))[[1]]) %>%
      str_split(pattern = ",") %>%
      unlist %>%
      trimws
  }
  if (length(names) == n) {
    names
  } else{
    rep("unknown variable", n)
  }
}

#' meta3_ninja
#'
#' This function streamlines the moderation process. Moderation strategy varies based on whether a numeric, or factoral variable is supplied.
#' @param call a call list
#' @param moderators a vector of moderators
#' @param binary_intercept a numeric. Constrain the intercept for matricies with binary elements
#' @param continuous_intercept a numeric. Constrain the intercept for matricies with continuous elements
#' @importFrom metaSEM meta3
#' @importFrom dplyr filter %>% add_row everything
#' @importFrom tibble as_tibble
#' @importFrom stats anova na.omit
#' @importFrom methods setClass representation
#' @importFrom future.apply future_lapply
#' @export meta3_ninja

meta3_ninja = function(call, moderators, binary_intercept = 0, continuous_intercept = NULL){

  #--------------------------------------- create base
  call$model.name = "Baseline"
  #--------------------------------------
  #Define variables to extract
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
      R2_2,
      R2_3,
      Mx_status
    )
  }

  data = call$data %>%
    as.character %>%
    parse(text = .) %>%
    eval %>%
    data.frame

  y = call$y %>%
    as.character
  v = call$v %>%
    as.character
  cluster = call$cluster %>%
    as.character

  # define calls ---------------------------------------------
  calls = lapply(everything(moderators), function(x) {
    #grab moderator details ----------------------------------
    mod_name = names(moderators[x])
    mod = moderators[x][[1]]
    new_call = call
    #grab moderator matrix -----------------------------------
    suppressMessages(attach(data))
    mod_matrix = mod %>%
      parse(text = .) %>%
      eval
    detach(data)
    # if not a matrix apply split to matrix ------------------
    if (!is.matrix(mod_matrix) & !is.numeric(mod_matrix)) {
      mod_matrix = split_to_matrix(mod_matrix)
      new_call$x = mod %>%
        add_function("split_to_matrix") %>%
        character_call()
    }else if(is.numeric(mod_matrix)){
      new_call$x = mod %>%
        character_call()
    }

    # stop if not a matrix -----------------------------------
    if (!is.matrix(mod_matrix) & !is.numeric(mod_matrix)) {
      stop(paste0("Could not coerce '", mod, "' to matrix."))
    }

    # Prepare intercepts -------------------------------------
    binary_elements = any(is_binary(mod_matrix))
    model_intercept = NULL
    if (binary_elements) {
      model_intercept = 0
      if (!is.null(binary_intercept)) {
        model_intercept = 0
      }

    } else if (!is.null(continuous_intercept)) {
      model_intercept = continuous_intercept
    }

    if (!is.null(model_intercept)) {
      new_call$intercept.constraints = model_intercept
    }

    new_call$model.name = mod_name

    return(new_call)
  })
  # create call list
  calls = append(list(call), calls)
  names(calls) = c(call$model.name,moderators)

  # run models ---------------------------------------------
  models = future.apply::future_lapply(calls, function(x){
    do.call(meta3,x) %>%
      try_even_harder() %>%
      fix_call
  })

  moderator_models = models[(2:length(models))]

  predictors = future.apply::future_lapply(calls,function(x){
    pred = x$x
  })

  # run anovas ---------------------------------------------
  model_anovas = future.apply::future_lapply(everything(moderator_models), function(x){
    out = anova(moderator_models[[x]],models$Baseline)
    data.frame(diffdf = out$diffdf[2], p =out$p[2])
  }) %>% do.call(rbind,.)

  ################ --------------------------------------------
  #Baseline table
  baseline_table = models[[1]] %>% extractData %>%
    mutate(moderation = "Baseline") %>%
    get_vars %>%
    mutate(type = "Baseline")

  ################ ---------------------------------------------------------------------
  # Moderator table
  model_table = future.apply::future_lapply(everything(moderator_models), function(x) {
    #message(x)
    # overall line --------------
    model_info = extractData(moderator_models[[x]])
    overall = data.frame(
      model.name = model_info$model.name,
      k = model_info$k,
      n = model_info$n,
      R2_2 = model_info$R2_2,
      R2_3 = model_info$R2_3,
      `anova p-value` = model_anovas$p[x],
      Mx_status = model_info$Mx_status,
      type = "moderator"
    )
    # model slopes --------------
    slopes = extractSlopes2(moderator_models[[x]])
    n_slopes = slopes$row[grepl("Slope", slopes$row)] %>%
      length
    slope_names = extract_colnames(moderator_models[[x]], n = n_slopes , data = data)
    slope_k_n = get_k_n(moderator_models[[x]]) %>%
      filter(name == "slope")
    slopes$k = NA; slopes$n = NA
    slopes[grepl("Slope",slopes$row),]$k = slope_k_n$k
    slopes[grepl("Slope",slopes$row),]$n = slope_k_n$n
    slopes$row[grepl("Slope", slopes$row)] = slope_names
    slopes = slopes %>% rename(model.name = row) %>%
      mutate(type = "moderator level")

    #bind
    plyr::rbind.fill(overall, slopes) %>%
      mutate(moderation = model_info$model.name) %>%
      select(moderation, model.name, k, n, estimate = est, SE = stde, lbound, ubound,
             R2_2,R2_3,
             Mx_status,`anova p-value`=anova.p.value, type ) %>%
      as_tibble %>%
      return()

  }) %>% do.call(rbind,.)

  merged_tables = plyr::rbind.fill(baseline_table,model_table)


  out = list(
    models = models,
    table = as_tibble(merged_tables),
    cluster = cluster,
    covariates = predictors,
    data = models[[1]]$call$data
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


