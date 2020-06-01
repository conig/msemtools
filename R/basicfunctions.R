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
    "Q_df",
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
  result$Q_df = summary$Q.stat$Q.df %>% safe_add
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
  # if (!is.na(coef[coef$row == "Tau2_2", "p"])) {
  #   if (coef[coef$row == "Tau2_2", "p"] < 0.05) {
  #     result$t2 = paste(result$t2, "*", sep = "")
  #   }
  # }
  # if (!is.na(result$t2_3)) {
  #   if (!is.na(coef[coef$row == "Tau2_3", "p"])) {
  #     if (coef[coef$row == "Tau2_3", "p"] < 0.05) {
  #       result$t2_3 = paste(result$t2_3, "*", sep = "")
  #     }
  #   }
  # }
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

fix_call = function(model){
  model$call[1] = call("meta3")
  model
}

#' character_matrix
#'
#' creates a predictor matrix when cells can contain multiple tags
#' @param x a character vector
#' @param levels a vector. Provides the level (and order) of colnames.
#' @param pattern pattern provided to str_split
#' @importFrom stringr str_split
#' @importFrom dplyr %>%
#' @export character_matrix

character_matrix = function(x, levels = NULL, pattern = ",") {
  if (!(is.factor(x))) {
    x = factor(x)
  }

  split = x %>%
    stringr::str_split(pattern) %>% #split based on pattern
    lapply(., trimws) #remove whitespace

  contents = split %>%
    unlist %>%
    unique %>%
    na.omit #don't record NAs

  out = lapply(seq_along(contents), function(c) {
    lapply(seq_along(split), function(s) {
      tag = contents[c]
      current = split[s][[1]]
      out = ifelse(tag %in% current, 1, 0)

      if (all(is.na(current))) {
        out = NA
      }
      out
    }) %>% unlist
  }) %>% do.call(cbind, .)

  colnames(out) = contents

  matrix_levels = levels(droplevels(x))
  levels_length_same <- length(matrix_levels) == ncol(out)

  if (levels_length_same & all(matrix_levels %in% colnames(out))) {
    out = out[, matrix_levels] #reorder matrix if possible
  }

  if (!is.null(levels)) {
    if (!all(colnames(out) %in% levels)) {
      current_colnames =  paste0("(", paste(colnames(out), collapse = ","), ")")
      current_levels = paste0("(", paste(levels, collapse = ","), ")")
      stop(
        paste0(
          "colnames ",
          current_colnames,
          " do not match supplied levels: ",
          current_levels
        )
      )
    } else {
      levels_missing = levels[!levels %in% colnames(out)]

      if (length(levels_missing) > 0) {
        find_data_rows = lapply(seq_along(out[, 1]), function(r) {
          all(!is.na(out[r, ]))
        }) %>% unlist
        warning(
          paste0(
            "The following levels were not found in the vector: ",
            paste(levels_missing, collapse = ", "),
            ". They have been added."
          )
        )

        for (l in levels_missing) {
          missing = matrix(rep(NA, nrow(out)))
          colnames(missing) = l


          missing[, 1][find_data_rows] = 0
          out =  cbind(out, missing)
        }
      }
    }
    out = out[, as.character(levels)]
  }

  return(out)
}

#' get_args
#'
#' Derrives arguments from elipses
#' @param ... elipises
#' @importFrom dplyr %>%

get_args <- function(...) {
  vars = substitute(list(...))[-1]
  as.list(vars)
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
  names = gsub("`","",names)

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
    n_data = model$data
    x_vars = names(n_data)[grepl("x",names(n_data))]

    lapply(seq_along(x_vars), function(i){

      temp_data = n_data[,c("y","v","cluster","time",x_vars[i])]

      if(is_binary(n_data[,x_vars[i]])){
      temp_data = temp_data[temp_data[,x_vars[i]] == 1, ]
      }

      k = length(unique(temp_data$cluster))
      n = nrow(temp_data)

      data.frame(name = "Slope",
                 k = k,
                 n = n)
    }) %>%
      do.call(rbind,.)
  }
}

#' moderation_instructions
#'
#' Takes instructions, can then be provided to moderate
#'
#' @param ... moderation instructions
#' @importFrom dplyr %>%
#' @export moderation_instructions

moderation_instructions = function(...){
  args = vars = as.list(match.call()[-1]) %>%
    prepare_args
  return(args)
}

#' moderate
#'
#' This is a wrapper to perform meta3 moderations with. The original data file must be in the environment.
#' @param model A meta3 model. The original data file must be available in the environment, with the same name.
#' @param ... moderators, entered as objects
#' @param moderators a character vector. A vector of moderator names may be supplied.
#' @param binary_intercept a numeric. Constrain the intercept for matricies with binary elements
#' @param continuous_intercept a numeric. Constrain the intercept for matricies with continuous elements
#' @param remove_empty_slopes a bool. If true, removes empty columns from matricies.
#' @importFrom Conigrave check_names
#' @return a meta_ninja \(until I rename it\)
#' @examples
#' @export moderate
#' @details
#' moderate simplifies moderation analyses by taking the call from a meta3, and then using it to generate
#' subsequent moderation models. A few rules are used to do this.
#' 1. If a continuous variable is used a predictor, then an intercept is recorded
#' 2. If binary variables are included, then intercepts are forced to be zero, these binary variables become the intercepts.
#' @examples
#' library(metaSEM); library(msemtools)
#'
#' model0 = meta3(y = drink_yi, v = drink_vi, cluster = study_id, dat = conigrave20)
#' summary(model0)
#'
#' m_moderated = model0 %>%
#'  moderate(Gender, Age)
#'
#' format_nicely(m_moderated, transform = metafor::transf.ilogit)
#'
#' plot(m_moderated)

# model = model0; moderators = NULL
# args = get_args()
moderate = function(model, ..., moderators = NULL,binary_intercept = 0, continuous_intercept = NULL, remove_empty_slopes = T) {
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
              continuous_intercept = continuous_intercept,
              remove_empty_slopes = remove_empty_slopes)
}

#' is_binary
#'
#' This function tests whether a matrix column is binary
#' @param x a matrix

is_binary = function(x){
  x = na.omit(x)
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

#' extractSlopes
#'
#' Grabs slope coefficient data
#'
#' @param model a model.

extractSlopes = function(model) {
  summary = summary(model)
  coef = summary$coefficients %>% data.frame %>% rownames_to_column
  names(coef) = c("row", "est", "stde", "lbound", "ubound", "z", "p")
  intercept = coef[grepl("Intercept", coef$row),]
  slopes = coef[grepl("Slope", coef$row),]
  rbind(intercept,slopes)
}

#' remove_function
#'
#' @param string a call represented as a character.
#' @importFrom dplyr %>%

remove_function = function(string){
  if(grepl("\\(",string)){
    string = gsub("[\\(\\)]", "", regmatches(string, gregexpr("\\(.*?\\)", string))[[1]]) %>%
      str_split(pattern = ",") %>%
      unlist %>%
      trimws %>%
      .[1] # keep first argument only
  }
  return(string)
}


#' extract_colnames
#'
#' Tries to determine column names of matricies
#' @param model the model to extract from
#' @param n the number of names to extract
#' @param data data
#' @param iteration the index to find the predictor matrix
#' @param pred_matricies a list of predictor matricies
#' @importFrom dplyr %>%
#' @importFrom stringr str_split

extract_colnames = function(model, n, data, iteration, pred_matricies) {
  names = c()
  predictor_matrix = pred_matricies[[iteration]]
  if (!is.null(colnames(predictor_matrix))) {
    names = colnames(predictor_matrix)
  }
  if (length(names) != n) {
    names = deparse(model$call$x) %>%
      remove_function()#just give the names of the input

  }
  if (length(names) != n) {
    obj = eval(original_x, data, enclos = sys.frame(sys.parent()))
    names = colnames(obj)
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

#' get_new_matrix
#'
#' Gets the final matrix to be used in models.
#'
#' @param model_call the call of a matrix

get_new_matrix = function(model_call) {
  predictor_matrix = eval(model_call$x, eval(model_call$data), enclos = sys.frame(sys.parent())) %>% #get mod matrix
    as.matrix
  whole_matrix = do.call(get_matrix_long, model_call) #get long data
  slopes = names(whole_matrix)[(grepl("x", names(whole_matrix)))] #find slope references
  at_least1 = which(colSums(abs(as.matrix(
    whole_matrix[, slopes], na.rm = T
  ))) > 0)
  preserve_colnames = colnames(predictor_matrix)[at_least1]
  new_predictor_matrix = as.matrix(predictor_matrix[, at_least1])
  colnames(new_predictor_matrix) = preserve_colnames
  new_predictor_matrix
}


#' get_final_matrix
#'
#' Evaluates models to see if sufficient information is available to moderate.
#'
#' @param model_call the call of a matrix

final_matrix_singular = function(model_call) {
  if (!is.null(model_call$x)) {
    predictor_matrix = eval(model_call$x,
                            eval(model_call$data),
                            enclos = sys.frame(sys.parent())) %>% #get mod matrix
      as.matrix
    whole_matrix = do.call(get_matrix_long, model_call) #get long data
    slopes = names(whole_matrix)[(grepl("x", names(whole_matrix)))] #find slope references
    slope_matrix = as.matrix(
      whole_matrix[, slopes], na.rm = T
    )
    at_least1 = which(colSums(abs(slope_matrix)) > 0)
    final_matrix = slope_matrix[, at_least1]
    if (all(final_matrix == 1)) {
      return(T)
    } else{
      return(F)
    }
  } else{
    return(F)
  }
}



#' meta3_ninja
#'
#' This function streamlines the moderation process. Moderation strategy varies based on whether a numeric, or factoral variable is supplied.
#' @param call a call list
#' @param moderators a vector of moderators
#' @param binary_intercept a numeric. Constrain the intercept for matricies with binary elements
#' @param continuous_intercept a numeric. Constrain the intercept for matricies with continuous elements
#' @param remove_empty_slopes a bool. Inspects each matrix and removes empty slopes as neccesary
#' @importFrom metaSEM meta3
#' @importFrom dplyr filter %>% add_row everything
#' @importFrom tibble as_tibble
#' @importFrom stats anova na.omit
#' @importFrom methods setClass representation
#' @importFrom future.apply future_lapply

#binary_intercept =0; continuous_intercept = NULL; remove_empty_slopes = T

meta3_ninja = function(call, moderators, binary_intercept = 0, continuous_intercept = NULL,
                       remove_empty_slopes){

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
      Q,
      Q_df,
      Q_p,
      Mx_status
    )
  }

  data = call$data %>%
    as.character %>%
    parse(text = .) %>%
    eval

  y = call$y %>%
    as.character
  v = call$v %>%
    as.character
  cluster = call$cluster %>%
    as.character

  # define calls ---------------------------------------------
  calls = lapply(everything(moderators), function(x) {
    #message(x)
    #grab moderator details ----------------------------------
    mod_name = names(moderators[x])
    mod = moderators[x][[1]]
    mod_string = enquote(mod) %>%
      as.character %>%
      .[2]

    new_call = call
    #grab moderator matrix -----------------------------------

    mod_matrix = mod_string %>%
      parse(text = .) %>%
      eval(data, enclos = sys.frame(sys.parent()))

    # if not a matrix apply split to matrix ------------------
    if (!is.matrix(mod_matrix) & !is.numeric(mod_matrix)) {
      mod_matrix = character_matrix(mod_matrix)
      new_call$x = mod_string %>%
        add_function("character_matrix") %>%
        character_call()
    }else if(is.numeric(mod_matrix)){
      new_call$x = mod_string %>%
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

    new_call$model.name = mod_name %>%
      remove_function() #get rid of pesky functions in model.names

    return(new_call)
  })

  # create call list
  calls = append(list(call), calls)
  call_names = lapply(calls, function(i){ #label calls with their names
    i$model.name
  }) %>% unlist
  names(calls) = call_names

  # predictor_matrticies ------------------------------------
  #----------------------------------------------------------


  # remove calls with empty moderators ----------------------
  #----------------------------------------------------------
  empty_moderators = lapply(calls, final_matrix_singular) %>% unlist
  removed_moderators = names(calls)[empty_moderators]
  calls = calls[!empty_moderators]
  call_names = call_names[!empty_moderators]

  predictor_matricies = lapply(seq_along(calls), function(x) {
    #message(x)
    #load call
    predictor_matrix = NULL
    temp_call = calls[[x]]
    predictor_matrix = temp_call$x
    if (!is.null(predictor_matrix)) {
    new_predictor_matrix = get_new_matrix(temp_call)
      if (remove_empty_slopes) {
        #if we want to remove slopes
        predictor_matrix = new_predictor_matrix
      }
    }

    return(predictor_matrix)
  })



  # run models ---------------------------------------------
  #---------------------------------------------------------
  models = lapply(seq_along(calls), function(x){
    #message(x)
    #load call
    temp_call = calls[[x]]
    original_x = temp_call$x
    predictor_matrix = predictor_matricies[[x]]

    if(!is.null(original_x)){
      temp_call$x = as.name("predictor_matrix")
    }

    model_out = do.call(meta3,temp_call) %>%
      try_even_harder() %>%
      fix_call

    if(!is.null(original_x)){
      model_out$call$x = original_x
    }

   return(model_out)
  })


  names(models) = call_names
  moderator_models = models[(2:length(models))]

  predictors = lapply(calls,function(x){
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
    mutate(type = "Baseline",
           n_slopes = 0)

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
      Q = model_info$Q,
      Q_df = model_info$Q_df,
      Q_p = model_info$Q_p,
      `anova p-value` = model_anovas$p[x],
      Mx_status = model_info$Mx_status,
      type = "moderator"
    )
    # model slopes --------------
    slopes = extractSlopes(moderator_models[[x]])
    n_slopes = slopes$row[grepl("Slope", slopes$row)] %>%
      length
    slope_names = extract_colnames(moderator_models[[x]], n = n_slopes , data = data, iteration = x + 1, pred_matricies = predictor_matricies)
    slope_k_n = get_k_n(moderator_models[[x]]) %>%
      filter(name == "Slope")
    slopes$k = NA; slopes$n = NA
    slopes[grepl("Slope",slopes$row),]$k = slope_k_n$k
    slopes[grepl("Slope",slopes$row),]$n = slope_k_n$n
    slopes$row[grepl("Slope", slopes$row)] = slope_names
    slopes = slopes %>% rename(model.name = row) %>%
      mutate(type = "moderator level")

    #bind
    plyr::rbind.fill(overall, slopes) %>%
      mutate(moderation = model_info$model.name,
             n_slopes = n_slopes) %>%
      select(moderation, model.name, k, n, estimate = est, SE = stde, lbound, ubound,
             R2_2,R2_3,Q,Q_df, Q_p,
             Mx_status,`anova p-value`=anova.p.value, type,
             n_slopes) %>%
      as_tibble %>%
      return()

  }) %>% do.call(rbind,.)

  merged_tables = plyr::rbind.fill(baseline_table,model_table)


  out = list(
    models = models,
    table = as_tibble(merged_tables),
    cluster = cluster,
    covariates = predictor_matricies,
    calls = calls,
    data = models[[1]]$call$data,
    removed_moderators = empty_moderators,
    moderators = moderators
  )
  class(out) = c("meta_ninja")

  names(out$covariates) = names(out$models)

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


