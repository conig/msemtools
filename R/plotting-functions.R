##TODO
#factor levels need to be respected in ordering of summary statistics

#' ninjaForest
#'
#' This function is used for plotting
#'
#' @param model an object belonging to the class 'meta_ninja'. These objects are created by the function 'meta3_moderation'.
#' @param xlab a string.
#' @param transform a function. If supplied effect sizes are transformed by this function
#' @param baseline_name a string. The label for the baseline model
#' @param factor.levels  a charcater vector. If supplied, only the factor.levels specified will be plotted.
#' @param vline a scalar. Dictates the x-intercept (the dashed line).
#' @param cluster study_id column
#' @param author the name of the author column
#' @param year the name of the year column
#' @export ninjaForest
#' @importFrom dplyr filter select %>% mutate
#' @importFrom ggplot2 ggplot aes geom_point geom_errorbarh theme_classic scale_x_continuous facet_grid geom_vline ylab
#' @importFrom stats reorder

#test arguments:
#xlab = "effect size"; baseline_name = "All" ; transform = function(x) papertools::logit2prob(x) ; factor.levels = NULL; vline = 0; cluster = NULL; author = NULL; year = NULL

ninjaForest = function(model, xlab = "effect size", transform = NULL, baseline_name = "All", factor.levels = NULL, vline = NULL, cluster = NULL, author = NULL, year = NULL){
  if(!"meta_ninja" %in% class(model)){
    stop("ninjaForest must be provided objects of class 'meta-ninja")
  }

  #find missing variables
  data = model$data
  if(is.null(year)){
    year = find_year(data)
  }
  if(is.null(author)){
    author = find_author(data)
  }

  if(is.null(cluster)){
    cluster = model$cluster
  }

  #this chunk sets up summary object
  df = model$table
  b_model = model$models$baseline
  #check that all moderators are factor, otherwise warn user
  numeric_moderators = data.frame(df[df$type == "numeric", "model.name"])
  if(nrow(numeric_moderators) > 0) {
    warning(paste0("Only categorical moderators are plotted. The following moderators will be ignored: '",
                   paste(numeric_moderators, collapse = "', '"),"'."),call. = F)
  }

  #### next we separte out the bits of the summary table we want to plot. We want to avoid
  baseline = df %>%
    dplyr::filter(type %in% c("baseline"))

  if(is.null(factor.levels)){
   not_present =factor.levels[!factor.levels %in% df$model.name]
   if(length(not_present) > 0){
     stop(paste0("The following supplied factor levels were not found in model.name: '",paste0(not_present, collapse = "', '"), "'."))
   }

     mod_data = df %>%
      dplyr::filter(type %in% c("factor level"))
  }else{
    mod_data = df %>%
      dplyr::filter(type == "factor level" & model.name %in% factor.levels)
  }

  summary = rbind(baseline, mod_data) %>%
    dplyr::select(cluster = model.name, k,n,est = estimate, lower = lbound, upper = ubound) %>%
    dplyr::mutate(type = "summary",
                  setting = "pooled",
                  year = 1)

  summary$cluster[summary$cluster == "baseline"] = baseline_name

  dat = b_model$data %>%
    dplyr::mutate(se = sqrt(v),
           lower = (y - 1.96*se),
           upper = (y + 1.96*se),
           k = NA,
           n = NA) %>%
    mutate(type = "data", setting = "effects") %>%
    dplyr::select(n,k,est = y, lower,upper,type,setting, cluster)

  dat$author = lapply(dat$cluster, function(x){
    x = as.numeric(as.character(x))
    data[data$study_id == x, "author"] %>% unlist() %>% .[1]
  }) %>% unlist

  dat$year = lapply(dat$cluster, function(x){
    x = as.numeric(as.character(x))
    data[data$study_id == x, "year"] %>% unlist() %>% .[1]
  }) %>% unlist
  dat = dat[order(dat$year),]
  dat$cluster = paste(dat$author,dat$year)
  #create empty columns in summary for author and year
  fun = dat[,c("cluster","k","n","est","lower","upper","type","setting","year")]
  #need to make summary have the same column names and orderings.
  dat = rbind(fun,
              summary)
  dat=dat[order(dat$year),]

  if(!is.null(transform)){
  dat = dat %>%
    dplyr::mutate(
      est = transform(est),
      lower = transform(lower),
      upper = transform(upper)
    )
  }

  if(is.null(vline)){
   if(!is.null(transform)){
     vline = transform(0)
   } else {
     vline = 0
   }
  }

  plot = ggplot(dat, aes(y = reorder(cluster,year), x = est, xmin = lower, xmax = upper)) + geom_point(color = "black") +
    geom_point(data = dat[dat$type == "summary",],color = 'black', shape = 18, size = 4.5)+
    #add the CI error bars
    geom_errorbarh(height=.1)+
    #Specify the limits of the x-axis and relabel it to something more meaningful
    scale_x_continuous(name= xlab)+
    ylab(NULL)+
    geom_vline(xintercept=vline, color='black', linetype='dashed') +
    facet_grid(setting~., scales= 'free', space='free')+
    theme_classic()

  return(plot)
}
