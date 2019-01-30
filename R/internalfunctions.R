#' find_year
#'
#' detects the string name of a column with year information
#' @param data data object

#data = f
find_year = function(data) {
  data = data.frame(data)
  count_chars = lapply(seq_along(names(data)), function(i) {
    suppressWarnings(var <-
                       as.numeric(as.character(data[, names(data)[i]]))) #strip factors, make numeric
    year = strsplit(as.character(Sys.Date()), split = "-")[[1]][1]
    var = ifelse(var > as.numeric(year) + 1 , NA, var)
    var = ifelse(var < 1800, NA, var)
    n = nchar(as.character(var)) == 4
    sum(n, na.rm = T) / length(n)
  })
  string = names(data)[which.max(count_chars)]
  message(paste0("year was not manually specified, using column:'", string, "'."),
          call. = F)
  return(string)
}


#' find_author
#'
#' detects the string name of a column with year information
#' @param data data object

find_author = function(data) {
  data = data.frame(data)
  vars = names(data)
  has_author_title =  as.numeric(grepl("author", tolower(vars)))
  et_al = lapply(vars, function(i) {
    num = grepl("et al", tolower(data[, i]))
    journal = grepl("journal", tolower(data[, i])) * 2
    sum(num, na.rm = T) - sum(journal, na.rm = T)
  })
  score = has_author_title + unlist(et_al)
  string = vars[which.max(score)]
  message(paste0(
    "author was not manually specified, using column:'",
    string,
    "'."
  ),
  call. = F)
  return(string)
}


#' count_levels
#'
#' counts levels in factors
#' @param y a string
#' @param v a string
#' @param cluster a string
#' @param factor a string
#' @param data a data object
#' @importFrom tibble tibble

count_levels = function(y,v,cluster,factor, data){
  temp_data = na.omit(data[,c(y,v,cluster,factor)])
  levels = levels(droplevels(temp_data[,factor]))
  out = tibble(level = levels)
  out$k = lapply(levels, function(i){
    length(unique(temp_data[temp_data[,factor] == i,cluster]))
  }) %>% unlist
  out$n = lapply(levels, function(i){
    length(temp_data[temp_data[,factor] == i,cluster])
  }) %>% unlist
  return(out)
}

#' get_name
#'
#' return's an object's name
#' @param x the object to return the name

get_name = function(x) {
  deparse(substitute(x))
}

#' try_even_harder
#'
#' reruns models with problems
#' @param model the model.
#' @importFrom dplyr %>%
#' @importFrom metaSEM rerun

try_even_harder = function(model) {
  if (!summary(model)$Mx.status %in% c(0, 1)) {
    suppressMessages(model <- metaSEM::rerun(model))
  }
  if (!summary(model)$Mx.status %in% c(0, 1)) {
    suppressMessages(model <- #all the rerun messages in bulk just get in the way.
                       metaSEM::rerun(
                         model,
                         extraTries = 19,
                         finetuneGradient = F
                       ))
  }
  return(model)
}
