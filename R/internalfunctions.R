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
  warning(paste0("year was not manually specified, using column:'", string, "'."),
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
  warning(paste0(
    "author was not manually specified, using column:'",
    string,
    "'."
  ),
  call. = F)
  return(string)
}

#' get_name
#'
#' return's an object's name
#' @param x the object to return the name

 get_name = function(x){
   deparse(substitute(x))
 }
