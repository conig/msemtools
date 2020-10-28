#' cormat_list
#'
#' Generate a list of correlation matrices for tssem
#' @param yi correlation effect
#' @param vi correlation variance
#' @param var1 the name of the column containing the first variable
#' @param var1 the name of the column containing the second variable
#' @param cluster the name of the clustering variable
#' @param data data.frame
#' @param transform a function to transform individual correlations
#' @param ni the name of the column containing the sample size
#' @import data.table

cormat_list = function(yi, vi, ni, var1, var2, cluster, data, transform = NULL){

  data = data.table::data.table(data)
  inspect_vars = data[,c(yi,vi,var1,var2,cluster), with = FALSE]
  missing = unlist(lapply(1:nrow(inspect_vars), function(x) any(is.na(inspect_vars[x,]))))
  data = data[!missing, ]
  vars <- unique(c(data[,var1], data[,var2]))

  make_matrix = function(vars) {
    m = matrix(nrow = length(vars), ncol = length(vars))
    rownames(m) = vars
    colnames(m) = vars

    diag(m) = 1
    m
  }

  get_matrix = function(id, vars, data, transform = NULL){
    m = make_matrix(vars)
    dat = data[which(data[,cluster, with = FALSE] == id),]

    for(col in seq_along(colnames(m))){
      for(row in seq_along(rownames(m))){
        # message("row = ",row, "col = ", col)
        if(col == row){
          m[row, col] = 1
          next
        }

        vars = c(colnames(m)[col], rownames(m)[row])
        valid_rows = unlist(dat[,var1, with = FALSE]) %in% vars &
          unlist(dat[,var2, with = FALSE]) %in% vars &
          !is.na(unlist(dat[,yi, with = FALSE])) &
          !is.na(unlist(dat[,vi, with = FALSE]))

        if(sum(valid_rows) == 0){
          m[row,col] = NA
          next
        }

        y <- dat[valid_rows, yi, with = FALSE]
        v <- dat[valid_rows, vi, with = FALSE]

        res <- stats::weighted.mean(y, 1/v)

        if(!is.null(transform)){
          res <- transform(res)
        }
        m[row,col] = res
      }

    }
    rownames(m) = rownames(m)
    colnames(m) = colnames(m)
    m

  }

  unique_ids = unique(unlist(data[,cluster, with = FALSE]))

  dat_list = list()
  dat_list$data = lapply(unique_ids, function(x) get_matrix(x, vars, data, transform))
  get_n = function(id, ni){
    ceiling(mean(unlist(data[unlist(data[,cluster, with = FALSE]) == id, ni, with = FALSE]),na.rm = TRUE))
  }


  dat_list$n = unlist(lapply(unique_ids, function(x) get_n(x, ni)))
  dat_list

}


star_matrix <- function(m, stars) {
  get_stars = function(p, stars) {
    if (is.na(p))
      p <- 1
    n_stars = sum(p < stars)
    paste(rep("*", n_stars), collapse = "")
  }

  s_matrix = m
  s_matrix[] =  sapply(m, function(p)
    get_stars(p, stars = stars))
  return(s_matrix)
}

#' tssem1_table
#'
#' Creates a table from the pooled correlation matrix from tssem1
#' @param model the tssem stage 1 model

tssem1_table = function(model){

  dim_names = model$original.names
  r_mat = coef(model, select = "fixed") %>%
    metaSEM::vec2symMat(diag = FALSE)
  dimnames(r_mat) = list(dim_names,dim_names)

  coefs = summary(model)$coefficients # get all coefs
  p_mat = coefs[grepl("Intercept", rownames(coefs)), "Pr(>|z|)"] %>% metaSEM::vec2symMat(diag = FALSE) #keep intercept coefs (not taus)
  p_mat[upper.tri(p_mat)] = 1 # I don't want stars for the diagonal.
  s_mat = star_matrix(p_mat , stars = c(0.05,0.01,0.001)) #star matrix

  i2 = summary(model)$I2.values[,"Estimate"]*100
  i2_mat = metaSEM::vec2symMat(i2, diag = FALSE) # create i2 matrix
  #r_mat = round(r_mat,2) #round r matrix
  r_mat[upper.tri(r_mat)] = i2_mat[upper.tri(i2_mat)] # merge in i2 matrix
  r_mat = papyr::digits(r_mat, 2)
  r_mat[] = paste0(r_mat,s_mat) #add in significance stars
  diag(r_mat) = "-" # add in diagonal
  r_mat[lower.tri(r_mat)] = gsub("0\\.",".",r_mat[lower.tri(r_mat)]) #get rid of leading zeros
  r_mat = data.frame(r_mat)
  rownames(r_mat) = paste0(seq_along(rownames(r_mat)),". ", rownames(r_mat)) #change rownames
  colnames(r_mat) = seq_along(rownames(r_mat)) #change colnames
  r_mat
}

#' tssem2_table
#'
#' Tabulate tssem2 regressions
#' @param wls a wls model
#' @param ... recode variable names, new_name = old_name
#' @param estimate string, name for estimate variable
#' @param transf transform function to apply to results
#' @param t.name name for transformed results
#' @export

tssem2_table = function(wls, ..., transf = NULL, t.name = NULL, estimate = "Estimate", round = 2){

  epi <- list(...)

  reg <- summary(wls)$coefficients
  vars <- colnames(wls$Cov)
  temp_vars <- paste0("v", 1:length(vars))


  reg <- tibble::rownames_to_column(reg)

  for(i in seq_along(vars)){
    reg$rowname = gsub(vars[i],temp_vars[i],reg$rowname)
  }

  # allow recodes -------------------------

  if(length(epi) > 0){
    for(i in seq_along(epi)){
      vars[grepl(epi[i],vars)] = names(epi[i])
    }
  }

  # ---------------------------------------


  colnames(reg) = c("Predictor", "Estimate","SE","lbound","ubound","z", "p")

  if(!is.null(transf)){
    reg$Estimate <- round(transf(reg$Estimate),2)
    reg$lbound <- round(transf(reg$lbound),2)
    reg$ubound <- round(transf(reg$ubound),2)
    reg$transf <- glue::glue("{reg$Estimate} [{reg$lbound}, {reg$ubound}]")
    reg = reg[,c(1, 8, 2:7)]
  }else{
    est <- round(reg$Estimate,2)
    lower <- round(reg$lbound,2)
    upper <- round(reg$ubound,2)
    reg$Estimate <- glue::glue("{est} [{lower}, {upper}]")
  }

  reg$lbound = NULL
  reg$ubound = NULL

  reg$outcome = gsub("on.*","",reg$Predictor)
  reg$outcome[grepl("with", reg$Predictor)] = "Covariances"
  reg$Predictor = gsub(".*on","", reg$Predictor)

  repLace <- function(x) {
    n <- which(sapply(temp_vars, function(i)
      grepl(i, x)))

    if(sum(n) == 0){
      return("Covariances")
    }

    if(length(n) > 1) {
      one <- vars[n[1]]
      two <- vars[n[2]]
      out <- paste(one, "with", two)
    }else{
      out <- vars[n]
    }
    out
  }

  reg$SE = papyr::digits(reg$SE, 2)
  reg$z = papyr::digits(reg$z, 2)
  reg$p = papyr::round_p(reg$p)

  reg$Predictor <- sapply(reg$Predictor, repLace)
  reg$outcome <- sapply(reg$outcome, repLace)
  papyr:::to_rowhead(reg, outcome)


}
