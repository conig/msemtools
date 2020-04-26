#' threePSM
#'
#' Perform a weight-function model to test whether the pvalue of estimates affects population estimates
#' @param model the meta3 model
#' @param ... arguments passed to weight::weightfunct

threePSM = function(model, ...){

result <- weightr::weightfunct(
    effect = model$data$y,
    v = model$data$v,
    ...
  )

df <- length(result[[2]]$par) - length(result[[1]]$par)
lrchisq <- 2 * (abs(result[[1]]$value - result[[2]]$value))
pvalue <- 1 - stats::pchisq(lrchisq, df)
adjusted_table = threePSM_table(result)

apa = glue::glue("$\\chi^2$({df}) = {papyr::digits(lrchisq,2)}, $p$ = {papyr::round_p(pvalue)}")

list(chisq = lrchisq,
     df = df,
     pvalue = pvalue,
     adjusted_table = adjusted_table,
     apa = apa,
     raw = result)
}

#this function is the modified print function from weightr.

threePSM_table = function(x){
  if (x$fe == FALSE) {
    if (is.null(x$weights)) {
      adj_int_est <- cbind(x$adj_est[2:((x$nsteps - 1) +
                                          (x$npred + 2))])
      adj_int_se <- cbind(x$adj_se[2:((x$nsteps - 1) +
                                        (x$npred + 2))])
    }
    else {
      adj_int_est <- cbind(c(round(x$adj_est[2:((x$npred +
                                                   2))], digits = 4), sprintf("%.4f", x$weights[2:length(x$weights)])))
      adj_int_se <- cbind(rep("---", length(x[[2]]$par[2:length(x[[2]]$par)])))
    }
  }
  if (x$fe == TRUE) {
    if (is.null(x$weights)) {
      adj_int_est <- cbind(x$adj_est[1:((x$nsteps - 1) +
                                          (x$npred + 1))])
      adj_int_se <- cbind(x$adj_se[1:((x$nsteps - 1) +
                                        (x$npred + 1))])
    }
    else {
      adj_int_est <- cbind(c(round(x$adj_est[1:((x$npred +
                                                   1))], digits = 4), sprintf("%.4f", x$weights[2:length(x$weights)])))
      adj_int_se <- cbind(rep("---", length(x[[2]]$par[1:length(x[[2]]$par)])))
    }
  }
  if (is.null(x$weights)) {
    z_stat_int <- adj_int_est/adj_int_se
    p_val_int <- (2 * stats::pnorm(-abs(z_stat_int)))
    ci.lb_int <- adj_int_est - stats::qnorm(0.975) * adj_int_se
    ci.ub_int <- adj_int_est + stats::qnorm(0.975) * adj_int_se
  }
  else {
    if (x$fe == FALSE) {
      length_a <- length(x[[2]]$par[2:length(x[[2]]$par)])
      z_stat_int <- rep("---", length_a)
      p_val_int <- rep("---", length_a)
      ci.lb_int <- rep("---", length_a)
      ci.ub_int <- rep("---", length_a)
    }
    else {
      length_aF <- length(x[[2]]$par[1:length(x[[2]]$par)])
      z_stat_int <- rep("---", length_aF)
      p_val_int <- rep("---", length_aF)
      ci.lb_int <- rep("---", length_aF)
      ci.ub_int <- rep("---", length_aF)
    }
  }
  res.table <- data.frame(matrix(c(adj_int_est, adj_int_se,
                                   z_stat_int, p_val_int, ci.lb_int, ci.ub_int), nrow = (x$npred +
                                                                                           1 + (x$nsteps - 1)), byrow = F), stringsAsFactors = FALSE)
  rowlabels1 <- rep(0, (x$npred + 1))
  rowlabels1[1] <- "Intercept"
  if (x$npred > 0) {
    for (i in 2:length(rowlabels1)) {
      rowlabels1[i] <- paste(c(colnames(x$XX)[i]))
    }
  }
  rowlabels2 <- rep(0, (x$nsteps - 1))
  for (i in 1:(length(rowlabels2))) {
    rowlabels2[i] <- paste(c(x$steps[i], "< p <", x$steps[i +
                                                            1]), collapse = " ")
  }
  row.names(res.table) <- c(rowlabels1, rowlabels2)
  colnames(res.table) <- c("estimate", "std.error",
                           "z-stat", "p-val", "ci.lb", "ci.ub")
  if (is.null(x$weights)) {
    res.table[, "p-val"] <- format.pval(res.table[,
                                                  "p-val"])
  }
  res.table
}
