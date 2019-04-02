#' get_matrix_long
#'
#' recreates the long matrix from meta3
#' @param y A vector of k studies of effect size.
#' @param v A vector of k studies of sampling variance.
#' @param cluster A vector of k characters or numbers indicating the clusters.
#' @param x A predictor or a k x m matrix of level-2 and level-3 predictors where m is the number of predictors.
#' @param data An optional data frame containing the variables in the model.
#' @param ... A bin for additional arguments

get_matrix_long = function (y, v, cluster, x, data, ...){
  mf <- match.call()
  if (missing(data)) {
    data <- sys.frame(sys.parent())
  } else {
    if (!is.data.frame(data)) {
      data <- data.frame(data)
    }
  }
  my.y <- mf[[match("y", names(mf))]]
  my.v <- mf[[match("v", names(mf))]]
  my.cluster <- mf[[match("cluster", names(mf))]]
  y <- eval(my.y, data, enclos = sys.frame(sys.parent()))
  v <- eval(my.v, data, enclos = sys.frame(sys.parent()))
  cluster <-
    eval(my.cluster, data, enclos = sys.frame(sys.parent()))
  if (!is.character(cluster)) {
    cluster <- as.character(cluster)
  }
  if (any(is.na(cluster))) {
    stop("Missing values are not allowed in \"cluster\".\n")
  }
  my.long <- data.frame(y, v, cluster)
  if (missing(x)) {
    no.x <- 0
    miss.x <- is.na(v)
  } else {
    my.x <- mf[[match("x", names(mf))]]
    x <- eval(my.x, data, enclos = sys.frame(sys.parent()))
    if (is.vector(x)) {
      no.x <- 1
    } else {
      no.x <- ncol(x)
    }
    old.labels <- names(my.long)
    my.long <- data.frame(my.long, x)
    names(my.long) <- c(old.labels, paste("x", 1:no.x, sep = ""))
    miss.x <- apply(is.na(cbind(v, x)), 1, any)
  }
  my.long <- my.long[!miss.x, ]
  my.long <- my.long[order(my.long$cluster), ]
  my.long$time <-
    c(unlist(sapply(split(my.long$y, as.character(my.long$cluster)),
                    function(x)
                      1:length(x))))
  return(my.long)
}
