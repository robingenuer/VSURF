#' Interpretation step of VSURF
#' 
#' Interpretation step aims to select all variables related to the response for
#' interpretation prupose. This is the second step of the \code{\link{VSURF}}
#' function. It is designed to be executed after the thresholding step
#' \code{\link{VSURF.thres}}.
#' 
#' \code{nfor.interp} embedded random forests models are grown, starting with
#' the random forest build with only the most important variable and ending
#' with all variables.  Then, \code{err.min} the minimum mean out-of-bag (OOB)
#' error rate of these models and its associated standard deviation
#' \code{sd.min} are computed.  Finally, the smallest model (and hence its
#' corresponding variables) having a mean OOB error less than \code{err.min} +
#' \code{nsd} * \code{sd.min} is selected.
#' 
#' @aliases VSURF.interp VSURF.interp.default VSURF.interp.formula
#' VSURF.interp.parallel VSURF.interp.parallel.default
#' VSURF.interp.parallel.formula
#' 
#' @param data a data frame containing the variables in the model.
#' @param na.action A function to specify the action to be taken if NAs are
#' found.  (NOTE: If given, this argument must be named, and as
#' \code{randomForest} it is only used with the formula-type call.)
#' @param x,formula A data frame or a matrix of predictors, the columns
#' represent the variables. Or a formula describing the model to be fitted.
#' @param y A response vector (must be a factor for classification problems and
#' numeric for regression ones).
#' @param vars A vector of variable indices. Typically, indices of variables
#' selected by thresholding step (see value \code{varselect.thres} of
#' \code{\link{VSURF.thres}} function).
#' @param nfor.interp Number of forests grown.
#' @param nsd Number of times the standard deviation of the minimum value of
#' \code{err.interp} is multiplied. See details below.
#' @param clusterType Type of the multiple cores cluster used to run VSURF in
#' parallel. Must be chosen among "PSOCK" (default: SOCKET cluster available
#' locally on all OS), "FORK" (local too, only available for Linux and Mac OS)
#' and "MPI" (can be used on a remote cluster, which needs \code{snow} and
#' \code{Rmpi} packages installed)
#' @param ncores Number of cores to use. Default is set to the number of cores
#' detected by R minus 1.
#' @param ...  others parameters to be passed on to the \code{randomForest}
#' function (see ?randomForest for further information)
#' 
#' @return An object of class \code{VSURF.interp}, which is a list with the
#' following components:
#' 
#' \item{varselect.interp}{A vector of indices of selected variables.}
#' 
#' \item{err.interp}{A vector of the mean OOB error rates of the embedded
#' random forests models.}
#' 
#' \item{sd.min}{The standard deviation of OOB error rates associated to
#' the random forests model attaining the minimum mean OOB error rate.}
#' 
#' \item{num.varselect.interp}{The number of selected variables.}
#' 
#' \item{varselect.thres}{A vector of indexes of variables selected after
#' "thresholding step", sorted according to their mean VI, in decreasing order.}
#' 
#' \item{comput.time}{Computation time.}
#'
#' \item{clusterType}{The type of the cluster used to run
#' \code{VSURF.parallel} (only if parallel version of VSURF is used).}
#'
#' \item{ncores}{The number of cores used to run \code{VSURF.parallel}
#' (only if parallel version of VSURF is used).}
#'
#' \item{call}{The original call to \code{VSURF}.}
#'
#' \item{terms}{Terms associated to the formula (only if formula-type call
#' was used).}
#' 
#' @author Robin Genuer, Jean-Michel Poggi and Christine Tuleau-Malot
#' @seealso \code{\link{VSURF}}, \code{\link{tune}}
#' @references Genuer, R. and Poggi, J.M. and Tuleau-Malot, C. (2010),
#' \emph{Variable selection using random forests}, Pattern Recognition Letters
#' 31(14), 2225-2236
#' @examples
#' 
#' data(iris)
#' iris.thres <- VSURF.thres(x=iris[,1:4], y=iris[,5], ntree=100, nfor.thres=20)
#' iris.interp <- VSURF.interp(x=iris[,1:4], y=iris[,5], vars=iris.thres$varselect.thres,
#'                             nfor.interp=10)
#' iris.interp
#' 
#' \dontrun{
#' # A more interesting example with toys data (see \code{\link{toys}})
#' # (a few minutes to execute)
#' data(toys)
#' toys.thres <- VSURF.thres(x=toys$x, y=toys$y)
#' toys.interp <- VSURF.interp(x=toys$x, y=toys$y, vars=toys.thres$varselect.thres)
#' toys.interp}
#' 
#' @rdname VSURF.interp
#' @method VSURF.interp default
#' @export
VSURF.interp.default <- function(x, y, vars, nfor.interp=25, nsd=1, ...) {
  
  # vars: selected variables indices after thresholding step
  # nfor.interp: number of forests to estimate each model
  # nsd: number of standard deviation: the selected model leads to an OOB error
  # smaller than the min error + nsd * (sd of the min error)

  start <- Sys.time()
  
  # one forest run to determine the problem type: classification or regression
  rf <- randomForest(x=x, y=y, ...)
  if (rf$type=="classification") {
    type <- "classif"
  }
  if (rf$type=="regression") {
    type <- "reg"
  } 
  
  nvars <- length(vars)
  n <- nrow(x)
  err.interp <- rep(NA, nvars)
  sd.interp <- rep(NA, nvars)
  
  for (i in 1:nvars){
    rf <- rep(NA, nfor.interp)
    u <- vars[1:i]
    w <- x[,u, drop=FALSE]
    if (type=="classif") {
      if (i <= n) {
        for (j in 1:nfor.interp) {
          rf[j] <- tail(randomForest(x=w, y=y, ...)$err.rate[,1], n=1)
        }
      }
      else {
        for (j in 1:nfor.interp) {
          rf[j] <- tail(randomForest(x=w, y=y, ntree=1000, mtry=i/3, ...)$err.rate[,1], n=1)
        }
      }
    }
    if (type=="reg") {
      if (i <= n) {
        for (j in 1:nfor.interp) {
          rf[j] <- tail(randomForest(x=w, y=y, ...)$mse, n=1)
        }
      }
      else {
        for (j in 1:nfor.interp) {
          rf[j] <- tail(randomForest(x=w, y=y, ntree=1000, ...)$mse, n=1)
        }
      }
    }
    err.interp[i] <- mean(rf)
    sd.interp[i] <- sd(rf)
  }
  
  var.min <- which.min(err.interp)
  sd.min <- sd.interp[var.min]
  
  nvarselect <- min( which(err.interp <= (err.interp[var.min] + nsd*sd.min)) )
  varselect <- vars[1:nvarselect]

  cl <- match.call()
  cl[[1]] <- as.name("VSURF.interp")

  comput.time <- Sys.time()-start

  output <- list('varselect.interp'=varselect,
                 'err.interp'=err.interp,
                 'sd.min'=sd.min,
                 'num.varselect.interp'=nvarselect,
                 'varselect.thres' = vars,
                 'comput.time'=comput.time,
                 'call'=cl)
  class(output) <- "VSURF.interp"
  output
}


#' @rdname VSURF.interp
#' @method VSURF.interp formula
#' @export
VSURF.interp.formula <- function(formula, data, ..., na.action = na.fail) {
### formula interface for VSURF.interp.
### code gratefully stolen from svm.formula (package e1071).
###
    if (!inherits(formula, "formula"))
        stop("method is only for formula objects")
    m <- match.call(expand.dots = FALSE)
    ## Catch xtest and ytest in arguments.
    if (any(c("xtest", "ytest") %in% names(m)))
        stop("xtest/ytest not supported through the formula interface")
    names(m)[2] <- "formula"
    if (is.matrix(eval(m$data, parent.frame())))
        m$data <- as.data.frame(data)
    m$... <- NULL
    m$na.action <- na.action
    m[[1]] <- as.name("model.frame")
    m <- eval(m, parent.frame())
    y <- model.response(m)
    Terms <- attr(m, "terms")
    attr(Terms, "intercept") <- 0
    ## Drop any "negative" terms in the formula.
    ## test with:
    ## randomForest(Fertility~.-Catholic+I(Catholic<50),data=swiss,mtry=2)
    m <- model.frame(terms(reformulate(attributes(Terms)$term.labels)),
                     data.frame(m))
    ## if (!is.null(y)) m <- m[, -1, drop=FALSE]
    for (i in seq(along=ncol(m))) {
        if (is.ordered(m[[i]])) m[[i]] <- as.numeric(m[[i]])
    }
    ret <- VSURF.interp(m, y, ...)
    cl <- match.call()
    cl[[1]] <- as.name("VSURF.interp")
    ret$call <- cl
    ret$terms <- Terms
    if (!is.null(attr(m, "na.action")))
        ret$na.action <- attr(m, "na.action")
    class(ret) <- c("VSURF.interp.formula", "VSURF.interp")
    warning(
        "VSURF with a formula-type call outputs selected variables
  which are indices of the input matrix based on the formula:
  you may reorder these to get indices of the original data")
    return(ret)
}

#' @rdname VSURF.interp
#' @method VSURF.interp.parallel default
#' @export VSURF.interp.parallel.default
VSURF.interp.parallel.default <- function(
    x, y, vars, nfor.interp=25, nsd=1,
    clusterType="PSOCK", ncores=detectCores()-1, ...) {
  
  # vars: selected variables indices after thresholding step
  # nfor.interp: number of forests to estimate each model
  # nsd: number of standard deviation: the selected model leads to an OOB error
  # smaller than the min error + nsd * (sd of the min error)

  start <- Sys.time()
  
  # one forest run to determine the problem type: classification or regression
  rf <- randomForest(x=x, y=y, ...)
  if (rf$type=="classification") {
    type <- "classif"
  }
  if (rf$type=="regression") {
    type <- "reg"
  } 
  
  nvars <- length(vars)
  n <- nrow(x)
  err.interp <- rep(NA, nvars)
  sd.interp <- rep(NA, nvars)
  
  rf.interp.classif <- function(i, ...) {
    rf <- rep(NA, nfor.interp)
    u <- vars[1:i]
    w <- x[,u, drop=FALSE]
    
    if (i <= n) {
      for (j in 1:nfor.interp) {
        rf[j] <- tail(randomForest(x=w, y=y, ...)$err.rate[,1], n=1)
      }
    }
    
    else {
      for (j in 1:nfor.interp) {
        rf[j] <- tail(randomForest(x=w, y=y, ntree=1000, mtry=i/3, ...)$err.rate[,1], n=1)
      }
    }
    out <- c(mean(rf), sd(rf))
  }
  
  rf.interp.reg <- function(i, ...) {
    rf <- rep(NA, nfor.interp)
    u <- vars[1:i]
    w <- x[,u, drop=FALSE]
    
    if (i <= n) {
      for (j in 1:nfor.interp) {
        rf[j] <- tail(randomForest(x=w, y=y, ...)$mse, n=1)
      }
    }
    
    else {
      for (j in 1:nfor.interp) {
        rf[j] <- tail(randomForest(x=w, y=y, ntree=1000, ...)$mse, n=1)
      }
    }
    out <- c(mean(rf), sd(rf))
  }
  
  ncores <- min(nvars, ncores)
  
  if (clusterType=="FORK") {
    if (type=="classif") {
      res <- mclapply(X=1:nvars, FUN=rf.interp.classif, ..., mc.cores=ncores,
                      mc.preschedule=FALSE)
    }
    
    if (type=="reg") {
      res <- mclapply(X=1:nvars, FUN=rf.interp.reg, ..., mc.cores=ncores,
                      mc.preschedule=FALSE)
    }
    
  }
  
  else {
    
    clust <- makeCluster(spec=ncores, type=clusterType)
    registerDoParallel(clust)
    
 #   i <- NULL #to avoid check NOTE...
    
    if (type=="classif") {
      res <- foreach(i=1:nvars, .packages="randomForest") %dopar% {
        out <- rf.interp.classif(i, ...)
      }
    }
    
    if (type=="reg") {
      res <- foreach(i=1:nvars, .packages="randomForest") %dopar% {
        out <- rf.interp.reg(i, ...)
      }
    }
    
    stopCluster(clust)
  }
 
  for (i in 1:nvars) {
    err.interp[i] <- res[[i]][1]
    sd.interp[i] <- res[[i]][2]
  }
 
  var.min <- which.min(err.interp)
  sd.min <- sd.interp[var.min]

  nvarselect <- min( which(err.interp <= (err.interp[var.min] + nsd*sd.min)) )
  varselect <- vars[1:nvarselect]

  cl <- match.call()
  cl[[1]] <- as.name("VSURF.interp")

  comput.time <- Sys.time()-start

  output <- list('varselect.interp'=varselect,
                 'err.interp'=err.interp,
                 'sd.min'=sd.min,
                 'num.varselect.interp'=nvarselect,
                 'varselect.thres' = vars,
                 'comput.time'=comput.time,
                 'clusterType'=clusterType,
                 'ncores'=ncores,
                 'call'=cl)
  class(output) <- "VSURF.interp"
  output
}


#' @rdname VSURF.interp
#' @method VSURF.interp.parallel formula
#' @export VSURF.interp.parallel.formula
VSURF.interp.parallel.formula <- function(formula, data, ..., na.action = na.fail) {
### formula interface for VSURF.interp.parallel.
### code gratefully stolen from svm.formula (package e1071).
###
    if (!inherits(formula, "formula"))
        stop("method is only for formula objects")
    m <- match.call(expand.dots = FALSE)
    ## Catch xtest and ytest in arguments.
    if (any(c("xtest", "ytest") %in% names(m)))
        stop("xtest/ytest not supported through the formula interface")
    names(m)[2] <- "formula"
    if (is.matrix(eval(m$data, parent.frame())))
        m$data <- as.data.frame(data)
    m$... <- NULL
    m$na.action <- na.action
    m[[1]] <- as.name("model.frame")
    m <- eval(m, parent.frame())
    y <- model.response(m)
    Terms <- attr(m, "terms")
    attr(Terms, "intercept") <- 0
    ## Drop any "negative" terms in the formula.
    ## test with:
    ## randomForest(Fertility~.-Catholic+I(Catholic<50),data=swiss,mtry=2)
    m <- model.frame(terms(reformulate(attributes(Terms)$term.labels)),
                     data.frame(m))
    ## if (!is.null(y)) m <- m[, -1, drop=FALSE]
    for (i in seq(along=ncol(m))) {
        if (is.ordered(m[[i]])) m[[i]] <- as.numeric(m[[i]])
    }
    ret <- VSURF.interp.parallel(m, y, ...)
    cl <- match.call()
    cl[[1]] <- as.name("VSURF.interp")
    ret$call <- cl
    ret$terms <- Terms
    if (!is.null(attr(m, "na.action")))
        ret$na.action <- attr(m, "na.action")
    class(ret) <- c("VSURF.interp.formula", "VSURF.interp")
    warning(
        "VSURF with a formula-type call outputs selected variables
  which are indices of the input matrix based on the formula:
  you may reorder these to get indices of the original data")
    return(ret)
}


#' @export
VSURF.interp <- function (x, ...) {
  UseMethod("VSURF.interp")
}


#' @export
VSURF.interp.parallel <- function (x, ...) {
    UseMethod("VSURF.interp.parallel")
}
