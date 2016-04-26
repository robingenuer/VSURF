#' Interpretation step of VSURF
#' 
#' Interpretation step aims to select all variables related to the response for
#' interpretation prupose. This is the second step of the \code{\link{VSURF}}
#' function. It is designed to be executed after the thresholding step
#' \code{\link{VSURF_thres}}.
#' 
#' \code{nfor.interp} embedded random forests models are grown, starting with
#' the random forest build with only the most important variable and ending
#' with all variables.  Then, \code{err.min} the minimum mean out-of-bag (OOB)
#' error rate of these models and its associated standard deviation
#' \code{sd.min} are computed.  Finally, the smallest model (and hence its
#' corresponding variables) having a mean OOB error less than \code{err.min} +
#' \code{nsd} * \code{sd.min} is selected.
#' 
#' Note that,
#' the \code{mtry} parameter of \code{randomForest} is set to its default value
#' (see \code{\link{randomForest}}) if \code{nvm}, the number of variables
#' in the model, is not greater than the number of observations,
#' while it is set to \code{nvm/3} otherwise. This is to ensure quality of OOB
#' error estimations along embedded RF models.
#' 
#' @param data a data frame containing the variables in the model.
#' @param na.action A function to specify the action to be taken if NAs are
#' found.  (NOTE: If given, this argument must be named, and as
#' \code{randomForest} it is only used with the formula-type call.)
#' @param x,formula A data frame or a matrix of predictors, the columns
#' represent the variables. Or a formula describing the model to be fitted.
#' @param y A response vector (must be a factor for classification problems and
#' numeric for regression ones).
#' @param ntree Number of trees in each forests grown. Standard parameter of
#' \code{randomForest}.
#' @param vars A vector of variable indices. Typically, indices of variables
#' selected by thresholding step (see value \code{varselect.thres} of
#' \code{\link{VSURF_thres}} function).
#' @param nfor.interp Number of forests grown.
#' @param nsd Number of times the standard deviation of the minimum value of
#' \code{err.interp} is multiplied. See details below.
#' @param parallel A logical indicating if you want VSURF to run in parallel on
#' multiple cores (default to FALSE).
#' @param ncores Number of cores to use. Default is set to the number of cores
#' detected by R minus 1.
#' @param clusterType Type of the multiple cores cluster used to run VSURF in
#' parallel. Must be chosen among "PSOCK" (default: SOCKET cluster available
#' locally on all OS), "FORK" (local too, only available for Linux and Mac OS)
#' and "MPI" (can be used on a remote cluster, which needs \code{snow} and
#' \code{Rmpi} packages installed).
#' @param ...  others parameters to be passed on to the \code{randomForest}
#' function (see ?randomForest for further information).
#' 
#' @return An object of class \code{VSURF_interp}, which is a list with the
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
#' \item{nsd}{Value of the parameter in the call.}
#' 
#' \item{comput.time}{Computation time.}
#'
#'\item{ncores}{The number of cores used to run \code{VSURF_interp}
#'  in parallel (NULL if VSURF_interp did not run in parallel).}
#'
#' \item{clusterType}{The type of the cluster used to run
#' \code{VSURF_interp} in parallel (NULL if VSURF_interp did not run in parallel).}
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
#' @references Genuer, R. and Poggi, J.M. and Tuleau-Malot, C. (2015),
#' \emph{VSURF: An R Package for Variable Selection Using Random Forests},
#' The R Journal 7(2):19-33
#' 
#' @examples
#' 
#' data(iris)
#' iris.thres <- VSURF_thres(iris[,1:4], iris[,5], ntree = 100, nfor.thres = 20)
#' iris.interp <- VSURF_interp(iris[,1:4], iris[,5], vars = iris.thres$varselect.thres,
#'                             nfor.interp = 10)
#' iris.interp
#' 
#' \dontrun{
#' # A more interesting example with toys data (see \code{\link{toys}})
#' # (a few minutes to execute)
#' data(toys)
#' toys.thres <- VSURF_thres(toys$x, toys$y)
#' toys.interp <- VSURF_interp(toys$x, toys$y, vars = toys.thres$varselect.thres)
#' toys.interp}
#'
#' @importFrom randomForest randomForest
#' @importFrom doParallel registerDoParallel
#' @importFrom foreach foreach %dopar%
#' @importFrom parallel makeCluster stopCluster mclapply detectCores
#' @export
VSURF_interp <- function (x, ...) {
  UseMethod("VSURF_interp")
}

#' @rdname VSURF_interp
#' @export
VSURF_interp.default <- function(
  x, y, ntree = 2000, vars, nfor.interp = 25, nsd = 1, parallel = FALSE,
  ncores = detectCores()-1, clusterType = "PSOCK",  ...) {
  
  # vars: selected variables indices after thresholding step
  # nfor.interp: number of forests to estimate each model
  # nsd: number of standard deviation: the selected model leads to an OOB error
  # smaller than the min error + nsd * (sd of the min error)
  
  start <- Sys.time()
  
  if (!parallel) {
    clusterType <- NULL
    ncores <- NULL
  }  
  
  # determinination the problem type: classification or regression
  # (code gratefully stolen from randomForest.default function of randomForest package)
  classRF <- is.factor(y)
  if (!classRF && length(unique(y)) <= 5) {
    warning("The response has five or fewer unique values.  Are you sure you want to do regression?")
  }
  if (classRF && length(unique(y)) < 2)
    stop("Need at least two classes to do classification.")
  
  if (classRF) {
    type <- "classif"
  }
  else {
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
        rf[j] <- tail(randomForest::randomForest(x=w, y=y, ...)$err.rate[,1], n=1)
      }
    }
    
    else {
      for (j in 1:nfor.interp) {
        rf[j] <- tail(randomForest::randomForest(x=w, y=y, mtry=i/3, ...)$err.rate[,1], n=1)
      }
    }
    
    out <- c(mean(rf), sd(rf))
  }
  
  rf.interp.reg <- function(i, ...) {
    rf <- rep(NA, nfor.interp)
    u <- vars[1:i]
    w <- x[,u, drop=FALSE]
    
    for (j in 1:nfor.interp) {
      rf[j] <- tail(randomForest::randomForest(x=w, y=y, ...)$mse, n=1)
    }
    
    out <- c(mean(rf), sd(rf))
  }
  
  if (!parallel) {
    if (type=="classif") {
      for (i in 1:nvars){
        res <- rf.interp.classif(i, ...)
        err.interp[i] <- res[1]
        sd.interp[i] <- res[2]
      }
    }
    if (type=="reg") {
      for (i in 1:nvars){
        res <- rf.interp.reg(i, ...)
        err.interp[i] <- res[1]
        sd.interp[i] <- res[2]
      }
    }
  }  
  
  else {    
    ncores <- min(nvars, ncores)
    
    if (clusterType=="FORK") {
      if (type=="classif") {
        res <- parallel::mclapply(X=1:nvars, FUN=rf.interp.classif, ..., mc.cores=ncores,
                                  mc.preschedule=FALSE)
      }
      if (type=="reg") {
        res <- parallel::mclapply(X=1:nvars, FUN=rf.interp.reg, ..., mc.cores=ncores,
                                  mc.preschedule=FALSE)
      }
    }
    
    else { 
      clust <- parallel::makeCluster(spec=ncores, type=clusterType)
      doParallel::registerDoParallel(clust)
      # i <- NULL #to avoid check NOTE...
      
      if (type=="classif") {
        res <- foreach::foreach(i=1:nvars, .packages="randomForest") %dopar% {
          out <- rf.interp.classif(i, ...)
        }
      }
      if (type=="reg") {
        res <- foreach::foreach(i=1:nvars, .packages="randomForest") %dopar% {
          out <- rf.interp.reg(i, ...)
        }
      }     
      parallel::stopCluster(clust)
    }
    
    for (i in 1:nvars) {
      err.interp[i] <- res[[i]][1]
      sd.interp[i] <- res[[i]][2]
    }
  }
  
  var.min <- which.min(err.interp)
  sd.min <- sd.interp[var.min]
  
  nvarselect <- min( which(err.interp <= (err.interp[var.min] + nsd*sd.min)) )
  varselect <- vars[1:nvarselect]
  
  cl <- match.call()
  cl[[1]] <- as.name("VSURF_interp")
  
  comput.time <- Sys.time()-start
  
  output <- list('varselect.interp'=varselect,
                 'err.interp'=err.interp,
                 'sd.min'=sd.min,
                 'num.varselect.interp'=nvarselect,
                 'varselect.thres' = vars,
                 'nsd' = nsd,
                 'comput.time'=comput.time,
                 'ncores'=ncores,
                 'clusterType'=clusterType,
                 'call'=cl)
  class(output) <- c("VSURF_interp")
  output
}

#' @rdname VSURF_interp
#' @export
VSURF_interp.formula <- function(formula, data, ..., na.action = na.fail) {
### formula interface for VSURF_interp.
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
  attr(y, "na.action") <- attr(m, "na.action")
  ## Drop any "negative" terms in the formula.
  ## test with:
  ## randomForest(Fertility~.-Catholic+I(Catholic<50),data=swiss,mtry=2)
  m <- model.frame(terms(reformulate(attributes(Terms)$term.labels)),
                   data.frame(m))
  ## if (!is.null(y)) m <- m[, -1, drop=FALSE]
  for (i in seq(along=ncol(m))) {
    if (is.ordered(m[[i]])) m[[i]] <- as.numeric(m[[i]])
  }
  ret <- VSURF_interp.default(x=m, y=y, ...)
  cl <- match.call()
  cl[[1]] <- as.name("VSURF")
  ret$call <- cl
  ret$terms <- Terms
  if (!is.null(attr(y, "na.action"))) {
    ret$na.action <- attr(y, "na.action")
  }
  class(ret) <- c("VSURF_interp.formula", class(ret))
    warning(
        "VSURF with a formula-type call outputs selected variables
  which are indices of the input matrix based on the formula:
  you may reorder these to get indices of the original data")
    return(ret)
}
