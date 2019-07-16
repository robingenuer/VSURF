#'Interpretation step of VSURF
#'
#'Interpretation step aims to select all variables related to the response for
#'interpretation purpose. This is the second step of the \code{\link{VSURF}}
#'function. It is designed to be executed after the thresholding step
#'\code{\link{VSURF_thres}}.
#'
#'\code{nfor.interp} embedded random forests models are grown, starting with the
#'random forest build with only the most important variable and ending with all
#'variables.  Then, \code{err.min} the minimum mean out-of-bag (OOB) error rate
#'of these models and its associated standard deviation \code{sd.min} are
#'computed.  Finally, the smallest model (and hence its corresponding variables)
#'having a mean OOB error less than \code{err.min} + \code{nsd} * \code{sd.min}
#'is selected.
#'
#'Note that, the \code{mtry} parameter of \code{randomForest} is set to its
#'default value (see \code{\link{randomForest}}) if \code{nvm}, the number of
#'variables in the model, is not greater than the number of observations, while
#'it is set to \code{nvm/3} otherwise. This is to ensure quality of OOB error
#'estimations along embedded RF models.
#'
#'@param x,formula A data frame or a matrix of predictors, the columns represent
#'  the variables. Or a formula describing the model to be fitted.
#'@param vars A vector of variable indices. Typically, indices of variables
#'  selected by thresholding step (see value \code{varselect.thres} of
#'  \code{\link{VSURF_thres}} function).
#'@param nfor.interp Number of forests grown.
#'@inheritParams VSURF
#'
#'@return An object of class \code{VSURF_interp}, which is a list with the
#'  following components:
#'
#'  \item{varselect.interp}{A vector of indices of selected variables.}
#'
#'  \item{err.interp}{A vector of the mean OOB error rates of the embedded
#'  random forests models.}
#'
#'  \item{sd.min}{The standard deviation of OOB error rates associated to the
#'  random forests model attaining the minimum mean OOB error rate.}
#'
#'  \item{num.varselect.interp}{The number of selected variables.}
#'
#'  \item{varselect.thres}{A vector of indexes of variables selected after
#'  "thresholding step", sorted according to their mean VI, in decreasing
#'  order.}
#'
#'  \item{nsd}{Value of the parameter in the call.}
#'
#'  \item{comput.time}{Computation time.}
#'
#'  \item{RFimplem}{The RF implementation used to run
#'  \code{VSURF_interp}.}
#'
#'  \item{ncores}{The number of cores used to run \code{VSURF_interp} in
#'  parallel (NULL if VSURF_interp did not run in parallel).}
#'
#'  \item{clusterType}{The type of the cluster used to run \code{VSURF_interp}
#'  in parallel (NULL if VSURF_interp did not run in parallel).}
#'
#'  \item{call}{The original call to \code{VSURF}.}
#'
#'  \item{terms}{Terms associated to the formula (only if formula-type call was
#'  used).}
#'
#'@author Robin Genuer, Jean-Michel Poggi and Christine Tuleau-Malot
#'@seealso \code{\link{VSURF}}, \code{\link{tune}}
#'@references Genuer, R. and Poggi, J.M. and Tuleau-Malot, C. (2010),
#'  \emph{Variable selection using random forests}, Pattern Recognition Letters
#'  31(14), 2225-2236
#'@references Genuer, R. and Poggi, J.M. and Tuleau-Malot, C. (2015),
#'  \emph{VSURF: An R Package for Variable Selection Using Random Forests}, The
#'  R Journal 7(2):19-33
#'
#' @examples
#'
#' data(iris)
#' iris.thres <- VSURF_thres(iris[,1:4], iris[,5], ntree = 100, nfor.thres = 20)
#' iris.interp <- VSURF_interp(iris[,1:4], iris[,5],
#'   vars = iris.thres$varselect.thres, nfor.interp = 10)
#' iris.interp
#'
#' \dontrun{
#' # A more interesting example with toys data (see \code{\link{toys}})
#' # (a few minutes to execute)
#' data(toys)
#' toys.thres <- VSURF_thres(toys$x, toys$y)
#' toys.interp <- VSURF_interp(toys$x, toys$y,
#'   vars = toys.thres$varselect.thres)
#' toys.interp}
#'
#'@importFrom randomForest randomForest
#'@importFrom doParallel registerDoParallel
#'@importFrom foreach foreach %dopar%
#'@importFrom parallel makeCluster stopCluster mclapply detectCores
#'@importFrom ranger ranger
#'@importFrom Rborist Rborist
#'@export
VSURF_interp <- function (x, ...) {
  UseMethod("VSURF_interp")
}

#' @rdname VSURF_interp
#' @export
VSURF_interp.default <- function(
  x, y, ntree = 2000, vars, nfor.interp = 25, nsd = 1, 
  RFimplem = "randomForest", parallel = FALSE,
  ncores = detectCores()-1, clusterType = "PSOCK",
  verbose = TRUE, ...) {
  
  # vars: selected variables indices after thresholding step
  # nfor.interp: number of forests to estimate each model
  # nsd: number of standard deviation: the selected model leads to an OOB error
  # smaller than the min error + nsd * (sd of the min error)
  
  start <- Sys.time()
  
  if (verbose == TRUE) cat(paste("\nInterpretation step (on", length(vars), "variables)\n"))
  
  if (!parallel) {
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
  
  if (RFimplem == "randomForest") {
    rf.interp.classif <- function(i, nfor.interp, ...) {
      rf <- rep(NA, nfor.interp)
      u <- vars[1:i]
      w <- x[, u, drop=FALSE]
      
      if (i <= n) {
        for (j in 1:nfor.interp) {
          rf[j] <- tail(randomForest::randomForest(x=w, y=y, ...)$err.rate[,1], n=1)
        }
      } else {
        for (j in 1:nfor.interp) {
          rf[j] <- tail(randomForest::randomForest(x=w, y=y, mtry=i/3, ...)$err.rate[,1], n=1)
        }
      }
      
      out <- c(mean(rf), sd(rf))
    }
    
    rf.interp.reg <- function(i, nfor.interp, ...) {
      rf <- rep(NA, nfor.interp)
      u <- vars[1:i]
      w <- x[,u, drop=FALSE]
      
      for (j in 1:nfor.interp) {
        rf[j] <- tail(randomForest::randomForest(x=w, y=y, ...)$mse, n=1)
      }
      
      out <- c(mean(rf), sd(rf))
    }
  }
  if (RFimplem == "ranger") {
    rf.interp.ranger <- function(i, nfor.interp, ...) {
      rf <- rep(NA, nfor.interp)
      u <- vars[1:i]
      w <- x[, u, drop=FALSE]
      
      dat <- cbind(w, "y" = y)
      
      if (i <= n) {
        for (j in 1:nfor.interp) {
          rf[j] <- ranger::ranger(dependent.variable.name="y", data=dat,
                                  num.trees = ntree, ...)$prediction.error
        }
      } else {
        for (j in 1:nfor.interp) {
          rf[j] <- ranger::ranger(dependent.variable.name="y", data=dat,
                                  num.trees = ntree,
                                  mtry=i/3, ...)$prediction.error
        }
      }
      
      return(c(mean(rf), sd(rf)))
    }
  }
  if (RFimplem == "Rborist") {
    rf.interp.Rborist <- function(i, nfor.interp, ...) {
      rf <- rep(NA, nfor.interp)
      u <- vars[1:i]
      w <- x[, u, drop=FALSE]
      
      dat <- cbind(w, "y" = y)
      
      if (i <= n) {
        for (j in 1:nfor.interp) {
          rf[j] <- Rborist::Rborist(x = w, y = y, nTree = ntree, minInfo = 0,
                                    ...)$validation$oobError
        }
      } else {
        for (j in 1:nfor.interp) {
          rf[j] <- Rborist::Rborist(x = w, y = y, nTree = ntree, minInfo = 0,
                                    predFixed = i/3, ...)$validation$oobError
        }
      }
      
      return(c(mean(rf), sd(rf)))
      
    }
  }
  
  if (verbose == TRUE) {
    if (RFimplem == "randomForest") {
      if (type=="classif") {
        timeOneRFOneVar <- system.time(rf.interp.classif(1, 1, ...))
        timeOneRFAllVar <- system.time(rf.interp.classif(nvars, 1, ...))
      }
      if (type=="reg") {
        timeOneRFOneVar <- system.time(rf.interp.reg(1, 1, ...))
        timeOneRFAllVar <- system.time(rf.interp.reg(nvars, 1, ...))
      }
    }
    if (RFimplem == "ranger") {
      timeOneRFOneVar <- system.time(rf.interp.ranger(1, 1, num.threads = 1, ...))
      timeOneRFAllVar <- system.time(rf.interp.ranger(nvars, 1, num.threads = 1, ...))
    }
    if (RFimplem == "Rborist") {
      timeOneRFOneVar <- system.time(rf.interp.Rborist(1, 1, nThread = 1, ...))
      timeOneRFAllVar <- system.time(rf.interp.Rborist(nvars, 1, nThread = 1, ...))
    }
    cat(paste("Estimated computational time (on one core): between",
              round(nvars * nfor.interp * timeOneRFOneVar[3], 1), "sec. and ",
              round(nvars * nfor.interp * timeOneRFAllVar[3], 1), "sec.\n"))
  }
  
  # initialization of the progress bar
  if (verbose == TRUE & (parallel == FALSE | clusterType %in% c("ranger", "Rborist"))) {
    pb <- utils::txtProgressBar(style = 3)
    nBar <- 1
  }
  
  if (!parallel) {
    if (RFimplem == "randomForest") {
      if (type=="classif") {
        for (i in 1:nvars){
          res <- rf.interp.classif(i, nfor.interp, ...)
          err.interp[i] <- res[1]
          sd.interp[i] <- res[2]
          if (verbose == TRUE) {
            utils::setTxtProgressBar(pb, nBar/nvars)
            nBar <- nBar + 1
          }
        }
      }
      if (type=="reg") {
        for (i in 1:nvars){
          res <- rf.interp.reg(i, nfor.interp, ...)
          err.interp[i] <- res[1]
          sd.interp[i] <- res[2]
          if (verbose == TRUE) {
            utils::setTxtProgressBar(pb, nBar/nvars)
            nBar <- nBar + 1
          }
        }
      }
    }
    if (RFimplem == "ranger") {
      for (i in 1:nvars){
        res <- rf.interp.ranger(i, nfor.interp, num.threads = 1, ...)
        err.interp[i] <- res[1]
        sd.interp[i] <- res[2]
        if (verbose == TRUE) {
          utils::setTxtProgressBar(pb, nBar/nvars)
          nBar <- nBar + 1
        }
      }
    }
    if (RFimplem == "Rborist") {
      for (i in 1:nvars){
        res <- rf.interp.Rborist(i, nfor.interp, nThread = 1, ...)
        err.interp[i] <- res[1]
        sd.interp[i] <- res[2]
        if (verbose == TRUE) {
          utils::setTxtProgressBar(pb, nBar/nvars)
          nBar <- nBar + 1
        }
      }
    }
  } else {
    if (clusterType == "ranger") {
      if (RFimplem != "ranger") stop("RFimplem must be set to 'ranger' to use clusterType 'ranger'")
      for (i in 1:nvars){
        res <- rf.interp.ranger(i, nfor.interp, num.threads = ncores, ...)
        err.interp[i] <- res[1]
        sd.interp[i] <- res[2]
        if (verbose == TRUE) {
          utils::setTxtProgressBar(pb, nBar/nvars)
          nBar <- nBar + 1
        }
      }
    } else {
      if (clusterType == "Rborist") {
        if (RFimplem != "Rborist") stop("RFimplem must be set to 'Rborist' to use clusterType 'Rborist'")
        for (i in 1:nvars){
          res <- rf.interp.Rborist(i, nfor.interp, nThread = ncores, ...)
          err.interp[i] <- res[1]
          sd.interp[i] <- res[2]
          if (verbose == TRUE) {
            utils::setTxtProgressBar(pb, nBar/nvars)
            nBar <- nBar + 1
          }
        }
      } else {
        ncores <- min(nvars, ncores)
        
        if (clusterType=="FORK") {
          if (RFimplem == "randomForest") {
            if (type=="classif") {
              res <- parallel::mclapply(X=1:nvars, FUN=rf.interp.classif,
                                        nfor.interp = nfor.interp, ...,
                                        mc.cores=ncores, mc.preschedule=FALSE)
            }
            if (type=="reg") {
              res <- parallel::mclapply(X=1:nvars, FUN=rf.interp.reg,
                                        nfor.interp = nfor.interp, ...,
                                        mc.cores=ncores, mc.preschedule=FALSE)
            }
          }
          if (RFimplem == "ranger") {
            res <- parallel::mclapply(X=1:nvars, FUN=rf.interp.ranger,
                                      nfor.interp = nfor.interp,
                                      num.threads = 1, ...,
                                      mc.cores=ncores, mc.preschedule=FALSE)
          }
          if (RFimplem == "Rborist") {
            res <- parallel::mclapply(X=1:nvars, FUN=rf.interp.Rborist,
                                      nfor.interp = nfor.interp,
                                      nThread = 1, ...,
                                      mc.cores=ncores, mc.preschedule=FALSE)
          }
        } else {
          clust <- parallel::makeCluster(spec=ncores, type=clusterType)
          doParallel::registerDoParallel(clust)
          # i <- NULL #to avoid check NOTE...
          
          if (RFimplem == "randomForest") {
            if (type=="classif") {
              res <- foreach::foreach(i=1:nvars, .packages="randomForest") %dopar% {
                out <- rf.interp.classif(i, nfor.interp, ...)
              }
            }
            if (type=="reg") {
              res <- foreach::foreach(i=1:nvars, .packages="randomForest") %dopar% {
                out <- rf.interp.reg(i, nfor.interp, ...)
              }
            }
          }
          if (RFimplem == "ranger") {
            res <- foreach::foreach(i=1:nvars, .packages="ranger") %dopar% {
              out <- rf.interp.ranger(i, nfor.interp, num.threads = 1, ...)
            }
          }
          if (RFimplem == "Rborist") {
            res <- foreach::foreach(i=1:nvars, .packages="Rborist") %dopar% {
              out <- rf.interp.Rborist(i, nfor.interp, nThread = 1, ...)
            }
          }
          parallel::stopCluster(clust)
        }
        
        for (i in 1:nvars) {
          err.interp[i] <- res[[i]][1]
          sd.interp[i] <- res[[i]][2]
        }
      }
    }
  }
  
  var.min <- which.min(err.interp)
  sd.min <- sd.interp[var.min]
  
  nvarselect <- min( which(err.interp <= (err.interp[var.min] + nsd*sd.min)) )
  varselect <- vars[1:nvarselect]
  
  cl <- match.call()
  cl[[1]] <- as.name("VSURF_interp")
  
  if (!parallel) clusterType <- NULL
  
  comput.time <- Sys.time()-start
  
  output <- list('varselect.interp'=varselect,
                 'err.interp'=err.interp,
                 'sd.min'=sd.min,
                 'num.varselect.interp'=nvarselect,
                 'varselect.thres' = vars,
                 'nsd' = nsd,
                 'comput.time'=comput.time,
                 'RFimplem'=RFimplem,
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
