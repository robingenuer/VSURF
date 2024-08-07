#' Prediction step of VSURF
#'
#' Prediction step refines the selection of interpretation step
#' \code{\link{VSURF_interp}} by eliminating redundancy in the set of variables
#' selected, for prediction purpose. This is the third step of the
#' \code{\link{VSURF}} function.
#'
#' \code{nfor.pred} embedded random forests models are grown, starting with the
#' random forest build with only the most important variable. Variables are
#' added to the model in a stepwise manner. The mean jump value \code{mean.jump}
#' is calculated using variables that have been left out by interpretation step,
#' and is set as the mean absolute difference between mean OOB errors of one
#' model and its first following model. Hence a variable is included in the
#' model if the mean OOB error decrease is larger than \code{nmj} *
#' \code{mean.jump}.
#'
#' Note that, the \code{mtry} parameter of \code{randomForest} is set to its
#' default value (see \code{\link[randomForest]{randomForest}}) if \code{nvm}, the number of
#' variables in the model, is not greater than the number of observations, while
#' it is set to \code{nvm/3} otherwise. This is to ensure quality of OOB error
#' estimations along embedded RF models.
#'
#' @param x,formula A data frame or a matrix of predictors, the columns
#'   represent the variables. Or a formula describing the model to be fitted.
#' @param err.interp A vector of the mean OOB error rates of the embedded random
#'   forests models build during interpretation step (value \code{err.interp} of
#'   function \code{\link{VSURF_interp}}).
#' @param varselect.interp A vector of indices of variables selected after
#'   interpretation step.
#' @param ntree.pred Number of trees of each forest grown.   
#' @param nfor.pred Number of forests grown.
#' @inheritParams VSURF
#'
#' @return An object of class \code{VSURF_pred}, which is a list with the
#'   following components:
#'
#'   \item{varselect.pred}{A vector of indices of variables selected after
#'   "prediction step".}
#'
#'   \item{err.pred}{A vector of the mean OOB error rates of the random forests
#'   models build during the "prediction step".}
#'
#'   \item{mean.jump}{The mean jump value computed during the "prediction
#'   step".}
#'
#'   \item{num.varselect.pred}{The number of selected variables.}
#'
#'   \item{nmj}{Value of the parameter in the call.}
#'
#'   \item{comput.time}{Computation time.}
#'
#'   \item{RFimplem}{The RF implementation used to run
#'   \code{VSURF_pred}.}
#'
#'   \item{call}{The original call to \code{VSURF}.}
#'
#'   \item{terms}{Terms associated to the formula (only if formula-type call was
#'   used).}
#'
#' @author Robin Genuer, Jean-Michel Poggi and Christine Tuleau-Malot
#' @seealso \code{\link{VSURF}}
#' @references Genuer, R. and Poggi, J.M. and Tuleau-Malot, C. (2010),
#'   \emph{Variable selection using random forests}, Pattern Recognition Letters
#'   31(14), 2225-2236
#' @references Genuer, R. and Poggi, J.M. and Tuleau-Malot, C. (2015),
#'   \emph{VSURF: An R Package for Variable Selection Using Random Forests}, The
#'   R Journal 7(2):19-33
#'
#' @examples
#' 
#' data(iris)
#' iris.thres <- VSURF_thres(iris[,1:4], iris[,5])
#' iris.interp <- VSURF_interp(iris[,1:4], iris[,5],
#'   vars = iris.thres$varselect.thres)
#' iris.pred <- VSURF_pred(iris[,1:4], iris[,5],
#'   err.interp = iris.interp$err.interp,
#'   varselect.interp = iris.interp$varselect.interp)
#' iris.pred
#' 
#' \dontrun{
#' # A more interesting example with toys data (see \code{\link{toys}})
#' # (a few minutes to execute)
#' data(toys)
#' toys.thres <- VSURF_thres(toys$x, toys$y)
#' toys.interp <- VSURF_interp(toys$x, toys$y,
#'   vars = toys.thres$varselect.thres)
#' toys.pred <- VSURF_pred(toys$x, toys$y, err.interp = toys.interp$err.interp,
#'   varselect.interp = toys.interp$varselect.interp)
#' toys.pred}
#'
#' @export
VSURF_pred <- function (x, ...) {
  UseMethod("VSURF_pred")
}

#' @rdname VSURF_pred
#' @export
VSURF_pred.default <-function(x, y, err.interp, varselect.interp,
  ntree.pred = 100, nfor.pred = 10, nmj = 1, RFimplem = "randomForest", parallel = FALSE,
  ncores = detectCores()-1, verbose = TRUE, ntree = NULL, ...) {
  
  # err.interp: interpretation models errors
  # varselect.interp: interpretation variables indices
  # nmj: number of mean jump: addition of a variable if it gives an error
  # reduction of at least nmj * mean jump value
  
  start <- Sys.time()
  
  if (!is.null(ntree)) cat(paste(
    "\nntree parameter is deprecated, please use ntree.pred instead\n"))
  
  if (verbose == TRUE) cat(paste("\nPrediction step (on", length(varselect.interp),
  "variables)\n"))
  
  # determinination the problem type: classification or regression
  # (code gratefully stolen from randomForest.default function of
  # randomForest package)
  classRF <- is.factor(y)
  if (!classRF && length(unique(y)) <= 5) {
    warning("The response has five or fewer unique values.
            Are you sure you want to do regression?")
  }
  if (classRF && length(unique(y)) < 2)
    stop("Need at least two classes to do classification.")
  
  if (classRF) {
    type <- "classif"
  } else {
    type <- "reg"
  }
  
  if (verbose == TRUE) {
    if (RFimplem == "randomForest") {
      timeOneRFAllVar <- system.time(
        randomForest::randomForest(x = x[, varselect.interp, drop=FALSE], y = y,
                                   ntree = ntree.pred, ...))
    }
    if (RFimplem == "ranger") {
      timeOneRFAllVar <- system.time(
        ranger::ranger(dependent.variable.name="y",
               data = cbind(x[, varselect.interp, drop=FALSE], "y" = y),
               num.trees = ntree.pred, num.threads = 1, ...))
    }
    if (RFimplem == "Rborist") {
      timeOneRFAllVar <- system.time(
        Rborist::Rborist(x = x[, varselect.interp, drop=FALSE], y = y,
                         minInfo = 0, nTree = ntree.pred, nThread = 1, ...))
    }
    cat(paste("Maximum estimated computational time (on one core):",
      round(length(varselect.interp) * nfor.pred * timeOneRFAllVar[3], 1),
      "sec.\n"))
  }
  
  # initialization of the progress bar
  if (verbose == TRUE) {
    pb <- utils::txtProgressBar(style = 3)
    nBar <- 1
  }
  
  k <- length(err.interp)
  l <- length(varselect.interp)
  
  if (k==l) {
    warning(
      "Unable to perform prediction step, because the interpretation step
did not eliminate variables")
    varselect.pred <- NULL
    err.pred <- NULL
    mean.jump <- NULL
  } else {
    # mean jump calculation
    s=NULL
    for (i in l:(k-1)){
      s <- c(s, abs(err.interp[i+1] - err.interp[i]) )
    }
    mean.jump <- mean(s)
    
    # comparison between the error with the variable and the precedent error
    # and test of the addition of the variable
    n <- nrow(x)
    varselect.pred <- varselect.interp[1]
    u <- varselect.pred
    w <- x[, u, drop=FALSE]
    rf <- rep(NA, nfor.pred)
    
    if (RFimplem == "randomForest") {
      if (type=="classif") {
        for (j in 1:nfor.pred) {
          rf[j] <- tail(randomForest::randomForest(x=w, y=y, ntree = ntree.pred,
                                                   ...)$err.rate[,1], n=1)
        }
        err.pred <- mean(rf)
      }
      if (type=="reg") {
        for (j in 1:nfor.pred) {
          rf[j] <- tail(randomForest::randomForest(x=w, y=y, ntree = ntree.pred,
                                                   ...)$mse, n=1)
        }
        err.pred <- mean(rf)
      }
    }
    if (RFimplem == "ranger") {
      dat <- cbind(w, "y" = y)
      for (j in 1:nfor.pred) {
        rf[j] <- ranger::ranger(dependent.variable.name="y", data=dat,
                                num.threads = ifelse(parallel, ncores, 1),
                                num.trees=ntree.pred, ...)$prediction.error
      }
      err.pred <- mean(rf)
    }
    if (RFimplem == "Rborist") {
      for (j in 1:nfor.pred) {
        rf[j] <- Rborist::Rborist(x = w, y = y, nTree = ntree.pred, minInfo = 0,
                                  nThread = ifelse(parallel, ncores, 1),
                                  ...)$validation$oobError
      }
      err.pred <- mean(rf)
    }
    t <- err.pred
    
    if (verbose == TRUE) {
      utils::setTxtProgressBar(pb, nBar/l)
      nBar <- nBar + 1
    }
    
    if (l>1) {
      for (i in 2:l){
        u <- c(varselect.pred, varselect.interp[i])
        w <- x[, u, drop=FALSE]
        rf <- rep(NA, nfor.pred)
        if (RFimplem == "randomForest") {
          if (type=="classif") {
            if (ncol(w) <= n) {
              for (j in 1:nfor.pred) {
                rf[j] <- tail(randomForest::randomForest(x=w, y=y,
                                ntree = ntree.pred, ...)$err.rate[,1], n=1)
              }
            } else {
              for (j in 1:nfor.pred) {
                rf[j] <- tail(randomForest::randomForest(x=w, y=y,
                                ntree = ntree.pred,
                                mtry=ncol(w)/3, ...)$err.rate[,1], n=1)
              }
            }
            z <- mean(rf)
          }
          if (type=="reg") {
            for (j in 1:nfor.pred) {
              rf[j] <- tail(randomForest::randomForest(x=w, y=y,
                              ntree = ntree.pred, ...)$mse, n=1)
            }
            z <- mean(rf)
          }
        }
        if (RFimplem == "ranger") {
          dat <- cbind(w, "y" = y)
          if (ncol(w) <= n) {
            for (j in 1:nfor.pred) {
              rf[j] <- ranger::ranger(dependent.variable.name="y", data=dat,
                         num.threads = ifelse(parallel, ncores, 1),
                         num.trees = ntree.pred, ...)$prediction.error
            }
          } else {
            for (j in 1:nfor.pred) {
              rf[j] <- ranger::ranger(dependent.variable.name="y", data=dat,
                         num.threads = ifelse(parallel, ncores, 1),
                         num.trees = ntree.pred,
                         mtry=ncol(w)/3, ...)$prediction.error
            }
          }
          z <- mean(rf)
        }
        if (RFimplem == "Rborist") {
          if (ncol(w) <= n) {
            for (j in 1:nfor.pred) {
              rf[j] <- Rborist::Rborist(x = w, y = y, nTree = ntree.pred,
                         minInfo = 0, nThread = ifelse(parallel, ncores, 1),
                         ...)$validation$oobError
            }
          } else {
            for (j in 1:nfor.pred) {
              rf[j] <- Rborist::Rborist(x = w, y = y, nTree = ntree.pred,
                         minInfo = 0, nThread = ifelse(parallel, ncores, 1),
                         predFixed = ncol(w)/3, ...)$validation$oobError
            }
          }
          z <- mean(rf)
        }
        if ((t-z) > nmj*mean.jump){
          varselect.pred <- c(varselect.pred, varselect.interp[i])
          err.pred <- c(err.pred, z)
          t <- z
        }
        if (verbose == TRUE) {
          utils::setTxtProgressBar(pb, nBar/l)
          nBar <- nBar + 1
        }
      }
    }
  }
  
  cl <- match.call()
  cl[[1]] <- as.name("VSURF_pred")
  
  comput.time <- Sys.time()-start
  
  output <- list('varselect.pred'=varselect.pred,
                 'err.pred'=err.pred,
                 'mean.jump'=mean.jump,
                 'num.varselect.pred'=length(varselect.pred),
                 'nmj' = nmj,
                 'comput.time'=comput.time,
                 'RFimplem' = RFimplem,
                 'call'=cl)
  class(output) <- c("VSURF_pred")
  output
}


#' @rdname VSURF_pred
#' @export
VSURF_pred.formula <- function(formula, data, ..., na.action = na.fail) {
### formula interface for VSURF_pred.
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
    ret <- VSURF_pred(m, y, ...)
    cl <- match.call()
    cl[[1]] <- as.name("VSURF_pred")
    ret$call <- cl
    ret$terms <- Terms
    if (!is.null(attr(m, "na.action")))
        ret$na.action <- attr(m, "na.action")
    class(ret) <- c("VSURF_pred.formula", class(ret))
    warning(
        "VSURF with a formula-type call outputs selected variables
  which are indices of the input matrix based on the formula:
  you may reorder these to get indices of the original data")
    return(ret)
}
