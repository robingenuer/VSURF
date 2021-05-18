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
#' default value (see \code{\link{randomForest}}) if \code{nvm}, the number of
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
#' iris.thres <- VSURF_thres(iris[,1:4], iris[,5], ntree = 100, nfor.thres = 20)
#' iris.interp <- VSURF_interp(iris[,1:4], iris[,5],
#'   vars = iris.thres$varselect.thres, nfor.interp = 10)
#' iris.pred <- VSURF_pred(iris[,1:4], iris[,5],
#'   err.interp = iris.interp$err.interp,
#'   varselect.interp = iris.interp$varselect.interp, nfor.pred = 10)
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
#' @importFrom randomForest randomForest
#' @export
VSURF_pred <- function (x, ...) {
  UseMethod("VSURF_pred")
}

#' @rdname VSURF_pred
#' @export
VSURF_pred.default <-function(x, y, ntree = 2000, err.interp, varselect.interp,
  nfor.pred = 25, nmj = 1, RFimplem = "randomForest", parallel = FALSE,
  ncores = detectCores()-1, verbose = TRUE, ...) {
  
  # err.interp: interpretation models errors
  # varselect.interp: interpretation variables indices
  # nmj: number of mean jump: addition of a variable if it gives an error
  # reduction of at least nmj * mean jump value
  
  start <- Sys.time()
  
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
  }
  else {
    type <- "reg"
  }
  
  if (verbose == TRUE) {
    if (RFimplem == "randomForest") {
      timeOneRFAllVar <- system.time(
        randomForest::randomForest(x = x[, varselect.interp, drop=FALSE], y = y, ...))
    }
    if (RFimplem == "ranger") {
      timeOneRFAllVar <- system.time(
        ranger(dependent.variable.name="y",
               data = cbind(x[, varselect.interp, drop=FALSE], "y" = y),
               num.trees = ntree, num.threads = 1, ...))
    }
    if (RFimplem == "Rborist") {
      timeOneRFAllVar <- system.time(
        Rborist(x = x[, varselect.interp, drop=FALSE], y = y, minInfo = 0,
                nTree = ntree, nThread = 1, ...))
    }
    cat(paste("Maximum estimated computational time (on one core):",
              round(length(varselect.interp) * nfor.pred * timeOneRFAllVar[3], 1), "sec.\n"))
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
          rf[j] <- tail(randomForest::randomForest(x=w, y=y,
                                                   ...)$err.rate[,1], n=1)
        }
        
        err.pred <- mean(rf)
      }
      if (type=="reg") {
        for (j in 1:nfor.pred) {
          rf[j] <- tail(randomForest::randomForest(x=w, y=y, ...)$mse, n=1)
        }
        err.pred <- mean(rf)
      }
    }
    if (RFimplem == "ranger") {
      dat <- cbind(w, "y" = y)
      for (j in 1:nfor.pred) {
        rf[j] <- ranger::ranger(dependent.variable.name="y", data=dat,
                                num.threads = ifelse(parallel, ncores, 1),
                                num.trees=ntree, ...)$prediction.error
      }
      err.pred <- mean(rf)
    }
    if (RFimplem == "Rborist") {
      for (j in 1:nfor.pred) {
        rf[j] <- Rborist::Rborist(x = w, y = y, nTree = ntree, minInfo = 0,
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
            if (i <= n) {
              for (j in 1:nfor.pred) {
                rf[j] <- tail(randomForest::randomForest(x=w, y=y,
                                ...)$err.rate[,1], n=1)
              }
            }
            
            else {
              for (j in 1:nfor.pred) {
                rf[j] <- tail(randomForest::randomForest(x=w, y=y,
                                mtry=i/3, ...)$err.rate[,1], n=1)
              }
            }
            z <- mean(rf)
          }
          if (type=="reg") {
            for (j in 1:nfor.pred) {
              rf[j] <- tail(randomForest::randomForest(x=w, y=y, ...)$mse, n=1)
            }
            z <- mean(rf)
          }
        }
        if (RFimplem == "ranger") {
          dat <- cbind(w, "y" = y)
          if (i <= n) {
            for (j in 1:nfor.pred) {
              rf[j] <- ranger::ranger(dependent.variable.name="y", data=dat,
                                      num.threads = ifelse(parallel, ncores, 1),
                                      num.trees=ntree, ...)$prediction.error
            }
          } else {
            for (j in 1:nfor.pred) {
              rf[j] <- ranger::ranger(dependent.variable.name="y", data=dat,
                                      num.threads = ifelse(parallel, ncores, 1),
                                      num.trees=ntree, mtry=i/3, ...)$prediction.error
            }
          }
          z <- mean(rf)
        }
        if (RFimplem == "Rborist") {
          if (i <= n) {
            for (j in 1:nfor.pred) {
              rf[j] <- Rborist::Rborist(x = w, y = y, nTree = ntree, minInfo = 0,
                                        nThread = ifelse(parallel, ncores, 1),
                                        ...)$validation$oobError
            }
          } else {
            for (j in 1:nfor.pred) {
              rf[j] <- Rborist::Rborist(x = w, y = y, nTree = ntree, minInfo = 0,
                                        nThread = ifelse(parallel, ncores, 1),
                                        predFixed = i/3, ...)$validation$oobError
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
VSURF_pred.formula <- function(formula, data, ntree = 2000, err.interp, varselect.interp,
                               nfor.pred = 25, nmj = 1, RFimplem = "randomForest", parallel = FALSE,
                               ncores = detectCores()-1, verbose = TRUE,  importance="permute", block.size = 1,..., na.action = na.fail) {
### formula interface for VSURF_pred.
  
  ### code gratefully stolen from rfsrc (package randomForestSRC) and adapted for this function.
  
  ## conduct preliminary formula validation
  formulaPrelim <- parseFormula(formula, data)
  ## save the call/formula for the return object
  my.call <- match.call()
  my.call$formula <- eval(formula)
  
  ## finalize the formula based on the pre-processed data
  formulaDetail <- finalizeFormula(formulaPrelim, data)  
  
  ## save the family for convenient access
  family <- formulaDetail$family
  
  if (family == "surv"){
    RFimplem <- "randomForestSRC"
    
    # err.interp: interpretation models errors
    # varselect.interp: interpretation variables indices
    # nmj: number of mean jump: addition of a variable if it gives an error
    # reduction of at least nmj * mean jump value
    
    start <- Sys.time()
    
    #num of the variables used for Surv in data
    num_surv <- which(colnames(data) == formulaDetail$yvar.names[1] | colnames(data)==formulaDetail$yvar.names[2])
   
     if (verbose == TRUE) cat(paste("\nPrediction step (on", length(varselect.interp),
                                   "variables)\n"))
    
    
    if (verbose == TRUE) {
      if (RFimplem=="randomForestSRC"){
        timeOneRFAllVar <- system.time(
          randomForestSRC::rfsrc(formula, data=data[,c(varselect.interp, num_surv), drop=FALSE],
                                 ...)
        )
      }
      cat(paste("Maximum estimated computational time (on one core):",
                round(length(varselect.interp) * nfor.pred * timeOneRFAllVar[3], 1), "sec.\n"))
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
      n <- nrow(data)
      varselect.pred <- varselect.interp[1]
      u <- c(varselect.pred, num_surv)
      w <- data[, u, drop=FALSE]
      rf <- rep(NA, nfor.pred)
      
      if (RFimplem == "randomForestSRC"){
        for (j in 1:nfor.pred) {
          rf[j]<-randomForestSRC::rfsrc(formula, data=w, ntree=ntree, importance=importance, block.size = block.size, ...)$err.rate[ntree]
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
          u <- c(varselect.pred, varselect.interp[i], num_surv)
          w <- data[, u, drop=FALSE]
          rf <- rep(NA, nfor.pred)
          
          if (RFimplem == "randomForestSRC"){
            for (j in 1:nfor.pred) {
              rf[j]<-randomForestSRC::rfsrc(formula, data=w, ntree=ntree, importance=importance, block.size = block.size, ...)$err.rate[ntree]
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
  
  else{
  
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
}


#_______________________________________________
#functions from randomForestSRC
parseFormula <- function(f, data, ytry = NULL, coerce.factor = NULL) {
  ## confirm coherency of the formula
  if (!inherits(f, "formula")) {
    stop("'formula' is not a formula object.")
  }
  if (is.null(data)) {
    stop("'data' is missing.")
  }
  if (!is.data.frame(data)) {
    stop("'data' must be a data frame.")
  }
  ## pull the family and y-variable names
  fmly <- all.names(f, max.names = 1e7)[2]
  all.names <- all.vars(f, max.names = 1e7)
  yvar.names <- all.vars(formula(paste(as.character(f)[2], "~ .")), max.names = 1e7)
  yvar.names <- yvar.names[-length(yvar.names)]
  ## Default scenario, no subject information when family is not
  ## time dependent covariates.  Can be overridden later.
  subj.names <- NULL
  ## is coerce.factor at play for the y-outcomes?
  coerce.factor.org <- coerce.factor
  coerce.factor <- vector("list", 2)
  names(coerce.factor) <- c("xvar.names", "yvar.names")
  if (!is.null(coerce.factor.org)) {
    coerce.factor$yvar.names <- intersect(yvar.names, coerce.factor.org)
    if (length(coerce.factor$yvar.names) == 0) {
      coerce.factor$yvar.names <- NULL
    }
    coerce.factor$xvar.names <- intersect(setdiff(colnames(data), yvar.names), coerce.factor.org)
  }
  ## survival forests
  if (fmly == "Surv") {
    ## Survival and competing risk will have 2 slots, namely time and censoring.
    ## Time dependent covariates will have 4 slots, namely id, start, stop, and event.
    ## If TDC is in effect, we remove the id from the yvars, and tag is an the subject identifier.
    if ((sum(is.element(yvar.names, names(data))) != 2) &&
        (sum(is.element(yvar.names, names(data))) != 4)) {
      stop("Survival formula incorrectly specified.")
    }
    else {
      if (sum(is.element(yvar.names, names(data))) == 4) {
        ## Time dependent covariates is in effect.
        subj.names <- yvar.names[1]
        yvar.names <- yvar.names[-1]
      }
    }
    family <- "surv"
    ytry <- 0
  }
  ## done: return the goodies
  return (list(all.names=all.names, family=family, subj.names=subj.names, yvar.names=yvar.names, ytry=ytry,
               coerce.factor = coerce.factor))
}

finalizeFormula <- function(formula.obj, data) {
  ## parse the formula object
  yvar.names <- formula.obj$yvar.names
  subj.names <- formula.obj$subj.names
  all.names  <- formula.obj$all.names
  fmly       <- formula.obj$family
  ytry       <- formula.obj$ytry
  index <- length(yvar.names)
  ## Adjust the index for the presence of subject names.
  if (fmly == "surv") {
    if (!is.null(subj.names)) {
      index <- index + 1
    }
  }
  ## total number of variables should exceed number of yvars
  if (length(all.names) <= index) {
    stop("formula is misspecified: total number of variables does not exceed total number of y-variables")
  }
  ## extract the xvar names
  if (all.names[index + 1] == ".") {
    if(index == 0) {
      xvar.names <- names(data)
    }
    else {
      xvar.names <- names(data)[!is.element(names(data), all.names[1:index])]
    }
  }
  else {
    if(index == 0) {
      xvar.names <- all.names
    }
    else {
      xvar.names <- all.names[-c(1:index)]
    }
    not.specified <- !is.element(xvar.names, names(data))
    if (sum(not.specified) > 0) {
      stop("formula is misspecified, object ", xvar.names[not.specified], " not found")
    }
  }
  ## return the goodies
  return (list(family=fmly, subj.names=subj.names, yvar.names=yvar.names, xvar.names=xvar.names, ytry=ytry))
}