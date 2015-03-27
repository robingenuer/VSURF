#' Thresholding step of VSURF
#' 
#' Thresholding step is dedicated to roughly eliminate irrelevant variables a
#' the dataset. This is the first step of the \code{\link{VSURF}} function. For
#' refined variable selection, see VSURF other steps:
#' \code{\link{VSURF.interp}} and \code{\link{VSURF.pred}}.
#' 
#' First, \code{nfor.thres} random forests are computed using the function
#' \code{randomForest} with arguments \code{importance=TRUE}. Then variables
#' are sorted according to their mean variable importance (VI), in decreasing
#' order. This order is kept all along the procedure.  Next, a threshold is
#' computed: \code{min.thres}, the minimum predicted value of a pruned CART
#' tree fitted to the curve of the standard deviations of VI.  Finally, the
#' actual thresholding is performed: only variables with a mean VI larger than
#' \code{nmin} * \code{min.thres} are kept.
#' 
#' @aliases VSURF.thres VSURF.thres.default VSURF.thres.formula
#' VSURF.thres.parallel VSURF.thres.parallel.default
#' VSURF.thres.parallel.formula
#' 
#' @param data a data frame containing the variables in the model.
#' @param na.action A function to specify the action to be taken if NAs are
#' found.  (NOTE: If given, this argument must be named, and as
#' \code{randomForest} it is only used with the formula-type call.)
#' @param x,formula A data frame or a matrix of predictors, the columns
#' represent the variables. Or a formula describing the model to be fitted.
#' @param y A response vector (must be a factor for classification problems and
#' numeric for regression ones).
#' @param ntree Number of trees in each forest grown. Standard
#' \code{randomForest} parameter.
#' @param mtry Number of variables randomly sampled as candidates at each
#' split. Standard \code{randomForest} parameter.
#' @param nfor.thres Number of forests grown.
#' @param nmin Number of times the "minimum value" is multiplied to set
#' threshold value. See details below.
#' @param para A logical indicating if you want VSURF to run in parallel on
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
#' @return An object of class \code{VSURF.thres}, which is a list with the
#' following components:
#' 
#' \item{varselect.thres}{A vector of indices of selected variables,
#' sorted according to their mean VI, in decreasing order.}
#' 
#' \item{imp.varselect.thres}{A vector of importances of the
#' \code{varselect.thres} variables.}
#' 
#' \item{min.thres}{The minimum predicted value of a pruned CART tree
#' fitted to the curve of the standard deviations of VI.}
#' 
#' \item{num.varselect.thres}{The number of selected variables.}
#' 
#' \item{ord.imp}{A list containing the order of all variables mean
#' importance. \code{$x} contains the mean importances in decreasing order.
#' \code{$ix} contains indices of the variables.}
#' 
#' \item{ord.sd}{A vector of standard deviations of all variables
#' importances. The order is given by \code{ord.imp}.}
#' 
#' \item{mean.perf}{The mean OOB error rate, obtained by a random forests
#' build with all variables.}
#' 
#' \item{pred.pruned.tree}{The predictions of the CART tree fitted to the
#' curve of the standard deviations of VI.}
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
#' iris.thres
#' 
#' \dontrun{
#' # A more interesting example with toys data (see \code{\link{toys}})
#' # (a few minutes to execute)
#' data(toys)
#' toys.thres <- VSURF.thres(x=toys$x, y=toys$y)
#' toys.thres}
#'
#' @rdname VSURF.thres
#' @method VSURF.thres default
#' @export VSURF.thres.default
VSURF.thres.default <- function(
  x, y, ntree=2000, mtry=max(floor(ncol(x)/3), 1), nfor.thres=50, nmin=1,
  para=FALSE, clusterType="PSOCK", ncores=detectCores()-1, ...) {
  
  # x: input
  # y: output
  # nfor.thres: number of forests to compute the mean importance of variables (IV)
  # nmin: thresholding parameter (if this procedure step keeps too much variables,
  # this value can be increased, e.g. to 3 or 5)
  
  start <- Sys.time()
  
  if (!para) {
    clusterType <- NULL
    ncores <- NULL
  }
  
  ncores <- min(nfor.thres, ncores)
  
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
  
  # m: matrix with IV
  # perf: matrix with OOB errors
  m <- matrix(NA, nrow=nfor.thres, ncol=ncol(x))
  perf <- matrix(NA, nrow=nfor.thres, ncol=1)
  
  # if all forests have to be stored in memory, lines involving "rfmem" must be uncommented
  #rfmem=list()
  
  # filling of matrix m by running nfor.thres forests and keeping IV
  # filling of perf with the nfor.thres forests OOB errors
  
  rf.classif <- function(i, ...) {
    rf <- randomForest(x=x, y=y, ntree=ntree, mtry=mtry, importance=TRUE, ...)
    m <- rf$importance[, length(levels(y))+1]
    perf <- tail(rf$err.rate[,1], n=1)
    out <- list(m=m, perf=perf)
  }
  
  rf.reg <- function(i, ...) {
    rf <- randomForest(x=x, y=y, ntree=ntree, mtry=mtry, importance=TRUE, ...)
    m <- rf$importance[, 1]
    perf <- tail(rf$mse, n=1)
    out <- list(m=m, perf=perf)
  }
  
  if (!para) {
    if (type=="classif") {
      for (i in 1:nfor.thres){
        rf <- randomForest(x=x, y=y, ntree=ntree, mtry=mtry, importance=TRUE, ...)
        #rfmem=c(rfmem,list(rf))
        m[i,] <- rf$importance[, length(levels(y))+1]
        perf[i] <- tail(rf$err.rate[,1], n=1)
      }
    }
    if (type=="reg") {
      for (i in 1:nfor.thres){
        rf <- randomForest(x=x, y=y, ntree=ntree, mtry=mtry, importance=TRUE, ...)
        #rfmem=c(rfmem,list(rf))
        m[i,] <- rf$importance[, 1]
        perf[i] <- tail(rf$mse, n=1)
      }
    }
  }
  
  else {
    if (clusterType=="FORK") {
      if (type=="classif") {
        res <- mclapply(X=1:nfor.thres, FUN=rf.classif, ..., mc.cores=ncores)
      }
      if (type=="reg") {
        res <- mclapply(X=1:nfor.thres, FUN=rf.reg, ..., mc.cores=ncores)
      }
    }
    
    else {
      clust <- makeCluster(spec=ncores, type=clusterType)
      registerDoParallel(clust)
      
      if (type=="classif") {
        res <- foreach(i=1:nfor.thres, .packages="randomForest") %dopar% {
          out <- rf.classif(i, ...)
        }
      }
      
      if (type=="reg") {
        res <- foreach(i=1:nfor.thres, .packages="randomForest") %dopar% {
          out <- rf.reg(i, ...)
        }
      }
      stopCluster(clust)
    }
    
    for (i in 1:nfor.thres) {
      m[i,] <- res[[i]]$m
      perf[i] <- res[[i]]$perf
    }
  }
  
  # ord.imp contains the IV means in decreasing order
  ord.imp <- sort( colMeans(m), index.return=TRUE, decreasing=TRUE)
  
  # mean.perf contains the forests mean OOB error
  mean.perf <- mean(perf)
  
  # ord.sd contains IV standard deviations of all variables sorted according to ord.imp
  sd.imp <- apply(m, 2, sd)
  ord.sd <- sd.imp[ord.imp$ix]
  
  # particular case where x has only one variable
  s <- NULL
  if (ncol(as.matrix(x))==1) {
    s <- 1
  }
  else {
    p <- ncol(x)
    u <- data.frame(ord.sd, 1:p)
    
    # estimation of the standard deviations curve with CART (using "rpart" package)
    
    # construction of the maximal tree and search of optimal complexity
    tree <- rpart(ord.sd ~., data=u, control=rpart.control(cp=0, minsplit=2))
    d <- tree$cptable
    argmin.cp <- which.min(d[,4])
    
    # pruning
    pruned.tree <- prune(tree, cp=d[argmin.cp, 1])
    pred.pruned.tree <- predict(pruned.tree)
    
    # determination of the y-value of the lowest stair: this is the estimation
    # of the mean standard deviation of IV
    min.pred <- min(pred.pruned.tree)
    
    # thresholding: all variables with IV mean lower than min.pred are discarded
    w <- which(ord.imp$x < nmin*min.pred)
    
    if (length(w)==0) {
      s <- p
    }
    else {
      s <- min(w)-1
    }
  }
  
  # varselect: selected variables index
  # impvarselect: corresponding IV means
  varselect.thres <- ord.imp$ix[1:s]
  imp.varselect.thres <- ord.imp$x[1:s]
  
  cl <- match.call()
  cl[[1]] <- as.name("VSURF.thres")
  
  comput.time <- Sys.time()-start
  
  output <- list('varselect.thres'=varselect.thres,
                 'imp.varselect.thres'=imp.varselect.thres,
                 'min.thres'=min.pred,
                 'num.varselect.thres'=s,
                 'ord.imp'=ord.imp,
                 'ord.sd'=ord.sd,
                 'mean.perf'=mean.perf,
                 'pred.pruned.tree'=pred.pruned.tree,
                 'nmin' = nmin,
                 'comput.time'=comput.time,
                 'clusterType'=clusterType,
                 'ncores'=ncores,
                 'call'=cl)
  class(output) <- "VSURF.thres"
  output
}


#' @rdname VSURF.thres
#' @method VSURF.thres formula
#' @export VSURF.thres.formula
VSURF.thres.formula <- function(formula, data, ..., na.action = na.fail) {
### formula interface for VSURF.thres.
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
    ret <- VSURF.thres(m, y, ...)
    cl <- match.call()
    cl[[1]] <- as.name("VSURF.thres")
    ret$call <- cl
    ret$terms <- Terms
    if (!is.null(attr(m, "na.action")))
        ret$na.action <- attr(m, "na.action")
    class(ret) <- c("VSURF.thres.formula", "VSURF.thres")
        warning(
        "VSURF with a formula-type call outputs selected variables
  which are indices of the input matrix based on the formula:
  you may reorder these to get indices of the original data")
    return(ret)
}

#' @rdname VSURF.thres
#' @method VSURF.thres.parallel default
#' @export VSURF.thres.parallel.default
VSURF.thres.parallel.default <- function(
  x, y, ntree=2000, mtry=max(floor(ncol(x)/3), 1),
  nfor.thres=50, nmin=1, clusterType="PSOCK",
  ncores=detectCores()-1, ...) {

  # x: input
  # y: output
  # nfor.thres: number of forests to compute the mean importance of variables (IV)
  # nmin: thresholding parameter (if this procedure step keeps too much variables,
  # this value can be increased, e.g. to 3 or 5)

  start <- Sys.time()

  ncores <- min(nfor.thres, ncores)
 
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
  
  # m: matrix with IV
  # perf: matrix with OOB errors
  m <- matrix(NA, nrow=nfor.thres, ncol=ncol(x))
  perf <- matrix(NA, nrow=nfor.thres, ncol=1)
  
  # if all forests have to be stored in memory, lines involving "rfmem" must be uncommented
  #rfmem=list()
  
  # filling of matrix m by running nfor.thres forests and keeping IV
  # filling of perf with the nfor.thres forests OOB errors

  rf.classif <- function(i, ...) {
      rf <- randomForest(x=x, y=y, ntree=ntree, mtry=mtry, importance=TRUE, ...)
      m <- rf$importance[, length(levels(y))+1]
      perf <- tail(rf$err.rate[,1], n=1)
      out <- list(m=m, perf=perf)
  }

  rf.reg <- function(i, ...) {
      rf <- randomForest(x=x, y=y, ntree=ntree, mtry=mtry, importance=TRUE, ...)
      m <- rf$importance[, 1]
      perf <- tail(rf$mse, n=1)
      out <- list(m=m, perf=perf)
  }
  
  if (clusterType=="FORK") {
    if (type=="classif") {
      res <- mclapply(X=1:nfor.thres, FUN=rf.classif, ..., mc.cores=ncores)
    }
    if (type=="reg") {
      res <- mclapply(X=1:nfor.thres, FUN=rf.reg, ..., mc.cores=ncores)
    }
  }
  
  else {
    clust <- makeCluster(spec=ncores, type=clusterType)
    registerDoParallel(clust)
    
    if (type=="classif") {
      res <- foreach(i=1:nfor.thres, .packages="randomForest") %dopar% {
        out <- rf.classif(i, ...)
      }
    }
    
    if (type=="reg") {
      res <- foreach(i=1:nfor.thres, .packages="randomForest") %dopar% {
        out <- rf.reg(i, ...)
      }
    }
    stopCluster(clust)
  }
  
  for (i in 1:nfor.thres) {
      m[i,] <- res[[i]]$m
      perf[i] <- res[[i]]$perf
  }
      
  # ord.imp contains the IV means in decreasing order
  ord.imp <- sort( colMeans(m), index.return=TRUE, decreasing=TRUE)
  
  # mean.perf contains the forests mean OOB error
  mean.perf <- mean(perf)
  
  # ord.sd contains IV standard deviations of all variables sorted according to ord.imp
  sd.imp <- apply(m, 2, sd)
  ord.sd <- sd.imp[ord.imp$ix]
    
  # particular case where x has only one variable
  s <- NULL
  if (ncol(as.matrix(x))==1) {
    s <- 1
  }
  else {
    p <- ncol(x)
    u <- data.frame(ord.sd, 1:p)
    
    # estimation of the standard deviations curve with CART (using "rpart" package)
    
    # construction of the maximal tree and search of optimal complexity
    tree <- rpart(ord.sd ~., data=u, control=rpart.control(cp=0, minsplit=2))
    d <- tree$cptable
    argmin.cp <- which.min(d[,4])
    
    # pruning
    pruned.tree <- prune(tree, cp=d[argmin.cp, 1])
    pred.pruned.tree <- predict(pruned.tree)
    
    # determination of the y-value of the lowest stair: this is the estimation
    # of the mean standard deviation of IV
    min.pred <- min(pred.pruned.tree)
    
    # thresholding: all variables with IV mean lower than min.pred are discarded
    w <- which(ord.imp$x < nmin*min.pred)
    
    if (length(w)==0) {
      s <- p
    }
    else {
      s <- min(w)-1
    }
  }
  
  # varselect: selected variables index
  # impvarselect: corresponding IV means
  varselect.thres <- ord.imp$ix[1:s]
  imp.varselect.thres <- ord.imp$x[1:s]
  
  cl <- match.call()
  cl[[1]] <- as.name("VSURF.thres")

  comput.time <- Sys.time()-start
  
  output <- list('varselect.thres'=varselect.thres,
                 'imp.varselect.thres'=imp.varselect.thres,
                 'min.thres'=min.pred,
                 'num.varselect.thres'=s,
                 'ord.imp'=ord.imp,
                 'ord.sd'=ord.sd,
                 'mean.perf'=mean.perf,
                 'pred.pruned.tree'=pred.pruned.tree,
                 'nmin' = nmin,
                 'comput.time'=comput.time,
                 'clusterType'=clusterType,
                 'ncores'=ncores,
                 'call'=cl)
  class(output) <- "VSURF.thres"
  output
}


#' @rdname VSURF.thres
#' @method VSURF.thres.parallel formula
#' @export VSURF.thres.parallel.formula
VSURF.thres.parallel.formula <- function(formula, data, ..., na.action = na.fail) {
### formula interface for VSURF.thres.parallel.
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
    ret <- VSURF.thres.parallel(m, y, ...)
    cl <- match.call()
    cl[[1]] <- as.name("VSURF.thres")
    ret$call <- cl
    ret$terms <- Terms
    if (!is.null(attr(m, "na.action")))
        ret$na.action <- attr(m, "na.action")
    class(ret) <- c("VSURF.thres.formula", "VSURF.thres")
        warning(
        "VSURF with a formula-type call outputs selected variables
  which are indices of the input matrix based on the formula:
  you may reorder these to get indices of the original data")
    return(ret)
}


#' @export
VSURF.thres <- function (x, ...) {
  UseMethod("VSURF.thres")
}


#' @export
VSURF.thres.parallel <- function (x, ...) {
  UseMethod("VSURF.thres.parallel")
}
