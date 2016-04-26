#' Thresholding step of VSURF
#' 
#' Thresholding step is dedicated to roughly eliminate irrelevant variables a
#' the dataset. This is the first step of the \code{\link{VSURF}} function. For
#' refined variable selection, see VSURF other steps:
#' \code{\link{VSURF_interp}} and \code{\link{VSURF_pred}}.
#' 
#' First, \code{nfor.thres} random forests are computed using the function
#' \code{randomForest} with arguments \code{importance=TRUE}, and our choice
#' of default values for 
#' \code{ntree} and \code{mtry} (which are higher than default in
#' \code{\link{randomForest}} to get a more stable variable importance measure).
#' Then variables
#' are sorted according to their mean variable importance (VI), in decreasing
#' order. This order is kept all along the procedure.  Next, a threshold is
#' computed: \code{min.thres}, the minimum predicted value of a pruned CART
#' tree fitted to the curve of the standard deviations of VI.  Finally, the
#' actual thresholding is performed: only variables with a mean VI larger than
#' \code{nmin} * \code{min.thres} are kept.
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
#' @return An object of class \code{VSURF_thres}, which is a list with the
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
#' \item{imp.mean.dec}{A vector of the variables importance means
#' (over \code{nfor.thres} runs), in decreasing order.}
#' 
#' \item{imp.mean.dec.ind}{The ordering index vector associated to the sorting
#' of variables importance means.}
#' 
#' \item{imp.sd.dec}{A vector of standard deviations of all variables
#' importances. The order is given by \code{imp.mean.dec.ind}.}
#' 
#' \item{mean.perf}{The mean OOB error rate, obtained by a random forests
#' build with all variables.}
#' 
#' \item{pred.pruned.tree}{The predictions of the CART tree fitted to the
#' curve of the standard deviations of VI.}
#' 
#' \item{nmin}{Value of the parameter in the call.}
#' 
#' \item{comput.time}{Computation time.}
#'
#'\item{ncores}{The number of cores used to run \code{VSURF_thres}
#'  in parallel (NULL if VSURF_thres did not run in parallel).}
#'
#' \item{clusterType}{The type of the cluster used to run
#' \code{VSURF_thres} in parallel (NULL if VSURF_thres did not run in parallel).}
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
#' iris.thres
#' 
#' \dontrun{
#' # A more interesting example with toys data (see \code{\link{toys}})
#' # (a few minutes to execute)
#' data(toys)
#' toys.thres <- VSURF_thres(toys$x, toys$y)
#' toys.thres}
#'
#' @importFrom randomForest randomForest
#' @importFrom rpart rpart prune
#' @importFrom doParallel registerDoParallel
#' @importFrom foreach foreach %dopar%
#' @importFrom parallel makeCluster stopCluster mclapply detectCores
#' @importFrom utils tail
#' @importFrom stats model.frame model.response na.fail predict reformulate
#' @importFrom stats sd terms na.omit
#' @export
VSURF_thres <- function (x, ...) {
  UseMethod("VSURF_thres")
}

#' @rdname VSURF_thres
#' @export
VSURF_thres.default <- function(
  x, y, ntree = 2000, mtry = max(floor(ncol(x) / 3), 1), nfor.thres = 50, nmin = 1,
  parallel = FALSE, clusterType = "PSOCK", ncores = detectCores() - 1, ...) {
  
  # x: input
  # y: output
  # nfor.thres: number of forests to compute the mean importance of variables (VI)
  # nmin: thresholding parameter (if this procedure step keeps too much variables,
  # this value can be increased, e.g. to 3 or 5)
  
  start <- Sys.time()
  
  if (!parallel) {
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
  
  # m: matrix with VI
  # perf: matrix with OOB errors
  m <- matrix(NA, nrow=nfor.thres, ncol=ncol(x))
  perf <- matrix(NA, nrow=nfor.thres, ncol=1)
  
  # if all forests have to be stored in memory, lines involving "rfmem" must be uncommented
  #rfmem=list()
  
  # filling of matrix m by running nfor.thres forests and keeping VI
  # filling of perf with the nfor.thres forests OOB errors
  
  rf.classif <- function(i, ...) {
    rf <- randomForest::randomForest(x=x, y=y, ntree=ntree, mtry=mtry, importance=TRUE, ...)
    m <- rf$importance[, length(levels(y))+1]
    perf <- tail(rf$err.rate[,1], n=1)
    out <- list(m=m, perf=perf)
  }
  
  rf.reg <- function(i, ...) {
    rf <- randomForest::randomForest(x=x, y=y, ntree=ntree, mtry=mtry, importance=TRUE, ...)
    m <- rf$importance[, 1]
    perf <- tail(rf$mse, n=1)
    out <- list(m=m, perf=perf)
  }
  
  if (!parallel) {
    if (type=="classif") {
      for (i in 1:nfor.thres){
        rf <- rf.classif(i, ...)
        m[i,] <- rf$m
        perf[i] <- rf$perf
      }
    }
    if (type=="reg") {
      for (i in 1:nfor.thres){
        rf <- rf.reg(i, ...)
        m[i,] <- rf$m
        perf[i] <- rf$perf
      }
    }
  }
  
  else {
    if (clusterType=="FORK") {
      if (type=="classif") {
        res <- parallel::mclapply(X=1:nfor.thres, FUN=rf.classif, ..., mc.cores=ncores)
      }
      if (type=="reg") {
        res <- parallel::mclapply(X=1:nfor.thres, FUN=rf.reg, ..., mc.cores=ncores)
      }
    }
    
    else {
      clust <- parallel::makeCluster(spec=ncores, type=clusterType)
      doParallel::registerDoParallel(clust)
      
      if (type=="classif") {
        res <- foreach::foreach(i=1:nfor.thres, .packages="randomForest") %dopar% {
          out <- rf.classif(i, ...)
        }
      }
      
      if (type=="reg") {
        res <- foreach::foreach(i=1:nfor.thres, .packages="randomForest") %dopar% {
          out <- rf.reg(i, ...)
        }
      }
      parallel::stopCluster(clust)
    }
    
    for (i in 1:nfor.thres) {
      m[i,] <- res[[i]]$m
      perf[i] <- res[[i]]$perf
    }
  }
  
  m_na.omit <- stats::na.omit(m)
  if (nrow(m_na.omit) != nrow(m)) {
      warning(
          paste0(nrow(m) - nrow(m_na.omit), " runs of randomForest were removed
                (among ", nfor.thres, ") because they contained no OOB
                observations for some trees")
      )
      m <- m_na.omit
  }
  
  # ord.imp contains the VI means in decreasing order
  ord.imp <- sort( colMeans(m), index.return=TRUE, decreasing=TRUE)
  imp.mean.dec <- ord.imp$x
  imp.mean.dec.ind <- ord.imp$ix
  
  # mean.perf contains the forests mean OOB error
  mean.perf <- mean(perf)
  
  # imp.sd.dec contains VI standard deviations of all variables sorted according to imp.mean.dec.ind
  sd.imp <- apply(m, 2, sd)
  imp.sd.dec <- sd.imp[imp.mean.dec.ind]
  
  # particular case where x has only one variable
  s <- NULL
  if (ncol(as.matrix(x))==1) {
    s <- 1
  }
  else {
    p <- ncol(x)
    u <- data.frame(imp.sd.dec, 1:p)
    
    # estimation of the standard deviations curve with CART (using "rpart" package)
    
    # construction of the maximal tree and search of optimal complexity
    tree <- rpart::rpart(imp.sd.dec ~., data=u, cp=0, minsplit=2)
    d <- tree$cptable
    argmin.cp <- which.min(d[,4])
    
    # pruning
    pruned.tree <- rpart::prune(tree, cp=d[argmin.cp, 1])
    pred.pruned.tree <- predict(pruned.tree)
    
    # determination of the y-value of the lowest stair: this is the estimation
    # of the mean standard deviation of VI
    min.pred <- min(pred.pruned.tree)
    
    # thresholding: all variables with VI mean lower than min.pred are discarded
    w <- which(imp.mean.dec < nmin*min.pred)
    
    if (length(w)==0) {
      s <- p
    }
    else {
      s <- min(w)-1
    }
  }
  
  # varselect: selected variables index
  # impvarselect: corresponding VI means
  varselect.thres <- imp.mean.dec.ind[1:s]
  imp.varselect.thres <- imp.mean.dec[1:s]
  
  cl <- match.call()
  cl[[1]] <- as.name("VSURF_thres")
  
  comput.time <- Sys.time()-start
  
  output <- list('varselect.thres'=varselect.thres,
                 'imp.varselect.thres'=imp.varselect.thres,
                 'min.thres'=min.pred,
                 'num.varselect.thres'=s,
                 'imp.mean.dec'=imp.mean.dec,
                 'imp.mean.dec.ind'=imp.mean.dec.ind,
                 'imp.sd.dec'=imp.sd.dec,
                 'mean.perf'=mean.perf,
                 'pred.pruned.tree'=pred.pruned.tree,
                 'nmin' = nmin,
                 'comput.time'=comput.time,
                 'ncores'=ncores,
                 'clusterType'=clusterType,
                 'call'=cl)
  class(output) <- c("VSURF_thres")
  output
}


#' @rdname VSURF_thres
#' @export
VSURF_thres.formula <- function(formula, data, ..., na.action = na.fail) {
### formula interface for VSURF_thres.
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
  ret <- VSURF_thres.default(x=m, y=y, ...)
  cl <- match.call()
  cl[[1]] <- as.name("VSURF")
  ret$call <- cl
  ret$terms <- Terms
  if (!is.null(attr(y, "na.action"))) {
    ret$na.action <- attr(y, "na.action")
  }
  class(ret) <- c("VSURF_thres.formula", class(ret))
        warning(
        "VSURF with a formula-type call outputs selected variables
  which are indices of the input matrix based on the formula:
  you may reorder these to get indices of the original data")
    return(ret)
}
