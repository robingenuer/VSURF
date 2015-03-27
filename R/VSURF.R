#' Variable Selection Using Random Forests
#' 
#' Three steps variable selection procedure based on random forests for
#' supervised classification and regression problems.  First step
#' ("thresholding step") is dedicated to eliminate irrelevant variables from
#' the dataset.  Second step ("interpretation step") aims to select all
#' variables related to the response for interpretation prupose.  Third step
#' ("prediction step") refines the selection by eliminating redundancy in the
#' set of variables selected by the second step, for prediction prupose.
#' 
#' \itemize{ \item First step ("thresholding step"): first, \code{nfor.thres}
#' random forests are computed using the function \code{randomForest} with
#' arguments \code{importance=TRUE}. Then variables are sorted according to
#' their mean variable importance (VI), in decreasing order.  This order is
#' kept all along the procedure. Next, a threshold is computed:
#' \code{min.thres}, the minimum predicted value of a pruned CART tree fitted
#' to the curve of the standard deviations of VI.  Finally, the actual
#' "thresholding step" is performed: only variables with a mean VI larger than
#' \code{nmin} * \code{min.thres} are kept.
#' 
#' \item Second step ("intepretation step"): the variables selected by the
#' first step are considered. \code{nfor.interp} embedded random forests models
#' are grown, starting with the random forest build with only the most
#' important variable and ending with all variables selected in the first step.
#' Then, \code{err.min} the minimum mean out-of-bag (OOB) error of these models
#' and its associated standard deviation \code{sd.min} are computed.  Finally,
#' the smallest model (and hence its corresponding variables) having a mean OOB
#' error less than \code{err.min} + \code{nsd} * \code{sd.min} is selected.
#' 
#' \item Third step ("prediction step"): the starting point is the same than in
#' the second step. However, now the variables are added to the model in a
#' stepwise manner. \code{mean.jump}, the mean jump value is calculated using
#' variables that have been left out by the second step, and is set as the mean
#' absolute difference between mean OOB errors of one model and its first
#' following model.  Hence a variable is included in the model if the mean OOB
#' error decrease is larger than \code{nmj} * \code{mean.jump}.  }
#'
#' VSURF.parallel is able to run VSURF using mutliple cores in parallel
#' (see \code{clusterType} and \code{ncores} arguments).
#' 
#' @aliases VSURF VSURF.default VSURF.formula VSURF.parallel
#' VSURF.parallel.default VSURF.parallel.formula
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
#' @param mtry Number of variables randomly sampled as candidates at each
#' split.  Standard parameter of \code{randomForest}.
#' @param nfor.thres Number of forests grown for "thresholding step" (first of
#' the three steps).
#' @param nmin Number of times the "minimum value" is multiplied to set
#' threshold value.
#' @param nfor.interp Number of forests grown for "intepretation step" (second
#' of the three steps).
#' @param nsd Number of times the standard deviation of the minimum value of
#' \code{err.interp} is multiplied.
#' @param nfor.pred Number of forests grown for "prediction step" (last of the
#' three steps).
#' @param nmj Number of times the mean jump is multiplied.
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
#' @return An object of class \code{VSURF}, which is a list with the following
#' components:
#' 
#' \item{varselect.thres}{A vector of indexes of variables selected after
#' "thresholding step", sorted according to their mean VI, in decreasing order.}
#' 
#' \item{varselect.interp}{A vector of indexes of variables selected after
#' "interpretation step".}
#' 
#' \item{varselect.pred}{A vector of indexes of variables selected after
#' "prediction step".}
#' 
#' \item{nums.varselect}{A vector of the 3 numbers of variables selected
#' resp. by "thresholding step", "interpretation step" and "prediction step".}
#' 
#' \item{imp.varselect.thres}{A vector of importances of the
#' \code{varselect.thres} variables.}
#' 
#' \item{min.thres}{The minimum predicted value of a pruned CART tree
#' fitted to the curve of the standard deviations of VI.}
#' 
#' \item{ord.imp}{A list containing the order of all variables mean
#' importance. \code{$x} contains the mean importances sorted in decreasing
#' order. \code{$ix} contains indexes of the variables.}
#' 
#' \item{ord.sd}{A vector of standard deviations of all variables
#' importance. The order is given by \code{ord.imp}.}
#' 
#' \item{mean.perf}{Mean OOB error rate, obtained by a random forests
#' build on all variables.}
#' 
#' \item{pred.pruned.tree}{Predictions of the CART tree fitted to the
#' curve of the standard deviations of VI.}
#' 
#' \item{err.interp}{A vector of the mean OOB error rates of the embedded
#' random forests models build during the "interpretation step".}
#' 
#' \item{sd.min}{The standard deviation of OOB error rates associated to
#' the random forests model attaining the minimum mean OOB error rate during
#' the "interpretation step".}
#' 
#' \item{err.pred}{A vector of the mean OOB error rates of the random
#' forests models build during the "prediction step".}
#' 
#' \item{mean.jump}{The mean jump value computed during the "prediction
#' step".}
#' 
#' \item{nmin,nsd,nmj}{Corresponding parameters values.}
#' 
#' \item{overall.time}{Overall computation time.}
#' 
#' \item{comput.times}{A list of the 3 computation times respectively
#' associated with the 3 steps: "thresholding", "interpretation" and
#' "prediction".}
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
#' @seealso \code{\link{plot.VSURF}}, \code{\link{summary.VSURF}},
#' \code{\link{VSURF.thres}}, \code{\link{VSURF.interp}},
#' \code{\link{VSURF.pred}}, \code{\link{tune}}
#' @references Genuer, R. and Poggi, J.M. and Tuleau-Malot, C. (2010), Variable
#' selection using random forests, Pattern Recognition Letters 31(14),
#' 2225-2236
#' @examples
#' 
#' data(iris)
#' iris.vsurf <- VSURF(x=iris[,1:4], y=iris[,5], ntree=100, nfor.thres=20,
#'                     nfor.interp=10, nfor.pred=10)
#' iris.vsurf
#' 
#' \dontrun{
#' # A more interesting example with toys data (see \code{\link{toys}})
#' # (a few minutes to execute)
#' data(toys)
#' toys.vsurf <- VSURF(x=toys$x, y=toys$y)
#' toys.vsurf
#'
#' # VSURF run on 2 cores in parallel (using a SOCKET cluster):
#' data(toys)
#' toys.vsurf.para <- VSURF.parallel(x=toys$x, y=toys$y, ncores=2)
#' }
#' 
#' @import randomForest
#' @import rpart
#' @import doParallel
#' 
#' @rdname VSURF
#' @method VSURF default
#' @export VSURF.default
VSURF.default <- function(
    x, y, ntree=2000, mtry=max(floor(ncol(x)/3), 1),
    nfor.thres=50, nmin=1, nfor.interp=25, nsd=1, nfor.pred=25, nmj=1, ...) {

  start <- Sys.time()
  
  thres <- VSURF.thres(x=x, y=y, ntree=ntree, mtry=mtry,
                       nfor.thres=nfor.thres, nmin=nmin, ...)
  
  interp <- VSURF.interp(x=x, y=y, vars=thres$varselect.thres,
                         nfor.interp=nfor.interp, nsd=nsd, ...)
  
  pred <- VSURF.pred(x=x, y=y, err.interp=interp$err.interp,
                     varselect.interp=interp$varselect.interp,
                     nfor.pred=nfor.pred, nmj=nmj, ...)

  cl <- match.call()
  cl[[1]] <- as.name("VSURF")

  overall.time <- Sys.time()-start

  output <- list('varselect.thres'=thres$varselect.thres,
                 'varselect.interp'=interp$varselect.interp,
                 'varselect.pred'=pred$varselect.pred,
                 'nums.varselect'=c(thres$num.varselect.thres,
                   interp$num.varselect.interp,
                   pred$num.varselect.pred),
                 'imp.varselect.thres'=thres$imp.varselect.thres,
                 'min.thres'=thres$min.thres,
                 'ord.imp'=thres$ord.imp,
                 'ord.sd'=thres$ord.sd,
                 'mean.perf'=thres$mean.perf,
                 'pred.pruned.tree' = thres$pred.pruned.tree,
                 'err.interp'=interp$err.interp,
                 'sd.min'=interp$sd.min,
                 'err.pred'=pred$err.pred,
                 'mean.jump'=pred$mean.jump,
                 'nmin' = nmin,
                 'nsd' = nsd,
                 'nmj' = nmj,
                 'overall.time'=overall.time,
                 'comput.times'=list(thres$comput.time, interp$comput.time, pred$comput.time),
                 'call'=cl)
  class(output) <- "VSURF"
  output
}


#' @rdname VSURF
#' @method VSURF formula
#' @export VSURF.formula
VSURF.formula <- function(formula, data, ..., na.action = na.fail) {
    ### formula interface for VSURF.
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
    ret <- VSURF(x=m, y=y, ...)
    cl <- match.call()
    cl[[1]] <- as.name("VSURF")
    ret$call <- cl
    ret$terms <- Terms
    if (!is.null(attr(m, "na.action")))
        ret$na.action <- attr(m, "na.action")
    class(ret) <- c("VSURF.formula", "VSURF")
    warning(
        "VSURF with a formula-type call outputs selected variables
which are indices of the input matrix based on the formula:
you may reorder these to get indices of the original data")
    return(ret)
}

#' @rdname VSURF
#' @method VSURF.parallel default
#' @export VSURF.parallel.default
VSURF.parallel.default <- function(
    x, y, ntree=2000, mtry=max(floor(ncol(x)/3), 1),
    nfor.thres=50, nmin=1, nfor.interp=25, nsd=1, nfor.pred=25, nmj=1,
    clusterType="PSOCK", ncores=detectCores()-1, ...) {

  start <- Sys.time()
  
  thres <- VSURF.thres.parallel(
      x=x, y=y, ntree=ntree, mtry=mtry, nfor.thres=nfor.thres, nmin=nmin,
      clusterType=clusterType, ncores=ncores, ...)
  
  interp <- VSURF.interp.parallel(
      x=x, y=y, vars=thres$varselect.thres, nfor.interp=nfor.interp, nsd=nsd,
      clusterType=clusterType, ncores=ncores, ...)
  
  pred <- VSURF.pred(x=x, y=y, err.interp=interp$err.interp,
                     varselect.interp=interp$varselect.interp,
                     nfor.pred=nfor.pred, nmj=nmj, ...)

  cl <- match.call()
  cl[[1]] <- as.name("VSURF")

  overall.time <- Sys.time()-start

  output <- list('varselect.thres'=thres$varselect.thres,
                 'varselect.interp'=interp$varselect.interp,
                 'varselect.pred'=pred$varselect.pred,
                 'nums.varselect'=c(thres$num.varselect.thres,
                   interp$num.varselect.interp,
                   pred$num.varselect.pred),
                 'imp.varselect.thres'=thres$imp.varselect.thres,
                 'min.thres'=thres$min.thres,
                 'ord.imp'=thres$ord.imp,
                 'ord.sd'=thres$ord.sd,
                 'mean.perf'=thres$mean.perf,
                 'pred.pruned.tree' = thres$pred.pruned.tree,
                 'err.interp'=interp$err.interp,
                 'sd.min'=interp$sd.min,
                 'err.pred'=pred$err.pred,
                 'mean.jump'=pred$mean.jump,
                 'nmin' = nmin,
                 'nsd' = nsd,
                 'nmj' = nmj,
                 'overall.time'=overall.time,
                 'comput.times'=list(thres$comput.time, interp$comput.time, pred$comput.time),
                 'clusterType'=clusterType,
                 'ncores'=ncores,
                 'call'=cl)
  class(output) <- "VSURF"
  output
}


#' @rdname VSURF
#' @method VSURF.parallel formula
#' @export VSURF.parallel.formula
VSURF.parallel.formula <- function(formula, data, ..., na.action = na.fail) {
    ### formula interface for VSURF.parallel.
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
    ret <- VSURF.parallel(x=m, y=y, ...)
    cl <- match.call()
    cl[[1]] <- as.name("VSURF")
    ret$call <- cl
    ret$terms <- Terms
    if (!is.null(attr(m, "na.action")))
        ret$na.action <- attr(m, "na.action")
    class(ret) <- c("VSURF.formula", "VSURF")
    warning(
        "VSURF with a formula-type call outputs selected variables
which are indices of the input matrix based on the formula:
you may reorder these to get indices of the original data")
    return(ret)
}


##' @export
VSURF <- function (x, ...) {
  UseMethod("VSURF")
}


##' @export
VSURF.parallel <- function (x, ...) {
  UseMethod("VSURF.parallel")
}
