#' Tuning of the thresholding and interpretation steps of VSURF
#' 
#' This function allows to tune the "thresholding" and "interpretation step" of
#' VSURF, without rerunning all computations.
#' 
#' In \code{\link{VSURF.thres}} function, the actual threshold is performed
#' like this: only variables with a mean VI larger than \code{nmin} *
#' \code{min.thres} are kept.  The function \code{tune.VSURF.thres} allows you
#' to change the value of \code{nmin} (which multiply the estimated threshold
#' value \code{min.thres}), without rerunning all computations.  To get a
#' softer threshold than default, choose a value of \code{nmin} less than 1,
#' and to get a harder one, choose a value larger than 1.
#' 
#' In \code{\link{VSURF.interp}} function, the smallest model (and hence its
#' corresponding variables) having a mean OOB error rate less than
#' \code{err.min} + \code{nsd} * \code{sd.min} is selected.  The function
#' \code{tune.VSURF.interp} allows to change the value of \code{nsd} (which
#' multiply the standard deviation of the minimum OOB error rate
#' \code{sd.min}), without rerunning all computations.  To get a larger model
#' than default, choose a value of \code{nsd} less than 1, and to get a smaller
#' one, choose a value larger than 1.
#' 
#' @aliases tune tune.VSURF.thres tune.VSURF.interp
#' 
#' @param x An object of class \code{VSURF.thres} or \code{VSURF.interp}, which
#' is the result of the \code{\link{VSURF.thres}} or \code{\link{VSURF.interp}}
#' function.
#' @param nmin Number of times the "minimum value" is multiplied to set
#' threshold value. See details below.
#' @param nsd Number of times the standard deviation of the minimum value of
#' \code{err.interp} is multiplied. See details below.
#' @param \dots Not used.
#' 
#' @return An object with the same structure than the original output (from
#' \code{\link{VSURF.thres}} or \code{\link{VSURF.interp}}).
#' @author Robin Genuer, Jean-Michel Poggi and Christine Tuleau-Malot
#' @seealso \code{\link{VSURF}}, \code{\link{VSURF.thres}},
#' \code{\link{VSURF.interp}}
#' @references Genuer, R. and Poggi, J.M. and Tuleau-Malot, C. (2010),
#' \emph{Variable selection using random forests}, Pattern Recognition Letters
#' 31(14), 2225-2236
#' @examples
#' 
#' \dontrun{
#' data(iris)
#' iris.thres <- VSURF.thres(x=iris[,1:4], y=iris[,5], ntree=100, nfor.thres=20)
#' iris.thres.tuned <- tune(x=iris.thres, nmin=10)
#' iris.thres.tuned
#' iris.interp <- VSURF.interp(x=iris[,1:4], y=iris[,5], vars=iris.thres$varselect.thres,
#'                             nfor.interp=10)
#' iris.interp.tuned <- tune(x=iris.interp, nsd=10)
#' iris.interp.tuned
#' }
#' 
#' @rdname tune
#' @method tune VSURF.thres
#' @export tune.VSURF.thres
tune.VSURF.thres <- function (x, nmin = 1, ...) {
  
  ord.imp <- x$ord.imp
  ord.sd <- x$ord.sd
  min.pred <- x$min.thres
  mean.perf <- x$mean.perf
  pred.pruned.tree <- x$pred.pruned.tree
  
  w <- which(ord.imp$x < nmin * min.pred)
  if (length(w) == 0) {
    s <- length(ord.sd)
  }
  else {
    s <- min(w)-1
  }
  
  varselect.thres <- ord.imp$ix[1:s]
  imp.varselect.thres <- ord.imp$x[1:s]
  
  output <- list('varselect.thres' = varselect.thres,
                 'imp.varselect.thres' = imp.varselect.thres, 
                 'min.thres' = min.pred,
                 'num.varselect' = s,
                 'ord.imp' = ord.imp,
                 'ord.sd' = ord.sd,
                 'mean.perf' = mean.perf,
                 'pred.pruned.tree' = pred.pruned.tree)
}

#' @rdname tune
#' @method tune VSURF.interp
#' @export tune.VSURF.interp
tune.VSURF.interp <- function (x, nsd = 1, ...) {
  
  err.interp <- x$err.interp
  sd.min <- x$sd.min
  vars <- x$varselect.thres

  var.min <- which.min(err.interp)
  nvarselect <- min(which(err.interp <= (err.interp[var.min] + nsd * sd.min)))
  varselect <- vars[1:nvarselect]

  output <- list('varselect.interp' = varselect,
                 'err.interp' = err.interp,
                 'sd.min' = sd.min,
                 'num.varselect.interp'= length(varselect),
                 'varselect.thres' = vars)
}


#' @export
tune <- function (x, ...) {
  UseMethod("tune")
}
