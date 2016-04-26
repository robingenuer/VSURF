#' Tuning of the thresholding and interpretation steps of VSURF
#' 
#' This function allows to tune the "thresholding" and "interpretation step" of
#' VSURF, without rerunning all computations.
#' 
#' In \code{\link{VSURF_thres}} function, the actual threshold is performed
#' like this: only variables with a mean VI larger than \code{nmin} *
#' \code{min.thres} are kept.  The function \code{tune.VSURF_thres} allows you
#' to change the value of \code{nmin} (which multiply the estimated threshold
#' value \code{min.thres}), without rerunning all computations.  To get a
#' softer threshold than default, choose a value of \code{nmin} less than 1,
#' and to get a harder one, choose a value larger than 1.
#' 
#' In \code{\link{VSURF_interp}} function, the smallest model (and hence its
#' corresponding variables) having a mean OOB error rate less than
#' \code{err.min} + \code{nsd} * \code{sd.min} is selected.  The function
#' \code{tune.VSURF_interp} allows to change the value of \code{nsd} (which
#' multiply the standard deviation of the minimum OOB error rate
#' \code{sd.min}), without rerunning all computations.  To get a larger model
#' than default, choose a value of \code{nsd} less than 1, and to get a smaller
#' one, choose a value larger than 1.
#' 
#' @param x An object of class \code{VSURF_thres} or \code{VSURF_interp}, which
#' is the result of the \code{\link{VSURF_thres}} or \code{\link{VSURF_interp}}
#' function.
#' @param nmin Number of times the "minimum value" is multiplied to set
#' threshold value. See details below.
#' @param nsd Number of times the standard deviation of the minimum value of
#' \code{err.interp} is multiplied. See details below.
#' @param \dots Not used.
#' 
#' @return An object with the same structure than the original output (from
#' \code{\link{VSURF_thres}} or \code{\link{VSURF_interp}}).
#' @author Robin Genuer, Jean-Michel Poggi and Christine Tuleau-Malot
#' @seealso \code{\link{VSURF}}, \code{\link{VSURF_thres}},
#' \code{\link{VSURF_interp}}
#' @references Genuer, R. and Poggi, J.M. and Tuleau-Malot, C. (2010),
#' \emph{Variable selection using random forests}, Pattern Recognition Letters
#' 31(14), 2225-2236
#' @references Genuer, R. and Poggi, J.M. and Tuleau-Malot, C. (2015),
#' \emph{VSURF: An R Package for Variable Selection Using Random Forests},
#' The R Journal 7(2):19-33
#' @examples
#' 
#' \dontrun{
#' data(iris)
#' iris.thres <- VSURF_thres(iris[,1:4], iris[,5], ntree = 100, nfor.thres = 20)
#' iris.thres.tuned <- tune(iris.thres, nmin = 10)
#' iris.thres.tuned
#' iris.interp <- VSURF_interp(iris[,1:4], iris[,5], vars = iris.thres$varselect.thres,
#'                             nfor.interp = 10)
#' iris.interp.tuned <- tune(iris.interp, nsd = 10)
#' iris.interp.tuned
#' }
#' 
#' @export
tune <- function (x, ...) {
  UseMethod("tune")
}

#' @rdname tune
#' @export
tune.VSURF_thres <- function (x, nmin = 1, ...) {
  
  # Begin "for bakward compatibility only"
  if (is.null(x$imp.mean.dec)) {
    x$imp.mean.dec <- x$ord.imp$x
    x$imp.mean.dec.ind <- x$ord.imp$ix
    x$imp.sd.dec <- x$ord.sd
  }
  # End "for bakward compatibility only"
  
  if (x$min.thres == 0) {
    stop("Tuning can not be performed because the minimum value of the CART
         fit (min.thres) is null.")
  }
  
  w <- which(x$imp.mean.dec < nmin * x$min.thres)
  if (length(w) == 0) {
    s <- length(x$imp.sd.dec)
  }
  else {
    s <- min(w)-1
  }
  
  x$varselect.thres <- x$imp.mean.dec.ind[1:s]
  x$imp.varselect.thres <- x$imp.mean.dec[1:s]
  x$num.varselect.thres <- s
  x$nmin <- nmin
  
  output <- x
}

#' @rdname tune
#' @export
tune.VSURF_interp <- function (x, nsd = 1, ...) {

  if (x$sd.min == 0) {
    stop("Tuning can not be performed because the standard deviation of the minimum
         (sd.min) is null.")
  }
  
  var.min <- which.min(x$err.interp)
  x$num.varselect.interp <- min(which(x$err.interp <= (x$err.interp[var.min] + nsd * x$sd.min)))
  x$varselect.interp <- x$varselect.thres[1:x$num.varselect.interp]
  x$nsd <- nsd
  
  output <- x
}
