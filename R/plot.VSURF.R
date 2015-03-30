#' Plot of VSURF results
#' 
#' This function plots 4 graphs illustrating VSURF results.
#' 
#' The 2 graphs of the top row correspond to the "thresholding step" (and only
#' these 2 graphs are plotted by the \code{plot.VSURF_thres} function).  The
#' top left graph plots the mean variable importance in decreasing order (black
#' curve). The red horizontal line represent the value of the threshold.  The
#' top right graph plots the standard deviation of variable importance with
#' variables ordered according to their mean variable importance in decreasing
#' order (black curve). The green line represents the predictions given by a
#' CART tree fitted to the black curve (the standard deviations). Finally, the
#' dotted horizontal red line represents the minimum value of the CART
#' predictions, which actually is the value of the threshold.
#' 
#' The bottom left graph corresponds to the "interpretation step" (and only
#' this graph is plotted by the \code{plot.VSURF_interp} function). It plots
#' the mean OOB error rate of embedded random forests models (from the one with
#' only one variable as predictor, to the one with all variables kept after the
#' "thresholding step"). The vertical red line indicates the retained model.
#' 
#' The bottom right graph corresponds to the "predicton step" (and only this
#' graph is plotted by the \code{plot.VSURF_pred} function). It plots the mean
#' OOB error rate of embedded random forests models (the difference, here,
#' being that variables are added to the model in a step-wise manner). The
#' retained model is the final one.
#' 
#' @param x An object of class \code{VSURF}, \code{VSURF_thres},
#' \code{VSURF_interp} or \code{VSURF_pred}, which is the result of the
#' \code{\link{VSURF}} function (or resp. \code{\link{VSURF_thres}},
#' \code{\link{VSURF_interp}} or \code{\link{VSURF_pred}}).
#' @param nvar.imp The number of variables to be kept for the VI mean plot (top
#' left graph).
#' @param nvar.sd The number of variables to be kept for the VI standard
#' deviation plot (top left graph).
#' @param imp If TRUE (default) VI mean is plotted, if FALSE not.
#' @param imp.sd If TRUE (default) VI standard deviation is plotted, if FALSE
#' not.
#' @param var.names If FALSE (default) xticks are the numbering given by the
#' sorting of VI mean, if TRUE they are the variables names.
#' @param \dots Arguments to be passed to \code{\link{par}} (they will affect
#' all plots)
#' 
#' @author Robin Genuer, Jean-Michel Poggi and Christine Tuleau-Malot
#' @seealso \code{\link{VSURF}}, \code{\link{summary.VSURF}}
#' @references Genuer, R. and Poggi, J.M. and Tuleau-Malot, C. (2010),
#' \emph{Variable selection using random forests}, Pattern Recognition Letters
#' 31(14), 2225-2236
#' @examples
#' 
#' \dontrun{
#' data(iris)
#' iris.vsurf <- VSURF(x=iris[,1:4], y=iris[,5])
#' plot(iris.vsurf)
#' plot(iris.vsurf, var.names=TRUE)
#' 
#' # A more interesting example with toys data (see \code{\link{toys}})
#' # (a few minutes to execute) and intermediate functions
#' data(toys)
#' toys.vsurf <- VSURF(x=toys$x, y=toys$y)
#' plot(toys.vsurf)
#' plot(toys.vsurf, nvar.imp=50, nvar.sd=50)
#' toys.thres <- VSURF_thres(x=toys$x, y=toys$y)
#' plot(toys.thres)
#' par(mfrow=c(1,1))
#' plot(toys.thres, nvar.imp=70, imp.sd=FALSE)
#' toys.interp <- VSURF_interp(x=toys$x, y=toys$y, vars=toys.thres$varselect.thres)
#' plot(toys.interp, var.names=TRUE)
#' toys.pred <- VSURF_pred(x=toys$x, y=toys$y, err.interp=toys.interp$err.interp,
#'                         varselect.interp=toys.interp$varselect.interp)
#' plot(toys.pred, var.names=TRUE)
#' }
#' 
#' @export
plot.VSURF <- function(x, nvar.imp=NULL, nvar.sd=NULL, var.names=FALSE, ...) {

  if (var.names) {
    if (!is.null(x$terms)) {
      input <- model.frame(terms(reformulate(attributes(x$terms)$term.labels)),
                           eval(as.expression(x$call$data)))
    }
    
    else {input <- eval(x$call$x)}
  }
  
  if (is.null(nvar.imp)) {nvar.imp <- length(x$ord.imp$x)}
  if (is.null(nvar.sd)) {nvar.sd <- length(x$ord.sd)}
  
  par(mfrow=c(2,2), mar=c(5, 4, 2, 2)+0.1, tck=-0.02)

  if (var.names) {
      plot(x$ord.imp$x[1:nvar.imp], type="l", xaxt="n", xlab="variables",
           ylab="VI mean", ...)
      axis(side=1, at=1:nvar.imp, labels=colnames(input[x$ord.imp$ix])[1:nvar.imp])
      abline(h=x$nmin * x$min.thres, col="red")
      
      plot(x$ord.sd[1:nvar.sd], type="l", xaxt="n", xlab="variables",
           ylab="VI standard deviation", ...)
      axis(side=1, at=1:nvar.sd, labels=colnames(input[x$ord.imp$ix])[1:nvar.sd])
      lines(x$pred.pruned.tree, type="s", col="green")
      abline(h=x$nmin * x$min.thres, col="red", lty = "dotted", ...)
      
      plot(x$err.interp, type="l", xaxt="n", xlab="nested models",
           ylab="OOB error",
           ylim=c(min(x$err.interp, x$err.pred)*0.95, max(x$err.interp)*1.05), ...)
      axis(side=1, at=1:length(x$varselect.thres),
           labels=colnames(input[x$varselect.thres]))
      abline(v=length(x$varselect.interp), col="red", ...)
       
      plot(x$err.pred, type="l", xaxt="n", xlab="predictive models",
           ylab="OOB error",
           ylim=c(min(x$err.interp, x$err.pred)*0.95, max(x$err.interp)*1.05), ...)
      axis(side=1, at=1:length(x$varselect.pred),
           labels=colnames(input[x$varselect.pred]))
  }

  else {  
      plot(x$ord.imp$x[1:nvar.imp], type="l", xlab="variables",
           ylab="VI mean", ...)
      abline(h=x$nmin * x$min.thres, col="red")

      plot(x$ord.sd[1:nvar.sd], type="l", xlab="variables",
           ylab="VI standard deviation", ...)
      lines(x$pred.pruned.tree, type="s", col="green")
      abline(h=x$nmin * x$min.thres, col="red", lty = "dotted", ...)
      
      plot(x$err.interp, type="l", xlab="nested models",
           ylab="OOB error",
           ylim=c(min(x$err.interp, x$err.pred)*0.95, max(x$err.interp)*1.05), ...)
      abline(v=length(x$varselect.interp), col="red", ...)
       
      plot(x$err.pred, type="l", xlab="predictive models",
           ylab="OOB error",
           ylim=c(min(x$err.interp, x$err.pred)*0.95, max(x$err.interp)*1.05), ...)
  }
}


#' @rdname plot.VSURF
#' @export
plot.VSURF_thres <- function(x, nvar.imp=NULL, nvar.sd=NULL, imp=TRUE, imp.sd=TRUE,
                             var.names=FALSE, ...) {

  if (var.names) {
    if (!is.null(x$terms)) {
      input <- model.frame(terms(reformulate(attributes(x$terms)$term.labels)),
                           eval(as.expression(x$call$data)))
    }
    
    else {input <- eval(x$call$x)}
  }
  
  if (is.null(nvar.imp)) {nvar.imp <- length(x$ord.imp$x)}
  if (is.null(nvar.sd)) {nvar.sd <- length(x$ord.sd)}
  
  par(mar=c(5, 4, 2, 2)+0.1, tck=-0.02)
  
  if (imp & imp.sd) {
    par(mfrow=c(1,2))
  }

  if (imp) {
      if (var.names) {
          plot(x$ord.imp$x[1:nvar.imp], type="l", xaxt="n", xlab="variables",
               ylab="VI mean", ...)
          axis(side=1, at=1:nvar.imp, labels=colnames(input[x$ord.imp$ix])[1:nvar.imp])
      }

      else {
          plot(x$ord.imp$x[1:nvar.imp], type="l", xlab="variables",
               ylab="VI mean", ...)
      }

      abline(h=x$nmin * x$min.thres, col="red", ...)
  }

  if (imp.sd) {
      if (var.names) {
          plot(x$ord.sd[1:nvar.sd], type="l", xaxt="n", xlab="variables",
               ylab="VI standard deviation", ...)
          axis(side=1, at=1:nvar.sd, labels=colnames(input[x$ord.imp$ix])[1:nvar.sd])
      }          

      else {
          plot(x$ord.sd[1:nvar.sd], type="l", xlab="variables",
               ylab="VI standard deviation", ...)
      }
      
      lines(x$pred.pruned.tree, type="s", col="green", ...)
      abline(h=x$nmin * x$min.thres, col="red", lty = "dotted", ...)
  }
}


#' @rdname plot.VSURF
#' @export
plot.VSURF_interp <- function(x, var.names=FALSE, ...) {

  if (var.names) {
    if (!is.null(x$terms)) {
      input <- model.frame(terms(reformulate(attributes(x$terms)$term.labels)),
                           eval(as.expression(x$call$data)))
    }
    
    else {
      input <- eval(x$call$x)
    }
  }
  
  par(mar=c(5, 4, 2, 2)+0.1, tck=-0.02)

  if (var.names) {
      plot(x$err.interp, type="l", xaxt="n", xlab="nested models",
           ylab="OOB error", ylim=c(0, max(x$err.interp)*1.05), ...)
      axis(side=1, at=1:length(x$varselect.thres),
           labels=colnames(input[x$varselect.thres]))
  }

  else {
      plot(x$err.interp, type="l", xlab="nested models",
           ylab="OOB error", ylim=c(0, max(x$err.interp)*1.05), ...)
  }

  abline(v=length(x$varselect.interp), col="red", ...)

}


#' @rdname plot.VSURF
#' @export
plot.VSURF_pred <- function(x, var.names=FALSE, ...) {

  if (var.names) {
    if (!is.null(x$terms)) {
      input <- model.frame(terms(reformulate(attributes(x$terms)$term.labels)),
                           eval(as.expression(x$call$data)))
    }
    
    else {
      input <- eval(x$call$x)
    }
  }
  
  par(mar=c(5, 4, 2, 2)+0.1, tck=-0.02)

  if (var.names) {
      plot(x$err.pred, type="l", xaxt="n", xlab="predictive models",
           ylab="OOB error", ylim=c(0, max(x$err.pred)*1.05), ...)
      axis(side=1, at=1:length(x$varselect.pred),
           labels=colnames(input[x$varselect.pred]))
  }

  else {
      plot(x$err.pred, type="l", xlab="predictive models",
           ylab="OOB error", ylim=c(0, max(x$err.pred)*1.05), ...)
  }
}   
