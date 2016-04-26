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
#' @param step A character string indicating which step must be plotted (default is "all").
#' Available choices are "thres", "interp", "pred".
#' @param var.names If FALSE (default) xticks are the numbering given by the
#' sorting of VI mean, if TRUE they are the variables names.
#' @param imp.mean If TRUE (default) VI mean is plotted, if FALSE it is not.
#' @param imp.sd If TRUE (default) VI standard deviation is plotted, if FALSE
#' it is not.
#' @param nvar.imp.mean The number of variables to be kept for the VI mean plot.
#' @param nvar.imp.sd The number of variables to be kept for the VI standard
#' deviation plot.
#' @param nvar.interp The number of variables to be kept for the "interp" plot.
#' @param nvar.pred The number of variables to be kept for the "pred" plot.
#' @param \dots Arguments to be passed to \code{\link{par}} (they will affect
#' all plots) or to others methods of plot.
#' 
#' @author Robin Genuer, Jean-Michel Poggi and Christine Tuleau-Malot
#' @seealso \code{\link{VSURF}}, \code{\link{summary.VSURF}}
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
#' iris.vsurf <- VSURF(iris[,1:4], iris[,5])
#' plot(iris.vsurf)
#' plot(iris.vsurf, var.names=TRUE)
#' plot(iris.vsurf, step="thres")
#' 
#' # A more interesting example with toys data (see \code{\link{toys}})
#' # (a few minutes to execute) and intermediate functions
#' data(toys)
#' toys.vsurf <- VSURF(toys$x, toys$y)
#' plot(toys.vsurf)
#' plot(toys.vsurf, nvar.imp.mean = 50, nvar.imp.sd = 50)
#' toys.thres <- VSURF_thres(toys$x, toys$y)
#' plot(toys.thres)
#' plot(toys.thres, nvar.imp.mean = 70, imp.sd = FALSE)
#' toys.interp <- VSURF_interp(toys$x, toys$y, vars = toys.thres$varselect.thres)
#' plot(toys.interp, var.names = TRUE)
#' toys.pred <- VSURF_pred(toys$x, toys$y, err.interp = toys.interp$err.interp,
#'                         varselect.interp = toys.interp$varselect.interp)
#' plot(toys.pred, var.names = TRUE)
#' }
#' 
#' @importFrom graphics abline axis lines par plot
#' @export
plot.VSURF <- function(x, step="all", var.names=FALSE, imp.mean=TRUE,
                       imp.sd=TRUE, nvar.imp.mean=length(x$imp.mean.dec),
                       nvar.imp.sd = length(x$imp.sd.dec),
                       nvar.interp = length(x$varselect.thres),
                       nvar.pred = length(x$varselect.pred), ...) {

  par_def <- par(no.readonly = TRUE)
  
  if (step=="all") {
    par(mfrow=c(2,2), mar=c(5, 4, 2, 2)+0.1, tck=-0.02)    
    plot_imp.mean(x, var.names, nvar.imp.mean, ...)
    plot_imp.sd(x, var.names, nvar.imp.sd, ...)
    plot.VSURF_interp(x, var.names, nvar.interp, ...)
    plot.VSURF_pred(x, var.names, nvar.pred, ...)
  }

  if (step=="thres") {
    plot.VSURF_thres(x, var.names, imp.mean, imp.sd, nvar.imp.mean, nvar.imp.sd, ...)
  }
  
  if (step=="interp") {
    plot.VSURF_interp(x, var.names, nvar.interp, ...)
  }
  
  if (step=="pred") {
    plot.VSURF_pred(x, var.names, nvar.pred, ...)
  }
  par(par_def)
}


#' @rdname plot.VSURF
#' @export
plot.VSURF_thres <- function(x, var.names=FALSE, imp.mean=TRUE, imp.sd=TRUE,
                             nvar.imp.mean=length(x$imp.mean.dec),
                             nvar.imp.sd=length(x$imp.sd.dec), ...) {
  par_def <- par(no.readonly = TRUE)
  
  if (imp.mean & imp.sd) {
    par(mfrow=c(1,2))
  }

  if (imp.mean) {
    plot_imp.mean(x, var.names, nvar.imp.mean, ...)
  }
  if (imp.sd) {
    plot_imp.sd(x, var.names, nvar.imp.sd, ...)
  }
  par(par_def)
}


#' @rdname plot.VSURF
#' @export
plot.VSURF_interp <- function(x, var.names=FALSE, nvar.interp=length(x$varselect.thres), ...) {
  if (var.names) {
    if (!is.null(x$terms)) {
      input <- model.frame(terms(reformulate(attributes(x$terms)$term.labels)),
                           eval(as.expression(x$call$data)))
    }
    
    else {
      input <- eval(x$call$x)
    }
    
    plot(x$err.interp[1:nvar.interp], type="l", xaxt="n", xlab="nested models",
         ylab="OOB error", ...)
    axis(side=1, at=1:nvar.interp,
         labels=colnames(input[x$varselect.thres[1:nvar.interp]]))
  }
  else {
    plot(x$err.interp[1:nvar.interp], type="l", xlab="nested models",
         ylab="OOB error", ...)
  }
  
  abline(v=length(x$varselect.interp), col="red", ...)
}  


#' @rdname plot.VSURF
#' @export
plot.VSURF_pred <- function(x, var.names=FALSE, nvar.pred=length(x$varselect.pred), ...) {
  if (var.names) {
    if (!is.null(x$terms)) {
      input <- model.frame(terms(reformulate(attributes(x$terms)$term.labels)),
                           eval(as.expression(x$call$data)))
    }
    
    else {
      input <- eval(x$call$x)
    }
    
    plot(x$err.pred[1:nvar.pred], type="l", xaxt="n", xlab="predictive models",
         ylab="OOB error", ...)
    axis(side=1, at=1:length(x$varselect.pred[1:nvar.pred]),
         labels=colnames(input[x$varselect.pred[1:nvar.pred]]))
  }
  
  else {
    plot(x$err.pred[1:nvar.pred], type="l", xlab="predictive models",
         ylab="OOB error", ...)
  }  
}


# non-exported function which only plots VI means
plot_imp.mean <- function(x, var.names=FALSE, nvar.imp.mean=length(x$imp.mean.dec), ...) {
  # Begin "for bakward compatibility only"
  if (is.null(x$imp.mean.dec)) {
    x$imp.mean.dec <- x$ord.imp$x
    x$imp.mean.dec.ind <- x$ord.imp$ix
    nvar.imp.mean <- length(x$imp.mean.dec)
  }
  # End "for bakward compatibility only"
  
  if (var.names) {
    if (!is.null(x$terms)) {
      input <- model.frame(terms(reformulate(attributes(x$terms)$term.labels)),
                           eval(as.expression(x$call$data)))
    }
    
    else {
      input <- eval(x$call$x)
    }

    plot(x$imp.mean.dec[1:nvar.imp.mean], type="l", xaxt="n", xlab="variables",
         ylab="VI mean", ...)
    axis(side=1, at=1:nvar.imp.mean, labels=colnames(input[x$imp.mean.dec.ind])[1:nvar.imp.mean])
  }
  
  else {
    plot(x$imp.mean.dec[1:nvar.imp.mean], type="l", xlab="variables",
         ylab="VI mean", ...)
  }
  
  abline(h=x$nmin * x$min.thres, col="red", ...)
}

# non-exported function which only plots VI sds
plot_imp.sd <- function(x, var.names=FALSE, nvar.imp.sd=length(x$imp.sd.dec), ...) {
  # Begin "for bakward compatibility only"
  if (is.null(x$imp.sd.dec)) {
    x$imp.sd.dec <- x$ord.sd
    x$imp.mean.dec.ind <- x$ord.imp$ix
    nvar.imp.sd <- length(x$imp.sd.dec)
  }
  # End "for bakward compatibility only"
  
  if (var.names) {
    if (!is.null(x$terms)) {
      input <- model.frame(terms(reformulate(attributes(x$terms)$term.labels)),
                           eval(as.expression(x$call$data)))
    }
    
    else {
      input <- eval(x$call$x)
    }

    plot(x$imp.sd.dec[1:nvar.imp.sd], type="l", xaxt="n", xlab="variables",
         ylab="VI standard deviation", ...)
    axis(side=1, at=1:nvar.imp.sd, labels=colnames(input[x$imp.mean.dec.ind])[1:nvar.imp.sd])
  }          
  
  else {
    plot(x$imp.sd.dec[1:nvar.imp.sd], type="l", xlab="variables",
         ylab="VI standard deviation", ...)
  }
  
  lines(x$pred.pruned.tree, type="s", col="green", ...)
  abline(h=x$nmin * x$min.thres, col="red", lty = "dotted", ...)
}
