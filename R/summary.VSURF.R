#' Summary of VSURF results
#' 
#' This function displays a summary of VSURF results
#' 
#' This function prints the total computation time of VSURF.  It also gives the
#' number of selected variables (and the computation time) at each step of
#' VSURF. In addition, it gives the number of cores and the type of cluster
#' if the parallel version of VSURF was used.
#' 
#' @param object An object of class \code{VSURF}, which is the result of the
#' \code{\link{VSURF}} function.
#' @param \dots Not used.
#' 
#' @author Robin Genuer, Jean-Michel Poggi and Christine Tuleau-Malot
#' @seealso \code{\link{VSURF}}, \code{\link{plot.VSURF}}
#' @references Genuer, R. and Poggi, J.M. and Tuleau-Malot, C. (2010),
#' \emph{Variable selection using random forests}, Pattern Recognition Letters
#' 31(14), 2225-2236
#' @references Genuer, R. and Poggi, J.M. and Tuleau-Malot, C. (2015),
#' \emph{VSURF: An R Package for Variable Selection Using Random Forests},
#' The R Journal 7(2):19-33
#' 
#' @examples
#' 
#' \dontrun{
#' data(iris)
#' iris.vsurf <- VSURF(iris[,1:4], iris[,5], ntree = 100, nfor.thres = 20,
#'                     nfor.interp = 10, nfor.pred = 10)
#' summary(iris.vsurf)
#' 
#' # A more interesting example with toys data (see \code{\link{toys}})
#' # (a few minutes to execute)
#' data(toys)
#' toys.vsurf <- VSURF(toys$x, toys$y)
#' summary(toys.vsurf)}
#' 
#' @export
summary.VSURF <- function(object, ...) {
  
  cat(paste("\n VSURF computation time:", round(object$overall.time, 1),
            attributes(object$overall.time)$units, "\n", sep=" ")
      )
  
  cat(paste("\n VSURF selected: \n",
            "\t", object$nums.varselect[1], " variables at thresholding step ",
            "(in ", round(object$comput.times[[1]], 1), " ",
            attributes(object$comput.times[[1]])$units, ")", "\n",
            "\t", object$nums.varselect[2], " variables at interpretation step ",
            "(in ", round(object$comput.times[[2]], 1), " ",
            attributes(object$comput.times[[2]])$units, ")", "\n",
            "\t", object$nums.varselect[3], " variables at prediction step ",
            "(in ", round(object$comput.times[[3]], 1), " ",
            attributes(object$comput.times[[3]])$units, ")", "\n",
            sep="")
      )

  if (!is.null(object$ncores)) {
    cat(paste("\n VSURF ran in parallel on a", object$clusterType,
              "cluster and used", object$ncores, "cores", "\n", sep=" "))
  }
}
