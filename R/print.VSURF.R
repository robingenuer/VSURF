##' Print of VSURF results
##'
##' This function display a small description of VSURF results
##' 
##' @param x An object of class \code{VSURF}, which is the result of the
##' \code{\link{VSURF}} function.
##' @param \dots Not used.
##'
##' @seealso \code{\link{VSURF}}, \code{\link{plot.VSURF}}, \code{\link{summary.VSURF}}
##' @author Robin Genuer, Jean-Michel Poggi and Christine Tuleau-Malot
##' @references Genuer, R. and Poggi, J.M. and Tuleau-Malot, C. (2010),
##' \emph{Variable selection using random forests}, Pattern Recognition Letters
##' 31(14), 2225-2236
##' @references Genuer, R. and Poggi, J.M. and Tuleau-Malot, C. (2015),
##' \emph{VSURF: An R Package for Variable Selection Using Random Forests},
##' The R Journal 7(2):19-33
##'
##' @examples
##'
##' \dontrun{
##' data(iris)
##' iris.vsurf <- VSURF(iris[,1:4], iris[,5], ntree = 100, nfor.thres = 20,
##'                     nfor.interp = 10, nfor.pred = 10)
##' iris.vsurf
##' }
##'
##' @export
print.VSURF <- function(x, ...) {
    
    cat(paste("** VSURF results **", 
"The results object is a list of length around 20.",
"Most interesting components are the following:\n\n", sep="\n"))

    res <- array("", c(6,2), list(1:6, c("name", "description")))
    res[1,] <- c("$varselect.thres", "variables selected after thesholding step")
    res[2,] <- c("$varselect.interp", "variables selected after interpretation step")
    res[3,] <- c("$varselect.pred", "variables selected after prediction step")
    res[4,] <- c("$ord.imp$x", "mean VI in decreasing order for all variables")
    res[5,] <- c("$ord.imp$ix", "indices of the ordering of all variables VI mean")
    res[6,] <- c("mean.perf", "mean OOB rate for RF build on all variables")

    print(res)

    cat(paste("\n For more information about VSURF outputs see the VSURF help page\n"))
}
