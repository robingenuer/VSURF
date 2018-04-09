##' A simulated dataset called toys data
##'
##' \code{toys} is a simple simulated dataset of a binary classification
##' problem, introduced by Weston et.al..
##'
##' It is an equiprobable two class problem, Y belongs to \{-1,1\}, with six
##' true  variables, the others being some noise.
##' The simulation model is defined through the conditional distribution
##' of the \eqn{X_i} for Y=y:
##' 
##' \itemize{
##'  \item with probability 0.7, X^j ~ N(yj,1) for j=1,2,3 and
##' X^j ~ N(0,1) for j=4,5,6 ;
##' 
##'  \item with probability 0.3, X^j ~ N(0,1) for j=1,2,3 and
##' X^j ~ N(y(j-3),1) for j=4,5,6 ;
##' 
##'  \item the other variables are noise, X^j ~ N(0,1)
##' for j=7,\dots,p.
##' }
##' 
##' After simulation, the obtained variables are finally standardized.
##' 
##' @format The format is a list of 2 components:
##' 
##' \describe{
##'  \item{x}{a dataframe containing input variables: with 100 obs. of 200
##' variables}
##'  \item{y}{output variable: a factor with 2 levels "-1" and "1"}
##'  }
##' 
##' @examples
##' data(toys)
##' toys.rf <- randomForest::randomForest(toys$x, toys$y)
##' toys.rf
##'
##' \dontrun{
##' # VSURF applied for toys data:
##' # (a few minutes to execute)
##' data(toys)
##' toys.vsurf <- VSURF(toys$x, toys$y)
##' toys.vsurf
##' }
##' 
##' @source   Weston, J., Elisseff, A., Schoelkopf, B., Tipping, M. (2003),
##' \emph{Use of the zero norm with linear models and Kernel methods},
##' J. Machine Learn. Res. 3, 1439-1461
##' 
##' @docType data
##' @name toys
NULL
