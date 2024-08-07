% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/VSURF_interp.R
\name{VSURF_interp}
\alias{VSURF_interp}
\alias{VSURF_interp.default}
\alias{VSURF_interp.formula}
\title{Interpretation step of VSURF}
\usage{
VSURF_interp(x, ...)

\method{VSURF_interp}{default}(
  x,
  y,
  vars,
  ntree.interp = 100,
  nfor.interp = 25,
  nsd = 1,
  RFimplem = "randomForest",
  parallel = FALSE,
  ncores = detectCores() - 1,
  clusterType = "PSOCK",
  verbose = TRUE,
  ntree = NULL,
  ...
)

\method{VSURF_interp}{formula}(formula, data, ..., na.action = na.fail)
}
\arguments{
\item{x, formula}{A data frame or a matrix of predictors, the columns represent
the variables. Or a formula describing the model to be fitted.}

\item{...}{others parameters to be passed on to the \code{randomForest}
function (see ?randomForest for further information).}

\item{y}{A response vector (must be a factor for classification problems and
numeric for regression ones).}

\item{vars}{A vector of variable indices. Typically, indices of variables
selected by thresholding step (see value \code{varselect.thres} of
\code{\link{VSURF_thres}} function).}

\item{ntree.interp}{Number of trees of each forest grown.}

\item{nfor.interp}{Number of forests grown.}

\item{nsd}{Number of times the standard deviation of the minimum value of
\code{err.interp} is multiplied. See details below.}

\item{RFimplem}{Choice of the random forests implementation to use :
"randomForest" (default), "ranger" or "Rborist" (not that if "Rborist" is
chosen, "randoForest" will still be used for the first step
\code{VSURF_thres}). If a vector of length 3 is given, each coordinate is
passed to each intermediate function: \code{VSURF_thres},
\code{VSURF_interp}, \code{VSURF_pred}, in this order.}

\item{parallel}{A logical indicating if you want VSURF to run in parallel on
multiple cores (default to FALSE). If a vector of length 3 is given,
each coordinate is passed to each intermediate function: \code{VSURF_thres},
\code{VSURF_interp}, \code{VSURF_pred}, in this order.}

\item{ncores}{Number of cores to use. Default is set to the number of cores
detected by R minus 1.}

\item{clusterType}{Type of the multiple cores cluster used to run VSURF in
parallel. Must be chosen among "PSOCK" (default: SOCKET cluster available
locally on all OS), "FORK" (local too, only available for Linux and Mac
OS), "MPI" (can be used on a remote cluster, which needs \code{snow} and
\code{Rmpi} packages installed), "ranger" and "Rborist" for internal
parallelizations of those packages (not that if "Rborist" is
chosen, "SOCKET" will still be used for the first step
\code{VSURF_thres}). If a vector of length 2 is given, each
coordinate is passed to each intermediate function: \code{VSURF_thres},
\code{VSURF_interp}, in this order.}

\item{verbose}{A logical indicating if information about method's progress
(included progress bars for each step) must be printed (default to TRUE).
Adds a small extra overload.}

\item{ntree}{(deprecated) Number of trees in each forest grown for
"thresholding step".}

\item{data}{a data frame containing the variables in the model.}

\item{na.action}{A function to specify the action to be taken if NAs are
found.  (NOTE: If given, this argument must be named, and as
\code{randomForest} it is only used with the formula-type call.)}
}
\value{
An object of class \code{VSURF_interp}, which is a list with the
 following components:

 \item{varselect.interp}{A vector of indices of selected variables.}

 \item{err.interp}{A vector of the mean OOB error rates of the embedded
 random forests models.}

 \item{sd.min}{The standard deviation of OOB error rates associated to the
 random forests model attaining the minimum mean OOB error rate.}

 \item{num.varselect.interp}{The number of selected variables.}

 \item{varselect.thres}{A vector of indexes of variables selected after
 "thresholding step", sorted according to their mean VI, in decreasing
 order.}

 \item{nsd}{Value of the parameter in the call.}

 \item{comput.time}{Computation time.}

 \item{RFimplem}{The RF implementation used to run
 \code{VSURF_interp}.}

 \item{ncores}{The number of cores used to run \code{VSURF_interp} in
 parallel (NULL if VSURF_interp did not run in parallel).}

 \item{clusterType}{The type of the cluster used to run \code{VSURF_interp}
 in parallel (NULL if VSURF_interp did not run in parallel).}

 \item{call}{The original call to \code{VSURF}.}

 \item{terms}{Terms associated to the formula (only if formula-type call was
 used).}
}
\description{
Interpretation step aims to select all variables related to the response for
interpretation purpose. This is the second step of the \code{\link{VSURF}}
function. It is designed to be executed after the thresholding step
\code{\link{VSURF_thres}}.
}
\details{
\code{nfor.interp} embedded random forests models are grown, starting with the
random forest build with only the most important variable and ending with all
variables.  Then, \code{err.min} the minimum mean out-of-bag (OOB) error rate
of these models and its associated standard deviation \code{sd.min} are
computed.  Finally, the smallest model (and hence its corresponding variables)
having a mean OOB error less than \code{err.min} + \code{nsd} * \code{sd.min}
is selected.

Note that, the \code{mtry} parameter of \code{randomForest} is set to its
default value (see \code{\link[randomForest]{randomForest}}) if \code{nvm}, the number of
variables in the model, is not greater than the number of observations, while
it is set to \code{nvm/3} otherwise. This is to ensure quality of OOB error
estimations along embedded RF models.
}
\examples{

data(iris)
iris.thres <- VSURF_thres(iris[,1:4], iris[,5])
iris.interp <- VSURF_interp(iris[,1:4], iris[,5],
  vars = iris.thres$varselect.thres)
iris.interp

\dontrun{
# A more interesting example with toys data (see \code{\link{toys}})
# (a few minutes to execute)
data(toys)
toys.thres <- VSURF_thres(toys$x, toys$y)
toys.interp <- VSURF_interp(toys$x, toys$y,
  vars = toys.thres$varselect.thres)
toys.interp}

}
\references{
Genuer, R. and Poggi, J.M. and Tuleau-Malot, C. (2010),
 \emph{Variable selection using random forests}, Pattern Recognition Letters
 31(14), 2225-2236

Genuer, R. and Poggi, J.M. and Tuleau-Malot, C. (2015),
 \emph{VSURF: An R Package for Variable Selection Using Random Forests}, The
 R Journal 7(2):19-33
}
\seealso{
\code{\link{VSURF}}, \code{\link{tune}}
}
\author{
Robin Genuer, Jean-Michel Poggi and Christine Tuleau-Malot
}
