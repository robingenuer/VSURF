% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/VSURF_pred.R
\name{VSURF_pred}
\alias{VSURF_pred}
\alias{VSURF_pred.default}
\alias{VSURF_pred.formula}
\title{Prediction step of VSURF}
\usage{
VSURF_pred(x, ...)

\method{VSURF_pred}{default}(
  x,
  y,
  err.interp,
  varselect.interp,
  ntree.pred = 100,
  nfor.pred = 10,
  nmj = 1,
  RFimplem = "randomForest",
  parallel = FALSE,
  ncores = detectCores() - 1,
  verbose = TRUE,
  ntree = NULL,
  ...
)

\method{VSURF_pred}{formula}(formula, data, ..., na.action = na.fail)
}
\arguments{
\item{x, formula}{A data frame or a matrix of predictors, the columns
represent the variables. Or a formula describing the model to be fitted.}

\item{...}{others parameters to be passed on to the \code{randomForest}
function (see ?randomForest for further information).}

\item{y}{A response vector (must be a factor for classification problems and
numeric for regression ones).}

\item{err.interp}{A vector of the mean OOB error rates of the embedded random
forests models build during interpretation step (value \code{err.interp} of
function \code{\link{VSURF_interp}}).}

\item{varselect.interp}{A vector of indices of variables selected after
interpretation step.}

\item{ntree.pred}{Number of trees of each forest grown.}

\item{nfor.pred}{Number of forests grown.}

\item{nmj}{Number of times the mean jump is multiplied. See details below.}

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
An object of class \code{VSURF_pred}, which is a list with the
  following components:

  \item{varselect.pred}{A vector of indices of variables selected after
  "prediction step".}

  \item{err.pred}{A vector of the mean OOB error rates of the random forests
  models build during the "prediction step".}

  \item{mean.jump}{The mean jump value computed during the "prediction
  step".}

  \item{num.varselect.pred}{The number of selected variables.}

  \item{nmj}{Value of the parameter in the call.}

  \item{comput.time}{Computation time.}

  \item{RFimplem}{The RF implementation used to run
  \code{VSURF_pred}.}

  \item{call}{The original call to \code{VSURF}.}

  \item{terms}{Terms associated to the formula (only if formula-type call was
  used).}
}
\description{
Prediction step refines the selection of interpretation step
\code{\link{VSURF_interp}} by eliminating redundancy in the set of variables
selected, for prediction purpose. This is the third step of the
\code{\link{VSURF}} function.
}
\details{
\code{nfor.pred} embedded random forests models are grown, starting with the
random forest build with only the most important variable. Variables are
added to the model in a stepwise manner. The mean jump value \code{mean.jump}
is calculated using variables that have been left out by interpretation step,
and is set as the mean absolute difference between mean OOB errors of one
model and its first following model. Hence a variable is included in the
model if the mean OOB error decrease is larger than \code{nmj} *
\code{mean.jump}.

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
iris.pred <- VSURF_pred(iris[,1:4], iris[,5],
  err.interp = iris.interp$err.interp,
  varselect.interp = iris.interp$varselect.interp)
iris.pred

\dontrun{
# A more interesting example with toys data (see \code{\link{toys}})
# (a few minutes to execute)
data(toys)
toys.thres <- VSURF_thres(toys$x, toys$y)
toys.interp <- VSURF_interp(toys$x, toys$y,
  vars = toys.thres$varselect.thres)
toys.pred <- VSURF_pred(toys$x, toys$y, err.interp = toys.interp$err.interp,
  varselect.interp = toys.interp$varselect.interp)
toys.pred}

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
\code{\link{VSURF}}
}
\author{
Robin Genuer, Jean-Michel Poggi and Christine Tuleau-Malot
}
