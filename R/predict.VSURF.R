#' Predict method for VSURF object
#' 
#' This function predicts new data with random forests, using variables selected by VSURF only.
#' 
#' This method applies for a VSURF object. VSURF selects two sets of variables during its two
#' last steps. For each set of variables, a random forest object is created, by running
#' \code{\link{randomForest}} on training data using this set of variables only. Then the
#' \code{\link{predict.randomForest}} function is used to predict new data.
#' 
#' @param object An object of class \code{VSURF}, which is the result of the
#' \code{\link{VSURF}} function.
#' @param newdata A data frame or matrix containing new data.
#' (Note: If not given, the out-of-bag predictions of the randomForest object is returned.)
#' @param step A character string indicating which variable set must be used to train
#' the \code{randomForest} object (default is c("interp", "pred")).
#' Available choices are "thres", "interp", "pred".
#' @param \dots further parameters passed to \code{\link{randomForest}} or
#' \code{\link{predict.randomForest}} functions (depending on their names).
#' 
#' @return If only one step is indicated in \code{step}, a vector of predicted values.
#' 
#' If two or more steps are indicated in \code{step}, a data frame of predicted values
#' (each column corresponding to a variable set).
#' 
#' @author Robin Genuer, Jean-Michel Poggi and Christine Tuleau-Malot
#' @seealso \code{\link{VSURF}}
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
#' iris.learn <- sample(1:nrow(iris), nrow(iris)/2)
#' iris.vsurf <- VSURF(iris[iris.learn, 1:4], iris[iris.learn, 5], ntree = 100, nfor.thres = 20,
#'                     nfor.interp = 10, nfor.pred = 10)
#' iris.predictions <- predict(iris.vsurf, newdata = iris[-iris.learn, 1:4])
#' 
#' # A more interesting example with toys data (see \code{\link{toys}})
#' # (a few minutes to execute)
#' data(toys)
#' toys.learn <- 1:(nrow(toys$x) / 2)
#' toys.vsurf <- VSURF(toys$x[toys.learn, ], toys$y[toys.learn])
#' toys.predictions <- predict(toys.vsurf, newdata = toys$x[-toys.learn, ])}
#' 
#' @export
predict.VSURF <- function(object, newdata, step = c("interp", "pred"), ...) {
  
  if (identical(step, "all")) {
    step <- c("thres", "interp", "pred")
  }
  
  if (!is.null(object$terms)) {
    x <- model.frame(terms(reformulate(attributes(object$terms)$term.labels)),
                     eval(as.expression(object$call$data)))
    rownames_del <- intersect(rownames(x), object$na.action)
    x <- x[-which(rownames(x) %in% rownames_del),]
    train_data <- object$call$data
    eval(parse(text = paste("y <- ", train_data, "$", object$call$formula[[2]],
                            sep = "")))
    y <- y[-object$na.action]
  }
  
  else {  
    x <- eval(object$call$x)
    y <- eval(object$call$y)
  }
  
  if (is.null(colnames(x)) & is.null(colnames(newdata))) {
    colnames(x) <- 1:ncol(x)
    colnames(newdata) <- 1:ncol(newdata)
  }
  
  if ("thres" %in% step) {
    varsel <- object$varselect.thres
    rf_thres <- randomForest::randomForest(x[, varsel, drop = FALSE], y, ...)
    pred_thres <- predict(rf_thres, newdata, ...)
  }
  
  if ("interp" %in% step) {
    varsel <- object$varselect.interp
    rf_interp <- randomForest::randomForest(x[, varsel, drop = FALSE], y, ...)
    pred_interp <- predict(rf_interp, newdata, ...)
  }
  
  if ("pred" %in% step) {
    varsel <- object$varselect.pred
    rf_pred <- randomForest::randomForest(x[, varsel, drop = FALSE], y, ...)
    pred_pred <- predict(rf_pred, newdata, ...)
  }
  
  if (length(step) == 1) {
    eval(parse(text = paste("predictions <- pred_", step, sep = "")))
  }
  
  if (length(step) > 1) {
    predictions <- data.frame(matrix(NA, nrow = nrow(newdata), ncol = length(step)))
    names(predictions) <- step
    for (i in seq_along(step)) {
      eval(parse(text = paste("predictions[i] <- pred_", step[i], sep = "")))
    }
  }
  
  out <- predictions
}