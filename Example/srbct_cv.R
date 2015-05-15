# Example of the use of an external cross-validation around VSURF variable selection procedure.
# WARNING: lots of calculations are done. It takes almost 20 minutes on a linux computing server with 40 cores
library(VSURF)
data(srbct, package = "mixOmics")
set.seed(322, "L'Ecuyer-CMRG")

K <- 5
ncores <- 40
clusterType <- "FORK"

x <- srbct$gene
y <- srbct$class
n <- length(y)
folds <- replicate(ceiling(n/K), sample(1:K))[1:n]

errtest.mat <- matrix(nrow=K, ncol=2)
colnames(errtest.mat) <- c("interp", "pred")
res.cv <- vector("list", K)

for (k in 1:K) {
    xtrain <- x[-which(folds==k),]
    ytrain <- y[-which(folds==k)]
    xtest <- x[which(folds==k),]
    ytest <- y[which(folds==k)]

    vsurf.fold <- VSURF(xtrain, ytrain, para = TRUE, clusterType="FORK", ncores=40)
    errtest.mat[k, 1] <- sum(ytest != predict(vsurf.fold, newdata = xtest, step = "interp"))
    errtest.mat[k, 2] <- sum(ytest != predict(vsurf.fold, newdata = xtest, step = "pred"))
    res.cv[[k]] <- vsurf.fold
}

errtest <- colSums(errtest.mat) / n
errtest
