library(MASS)
library(RColorBrewer)
library(class)

mycols <- brewer.pal(8, "Dark2")[c(3,2)]
cols <- mycols[y+1]
colsPredicted <- mycols[as.numeric(yPredicted>0.5)+1]

linear.fit <- lm(y~X1+X2)
m <- -linear.fit$coef[2]/linear.fit$coef[3]
b <- (0.5 - linear.fit$coef[1])/linear.fit$coef[3]

plot(X[,c(2,3)],type="n")
abline(b,m)
points(X[,c(2,3)], col=colshat, pch=".")
points(X[,c(2,3)],col=cols)
title("Linear regression")
######################################3

# KNN (1)
yPredicted <- knn(X, newx, y, k=1)
cols <- mycols[as.numeric(y)+1]

XLIM <- range(X[,1])
tmpx <- seq(XLIM[1], XLIM[2], len=GS)

YLIM <- range(X[,2])
tmpy <- seq(YLIM[1], YLIM[2], len=GS)


plot(X, xlab="X1", ylab="X2",type="n")
contour(tmpx, tmpy, matrix(as.numeric(yhat),GS,GS), levels=c(1,2), add=TRUE, drawlabels=FALSE)
contour(XLIM[1], XLIM[2], size(X)[1],
        YLIM[1], YLIM[2], size(X)[1],
        matrix(as.numeric(y),size(X)[1],size(X)[1]))
points(X[,1],X[,2], col=cols)
title("KNN (1)")

# KNN (15)
yhat <- knn(x, newx, y, k=15)
colshat <- mycols[as.numeric(yhat)]

plot(x, xlab="X1", ylab="X2", xlim=XLIM, ylim=YLIM, type="n")
points(newx, col=colshat, pch=".")
contour(tmpx, tmpy, matrix(as.numeric(yhat),GS,GS), levels=c(1,2), add=TRUE, drawlabels=FALSE)
points(x, col=cols)
title("KNN (15)")