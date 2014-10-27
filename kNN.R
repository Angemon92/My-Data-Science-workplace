        
        x = matrix(c(95,96,97,98,95,97,95,96),4,2)
        X1 = rbind(X,x)
        X = X1
        
        ynew = c(0,0,0,0)
        y1 = append(y,ynew)
        y=y1
        
plot <- function(){
        library(MASS)
        library(RColorBrewer)
        library(class)

        data = read.csv("ExamMLLogRegression.csv",head=F); 
        X= data[,-3]
        y= data[,3]

        mycols <- brewer.pal(8, "Dark2")[c(3,2)]
        cols <- mycols[y+1]

        GS <- 100 #dim(X)[1] # put data in a Gs x Gs grid
        X1LIM <- range(X[,1])
        tmpx1 <- seq(X1LIM[1], X1LIM[2], len=GS)
        X2LIM <- range(X[,2])
        tmpx2 <- seq(X2LIM[1], X2LIM[2], len=GS)

        #all possible X
        newx <- expand.grid(tmpx1, tmpx2)

        k=4

        yPredict <- knn(X, newx, y, k)
        colsPredict <- mycols[as.numeric(yPredict)]


        plot(X, xlab="X1", ylab="X2", xlim=X1LIM, ylim=X2LIM, type="n")
        points(newx, col=colsPredict, pch=".")

        contour = matrix(as.numeric(yPredict),GS,GS)

        contour(tmpx1, tmpx2, contour, levels=c(1,2), add=T, drawlabels=F)
        points(X, col=cols)
        title(sprintf("%i nearest naighbour ", k))
}

caretExample <- function(){
        library(caret)
        
        data = read.csv("ExamMLLogRegression.csv",head=F); 
        
        inTrain <- createDataPartition(y=data[,3], p = .8, list=F)
        
        train <- data[inTrain,]
        test <- data[-inTrain,]
        
        trainX <- data[inTrain,-3]
        trainY <- data[inTrain,3]
        
        testX <- data[-inTrain,-3]
        testY <- data[-inTrain,3]
        
        knnPredict <- predict(model, testX)
        #Get the confusion matrix to see accuracy value and other parameter values
        confusionMatrix(as.factor(knnPredict),as.factor(testY))
      
        
        model <- knn(trainX,trainY, k = 3,cl=)
        
        plot(testY, predict(model, testX))
}

