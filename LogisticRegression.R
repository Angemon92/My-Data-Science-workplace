## Machine learning - COURSERA
startAll <- function(){
     
        X = initData()$X
        y = initData()$y
        
        list = gradientDescent(X,y,alpha=0.001,numOfIter=2000)
        theta = list$theta
        J_history = list$J_history
        theta_history = list$theta_history
        
        plotLogicReg(X,y,theta)
        accurancy(X,y,theta)

#use lm() function
        thetaFromModel = useLinearModel(X,y)
        plotLogicReg(X,y,thetaFromModel)
        accurancy(X,y,thetaFromModel)

}


initData <- function(){
        library(ggplot2)
        
        dataDF = read.csv("ExamMLLogRegression.csv",head=F); 
        
        data = as.matrix(sapply(dataDF, as.numeric)) 
        
        X= cbind(1,data[,c(1,2)])
        y= data[,3]
      
        qplot(Exam.1.score,Exam.2.score,data=dataDF,colour=Admitted)
        rm(dataDF)
        
        return (list(X=X,y=y))
}

computeCost <-function(X,y,theta){
        
        m = length(y)
        J = 0
        h = sigmoid(X%*%theta)
        
        J = ( t(-y) %*% log(h)- t(1-y) %*% log(1-h)) /m
        
        rm(m,h)
        
        J
}


gradientDescent <- function(X,y,alpha,numOfIter){
        library(pracma)
        
        
        m = length(y)
        theta=matrix(0,dim(X)[2])
        J = 0
        grad = matrix(0,length(theta));
        
        J_history = matrix(0,numOfIter)        
        theta_history = matrix(0,numOfIter+1,length(theta)) 
        theta_history[1,]=0 ##init theta
        
        for(i in 1:numOfIter){ 
        
                h = sigmoid(X%*%theta)
                grad = (t(X)%*%(h - y))/m
                theta = theta - alpha * grad
        
                J = computeCost(X,y,theta)
                
                J_history[i] = J
                theta_history[i+1,] = theta
        }
        
        plotJHistory(J_history,numOfIter)
        
        return (list(theta=theta,J_history=J_history,theta_history=theta_history))
}

predict <- function(X,theta){
#p = PREDICT(theta, X) computes the predictions for X using a
#threshold at 0.5 (i.e., if sigmoid(theta'*x) >= 0.5, predict 1)    
       
        p = round(sigmoid(X%*%theta))
        
}


plotLogicReg <- function(X,y,theta){
        
        library(MASS)
        library(RColorBrewer)
        library(class)
        
        X1 = X[,2]
        X2 = X[,3]
        
        mycols <- brewer.pal(8, "Dark2")[c(3,2)]
        cols <- mycols[y+1]
#       colsPredicted <- mycols[as.numeric(yPredicted>0.5)+1]     
 
        m <- -theta[2]/theta[3]
        b <- (0.5 - theta[1])/theta[3]

        plot(X1,X2,xlab="X1", ylab="X2",type="n")
        abline(b,m)
#       points(X[,c(2,3)], col=colsPredicted, pch=".")
        points(X1,X2,col=cols)
        title("Linear regression")
        
}

plotJHistory <- function(J_history,numOfIter=1500){
        
        plot(c(1:numOfIter),J_history[,1],type="l")
}

accurancy <- function(X,y,theta){
        p = predict(X, theta)
        accurancy = mean(as.numeric(p == y)) * 100
        rm(p)
        
        return (accurancy)
}

useLinearModel <- function(X,y){
        
        X1 = X[,2]
        X2 = X[,3]
        
        modelFit = lm(y ~ X1+X2)
        theta = modelFit$coef
        return(theta)
        
}

## Weighted logistic regression

playWithPlots < function(){
        data = read.csv("ExamMLLogRegression.csv",head=F);
        names(data) = c("test1","test2","test3")
        
        model = glm( pass ~ test1 + test2, data, family="gaussian" )
        
# playing with data        
        par(mfrow=c(3,2))
        
        plot(data[,1], data[,2], main="Data", xlab="test1", ylab="test2")
        
        plot(density(data[,1]), col="blue", main="Density distribution")
        lines(density(data[,2]), col="green") 
        legend("bottom", c("test1","test2"), col=c("blue","green"), bty="n", lty=1,  cex=0.7)
        
        #same for log2() and log10()
        plot(density(log(data[,1])), col="blue", main="log() density distribution")
        lines(density(log(data[,2])), col="green") 
        legend("bottom", c("test1","test2"), col=c("blue","green"), bty="n", lty=1,  cex=0.7)
        
        plot(density( (data[,1]-mean(data[,1]))/var(data[,1]) ), col="blue", main="(x-mean)/variance density distribution")
        lines(density( (data[,2]-mean(data[,2]))/var(data[,2]) ), col="green") 
        legend("bottom", c("test1","test2"), col=c("blue","green"), bty="n", lty=1,  cex=0.7)
        
        plot(density( data[,1]/var(data[,1]) ), col="blue", main="x/variance density distribution")
        lines(density( data[,2]/var(data[,2]) ), col="green") 
        legend("bottom", c("test1","test2"), col=c("blue","green"), bty="n", lty=1,  cex=0.7)
        
        plot(density( data[,1]/(max(data[,1])-min(data[,1])) ), col="blue", main="x/max()-min() density distribution")
        lines(density( data[,2]/(max(data[,2])-min(data[,2])) ), col="green") 
        legend("bottom", c("test1","test2"), col=c("blue","green"), bty="n", lty=1,  cex=0.7)
        
}


