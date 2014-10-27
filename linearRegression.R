initData <- function(X=null, y=null){
        

        data = read.csv("HousesMLRegression.csv")
        X= cbind(1,matrix(data[,1]))
        y=matrix(data[,2])
        rm(data);
        plot(X[,2],y, main="Ploted data.")
        
        return (list(X=X,y=y))
}

##Normal equation
## NORMALEQN(X,y) computes the closed-form solution to linear 
## regression using the normal equations.
normalEquation <- function(X,y){
        thetaOptimal = matrix(0,dim(X)[2]);
        thetaOptimal = solve(t(X) %*% X) %*% t(X) %*% y;
        plot(X[,2],y,  main="Optimal theta, used Normal equation.")
        abline(thetaOptimal, col="gray")
        
        thetaOptimal
}

# cost function
computeCost <- function(X, y, theta){
        
        m = dim(y)[1]
        J = 0
        h = X%*%theta
        sqrErrors = (h - y)^2
        
        J = 1/(2*m) * sum( sqrErrors )
        
        rm(m,h,sqrErrors)
        
        J
}

#1. col always has value 1 for all rows
featureNormalization <- function(X){
        X_norm=X;
        
        mean = apply(X,2,mean)
        std<-apply(X,2,sd) #sigma
 #       std = replace(std, std==0, 1)

        for(i in 1:dim(X)[1])
                for(j in 1:dim(X)[2])
                        X_norm[i,j]<-(X[i,j]-mean[j])/std[j]
        
        X_norm[,1] = 1 
        
        rm(i,j)
}

gradientDescent <- function(X,y,alpha=0.01,numOfIter=1500){
        m = length(y)
        theta = matrix(0,dim(X)[2])

        
        J_history = matrix(0,numOfIter)        
        theta_history = matrix(0,numOfIter+1,length(theta)) 
        theta_history[1,]=0 ##init theta
        
        
        
                for(i in 1:numOfIter){
                        
                        h = X%*%theta
                        gradJ = 1/(m) * ( t(X) %*% (h - y));       
                        theta = theta - alpha * gradJ;
                 
                 
        #real    theta = theta - alpha*(1/m)*(t(X) %*% X %*% theta - t(X)%*%y)(  t(X) %*% ( X %*% theta -  y));
                # 2x1    2x1     const const (2x97 *((97x2 * 2x1)- 97x1))
                #                            2x97 *        97x1        
                # 2x1 = 2x1 - 2x1 TRUE!
                
                J = computeCost(X, y, theta)
                
                J_history[i] = J
                theta_history[i+1,] = theta
        }
        
        rm(m,J,i)
        
        plot(X[,2],y,  main="Theta found by Gradient descent method.")
        abline(theta, col="red")
        
        return (list(theta=theta,J_history=J_history,theta_history=theta_history))
}

plotConture<-function(X,y,theta_history){
        
        
        # Grid over which we will calculate J
        theta0_vals = seq(-10, 10, 0.1);
        theta1_vals = seq(-1, 4, 0.1);
        J_vals = matrix(0,length(theta0_vals), length(theta1_vals));
        
        for (i in 1:length(theta0_vals))
                for (j in 1:length(theta1_vals)){  
                        allThetaValues = c(theta0_vals[i], theta1_vals[j])    
                        J_vals[i,j] = computeCost(X, y, allThetaValues);
                }
        

        library(rgl)
        
        contour(theta0_vals,theta1_vals,J_vals,method="flattest",100, main="Contour of J-cost function.")
        points(theta_history[seq(1,dim(theta_history)[1],30),], col = "red", pch = 4)
        
        #persp(theta0_vals,theta1_vals,J_vals,theta=30,phi=30,ticktype="detailed",xlab="theta0",ylab="theta1",zlab="J",col="red") 
        persp3d(theta0_vals,theta1_vals,J_vals,xlab="theta0",ylab="theta1",zlab="J",col="red", main="J-cost function.")

   
        

}

startAll <- function(){
        
        X = initData()$X
        y = initData()$y
        
        optimalTheta = normalEquation(X,y)
        
        list = gradientDescent(X,y,alpha=0.01,numOfIter=1500)
        
        theta = list$theta
        
        data = data.frame(cbind(theta[1] + theta[2]*X[,2],y))
        names(data) = c("squareFeet", "price")
        modelGradient = glm( price ~ squareFeet , data = data)
        
        J_history = list$J_history
        theta_history = list$theta_history
        
        plotConture(X,y,theta_history)
}

## Ng slides, weighted linear regression
weighted <- function(){
        library(hydroGOF)
        
        data = read.csv("HousesMLRegression.csv")
        names(data) = c("squareFeet", "price")

        modelAnalytic = lm(price ~ squareFeet, data)
        
        bandwidth = density(data[,1])$bw ^2
        weights = ( (predict(modelAnalytic,data) - data[,2])^2 )       
        weights = (weights/bandwidth)
        weights = exp( (-1/2)*weights ) 
        
        modelWeighted = glm(price ~ squareFeet, data = data, weights = weights)
        
        plot(data)
        abline(coef = modelAnalytic$coefficient, col="red" )
        abline(coef = modelWeighted$coefficient, col="black" )
        abline(coef = theta, col="blue" ) # go to startAll( function to get theta)
        legend("topleft", c("analytic", "gradient", "waighted"), col=c("red", "blue", "black"),lty=1, bty="n", cex=0.7 )
        
        list(modelAnalytic=rmse(data$price,predict(modelAnalytic)), modelWeighted=rmse(data$price,predict(modelWeighted)), modelGradient=rmse(data$price,predict(modelGradient)) )
}
