go <- function(M,r,iter=10,betha=0.7){
        
        r_history = matrix(0,iter,3)
        
        for(i in 1:iter){
                r1=betha*M%*%r  + (1-betha)/3
                r_history[i] = r
        }
        
        return (list(r=r,r_history=r_history)) 
  
}