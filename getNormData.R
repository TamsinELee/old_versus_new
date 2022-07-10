# 9 July 2022
# Tamsin E. Lee
# code to match article at https://tamsinelee.com/2022/07/08/old-versus-new/

# Getting the normalised data based on their learning curve, and the subsection each disease occupies.

getNormData <- function(rangeMat, growthVec, noise) {
  
  options(warn = -1)
  
  Nx           <- 41
  
  DFall        <- data.frame()
  
  x_used       <- seq(0, 100, length = Nx) 
  
  for (kd in 1:NDiseases){
   
    thisRange    <- rangeMat[kd,]
    x_range      <- seq(thisRange[1], thisRange[2], length.out = Nx) ### This is the important bit that changes for each disease. 
    x_orig       <-  x_range 
    
    DF           <- data.frame(matrix(nrow = length(x_used), ncol = 2))
    colnames(DF) <- c("bankroll", "Knowledge")
    DF$bankroll  <- x_used
  
    thisS                               <-  1 / (1 + exp(-growthVec[kd]  * x_orig))
    thisS_scaled                        <- (thisS - min(thisS))/(max(thisS) - min(thisS)) * 100 
    DF$Knowledge                        <- thisS_scaled
    
    if (noise == 1){
      thisS_scaled_noise                <- (thisS - min(thisS))/(max(thisS) - min(thisS)) * 100 + 
                                           runif(Nx, -100/x_used, 100/x_used) # To add noise (which gets smaller as amount increases). 
      thisS_scaled_noise[1]             <- 0
      thisS_scaled_noise[length(thisS)] <- 100
      DF$Knowledge                      <-  pmax(0, thisS_scaled_noise)
    }# with noise
    
   
    DF$diseaseID                        <- kd
    
    DFall <- rbind(DFall, DF)
  }
  
  return(DFall)
  
}

  
  
  