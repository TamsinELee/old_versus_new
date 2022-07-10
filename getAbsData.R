# 9 July 2022
# Tamsin E. Lee
# code to match article at https://tamsinelee.com/2022/07/08/old-versus-new/

# Converting the normalised data (from NormData.R) to allow for different burden of diseases.

getAbsData<- function(normData, maxValuesRep) {
  
  allBankroll                <- list()
  allKnowledge               <- list()
  allDisease                 <- list()
  
  for (kd in 1:NDiseases){
    
    thisDiseaseNorm           <- normData %>% filter(diseaseID == kd)
    thisDiseaseMax            <- maxValuesRep[kd,]
    thisDiseaseBankroll       <- seq(0, thisDiseaseMax$bankroll, by = incrementSize)
    NthisDisease              <- length(thisDiseaseBankroll)
    

    thisDiseaseDisease        <- rep(thisDiseaseNorm$diseaseID[1], NthisDisease)
    
    thisDiseaseKnowledge      <- matrix(thisDiseaseMax$Knowledge, nrow = NthisDisease, ncol = 1)
    thisDiseaseKnowledge[1]   <- 0
    thisDiseaseBankrollDots   <- thisDiseaseNorm$bankroll   / 100 * thisDiseaseMax$bankroll
    thisDiseaseKnowledgeDots  <- thisDiseaseNorm$Knowledge  / 100 * thisDiseaseMax$Knowledge
    
    for (k1 in 2:(NthisDisease-1)){
      
      xx                      <- thisDiseaseBankroll[k1]
      idx1                    <- max(which(thisDiseaseBankrollDots - xx < 0))
      idx2                    <- idx1 + 1
      if (thisDiseaseBankrollDots[idx2] - xx == 0){
        thisDiseaseKnowledge[k1] <- thisDiseaseKnowledgeDots[idx2]
      } else { # Linear regression
        x1                    <- thisDiseaseBankrollDots[idx1]
        x2                    <- thisDiseaseBankrollDots[idx2]
        y1                    <- thisDiseaseKnowledgeDots[idx1]
        y2                    <- thisDiseaseKnowledgeDots[idx2]
        m                     <- (y2 - y1) / (x2 - x1)
        thisDiseaseKnowledge[k1]  <- m * (xx - x1) + y1
      } # close if
      allBankroll[[kd]]       <- thisDiseaseBankroll 
      allKnowledge[[kd]]      <- thisDiseaseKnowledge
    } # close k1
    allDisease[[kd]]          <- thisDiseaseDisease
  } #close kd
  
  DF <- data.frame(cbind(unlist(allDisease), unlist(allBankroll), unlist(allKnowledge)))
  colnames(DF)<- c("diseaseID", "bankroll", "Knowledge")

  return(DF)
}












