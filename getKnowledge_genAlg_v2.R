# 9 July 2022
# Tamsin E. Lee
# code to match article at https://tamsinelee.com/2022/07/08/old-versus-new/

# Optimising allocation of money across diseases (from AbsData.R) to have maximum Knowledge gained.
# Used the genetic algorithm.

getKnowledge_genAlg_v2 <- function(absData, popSize, genSize) {
  
  DF                  <- absData

  diseaseVec          <- seq(1, NDiseases)
  filename            <- "allDiseases_finalKnowledge_test.csv"

  # Model parameters
  randomPertPerc      <- 0.01
  NGen                <- genSize
  
  finalAmount         <- matrix(NA, nrow = NDiseases, ncol = length(FFvec))
  finalKnowledge      <- matrix(NA, nrow = NDiseases, ncol = length(FFvec))
  
  for (fk in 1:length(FFvec)){
    
    print(paste("Increment amount", fk, "out of ", length(FFvec)))
    
    FF                  <- FFvec[fk]
    PP                  <- popSize
    
    NIF                 <- FF / incrementSize
    randomPertN         <- round(randomPertPerc * NIF, 0)
    
    ###
    # Initial population
    ###
    
    # The vector to sample allocation amounts from
    maxIncrements <- c()
    for (kc in 1:NDiseases){
      maxIncrements     <- c(maxIncrements, rep(kc, ceiling(maxNincrements[kc])))
    }
    
    initPop             <- data.frame(replicate(PP, sample(maxIncrements, size = NIF)))
    for (kp in 1:NIF){
      sample(seq(1:NDiseases), size = 1)
    }
    population          <- initPop
    amount              <- matrix(NA, nrow = NDiseases, ncol = PP)
    Knowledge           <- matrix(NA, nrow = NDiseases, ncol = PP)
    
    for (k1 in 1:PP){
      for (kc in 1:NDiseases){
        amount[kc, k1]              <- sum(population[, k1] == kc) * incrementSize
      } #close kc for
    } # close k1 for
    
    # Calculate amount allocated to each disease based on population
    for (k1 in 1:PP){
      for (kc in 1:NDiseases){
        amount[kc, k1]              <- sum(population[, k1] == kc) * incrementSize
      } #close kc for
      overAllocDiseases              <- which(amount[, k1] - maxBankrollVec > 1) 
      underAllocDiseases             <- setdiff(c(1:NDiseases), overAllocDiseases)      
      while (length(overAllocDiseases ) > 0){
        for (k2 in overAllocDiseases ){
          overAllocRegionIdx        <- which(population[, k1] == k2)
          overAllocNumber           <- length(overAllocRegionIdx) - sum(maxIncrements == k2) 
          population[overAllocRegionIdx[1:overAllocNumber], k1] <- sample(underAllocDiseases, overAllocNumber, replace = TRUE)
        }
        for (kc in 1:NDiseases){
          amount[kc, k1]              <- sum(population[, k1] == kc) * incrementSize
        } #close kc for
        overAllocDiseases              <- which(amount[, k1] - maxBankrollVec > 0) 
        underAllocDiseases             <- setdiff(c(1:NDiseases), overAllocDiseases)      
      } #close while
    } # close k1 PP population size
    
    for (k1 in 1:PP){
      for (kc in 1:NDiseases){
        thisAmount     <- amount[kc, k1]
        thisDF         <- DF[which(DF$diseaseID == diseaseVec[kc]),]
        Knowledge[kc, k1] <- thisDF$Knowledge[which(thisDF$bankroll == min(amount[kc, k1], max(thisDF$bankroll)))]
      }
    }
    fitnessOrder <- order(colSums(Knowledge))
    pop1_idx     <- fitnessOrder[seq(1, PP, by = 2)]
    pop2_idx     <- fitnessOrder[seq(2, PP, by = 2)] 
    
    for (GG in 1:NGen){
      
      # Mix to give a new population.
      for (kp in 1:(PP/2)){
        pop1 <- population[, pop1_idx[kp]]
        pop2 <- population[, pop2_idx[kp]]
        idx1 <- sample(seq(1:NIF), size = NIF/2)
        idx2 <- setdiff(c(1:NIF), idx1)
        population[idx1, kp]         <- pop1[idx1]     
        population[idx2, kp]         <- pop2[idx2] 
        population[idx1, kp+ (PP/2)] <- pop2[idx1] # perhaps should do another random (not the inverse) - but lazy for now 
        population[idx2, kp+ (PP/2)] <- pop1[idx2] 
        # Random pertubation
        population[sample(c(1:NIF), randomPertN), kp + (PP/2)] <- sample(c(1:NDiseases), randomPertN, replace = TRUE)
        population[sample(c(1:NIF), randomPertN), kp]          <- sample(c(1:NDiseases), randomPertN, replace = TRUE)
      }
      
      # Calculate amount allocated to each disease based on population
      for (k1 in 1:PP){
        for (kc in 1:NDiseases){
          amount[kc, k1]              <- sum(population[, k1] == kc) * incrementSize
        } #close kc for
        overAllocDiseases              <- which(amount[, k1] - maxBankrollVec > 1) # set to 1 (allows over allocation by less than 1) 
        underAllocDiseases             <- setdiff(c(1:NDiseases), overAllocDiseases)      
        while (length(overAllocDiseases ) > 0){
          for (k2 in overAllocDiseases ){
            overAllocRegionIdx        <- which(population[, k1] == k2)
            overAllocNumber           <- length(overAllocRegionIdx) - sum(maxIncrements == k2) 
            population[overAllocRegionIdx[1:overAllocNumber], k1] <- sample(underAllocDiseases, overAllocNumber, replace = TRUE)
          }
          for (kc in 1:NDiseases){
            amount[kc, k1]              <- sum(population[, k1] == kc) * incrementSize
          } #close kc for
          overAllocDiseases            <- which(amount[, k1] - maxBankrollVec > 1)  # set to 1 (allows over allocation by less than 1)
          underAllocDiseases           <- setdiff(c(1:NDiseases), overAllocDiseases)      
        } #close while
      } # close k1 PP population size
      
      # Look up Knowledge associated with the amount allocated to the disease
      for (k1 in 1:PP){
        for (kc in 1:NDiseases){
          thisAmount     <- amount[kc, k1]
          thisDF         <- DF[which(DF$diseaseID == diseaseVec[kc]),]
          Knowledge[kc, k1] <- thisDF$Knowledge[which(thisDF$bankroll == min(amount[kc, k1], max(thisDF$bankroll)))]
        }
      }
      
      # Order populations
      fitnessOrder     <- order(-colSums(Knowledge))
      pop1_idx         <- fitnessOrder[seq(1, PP, by = 2)]
      pop2_idx         <- fitnessOrder[seq(2, PP, by = 2)] 
      
      #print(round(max(colSums(Knowledge)), 0))
    }
    
    finalKnowledgePop     <- colSums(Knowledge)
    bestPop               <- which(finalKnowledgePop == max(finalKnowledgePop))
    
    finalAmount[,fk]      <- amount[, bestPop[1]]
    finalKnowledge[fk]    <- finalKnowledgePop[bestPop[1]]
    
  } #close bankroll loop
  
  return(list(finalKnowledge, finalAmount))
  
}

