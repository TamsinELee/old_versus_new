# 9 July 2022
# Tamsin E. Lee
# code to match article at https://tamsinelee.com/2022/07/08/old-versus-new/


library(ggplot2)
library(reshape2)
library(dplyr)

# function files

source("getNormData.R") # Getting the normalised data based on their learning curve, and the subsection each disease occupies.
source("getAbsData.R")  # Converting the normalised data (from NormData.R) to allow for different burden of diseases.
source("getKnowledge_genAlg_v2.R") # Optimising allocation of money across diseases (from AbsData.R) to have maximum Knowledge gained.


###########################################
# SCENARIO SETTINGS. 
# Comment and uncomment to explore. 
###########################################

# The number of diseases. Default is 2.
NDiseases                  <- 2

# The occupied subsection on the learning curve for each disease (a range between [-5, 5]).
#rangeMat                   <- matrix(sort(runif(2 * NDiseases, -5, 5)), nrow = NDiseases, byrow = TRUE) # random selection for any number of diseases
rangeMat                   <- matrix(c(-5, 0, 0, 5), nrow = NDiseases, byrow = TRUE) # for two diseases where the first ranges [-5, 0] and the second [0, 5]
# rangeMat                   <- matrix(c(-5, 2, -2, 5), nrow = NDiseases, byrow = TRUE) # for two diseases where their ages overlap: [-5, 2] and [-2, 5]
# rangeMat                   <- matrix(c(-5, -3, -3, 3, 2, 5), nrow = NDiseases, byrow = TRUE) # for three diseases: [-5, -3], [-3, 3], [2, 5]
# rangeMat                   <- matrix(c(-5, 2, 1, 4, 0, 5), nrow = NDiseases, byrow = TRUE) # for three diseases


# The different burden and costs of the diseases
burdenMat                  <- matrix(rep(1, 2 * NDiseases), nrow = NDiseases, byrow = TRUE) 
#burdenMat                  <- matrix(c(3, 2, 1, 1), nrow = NDiseases, byrow = TRUE) # for two diseases where the first requires 3 times more 'Knowledge' than the second, and costs 2 times more. 

# The shape of the learning curve. 
growthVec                  <- rep(1, NDiseases) # 1 for all diseases
#growthVec                 <- runif(1, 0.5, 1.8) # A random allocation between [0.5, 1.8] for each disease.

# Add noise to the learning curve (make it more wibbly and wobbly, but still the same trend) 
noise                      <- 0
#noise                      <- 1

###########################################
# Model parameters
# comment and uncomment for speed/accuracy
###########################################

#genSize                     <- 40  # for accurate results
#popSize                     <- 400 # for accurate results 

genSize                     <- 5  # for quick results
popSize                     <- 100  # for quick results 

###########################################
###########################################
###########################################

# Data parameter choices

incrementSize              <- 1
maxValues                  <- data.frame(100, 100)
colnames(maxValues)        <- c("Knowledge", "bankroll")

maxValuesRep                   <- do.call("rbind", replicate(NDiseases, maxValues, simplify = FALSE))
for (kd in 1:NDiseases){
  maxValuesRep$Knowledge[kd]   <- burdenMat[kd, 1] * maxValuesRep$Knowledge[kd] 
  maxValuesRep$bankroll[kd]    <- burdenMat[kd, 2] * maxValuesRep$bankroll[kd]
}

###########################################

# Generate data

normData                   <- getNormData(rangeMat, growthVec, noise)
#normData$Knowledge[which(normData$diseaseID == 1)] <- normData$Knowledge[which(normData$diseaseID == 2)] # to make diseases identical
absData                    <- getAbsData(normData, maxValuesRep)  

# Get split of disease burden
#absData_orig               <- absData
#absData$Knowledge[which(absData$diseaseID == 1)] <- 1 * absData$Knowledge[which(absData$diseaseID == 1)]
#diseaseKnowledgeSplit         <- 100 * max(absData$Knowledge[which(absData$diseaseID == 1)]) / 
#  (max(absData$Knowledge[which(absData$diseaseID == 2)]) + max(absData$Knowledge[which(absData$diseaseID == 1)]))

# Key values
maxBankrollVec              <- maxValuesRep$bankroll                  # the maximum amount each disease needs
maxBankrollTotal            <- sum(maxBankrollVec)                    # the total maximum amount for all diseases
maxKnowledgeTotal           <- sum(maxValuesRep$Knowledge)            # the maximum Knowledge for all diseases (if received 100% bankroll)

###########################################

# Model parameters

maxNincrements              <- maxBankrollVec/incrementSize
GAfundingSteps              <- 5
FFvec                       <- seq(GAfundingSteps, floor(maxBankrollTotal * 0.99), by = GAfundingSteps)

finalKnowledgeAmount       <- getKnowledge_genAlg_v2(absData, popSize, genSize)

###########################################

# Results

finalKnowledge             <- finalKnowledgeAmount[[1]]
finalAmount                <- finalKnowledgeAmount[[2]]

###########################################

# Quick plotting of results for a simulation with any number of diseases

DF_quick <- finalAmount
DF_quick                   <- data.frame(finalAmount) %>% mutate_at(vars(1: length(FFvec)), funs(100*./sum(.)))
colnames(DF_quick)         <- FFvec/maxBankrollTotal * 100
DF_quick                   <- t(DF_quick)
colnames(DF_quick)         <- c(paste0("Disease ",seq(1:NDiseases)))
DF_plot                    <- melt(DF_quick)
colnames(DF_plot)          <- c("bankroll", "disease", "alloc_perc")

ggplot(DF_plot, aes(x = bankroll, y = alloc_perc)) + 
  geom_line(size = 2) + 
  facet_wrap(~disease) + 
  scale_x_continuous("bankroll") + 
  scale_y_continuous("allocation percentage") + 
  theme_bw(base_size = 24) 


ggplot(absData, aes(x = bankroll, y = Knowledge, color = as.factor(diseaseID))) + 
  geom_line(size = 3) + 
  scale_x_continuous("money (yips)") + 
  theme_bw(base_size = 16) +
  theme(legend.position = c(0.25, 0.91), legend.title = element_blank(), legend.background=element_blank(), 
        aspect.ratio=1, panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"), panel.border = element_blank())



###########################################
# # PRETTY PLOTTING (specific for two diseases)
# 
# if (NDiseases == 2){
#   
#   library(grid)
#   library(ggplotify)
#   
#   colorvec_3                  <- c("#801515",  "#0D4D4D", "#567714")
#   
#   ResultsDF                   <- data.frame(matrix(NA, nrow = length(FFvec) * NDiseases, ncol = 3))
#   colnames(ResultsDF)         <- c("Bankroll_prop.", "Allocation_prop.", "Disease")
#   ResultsDF$Bankroll_prop.    <- rep(FFvec/maxBankrollTotal, NDiseases)
#   ResultsDF$Allocation_prop.  <- c(finalAmount[1,] / colSums(finalAmount),
#                                    finalAmount[2,] / colSums(finalAmount))
#   ResultsDF$Disease           <- c(rep("new", length(FFvec)), rep("old", length(FFvec)))
#   
#   g1 <- ggplot(ResultsDF, aes(x = Bankroll_prop. * 100, y = Allocation_prop. * 100, fill = Disease)) + 
#     geom_area(alpha = 0.6 , size=1) + 
#     scale_fill_manual(values=colorvec_3) + 
#     scale_x_continuous("bankroll") + 
#     scale_y_continuous("allocation percentage") + 
#     annotate("text", x = c(15, 80), y = c(95, 95), label = c("old disease", "new disease"), size = 10) + 
#     #geom_hline(yintercept = diseaseKnowledgeSplit) + 
#     theme_bw(base_size = 24) + 
#     theme(legend.position = "none", panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
#           panel.background = element_blank(), axis.line = element_line(colour = "black"), panel.border = element_blank())
#   g1
#   p1 <- as.grob(g1)
#   
#   g2 <- ggplot(absData, aes(x = bankroll, y = Knowledge, color = as.factor(diseaseID))) + 
#     geom_line(size = 3) + 
#     scale_color_manual(values=colorvec_3, labels = c("new disease", "old disease")) + 
#     scale_x_continuous("money (yips)") + 
#     theme_bw(base_size = 16) +
#     theme(legend.position = c(0.25, 0.91), legend.title = element_blank(), legend.background=element_blank(), 
#           aspect.ratio=1, panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
#           panel.background = element_blank(), axis.line = element_line(colour = "black"), panel.border = element_blank())
#   
#   p2 <- as.grob(g2)
#   
#   grid.newpage()
#   grid.draw(p1)
#   vp = viewport(x=0.24, y=.39, width=.35, height=.45)
#   pushViewport(vp)
#   grid.draw(p2)
#   upViewport()
#   #ggsave("oldk1b1_newk3b2_5005.pdf", height = 18 , width = 35, units = "cm")# height = 24 , width = 30, units = "cm")
#   #ggsave("oldk1b1_newk3b2_5005.png", height = 18 , width = 35, units = "cm") # height = 24 , width = 30, units = "cm")
# 
# }



