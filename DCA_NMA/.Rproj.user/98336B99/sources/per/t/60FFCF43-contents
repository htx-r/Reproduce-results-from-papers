############################################################
#         Master analysis for the Decision curve analysis in
#                for RRMS NMA Prediction MODEL
############################################################


##########################################################
############### LIBRARIES #################################
### Load needed variables
library(readxl)
library(ggpubr)
library(gridExtra)
library(meta)
library(netmeta)
library(ggplot2)
library(hrbrthemes)

######################################################################
####### The RCTs dataset and the predicted probabilities #######
##########         via the personalized prediction model ###############
###########       under each one of the available treatments  ####################
#################            is needed    #####################################
########################################################################

RiskData$RiskpredictedPl<-NA
RiskData$RiskpredictedDF<-NA
RiskData$RiskpredictedGA<-NA
RiskData$RiskpredictedN<-NA
##
for (i in 1:nrow(RiskData)){
  for(j in 1:99){
    if (round(RiskData$RiskPreSpecified[i],2)==round(PlF$Risknew[j],2)) {RiskData$RiskpredictedPl[i]<-PlF$prelapse[j]}
    if (round(RiskData$RiskPreSpecified[i],2)==round(DFF$Risknew[j],2)) {RiskData$RiskpredictedDF[i]<-DFF$prelapse[j]}
   if (round(RiskData$RiskPreSpecified[i],2)==round(GAF$Risknew[j],2)) {RiskData$RiskpredictedGA[i]<-GAF$prelapse[j]}
    if (round(RiskData$RiskPreSpecified[i],2)==round(NF$Risknew[j],2)) {RiskData$RiskpredictedN[i]<-NF$prelapse[j]}


     }
}



#Create a column for the risk difference RD, as the difference in
### predicted probabilities under the control treatment
#### minus the corresponding active treatment

RiskData$Treatment<-RiskData$TRT01A
RiskData$RDN<-RiskData$RiskpredictedPl-RiskData$RiskpredictedN
RiskData$RDDF<-RiskData$RiskpredictedPl-RiskData$RiskpredictedDF
RiskData$RDGA<-RiskData$RiskpredictedPl-RiskData$RiskpredictedGA


############################################### DECISION CURVE ANALYSIS  #########################################################


### for a set of threshold values, T_N=20%, T_DF=T_GA=10%
source("DCA_Constant_Thresholds.R")
Net_Benefit


#plot for fix values of DF and GA in a range of threshold probabilities for Natalizumab
source("DCA_Natalizumab_Plot.R")

#plot for a range of threshold values (common) for DF and GA between 5%-25% and 19%-40% for Natalizumab
source("DCA_ThersholdAllRanges.R")
Plot_Threshold_Ranges

rm(list=ls())
