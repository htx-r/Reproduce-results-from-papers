############################################################
#       Master analysis for IPD & AD NMR Prediction MODEL,
#             with observational data & RCTS,
#
############################################################



##########################################################
############### LIBRARIES #################################
### Load needed variables
library(readxl)
library(dplyr)
library(rms)
library(lme4)
library(R2jags)
library(vcd)
library(glmnet)
library(xlsx)
library(ggpubr)
library(gridExtra)
library(devtools)
library(pROC)
library(plyr)
library(tidyverse)

##############################################################################################################################
######################## STAGE 1 - DEVELOPMENT AND VALIDATION OF THE PROGNOSTIC MODEL  ########################
###############################             USING OBSERVATIONAL DATA            ################################3
###############################################################################################################

### Need for the SMSC dataset

## Code for the 1st stage available in:
#https://github.com/htx-r/Reproduce-results-from-papers/tree/master/PrognosticModelRRMS

##############################################################################################################################
#########                    STAGE 2              ###########
#####          RECALIBRATION FOR THE RCTs DATASET  #########################################
###############################################################################################################3
###############################################################################################################

################ IPD DATA (3 RCTS & Placebo-arms from RCTs) ###############
###### Load the data for the 3 RCTs and the Placebo-arms RCTs
mydatapath="C:/Users/kc19o338/Desktop/RealWorldPredictionModel/HTx/data/IPD data from 6 Biogen trials"
mydatapath1="C:/Users/kc19o338/Desktop/RealWorldPredictionModel/HTx/Placebo Arms"
######## load data
###cleaning the data from BIOGEN, defined the outcomes in columns: RELAPSE02Year, RELAPSE01Year, names of Treatments and Drugs
source("R/CleanBiogenEdited.fun.R")
cleanBIOGENtrials<-CleanBiogenEdited.fun(mydatapath)
source("R/cleanPLACEBOtrialsEdited.fun.R")
PlaceboArms<-cleanPLACEBOtrialsEdited.fun(mydatapath1)
#use only those needed
RCTs0<-cleanBIOGENtrials$adsl01
RCTs0$TRT01A[which(RCTs0$TRT01A=="AVONEX® 30 mcg")]<-"Avonex"
###drop SENTTINEL STUDY because of combination of treatments and
### drop ADVANCE study because does not provide information for Relapse in 2 years (only for 1 year)
RCTs<-RCTs0[RCTs0$STUDYID!="SENTINEL" & RCTs0$STUDYID!="ADVANCE" ,]

##### Recalibration

##fix the data to be the same as SMSC (proper transformations) and add the variable Risk
## with the pooled estimates of stage 1
source("RCTsData.R")
# recalibrate the model for the RCTs (use of 4 different methods and we selected the one with the highest AUC)
source("Recalibration.R")
#plots for the risk distribution in RCTs
source("PlotsRCTs.R")
# The Distribution of risk in the whole dataset
RiskDistribution
#The distribution of risk for those who relapsed and did not relapse
RiskPrFactor
# statistical signifant different Risk score for those
# who relapsed and for those who didn't
t.test(RiskData$Risknew4[RiskData$outcome==1])
t.test(RiskData$Risknew4[RiskData$relapse.2y.after.study==0])

#AUC in RCTs
app_cstat_model4$auc #0.61
# calibration slope and intercept in RCTs
glm(outcome~logitp4,family="binomial",data=RiskData) #c-intercept=0.0, c-slope=1

##############################################################################################################################
######################## STAGE 3 - IPD & AD NETWORK META-REGRESSION   #################################################3
################# WITH RISK AS THE ONLY PREDICTOR AND EFFECT MODIFIER    ###################################################3
###############################################################################################################

#data for jags model
source("DataforIPDADNMR.R")
#model IPD & AD NMR with imputation for missing study-level covariates
source("R/modelIPDADNMR_imputation.R") #source the model will be used for rjags
#run the model
set.seed(2000)
IPDADNMRJAGSresultsSMSC <- jags.parallel(data = jagsdataIPDADNMR,inits=NULL,parameters.to.save = c('gamma.w','gamma.b', 'meanRisk','logitpplacebo','lognr.Gd.enhanced.lesionsAD','gamma', 'ORref','delta','u','loggenderAD', 'meanRisk','months.since.last.relapseAD','ageAD', 'edssAD', 'disease.durationAD', 'logtreatment.naive.prior.visitAD', 'logitp'),model.file = modelIPDADNMR,
                                       n.chains=2,n.iter = 10000,n.burnin = 1000,DIC=F,n.thin = 10)


# Results using Bayesian NMR model
print(IPDADNMRJAGSresultsSMSC)
traceplot(IPDADNMRJAGSresultsSMSC$BUGSoutput,varname=c("ORref","u", "gamma.w", "gamma"))
### plot for predicted probabilities to relapse under all the available options
source("GraphForPredictedRisk.R")
Graph #for absolute presicted probabilities
Plot_OR #for ORs


