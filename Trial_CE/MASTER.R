############################################################
#         Master analysis for MS NMA Prediction MODEL
############################################################



##########################################################
############### LIBRARIES #################################

###load the needed libraries
library(devtools)
install_github("htx-r/CleaningData",force=TRUE)
library(CleaningData)
library(R2jags)
library(dplyr)
library(glmpath)
library(readxl)
library(car)
library(glmnet)
library(Hmisc)
library(rms)
library(gridExtra)
library(ggpubr)
library(ggplot2)
library(synthpop)
library(pmsampsize)
library(selectiveInference)
library(plyr)
library(vcd)
library(pROC)
library(metafor)
library(meta)
library(netmeta)
library(DiagrammeR)
library(tidyverse)
library(survival)
library(survminer)

#######################################################################################
####################################  DATA   ###########################################
##Open your existing data "TwoStageRCTsResults.RData" from the folder "C:\Users\kc19o338\Desktop\Real world predictions project"
RCTs
##Load the new variable EDSS dataset
ades<-read_csv("C:/Users/kc19o338/Desktop/RealWorldPredictionModel/HTx/data/IPD data from 6 Biogen trials/ades.csv")
######Fix the needed variable CDP based on EDSS measures (1 point increase (at least) in edss from bl for baseline edss 0-5.5, and 0.5 increase for those with baseline edss >=6)
#### confirmed after 3 months (sustained)

setwd("C:/Users/kc19o338/Documents/GitHub/Reproduce-results-from-papers/Trial_CE")

source("EDSS_variable.R") #data_CPD the new dataset of interest

##Keep from data_CPD dataset the variables you need
source("Final_dataset.R") #data_CPD_final the new dataset of interest
#omit rows with NAs
data_CDP_CC<-na.omit(data_CDP_final) ###2105 patients


#######################################################################################
############################ STAGE 1 - RISK MODEL ###############################################
######################################################################################

#Step 1. Obtain the EPV and the required sample size for the development of the models

source('EPVandSampleSize.R')


#Step 2. Build a risk model with shrinkage

######## Model - results of Pre-specified model after shrinkge (penalized the same with Pellegrini way in twostage model)

source("RiskModel.R")
finalmodel.pen
#Step 3. Bootstrap calibration and discrimination by using the same bootstrap sample for two models each time
###needs more than 3 hours to run! You can skip it without any problem further
###Performance table of models : discrimination and calibration
#data1<-na.omit(MSrelapse)
#todrop<-c("STUDYID","USUBJID","TRT01A")
#data1<-data1[ , !(names(data1) %in% todrop)]
#Internal_validation<-BootstrapValidation.fun(data=data1, samples = 500, alpha = 1, modelElasticNet = LASSOModel$lassomodel, modelSpecific = PreSpecifiedModel$PreSpecifiedmodel)
#Discrimination_Calibration<-as.data.frame(Internal_validation[[7]])
#Discrimination_Calibration### bootstrap optimism corrected discriminatio and calibration of the models

#Step 4. Create a dataset that includes Risk's and logit Risk's predictions for each individual and for both models
##Also make treatment and studies numerical values
source("RiskData.R")

#Step 5. Source the script for the plots of risk score
source('Plots_BaselineRisk.R')
PrognosticRisk

#######################################################################################
####################### STAGE 2 - NMR PREDICTION MODEL ###############################################
######################################################################################

#combine the dataset with the time to confirmed progressions
df1<-df1[,c(1,3,4,5)]
data_CDP_CC<-merge(data_CDP_CC, df1, by="USUBJID")


###############CDP outcome
###Step 1.  find the proper distribution for the time-to-event outcome

source('Exponential_or_Weibul.R')
source('Exponential_or_lognormal.R')
ggarrange(DEFINE_curves_EW,CONFIRM_curves_EW,AFFIRM_curves_EW, ncol = 2, nrow=2)
ggarrange(DEFINE_curves_EL,CONFIRM_curves_EL,AFFIRM_curves_EL, ncol = 2, nrow=2)

####################### Simple NMA for relapse model without risk
#source('DataForIPDNMA_both.R')
#source("R/modelIPDNMA_Relapse.fun.R") #source the model will be used for rjags

#mydatapath1="C:/Users/kc19o338/Desktop/RealWorldPredictionModel/HTx/Placebo Arms"
#source("R/cleanPLACEBOtrialsEdited.fun.R")
#PlaceboArms<-CleanPlaceboTrialsEdited.fun(mydatapath1)

#datapath="C:/Users/kc19o338/Desktop/SMSC"
#SMSCpath=paste(datapath,"/SMSC_corrected.xlsx",sep="")
#SMSCdata <- read_excel(SMSCpath)
#SMSC_placebo<-SMSCdata[SMSCdata$treatment.during.cycle==FALSE,]


### run the model
#set.seed(2000)
#IPDNMAJAGSresults_Relapse <- jags.parallel(data = jagsdataIPDNMA_Relapse,inits=NULL,parameters.to.save = c('u','logitpplacebo','logitpplacebo2','ORref','delta','logitp','logitp2'),model.file = modelIPDNMA_Relapse,
                  #                       n.chains=2,n.iter = 10000,n.burnin = 100,DIC=F,n.thin = 1)
#print(IPDNMAJAGSresults_Relapse)
#traceplot(IPDNMAJAGSresults_Relapse$BUGSoutput)



#source("R/modelIPDNMA_CDP.fun.R") #source the model will be used for rjags

### run the model
#set.seed(2000)
#IPDNMAJAGSresults_CDP <- jags.parallel(data = jagsdataIPDNMA_CDP,inits=NULL,parameters.to.save = c('u','logPlacebo','logh','delta'),model.file = modelIPDNMA_HR,
 #                                        n.chains=2,n.iter = 10000,n.burnin = 100,DIC=F,n.thin = 1)
#print(IPDNMAJAGSresults_CDP)
#traceplot(IPDNMRJAGSresults_CDP$BUGSoutput)

#Step 2.  Add proper columns in the RiskData, like arm, meanRisk, and make data for jags model etc.
source('DataForIPDNMR_CDP.R')

source("R/modelIPDNMR_CDP.fun.R") #source the model will be used for rjags

#Step 2. Run the model & results & check of traceplots


### run the model
set.seed(2000)
IPDADNMRJAGSresults_CDP <- jags.parallel(data = jagsdataIPDNMR_CDP,inits=NULL,parameters.to.save = c('transition_prob','loghazards_meanRisk','logPlacebo','g0Placebo','delta','gamma','gamma0'),model.file = modelIPDNMR_HR,
                                         n.chains=2,n.iter = 1000,n.burnin = 10,DIC=F,n.thin = 5)
print(IPDADNMRJAGSresults_CDP)
traceplot(IPDADNMRJAGSresults_CDP$BUGSoutput)
#1: DF, HR:0.7153381, 2:GA, HR:1.077884, 3:N, HR:0.5638314, 4:Placebo
set.seed(2000)
IPDADNMRJAGSresults_CDP2 <- jags.parallel(data = jagsdataIPDNMR_CDP,inits=NULL,parameters.to.save = c('logh','trans_p'),model.file = modelIPDNMR_HR,
                                         n.chains=2,n.iter = 10000,n.burnin = 100,DIC=F,n.thin = 5)
set.seed(2000)
IPDADNMRJAGSresults_CDP3 <- jags.parallel(data = jagsdataIPDNMR_CDP,inits=NULL,parameters.to.save = c('trans_p'),model.file = modelIPDNMR_HR,
                                          n.chains=2,n.iter = 1000,n.burnin = 10,DIC=F,n.thin = 5)
print(IPDADNMRJAGSresults_CDP3)
source("transition probabilities per risk")
save(IPDADNMRJAGSresults_CDP3, file="C:/Users/kc19o338/Documents/GitHub/Reproduce-results-from-papers/Trial_CE/CDP.RData")

as.mcmc(IPDADNMRJAGSresults_CDP2)
###############Relapse outcome

source('DataForIPDNMR_Relapse.R')

source("R/modelIPDNMR_Relapse.fun.R") #source the model will be used for rjags

#Step 2. Run the model & results & check of traceplots


### run the model
set.seed(2000)
IPDADNMRJAGSresults_Relapse <- jags.parallel(data = jagsdataIPDNMR_Relapse,inits=NULL,parameters.to.save = c('annualized_rate','logit_probability','delta','logitpplacebo','gamma.w','gamma'),model.file = modelIPDNMR_Relapse,
                                         n.chains=2,n.iter = 10000,n.burnin = 100,DIC=F,n.thin = 1)
print(IPDADNMRJAGSresults_Relapse)
traceplot(IPDADNMRJAGSresults_Relapse$BUGSoutput)

#1: DF, HR:0.7153381, 2:GA, HR:1.077884, 3:N, HR:0.5638314, 4:Placebo
set.seed(2000)
IPDADNMRJAGSresults_Relapse2 <- jags.parallel(data = jagsdataIPDNMR_Relapse,inits=NULL,parameters.to.save = c('logitp', 'annualized'),model.file = modelIPDNMR_Relapse,
                                             n.chains=2,n.iter = 10000,n.burnin = 100,DIC=F,n.thin = 1)



#Plots for both outcomes
source("GraphforPredicted_Risk&Hazards.R")
IPDplot_CDP
IPDplot_Relapse

rm(list=ls())
