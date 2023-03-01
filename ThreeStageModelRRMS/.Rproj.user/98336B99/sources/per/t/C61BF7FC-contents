#################################################################
#         Master analysis for MS Prognostic model MODEL         #
#          using SMSC observational study - repeated measures   #
#               Generalized mixed effects model                 #
#################################################################



##########################################################
############### LIBRARIES #################################
### Load needed libraries
library(readxl)
library(dplyr)
library(rms)
library(lme4)
library(R2jags)
library(mitml)
library(jomo)
library(xlsx)
library(ggpubr)
library(gridExtra)
library(devtools)
library(pROC)
library(vcd)
library(plyr)
library(pmsampsize)
library(shiny)
library(shinythemes)
library(devtools)
install_git("https://github.com/BavoDC/CalibrationCurves")
library(CalibrationCurves)

# give/change your datapath (the datapath that includes the data)
mydatapath="C:/Users/kc19o338/Desktop/SMSC"

########################## DATASET NEEDED FOR THE PRE-SPECIFIED PROGNOSTIC MODEL ##############################
# function that keeps only the needed variables (selected via pre-existing prognostic models on the literature, for RRMS patients)
# and makses the proper transformations for continues and categorical variables
# (in case you want to see all summary statistics for the SMSC data before and after the transformations you can run the SMSC_Summary.R script)
source("R/FinalDataSMSC.fun.R")
SMSCdata<-FinalDataSMSC.fun(mydatapath)
# the dataset with complete cases only, no missing values at all
SMSCdataC<-na.omit(SMSCdata)


############################ Frequentist framework ######################################################################
# just a test using generalized linear mixed effects model in a frequentist setting

source("Frequentist_glmm.R")

#the results of the mixed-effects model
summary(glmer_out)
# compare them with a model with fixed effects only (glm)
summary(glm_out)

#check the correlations between the categorical variables
#assocstats(table(SMSCdata_glmer$nr.Gd.enhanced.lesions,SMSCdata_glmer$nr.relapses.2y.prior.study))
#assocstats(table(SMSCdata_glmer$nr.Gd.enhanced.lesions,SMSCdata_glmer$treatment.naive.prior.visit))
#t.test(SMSCdata_glmer$months.since.last.relapse[which(SMSCdata_glmer$nr.Gd.enhanced.lesions==0)], SMSCdata_glmer$months.since.last.relapse[which(SMSCdata_glmer$nr.Gd.enhanced.lesions==1)])
#we can see that Nr.Gd enhanced.lessions is associated with nr.relapses.prior.study, treatment.naive.prior.visit and months.since.last.relapse

#check if resstricted cubic splines are needed
source("Check_rcs.R")
anova(glm_rcs) #it seems that there is no variable with non-linear relationhip

############################ Bayesian framework ######################################################################

##### Read the data needed for the jags model
source("DataJagsPrM.R")
#source the jags model
source("R/jagsmodelSMSC.R")
# give initial values for the jags model
jags.inits <- function(){
  list("b"=c(0,0,0,0,0,0,0,0,0,0,0),"sigma"=0.02,"rho"=0.4)
}
# run the jugs model
set.seed(2000)
SMSCjagsResults <- jags.parallel(data = jagsdataSMSC ,inits= jags.inits,
                                 parameters.to.save = c('b','sigma', 'rho' ),model.file = jagsmodelSMSC,
                                 n.chains=2,n.iter = 50000,n.burnin = 10000,n.thin = 10)

#results
print(SMSCjagsResults)
#check of convergence
traceplot(SMSCjagsResults)



######################  DEALING WITH MISSING DATA  ##################
# we need the data with all the variables
source("R/MIDataSMSC.fun.R")
MISMSCdata<-MIDataSMSC.fun(mydatapath)
# step 1: Check for auxiliary variables
source("Auxiliary_variables.R")
##only kfs.2 is important auxiliary variable both for the outcome and for th covariate we want to impute
summary(glm_out_MI) #for the outcome
summary(glm_out_MI1) #for the prognostic factor with the missing values
summary(glm_out_MI2)
summary(glm_out_MI3)
summary(glm_out_MI4)
#we keep kfs.2 as auxiliary variable

## Step 2: Creation of 10 imputed datasets
source("MultipleImputations.R")
## Step 3: Calculate the Bayesian estimates in all imputed datasets
source("ModelInImputedDatasets.R")
## Step 4: use the Rubin's rules to pool the stimates & table with all the estimates in the inputed datasets
source("PooledEstimates.R")
  #table of all Bayesian results from each one of the imputed datasets
tablewithresults
  #pooled estimates
Pooled
Pooled[2,]
##Table 2 of paper
round(Pooled$estimates[2:11,1],3)
round(exp(Pooled$estimates[2:11,1]),2)
round(exp(Pooled$estimates[2:11,1]-1.96*Pooled$estimates[2:11,2]),2)
round(exp(Pooled$estimates[2:11,1]+1.96*Pooled$estimates[2:11,2]),2)

Table2<-cbind(round(Pooled$estimates[2:11,1],3),
      round(exp(Pooled$estimates[2:11,1]),2),
      paste("(",round(exp(Pooled$estimates[2:11,1]-1.96*Pooled$estimates[2:11,2]),2),",",round(exp(Pooled$estimates[2:11,1]+1.96*Pooled$estimates[2:11,2]),2),")")
)
colnames(Table2)<-c("β(k)", "OR", "95% Cr. I.")
Table2
## creation of two new columns with the predicted risk of SMSC individuals and the predicted logit risk
source("SMSCRiskDataset.R")

### plot of the distribution of Risk in SMSC
source("PlotsPrognosticModel.R")
RiskDistribution
RiskPrFactor

########################  APPARENT VALIDATION  & Graphs   ############
source("ApparentValidationPrModel.R")

app_cstat_model
mod_log_2
confint(mod_log_2)

####################################  Calibration Plot  #################################################################
val.prob.ci.2(SMSCdataC$Risk, as.numeric(SMSCdataC$outcome)-1, xlim=c(0,0.8))

########################  Calculation of the EPV and the required sample size based on Riley et al   ############
source("EPVandSampleSizePrModel.R")
EPV

##################################### DECISION CURVE ANALYSIS ##################################################
source("R/dca.fun.R")
SMSCdataC$outcome2<-as.numeric(SMSCdataC$outcome)-1
SMSCdataC$PrognosticModel<-SMSCdataC$Risk
SMSCdataC<-as.data.frame(SMSCdataC)
dca.fun(data=SMSCdataC, outcome="outcome2", predictors="PrognosticModel", xstop = 0.5)

############################# R-shiny app ##################################################################
source('R-shiny.R')
shinyApp(ui = ui, server = server) #also available in https://cinema.ispm.unibe.ch/shinies/rrms/

######################  INTERNAL VALIDATION  ##################

##centralize the variables
source("imp_centralize.R")
#source the function for bootstrap validation
source("R/BootstrapValidation_PrModel.fun.R")
optimism_AUC<-NA
for (i in (1:10)){
  optimism_AUC[i]<-BootstrapValidation_PrModel.fun(imp.list1[[i]], samples=500)
  }
source("R/BootstrapValidation_PrModel2.fun.R")
optimism_c_slope<-NA
for (i in (1:10)){
  optimism_c_slope[i]<-BootstrapValidation_PrModel2.fun(imp.list1[[i]], samples=500)
}
m_optimism_AUC<-mean(optimism_AUC)
m_optimism_c_slope<-mean(optimism_c_slope)

AUC_corrected<-app_cstat_model$auc[1]-m_optimism_AUC
cslope_corrected<-mod_log_2$coefficients[2]-m_optimism_c_slope

##################################### DECISION CURVE ANALYSIS ##################################################
source("R/dca.fun.R")
SMSCdataC$outcome2<-as.numeric(SMSCdataC$outcome)-1
SMSCdataC$PrognosticModel<-SMSCdataC$Risk
dca.fun(data=SMSCdataC, outcome="outcome2", predictors="PrognosticModel", xstop = 0.4)
