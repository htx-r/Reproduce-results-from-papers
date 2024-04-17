#Risk estimation for observational data

mydatapath="C:/Users/kc19o338/Desktop/SMSC"

########################## DATASET NEEDED FOR THE PRE-SPECIFIED PROGNOSTIC MODEL ##############################
# function that keeps only the needed variables (selected via pre-existing prognostic models on the literature, for RRMS patients)
# and makses the proper transformations for continues and categorical variables
# (in case you want to see all summary statistics for the SMSC data before and after the transformations you can run the SMSC_Summary.R script)
source("FinalDataSMSC.R")
SMSCdata<-FinalDataSMSC.fun(mydatapath)
# the dataset with complete cases only, no missing values at all
SMSCdataC<-na.omit(SMSCdata)

SMSCdataC$gender<-as.numeric(SMSCdataC$gender)
SMSCdataC$gender[SMSCdataC$gender==2]<-0

table(SMSCdataC$treatment.naive.prior.visit)
SMSCdataC$treatment.naive.prior.visit<-as.numeric(SMSCdataC$treatment.naive.prior.visit)
SMSCdataC$treatment.naive.prior.visit[SMSCdataC$treatment.naive.prior.visit==2]<-0

summary(SMSCdataC$months.since.last.relapse)
SMSCdataC$months.since.last.relapse<-exp(SMSCdataC$months.since.last.relapse)-10
SMSCdataC$months.since.last.relapse<-log(SMSCdataC$months.since.last.relapse+1)

table(as.numeric(SMSCdataC$nr.Gd.enhanced.lesions))
SMSCdataC$nr.Gd.enhanced.lesions<-as.numeric(SMSCdataC$nr.Gd.enhanced.lesions)
SMSCdataC$nr.Gd.enhanced.lesions[SMSCdataC$nr.Gd.enhanced.lesions==2]<-0

class((SMSCdataC$treatment.naive.prior.visit))
SMSCdataC$nr.Gd.enhanced.lesions<-as.numeric(SMSCdataC$nr.Gd.enhanced.lesions)
SMSCdataC$nr.Gd.enhanced.lesions[SMSCdataC$nr.Gd.enhanced.lesions==2]<-0

table((SMSCdataC$nr.relapses.2y.prior.study))
SMSCdataC$nr.relapses.2y.prior.study<-as.numeric(SMSCdataC$nr.relapses.2y.prior.study)-1


SMSCdataC$Risk_SMSC<-finalmodel.pen$coefficients[1]+finalmodel.pen$coefficients[2]*SMSCdataC$age+finalmodel.pen$coefficients[3]*SMSCdataC$gender+finalmodel.pen$coefficients[4]*SMSCdataC$edss+finalmodel.pen$coefficients[5]*(exp(SMSCdataC$disease.duration)-10)+
  finalmodel.pen$coefficients[6]*SMSCdataC$treatment.naive.prior.visit+finalmodel.pen$coefficients[7]*SMSCdataC$nr.Gd.enhanced.lesions+
  finalmodel.pen$coefficients[8]*SMSCdataC$months.since.last.relapse+ finalmodel.pen$coefficients[9]*SMSCdataC$nr.relapses.2y.prior.study

SMSCdataC$Risk_SMSC<-expit(SMSCdataC$Risk_SMSC)
 #risk
summary(SMSCdataC$Risk_SMSC)
RiskDist<-ggdensity(SMSCdataC, x = "Risk_SMSC",
                    fill = "#0073C2FF", color = "#0073C2FF",
                    add = "mean", rug = TRUE, xlim=c(0,1))

RiskDist



