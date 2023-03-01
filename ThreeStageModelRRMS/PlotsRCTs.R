###############################################################################################################
###################################   SCRIPT FOR THE DISTRIBUTION PLOTS OF RISK IN RCTs ######################3
#################################################################################################################
expit<-function(x) {exp(x)/(1+exp(x))}

SMSCdataC$Risknew<-NA
SMSCdataC$Risknew<--1.137-0.025*(SMSCdataC$age-37.04233)+0.237*(SMSCdataC$disease.duration-2.824242)+0.265*(SMSCdataC$edss-2.390698)+0.217*SMSCdataC$nr.Gd.enhanced.lesions-0.049*SMSCdataC$nr.relapses.2y.prior.study1+0.093*SMSCdataC$nr.relapses.2y.prior.study2-0.335*(SMSCdataC$months.since.last.relapse-2.774328)-0.244*SMSCdataC$treatment.naive.prior.visit+0.178*SMSCdataC$gender
SMSCdataC$Risknew<-expit(SMSCdataC$Risknew)

SMSCdataC$Risknew2<-NA
SMSCdataC$Risknew2<--1.137-0.025*(SMSCdataC$age-37.04233)+0.237*(SMSCdataC$disease.duration-2.824242)+0.265*(SMSCdataC$edss-2.390698)+0.217*SMSCdataC$nr.Gd.enhanced.lesions-0.049*SMSCdataC$nr.relapses.2y.prior.study1+0.093*SMSCdataC$nr.relapses.2y.prior.study2-0.335*(SMSCdataC$months.since.last.relapse-2.774328)-0.244*SMSCdataC$treatment.naive.prior.visit+0.178*SMSCdataC$gender
SMSCdataC$Risknew2<-expit(SMSCdataC$Risknew2)

SMSCdataC$Risknew3<-NA
SMSCdataC$Risknew3<--1.137-0.025*(SMSCdataC$age-37.04233)+0.237*(SMSCdataC$disease.duration-2.824242)+0.265*(SMSCdataC$edss-2.390698)+0.217*SMSCdataC$nr.Gd.enhanced.lesions-0.049*SMSCdataC$nr.relapses.2y.prior.study1+0.093*SMSCdataC$nr.relapses.2y.prior.study2-0.335*(SMSCdataC$months.since.last.relapse-2.774328)-0.244*SMSCdataC$treatment.naive.prior.visit+0.178*SMSCdataC$gender
SMSCdataC$Risknew3<-expit(SMSCdataC$Risknew3)
### Distribution in RCTs for those who did relapse
## and for those who did not, seperately
RiskData$outcome<-as.factor(RiskData$outcome)
RiskPrFactor<-ggdensity(RiskData, x = "Risknew4",
          add = "mean", rug = TRUE,
          color = "outcome", fill = "outcome",
          palette = c("yellow", "blue"), xlim=c(0,0.75),xlab = "Risk score as a prognostic factor")
t.test(RiskData$Risknew4[RiskData$outcome==1])
t.test(RiskData$Risknew4[RiskData$outcome==0])

###Distribution of risk score in the entire dataset of RCTs
RiskDistribution<-ggdensity(RiskData, x = "Risknew4",
         fill = "red", color = "red",
         add = "mean", rug = TRUE)

summary(RiskData$Risknew4)
summary((RiskData$Risknew4[RiskData$relapse.2y.after.study==1]))
summary((RiskData$Risknew4[RiskData$relapse.2y.after.study==0]))


RiskPrFactor_SMSC<-ggdensity(SMSCdataC, x = "Risknew",
                        add = "mean", rug = TRUE,
                        color = "outcome", fill = "outcome",
                        palette = c("yellow", "blue"), xlim=c(0,0.75),xlab = "Risk score as a prognostic factor")

RiskDistribution_SMSC<-ggdensity(SMSCdataC, x = "Risknew",
                            fill = "red", color = "red",
                            add = "mean", rug = TRUE)+xlim(0,1)
