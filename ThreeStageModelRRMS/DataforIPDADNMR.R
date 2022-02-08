######################################################################################
### Script: Data needed for the IPD & AD NMR jags model ###################
###############################################################################


#### 1. LOAD the Aggregated data
#MS <- read_excel("C:/Users/kc19o338/Desktop/old profile/Desktop/RealWorldPredictionModel/NMAPredictionsRiskModel/MS.xlsx")
#MS <- as.data.frame(MS)
### keep studied of interest
#ADdata<-MS[which(MS$study=="Bornstein" | MS$study=="Johnson"),]
###study-level covariates you know

#A
######## relapse rate two years prior study
############# we want the patients with 0, 1, more than 2 relapses

#Bornstein study
#dpois(x=0, lambda=3.85)
#dpois(x=1, lambda=3.85)
#1-(dpois(x=0, lambda=3.85)+dpois(x=1, lambda=3.85))
#Johnson study
#dpois(x=0, lambda=2.9)
#dpois(x=1, lambda=2.9)
#1-(dpois(x=0, lambda=2.9)+dpois(x=1, lambda=2.9))


#recode of studyid
RiskData$STUDYID<-as.numeric(as.factor(RiskData$STUDYID))
RiskData$STUDYID[which(RiskData$STUDYID==1)]<-4
RiskData$STUDYID[which(RiskData$STUDYID==3)]<-1
RiskData$STUDYID[which(RiskData$STUDYID==4)]<-3
## 1=DEFINE, 2=CONFIRM, 3=AFFIRM
RiskData$STUDYID<-as.factor(RiskData$STUDYID)
RiskData$TRT01A<-RiskData$Treatment
#arm
RiskData$arm<-NA
RiskData$arm[RiskData$STUDYID==1 & RiskData$TRT01A==1]<-1
RiskData$arm[RiskData$STUDYID==1 & RiskData$TRT01A==4]<-2
RiskData$arm[RiskData$STUDYID==2 & RiskData$TRT01A==1]<-1
RiskData$arm[RiskData$STUDYID==2 & RiskData$TRT01A==2]<-2
RiskData$arm[RiskData$STUDYID==2 & RiskData$TRT01A==4]<-3
RiskData$arm[RiskData$STUDYID==3 & RiskData$TRT01A==3]<-1
RiskData$arm[RiskData$STUDYID==3 & RiskData$TRT01A==4]<-2

##new Risk & logit Risk
Risknew1<-seq(0.01,0.99,0.01)
Risknew1<-as.data.frame(Risknew1)
logit<-function(x) {log(x/(1-x))}
expit<-function(x) {exp(x)/(1+exp(x))}
logitRisknew1<-NA
logitRisknew1<-as.data.frame(logitRisknew1)
for (i in 1:99) {
  logitRisknew1[i,1]<-logit(Risknew1[i,1])
}
logitRisknew1<-as.data.frame(logitRisknew1)
logitmeanRisknew1<-mean(logitRisknew1[,1])

jagsdataIPDADNMR <- list(
  N.IPD.studies=3,
  Np=sum(as.numeric(table(as.numeric(as.factor(RiskData$STUDYID))))),
  studyid=as.numeric(as.factor(RiskData$STUDYID)),
  outcome=as.numeric(RiskData$outcome)-1,
  treat= rbind(c(1,4,NA),c(1,2,4),c(3,4,NA), c(2,4,NA),c(2,4,NA)),
  na=c(2,3,2,2,2),
  arm=RiskData$arm,
  outcomeP=PlaceboArms$Relapse2year,
  NpPlacebo=nrow(PlaceboArms),
  logitRisknew=logitRisknew1,
  logitmeanRisknew=-0.5632 ,
  Nnew=99,
  ref=4,
  nt=4,
  N.AD.studies=2,
  r=rbind(c(NA,NA,NA,NA),c(NA,NA,NA,NA),c(NA,NA,NA,NA),c(NA,11,NA,19),c(NA,89,NA,97)),
  n=rbind(c(NA,NA,NA,NA),c(NA,NA,NA,NA),c(NA,NA,NA,NA),c(NA,25,NA,25),c(NA,125,NA,126)),
  Risk=RiskData$logitp4,
  coef0=as.numeric(coef(fit4)[1]),
  coef1=as.numeric(coef(fit4)[2]),
  coef3=as.numeric(coef(lp3)[1]),
  coef4=as.numeric(coef(lp3)[2]),
  coef5=as.numeric(coef(fit4)[3]),
  coef6=as.numeric(coef(fit4)[4]),
  coef7=as.numeric(coef(fit4)[5]),
  PooledEst=Pooled[["estimates"]],
  meanRisk=c(tapply(RiskData$logitp4, RiskData$STUDYID, summary)$`1`[4],tapply(RiskData$logitp4, RiskData$STUDYID, summary)$`2`[4],tapply(RiskData$logitp4, RiskData$STUDYID, summary)$`3`[4], NA, NA),##here is the mean of logit of risk in AD
  ageAD=as.numeric(c(tapply(RiskData$age, RiskData$STUDYID, summary)$`1`[4],tapply(RiskData$age, RiskData$STUDYID, summary)$`2`[4],tapply(RiskData$age, RiskData$STUDYID, summary)$`3`[4], 30.5, 34.45)),##here is the mean of logit of risk in AD
  disease.durationAD=as.numeric(c(tapply(RiskData$disease.duration, RiskData$STUDYID, summary)$`1`[4],tapply(RiskData$disease.duration, RiskData$STUDYID, summary)$`2`[4],tapply(RiskData$disease.duration, RiskData$STUDYID, summary)$`3`[4], 5.65, 6.94)),##here is the mean of logit of risk in AD
  edssAD=as.numeric(c(tapply(RiskData$edss, RiskData$STUDYID, summary)$`1`[4],tapply(RiskData$edss, RiskData$STUDYID, summary)$`2`[4],tapply(RiskData$edss, RiskData$STUDYID, summary)$`3`[4], 3.05, 2.6)),##here is the mean of logit of risk in AD
  genderAD=c(0.7713755,0.7007407,0.6990395,0.58,0.733),
  loggenderAD=c(1.2160944,0.8508275,0.8427282,0.3227734,1.0098970),
  nr.Gd.enhanced.lesionsAD=c(0.3624535, 0.4577778, 0.4909285 ,NA,NA),
  lognr.Gd.enhanced.lesionsAD=c(-0.56473103, -0.16929197,-0.03628998,NA,NA),
  treatment.naive.prior.visitAD=c(0.3828996,0.6118519,0.6979723,NA,NA),
  logtreatment.naive.prior.visitAD=c(-0.4772589,0.4551033,0.8376607,NA,NA),
  nr.relapses.2y.prior.study1AD=c(0.2240483,0.2059259,0.1664888,0.082,0.16),
  lognr.relapses.2y.prior.study1AD=c(-1.242229,-1.349660,-1.610719,-2.415478,-1.658228),
  nr.relapses.2y.prior.study2AD=c(0.7527881,0.7703704,0.8303095,0.897,0.785),
  lognr.relapses.2y.prior.study2AD=c(1.113538,1.210404,1.587822,2.164327,1.295046),
  months.since.last.relapseAD=as.numeric(c(tapply(RiskData$months.since.last.relapse, RiskData$STUDYID, summary)$`1`[4],tapply(RiskData$months.since.last.relapse, RiskData$STUDYID, summary)$`2`[4],tapply(RiskData$months.since.last.relapse, RiskData$STUDYID, summary)$`3`[4], NA, NA)),##here is the mean of logit of risk in AD
  meanAge=as.numeric(mean(RiskData$age)),
  meanDiseaseDuration=as.numeric(mean(RiskData$disease.duration)),
  meanEdss=as.numeric(mean(RiskData$edss)),
  meanMonths.since.last.relapse=as.numeric(mean(RiskData$months.since.last.relapse))
)
