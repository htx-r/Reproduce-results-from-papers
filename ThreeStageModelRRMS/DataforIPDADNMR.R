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

## 1=DEFINE, 2=CONFIRM, 3=AFFIRM
RiskData$STUDYID<-as.factor(RiskData$STUDYID)
RiskData$TRT01A<-RiskData$Treatment
RiskData$TRT01A[RiskData$TRT01A=="Dimethyl fumarate"]<-1
RiskData$TRT01A[RiskData$TRT01A=="Glatiramer acetate"]<-2
RiskData$TRT01A[RiskData$TRT01A=="Natalizumab"]<-3
RiskData$TRT01A[RiskData$TRT01A=="Placebo"]<-4
#arm
RiskData$arm<-NA
RiskData$arm[RiskData$STUDYID==1 & RiskData$TRT01A==3]<-1
RiskData$arm[RiskData$STUDYID==1 & RiskData$TRT01A==4]<-2
RiskData$arm[RiskData$STUDYID==2 & RiskData$TRT01A==1]<-1
RiskData$arm[RiskData$STUDYID==2 & RiskData$TRT01A==2]<-2
RiskData$arm[RiskData$STUDYID==2 & RiskData$TRT01A==4]<-3
RiskData$arm[RiskData$STUDYID==3 & RiskData$TRT01A==1]<-1
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
  Np_N=sum(as.numeric(table(as.numeric(as.factor(RiskData$STUDYID[RiskData$Treatment=="Natalizumab"]))))),
  studyid=as.numeric(as.factor(RiskData$STUDYID)),
  outcome=as.numeric(RiskData$outcome)-1,
  treat= rbind(c(3,4,NA),c(1,2,4),c(1,4,NA), c(2,4,NA),c(2,4,NA)),
  treatment=as.numeric(RiskData$TRT01A),
  na=c(2,3,2,2,2),
  arm=RiskData$arm,
  outcomeP=PlaceboArms$Relapse2year,
  outcomeP2=as.numeric(SMSCdataC$outcome[SMSCdataC$treatment.during.cycle==0])-1,
  outcomeP3=RiskData$relapse.2y.after.study[RiskData$Treatment=="Placebo"],
  NpPlacebo=nrow(PlaceboArms),
  NpPlacebo2=nrow(SMSCdataC[SMSCdataC$treatment.during.cycle==0,]),
  NpPlacebo3=nrow(RiskData[RiskData$Treatment=="Placebo",]),
  logitRisknew=logitRisknew1,
  logitRisknew2=logitRisknew1,
  logitmeanRisknew=-0.5632 ,
  logitmeanRisknew2=-1.4594,
  Nnew=99,
  ref=4,
  nt=4,
  N.AD.studies=2,
  r=rbind(c(NA,NA,NA,NA),c(NA,NA,NA,NA),c(NA,NA,NA,NA),c(NA,11,NA,19),c(NA,89,NA,97)),
  n=rbind(c(NA,NA,NA,NA),c(NA,NA,NA,NA),c(NA,NA,NA,NA),c(NA,25,NA,25),c(NA,125,NA,126)),
  Risk=RiskData$logitp4,
  Risk_placebo3=RiskData$logitp4[RiskData$Treatment=="Placebo"],
  RiskPlacebo2=SMSCdataC$Risknew[SMSCdataC$treatment.during.cycle==0],
  coef0=as.numeric(coef(fit4)[1]),
  coef1=as.numeric(coef(fit4)[2]),
  coef3=as.numeric(coef(lp3)[1]),
  coef4=as.numeric(coef(lp3)[2]),
  coef5=as.numeric(coef(fit4)[3]),
  coef6=as.numeric(coef(fit4)[4]),
  coef7=as.numeric(coef(fit4)[5]),
  PooledEst=Pooled[["estimates"]],
  meanRisk=c(tapply(RiskData$logitp4, RiskData$STUDYID, summary)$`1`[4],tapply(RiskData$logitp4, RiskData$STUDYID, summary)$`2`[4],tapply(RiskData$logitp4, RiskData$STUDYID, summary)$`3`[4], -0.6093, -0.8213),##here is the mean of logit of risk in AD
  ageAD=as.numeric(c(tapply(RiskData$age, RiskData$STUDYID, summary)$`1`[4],tapply(RiskData$age, RiskData$STUDYID, summary)$`2`[4],tapply(RiskData$age, RiskData$STUDYID, summary)$`3`[4], 30.5, 34.45)),##here is the mean of logit of risk in AD
  disease.durarionAD=as.numeric(c(tapply(RiskData$disease.duration, RiskData$STUDYID, summary)$`1`[4],tapply(RiskData$disease.duration, RiskData$STUDYID, summary)$`2`[4],tapply(RiskData$disease.duration, RiskData$STUDYID, summary)$`3`[4], log(5.65+10), log(6.94+10))),##here is the mean of logit of risk in AD
  edssAD=as.numeric(c(tapply(RiskData$edss, RiskData$STUDYID, summary)$`1`[4],tapply(RiskData$edss, RiskData$STUDYID, summary)$`2`[4],tapply(RiskData$edss, RiskData$STUDYID, summary)$`3`[4], 3.05, 2.6)),##here is the mean of logit of risk in AD
  genderAD=c(0.6990395,0.7007407,0.7713755,0.58,0.733),
  loggenderAD=c(0.8427282,0.8508275,1.2160944,0.3227734,1.0098970),
  nr.Gd.enhanced.lesionsAD=c(0.4909285, 0.4577778,0.3624535, NA,NA),
  lognr.Gd.enhanced.lesionsAD=c(-0.03628998, -0.16929197,-0.56473103,NA,NA),
  treatment.naive.prior.visitAD=c(0.6979723,0.6118519,0.3828996,NA,NA),
  logtreatment.naive.prior.visitAD=c(0.8376607,0.4551033,-0.4772589,NA,NA),
  mean_relapse=c(0.3820704,0.3437037,0.3736059,0.60,0.741),
  logmean_relapse=c(-0.4807697,-0.6468324,-0.5167782,0.4054651,1.051173),
  nr.relapses.2y.prior.study1AD=c(0.1664888,0.2059259,0.2230483,0.082,0.16),
  lognr.relapses.2y.prior.study1AD=c(-1.610719,-1.349660,-1.24799,-2.415478,-1.658228),
  nr.relapses.2y.prior.study2AD=c(0.8303095,0.7703704,0.7527881,0.897,0.785),
  lognr.relapses.2y.prior.study2AD=c(1.587822,1.210404,1.113538,2.164327,1.295046),
  months.since.last.relapseAD=as.numeric(c(tapply(RiskData$months.since.last.relapse, RiskData$STUDYID, summary)$`1`[4],tapply(RiskData$months.since.last.relapse, RiskData$STUDYID, summary)$`2`[4],tapply(RiskData$months.since.last.relapse, RiskData$STUDYID, summary)$`3`[4], NA, NA)),##here is the mean of logit of risk in AD
  meanAge=as.numeric(mean(RiskData$age)),
  meanDiseaseDuration=as.numeric(mean(RiskData$disease.duration)),
  meanEdss=as.numeric(mean(RiskData$edss)),
  meanMonths.since.last.relapse=as.numeric(mean(RiskData$months.since.last.relapse))
)


jagsdataIPDADNMR_model2 <- list(
  N.IPD.studies=3,
  Np=sum(as.numeric(table(as.numeric(as.factor(RiskData$STUDYID))))),
  studyid=as.numeric(as.factor(RiskData$STUDYID)),
  outcome=as.numeric(RiskData$outcome)-1,
  treat= rbind(c(3,4,NA),c(1,2,4),c(1,4,NA), c(2,4,NA),c(2,4,NA)),
  na=c(2,3,2,2,2),
  arm=RiskData$arm,
  RiskPlacebo2=SMSCdataC$Risknew[SMSCdataC$treatment.during.cycle==0],
  outcomeP=PlaceboArms$Relapse2year,
  NpPlacebo=nrow(PlaceboArms),
  NpPlacebo2=nrow(SMSCdataC[SMSCdataC$treatment.during.cycle==0,]),
  logitRisknew=logitRisknew1,
  logitRisknew2=logitRisknew1,
  logitmeanRisknew=-0.5632 ,
  logitmeanRisknew2=-1.4594,
  Nnew=99,
  ref=4,
  nt=4,
  N.AD.studies=2,
  r=rbind(c(NA,NA,NA,NA),c(NA,NA,NA,NA),c(NA,NA,NA,NA),c(NA,11,NA,19),c(NA,89,NA,97)),
  n=rbind(c(NA,NA,NA,NA),c(NA,NA,NA,NA),c(NA,NA,NA,NA),c(NA,25,NA,25),c(NA,125,NA,126)),
  Risk=RiskData$logitp2,
  Risk_placebo3=RiskData$logitp2[RiskData$Treatment=="Placebo"],
  NpPlacebo3=nrow(RiskData[RiskData$Treatment=="Placebo",]),
  outcomeP2=as.numeric(SMSCdataC$outcome[SMSCdataC$treatment.during.cycle==0])-1,
  outcomeP3=RiskData$relapse.2y.after.study[RiskData$Treatment=="Placebo"],
  treatment=as.numeric(RiskData$TRT01A),
  coef0=as.numeric(coef(fit4)[1]),
  coef1=as.numeric(coef(fit4)[2]),
  coef3=as.numeric(coef(lp3)[1]),
  coef4=as.numeric(coef(lp3)[2]),
  coef5=as.numeric(coef(fit4)[3]),
  coef6=as.numeric(coef(fit4)[4]),
  coef7=as.numeric(coef(fit4)[5]),
  PooledEst=Pooled[["estimates"]],
  meanRisk=c(tapply(RiskData$logitp4, RiskData$STUDYID, summary)$`1`[4],tapply(RiskData$logitp4, RiskData$STUDYID, summary)$`2`[4],tapply(RiskData$logitp4, RiskData$STUDYID, summary)$`3`[4], -0.6093, -0.8213),##here is the mean of logit of risk in AD
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

jagsdataIPDADNMR_model3 <- list(
  N.IPD.studies=3,
  RiskPlacebo2=SMSCdataC$Risknew[SMSCdataC$treatment.during.cycle==0],
  Np=sum(as.numeric(table(as.numeric(as.factor(RiskData$STUDYID))))),
  studyid=as.numeric(as.factor(RiskData$STUDYID)),
  outcome=as.numeric(RiskData$outcome)-1,
  NpPlacebo2=nrow(SMSCdataC[SMSCdataC$treatment.during.cycle==0,]),
   treat= rbind(c(3,4,NA),c(1,2,4),c(1,4,NA), c(2,4,NA),c(2,4,NA)),
  na=c(2,3,2,2,2),
  arm=RiskData$arm,
  outcomeP=PlaceboArms$Relapse2year,
  outcomeP2=as.numeric(SMSCdataC$outcome[SMSCdataC$treatment.during.cycle==0])-1,
  outcomeP3=RiskData$relapse.2y.after.study[RiskData$Treatment=="Placebo"],
   NpPlacebo=nrow(PlaceboArms),
  logitRisknew=logitRisknew1,
  logitRisknew2=logitRisknew1,
  Risk=RiskData$logitp3,
  Risk_placebo3=RiskData$logitp3[RiskData$Treatment=="Placebo"],
  NpPlacebo3=nrow(RiskData[RiskData$Treatment=="Placebo",]),
  treatment=as.numeric(RiskData$TRT01A),
  logitmeanRisknew=-0.5632 ,
  logitmeanRisknew2=-1.4594,
  Nnew=99,
  ref=4,
  nt=4,
  N.AD.studies=2,
  r=rbind(c(NA,NA,NA,NA),c(NA,NA,NA,NA),c(NA,NA,NA,NA),c(NA,11,NA,19),c(NA,89,NA,97)),
  n=rbind(c(NA,NA,NA,NA),c(NA,NA,NA,NA),c(NA,NA,NA,NA),c(NA,25,NA,25),c(NA,125,NA,126)),
  coef0=as.numeric(coef(fit4)[1]),
  coef1=as.numeric(coef(fit4)[2]),
  coef3=as.numeric(coef(lp3)[1]),
  coef4=as.numeric(coef(lp3)[2]),
  coef5=as.numeric(coef(fit4)[3]),
  coef6=as.numeric(coef(fit4)[4]),
  coef7=as.numeric(coef(fit4)[5]),
  PooledEst=Pooled[["estimates"]],
  meanRisk=c(tapply(RiskData$logitp4, RiskData$STUDYID, summary)$`1`[4],tapply(RiskData$logitp4, RiskData$STUDYID, summary)$`2`[4],tapply(RiskData$logitp4, RiskData$STUDYID, summary)$`3`[4], -0.6093, -0.8213),##here is the mean of logit of risk in AD
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




jagsdataIPDNMR <- list(
  Nstudies=3,
  Np=sum(as.numeric(table(as.numeric(as.factor(RiskData$STUDYID))))),
  studyid=as.numeric(as.factor(RiskData$STUDYID)),
  outcome=as.numeric(RiskData$outcome)-1,
  treat= rbind(c(3,4,NA),c(1,2,4),c(1,4,NA)),
  treatment=as.numeric(RiskData$TRT01A),
  na=c(2,3,2),
  arm=RiskData$arm,
  outcomeP=PlaceboArms$Relapse2year,
  NpPlacebo=nrow(RiskData[RiskData$Treatment=="Placebo",]),
  logitRisknew=logitRisknew1,
  logitmeanRisknew=-0.5632 ,
  logitmeanRisknew2=-1.4594,
  Nnew=99,
  ref=4,
  nt=4,
  Risk=RiskData$logitp4,
  Risk_placebo=RiskData$logitp4[RiskData$Treatment=="Placebo"],
  meanRisk=c(tapply(RiskData$logitp4, RiskData$STUDYID, summary)$`1`[4],tapply(RiskData$logitp4, RiskData$STUDYID, summary)$`2`[4],tapply(RiskData$logitp4, RiskData$STUDYID, summary)$`3`[4])##here is the mean of logit of risk in AD
  )
