############################################################################################################
####################   Script that creates two new columns for the predicted risk in SMSC   ################
###############################################################################################################



SMSCdataC$nr.Gd.enhanced.lesions<-as.numeric(SMSCdataC$nr.Gd.enhanced.lesions)-1
SMSCdataC$gender<-as.numeric(SMSCdataC$gender)-1
SMSCdataC$nr.relapses.2y.prior.study1<-as.numeric(SMSCdataC$nr.relapses.2y.prior.study1)-1
SMSCdataC$nr.relapses.2y.prior.study2<-as.numeric(SMSCdataC$nr.relapses.2y.prior.study2)-1
SMSCdataC$treatment.naive.prior.visit<-as.numeric(SMSCdataC$treatment.naive.prior.visit)-1
SMSCdataC$treatment.during.cycle<-as.numeric(SMSCdataC$treatment.during.cycle)-1

logitp<-NA
for (i in 1:nrow(SMSCdataC)){
  logitp[i]=0.294878+Pooled[["estimates"]][1,1]+Pooled[["estimates"]][2,1]*(SMSCdataC$age[i]-mean(SMSCdataC$age))+Pooled[["estimates"]][3,1]*(SMSCdataC$disease.duration[i]-mean(SMSCdataC$disease.duration))+Pooled[["estimates"]][4,1]*(SMSCdataC$edss[i]-mean(SMSCdataC$edss))+Pooled[["estimates"]][5,1]*SMSCdataC$nr.Gd.enhanced.lesions[i]+Pooled[["estimates"]][6,1]*SMSCdataC$nr.relapses.2y.prior.study1[i]+Pooled[["estimates"]][7,1]*SMSCdataC$nr.relapses.2y.prior.study2[i]+Pooled[["estimates"]][8,1]*(SMSCdataC$months.since.last.relapse[i]-mean(SMSCdataC$months.since.last.relapse))+Pooled[["estimates"]][9,1]*SMSCdataC$treatment.naive.prior.visit[i]+Pooled[["estimates"]][10,1]*SMSCdataC$gender[i]+Pooled[["estimates"]][11,1]*SMSCdataC$treatment.during.cycle[i]
}
SMSCdataC$logitp<-logitp

SMSCdataC$Risk<-exp(SMSCdataC$logitp)/(1+exp(SMSCdataC$logitp))
mean(SMSCdataC$Risk)
mean(as.numeric(SMSCdataC$outcome)-1)
