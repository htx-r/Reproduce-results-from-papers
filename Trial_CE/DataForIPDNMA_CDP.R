####################################################################
################## Script to add columns arm and meanRisk ############
################## that are needed for the IPD NMR model ################



#arm
data_CDP_CC$arm<-NA
data_CDP_CC$arm[data_CDP_CC$STUDYID==1 & data_CDP_CC$TRT01A==1]<-1
data_CDP_CC$arm[data_CDP_CC$STUDYID==1 & data_CDP_CC$TRT01A==4]<-2
data_CDP_CC$arm[data_CDP_CC$STUDYID==2 & data_CDP_CC$TRT01A==1]<-1
data_CDP_CC$arm[data_CDP_CC$STUDYID==2 & data_CDP_CC$TRT01A==2]<-2
data_CDP_CC$arm[data_CDP_CC$STUDYID==2 & data_CDP_CC$TRT01A==4]<-3
data_CDP_CC$arm[data_CDP_CC$STUDYID==3 & data_CDP_CC$TRT01A==3]<-1
data_CDP_CC$arm[data_CDP_CC$STUDYID==3 & data_CDP_CC$TRT01A==4]<-2

data_CDP_CC$is.censored[(data_CDP_CC$CDP==1)]<-0
data_CDP_CC$is.censored[(data_CDP_CC$CDP==0)]<-1

data_CDP_CC$Censored_Time[is.na(data_CDP_CC$Censored_Time)]<-0
data_CDP_CC_placebo<-data_CDP_CC[data_CDP_CC$TRT01A==4,]

jagsdataIPDNMA_CDP2<- list(
  N=nrow(data_CDP_CC),
  Nstudies=3,
  NPstudies=3,
  NpPlacebo=nrow(data_CDP_CC_placebo),
  nt=4,
  ref=4,
  studyid=as.numeric(factor(data_CDP_CC$STUDYID)),
  t.obs=data_CDP_CC$Time_to_CDP,
  t.cens=data_CDP_CC$Censored_Time,
  is.censored=as.numeric(data_CDP_CC$is.censored),
  treat= rbind(c(1,4,NA),c(1,2,4),c(3,4,NA)),
  na=c(2,3,2),
  arm=data_CDP_CC$arm,
  studyidP=as.numeric(factor(data_CDP_CC_placebo$STUDYID)),
  t.obsP=data_CDP_CC_placebo$Time_to_CDP,
  t.censP=data_CDP_CC_placebo$Censored_Time,
  is.censoredP=as.numeric(data_CDP_CC_placebo$is.censored)

)


