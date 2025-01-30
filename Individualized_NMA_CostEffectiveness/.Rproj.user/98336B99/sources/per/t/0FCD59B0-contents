
############################################################
####  Script creating the data needed for the  ######
### JAGS IPD NMR model for time to progression  ####
############################################################


###Data needed for jags IPD HR model
table(as.numeric(factor(df$TRTA)))
#1: DF, 2:GA, 3:N, 4:Placebo
df$TRTA<-as.numeric(factor(df$TRTA))
#arm
df$arm<-NA
df$arm[df$STUDYID=="109MS301" & df$TRTA==1]<-1
df$arm[df$STUDYID=="109MS301" & df$TRTA==4]<-2
df$arm[df$STUDYID=="109MS302" & df$TRTA==1]<-1
df$arm[df$STUDYID=="109MS302" & df$TRTA==2]<-2
df$arm[df$STUDYID=="109MS302" & df$TRTA==4]<-3
df$arm[df$STUDYID=="C-1801" & df$TRTA==3]<-1
df$arm[df$STUDYID=="C-1801" & df$TRTA==4]<-2

jagsdataIPDNMR_HR<- list(
  N=nrow(df),
  Nstudies=3,
  nt=4,
  ref=4,
  studyid=as.numeric(factor(df$STUDYID)),
  t.obs=df$Time_to_CPD,
  t.cens=df$Censored_Time,
  is.censored=as.numeric(df$is.censored),
  treat= rbind(c(1,4,NA),c(1,2,4),c(3,4,NA)),
  na=c(2,3,2),
  arm=df$arm,
  cov=df$BASE,
  meancov=mean(df$BASE)
)
