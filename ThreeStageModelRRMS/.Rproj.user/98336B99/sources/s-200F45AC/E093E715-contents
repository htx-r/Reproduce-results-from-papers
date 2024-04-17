##################################  Script for using the RCTs with exactly the same variables  ############
############################################## as in SMSC dataset ###########################################

RCTs<-RCTs[RCTs$STUDYID!="MSCRG" ,]

#keep only the variables needed for the prognostic model
tokeep<-c("STUDYID" ,  "USUBJID", "TRT01A", "AGE" , "SEX" , "EDSSBL" ,  "ONSYRS" , "PRMSGR", "TRELMOS", "RLPS1YR","RLPS3YR",
          "GDLESBL", "RELAPSE2year")
RCTs<- RCTs[,(names(RCTs) %in% tokeep)]
RCTs<-na.omit(RCTs)

# make the sex same as in SMSC dataset 0 for men 1 for women
RCTs$SEX<-as.numeric(as.factor(RCTs$SEX))
RCTs$SEX[which(RCTs$SEX==2)]<-0
RCTs$SEX<-as.factor(RCTs$SEX)

#SOS PRMSGR opposite to have treatment naive as in SMSC datase
RCTs$PRMSGR<-RCTs$PRMSGR+1
RCTs$PRMSGR[which(RCTs$PRMSGR==2)]<-0

RCTs$RLPS2YR<-(RCTs$RLPS1YR+RCTs$RLPS3YR)/2

todrop<-c("RLPS1YR","RLPS3YR")
RCTs<- RCTs[,!(names(RCTs) %in% todrop)]


#rename the variables as in SMSC dataset

names(RCTs)<-c("STUDYID",  "USUBJID", "age", "gender","Treatment", "edss", "disease.duration","months.since.last.relapse",
               "treatment.naive.prior.visit","nr.Gd.enhanced.lesions", "relapse.2y.after.study", "nr.relapses.2y.prior.study")

# do the tranformations in variables as in SMSC dataset
# disease duration in transformed to log(disease duration +10)
RCTs$disease.duration<-log(RCTs$disease.duration+10)
# months since last relapse relapse is transformed to log(months since last relapse +10)
RCTs$months.since.last.relapse<-log(RCTs$months.since.last.relapse+10)

######## Needed transformations for CATEGORICAL variables #####################

# GD enhanced lessions, instead of 0, 1, ..., 18 now we will have 0 and >0
RCTs$nr.Gd.enhanced.lesions[which(RCTs$nr.Gd.enhanced.lesions>0)]<-1
RCTs$nr.Gd.enhanced.lesions<-as.factor(RCTs$nr.Gd.enhanced.lesions)

# Number of relapses 2 years prior to study, instead of 0, 1, ..., 6 now we will have 0 and 1 and >1
RCTs$nr.relapses.2y.prior.study[which(RCTs$nr.relapses.2y.prior.study>1)]<-2
RCTs$nr.relapses.2y.prior.study[which(RCTs$nr.relapses.2y.prior.study<1)]<-0
RCTs$nr.relapses.2y.prior.study<-as.factor(RCTs$nr.relapses.2y.prior.study)

#outcome as numeric
RCTs$outcome<-RCTs$relapse.2y.after.study
RCTs$outcome<-as.factor(RCTs$outcome)

#make exactly the same classes
RCTs$age<-as.numeric(RCTs$age)
RCTs$treatment.naive.prior.visit<-as.factor(RCTs$treatment.naive.prior.visit)

#dummy variables for nr.relapses.2y.prior.study as in SMSC dataset
RCTs$nr.relapses.2y.prior.study1<-RCTs$nr.relapses.2y.prior.study
RCTs$nr.relapses.2y.prior.study2<-RCTs$nr.relapses.2y.prior.study
RCTs$nr.relapses.2y.prior.study1[which(RCTs$nr.relapses.2y.prior.study1==2)]<-0
RCTs$nr.relapses.2y.prior.study2[which(RCTs$nr.relapses.2y.prior.study2==1)]<-0
RCTs$nr.relapses.2y.prior.study2[which(RCTs$nr.relapses.2y.prior.study2==2)]<-1

####### NEW variable - Risk of relapse in two years for each one of the patients

RCTs$nr.Gd.enhanced.lesions<-as.numeric(RCTs$nr.Gd.enhanced.lesions)-1
RCTs$nr.relapses.2y.prior.study1<-as.numeric(RCTs$nr.relapses.2y.prior.study1)-1
RCTs$nr.relapses.2y.prior.study2<-as.numeric(RCTs$nr.relapses.2y.prior.study2)-1
RCTs$treatment.naive.prior.visit<-as.numeric(RCTs$treatment.naive.prior.visit)-1
RCTs$gender<-as.numeric(RCTs$gender)-1
##RCTs$TreatmentNow<-NA
#RCTS$TreatmentNow[which(RCTS$Treatment==)]


logitp<-NA
for (i in 1:nrow(RCTs)){
logitp[i]=0.294872 + Pooled[["estimates"]][1,1]+Pooled[["estimates"]][2,1]*(RCTs$age[i]-mean(RCTs$age))+Pooled[["estimates"]][3,1]*(RCTs$disease.duration[i]-mean(RCTs$disease.duration))+Pooled[["estimates"]][4,1]*(RCTs$edss[i]-mean(RCTs$edss))+Pooled[["estimates"]][5,1]*RCTs$nr.Gd.enhanced.lesions[i]+Pooled[["estimates"]][6,1]*RCTs$nr.relapses.2y.prior.study1[i]+Pooled[["estimates"]][7,1]*RCTs$nr.relapses.2y.prior.study2[i]+Pooled[["estimates"]][8,1]*(RCTs$months.since.last.relapse[i]-mean(RCTs$months.since.last.relapse))+Pooled[["estimates"]][9,1]*RCTs$treatment.naive.prior.visit[i]+Pooled[["estimates"]][10,1]*RCTs$gender[i]
}
RCTs$logitp<-logitp

RCTs$Risk<-exp(RCTs$logitp)/(1+exp(RCTs$logitp))
mean(RCTs$Risk)
mean(RCTs$relapse.2y.after.study)

