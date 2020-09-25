###################################################################################################################
####################  Script that checks the result of Frequentist Generalized Mixed-Effects Model ###########
###################################################################################################################



#also categorical variable were not included as random effects, as the glmer did no run with an error

SMSCdata_glmer<-SMSCdataC
SMSCdata_glmer$nr.Gd.enhanced.lesions<-as.numeric(SMSCdata_glmer$nr.Gd.enhanced.lesions)-1
SMSCdata_glmer$treatment.naive.prior.visit<-as.numeric(SMSCdata_glmer$treatment.naive.prior.visit)-1
SMSCdata_glmer$gender<-as.numeric(as.factor(SMSCdata_glmer$gender))-1
SMSCdata_glmer$nr.relapses.2y.prior.study<-as.numeric(SMSCdata_glmer$nr.relapses.2y.prior.study)-1
SMSCdata_glmer$age<-(SMSCdata_glmer$age-mean(SMSCdata_glmer$age))
SMSCdata_glmer$disease.duration<-(SMSCdata_glmer$disease.duration-mean(SMSCdata_glmer$disease.duration))
SMSCdata_glmer$edss<-(SMSCdata_glmer$edss-mean(SMSCdata_glmer$edss))
SMSCdata_glmer$months.since.last.relapse<-(SMSCdata_glmer$months.since.last.relapse-mean(SMSCdata_glmer$months.since.last.relapse))
SMSCdata_glmer$treatment.during.cycle<-as.numeric(SMSCdata_glmer$treatment.during.cycle)

#dummy variables for nr.relapses.2y.prior.study
SMSCdata_glmer$nr.relapses.2y.prior.study1<-SMSCdata_glmer$nr.relapses.2y.prior.study
SMSCdata_glmer$nr.relapses.2y.prior.study2<-SMSCdata_glmer$nr.relapses.2y.prior.study
SMSCdata_glmer$nr.relapses.2y.prior.study1[which(SMSCdata_glmer$nr.relapses.2y.prior.study1==2)]<-0
SMSCdata_glmer$nr.relapses.2y.prior.study2[which(SMSCdata_glmer$nr.relapses.2y.prior.study2==1)]<-0
SMSCdata_glmer$nr.relapses.2y.prior.study2[which(SMSCdata_glmer$nr.relapses.2y.prior.study2==2)]<-1

## check with mixed effects model
 glmer_out<-glmer(outcome ~ 1+(1|patient.id) + age + disease.duration + edss + nr.Gd.enhanced.lesions + nr.relapses.2y.prior.study1+ nr.relapses.2y.prior.study2+
              months.since.last.relapse + treatment.naive.prior.visit + gender + treatment.during.cycle +
              (-1+age|patient.id)+(-1+disease.duration|patient.id)+(-1+edss|patient.id)+(-1+nr.Gd.enhanced.lesions|patient.id)+
                + (-1+nr.relapses.2y.prior.study1|patient.id)+ (-1+nr.relapses.2y.prior.study2|patient.id)+(-1+months.since.last.relapse|patient.id) + (-1+treatment.naive.prior.visit|patient.id)
                + (-1+gender|patient.id)  + (-1+treatment.during.cycle|patient.id),
                data=SMSCdata_glmer, family = binomial, nAGQ = 0)

## check with a generalized linear model (no mixed effects)
  glm_out<-glm((outcome)~ age + disease.duration + edss + nr.Gd.enhanced.lesions + nr.relapses.2y.prior.study1 + nr.relapses.2y.prior.study2 +
                 months.since.last.relapse + treatment.naive.prior.visit + gender + treatment.during.cycle,
              family = "binomial",data=SMSCdata_glmer)


