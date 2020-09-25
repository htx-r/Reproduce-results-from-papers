#######################################################################################################################
#######################################   Script to check if restricted cubic splines are needed  ######################3
##################################### If yes, also make the proper transformations for the model & run it  ########
#######################################################################################################################


SMSCdata_glmer$nr.Gd.enhanced.lesions<-as.factor(SMSCdata_glmer$nr.Gd.enhanced.lesions)
SMSCdata_glmer$nr.relapses.2y.prior.study<-as.factor(SMSCdata_glmer$nr.relapses.2y.prior.study)
SMSCdata_glmer$treatment.naive.prior.visit<-as.factor(SMSCdata_glmer$treatment.naive.prior.visit)
SMSCdata_glmer$gender<-as.factor(SMSCdata_glmer$gender)
SMSCdata_glmer$treatment.during.cycle<-as.factor(SMSCdata_glmer$treatment.during.cycle)
SMSCdata_glmer$treatment.time.during.cycle.monthsFactor<-as.factor(SMSCdata_glmer$treatment.time.during.cycle.monthsFactor)
SMSCdata_glmer$outcome<-as.factor(SMSCdata_glmer$outcome)

glm_rcs<-lrm((outcome)~ rcs(age,4) + rcs(disease.duration,4) + rcs(edss,4) + nr.Gd.enhanced.lesions + nr.relapses.2y.prior.study +
               rcs(months.since.last.relapse,4) + treatment.naive.prior.visit + gender + treatment.during.cycle,
             x=TRUE,y=TRUE,data=SMSCdata_glmer,maxit=1000)
anova(glm_rcs) #none of the variables has non-linear relationship with the outcome


