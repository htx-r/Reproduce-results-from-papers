#################################################################################################################
#######################################  Script that checks about auxiliary variables  ############################################
#################################################################################################################



#check the outcome with all auxiliary variables (there is enough power)

glm_out_MI<-glm((outcome)~ nr.Gd.enhanced.lesions + age + edss + kfs.1 + kfs.2 + kfs.3 + kfs.4 + kfs.5.converted + kfs.6.converted + kfs.7 +
               dominant.hand + lesion_volume_ml_3mmfilter + disease.duration + nr.relapses.2y.prior.study +
               months.since.last.relapse + treatment.naive.prior.visit + ave.nine.hole.peg.test + gender + treatment.during.cycle,
             family = "binomial",data=MISMSCdata)
summary(glm_out_MI)#only kfs.2

#check for the missing covariate
#not enough power, so we check some of them in each one of the following 4 models
#############################################################################################################


glm_out_MI1<-glm((nr.Gd.enhanced.lesions)~ outcome + age + edss  + disease.duration + nr.relapses.2y.prior.study +
                months.since.last.relapse + treatment.naive.prior.visit +  gender + treatment.during.cycle + kfs.1+ kfs.2+ kfs.3+kfs.4,
              family = "binomial",data=MISMSCdata)
summary(glm_out_MI1)#only kfs.2



glm_out_MI2<-glm((nr.Gd.enhanced.lesions)~ outcome + age + edss  + disease.duration + nr.relapses.2y.prior.study +
                months.since.last.relapse + treatment.naive.prior.visit +  gender + treatment.during.cycle+ kfs.5.converted,
              family = "binomial",data=MISMSCdata)
summary(glm_out_MI2) #no

glm_out_MI3<-glm((nr.Gd.enhanced.lesions)~ outcome + age + edss  + disease.duration + nr.relapses.2y.prior.study +
                months.since.last.relapse + treatment.naive.prior.visit +  gender  + treatment.during.cycle  + kfs.6.converted +kfs.7,
              family = "binomial",data=MISMSCdata)
summary(glm_out_MI3)##no

glm_out_MI4<-glm((nr.Gd.enhanced.lesions)~ outcome + age + edss  + disease.duration + nr.relapses.2y.prior.study +
                months.since.last.relapse + treatment.naive.prior.visit +  gender + treatment.during.cycle  + dominant.hand + lesion_volume_ml_3mmfilter+ ave.nine.hole.peg.test ,
              family = "binomial",data=MISMSCdata)
summary(glm_out_MI4)#no

######### auxiliary variable is ksf.2

#keep only the auxiliary variable that is needed and drom all the other variables
todrop<-c("kfs.1","kfs.2","kfs.3","kfs.4","kfs.5.converted","kfs.6.converted","kfs.7", "dominant.hand")
MISMSCdata <- MISMSCdata[which(!is.na(MISMSCdata$kfs.2)),]



