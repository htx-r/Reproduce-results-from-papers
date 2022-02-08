###############################################################################################################
############################## Script that calculates the EPV of the model and ##################################
############################# the required sample size based on Riley's et al. method #####################
#############################################################################################################


df<-22
events<-nrow(SMSCdata[which(SMSCdata$outcome==1),])
EPV<-events/df

#### sample size by Riley et al
#null model
mod0 <- lrm(outcome~1,x=TRUE,y=TRUE,data=SMSCdata)
MaxRcs<-(1-exp(2*as.numeric(logLik(mod0)/1752)))
##R2csadj=0.15*0.73, 0.15 is recommended by RIley et al.
R2csadj=0.15*MaxRcs
sample_size<-pmsampsize(type="b", rsquared=R2csadj,  parameters = df, prevalence= events/1752)
cat("The needed sample size  is")
print(sample_size$results_table)
