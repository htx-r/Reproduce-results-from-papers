############################### SAMPLE SIZE CRRITERIA ####################

#######EPV of candidate variables
dataset<-MSrelapse
todrop<-c("STUDYID","USUBJID","TRT01A","RELAPSE1year")
X<-dataset[ , !(names(dataset) %in% todrop)]
X<-na.omit(X)
fullmodel<-lrm(RELAPSE2year~rcs(AGE,4)+rcs(GDVOLBL,4)+SEX+RACE+rcs(HEIGHTBL,4)+rcs(WEIGHTBL,4)+rcs(EDSSBL,4)+rcs(ONSYRS,4)+DOMIHAND+rcs(RLPS1YR,4)+rcs(TRELMOS,4)+MCDBL+PRMSGR+REGION+rcs(T25FWABL,4)+rcs(NHPTMBL,4)+rcs(PASATABL,4)+rcs(VFT25BL,4)+rcs(SFPCSBL,4)+rcs(SFMCSBL,4)+rcs(VASBL,4)+VISUALBL+BRAINBL+PYRAMIBL+SENSORBL+BOWLBLBL+CEREBRBL+DISTWKBL+rcs(T25FWPC,4)+rcs(NHPTMPC,4)+rcs(PASATPC,4),x=TRUE,y=TRUE,data=X)
anova(fullmodel)
fullmodel<-lrm(RELAPSE2year~AGE+SEX+REGION+RACE+GDVOLBL+HEIGHTBL+WEIGHTBL+EDSSBL+ONSYRS+DOMIHAND+RLPS1YR+TRELMOS+MCDBL+PRMSGR+REGION+T25FWABL+NHPTMBL+PASATABL+VFT25BL+SFPCSBL+SFMCSBL+VASBL+VISUALBL+BRAINBL+PYRAMIBL+SENSORBL+BOWLBLBL+CEREBRBL+DISTWKBL+T25FWPC+NHPTMPC+PASATPC,x=TRUE,y=TRUE,data=X)
df<-fullmodel[["stats"]][["d.f."]]
events<- nrow(X[which(X$RELAPSE2year==1),])
EPV<-events/df
cat("The EPV of the LASSO model is", EPV, fill=TRUE)

#### sample size by Riley et al
#null model
mod0 <- lrm(RELAPSE2year~1,x=TRUE,y=TRUE,data=X)
MaxRcs<-(1-exp(2*as.numeric(logLik(mod0)/2000)))
##R2csadj=0.15*0.73, 0.15 is recommended by RIley et al.
R2csadj=0.15*MaxRcs
sample_size<-pmsampsize(type="b", rsquared=R2csadj, shrinkage=0.90, parameters = df, prevalence= 742/2000)
cat("The needed sample size for the LASSO model is")
print(sample_size$results_table)

##### for PreSpecified model:

Fmodel<-lrm(RELAPSE2year~AGE+SEX+RACE+EDSSBL+ONSYRS+RLPS1YR+TRELMOS+PRMSGR+T25FWABL+NHPTMBL+PASATABL+VFT25BL+SFPCSBL+SFMCSBL,x=TRUE,y=TRUE,data=X)
Fdf<-Fmodel[["stats"]][["d.f."]]
Fevents<- nrow(X[which(X$RELAPSE2year==1),])
FEPV<-Fevents/Fdf
cat("The EPV of the PreSpecified model is", FEPV, fill=TRUE)

#### sample size by Riley et al
#null model
mod0 <- lrm(RELAPSE2year~1,x=TRUE,y=TRUE,data=X)
MaxRcs<-(1-exp(2*as.numeric(logLik(mod0)/2000)))
##R2csadj=0.15*0.73, 0.15 is recommended by RIley et al.
R2csadj=0.15*MaxRcs
Fsample_size<-pmsampsize(type="b", rsquared=R2csadj, shrinkage=0.90, parameters = Fdf, prevalence= 742/2000)
cat("The needed sample size for the PreSpecified model is")
print(Fsample_size$results_table)


rm(dataset)
rm(fullmodel)
rm(mod0)
rm(X)
rm(R2csadj)
rm(MaxRcs)
