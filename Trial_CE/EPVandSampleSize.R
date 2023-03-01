###########################################################################################################################
##############  Function for EPV and recommended sample size by Riley et. al. ####################
#####################################################################################################################################################


#######EPV of candidate variables
dataset<-data_CDP_CC
fullmodel<-lrm(CDP~AGE+SEX+EDSSBL+DIAGYRS+PRMSGR+GDVOLBL+TRELMOS+RLPS2YR,x=TRUE,y=TRUE,data=dataset)
anova(fullmodel)

df<-fullmodel[["stats"]][["d.f."]]
events<- nrow(X[which(dataset$RELAPSE2year==1),])
EPV<-events/df
cat("The EPV is", EPV, fill=TRUE)

mod0<-glm(formula = RELAPSE2year~1, family = binomial(link = "logit"),
          data = dataset)
MaxRcs<-(1-exp(2*as.numeric(logLik(mod0)/nrow(dataset))))
##R2csadj=0.15*0.73, 0.15 is recommended by RIley et al.
R2csadj=0.15*MaxRcs
sample_size<-pmsampsize(type="b", rsquared=R2csadj, shrinkage=0.90, parameters = df, prevalence= events/nrow(dataset))
cat("The needed sample size for the LASSO model is")
print(sample_size$results_table)

