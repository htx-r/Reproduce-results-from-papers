###############################################################################
########## FUNCTION FOR developing the logistic model with shrinkage       #########################
########################################################################## #######


#drop STUDYID, USIBJID (no needed for the matrix), drop TRT01A (blinded to treatment)

todrop<-c("STUDYID","USUBJID","TRT01A")
dataset<-data_CDP_CC
X<-dataset[ , !(names(dataset) %in% todrop)]
X<-na.omit(X)

finalmodel<-lrm(RELAPSE2year~AGE+SEX+EDSSBL+DIAGYRS+PRMSGR+GDVOLBL+TRELMOS+RLPS2YR,x=TRUE,y=TRUE,linear.predictors = TRUE,data=X)
finalmodel



### Method of shrinkage. Make a PMLE penalized model based on minimizing a modified AIC criterion
# Make a penalized model
set.seed(1)
penalized	<-  pentrace(finalmodel, seq(0,200,0.1))
finalmodel.pen <- update (finalmodel, penalty=penalized$penalty)



cat("PreSpecified . model is:")
print(finalmodel.pen)

