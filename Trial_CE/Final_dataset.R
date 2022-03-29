###########################################################################################################################
##############  function that selects variables of interest and ####################
##############         recodes some of factors        #############################
#####################################################################################################################################################


tokeep<-c("USUBJID","STUDYID","TRT01A","AGE","SEX","EDSSBL","DIAGYRS", "GDVOLBL","RLPS3YR","RLPS1YR",
          "PRMSGR","TRELMOS", "CDP","RELAPSE2year")
data_CDP_final <- data_CDP[,(names(data_CDP) %in% tokeep)]



####recoding variables
data_CDP_final$SEX<-recode(data_CDP_final$SEX, "'M'=1; 'F'=0")
#prior treatments as factor
data_CDP_final$PRMSGR<-as.factor(data_CDP_final$PRMSGR)


##make Gd lesions 0 vs >0 and factor
data_CDP_final$GDVOLBL[which(data_CDP_final$GDVOLBL>0)]<-1
data_CDP_final$GDVOLBL<-as.factor(data_CDP_final$GDVOLBL)

#CDP as factor
data_CDP_final$CDP<-as.factor(data_CDP_final$CDP)
data_CDP_final$TRELMOS<-log(data_CDP_final$TRELMOS+1)
data_CDP_final$RLPS2YR<-(data_CDP_final$RLPS3YR+data_CDP_final$RLPS1YR)/2
data_CDP_final$RLPS2YR<-log(data_CDP_final$RLPS2YR+1)
todrop<-c("RLPS1YR","RLPS3YR")
data_CDP_final<-data_CDP_final[ , !(names(data_CDP_final) %in% todrop)]

