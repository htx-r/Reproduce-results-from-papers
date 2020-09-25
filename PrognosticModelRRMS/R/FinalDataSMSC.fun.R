###################################################################################################################
####################  Function that transforms and selects the needed variables #####################################
###################################################################################################################



FinalDataSMSC.fun=function(datapath){

    ## Read the IPD SMSC data

    # path and loading the data
    SMSCpath=paste(datapath,"/SMSC_phase1_cycles.xlsx",sep="")
    SMSCdata <- read_excel(SMSCpath)
    #keep only needed variables, based on the literature and pre-existing prognostic models on relapses for RRMS patients
    tokeep<-c("unique.visit.id" , "patient.id", "age", "gender","edss", "disease.duration","treatment.naive.prior.visit", "months.since.last.relapse", "nr.relapses.2y.prior.study",
              "nr.Gd.enhanced.lesions","treatment.during.cycle","treatment.time.during.cycle.months","relapse.2y.after.study")
    SMSCdata <- SMSCdata[,(names(SMSCdata) %in% tokeep)]

    ######## Needed transformations for CONTINUOUS variables to approximate the normal distribution #####################

    # disease duration in transformed to log(disease duration +10)
    SMSCdata$disease.duration<-log(SMSCdata$disease.duration+10)
    # months since last relapse relapse is transformed to log(months since last relapse +10)
    SMSCdata$months.since.last.relapse<-log(SMSCdata$months.since.last.relapse+10)

     ######## Needed transformations for CATEGORICAL variables #####################

    # GD enhanced lessions, instead of 0, 1, ..., 18 now we will have 0 and >0
    SMSCdata$nr.Gd.enhanced.lesions[which(SMSCdata$nr.Gd.enhanced.lesions>0)]<-1
    SMSCdata$nr.Gd.enhanced.lesions<-as.factor(SMSCdata$nr.Gd.enhanced.lesions)

    # Number of relapses 2 years prior to study, instead of 0, 1, ..., 6 now we will have 0 and 1 and >1
    SMSCdata$nr.relapses.2y.prior.study[which(SMSCdata$nr.relapses.2y.prior.study>1)]<-2
    SMSCdata$nr.relapses.2y.prior.study<-as.factor(SMSCdata$nr.relapses.2y.prior.study)

    ## treatment during cycle 0 for no and 1 for yes
    SMSCdata$treatment.naive.prior.visit[which(SMSCdata$treatment.naive.prior.visit==FALSE)]<-0
    SMSCdata$treatment.naive.prior.visit[which(SMSCdata$treatment.naive.prior.visit==TRUE)]<-1
    SMSCdata$treatment.naive.prior.visit<-as.factor(SMSCdata$treatment.naive.prior.visit)

    ### gender 0 for men and 1 for women
    SMSCdata$gender[which(SMSCdata$gender=="Men")]<-0
    SMSCdata$gender[which(SMSCdata$gender=="Women")]<-1
    SMSCdata$gender<-as.factor(SMSCdata$gender)

    ### treatment during cycle 0 for no and 1 for yes
    SMSCdata$treatment.during.cycle[which(SMSCdata$treatment.during.cycle=="No")]<-0
    SMSCdata$treatment.during.cycle[which(SMSCdata$treatment.during.cycle=="Yes")]<-1
    SMSCdata$treatment.during.cycle<-as.factor(SMSCdata$treatment.during.cycle)

    ##make a numeric outcome
    SMSCdata$outcome<-NA
    SMSCdata$outcome[which(SMSCdata$relapse.2y.after.study=="Yes")]<-1
    SMSCdata$outcome[which(SMSCdata$relapse.2y.after.study=="No")]<-0
    SMSCdata$outcome<-as.factor(SMSCdata$outcome)


    todrop<-c("relapse.2y.after.study")
    SMSCdata <- SMSCdata[,!(names(SMSCdata) %in% todrop)]

    # treatment time during cycle should be zero when treatment during cycle is 0

    SMSCdata$treatment.time.during.cycle.months[which(SMSCdata$treatment.during.cycle==0)]<-0
    SMSCdata$treatment.time.during.cycle.monthsFactor<-NA
    SMSCdata$treatment.time.during.cycle.monthsFactor<-SMSCdata$treatment.time.during.cycle.months
    SMSCdata$treatment.time.during.cycle.monthsFactor[which(SMSCdata$treatment.time.during.cycle.monthsFactor<9)]<-0
    SMSCdata$treatment.time.during.cycle.monthsFactor[which(SMSCdata$treatment.time.during.cycle.monthsFactor>=9)]<-1
    SMSCdata$treatment.time.during.cycle.monthsFactor<-as.factor(SMSCdata$treatment.time.during.cycle.monthsFactor)



    return(SMSCdata)

}






