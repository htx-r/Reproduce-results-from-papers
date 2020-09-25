###################################################################################################################
####################  Function that prepares the data needed for Multiple Imputations ###########
####################  All data (plus variables with missing values) with proper transformations ###########
###################################################################################################################



MIDataSMSC.fun=function(datapath){
  datasetpath=paste(datapath,"/SMSC_phase1_cycles.xlsx",sep="")
  dataset<- read_excel(datasetpath)

  todrop<-c("visit.date","visit.type","disease.course","edss.date","excluded.during.manual.review","nr.relapses.2y.after.study", "birth.date","ethnic.origin",
            "symptoms.date.partial.dates.processed","ms.diagnosis.date.partial.dates.processed","baseline.visit.date","confirmed.by.clinical.findings","confirmed.by.mri","confirmed.by.csf",  "confirmed.by.vep",
            "confirmed.by.other", "followup.time.y" , "last.visit.date", "remaining.followup","medi.interval", "ms.specific.treatment","followup.interval", "kfs.5", "kfs.6"  )
  dataset <- dataset[,!(names(dataset) %in% todrop)]


  ################################ MAKE THE PROPER TRANSFORMATIONS  ########################

  ######## Needed transformations for CONTINUOUS variables to approximate the normal distribution #####################

  # disease duration in transformed to log(disease duration +10)
  dataset$disease.duration<-log(dataset$disease.duration+10)
  # months since last relapse relapse is transformed to log(months since last relapse +10)
  dataset$months.since.last.relapse<-log(dataset$months.since.last.relapse+10)
  #esion_volume_ml_3mmfilteris transformed to log(esion_volume_ml_3mmfilter +1)
  dataset$lesion_volume_ml_3mmfilter<-log(dataset$lesion_volume_ml_3mmfilter+1)

  ######## Needed transformations and recoding for CATEGORICAL variables #####################

  #for kfs.1 make categories 0, 1, 2, 3 instead of 0, 1, 2, 3, 4
  dataset$kfs.1<-as.factor(dataset$kfs.1)
  dataset$kfs.1[which(dataset$kfs.1==4)]<-3

  #for kfs.2 make categories 0, 1, 2, 3 instead of 0, 1, 2, 3, 4, 5
  dataset$kfs.2<-as.factor(dataset$kfs.2)
  dataset$kfs.2[which(dataset$kfs.2==4)]<-3
  dataset$kfs.2[which(dataset$kfs.2==5)]<-3
  #for kfs.3 keep same categories 0, 1, 2, 3 and make it a factor
  dataset$kfs.3<-as.factor(dataset$kfs.3)
  #for kfs.4 make categories 0, 1, 2, 3 instead of 0, 1, 2, 3, 4, 5
  dataset$kfs.4<-as.factor(dataset$kfs.4)
  dataset$kfs.4[which(dataset$kfs.4==4)]<-3
  dataset$kfs.4[which(dataset$kfs.4==5)]<-3
  #for kfs.5.converted make categories 0, 1, 2, 3 instead of 0, 1, 2, 3, 4
  dataset$kfs.5.converted<-as.factor(dataset$kfs.5.converted)
  dataset$kfs.5.converted[which(dataset$kfs.5.converted==4)]<-3
  #for kfs.6.converted make categories 0, 1, 2, 3 instead of 0, 1, 2, 3, 4
  dataset$kfs.6.converted<-as.factor(dataset$kfs.6.converted)
  dataset$kfs.6.converted[which(dataset$kfs.6.converted==4)]<-3
  #for kfs.7 keep same categories 0, 1, 2, 3 and make it a factor
  dataset$kfs.7<-as.factor(dataset$kfs.7)

  ### dominant hand 0 for right and 1 for left
  dataset$dominant.hand[which(dataset$dominant.hand=="Left")]<-1
  dataset$dominant.hand[which(dataset$dominant.hand=="Right")]<-0
  dataset$dominant.hand[which(dataset$dominant.hand=="Unknown")]<-NA
  dataset$dominant.hand<-as.factor(dataset$dominant.hand)

  ### gender 0 for men and 1 for women
  dataset$gender[which(dataset$gender=="Men")]<-0
  dataset$gender[which(dataset$gender=="Women")]<-1
  dataset$gender<-as.factor(dataset$gender)

  ### treatment during cycle 0 for no and 1 for yes
  dataset$treatment.during.cycle[which(dataset$treatment.during.cycle=="No")]<-0
  dataset$treatment.during.cycle[which(dataset$treatment.during.cycle=="Yes")]<-1
  dataset$treatment.during.cycle<-as.factor(dataset$treatment.during.cycle)

  ### treatment during cycle 0 for no and 1 for yes
  dataset$treatment.naive.prior.visit[which(dataset$treatment.naive.prior.visit==FALSE)]<-0
  dataset$treatment.naive.prior.visit[which(dataset$treatment.naive.prior.visit==TRUE)]<-1
  dataset$treatment.naive.prior.visit<-as.factor(dataset$treatment.naive.prior.visit)

  ### treatmentt time during cycle 0 months if no treatment was given
  dataset$treatment.time.during.cycle.months[which(dataset$treatment.during.cycle==0)]<-0
  dataset$treatment.time.during.cycle.months<-exp(dataset$treatment.time.during.cycle.months)*(10^(-9))

  dataset$treatment.time.during.cycle.monthsFactor<-NA
  dataset$treatment.time.during.cycle.monthsFactor<- dataset$treatment.time.during.cycle.months
  dataset$treatment.time.during.cycle.monthsFactor[which( dataset$treatment.time.during.cycle.monthsFactor<9)]<-0
  dataset$treatment.time.during.cycle.monthsFactor[which( dataset$treatment.time.during.cycle.monthsFactor>=9)]<-1
  dataset$treatment.time.during.cycle.monthsFactor<-as.factor( dataset$treatment.time.during.cycle.monthsFactor)


  # GD enhanced lessions, instead of 0, 1, ..., 18 now we will have 0 and >0
  dataset$nr.Gd.enhanced.lesions[which(dataset$nr.Gd.enhanced.lesions>0)]<-1
  dataset$nr.Gd.enhanced.lesions<-as.factor(dataset$nr.Gd.enhanced.lesions)

  # Number of relapses 2 years prior to study, instead of 0, 1, ..., 6 now we will have 0 and 1 and >1
  dataset$nr.relapses.2y.prior.study[which(dataset$nr.relapses.2y.prior.study>1)]<-2
  dataset$nr.relapses.2y.prior.study<-as.factor(dataset$nr.relapses.2y.prior.study)
  # Number of relapses 1 year prior to study, instead of 0, 1, ..., 6 now we will have 0 and 1 and >1
  dataset$nr.relapses.1y.prior.study[which(dataset$nr.relapses.1y.prior.study>1)]<-2
  dataset$nr.relapses.1y.prior.study<-as.factor(dataset$nr.relapses.1y.prior.study)

  dataset$relapse.2y.after.study<-as.factor(dataset$relapse.2y.after.study)
  ##make a numeric outcome
  dataset$outcome<-NA
  dataset$outcome[which(dataset$relapse.2y.after.study=="Yes")]<-1
  dataset$outcome[which(dataset$relapse.2y.after.study=="No")]<-0
  dataset$outcome<-as.factor(dataset$outcome)


  dataset<-as.data.frame(dataset)

  ##drop the correlated variables identified by LASSO fun / and are not needed because they have the same number of NAs
  todrop<-c("nr.relapses.1y.prior.study")
  dataset <- dataset[,!(names(dataset) %in% todrop)]
   todrop<-c("nine.hole.peg.test.1.r", "nine.hole.peg.test.1.l", "nine.hole.peg.test.2.r", "nine.hole.peg.test.2.l",
            "ave.dom.nine.hole.peg.test" ,"ave.non.dom.nine.hole.peg.test")
  dataset <- dataset[,!(names(dataset) %in% todrop)]
  ## T2 lesions: we keep T2 lession volume instead of count (correlated 83%)
  todrop<-c("lesion_count_3mmfilter")
  dataset <- dataset[,!(names(dataset) %in% todrop)]


}
