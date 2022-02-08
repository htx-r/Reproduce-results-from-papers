########################################################
###########       Cleaning BIOGEN IPD DATA (3 RRTs)  ###########
########################################################

CleanBiogenEdited.fun<-function (datapath)
{
  library(readxl)
  adslpath = paste(datapath, "/adsl.csv", sep = "")
  adarrpath = paste(datapath, "/adarr.csv", sep = "")
  adrelpath = paste(datapath, "/adrel.csv", sep = "")
  adsl <- read.csv(adslpath)
  adarr <- read.csv(adarrpath)
  adrel <- read.csv(adrelpath)
  adarr_OBJREL <- adarr[adarr$PARAMCD == "OBJREL", ]
  adarr_OBJREL01year <- adarr_OBJREL[adarr_OBJREL$AVISIT ==
                                       "0-1 Year", ]
  RELAPSE01Year <- ifelse(adarr_OBJREL01year$AVAL > 0, 1, 0)
  nsubjects <- length(unique(adarr$USUBJID))
  usubjectID <- unique(adarr$USUBJID)
  adarr_OBJREL_02year_ds <- sapply(1:nsubjects, function(i) subset(adarr_OBJREL,
                                                                   subset = adarr_OBJREL$USUBJID == as.character(usubjectID[i]) &
                                                                     adarr_OBJREL$AVISIT == "Overall 0-2 Years"),
                                   simplify = F)
  RELAPSE02Year <- 1:nsubjects
  for (i in 1:nsubjects) {
    if (nrow(adarr_OBJREL_02year_ds[[i]]) == 0) {
      RELAPSE02Year[i] <- NA
    }
    else {
      if (adarr_OBJREL_02year_ds[[i]]$AVAL > 0) {
        RELAPSE02Year[i] <- 1
      }
      else {
        RELAPSE02Year[i] <- 0
      }
    }
  }
  adsl01 <- adsl
  adsl01$RELAPSE1year <- RELAPSE01Year
  adsl01$RELAPSE2year <- RELAPSE02Year

  adsl01$STUDYID[which(adsl01$STUDYID=="105MS301")]<-"ADVANCE"
  adsl01$STUDYID[which(adsl01$STUDYID=="109MS301")]<-"DEFINE"
  adsl01$STUDYID[which(adsl01$STUDYID=="109MS302")]<-"CONFIRM"
  adsl01$STUDYID[which(adsl01$STUDYID=="C-1801")]<-"AFFIRM"
  adsl01$STUDYID[which(adsl01$STUDYID=="C-1802")]<-"SENTINEL"
  adsl01$STUDYID[which(adsl01$STUDYID=="NS26321")]<-"MSCRG"


  adarr_OBJREL$STUDYID[which(adarr_OBJREL$STUDYID=="105MS301")]<-"ADVANCE"
  adarr_OBJREL$STUDYID[which(adarr_OBJREL$STUDYID=="109MS301")]<-"DEFINE"
  adarr_OBJREL$STUDYID[which(adarr_OBJREL$STUDYID=="109MS302")]<-"CONFIRM"
  adarr_OBJREL$STUDYID[which(adarr_OBJREL$STUDYID=="C-1801")]<-"AFFIRM"
  adarr_OBJREL$STUDYID[which(adarr_OBJREL$STUDYID=="C-1802")]<-"SENTINEL"
  adarr_OBJREL$STUDYID[which(adarr_OBJREL$STUDYID=="NS26321")]<-"MSCRG"




adsl01$TRT01A[which(adsl01$TRT01A=="AVONEX® 30 mcg")]<-"Avonex"
adsl01$TRT01A[which(adsl01$TRT01A=="BG00012 240 mg BID")]<-"Dimethyl fumarate"
adsl01$TRT01A[which(adsl01$TRT01A=="BG00012 240 mg TID")]<-"Dimethyl fumarate"
adsl01$TRT01A[which(adsl01$TRT01A=="BIIB017 125 mcg every 2 weeks")]<-"Peginterferon Beta-1a"
adsl01$TRT01A[which(adsl01$TRT01A=="BIIB017 125 mcg every 4 weeks")]<-"Peginterferon Beta-1a"
adsl01$TRT01A[which(adsl01$TRT01A=="GA")]<-"Glatiramer acetate"
adsl01$TRT01A[which(adsl01$TRT01A=="Placebo + Avonex")]<-"Avonex"



adarr_OBJREL$TRTA[which(adarr_OBJREL$TRTA=="AVONEX® 30 mcg")]<-"Avonex"
adarr_OBJREL$TRTA[which(adarr_OBJREL$TRTA=="BG00012 240 mg BID")]<-"Dimethyl fumarate"
adarr_OBJREL$TRTA[which(adarr_OBJREL$TRTA=="BG00012 240 mg TID")]<-"Dimethyl fumarate"
adarr_OBJREL$TRTA[which(adarr_OBJREL$TRTA=="BIIB017 125 mcg every 2 weeks")]<-"Peginterferon Beta-1a"
adarr_OBJREL$TRTA[which(adarr_OBJREL$TRTA=="BIIB017 125 mcg every 4 weeks")]<-"Peginterferon Beta-1a"
adarr_OBJREL$TRTA[which(adarr_OBJREL$TRTA=="GA")]<-"Glatiramer acetate"
adarr_OBJREL$TRTA[which(adarr_OBJREL$TRTA=="Placebo + Avonex")]<-"Avonex"





  return(list(adsl01 = adsl01, adarr_OBJREL = adarr_OBJREL))
}
