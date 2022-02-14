############################################################
####  Script which creates the needed variables:  ######
### Progression, time to progression, time of censoring  ####
### and combines the progression information with the #####
####   baseline characteristics of the patients  ##########
#####    into a single dataset ##########################
############################################################





##Load the new variable EDSS dataset
ades<-read.sas7bdat("C:/Users/kc19o338/Desktop/RealWorldPredictionModel/HTx/data/IPD data from 6 Biogen trials/ades.sas7bdat")

##Open your existing data "TwoStageRCTsResults.RData" from the folder "C:\Users\kc19o338\Desktop\Real world predictions project"
MSrelapse
##keep only the studies we need from relapses&Baseline dataset
keep<-c("DEFINE", "CONFIRM", "AFFIRM")
MSrelapse<-MSrelapse[MSrelapse$STUDYID %in% keep, ]
##keep only the studies we need from EDSS dataset
keep<-c("109MS301", "109MS302", "C-1801")
df<-ades[ades$STUDYID %in% keep, ]

##check if the number of patients is the same
length(unique(df$USUBJID))
nrow(MSrelapse)

### 2 dosages of DF make them 1
df$TRTA[which(df$TRTA=="BG00012 240 mg BID")]<-"Dimethyl Fumarate"
df$TRTA[which(df$TRTA=="BG00012 240 mg TID")]<-"Dimethyl Fumarate"


###create progression outcome based on Johannes recommendation
library(dplyr)
df$PROGRESSION<-NA

df <- mutate(df,
             PROGRESSION = case_when(
               (BASE<=5.5 & CHG>=1) | (BASE>=6.0 & CHG>=0.5) ~ 1,
               (BASE<=5.5 & CHG<1) | (BASE>=6.0 & CHG<0.5) ~ 0,
               TRUE ~ NA_real_ # This is for all other values
             ))      # not covered by the above.

###keep only the scheduled visits that are week12, week24, etc (every 3 months)
drop<-c("UNSCHED RELAPSE","UNSCHED RELAPSE2","UNSCHED RELAPSE3","UNSCHED RELAPSE4","UNSCHED RELAPSE5","UNSCHED RELAPSE6","UNSCHED RELAPSE7","UNSCHED RELAPSE8","UNSCHED RELAPSE9","UNSCHED RELAPSE10","UNSCHEDRELAPSE10","UNSCHEDULED")
df<-df[!(df$VISIT %in% drop), ]


##creation of Confirmed progression sustained after 3 months
df$CPD<-NA

##if a patient is progressed in one visit and then in the next scheduled visit (after 3 months) is still progressed then CPD=1
df<-df %>%
  group_by(USUBJID) %>%
  dplyr::mutate(
    CPD= ifelse(
      (PROGRESSION[dplyr::row_number()] == 1 & PROGRESSION[dplyr::row_number()+1]==1), 1, 0
    )
  )

#df_test<-df %>%
#  group_by(USUBJID) %>%
#  mutate(
#    CPD= ifelse(
#      (PROGRESSION[row_number()] == 1 & PROGRESSION[row_number()+1]==1 & (ADY[row_number()+1]-ADY[row_number()])>=80) | (PROGRESSION[row_number()] == 1 & PROGRESSION[row_number()+2]==1 & (ADY[row_number()+1]-ADY[row_number()])>=80) | (PROGRESSION[row_number()] == 1 & PROGRESSION[row_number()+3]==1  & (ADY[row_number()+2]-ADY[row_number()])<80 )  , 1, 0
#      )
#  )

##time to CPD or censored time
df<-df %>%
  group_by(USUBJID) %>%
  dplyr::mutate(
    Time_to_CPD_orCensored= ifelse(CPD == 1, ADY[dplyr::row_number()], max(ADY))
  )

##time to cpd only for those who CPD=1
df<-df %>%
  group_by(USUBJID) %>%
  dplyr::mutate(
    Time_to_CPD= ifelse(CPD == 1, ADY[dplyr::row_number()], NA)
  )

##censored time for thos with CPD=0
df<-df %>%
  group_by(USUBJID) %>%
  dplyr::mutate(
    Censored_Time= ifelse(CPD == 0, max(ADY), NA)
  )

#keep only the maximum per patient of CPD (either never CPD and hence all of them 0), or CPD and hence CPD=1
df<-df %>%
  group_by(USUBJID) %>%
  slice_max(CPD) %>%
  ungroup
##keep only the first row
df<-df[!duplicated(df$USUBJID),]

df$Censored_Time[is.na(df$Censored_Time)]<-0

df$is.censored<-!df$CPD
###check the outcome
table(df$CPD)
#625 out of 3493, 17.8% with the event
##########################DEFINE
table(df$CPD[which(df$STUDYID=="109MS301")])
## 225 out of 1186, 19%
table(df$CPD[which(df$STUDYID=="109MS301" & df$TRTA=="Dimethyl Fumarate")])
## 130 out of 789 = 16,5% under DF
table(df$CPD[which(df$STUDYID=="109MS301" & df$TRTA=="Placebo")])
## 95 out of 397 = 23,9% under Placebo


##########################CONFIRM
table(df$CPD[which(df$STUDYID=="109MS302")])
## 194 out of 1372 (14.1%)
table(df$CPD[which(df$STUDYID=="109MS302" & df$TRTA=="Dimethyl Fumarate")])
#85 out of 681 = 12.4% under DF
table(df$CPD[which(df$STUDYID=="109MS302" & df$TRTA=="GA")])
#55 out of 338 = 16.3% under GA
table(df$CPD[which(df$STUDYID=="109MS302" & df$TRTA=="Placebo")])
##54 out of 353 = 15.3% under Placebo

##########AFFIRM
table(df$CPD[which(df$STUDYID=="C-1801")])
## 206 out of 935 (22%)
table(df$CPD[which(df$STUDYID=="C-1801" & df$TRTA=="Natalizumab")])
## 110 out of 626 = 17,6% under Natalizumab
table(df$CPD[which(df$STUDYID=="C-1801" & df$TRTA=="Placebo")])
## 96 out of 309 = 31.1% under Placebo



#drop<-c("Screening","Baseline")
#df1<-df[!(df$AVISIT %in% drop), ]



### keep either 48 or 60 as these are close to 52=1year and select 48 (closer to 52) if exists otherwise slect the 60
#keep<-c("48","60")
#df1<-df[df$AVISITN %in% keep, ]
#df1<-df1%>% arrange(ordered(USUBJID, unique(USUBJID)), AVISITN)
#df<-df1[!duplicated(df1$USUBJID),]

#
#keep<-c("USUBJID","AVAL","CHG")
#df<-df[,keep]

RRMS<-merge(MSrelapse, df, by="USUBJID")
