
##keep only the studies we need from relapses&Baseline dataset
keep<-c("DEFINE", "CONFIRM", "AFFIRM")
MSrelapse<-RCTs[RCTs$STUDYID %in% keep, ]
##keep only the studies we need from EDSS dataset
keep<-c("109MS301", "109MS302", "C-1801")
df1<-ades[ades$STUDYID %in% keep, ]

##check if the number of patients is the same
length(unique(df1$USUBJID))
nrow(MSrelapse)

### 2 dosages of DF make them 1
df1$TRTA[which(df1$TRTA=="BG00012 240 mg BID")]<-"Dimethyl Fumarate"
df1$TRTA[which(df1$TRTA=="BG00012 240 mg TID")]<-"Dimethyl Fumarate"


###create progression outcome based on Johannes recommendation
library(dplyr)
df1$PROGRESSION<-NA

df1 <- mutate(df1,
             PROGRESSION = case_when(
                (BASE<=5.5 & CHG>=1) | (BASE>=6.0 & CHG>=0.5) ~ 1,
               (BASE<=5.5 & CHG<1) | (BASE>=6.0 & CHG<0.5) ~ 0,
                   TRUE ~ NA_real_ # This is for all other values
                ))      # not covered by the above.

#df1 <- mutate(df1,
 #                           PROGRESSION = case_when(
  #                            (BASE<1.0 & CHG>=1.5) | ((BASE>=1 & BASE<=5.5) & CHG>=1) | (BASE>5.5 & CHG>=0.5) ~ 1,
   #                         (BASE<1.0 & CHG<1.5)| ((BASE>=1 & BASE<=5.5) & CHG<1) | (BASE>5.5 & CHG<0.5) ~ 0,
    #                          TRUE ~ NA_real_ # This is for all other values
     #                      ))      # not covered by the above.

###keep only the scheduled visits that are week12, week24, etc (every 3 months)
df1<-df1[!is.na(df1$AVISITN),]
df1<-df1[df1$AVISITN>=0,]


##creation of Confirmed progression sustained after 3 months
df1$CDP<-NA

##if a patient is progressed in one visit and then in the next scheduled visit (after 3 months) is still progressed then CDP=1
df1<-df1 %>%
  group_by(USUBJID) %>%
  dplyr::mutate(
    CDP= ifelse(
      (PROGRESSION[dplyr::row_number()] == 1 & PROGRESSION[dplyr::row_number()+1]==1), 1, 0
    )
  )

#df_test<-df %>%
#  group_by(USUBJID) %>%
#  mutate(
#    CDP= ifelse(
#      (PROGRESSION[row_number()] == 1 & PROGRESSION[row_number()+1]==1 & (ADY[row_number()+1]-ADY[row_number()])>=80) | (PROGRESSION[row_number()] == 1 & PROGRESSION[row_number()+2]==1 & (ADY[row_number()+1]-ADY[row_number()])>=80) | (PROGRESSION[row_number()] == 1 & PROGRESSION[row_number()+3]==1  & (ADY[row_number()+2]-ADY[row_number()])<80 )  , 1, 0
#      )
#  )

##time to CDP or censored time
df1<-df1 %>%
  group_by(USUBJID) %>%
  dplyr::mutate(
    Time_to_CDP_orCensored= ifelse(CDP == 1, AVISITN[dplyr::row_number()], max(AVISITN))
  )

##time to CDP only for those who CDP=1
df1<-df1 %>%
  group_by(USUBJID) %>%
  dplyr::mutate(
    Time_to_CDP= ifelse(CDP == 1, AVISITN[dplyr::row_number()], NA)
  )

##censored time for thos with CDP=0
df1<-df1 %>%
  group_by(USUBJID) %>%
  dplyr::mutate(
    Censored_Time= ifelse(CDP == 0, max(AVISITN), NA)
  )

#keep only the maximum per patient of CDP (either never CDP and hence all of them 0), or CDP and hence CDP=1
df1<-df1 %>%
  group_by(USUBJID) %>%
  dplyr::slice_max(CDP) %>%
  ungroup
##keep only the first row
df1<-df1[!duplicated(df1$USUBJID),]


###check the outcome
table(df1$CDP)
#650   out of 3495, 18.6% with the event

##########################DEFINE
table(df1$CDP[which(df1$STUDYID=="109MS301")])
## 211  out of 1187, 17.8%
table(df1$CDP[which(df1$STUDYID=="109MS301" & df1$TRTA=="Dimethyl Fumarate")])
## 121 out of 790= 15.3% under DF
table(df1$CDP[which(df1$STUDYID=="109MS301" & df1$TRTA=="Placebo")])
## 90 out of 397 = 22,7% under Placebo


##########################CONFIRM
table(df1$CDP[which(df1$STUDYID=="109MS302")])
## 175 out of 1372 (12.8%)
table(df1$CDP[which(df1$STUDYID=="109MS302" & df1$TRTA=="Dimethyl Fumarate")])
#77 out of 681 = 11.3% under DF
table(df1$CDP[which(df1$STUDYID=="109MS302" & df1$TRTA=="GA")])
#48 out of 338 CDP = 14.2% under GA
table(df1$CDP[which(df1$STUDYID=="109MS302" & df1$TRTA=="Placebo")])
##50 out of 353 = 14.5% under Placebo

##########AFFIRM
table(df1$CDP[which(df1$STUDYID=="C-1801")])
## 219  out of 936 (23.4%)
table(df1$CDP[which(df1$STUDYID=="C-1801" & df1$TRTA=="Natalizumab")])
## 116 out of 626 = 18.5% under Natalizumab
table(df1$CDP[which(df1$STUDYID=="C-1801" & df1$TRTA=="Placebo")])
## 103 out of 309 = 33.3% under Placebo



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
df1<-df1[,c(2,36,37,38,39)]

data_CDP<-merge(MSrelapse, df1, by="USUBJID")

