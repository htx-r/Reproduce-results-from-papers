###########################################################################################################################
##############  function that selects variables of interest and ####################
##############         recodes some of factors        #############################
#####################################################################################################################################################

numericalDataRisk.fun=function(dataset){
  #library(car)
  ##################################HANDLING VARIABLES##############################################
  ##keep only needed variables
  ## remove variables: TRT01AN is the same as TRTO1A but numeric, SAFFL=Safety population flag that included only 1 category: YES
  ### TRTSDT, TRTEDT, TRTDYS, STUDYS that are regarding the starting , ended and study's dates and we took them into account for relapse 1 and 2 years
  ####COMPLFL= completers population flag that has to do with the analysis,PPROTFEL=per protocol population flag
  ### COUNTRY as 44 countries are redorded and we kept region
  ####No prognostic factors: MRIFL=MRI population flag, ALTMSFL= Alternative MS treatment flag
  ####ALTMSDT=Start date of alt ms treatment & ALTMSDY, CEREBEBL: not normal categories,
  ### relapse 1 year as it is not our outcome
  todrop<-c("TRT01AN","SAFFL","TRTSDT","TRTEDT","TRTDYS","STUDYS","COMPLFL","COUNTRYC", "MRIFL","ALTMSFL",
            "ALTMSDT","ALTMSDY","PPROTFL","CEREBEBL","RELAPSE1year")
  dataset <- dataset[,!(names(dataset) %in% todrop)]
##exclude also BVZBL* GDLESBL* T1VOLBL* T2VOLBL* as Fabio did a sensitivity test and showed that these variables
  ## do not influence the discriminative ability of the prognostic model. Also a big ammount of missing values

  todrop<-c("BVZBL","GDLESBL","T1VOLBL","T2VOLBL")
  dataset <- dataset[,!(names(dataset) %in% todrop)]
  ##remove variables with more than 30% missing values
  colSums(is.na(dataset))/nrow(dataset)
  tokeep<-names(dataset[colSums(is.na(dataset))/nrow(dataset) < .5])
  names(dataset[colSums(is.na(dataset))/nrow(dataset) >= .5])
  dataset <- dataset[,(names(dataset) %in% tokeep)]
  MSrelapse<-dataset



  ##################################HANDLING VARIABLES############################
  ####################        FACTORS    ##########################################
  ###########################RECODE VARIABLES and make them factors############################
  MSrelapse$SEX<-recode(MSrelapse$SEX, "'M'=1; 'F'=0")
  MSrelapse$RACE<-recode(MSrelapse$RACE, "'WHITE'=1; 'NON-WHITE'=0")
  MSrelapse$DOMIHAND<-recode(MSrelapse$DOMIHAND, "'Left'=1;'LEFT'=1;  'Right'=0; 'RIGHT'=0;" )
  MSrelapse$DOMIHAND[which(MSrelapse$DOMIHAND=="")]<-NA
  ###MCDBL categories instead of 1, 2, 3, 4 there are  1, 2, >=3
  MSrelapse$MCDBL[which(MSrelapse$MCDBL==4)]<-3
  MSrelapse$MCDBL<-as.factor(MSrelapse$MCDBL)
  MSrelapse$PRMSGR<-as.factor(MSrelapse$PRMSGR)
  MSrelapse$PRDMTGR<-as.factor(MSrelapse$PRDMTGR)
  MSrelapse$REGION<-recode(MSrelapse$REGION, "'Eastern Europe'=1;'India'=2;  'North America'=3; 'ROW'=4;'Western Europe'=5 " )
  ###VISUALBL categories instead of 0, 1, 2, 3, 4, 5, 6 there are 0, 1, 2, >=3
  MSrelapse$VISUALBL[which(MSrelapse$VISUALBL==4 | MSrelapse$VISUALBL==5 | MSrelapse$VISUALBL==6)]<-3
  MSrelapse$VISUALBL<-as.factor(MSrelapse$VISUALBL)
  ###BRAINBL categories instead of 0, 1, 2, 3, 4 there are 0, 1, >=2
  MSrelapse$BRAINBL[which(MSrelapse$BRAINBL==3 | MSrelapse$BRAINBL==4)]<-2
  MSrelapse$BRAINBL<-as.factor(MSrelapse$BRAINBL)
  ###PYRAMIBL categories instead of 0, 1, 2, 3, 4, 5, 6 there are 0, 1, 2, >=3
  MSrelapse$PYRAMIBL[which(MSrelapse$PYRAMIBL==4 | MSrelapse$PYRAMIBL==5)]<-3
  MSrelapse$PYRAMIBL<-as.factor(MSrelapse$PYRAMIBL)

  ###SENSOR BL categories instead of 0, 1, 2, 3, 4, 5, 6 there are 0, 1, 2, >=3
  MSrelapse$SENSORBL[which(MSrelapse$SENSORBL==4 | MSrelapse$SENSORBL==5 | MSrelapse$SENSORBL==6)]<-3
  MSrelapse$SENSORBL<-as.factor(MSrelapse$SENSORBL)
  ###BOWLBLBLL categories instead of 0, 1, 2, 3, 4, 5, 6 there are 0, 1, 2, >=3
   MSrelapse$BOWLBLBL[which(MSrelapse$BOWLBLBL==4 | MSrelapse$BOWLBLBL==5 | MSrelapse$BOWLBLBL==6)]<-3
  MSrelapse$BOWLBLBL<-as.factor(MSrelapse$BOWLBLBL)
  ###CEREBRBL categories instead of 0 ,1, 2, 3, 4 there are 0, 1, >=2
   MSrelapse$CEREBRBL[which(MSrelapse$CEREBRBL==3 | MSrelapse$CEREBRBL==4)]<-2
  MSrelapse$CEREBRBL<-as.factor(MSrelapse$CEREBRBL)
  ###DISTWKBL
  MSrelapse$DISTWKBL<-as.numeric(MSrelapse$DISTWKBL)
  MSrelapse$DISTWKBL[which(MSrelapse$DISTWKBL==1)]<-NA
  MSrelapse$DISTWKBL[which(MSrelapse$DISTWKBL==2 | MSrelapse$DISTWKBL==3 | MSrelapse$DISTWKBL==4 | MSrelapse$DISTWKBL==5 )]<-0
  MSrelapse$DISTWKBL[which(MSrelapse$DISTWKBL==6)]<-1
  MSrelapse$DISTWKBL<-as.factor(MSrelapse$DISTWKBL)

  MSrelapse$RELAPSE2year<-as.factor(MSrelapse$RELAPSE2year)

  ###########################  check correlations and drop more than 60% correlated variables ####

  ########### categorical variables : cramer's v correlation
  is.fact <- sapply(MSrelapse, is.factor)
  vars<-names(MSrelapse[, is.fact])
  catcorrm <- function(vars, MSrelapse) sapply(vars, function(y) sapply(vars, function(x) assocstats(table(MSrelapse[,x], MSrelapse[,y]))$cramer))
  correlations_cat<-catcorrm(vars,MSrelapse)
  correlations_cat<-as.data.frame(correlations_cat)
  ###more than 70% correlated:PRMSGR with PRDMTGR     - RACE REGION (66%)
  ## we drop PRDMTGR and keep PRMSGR because Fabio uses PRMSGR
  todrop<-c("PRDMTGR")
  MSrelapse <- MSrelapse[,!(names(MSrelapse) %in% todrop)]
  ########## continuous variables spearman's correlation
  is.fact <- sapply(MSrelapse, is.factor)
  vars<-names(MSrelapse[, !is.fact])
  correlation_cont<-cor(MSrelapse[, !is.fact],method="spearman",use="complete.obs")
  correlation_cont<-as.data.frame(correlation_cont)
#for(i in 1:35){
 # print(i)
  #print(correlation_cont[(correlation_cont[,i]< -0.6 | correlation_cont[i]>0.6) ])
  #}

## ONYRS and DIAGYRS correlated, ONSYRS is kept as it has no missing values (also Fabio kept that)
## T25FWABL, T25FWZBL are exactly the same (r=-1) and higly correlated with T25FWABL was keplt because of Fabio.
## PASATABL=PASATZBL (r=1) and highly correlated with PASATP1 and MSFCBL. We keep PASATABL because of Fabio
## VFT100BL,VFT25BL,VFT125BL higly correlated. We keep VFT25BL because of its distribution (almost normal)
##NHPTDHPC,NHPTNHPC and NHPTMPC are higly correlated. NHPTMPC is kept because it is the average of the other two
## NHPTMP1, NHPTDHP1, NHPTNHP1, NHPTMBL, NHPTDHBL, NHPTNHBL, NHPTZBL higjly correlated. We kept NHPTMBL because of Fabio
 ### we dropped also RLPS3YRS as it containes much or more the same infos as RLPS1yr
   todrop<-c("DIAGYRS", "T25FWP1", "T25FWZBL", "MSFCBL", "PASATZBL","PASATP1","VFT100BL","VFT125BL",
            "NHPTDHPC","NHPTNHPC","NHPTNHBL","NHPTDHBL","NHPTZBL","NHPTNHP1","NHPTDHP1","NHPTMP1","RLPS3YR")

  MSrelapse <- MSrelapse[,!(names(MSrelapse) %in% todrop)]

## also we dropped RLPS3YR as we have RLPS1YR and those to variables are nested. We kept RLPS1yr because of the literature (Fabio kept this also)
  ###### transformations of continues variables to approximate normal distribution
  MSrelapse$ONSYRS<-log(MSrelapse$ONSYRS+1)
  MSrelapse$T25FWABL<-log(MSrelapse$T25FWABL+1)
  MSrelapse$NHPTMBL<-log(MSrelapse$NHPTMBL+1)
  MSrelapse$PASATPC<-log(MSrelapse$PASATPC+101)
  MSrelapse$NHPTMPC<-log(MSrelapse$NHPTMPC+100)
  MSrelapse$T25FWPC<-log(MSrelapse$T25FWPC+100)
  MSrelapse$RLPS1YR<-log(MSrelapse$RLPS1YR+1)

  return(MSrelapse)
}
