#######################################################################################################################
#################################     SUMMARY, HISTOGRAMS AND TRANDFORMATIONS OF SMSC WITH REPEATED MEASURES    ################################################
#######################################################################################################################

### Load needed variables
library(readxl)
library(dplyr)
library(rms)

### 1st dataset for the first stage with repeated cycles for each patient (cycles are between 1 and 3 for each patient)

#load the data
observational_cycles_all=read_excel("C:/Users/kc19o338/Desktop/SMSC Basel/SMSC_phase1_cycles.xlsx")

## keep needed variables
tokeep<-c("unique.visit.id" , "patient.id", "age", "gender","edss", "disease.duration","treatment.naive.prior.visit", "months.since.last.relapse", "nr.relapses.2y.prior.study",
          "nr.Gd.enhanced.lesions","treatment.during.cycle","treatment.time.during.cycle.months","relapse.2y.after.study")
observational_cycles <- observational_cycles_all[,(names(observational_cycles_all) %in% tokeep)]

#number of rows
nrow(observational_cycles) #1752
#number of unique patients
length(unique(observational_cycles$patient.id)) #935
# number of complete cases
nrow(na.omit(observational_cycles)) #459
#variables included in the final dataset
names(observational_cycles)

#number of patients with 1, 2, or 3 two-years cycles
n_occur <- data.frame(table(observational_cycles$patient.id))

cycles_rows<-as.data.frame(rbind(nrow(n_occur[n_occur$Freq == 1,]),nrow(n_occur[n_occur$Freq == 2,]),nrow(n_occur[n_occur$Freq == 3,])))
colnames(cycles_rows)<-c("Number of patients")
cycles_columns<-as.data.frame(c("1","2","3"))
colnames(cycles_columns)<- c("Number of cycles/patient")
cycles_per_patient<-cbind(cycles_columns,cycles_rows)
cycles_per_patient

# just a test
nrow(n_occur[n_occur$Freq == 1,])+2*nrow(n_occur[n_occur$Freq == 2,])+3*nrow(n_occur[n_occur$Freq == 3,])==nrow(observational_cycles)

###### SUMMARY STATISTICS AND GRAPHS

##dataset with the first visit for each unique patient
observational_cycles_unique<-observational_cycles[!duplicated(observational_cycles$patient.id),]
nrow(observational_cycles_unique)

#### OUTCOME: RELAPSE OR NO RELAPSE DURING THE NEXT 2 YEARS

#repeated
counts <- table(observational_cycles$relapse.2y.after.study, useNA = "ifany")
names(counts)[is.na(names(counts))] <- "NA"
barplot(counts, main="Relapse during the next 2 years Distribution (cycles)",
        xlab="Relapse during the next 2 years", col = "blue", space=2)
table(observational_cycles$relapse.2y.after.study,useNA = "ifany")

#unique
counts <- table(observational_cycles_unique$relapse.2y.after.study, useNA = "ifany")
names(counts)[is.na(names(counts))] <- "NA"
barplot(counts, main="Relapse during the next 2 years Distribution (unique)",
        xlab="Relapse during the next 2 years", col = "blue", space=2)
table(observational_cycles_unique$relapse.2y.after.study,useNA = "ifany")


### AGE  ###

#repeated
hist(observational_cycles$age, col="blue", main="Distribution of Age (cycles)", xlab = "Age", breaks = 100)
summary(observational_cycles$age)
sum(is.na(observational_cycles$age))

#unique
hist(observational_cycles_unique$age, col="blue", main="Distribution of Age (unique)", xlab = "Age", breaks = 100)
summary(observational_cycles_unique$age)
sum(is.na(observational_cycles_unique$age))

### DISEASE DURATION  ###

#repeated
hist(observational_cycles$disease.duration, col="blue", main="Distribution of Disease Duration (cycles)", xlab = "Disease Duration", breaks = 100)
summary(observational_cycles$disease.duration)

#unique
hist(observational_cycles_unique$disease.duration, col="blue", main="Distribution of Disease Duration (unique)", xlab = "Disease Duration", breaks = 100)
summary(observational_cycles_unique$disease.duration)

#transformation of disease duration to log(disease duration + 10)
observational_cycles$disease.duration<-log(observational_cycles$disease.duration+10)
observational_cycles_unique$disease.duration<-log(observational_cycles_unique$disease.duration+10)
#repeated
hist(observational_cycles$disease.duration, col="blue", main="Distribution of Disease Duration (cycles)", xlab = "Disease Duration", breaks = 100)
summary(observational_cycles$disease.duration)
#unique
hist(observational_cycles_unique$disease.duration, col="blue", main="Distribution of Disease Duration (cycles)", xlab = "Disease Duration", breaks = 100)
summary(observational_cycles_unique$disease.duration)

### EDSS  ###

#repeated
hist(observational_cycles$edss, col="blue", main="Distribution of EDSS (cycles)",xlim = c(0,7), xlab = "EDSS", breaks = 10)
summary(observational_cycles$edss)

#unique
hist(observational_cycles_unique$edss, col="blue", main="Distribution of EDSS (unique)" ,xlim = c(0,7), xlab = "EDSS", breaks = 10)
summary(observational_cycles_unique$edss)


### GD enhanced lessions  ###

table(observational_cycles$nr.Gd.enhanced.lesions)
### transformation of GD enhanced lessions, instead of 0, 1, ..., 18 now we will have 0 and >0
observational_cycles$nr.Gd.enhanced.lesions[which(observational_cycles$nr.Gd.enhanced.lesions>0)]<-1
observational_cycles_unique$nr.Gd.enhanced.lesions[which(observational_cycles_unique$nr.Gd.enhanced.lesions>0)]<-1

#repeated
counts <- table(observational_cycles$nr.Gd.enhanced.lesions, useNA = "ifany")
names(counts)[is.na(names(counts))] <- "NA"
barplot(counts, main="GD Enhanced lessions Distribution (cycles)",
        xlab="Number of GD Enhanced lessions", col = "blue", space=2)
table(observational_cycles$nr.Gd.enhanced.lesions,useNA = "ifany")

#unique
counts <- table(observational_cycles_unique$nr.Gd.enhanced.lesions, useNA = "ifany")
names(counts)[is.na(names(counts))] <- "NA"
barplot(counts, main="GD Enhanced lessions Distribution (unique)",
        xlab="Number of GD Enhanced lessions", col = "blue", space=2)
table(observational_cycles_unique$nr.Gd.enhanced.lesions,useNA = "ifany")

### NUMBER OF RELAPSES 2 YEARS PRIOR TO STUDY  ###

table(observational_cycles$nr.relapses.2y.prior.study)

### transformation of number of relapses 2 years prior to study, instead of 0, 1, ..., 6 now we will have 0 and 1 and >1
observational_cycles$nr.relapses.2y.prior.study[which(observational_cycles$nr.relapses.2y.prior.study>1)]<-2
observational_cycles_unique$nr.relapses.2y.prior.study[which(observational_cycles_unique$nr.relapses.2y.prior.study>1)]<-2

#repeated
counts <- table(observational_cycles$nr.relapses.2y.prior.study, useNA = "ifany")
names(counts)[is.na(names(counts))] <- "NA"
barplot(counts, main="Number of relapses 2 years prior to study Distribution (cycles)",
        xlab="Number of relapses 2 years prior to study", col = "blue", space=2)
table(observational_cycles$nr.relapses.2y.prior.study,useNA = "ifany")

#unique
counts <- table(observational_cycles_unique$nr.relapses.2y.prior.study, useNA = "ifany")
names(counts)[is.na(names(counts))] <- "NA"
barplot(counts, main="Number of relapses 2 years prior to study Distribution (unique)",
        xlab="Number of relapses 2 years prior to study", col = "blue", space=2)
table(observational_cycles_unique$nr.relapses.2y.prior.study,useNA = "ifany")



### MONTHS SINCE PRE STUDY RELAPSE  ###

#repeated
hist(observational_cycles$months.since.last.relapse, col="blue", main="Distribution of Months since pre study relapse (cycles)", xlab = "Months since pre study relapse (cycles)", breaks = 100)
summary(observational_cycles$months.since.last.relapse)

#unique
hist(observational_cycles_unique$months.since.last.relapse, col="blue", main="Distribution of Months since pre study relapse (unique)" , xlab = "Months since pre study relapse", breaks = 100)
summary(observational_cycles_unique$months.since.last.relapse)

#transformation of months since pre study relapse to log(months since pre study relapse +10)
observational_cycles$months.since.last.relapse<-log(observational_cycles$months.since.last.relapse+10)
observational_cycles_unique$months.since.last.relapse<-log(observational_cycles_unique$months.since.last.relapse+10)

#repeated
hist(observational_cycles$months.since.last.relapse, col="blue", main="Distribution of Months since pre study relapse (cycles)", xlab = "Months since pre study relapse (cycles)", breaks = 100)
summary(observational_cycles$months.since.last.relapse)

#unique
hist(observational_cycles_unique$months.since.last.relapse, col="blue", main="Distribution of Months since pre study relapse (unique)" , xlab = "Months since pre study relapse", breaks = 100)
summary(observational_cycles_unique$months.since.last.relapse)


### TREATMENT NAIVE PRIOT TO VISIT ###

table(observational_cycles$treatment.naive.prior.visit, useNA = "ifany")
#repeated
counts <- table(observational_cycles$treatment.naive.prior.visit, useNA = "ifany")
names(counts)[is.na(names(counts))] <- "NA"
barplot(counts, main="Treatment naive Distribution (cycles)",
        xlab="Treatment naive", col = "blue", space=2)
table(observational_cycles$treatment.naive.prior.visit,useNA = "ifany")

#unique
counts <- table(observational_cycles_unique$treatment.naive.prior.visit, useNA = "ifany")
names(counts)[is.na(names(counts))] <- "NA"
barplot(counts, main="Treatment naive Distribution (unique)",
        xlab="Treatment naive", col = "blue", space=2)
table(observational_cycles_unique$treatment.naive.prior.visit,useNA = "ifany")

### GENDER ###

table(observational_cycles$gender, useNA = "ifany")
#repeated
counts <- table(observational_cycles$gender, useNA = "ifany")
names(counts)[is.na(names(counts))] <- "NA"
barplot(counts, main="Gender Distribution (cycles)",
        xlab="Gender", col = "blue", space=2)
table(observational_cycles$gender,useNA = "ifany")

#unique
counts <- table(observational_cycles_unique$gender, useNA = "ifany")
names(counts)[is.na(names(counts))] <- "NA"
barplot(counts, main="Gender Distribution (unique)",
        xlab="Gender", col = "blue", space=2)
table(observational_cycles_unique$gender,useNA = "ifany")

### TREATMENT DURING CYCLE ###

table(observational_cycles$treatment.during.cycle, useNA = "ifany")
#repeated
counts <- table(observational_cycles$treatment.during.cycle, useNA = "ifany")
names(counts)[is.na(names(counts))] <- "NA"
barplot(counts, main="Treatment during cycle (cycles)",
        xlab="Treatment during cycle", col = "blue", space=2)
table(observational_cycles$treatment.during.cycle,useNA = "ifany")

#unique
counts <- table(observational_cycles_unique$treatment.during.cycle, useNA = "ifany")
names(counts)[is.na(names(counts))] <- "NA"
barplot(counts, main="Treatment during cycle (unique)",
        xlab="Treatment during cycle", col = "blue", space=2)
table(observational_cycles_unique$treatment.during.cycle,useNA = "ifany")


### TIME OF TREATMENT DURING CYCLE ###

#no proper transformation
#repeated
hist(observational_cycles$treatment.time.during.cycle.months, col="blue", main="Distribution of Time of Treatment during the cycle - months (cycles)", xlab = "Time of Treatment during the cycle - months", breaks = 100)
summary(observational_cycles$treatment.time.during.cycle.months)

#unique
hist(observational_cycles_unique$treatment.time.during.cycle.months, col="blue", main="Distribution of Time of Treatment during the cycle - months (unique)", xlab = "Time of Treatment during the cycle - months", breaks = 100)
summary(observational_cycles_unique$treatment.time.during.cycle.months)

rm(list=ls())
