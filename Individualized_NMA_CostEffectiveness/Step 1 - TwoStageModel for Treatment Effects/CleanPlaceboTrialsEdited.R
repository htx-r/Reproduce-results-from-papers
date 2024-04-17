########################################################
###########       Cleaning Placebo-arms RCTs  ###########
########################################################

CleanPlaceboTrialsEdited.fun<-function (datapath)
{
  library(readxl)
  library(tidyverse)
  library(car)
  library(MASS)
  dmpath = paste(datapath, "/dm.csv", sep = "")
  cepath = paste(datapath, "/ce.csv", sep = "")
  ftpath = paste(datapath, "/ft.csv", sep = "")
  qspath = paste(datapath, "/qs.csv", sep = "")
  datadm <- read.csv(dmpath)
  datace <- read.csv(cepath)
  ft <- read.csv(ftpath)
  qs <- read.csv(qspath)
  keeps <- c("USUBJID", "AGE", "SEX", "RACE")
  demographics <- datadm[, keeps]
  demographics$RACE[demographics$RACE == ""] <- NA
  demographics <- demographics[order(demographics$USUBJID),
  ]
  datace <- datace[order(datace$USUBJID), ]
  keeps <- c("USUBJID", "CESTDY", "MIDS")
  unconfirmed1 <- datace[, keeps]
  unconfirmed1$MIDS[unconfirmed1$MIDS == ""] <- NA
  unconfirmed1$MIDS <- as.numeric(as.factor(unconfirmed1$MIDS))
  unconfirmed1$MIDS <- recode(unconfirmed1$MIDS, "2=1; 3=1; 4=1; 5=1; 6=1; 7=1; 8=1; 9=1; 10=1;")
  unconfirmed1$MIDS[is.na(unconfirmed1$MIDS)] <- 0
  confirmed <- unconfirmed1[which(unconfirmed1$MIDS == 1),
  ]
  confirmed1 <- confirmed %>% group_by(USUBJID) %>% slice(which.min(CESTDY))
  f <- confirmed[which(is.na(confirmed$CESTDY)), ]
  relapsestime <- merge(f, confirmed1, by = "USUBJID",
                        all = TRUE)
  relapsestime$time <- pmax(relapsestime$CESTDY.x, relapsestime$CESTDY.y,
                            na.rm = TRUE)
  relapsestime$relapse <- pmax(relapsestime$MIDS.x, relapsestime$MIDS.y,
                               na.rm = TRUE)
  keeps <- c("USUBJID", "time", "relapse")
  relapsestime <- relapsestime[, keeps]
  relapsestime <- unique(relapsestime)
  unconfirmed <- unconfirmed1[which(unconfirmed1$MIDS == 0),
  ]
  unconfirmed2 <- unconfirmed %>% group_by(USUBJID) %>% slice(which.max(CESTDY))
  q <- unconfirmed[which(is.na(unconfirmed$CESTDY)), ]
  q <- unique(q)
  unconfirm <- merge(unconfirmed2, q, by = "USUBJID",
                     all = TRUE)
  keeps <- c("USUBJID", "CESTDY.x")
  unconfirm <- unconfirm[, keeps]
  unconfirm$relapse <- 0
  names(unconfirm) = c("USUBJID", "VISIT", "RELAPSE")
  keeps <- c("USUBJID", "FTDY")
  ft <- ft[, keeps]
  ft <- ft[order(ft$USUBJID), ]
  ft <- unique(ft)
  ft1 <- ft %>% group_by(USUBJID) %>% slice(which.max(FTDY))
  keep <- c("USUBJID", "QSDY")
  qs <- qs[, keep]
  qs <- unique(qs)
  qs1 <- qs %>% group_by(USUBJID) %>% slice(which.max(QSDY))
  keep <- c("USUBJID", "VISIT")
  obs3 <- unconfirm[, keep]
  observationaltime <- merge(qs1, ft1, by = "USUBJID",
                             all = TRUE)
  observationaltime <- merge(observationaltime, obs3, by = "USUBJID",
                             all = TRUE)
  observationaltime$maxobs <- pmax(observationaltime$QSDY,
                                   observationaltime$FTDY, observationaltime$VISIT, na.rm = TRUE)
  keep <- c("USUBJID", "maxobs")
  observationaltime <- observationaltime[, keep]
  unconfirmtime <- merge(unconfirm, observationaltime, by = "USUBJID",
                         all = TRUE)
  unconfirmtime <- unconfirmtime[which(unconfirmtime$RELAPSE ==
                                         0), ]
  keep <- c("USUBJID", "RELAPSE", "maxobs")
  unconfirmtime <- unconfirmtime[, keep]
  conuncontimes <- merge(unconfirmtime, relapsestime, by = "USUBJID",
                         all = TRUE)
  conuncontimes$relapse01 <- pmax(conuncontimes$RELAPSE, conuncontimes$relapse,
                                  na.rm = TRUE)
  keep <- c("USUBJID", "maxobs", "time",
            "relapse01")
  conuncontimes <- conuncontimes[, keep]
  conuncontimes$maxobs[which((conuncontimes$relapse01 == 1))] <- NA
  allrel <- merge(demographics, conuncontimes, by = "USUBJID",
                  all = TRUE)
  names(allrel) <- c("USUBJID", "AGE", "SEX",
                     "RACE", "Obstime", "RelapseTime", "Relapse")
  allrel$Relapse1year <- NA
  allrel$Relapse1year[which(allrel$Relapse == 1 & allrel$RelapseTime <=
                              365)] <- 1
  allrel$Relapse1year[which(allrel$Relapse == 1 & allrel$RelapseTime >
                              365)] <- 0
  allrel$Relapse1year[which(allrel$Relapse == 0 & allrel$Obstime >
                              355)] <- 0
  allrel$Relapse2year <- NA
  allrel$Relapse2year[which(allrel$Relapse == 1 & allrel$RelapseTime <=
                              730)] <- 1
  allrel$Relapse2year[which(allrel$Relapse == 1 & allrel$RelapseTime >
                              730)] <- 0
  allrel$Relapse2year[which(allrel$Relapse == 0 & allrel$Obstime >
                              710)] <- 0
  rm(conuncontimes, datace, datadm, demographics, f, ft, ft1,
     obs3, observationaltime, q, qs, qs1, relapsestime, unconfirm,
     unconfirmed, unconfirmed1, unconfirmed2, unconfirmtime,
     confirmed, confirmed1, keep, keeps)
  return(allrel)
}
