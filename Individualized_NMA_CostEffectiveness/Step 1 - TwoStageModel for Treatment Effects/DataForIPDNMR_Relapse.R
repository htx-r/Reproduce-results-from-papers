#################################################################################
############## Preperation of data needed for the jags Relapse model ##########
#####################################################################


jagsdataIPDNMR_Relapse <- list(
  Nstudies=3,
  NPstudies=3,
  Np=nrow(data_CDP_CC),
  studyid=as.numeric(data_CDP_CC$STUDYID),
  outcome=as.numeric(data_CDP_CC$RELAPSE2year)-1,
  outcomeP=as.numeric(data_CDP_CC_placebo$RELAPSE2year)-1,
  NpPlacebo=nrow(data_CDP_CC_placebo),
  treat= rbind(c(1,4,NA),c(1,2,4),c(3,4,NA)),
  na=c(2,3,2),
  logitRisknew=logitRisknew,
  logitmeanRisknew=0,
  studyidP=as.numeric(data_CDP_CC_placebo$STUDYID),
  Nnew=99,
  mean_cov=mean(data_CDP_CC$logitRisk),
  arm=data_CDP_CC$arm,
  Risk=data_CDP_CC$logitRisk,
  Risk_p=data_CDP_CC_placebo$logitRisk,
  nt=4,
  ref=4,
  mean_cov_internal=mean(data_CDP_CC$logitRisk),
  logitRisk_internal=data_CDP_CC$logitRisk,
  treatment=data_CDP_CC$TRT01A

)
