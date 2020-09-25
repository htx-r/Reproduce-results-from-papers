#############################################################################################################
################################### Script for apparent discrimination and calibration measures #################
##################################################################################################################

mean(SMSCdataC$Risk)
mean(as.numeric(SMSCdataC$outcome)-1)

#the original with no changes - use logitp and Risk

app_cstat_model <- roc(outcome~logitp,data=SMSCdataC, ci=TRUE)
app_cstat_model$auc #0.67


# C-slope
mod_log_2 <- glm(outcome~logitp,family="binomial",data=SMSCdataC,x=TRUE,y=TRUE)
mod_log_2
confint(mod_log_2)


