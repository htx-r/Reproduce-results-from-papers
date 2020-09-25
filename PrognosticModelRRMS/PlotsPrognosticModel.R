##############################################################################################################
####################### Script for the plot of risk distribution in SMSC dataset ################################
###################################################################################################################




SMSCdataC$outcome<-as.factor(SMSCdataC$outcome)
RiskPrFactor<-ggdensity(SMSCdataC, x = "Risk",
                        add = "mean", rug = TRUE,
                        color = "outcome", fill = "outcome",
                        palette = c("yellow", "blue"), xlim=c(0,0.75),xlab = "Risk score as a prognostic factor")
t.test(SMSCdataC$Risk[SMSCdataC$outcome==1])
t.test(SMSCdataC$Risk[SMSCdataC$outcome==0])


RiskDistribution<-ggdensity(SMSCdataC, x = "Risk",
                            fill = "red", color = "red",
                            add = "mean", rug = TRUE)

summary(SMSCdataC$Risk)
summary((SMSCdataC$Risk[SMSCdataC$outcome==1]))
summary((SMSCdataC$Risk[SMSCdataC$outcome==0]))
