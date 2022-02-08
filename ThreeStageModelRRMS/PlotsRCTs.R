###############################################################################################################
###################################   SCRIPT FOR THE DISTRIBUTION PLOTS OF RISK IN RCTs ######################3
#################################################################################################################


### Distribution in RCTs for those who did relapse
## and for those who did not, seperately
RiskData$outcome<-as.factor(RiskData$outcome)
RiskPrFactor<-ggdensity(RiskData, x = "Risknew4",
          add = "mean", rug = TRUE,
          color = "outcome", fill = "outcome",
          palette = c("yellow", "blue"), xlim=c(0,0.75),xlab = "Risk score as a prognostic factor")
t.test(RiskData$Risknew4[RiskData$outcome==1])
t.test(RiskData$Risknew4[RiskData$outcome==0])

###Distribution of risk score in the entire dataset of RCTs
RiskDistribution<-ggdensity(RiskData, x = "Risknew4",
         fill = "red", color = "red",
         add = "mean", rug = TRUE)

summary(RiskData$Risknew4)
summary((RiskData$Risknew4[RiskData$relapse.2y.after.study==1]))
summary((RiskData$Risknew4[RiskData$relapse.2y.after.study==0]))
