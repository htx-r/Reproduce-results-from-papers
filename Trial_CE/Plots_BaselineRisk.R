######################### SCRIPT PLOTTING RISK & LOGITRISK DISTRIBUTIONS  ###########


#library(ggpubr)
#install.packages("gridExtra")
#library(gridExtra)

####col.names of dataset
dataset=data_CDP_CC
names(dataset)[names(dataset) == "TRT01A"] <- "Treatment"
dataset$Treatment<-recode(dataset$Treatment,"1='Dimethyl fumarate';2='Galtiramer acetate'; 3='Natalizumab';4='Placebo'")
##### A. The density of the risk score in the whole dataset
#a <- ggplot(dataset, aes(x = LogitRisk))
# y axis scale = ..density.. (default behaviour)
#a + geom_density(fill = "lightgray") +
#  geom_vline(aes(xintercept = mean(LogitRisk)),
#             linetype = "dashed", size = 0.6,color = "#FC4E07")

#################################################LASSO model########################################
#################################################################################################

# A. The distribution in the whole dataset
#risk
summary(data_CDP_CC$Risk)
RiskDist<-ggdensity(data_CDP_CC, x = "Risk",
                         fill = "#0073C2FF", color = "#0073C2FF",
                         add = "mean", rug = TRUE, xlim=c(0,0.75))



#C. The distributionin those with true relapse and true non-relapse in the entire dataset

data_CDP_CC$RELAPSE2year<-as.factor(data_CDP_CC$RELAPSE2year)
#Risk
PrognosticRisk<-ggdensity(data_CDP_CC, x = "Risk",
                               add = "mean", rug = TRUE,
                               color = "RELAPSE2year", fill = "RELAPSE2year",
                               palette = c("blue", "yellow"), xlim=c(0,0.75),xlab = "Risk score as a prognostic factor")

