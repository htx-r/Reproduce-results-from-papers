###################################### SCRIPT THAT CENTRALIZE THE CONTINUES VARIABLES IN IMUTED DATASETS ###################3
#######################################       FOR USE IN GLM MODEL     ####################################################3
######################################################################################################################


imp.list1<-list(imp.list[[1]],imp.list[[2]],imp.list[[3]],imp.list[[4]],imp.list[[5]],imp.list[[6]],imp.list[[7]],imp.list[[8]],imp.list[[9]],imp.list[[10]])
#1st imputed dataset
for (i in 1:10){
imp.list1[[i]]<-imp.list[[i]]
imp.list1[[i]]$age<-imp.list1[[i]]$age-mean(imp.list1[[i]]$age)
imp.list1[[i]]$disease.duration<-imp.list1[[i]]$disease.duration-mean(imp.list1[[i]]$disease.duration)
imp.list1[[i]]$edss<-imp.list1[[i]]$edss-mean(imp.list1[[i]]$edss)
imp.list1[[i]]$months.since.last.relapse<-imp.list1[[i]]$months.since.last.relapse-mean(imp.list1[[i]]$months.since.last.relapse)
}

