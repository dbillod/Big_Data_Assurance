###############################################
## 		BIG DATA ET ASSURANCE		   ##
###############################################

#----------------------#
# Chargement packages  #
#----------------------#
library(ggplot2)
library(randomForest)






#================================#
# 	Chargement des donnees	   #
#================================#

base_a = read.csv("C:/Users/David/Documents/ENSAE/3A/Big Data et assurance/BNP Claims Management/train.csv", sep = ",")
head(base_a)
base_a[1,1]
dim(base_a)

attach(base_a)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#Protocole : Variable quantitative  #
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

#####
##-> summary
#####



#####
##-> Histogramme
#####




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#Protocole : Variable qualitative   #
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

#####
##-> summary
#####



#####
##-> Plot
#####






#-----------------------------#
# Sélection de variables	#
#-----------------------------#

##########
## On repère les variables qualitatives
##########



VariableCat=rep(1,131)
for (i in 1:131){
	if (is.numeric(base_a[1,i+2]))
	{
		VariableCat[i]=0
	}

}
VariableCat
sum(VariableCat)
posVarCat=which(VariableCat==1)
posVarCat

#########
## On va regarder le nombre de modalités des variables qualitatives
#########

nb_levels = matrix(0,nrow = 2, ncol = length(posVarCat))
nb_levels[1,] = posVarCat
for (i in 1:length(posVarCat)){
	nb_levels[2,i] = length(levels(base_a[,posVarCat[i]+2]))
}
nb_levels


##########
## Test du Chi2
##########

#On va créer un data frame qui résumera les résultats des tests du chi2
name = names(base_a)
name = name[-which(name == "ID" | name == "target"  )] 
test_indep  = data.frame(nom_var = name, pval = rep(2,length(name)), pourcent5 = rep(2,length(name)))



for (i in 1:(dim(base_a)[2]-2)){
	
	#on ne va pas jusqu'à dim(base_a)[2] car les 2 première colonnes sont *
	#ID et target, et donc ne seront pas sélectionnées
	
	var_act = test_indep[i,1]
	table_act = table(base_a[,var_act],target)
	chisqtest_act = chisq.test(table_act)
	
	test_indep[i,2] = chisqtest_act$p.value
	#if (length(warnings())> taillewarnings){
	#	taillewarnings = taillewarnings +1
	#	test_indep[i,4] = 1 
	#}

}
test_indep[,3] = test_indep[,2]>0.05
test_indep

##On va créer un autre tableau pour limiter les résultats
##du chi2 aux seules variables qualitatives

test_indep_quali = test_indep[posVarCat-2,]
test_indep_quali


#///////////////////////////////#
# 	RANDOM FOREST		  #
#///////////////////////////////#


rf1 = randomForest(as.factor(target)~.,data  = base_a
, na.action = na.omit
, ntree = 5
, mtry = 5)
rf1

rf2 = rfImpute(x = base_a, y = as.factor(target), ntree = 10)

##On va enlever les variables avec plus de 53 modalités
posvar_53 = NULL
for (i in 1:length(posVarCat)){
	if (nb_levels[2,i]>=53){
		posvar_53 = c(posvar_53,nb_levels[1,i])
	}
}
posvar_53

base_a1 = base_a[,-(posvar_53+2)]
rf3 = rfImpute(x = base_a1[,-c(1,2)], y = as.factor(base_a1$target), ntree = 10)

memory.size()
memory.limit(size = 98000)

listindiv = sample(dim(base_a)[1], 100, T)
rf4 = rfImpute(x = base_a1[listindiv,-c(1,2)], y = as.factor(base_a1$target)[listindiv], ntree = 10)

varcible = as.factor(base_a1$target)[listindiv]
randomforest1 = randomForest(varcible ~base_a1[listindiv,-c(1,2)],  rf4)
importance(randomForest1)


library(parallel)
library(foreach)
library(doParallel)

# Calculate the number of cores
no_cores <- detectCores() - 1

# Initiate cluster
cl <- makeCluster(no_cores)

registerDoParallel(cl)
rf <- foreach(ntree=rep(100, no_cores), .combine= c, .packages='randomForest') %dopar% {
  #randomForest((Surv1==1)~Gender+Type+Category
  #             +Occupation +Age +Group1 +Bonus
  #             +Poldur +Value +Adind +Group2 +Density,
  #             data=base_test,
  #             ntree=ntree, importance=TRUE, keep.forest=TRUE)



}
stopCluster(cl)

library(randomForest)
varImpPlot (rf , main ="")
importance (rf)

courbeROC(rf,base_pred$Surv1,"Random Forest")?





#///////////////////////////////#
#	MODELE LOGIT		  #
#///////////////////////////////#
