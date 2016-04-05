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


##''''''''''''''''''''''''''''''''##
## Premier essai de Random Forest ##
##''''''''''''''''''''''''''''''''##


rf1 = randomForest(as.factor(target)~.,data  = base_a
, na.action = na.omit
, ntree = 5
, mtry = 5)
rf1

rf2 = rfImpute(x = base_a, y = as.factor(target), ntree = 10)

##''''''''''''''''''''''''''''''''##
## CCL : trop grosse base 	    ##
##''''''''''''''''''''''''''''''''##







##On va enlever les variables avec plus de 53 modalités, car randomForest ne veut pas les gérer
posvar_53 = NULL
for (i in 1:length(posVarCat)){
	if (nb_levels[2,i]>=53){
		posvar_53 = c(posvar_53,nb_levels[1,i])
	}
}
posvar_53

#####
## Il y a 3 variables à plus de 53 modalités
#####
base_a1 = base_a[,-(posvar_53+2)]

rf3 = rfImpute(x = base_a1[,-c(1,2)], y = as.factor(base_a1$target), ntree = 10)

memory.size()
memory.limit(size = 98000)

listindiv = sample(dim(base_a)[1], 100, T)
rf4 = rfImpute(x = base_a1[listindiv,-c(1,2)], y = as.factor(base_a1$target)[listindiv], ntree = 10)


varcible = as.factor(base_a1$target)[listindiv]

#randomforest1 = randomForest(varcible ~rf4[,2:129],  rf4)
randomforest1 = randomForest(y = varcible, x = rf4[,2:129],  data=rf4)
importance(randomforest1)
varImpPlot(randomforest1)


## -> On a réussi à faire une random forest sur une sous-base : 
##		seulement 100 lignes
##		on a retiré les variables catégorielles avec trop de modalités



## -> On va bootstraper : on va tirer M fois un échantillon de N personnes
## Ensuite, on va regarder quelles sont les variables qui ont la plus haute importance, en leur donnant un score
## Si une variable arrive "en tête" de l'importance, alors on lui donne un score de 10
##														5 pour la 2eme
##														1 pour la 3ème

M = 100
N = 1000
resimportance = rep(0, dim(base_a)[2]-2)

## On enlève 2 variables : ID, target
## Pour 3 autres : celles de plus de 53 modalités, on met -1
resimportance[posvar_53] = -1

for (i in 1:M){
	listindiv = sample(dim(base_a)[1], N, T)
	rf4 = rfImpute(x = base_a1[listindiv,-c(1,2)], y = as.factor(base_a1$target)[listindiv], ntree = 10)


	varcible = as.factor(base_a1$target)[listindiv]

	randomforest1_boot = randomForest(y = varcible, x = rf4[,2:129],  data=rf4)
	
	max1 = which(importance(randomforest1_boot) == max(importance(randomforest1_boot)))
	max2 = which(importance(randomforest1_boot)[-max1] == max(importance(randomforest1_boot)[-max1]))
	max3 = which(importance(randomforest1_boot)[-c(max1,max2)] == max(importance(randomforest1_boot)[-c(max1,max2)]))
	
	#Il faut voir s'il faut augmenter max1,...,max3 car on a enlevé certaines variables (>53 modalités)
	#Donc les indices ne correspondent plus forcément aux variables
	
	max1 = max1 + sum(posvar_53-max1<=0)
	max2 = max2 + sum(posvar_53-max2<=0)
	max3 = max3 + sum(posvar_53-max3<=0)
	
	
	resimportance[max1] = resimportance[max1] + 10
	resimportance[max2] = resimportance[max2] + 5
	resimportance[max3] = resimportance[max3] + 1

}

resimportance
which(resimportance>0)

## -> On va désormais essayer d'intégrer l'ensemble des lignes



rf3_1 = rfImpute(x = base_a1[,-c(1,2)], y = as.factor(base_a1$target), ntree = 10)


#==================================#
# 	Tentatvie ACM		     #
#==================================#

library(FactoMineR)


listeMCA = sample(dim(base_a)[1], 1000, T)

dataMCA1 = base_a1[listeMCA, 2+posVarCat]
MCA1 = MCA(dataMCA1
MCA1
MCA1$var$contrib

dataMCA2 = base_a1[listeMCA, 2+which(resimportance>0)]
MCA2 = MCA(dataMCA2)
MCA2

##On obtient 2-3 groupes de variables








library(parallel)
library(foreach)
library(doParallel)

# Calculate the number of cores
no_cores <- detectCores() - 1

# Initiate cluster
cl <- makeCluster(no_cores)

registerDoParallel(cl)
rf1 <- foreach(ntree=rep(100, no_cores), .combine= c, .packages='randomForest') %dopar% {
	rf4_cluster = rfImpute(x = base_a1[,-c(1,2)], y = as.factor(base_a1$target), ntree = 10)

  randomForest(y = rf4_cluster[,1], x = rf4_cluster[,-1],
               data=rf4_cluster,
               ntree=ntree, importance=TRUE, keep.forest=TRUE)

}
stopCluster(cl)

library(randomForest)
varImpPlot (rf1 , main ="")
importance (rf1)

courbeROC(rf1,base_pred$Surv1,"Random Forest")?





#///////////////////////////////#
#	MODELE LOGIT		  #
#///////////////////////////////#
