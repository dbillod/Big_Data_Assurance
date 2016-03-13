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
posVarCat=which(VariableCat==1)
posVarCat

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
, ntree = 10
, mtry = 5)
rf1



#///////////////////////////////#
#	MODELE LOGIT		  #
#///////////////////////////////#
