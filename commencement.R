library(ROCR)
library(Rcmdr)
library(rpart.plot)
library(rpart)
library(ipred)
library(hmeasure)
library(rattle)



base=read.csv("//paradis/eleves/BWHANNOU/productions/train.csv",sep=",")
head(base)
base[1,]

summary(base)

is.character("a")

####################################################################
############### base de données ####################################


VariableCat=rep(1,131)
for (i in 1:131){
	if (is.numeric(base[1,i+2]))
	{
		VariableCat[i]=0
	}

}
sum(VariableCat)

posVarCat=which(VariableCat==1)

# 19 variables catégorielles
# 112 variables numériques

base$target
#######################  #############################


	tableau<-table(base[,2+posVarCat[7]],base$target) 
	print(tableau)
	addmargins(tableau)
	bar <- as.table( apply(tableau, c(1,2), sum) )
	barplot(t(bar), legend.text = attr(t(bar), "dimnames")[[1]])
	prop.table(tableau,1) 

rf <-randomForest(as.factor(base$target) ~ . ,
                  data = a, ntree=100, maxnodes=200, do.trace=TRUE,importance=TRUE)


