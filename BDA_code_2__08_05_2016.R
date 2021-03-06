rm(list = ls())
#setwd("C:/BDA")
setwd("C:/Users/David/Documents/ENSAE/3A/Big Data et assurance/BNP Claims Management")

library(ggplot2)
library(readr)
library(xgboost)
library(caret)
library(e1071)
library(plyr)

#On lit les bases de train et de test
train_raw = read_csv("train.csv")
test_raw = read_csv("test.csv")

#On cr�e un vecteur contenant la variable 0-1 target
y = train_raw[, "target"]

#On s�pare les id suivant s'ils viennent de train ou de test
id_train = train_raw[,"ID"]
id_test = test_raw[,"ID"]

# On va enlever les variables target et ID de train_raw et test_raw
train_raw = train_raw[, !names(train_raw) %in% c("ID","target")]
test_raw = test_raw[, !names(test_raw) %in% c("ID")]

data = rbind(train_raw,test_raw)

#On cr�e 2 nouvelles variables qui vont �tre �gales � la 1ere lettre de v56 pour l'une et � la 2eme pour l'autre
data$v53_1 = sapply(data$v56, function(x) strsplit(x,NULL)[[1]][1])
data$v53_2 = sapply(data$v56, function(x) strsplit(x,NULL)[[1]][2])

# On met les NA � -1
data[is.na(data)] = -1


# On convertit les variables cat�gorielles en num�riques
for(i in 1:ncol(data)){
  if(is.character(data[,i])){
    data[,i] = as.numeric(as.factor(data[,i]))
  }
}

#On regarde le nombre de NA dans data
data$count_na = apply(data, 1, function(x) -sum(x[x==-1]))

train_final = cbind(data[1:nrow(train_raw),],y)
test_final = data[(nrow(train_raw)+1):nrow(data),]

bound = floor((nrow(train_final)/5)*4)
train_final = train_final[sample(nrow(train_final)),]

#on s�pare la base de train en une sous-base de train et une sous base de test, avec une proportion de 4/5-1/5 
train = train_final[1:bound,] 
test = train_final[(bound+1):nrow(train_final),]

y_train = as.factor(train[,"y"])
y_val = as.factor(test[,"y"])
train[,"y"] = NULL
test[,"y"] = NULL

#---------------------------------------------------------------------#
# On va essayer de faire autant de mod�les que de variables 
# pour voir quelles sont celles avec le meilleur pouvoir explicatif
#---------------------------------------------------------------------#

LogLoss = function(actual, predicted, eps=0.00001){
  predicted = pmin(pmax(predicted, eps), 1-eps)
  -1/length(actual)*(sum(actual*log(predicted)+(1-actual)*log(1-predicted)))
}

#On va cr�er une matrice dont la premi�re ligne va �tre le num�ro de la vavriable test�e,
#et la seconde ligne le score associ�e
ess1var = matrix(-1, ncol = ncol(data), nrow = 2)
ess1var[1,] = names(data)
for ( i in 1:(ncol(data))){ # Cela va aussi prendre comme variable explicative la  variable count_na 
  train_ess1var = train[,i]
  test_ess1var = test[,i]
  
  #On cr�e un objet recapitulant les param�tres de la cross validation que nous allons faire
  CV_control_ess1var = trainControl(method = "cv",
                            number = 2,
                            repeats = 1,
                            verboseIter = TRUE,
                            allowParallel = TRUE,
                            returnResamp = "all",
                            preProcOptions = list(thresh = 0.999,ICAcomp = 111))
  
  #On va cr�er le tableau de toutes les combinaisons de param�tres � parcourir
  CV_grid_ess1var = expand.grid(nrounds = 10*(1:50),
                        eta = c(0.05),
                        max_depth = c(8),
                        min_child_weight = c(4),
                        colsample_bytree =0.8,
                        gamma = 1)
  
  
  xgbTree_ess1var = train(x = train_ess1var,
                  y = y_train,
                  method = "xgbTree",
                  tuneGrid = CV_grid_ess1var,                            
                  trControl = CV_control_ess1var,
                  missing = -1,
                  eta_decay = 0.997,
                  eval_metric = "logloss",
                  objective = "binary:logistic",
                  base_score = 0.5,
                  nthread = 7)
  
  pred_ess1var = predict(xgbTree_ess1var,test_ess1var,type = "prob")
  
  
  ess1var[2,i] = LogLoss(as.numeric(y_val)-1,as.numeric(pred_ess1var[,2])-1)
}

length(unique(ess1var[2,])) == length(ess1var[2,])

# On va trier les variables selon leur loogloss
ess1var_tri = matrix(-1, ncol = ncol(data), nrow = 2)
ess1var_tri[2,] = sort(as.numeric(ess1var[2,]))
for (k in 1:ncol(ess1var)){
  loss_tri = ess1var_tri[2,k]
  ind_tri = which(ess1var[2,] == loss_tri)[1] # au cas o� plusieurs variables auraient la m�me loss
  
  ess1var_tri[1,k] = ind_tri
}

ess1var_tri


#On cr�e un objet recapitulant les param�tres de la cross validation que nous allons faire
CV_control = trainControl(method = "cv",
                          number = 2,
                          repeats = 1,
                          verboseIter = TRUE,
                          allowParallel = TRUE,
                          returnResamp = "all",
                          preProcOptions = list(thresh = 0.999,ICAcomp = 111))

#On va cr�er le tableau de toutes les combinaisons de param�tres � parcourir
CV_grid = expand.grid(nrounds = 10*(1:50),
                      eta = c(0.05),
                      max_depth = c(8),
                      min_child_weight = c(4),
                      colsample_bytree =0.8,
                      gamma = 1)


xgbTree = train(x = train,
                y = y_train,
                method = "xgbTree",
                tuneGrid = CV_grid,                            
                trControl = CV_control,
                missing = -1,
                eta_decay = 0.997,
                eval_metric = "logloss",
                objective = "binary:logistic",
                base_score = 0.5,
                nthread = 7)

plot(xgbTree)

pred = predict(xgbTree,test,type = "prob")



LogLoss(as.numeric(y_val)-1,as.numeric(pred[,2])-1)

submission = read_csv("sample_submission.csv")
submission$PredictedProb = pred[,2]

write.csv(submission, "submission.csv", row.names=F, quote=F)

c<-cor(train_final)

install.packages("xlsx")
library(xlsx)
write.xlsx(c,"C:/Users/David/Documents/ENSAE/3A/Big Data et assurance/BNP Claims Management/corra.xlsx")

install.packages("corrplot")
library(corrplot)
corrplot(c, type="upper", order="hclust", tl.col="black", tl.srt=45)

col<- colorRampPalette(c("blue", "white", "red"))(20)
cormat<-rquery.cormat(c, type="full", col=col)



