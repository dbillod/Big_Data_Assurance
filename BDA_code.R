rm(list = ls())
#setwd("C:/BDA")
setwd("C:/Users/David/Documents/ENSAE/3A/Big Data et assurance/BNP Claims Management")
library(ggplot2)
library(readr)
library(xgboost)
library(cvTools)


train_raw = read_csv("train.csv")
test_raw = read_csv("test.csv")

y = train_raw[, "target"]

id_train = train_raw[,"ID"]
id_train = test_raw[,"ID"]

## On va enlever les variables target et ID de train_raw et test_raw
train_raw = train_raw[, !names(train_raw) %in% c("ID","target")]
test_raw = test_raw[, !names(test_raw) %in% c("ID")]


#On reconstitue nos données en fusionnant train et test
data = rbind(train_raw,test_raw)

# Find factor variables and translate to numeric

for(i in 1:ncol(data)){
  if(is.character(data[,i])){
    data[,i] = as.numeric(as.factor(data[,i]))
  }
}

# On regarde la plus faible valeur de data.
# Comme elle est très proche de 0 et assez loin de -1, on va mettre -1 aux NA
min(data,na.rm = T)
data[is.na(data)] = -1

# Comme on a traité nos données, on les re-sépare
train = data[1:nrow(train_raw),]
test = data[(nrow(train_raw)+1):nrow(data),]



# On crée une fonction qui retourne un vecteur de prédictions
# On fait pour cela du gradient boosting
doTest = function(y, train, test, param0) {
  n = nrow(train)
  # Le traitement en xgb.DMatrix permet d'optimiser le stockage mémoire
  xgtrain = xgb.DMatrix(as.matrix(train), label = y) 
  xgval = xgb.DMatrix(as.matrix(test))
  watchlist = list('train' = xgtrain)
  model = xgb.train(params = param0,
                    data = xgtrain,
                    watchlist = watchlist,
                    print.every.n = 100,
                    nthread = 4)
  pred = predict(model, xgval)
  return(pred)
}

param0 = list("objective"  = "binary:logistic", # comme on veut prédire un 0-1, on utilise une regression logistique pour classification
              "eval_metric" = "logloss", # définit une fonction de perte
              "eta" = 0.02, # tux d'apprentissage
              "missing" = -1, #indique la valeur mise aux NA
              "subsample" = 0.68,
              "colsample_bytree" = 0.78,
              "min_child_weight" = 4,
              "max_depth" = 6, #profondeur maximale des arbres
              "n_estimators" = 1000)



# On veut faire une cross-validation

# On définit à 3 le nombre de sous-bases de cross_validation
k_cv = 3

# On va récupérer les indices qui vont être les lindividus des 3 bases

ind = cvFolds(nrow(train), K = k_cv)
mat_ind = matrix(ncol = 2 ,  nrow = length(ind$which))
mat_ind[,1] = ind$which
mat_ind[,2] = ind$subsets

ind_fin1 = mat_ind[which(mat_ind[,1]==1),2]
ind_fin2 = mat_ind[which(mat_ind[,1]==2),2]
ind_fin3 = mat_ind[which(mat_ind[,1]==3),2]
 
length(ind_fin1) + length(ind_fin2)+length(ind_fin3)
nrow(train)

base_cv1 = train[ind_fin1,]
base_cv2 = train[ind_fin2,]
base_cv3 = train[ind_fin3,]





# total analysis
# On récupère le fichier à soumettre pour kaggle
submission = read_csv("sample_submission.csv") 
ensemble = rep(0, nrow(test))

# change to 1:5 to get result
# Cette boucle permet de moyenner les prédictions en faisant tourner plusieurs fois la fonction doTest
for (i in 1:1) {
  p = doTest(y, train, test, param0) 
  ensemble = ensemble + p
}
submission$PredictedProb = ensemble/i

# On écrit le fichier à soumettre
write.csv(submission, "submission2.csv", row.names=F, quote=F)



