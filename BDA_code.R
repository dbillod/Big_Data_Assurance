rm(list = ls())
setwd("C:/BDA")

library(ggplot2)
library(readr)
library(xgboost)

train_raw = read_csv("train.csv")
test_raw = read_csv("test.csv")

y = train_raw[, "target"]

id_train = train_raw[,"ID"]
id_train = test_raw[,"ID"]

train_raw = train_raw[, !names(train_raw) %in% c("ID","target")]
test_raw = test_raw[, !names(test_raw) %in% c("ID")]



data = rbind(train_raw,test_raw)

# Find factor variables and translate to numeric

for(i in 1:ncol(data)){
  if(is.character(data[,i])){
    data[,i] = as.numeric(as.factor(data[,i]))
  }
}

min(data,na.rm = T)

data[is.na(data)] = -1

train = data[1:nrow(train_raw),]
test = data[(nrow(train_raw)+1):nrow(data),]


doTest = function(y, train, test, param0) {
  n = nrow(train)
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

param0 = list("objective"  = "binary:logistic",
              "eval_metric" = "logloss",
              "eta" = 0.02,
              "missing" = -1,
              "subsample" = 0.68,
              "colsample_bytree" = 0.78,
              "min_child_weight" = 4,
              "max_depth" = 6,
              "n_estimators" = 1000)

# total analysis
submission = read_csv("sample_submission.csv")
ensemble = rep(0, nrow(test))
# change to 1:5 to get result
for (i in 1:1) {
  p = doTest(y, train, test, param0) 
  ensemble = ensemble + p
}
submission$PredictedProb = ensemble/i
write.csv(submission, "submission2.csv", row.names=F, quote=F)
