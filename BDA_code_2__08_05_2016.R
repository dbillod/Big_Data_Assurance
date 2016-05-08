rm(list = ls())
setwd("C:/BDA")

library(ggplot2)
library(readr)
library(xgboost)
library(caret)
library(e1071)

train_raw = read_csv("train.csv")
test_raw = read_csv("test.csv")

y = train_raw[, "target"]

id_train = train_raw[,"ID"]
id_train = test_raw[,"ID"]

train_raw = train_raw[, !names(train_raw) %in% c("ID","target")]
test_raw = test_raw[, !names(test_raw) %in% c("ID")]

data = rbind(train_raw,test_raw)

data$v53_1 = sapply(data$v56, function(x) strsplit(x,NULL)[[1]][1])
data$v53_2 = sapply(data$v56, function(x) strsplit(x,NULL)[[1]][2])

data[is.na(data)] = -1

for(i in 1:ncol(data)){
  if(is.character(data[,i])){
    data[,i] = as.numeric(as.factor(data[,i]))
  }
}

data$count_na = apply(data, 1, function(x) -sum(x[x==-1]))

train_final = cbind(data[1:nrow(train_raw),],y)
test_final = data[(nrow(train_raw)+1):nrow(data),]

bound = floor((nrow(train_final)/5)*4)
train_final = train_final[sample(nrow(train_final)),]

train = train_final[1:bound,] 
test = train_final[(bound+1):nrow(train_final),]

y_train = as.factor(train[,"y"])
y_val = as.factor(test[,"y"])
train[,"y"] = NULL
test[,"y"] = NULL

CV_control = trainControl(method = "cv",
                          number = 2,
                          repeats = 1,
                          verboseIter = TRUE,
                          allowParallel = TRUE,
                          returnResamp = "all",
                          preProcOptions = list(thresh = 0.999,ICAcomp = 111))

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

LogLoss = function(actual, predicted, eps=0.00001){
  predicted = pmin(pmax(predicted, eps), 1-eps)
  -1/length(actual)*(sum(actual*log(predicted)+(1-actual)*log(1-predicted)))
}

LogLoss(as.numeric(y_val)-1,as.numeric(pred[,2])-1)

submission = read_csv("sample_submission.csv")
submission$PredictedProb = pred[,2]

write.csv(submission, "submission.csv", row.names=F, quote=F)




