library(randomForest)
CV_control_rf = trainControl(method = "cv",
                          number = 2,
                          repeats = 1,
                          verboseIter = TRUE,
                          allowParallel = TRUE,
                          returnResamp = "all",
                          preProcOptions = list(thresh = 0.999,ICAcomp = 111))

CV_grid_rf = expand.grid(mtry=c(30))


rf1 = train(x = train,
                y = y_train,
                method = "rf",
                tuneGrid = CV_grid_rf,                            
                trControl = CV_control_rf,
                missing = -1,
                eta_decay = 0.997,
                eval_metric = "logloss",
                objective = "binary:logistic",
                base_score = 0.5,
                nthread = 7)

plot(rf1)?

pred_rf = predict(rf1,test,type = "prob")


LogLoss(as.numeric(y_val)-1,as.numeric(pred_rf[,2]))

