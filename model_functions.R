
#install.packages("doMC", repos="http://R-Forge.R-project.org")

#Logistic Model
logisticChurn = function(train=trainingRed, test = testingRed, predictors = predictorsRed, rocChartName="ROC Curve")
{
  set.seed(715)
  lr = train(x = train[,predictors]
                 ,y = train$churn
                 ,method='glm'
                 ,trControl=fitControl
                 ,metric = "ROC")
  summary(lr)
  
  #PREDICT ON TEST SET 
  lrTestPred = predict(lr, test, type = "prob")
  test$logProb = lrTestPred[,"yes"]
  test$logClass = predict(lr, test)
  
  #Confustion Matrix
  confMatrix = confusionMatrix(data = test$logClass,
                  reference = test$churn, 
                  positive = "yes")
  
  rocCurveTest = roc(response = test$churn,
                  predictor = test$logProb)
  
  rocPlotTest = plot.roc(rocCurveTest, legacy.axes = TRUE
       ,print.thres = c(.5,.25,.15), type = "S"
       ,print.thres.pattern = "%.3f (Spec = %.2f, Sens = %.2f)"
       ,print.thres.cex = .75
       ,main = paste("Test ",   rocChartName)
       #,xlim = c(0.0,1.0)
  )
  
  rocCurveTrain = roc(response = train$churn,
                     predictor = predict(lr, train, type = "prob")[,"yes"])
  
  rocPlotTrain = plot.roc(rocCurveTrain, legacy.axes = TRUE
                 ,print.thres = c(.5,.25,.15), type = "S"
                 ,print.thres.pattern = "%.3f (Spec = %.2f, Sens = %.2f)"
                 ,print.thres.cex = .75
                 ,main = paste("Train ",   rocChartName)
                 )
  
  YoudenLogTresh = coords(rocCurveTrain,x = "best", best.method = "closest.topleft")
  
  list(rocCurveTest = rocCurveTest, rocCurveTrain = rocCurveTrain, YoudensIndex = YoudenLogTresh, logisticModel = (lr), rocCurvePlotTrain = rocPlotTrain, rocCurvePlotTest = rocPlotTest, confMat = confMatrix)
}



#LDA Model

ldaChurn = function(train=trainingRed, test = testingRed, predictors = predictorsRed, rocChartName="ROC Curve")
{
  set.seed(715)
  ldaModel = train(churn~.,
                 data = train
                 ,method="lda"
                 ,trControl=fitControl
                 ,metric = "ROC"
                 ,preProcess=c('scale', 'center'))

  ldaTestPred = predict(ldaModel, test, type = "prob")
  test$ldaProb = ldaTestPred[,"yes"]
  test$ldaClass = predict(ldaModel, test)
  train$ldaProb = predict(ldaModel, train, type = "prob")[,"yes"]
  #default cutoff
  confMatrix = confusionMatrix(data = test$ldaClass, reference = test$churn, positive = "yes")
  
  rocCurveTest = roc(response = test$churn,
                 predictor = test$ldaProb)
  
  rocPlotTest = plot(rocCurveTest, legacy.axes = TRUE
                 ,print.thres = c(.5,.25,.15), type = "S"
                 ,print.thres.pattern = "%.3f (Spec = %.2f, Sens = %.2f)"
                 ,print.thres.cex = .75
                 ,main = paste("Test ",   rocChartName)
                 #,xlim = c(0.0,1.0)
                 )
  
  rocCurveTrain = roc(response = train$churn,
                     predictor = train$ldaProb)
  
  rocPlotTrain = plot(rocCurveTrain, legacy.axes = TRUE
                     ,print.thres = c(.5,.25,.15), type = "S"
                     ,print.thres.pattern = "%.3f (Spec = %.2f, Sens = %.2f)"
                     ,print.thres.cex = .75
                     ,main = paste("Train ",   rocChartName)
                     #,xlim = c(0.0,1.0)
  )
  
  
  YoudenLogTresh = coords(rocCurveTrain,x = "best", best.method = "closest.topleft")
  
  list(rocCurveTest = rocCurveTest, rocCurveTrain = rocCurveTrain, YoudensIndex = YoudenLogTresh, LDAModel = (ldaModel), rocCurvePlotTrain = rocPlotTrain,rocCurvePlotTest = rocPlotTest, confMat = confMatrix)

}


#QDA Model

qdaChurn = function(train=trainingRed, test = testingRed, predictors = predictorsRed, rocChartName="ROC Curve")
{
  set.seed(715)
  qdaModel = train(churn~.
                 ,data = train
                 ,method="qda"
                 ,trControl=fitControl
                 ,metric = "ROC"
                 ,preProcess=c('scale', 'center'))
  
  qdaTestPred = predict(qdaModel, test, type = "prob")
  test$qdaProb = qdaTestPred[,"yes"]
  test$qdaClass = predict(qdaModel, test)
  confMatrix = confusionMatrix(data = test$qdaClass, reference = test$churn, positive = "yes")
  train$qdaProb = predict(qdaModel, train, type = "prob")[,"yes"]
  #QDA ROC 
  rocCurveTest = roc(test$churn, test$qdaProb)
  #PLot QDA ROC 
  rocPlotTest = plot(rocCurveTest, legacy.axes = TRUE
                 ,print.thres = c(.5,.25,.15), type = "S"
                 ,print.thres.pattern = "%.3f (Spec = %.2f, Sens = %.2f)"
                 ,print.thres.cex = .75
                 ,main = paste("Test ",   rocChartName)
                 #,xlim = c(0.0,1.0)
  )
  
  #QDA ROC 
  rocCurveTrain = roc(train$churn, train$qdaProb)
  #PLot QDA ROC 
  rocPlotTrain = plot(rocCurveTrain, legacy.axes = TRUE
                 ,print.thres = c(.5,.25,.15), type = "S"
                 ,print.thres.pattern = "%.3f (Spec = %.2f, Sens = %.2f)"
                 ,print.thres.cex = .75
                 ,main = paste("Train ",   rocChartName)
                 #,xlim = c(0.0,1.0)
  )
  
  YoudenLogTresh = coords(rocCurveTrain,x = "best", best.method = "closest.topleft")
  
  list(rocCurveTest = rocCurveTest, rocCurveTrain = rocCurveTrain, YoudensIndex = YoudenLogTresh, QDAModel = (qdaModel), rocCurvePlotTrain = rocPlotTrain,rocCurvePlotTest = rocPlotTest, confMat = confMatrix)

}

randomForestChurn = function(train=trainingRed, test = testingRed, predictors = predictorsRed, rocChartName="ROC Curve")
{
  
  set.seed(715)
  library(parallel)
  library(doMC)
  
  numCores <- detectCores()
  registerDoMC(cores = numCores)
  
  mtry = floor(sqrt(ncol(train))) + 1
  
  rfMod <- train(churn~.
                 , data=train
                 , method="rf"
                 , metric="ROC"
                 , tuneGrid= data.frame(mtry = c(mtry-1,mtry,mtry+1))
                 , trControl=fitControl)
  test$rfPred = predict(rfMod, test)
  
  confMatrix = confusionMatrix(data = test$rfPred,
                  reference = test$churn, 
                  positive = "yes")
  
  #ROC 
  rocCurveTest = roc(test$churn, 
            predict(rfMod, test, type = "prob")[ , "yes"],
            levels = (levels(test$churn)))
  #Plot ROC 
  rocPlotTest = plot(rocCurveTest, print.thres = c(.5), type = "S"
                ,print.thres.pattern = "%.3f (Spec = %.2f, Sens = %.2f)"
                ,print.thres.cex = .8
                ,legacy.axes = TRUE
                ,main = paste("Test ",   rocChartName)
                )
  #ROC 
  rocCurveTrain = roc(train$churn, 
                     predict(rfMod, train, type = "prob")[ , "yes"],
                     levels = (levels(train$churn)))
  #Plot ROC 
  rocPlotTrain = plot(rocCurveTrain, print.thres = c(.5), type = "S"
                     ,print.thres.pattern = "%.3f (Spec = %.2f, Sens = %.2f)"
                     ,print.thres.cex = .8
                     ,legacy.axes = TRUE
                     ,main = paste("Train ",   rocChartName)
                     )
  
  #Variable Importance Metrics 
  varImportance = varImp(rfMod)
  varImportPlot = plot(varImportance)
  
  YoudenLogTresh = coords(rocCurveTrain,x = "best", best.method = "closest.topleft")
  
  list(rocCurveTest = rocCurveTest, rocCurveTrain = rocCurveTrain, YoudensIndex = YoudenLogTresh, RFModel = rfMod, rocCurvePlotTrain = rocPlotTrain, rocCurvePlotTest = rocPlotTest, confMat = confMatrix, varImportance = varImportance, varImpPlot = varImportPlot)
}


treeChurn = function(train=trainingRed, test = testingRed, predictors = predictorsRed, rocChartName="ROC Curve")
{

set.seed(715)
tree_churn = tree(churn~., data = train)

#PRune TREE 
#prune the tree
set.seed(725)
cv.tree.churn = cv.tree(tree_churn, FUN = prune.misclass)
cv.tree.churn
minSD = min(cv.tree.churn$dev)
minSDTreeSize = which(cv.tree.churn$dev == minSD)
bestTreeSize = min(cv.tree.churn$size[minSDTreeSize])

# par(mfrow=c(1,2)) #i want to plot size vs deviance 
# plot(cv.tree.churn$size, cv.tree.churn$dev,type="b") 
# plot(cv.tree.churn$k, cv.tree.churn$dev,type="b") #best tree size is 8

#build best tree based on prunning 
prune.churn.tree = prune.misclass(tree_churn, best = bestTreeSize)
plot(prune.churn.tree, type = "uniform")
text(prune.churn.tree, pretty=0)

#predictions on test data 
tree.pred = predict(prune.churn.tree, newdata = test)
tree.pred.class = predict(prune.churn.tree, newdata = test, type = "class")
tree.pred.train = predict(prune.churn.tree, newdata = train)
#confusion matrix
confMatrix = confusionMatrix(tree.pred.class,test$churn, positive = "yes" )

#ROC 
rocCurveTest = roc(test$churn, 
               tree.pred[,2],
               levels = rev(levels(test$churn)))

#Plot ROC 
rocPlotTest = plot(rocCurveTest, print.thres = c(.5), type = "S"
               ,print.thres.pattern = "%.3f (Spec = %.2f, Sens = %.2f)"
               ,print.thres.cex = .8
               ,legacy.axes = TRUE
               ,main = paste("Test ",   rocChartName)
               )

rocCurveTrain = roc(train$churn, 
                    tree.pred.train[,2],
                    levels = rev(levels(train$churn)))

#Plot ROC 
rocPlotTrain = plot(rocCurveTrain, print.thres = c(.5), type = "S"
                   ,print.thres.pattern = "%.3f (Spec = %.2f, Sens = %.2f)"
                   ,print.thres.cex = .8 
                   ,legacy.axes = TRUE
                   ,main = paste("Train ",   rocChartName)
                   )


YoudenLogTresh = coords(rocCurveTrain,x = "best", best.method = "closest.topleft")

list(rocCurveTest = rocCurveTest
     , rocCurveTrain = rocCurveTrain
     , YoudensIndex = YoudenLogTresh
     , prunedTree = prune.churn.tree
     , rocCurvePlotTrain = rocPlotTrain
     , rocCurvePlotTest = rocPlotTest
     , confMat = confMatrix
    )
}


caretEnsembleChurn = function(train=trainingRed, test = testingRed, predictors = predictorsRed, rocChartName="ROC Curve")
{

  library(parallel)
  library(doMC)

  numCores <- detectCores()
  registerDoMC(cores = numCores)

  # Model
  rf_model <- randomForest(churn ~ . , data = train, importance = TRUE)

  # Perform predictions on the validation set (20% of training data)
  rf_pred <- as.factor(predict(rf_model, test))

  rf_conf_mat <- table(pred = rf_pred, true = test$churn)

  # Results
  print(rf_model)

  cat("\n", "RF Sensitivity Validation:", sensitivity(rf_conf_mat, positive = "yes"), "\n")


  control <- trainControl(method = "repeatedcv", number = 10, repeats = 3, search = "grid", savePredictions = "final", index = createResample(trainingRed$churn, 10), summaryFunction = twoClassSummary, classProbs = TRUE, verboseIter = TRUE)

  # List of algorithms to use in ensemble
  alg_list <- c("rf", "glm", "lda","qda","rpart")

  multi_mod <- caretList((churn)  ~ . , data = train, trControl = control, methodList = alg_list, metric = "ROC")

  # Results
  res <- resamples(multi_mod)

  summary(res)

  # Stack
  stackControl <- trainControl(method = "repeatedcv", number = 10, repeats = 3, savePredictions = TRUE, classProbs = TRUE, verboseIter = TRUE)

  stack <- caretStack(multi_mod, method = "rf", metric = "Accuracy", trControl = stackControl)

  # Predict
  #stack_val_preds <- data.frame(predict(stack, testingRed, type = "prob"))
  stack_test_preds <- data.frame(predict(stack, test, type = "prob"))
  stack_test_class = predict(stack, test)
  stackConfMat = confusionMatrix(data = (stack_test_class), reference = test$churn, positive = "yes" )

  model_preds_test = lapply(multi_mod, predict, newdata = test, type='prob')
  model_preds_test = lapply(model_preds_test, function(x) x[,'yes'])
  model_preds_test = data.frame(model_preds_test)

  aucPlot = caTools::colAUC(model_preds_test, test$churn, plot = TRUE)


  list(rfModelConfMatrix = rf_conf_mat
       , stackConfMatrix = stackConfMat
       , AUCPlot = aucPlot
       , stackModel = stack
       , ModelsPredTest = model_preds_test
  )

}