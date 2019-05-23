library(caret)
library(randomForest)
library(kernlab)

#dataset
data(PimaIndiansDiabetes)
df = PimaIndiansDiabetes

set.seed(2);
modFit <- train(diabetes~., data=df, preProcess=c("center","scale"), trControl=trainControl(method="repeatedcv", number=10, repeats=1, classProbs=TRUE, summaryFunction = twoClassSummary), method="rf", metric="ROC")
AUC_rf = modFit$results[modFit$results$mtry == as.numeric(modFit$bestTune),]$ROC
sens_rf = modFit$results[modFit$results$mtry == as.numeric(modFit$bestTune),]$Sens
spec_rf = modFit$results[modFit$results$mtry == as.numeric(modFit$bestTune),]$Spec
cm = confusionMatrix(modFit$finalModel$predicted, modFit$finalModel$y)
acc_rf = as.numeric(cm$overall["Accuracy"])
kappa_rf = as.numeric(cm$overall["Kappa"])
recall_rf = cm$table[1,1]/(cm$table[1,1]+cm$table[2,1])
precision_rf = cm$table[1,1]/(cm$table[1,1]+cm$table[1,2])
f1score_rf = 2*(precision_rf*recall_rf)/(precision_rf+recall_rf)


#ROC Curve
#install.packages("pROC")
library(pROC)
ctrl <- trainControl(method="cv", 
                     summaryFunction=twoClassSummary, 
                     classProbs=T,
                     savePredictions = T)
rfFit <- train(diabetes ~ ., data=df, 
               method="rf", preProc=c("center", "scale"), 
               trControl=ctrl)
# Plot:
plot.roc(rfFit$pred$obs,
         rfFit$pred$mtry)

