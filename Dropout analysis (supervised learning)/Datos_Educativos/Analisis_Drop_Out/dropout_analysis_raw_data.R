### Description of project ###

# We have a serie of features about students in a MOOC and we want
# to predict the dropout of a student.

# For this purpose, we train differents models and comparing them with some metrics
# like accuracy, kappa, precision, recall, f1score.

# In general way, we have done the next steps:
# 1) Analyze data (distribution)
# 2) Feature selection (using Recursive Feature Elimination)
# 3) Split data in train and test
# 4) Train models (svm, random forest, decision tree, knn, logistic regression)
# 5) Test every model
# 6) Get metrics from every models
# 7) Compare metrics between every models

# Set working directory
setwd("/home/leo/Escritorio/magister_uach/MAD/own_projects/Dropout analysis (supervised learning)/Datos_Educativos/Analisis_Drop_Out/")

### Function for get metrics ###

get_model_metrics = function(cm, model) {
  
  accuracy = as.numeric(cm$overall["Accuracy"])
  kappa = as.numeric(cm$overall["Kappa"])
  recall = cm$table[1,1]/(cm$table[1,1]+cm$table[2,1])
  precision = cm$table[1,1]/(cm$table[1,1]+cm$table[1,2])
  f1score = 2*(precision*recall)/(precision+recall)
  
  return(c(model, accuracy, kappa, recall, precision, f1score))
  
}

### Function for get metrics ###

# Read csv file
# dataset from raw data
file_name = "dataset_procesado.csv"
df <- read.csv(file_name, sep = ';')

### Data visualization ###

library(ggplot2)

# distribution of target in raw data
ggplot(df, aes(x = Dropout)) + 
  geom_histogram(binwidth = 0.5) +
  ggtitle('Dropout distribution')

# Libraries for visualizaion
library(purrr)
library(tidyr)
library(ggplot2)

# plot density of all variables
# the first
df[1:13] %>%
  keep(is.numeric) %>% 
  gather() %>% 
  ggplot(aes(value)) +
  facet_wrap(~ key, scales = "free") +
  geom_density() +
  theme(axis.text.x = element_text(size = 8, angle = 30))
  # theme(axis.text.x = element_text(angle = 90, hjust = 1))

# the last
df[14:26] %>%
  keep(is.numeric) %>% 
  gather() %>% 
  ggplot(aes(value)) +
  facet_wrap(~ key, scales = "free") +
  geom_density() +
  theme(axis.text.x = element_text(size = 8, angle = 30))

# plot of boxplot of all variables
ggplot(stack(df[1:8]), aes(x = ind, y = values)) +
  geom_boxplot()  +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(stack(df[9:25]), aes(x = ind, y = values)) +
  geom_boxplot()  +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(stack(df[26]), aes(x = ind, y = values)) +
  geom_boxplot()  +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


# MAYBE DO SOME PLOTS FOR ANALYZE DISTRIBUTION AND MEANS AND LOOK FOR SOME TREND WITH DROUPT

# Delete student column
df$Estudiante = NULL

# dropout to categorical
df[["Dropout"]] = factor(df[["Dropout"]])

# Correlation matrix
library(corrplot)
#install.packages('corrplot')
library("dplyr")

# From raw data
#select_if(df, is.numeric)
corrplot(cor(select_if(df, is.numeric)))

### 
# From histogram (prior plot): Data is balanced with respect to dropout
###

# load library
library(caret)

### Feature selection ###

# set control options
control <- rfeControl(functions=rfFuncs, method="cv", number=10)

# run the RFE algorithm
results <- rfe(df[,1:24], df[,25],  rfeControl=control)

# plot results
plot(results, type=c("g", "o"))

# Select the selected features
df = df[, c(predictors(results), 'Dropout')]

### Feature selection ###

### Spliting data ###

# Split data into train and testing
intrain <- createDataPartition(y = df$Dropout, p= 0.7, list = FALSE)
training <- df[intrain,]
testing <- df[-intrain,]

# To categorical
# training[["Dropout"]] = factor(training[["Dropout"]])

# training control for training
trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)

### SVM ###

# train svm model with lineal kernel
svm_Linear <- train(Dropout ~., data = training, method = "svmLinear",
                    trControl=trctrl,
                    preProcess = c("center", "scale"),
                    tuneLength = 10)

# testing model
test_pred_svm <- predict(svm_Linear, newdata = testing)

# Get accuraccy (It can use accurarcy because dropout is balanced)
library(e1071)
#install.packages('e1071')
cm_svm = confusionMatrix(table(test_pred_svm, testing$Dropout))

# Stats of model
svm_Linear_stats = get_model_metrics(cm_svm, 'svm linear')

### Decistion Tree ###

# Train model
decision_tree = train(Dropout ~ ., 
                  data=training, 
                  method="rpart", 
                  trControl=trctrl,
                  preProcess = c("center", "scale"),
                  tuneLength = 10)
# testing model
test_pred_dt <- predict(decision_tree, newdata = testing)

# Get accuraccy (It can use accurarcy because dropout is balanced)
library(e1071)
#install.packages('e1071')
cm_dt = confusionMatrix(table(test_pred_dt, testing$Dropout))

# Stats of model
dt_stats = get_model_metrics(cm_dt, 'decision tree')

### Random forest ###

# DANGER
# It takes so much time, maybe it can change some parameters for speed up
random_forest = train(Dropout ~ ., 
                      data=training, 
                      method="rf", 
                      trControl=trctrl,
                      preProcess = c("center", "scale"),
                      tuneLength = 3)

# testing model
test_pred_rf <- predict(random_forest, newdata = testing)

# Get accuraccy (It can use accurarcy because dropout is balanced)
library(e1071)
#install.packages('e1071')
cm_rf = confusionMatrix(table(test_pred_rf, testing$Dropout))

# Stats of model
rf_stats = get_model_metrics(cm_rf, 'rain forest')

### Knn ###

# Train model
knn = train(Dropout ~ ., 
                      data=training, 
                      method="knn", 
                      trControl=trctrl,
                      preProcess = c("center", "scale"),
                      tuneLength = 10)

# testing model
test_pred_knn <- predict(knn, newdata = testing)

# Get accuraccy (It can use accurarcy because dropout is balanced)
library(e1071)
#install.packages('e1071')
cm_knn = confusionMatrix(table(test_pred_knn, testing$Dropout))

# Stats of model
knn_stats = get_model_metrics(cm_knn, 'knn')

### Logistic Regression ###

# Train model
logistic = train(Dropout ~ ., 
            data=training, 
            method="glm", 
            trControl=trctrl,
            preProcess = c("center", "scale"),
            tuneLength = 10)

# testing model
test_pred_logistic <- predict(logistic, newdata = testing)

# Get accuraccy (It can use accurarcy because dropout is balanced)
library(e1071)
#install.packages('e1071')
cm_logistic = confusionMatrix(table(test_pred_logistic, testing$Dropout))

# Stats of model
logistic_stats = get_model_metrics(cm_logistic, 'logistic regression')

### STATS OF MODELS ###

# Dataframe for store stats from models
stats_names = c('model','accuracy', 'kappa', 'recall', 'precision', 'f1_score')
df_stats = setNames(data.frame(matrix(ncol = length(stats_names), nrow = 0)), stats_names)

# Manually added
df_stats[1,] = svm_Linear_stats
df_stats[2,] = dt_stats
df_stats[3,] = knn_stats

# some times we haven't the stats for rf because it takes too much time
df_stats[4,] = rf_stats
df_stats[5,] = logistic_stats
