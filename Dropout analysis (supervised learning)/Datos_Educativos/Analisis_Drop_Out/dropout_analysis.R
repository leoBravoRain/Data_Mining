# Set working directory
setwd("/home/leo/Escritorio/magister_uach/MAD/own_projects/Dropout analysis (supervised learning)/Datos_Educativos/Analisis_Drop_Out/")

# Read csv file
file_name = "dataset_procesado.csv"
df <- read.csv(file_name, sep = ';')

# Analyze data
ggplot(df, aes(x = Dropout)) + 
  geom_histogram(binwidth = 0.5) +
  ggtitle('Dropout distribution')

# Correlation matrix
library(corrplot)
#install.packages('corrplot')
library("dplyr")
select_if(df, is.numeric)
corrplot(cor(select_if(df, is.numeric)))
### 
# From histogram (prior plot): Data is balanced with respect to dropout
###

# load library
library(caret)

# Split data into train and testing
intrain <- createDataPartition(y = df$Dropout, p= 0.7, list = FALSE)
training <- df[intrain,]
testing <- df[-intrain,]

# To categorical
training[["Dropout"]] = factor(training[["Dropout"]])

# training control
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
confusionMatrix(table(test_pred_svm, testing$Dropout))

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
confusionMatrix(table(test_pred_dt, testing$Dropout))

### Random forest ###

# It takes so much time, maybe it can change some parameters for speed up
random_forest = train(Dropout ~ ., 
                      data=training, 
                      method="rf", 
                      trControl=trctrl,
                      preProcess = c("center", "scale"),
                      tuneLength = 10)

# testing model
test_pred_rf <- predict(random_forest, newdata = testing)

# Get accuraccy (It can use accurarcy because dropout is balanced)
library(e1071)
#install.packages('e1071')
confusionMatrix(table(test_pred_rf, testing$Dropout))

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
confusionMatrix(table(test_pred_knn, testing$Dropout))
