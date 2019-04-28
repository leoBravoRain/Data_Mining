# set.seed(7)
# load the library
library(mlbench)
library(caret)
# load the data
data(PimaIndiansDiabetes)
# calculate correlation matrix
correlationMatrix <- cor(PimaIndiansDiabetes[,1:8])
# summarize the correlation matrix
print(correlationMatrix)
# find attributes that are highly corrected (ideally >0.75)
highlyCorrelated <- findCorrelation(correlationMatrix, cutoff=0.5)
# print indexes of highly correlated attributes
print(highlyCorrelated)

# data(PimaIndiansDiabetes)
# 
df = PimaIndiansDiabetes
# 
# summary(df)
# 
# # myData <- select(myData, -B)
# 
# # Convert to dummy variables
# # df <- fastDummies::dummy_cols(df, select_columns = "diabetes")
# 
# # Correlation matrix
# corr = cor(df[, 1:8])
# 
# # hc <- findCorrelation(correlationMatrix, cutoff = 0.5, names = True)
# # print()
# 
# # control <- trainControl()

# plot <- ggplot(df, aes(x = glucose, y = insulin, color = factor(diabetes))) + geom_point()

# scat <- ggplot(df_tmp, aes(x = Agno, y = Calificacion, color = Agno)) + ggtitle(title) + geom_point()

# plot(plot)
# y = df[df$diabetes]
library(e1071)

svmfit = svm(diabetes ~ ., data = df, kernel = "linear", cost = 10, scale = FALSE)

