# Setups
setwd(dir = '/home/leo/Escritorio/magister_uach/MAD/own_projects/Homework_Student_Analysis/')

# LOAD DATA

# Load library for read excel file
library("readxl")
# Read excel file
file1 = 'attempts.csv'
df_attemps = read.csv(file1, sep=",", header = TRUE)

file2 = 'student_data.csv'
df_students = read.csv(file2, sep=",", header = TRUE)

# Remove unused variables
df_students$grp <- NULL
df_students$take_exam <- NULL
df_students$posttest <- NULL
df_students$exam <- NULL

# head(df_student)
# summary(df_student)
# head(df_student)
# View(df_student)

# DATA TRANSFORMATION

# Categorial data
df_students$social  = factor(df_students$social, labels=c('individual','social'))
df_students$gender  = factor(df_students$gender,labels=c('female','male'))
# df_students$take_exam  = factor(df_students$take_exam,labels=c('no','yes'))

# Merge data
df = merge(df_students, df_attemps, by.x = 'student', by.y = 'student')

# Aggregate data
# Separate by student and by applabel
library(plyr)
df_activity = ddply(df, .(student, applabel), summarise, n_activities = length(durationseconds))

$
# Get applabel names

# 1) 'QUIZPET'
# 2) 'ANIMATED_EXAMPLE
# 3) 'PARSONS'
# 4) 'WEBEX'
activities = unique(df_activity$applabel)

# Get each dataframe for each applabel
split_df = split(df_activity, df_activity$applabel)
df_quizpet = split_df[['QUIZPET']]
df_animated_example = split_df[['ANIMATED_EXAMPLE']]
df_parsons = split_df[['PARSONS']]
df_webex = split_df[['WEBEX']]


# Plot for display the details of the activities
# library(ggplot2)
# ggplot(df_animated_example, aes(x = applabel)) + geom_bar()

# Plot activities in semester
# ggplot(df_attemps, aes(x = relativetime, color = applabel)) + geom_density()

# Merge data for each applabel
df_agg_quizpet = merge(df_students, df_quizpet, by.x = 'student', by.y = 'student')
df_agg_animated_example = merge(df_students, df_animated_example, by.x = 'student', by.y = 'student') # 307 instances
df_agg_pasons = merge(df_students, df_parsons, by.x = 'student', by.y = 'student')
df_agg_webex = merge(df_students, df_webex, by.x = 'student', by.y = 'student')

# Remove students without complete data (remove students with NA values in some variable) for each applabel
df_qp = df_agg_quizpet[complete.cases(df_agg_quizpet), ] # Remove 85 instances 
df_ae = df_agg_animated_example[complete.cases(df_agg_animated_example), ]  # Remove 74 instances (233 keep)
df_pa = df_agg_pasons[complete.cases(df_agg_pasons), ]
df_wb = df_agg_webex[complete.cases(df_agg_webex), ]

# Remove student because it is just and id
df_qp$student <- NULL
df_ae$student <- NULL
df_pa$student <- NULL
df_wb$student <- NULL

# Remove applabel because is the same for each dataframe
df_qp$applabel <- NULL
df_ae$applabel <- NULL
df_pa$applabel <- NULL
df_wb$applabel <- NULL


# variables:
# 1. "social"
# 2. "gender"
# 3. "pretest"    
# 6. "Fi"       
# 7 "CBi"
# 8. "Vi"
# 9. "MApi"
# 10. "PApi"
# 11. "n_activites"

# categorical_var = c('social','gender')
# IV 
dep_var = 'n_activities'

# DV
ind_var = c("social","gender","pretest", "Fi", "CBi", "Vi", "MApi", "PApi")
ind_var_num = c("pretest", "Fi", "CBi", "Vi", "MApi", "PApi")
ind_var_cat = c("social","gender")

# DATA DESCRIPTION


# load library for plotting
library(ggplot2)
library(purrr)
library(tidyr)

# DESCRIBE DV (Dependent variable)

# df_qp
# df_ae
# df_pa
# df_wb


# Choose df

df = df_qp



# Distribution of numeric data (DV)
df[dep_var] %>%
  keep(is.numeric) %>%
  gather() %>% 
  ggplot(aes(value)) +
  facet_wrap(~ key, scales = "free") +
  # geom_histogram()
  geom_density()

# DESCRIBE IV (Independent variables)

# Distribution of numeric data (IV)
df[ind_var_num] %>%
  keep(is.numeric) %>%
  gather() %>% 
  ggplot(aes(value)) +
  facet_wrap(~ key, scales = "free") +
  # geom_histogram()
  geom_density() +
  theme(
    axis.text=element_text(size=6),
    # axis.text.x = element_text(angle = 90, hjust = 1)  
  )

# Counting of categorical data
df[,ind_var_cat] %>%
  # keep(is.not.numeric) %>%
  gather() %>% 
  ggplot(aes(value)) +
  facet_wrap(~ key, scales = "free") +
  geom_histogram(stat = 'count')
# geom_count()
# geom_bar() +
theme(axis.text.x = element_text(angle = 90, hjust = 1))

# ANALYSIS OF CORRELATION BETWEEN IV

# Plot corr matrix between IV 
library(corrplot)

# Next lines are necessary for transform categorical data to numeric for plot in correlation matrix
DF = df[,ind_var]
DF[] <- lapply(df[, ind_var],as.double)
# corrplot(cor(DF))

#  Get the more independent variable by ordering the variables by the smaller sum of absolute values in corr matrix
corr_var_list = sort((colSums(abs(cor(DF)))))

# Display
View(corr_var_list)

# RELEVANCE OF FEATURES OVER THE TARGET

# Get correlation matrix including DV
DF_2 = df
DF_2[] <- lapply(df,as.double)
corrplot(cor(DF_2))

# Get the factor of corr matrix with respecto to DV ordered (considering absolute value)
corr_var_list_ind = sort(abs((cor(DF_2)[, 'n_activities'])), decreasing = TRUE)

# Display dataframe
View(corr_var_list_ind)

# Start to test the relation between DV and other IV

# 1)  'n_activities' = 'pretest'

# 1
l_m_1 <- lm(n_activities ~ Fi, data= df)
summary(l_m_1)

# 2
l_m_2 <- lm(n_activities ~ Fi + Vi, data= df)
summary(l_m_2)

# 3)  'n_activities' = 'pretest' + Fi + social
# l_m_3 <- lm(n_activities ~ Fi + Vi + social, data= df)
l_m_3 <- lm(n_activities ~ Fi + Vi + social, data= df)
summary(l_m_3)

# 4)  'n_activities' = 'pretest' + Fi + social
l_m_4 <- lm(n_activities ~ Fi + Vi + social + PApi, data= df)
summary(l_m_4)

# 4)  'n_activities' = 'pretest' + Fi + social + gender
l_m_5 <- lm(n_activities ~ Fi + Vi + social + PApi + pretest, data= df)
summary(l_m_5)



# PREDICTION

# Split in train/test data
set.seed(101) # Set Seed so that same sample can be reproduced in future also
# Now Selecting 70% of data as sample from total 'n' rows of the data  
sample <- sample.int(n = nrow(df), size = floor(.70*nrow(df)), replace = F)
train <- df[sample, ]
test  <- df[-sample, ]

# Build the model on training data -
# model = l_m_5 
# model <- lm(n_activities ~ pretest + Fi + social + social*pretest, data= train)
# model <- lm(n_activities  ~ pretest + Vi + social, data= train)
model <- lm(n_activities  ~ Fi + Vi + social + PApi + pretest, data= train)
summary(model)

# Make predictions
predictions <- predict(model, test)

# Accuracy of model
cor(predictions, test$n_activities)

# COPIAR Y PEGAR CODIGO PERO AHORA CAMBIAR DF POR EL SIGUIENTE DF Y REPETIR ANALISIS

# DESCRIBE DV (Dependent variable)

# df_qp
# df_ae
# df_pa
# df_wb

# Choose df

df = df_ae



# Distribution of numeric data (DV)
df[dep_var] %>%
  keep(is.numeric) %>%
  gather() %>% 
  ggplot(aes(value)) +
  facet_wrap(~ key, scales = "free") +
  # geom_histogram()
  geom_density()

# DESCRIBE IV (Independent variables)

# Distribution of numeric data (IV)
df[ind_var_num] %>%
  keep(is.numeric) %>%
  gather() %>% 
  ggplot(aes(value)) +
  facet_wrap(~ key, scales = "free") +
  # geom_histogram()
  geom_density() +
  theme(
    axis.text=element_text(size=6),
    # axis.text.x = element_text(angle = 90, hjust = 1)  
  )

# Counting of categorical data
df[,ind_var_cat] %>%
  # keep(is.not.numeric) %>%
  gather() %>% 
  ggplot(aes(value)) +
  facet_wrap(~ key, scales = "free") +
  geom_histogram(stat = 'count')
# geom_count()
# geom_bar() +
theme(axis.text.x = element_text(angle = 90, hjust = 1))

# ANALYSIS OF CORRELATION BETWEEN IV

# Plot corr matrix between IV 
library(corrplot)

# Next lines are necessary for transform categorical data to numeric for plot in correlation matrix
DF = df[,ind_var]
DF[] <- lapply(df[, ind_var],as.double)
# corrplot(cor(DF))

#  Get the more independent variable by ordering the variables by the smaller sum of absolute values in corr matrix
corr_var_list = sort((colSums(abs(cor(DF)))))

# Display
View(corr_var_list)

# RELEVANCE OF FEATURES OVER THE TARGET

# Get correlation matrix including DV
DF_2 = df
DF_2[] <- lapply(df,as.double)
corrplot(cor(DF_2))

# Get the factor of corr matrix with respecto to DV ordered (considering absolute value)
corr_var_list_ind = sort(abs((cor(DF_2)[, 'n_activities'])), decreasing = TRUE)

# Display dataframe
View(corr_var_list_ind)

# Start to test the relation between DV and other IV

# 1)  'n_activities' = 'pretest'

# 1
l_m_1 <- lm(n_activities ~ + pretest + social + Fi + CBi, data= df)
summary(l_m_1)

# # 2
# l_m_2 <- lm(n_activities ~ Fi + Vi, data= df)
# summary(l_m_2)
# 
# # 3)  'n_activities' = 'pretest' + Fi + social
# # l_m_3 <- lm(n_activities ~ Fi + Vi + social, data= df)
# l_m_3 <- lm(n_activities ~ Fi + Vi + social, data= df)
# summary(l_m_3)
# 
# # 4)  'n_activities' = 'pretest' + Fi + social
# l_m_4 <- lm(n_activities ~ Fi + Vi + social + PApi, data= df)
# summary(l_m_4)
# 
# # 4)  'n_activities' = 'pretest' + Fi + social + gender
# l_m_5 <- lm(n_activities ~ Fi + Vi + social + PApi + pretest, data= df)
# summary(l_m_5)



# PREDICTION

# Split in train/test data
set.seed(101) # Set Seed so that same sample can be reproduced in future also
# Now Selecting 70% of data as sample from total 'n' rows of the data  
sample <- sample.int(n = nrow(df), size = floor(.70*nrow(df)), replace = F)
train <- df[sample, ]
test  <- df[-sample, ]

# Build the model on training data -
# model = l_m_5 
# model <- lm(n_activities ~ pretest + Fi + social + social*pretest, data= train)
# model <- lm(n_activities  ~ pretest + Vi + social, data= train)
model <- lm(n_activities ~ + pretest + social + Fi + CBi, data= train)
summary(model)

# Make predictions
predictions <- predict(model, test)

# Accuracy of model
cor(predictions, test$n_activities)


# df_qp
# df_ae
# df_pa
# df_wb

# Choose df

df = df_pa



# Distribution of numeric data (DV)
df[dep_var] %>%
  keep(is.numeric) %>%
  gather() %>% 
  ggplot(aes(value)) +
  facet_wrap(~ key, scales = "free") +
  # geom_histogram()
  geom_density()

# DESCRIBE IV (Independent variables)

# Distribution of numeric data (IV)
df[ind_var_num] %>%
  keep(is.numeric) %>%
  gather() %>% 
  ggplot(aes(value)) +
  facet_wrap(~ key, scales = "free") +
  # geom_histogram()
  geom_density() +
  theme(
    axis.text=element_text(size=6),
    # axis.text.x = element_text(angle = 90, hjust = 1)  
  )

# Counting of categorical data
df[,ind_var_cat] %>%
  # keep(is.not.numeric) %>%
  gather() %>% 
  ggplot(aes(value)) +
  facet_wrap(~ key, scales = "free") +
  geom_histogram(stat = 'count')
# geom_count()
# geom_bar() +
theme(axis.text.x = element_text(angle = 90, hjust = 1))

# ANALYSIS OF CORRELATION BETWEEN IV

# Plot corr matrix between IV 
library(corrplot)

# Next lines are necessary for transform categorical data to numeric for plot in correlation matrix
DF = df[,ind_var]
DF[] <- lapply(df[, ind_var],as.double)
# corrplot(cor(DF))

#  Get the more independent variable by ordering the variables by the smaller sum of absolute values in corr matrix
corr_var_list = sort((colSums(abs(cor(DF)))))

# Display
View(corr_var_list)

# RELEVANCE OF FEATURES OVER THE TARGET

# Get correlation matrix including DV
DF_2 = df
DF_2[] <- lapply(df,as.double)
corrplot(cor(DF_2))

# Get the factor of corr matrix with respecto to DV ordered (considering absolute value)
corr_var_list_ind = sort(abs((cor(DF_2)[, 'n_activities'])), decreasing = TRUE)

# Display dataframe
View(corr_var_list_ind)

# Start to test the relation between DV and other IV

# 1)  'n_activities' = 'pretest'

# 1
l_m_1 <- lm(n_activities ~ + Fi + Vi + pretest + social, data= df)
summary(l_m_1)

# # 2
# l_m_2 <- lm(n_activities ~ Fi + Vi, data= df)
# summary(l_m_2)
# 
# # 3)  'n_activities' = 'pretest' + Fi + social
# # l_m_3 <- lm(n_activities ~ Fi + Vi + social, data= df)
# l_m_3 <- lm(n_activities ~ Fi + Vi + social, data= df)
# summary(l_m_3)
# 
# # 4)  'n_activities' = 'pretest' + Fi + social
# l_m_4 <- lm(n_activities ~ Fi + Vi + social + PApi, data= df)
# summary(l_m_4)
# 
# # 4)  'n_activities' = 'pretest' + Fi + social + gender
# l_m_5 <- lm(n_activities ~ Fi + Vi + social + PApi + pretest, data= df)
# summary(l_m_5)



# PREDICTION

# Split in train/test data
set.seed(101) # Set Seed so that same sample can be reproduced in future also
# Now Selecting 70% of data as sample from total 'n' rows of the data  
sample <- sample.int(n = nrow(df), size = floor(.70*nrow(df)), replace = F)
train <- df[sample, ]
test  <- df[-sample, ]

# Build the model on training data -
# model = l_m_5 
# model <- lm(n_activities ~ pretest + Fi + social + social*pretest, data= train)
# model <- lm(n_activities  ~ pretest + Vi + social, data= train)
model <- lm(n_activities ~ + Fi + Vi + pretest + social, data= train)
summary(model)

# Make predictions
predictions <- predict(model, test)

# Accuracy of model
cor(predictions, test$n_activities)

# COPIAR Y PEGAR CODIGO PERO AHORA CAMBIAR DF POR EL SIGUIENTE DF Y REPETIR ANALISIS

# df_qp
# df_ae
# df_pa
# df_wb

# Choose df

df = df_wb



# Distribution of numeric data (DV)
df[dep_var] %>%
  keep(is.numeric) %>%
  gather() %>% 
  ggplot(aes(value)) +
  facet_wrap(~ key, scales = "free") +
  # geom_histogram()
  geom_density()

# DESCRIBE IV (Independent variables)

# Distribution of numeric data (IV)
df[ind_var_num] %>%
  keep(is.numeric) %>%
  gather() %>% 
  ggplot(aes(value)) +
  facet_wrap(~ key, scales = "free") +
  # geom_histogram()
  geom_density() +
  theme(
    axis.text=element_text(size=6),
    # axis.text.x = element_text(angle = 90, hjust = 1)  
  )

# Counting of categorical data
df[,ind_var_cat] %>%
  # keep(is.not.numeric) %>%
  gather() %>% 
  ggplot(aes(value)) +
  facet_wrap(~ key, scales = "free") +
  geom_histogram(stat = 'count')
# geom_count()
# geom_bar() +
theme(axis.text.x = element_text(angle = 90, hjust = 1))

# ANALYSIS OF CORRELATION BETWEEN IV

# Plot corr matrix between IV 
library(corrplot)

# Next lines are necessary for transform categorical data to numeric for plot in correlation matrix
DF = df[,ind_var]
DF[] <- lapply(df[, ind_var],as.double)
# corrplot(cor(DF))

#  Get the more independent variable by ordering the variables by the smaller sum of absolute values in corr matrix
corr_var_list = sort((colSums(abs(cor(DF)))))

# Display
View(corr_var_list)

# RELEVANCE OF FEATURES OVER THE TARGET

# Get correlation matrix including DV
DF_2 = df
DF_2[] <- lapply(df,as.double)
corrplot(cor(DF_2))

# Get the factor of corr matrix with respecto to DV ordered (considering absolute value)
corr_var_list_ind = sort(abs((cor(DF_2)[, 'n_activities'])), decreasing = TRUE)

# Display dataframe
View(corr_var_list_ind)

# Start to test the relation between DV and other IV

# 1)  'n_activities' = 'pretest'

# 1
l_m_1 <- lm(n_activities ~ + pretest + Fi + CBi + MApi, data= df)
summary(l_m_1)

# # 2
# l_m_2 <- lm(n_activities ~ Fi + Vi, data= df)
# summary(l_m_2)
# 
# # 3)  'n_activities' = 'pretest' + Fi + social
# # l_m_3 <- lm(n_activities ~ Fi + Vi + social, data= df)
# l_m_3 <- lm(n_activities ~ Fi + Vi + social, data= df)
# summary(l_m_3)
# 
# # 4)  'n_activities' = 'pretest' + Fi + social
# l_m_4 <- lm(n_activities ~ Fi + Vi + social + PApi, data= df)
# summary(l_m_4)
# 
# # 4)  'n_activities' = 'pretest' + Fi + social + gender
# l_m_5 <- lm(n_activities ~ Fi + Vi + social + PApi + pretest, data= df)
# summary(l_m_5)



# PREDICTION

# Split in train/test data
set.seed(101) # Set Seed so that same sample can be reproduced in future also
# Now Selecting 70% of data as sample from total 'n' rows of the data  
sample <- sample.int(n = nrow(df), size = floor(.70*nrow(df)), replace = F)
train <- df[sample, ]
test  <- df[-sample, ]

# Build the model on training data -
# model = l_m_5 
# model <- lm(n_activities ~ pretest + Fi + social + social*pretest, data= train)
# model <- lm(n_activities  ~ pretest + Vi + social, data= train)
model <- lm(n_activities ~ + pretest + Fi + CBi + MApi, data= train)
summary(model)

# Make predictions
predictions <- predict(model, test)

# Accuracy of model
cor(predictions, test$n_activities)

# COPIAR Y PEGAR CODIGO PERO AHORA CAMBIAR DF POR EL SIGUIENTE DF Y REPETIR ANALISIS