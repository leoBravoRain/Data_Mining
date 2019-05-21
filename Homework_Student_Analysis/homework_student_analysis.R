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

# head(df_student)
# summary(df_student)
# head(df_student)
# View(df_student)

# DATA TRANSFORMATION

# Categorial data
df_students$social  = factor(df_students$social, labels=c('individual','social'))
df_students$gender  = factor(df_students$gender,labels=c('female','male'))
df_students$take_exam  = factor(df_students$take_exam,labels=c('no','yes'))

# Merge data
df = merge(df_students, df_attemps, by.x = 'student', by.y = 'student')

# Aggregate data
# For each student add the total time in activities per semester and add n_activities per semester
# This is not considering the differents activities per content type and date of semester
library(plyr)
df_activity = ddply(df, .(student), summarise, n_activities = length(durationseconds))

# ANALYSIS BEFORE MERGE DATA CONSIDERING ALL ACTIVITES LIKE ONE ALL

# Plot for display the details of the activities
library(ggplot2)
# ggplot(df, aes(x = applabel)) + geom_bar()

# Plot activities in semester
ggplot(df_attemps, aes(x = relativetime, color = applabel)) + geom_density()

# Merge data
df_agg = merge(df_students, df_activity, by.x = 'student', by.y = 'student')

# Remove students without complete data (remove students with NA values in some variable)
df = df_agg[complete.cases(df_agg), ]

# Remove take_exam from variables because the majority of students take the exam (262 of 264)
df$take_exam <- NULL

# Remove group variable beacuese is redundant with social
df$grp <- NULL

# Remove student because it is just and id
df$student <- NULL


# Backup for use for build the model
df_definitive = df

# variables:
# 1. "social"
# 2. "gender"
# 3. "pretest"    
# 4."posttest"
# 5. "exam"
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
ind_var = c("social","gender","pretest", "posttest", "exam", "Fi", "CBi", "Vi", "MApi", "PApi")
ind_var_num = c("pretest", "posttest", "exam", "Fi", "CBi", "Vi", "MApi", "PApi")
ind_var_cat = c("social","gender")

# DATA DESCRIPTION


# load library for plotting
library(ggplot2)
library(purrr)
library(tidyr)

# DESCRIBE DV (Dependent variable)

# Distribution of numeric data (DV)
df[dep_var] %>%
  keep(is.numeric) %>%
  gather() %>% 
  ggplot(aes(value)) +
  facet_wrap(~ key, scales = "free") +
  # geom_histogram()
  geom_density()

# Mean and std of independet variable
mean_dep = mean(df[, dep_var])
std_dep = sd(df[, dep_var])

# Data frame for stats of IV (independent value)
df_stats_dep = data.frame(var = dep_var, 'mean'= mean_dep, 'std' = std_dep)

# View df
View(df_stats_dep)

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

  
# Plot box plot
library(reshape2)
ggplot(melt(df[, ind_var_num]), aes(x = '' , y = value)) +

            geom_boxplot() + facet_wrap(~ variable, scales = "free") +

            theme(axis.text.x = element_text(angle = 90, hjust = 1))

# # From boxplot of each numerical variable, it get that thear are outlayers. So we are going to remove them

 #OUTLAIER ANALYSIS

# # Outlier for each variable
# # pretest: >= 0,55
# # exam: <= 50
# # Fi: >= 1,0 and <= 0,17
# # Vi: <= 0,2
# # MApi <= 0,22
# # PApi: <= 0,06
# # n_activities: >= 830
# 
# Remove outliers
# df = df[!((df$pretest>= 0.55)  | (df$exam <= 50) | (df$Fi >= 1) |
        # (df$Fi<=0.17) | (df$Vi <= 0.2) | (df$MApi <= 0.22) | 
        # (df$PApi <= 0.06) | (df$n_activities >= 830)),]

# Removing only n_activities outliers >= 830
# df = df[!(df$n_activities>= 830),]

# From analisys I decide not remove outliers

# CONTINUE WITH IV

# Compute mean of each variable (Numeric)
means_ind = df[, ind_var_num] %>%
        keep(is.numeric) %>%
        colMeans()

# Compute standar deviation of each variable (Numeric)
sd_ind = df[,ind_var_num] %>%
      keep(is.numeric) %>%
      apply(2, sd)

# Get numeric names columns
numeric_names = df[,ind_var_num] %>%
  keep(is.numeric) %>%
  names

# Create dataframe for stats
df_stats_ind = data.frame('var' = numeric_names, 'mean' = means_ind, 'std' = sd_ind)

# Display stats
View(df_stats_ind)

# # Plot the stats
# ggplot(df_stats, aes(x=var, y = means, group = var)) + 
#   geom_errorbar(aes(ymin=means-std, ymax=means+std), width=.2) +
#   geom_line() +
#   geom_point() +
#   facet_wrap(~var, scales = 'free')
      
# ANALYSIS OF CORRELATION BETWEEN IV

# Plot corr matrix between IV 
library(corrplot)

# Next lines are necessary for transform categorical data to numeric for plot in correlation matrix
DF = df[,ind_var]
DF[] <- lapply(df[, ind_var],as.double)
corrplot(cor(DF))

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

# Linear model
l_m_1 <- lm(n_activities ~ pretest, data= df)
summary(l_m_1)

# Scatter plot 
# ggplot(df, aes(x = pretest, y = n_activities)) + geom_point() + geom_abline(slope = l_m_1$coefficients[2], intercept = l_m_1$coefficients[1], color = "#00AFBB")

# 2)  'n_activities' = 'pretest' + Fi
l_m_2 <- lm(n_activities ~ pretest + Fi, data= df)
summary(l_m_2)

# 3)  'n_activities' = 'pretest' + Fi + social
l_m_3 <- lm(n_activities ~ pretest + Fi + social, data= df)
summary(l_m_3)

# 3)  'n_activities' = 'pretest' + Fi + social
l_m_3 <- lm(n_activities ~ pretest + Fi + social, data= df)
summary(l_m_3)

# 4)  'n_activities' = 'pretest' + Fi + social + gender
l_m_4 <- lm(n_activities ~ pretest + Fi + social + gender, data= df)
summary(l_m_4)

# 5)  'n_activities' = 'pretest' + Fi + social + ǵender + social*pretest
l_m_5 <- lm(n_activities ~ pretest + Fi + social + gender + social*pretest, data= df)
summary(l_m_5)

# 6)  'n_activities' = 'pretest' + Fi + social + social*pretest + social*Fi
l_m_6 <- lm(n_activities ~ pretest + Fi + social + social*pretest + social*Fi, data= df)
summary(l_m_6)

# PREDICTION

# Split in train/test data
set.seed(101) # Set Seed so that same sample can be reproduced in future also
# Now Selecting 70% of data as sample from total 'n' rows of the data  
sample <- sample.int(n = nrow(df_definitive), size = floor(.70*nrow(df_definitive)), replace = F)
train <- df[sample, ]
test  <- df[-sample, ]

# Build the model on training data -
model <- lm(n_activities ~ pretest + Fi + social + gender + social*pretest, data= train)
summary(model)

# Make predictions
predictions <- predict(model, test)

# Accuracy of model
cor(predictions, test$n_activities)

# predictions == test$n_activities
# ifelse(dat$Genotype==dat$S288C,1,ifelse(dat$Genotype==dat$SK1,0,NA))

# count rows who are TRUE to compare values between target and predictions
# class(predictions)
# class(test$n_activities)

# sum(predictions == test$n_activities)
