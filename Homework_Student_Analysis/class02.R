#####
# The goal today is to find relations between variables in the data to answer the 
# following questions:
# a) which factors influence the performance (posttest, exam)?
#    here factors refer to different 'independent' variables present in the data
#    such as the motivation (Fi, CBi, Vi, MApi, PApi), amount of activity in the system
#    or the previous knowledge (pretest)
#    a.2) is the level of activity in the system related to better performance?
#    if this last relation is true, then we will be interested in knowing:
#
# b) which factors influence the level of activity in the system?
#
# This script is not completed. The idea is to guide the process of knowledge discovery
# as we progress. As a first step we load the original data, and then we compute the level of
# activity in the system for each student.
# By folowing this process there are a few ideas I suggest you to keep in mind:
# - The analytical process, the analyses we do, are guided by more or less clear research questions 
#   or hypothesis. If we don't have an idea of these, the process become unclear and endless
# - The features (or variables) we compute and use, such as amount of activity in the system,
#   deserve some exploration. What type of distribution they have? are they related to each other? 
#   (in many analyses the variables are supposed to be 'independent' but usually they are not; also,
#   many models suppose variables distributed normally, which is not as 'normal' as we may think). 
#   We can see correlations between these variables, recognize and discard outliers, explicitly deal with
#   missing data, etc.

setwd("/julio/INFORMATICA/docencia/2019_1/INFO343_DM/clase_1/")
file1    <- "attempts.csv"
file2    <- "student_data.csv"
d.traces <- read.csv(file1, sep=",", header = TRUE)
d.demogr <- read.csv(file2, sep=",", header = TRUE)

d.demogr$social  <- factor(d.demogr$social,labels=c('individual','social'))
d.demogr$gender  <- factor(d.demogr$gender,labels=c('female','male'))

# Merge data adding demographics to traces rows
data.all <- merge(d.demogr,d.traces,by.x=c("student"), by.y = c("student"))

# Aggregate the data by student (count actions)
library('plyr')
data.agg <- ddply(data.all,.(student),summarise,time=sum(durationseconds),n_activities = length(durationseconds))

# After computing the counting of activities per student we need to merge this dataframe 
# and  the dataframe d.demogr
