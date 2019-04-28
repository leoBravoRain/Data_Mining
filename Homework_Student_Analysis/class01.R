# 

########################################
# 1. KNOW YOUR DATA!!
# Read the data, and see basic stats
########################################
setwd(dir = '/home/leo/Escritorio/magister_uach/MAD/own_projects/Homework_Student_Analysis/')
file1    <- "attempts.csv"
file2    <- "student_data.csv"
d.traces <- read.csv(file1, sep=",", header = TRUE)
d.demogr <- read.csv(file2, sep=",", header = TRUE)

# Inspect the data and common stats
View(d.traces)
class(d.traces)
dim(d.traces)
?summary #getting help
summary(d.traces)
summary(d.traces$durationseconds)
summary(d.demogr)

mean(d.demogr$Fi)
?mean
mean(d.demogr$Fi,na.rm = TRUE)
sd(d.demogr$CBi,na.rm = TRUE)




########################################
# 2. TRANSFORMATIONS
# 
########################################
# Factorizing data
d.demogr$social  <- factor(d.demogr$social,labels=c('individual','social'))
d.demogr$gender  <- factor(d.demogr$gender,labels=c('female','male'))

# Merge data adding demographics to traces rows
data <- merge(d.demogr,d.traces,by.x=c("student"), by.y = c("student"))

#Tables
table(d.demogr$social, d.demogr$gender)

# 
########################################
# 3. PLOTTING
########################################
# install.packages('ggplot2')
library('ggplot2')

# histograms
ggplot(d.demogr, aes(x = Fi)) +
  geom_histogram(binwidth=.1) # try different values for binwidth

# density
ggplot(d.demogr, aes(x = Fi)) + geom_density()
ggplot(d.demogr, aes(x = CBi, fill=gender)) + geom_density(alpha=.5)

# scatterplots 
ggplot(d.demogr, aes(x = Fi, y = CBi, color=gender)) + geom_point(alpha=.5)

# traces in the semester
ggplot(data=data[!is.na(data$social) & data$durationseconds >= 2,], aes(y = courseorder, x=relativetime)) + 
  geom_point(size=.5,aes(colour=social),alpha= .1) +  #scale_size(range = c(3, 10)) + 
  scale_x_continuous(breaks=seq(min(data$relativetime),max(data$relativetime),1000000)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

# density
ggplot(data=data[!is.na(data$social),],  aes(x=relativetime, fill=social)) + 
  geom_density(alpha= .5) + scale_x_continuous(breaks=seq(min(data$relativetime),max(data$relativetime),1000000))

