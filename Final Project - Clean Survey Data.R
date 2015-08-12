library(ggplot2)

data <- read.csv("C:/Users/Rich/Downloads/Questionnaire__Copy (1).csv", stringsAsFactors=FALSE)

ci <- function(x) {
  se <- sd(x, na.rm=T)/sqrt(na.omit(length(x)))
  mn <- mean(x, na.rm=T)
  c(mn-1.96*se, mn+1.96*se)
}

#############################################################################################################################

data$height <- gsub("1,9.*", "75", data$height, ignore.case=TRUE)
data$height <- gsub("5'2", '62', data$height, ignore.case=TRUE)
data$height <- gsub("5'8", '68', data$height, ignore.case=TRUE)
data$height <- gsub("inches", '', data$height, ignore.case=TRUE)
data$height <- gsub('"$', '', data$height, ignore.case=TRUE)
data$height[data$height == ''] <- NA
data$height <- as.numeric(data$height)

# Clean-up Income
data$annual.income <- gsub(',', '', data$annual.income)
data$annual.income <- as.numeric(data$annual.income)
data$annual.income[data$annual.income==290009] <- 29000 # I assume this was an errant 9
data$annual.income[data$annual.income < 100] <- NA # Getting rid of as not plausible answers
data$annual.income[data$annual.income==''] <- NA
data$annual.income <- as.numeric(data$annual.income)

# Recode True/False
data$resentful <- data$resentful %% 2
data$given.up <- data$given.up %% 2
data$rebel<- data$rebel %% 2
data$good.listener <- data$good.listener %% 2
data$took.advantage <- data$took.advantage %% 2
data$admit.mistake <- data$admit.mistake %% 2
data$get.even <- data$get.even %% 2
data$courteous <- data$courteous %% 2
data$ideas.different.from.own <- data$ideas.different.from.own %% 2
data$jealous.of.others <- data$jealous.of.others %% 2
data$irritated.by.favors <- data$irritated.by.favors %% 2
data$hurt.someones.feelings <- data$hurt.someones.feelings %% 2

data$gender[data$gender == 1] <- 'Male'
data$gender[data$gender == 2] <- 'Female'

data$ethnicity[data$ethnicity == 1] <- 'White'
data$ethnicity[data$ethnicity == 2] <- 'Hispanic or Latino'
data$ethnicity[data$ethnicity == 3] <- 'Black or African American'
data$ethnicity[data$ethnicity == 4] <- 'Native American or American Indian'
data$ethnicity[data$ethnicity == 5] <- 'Asian/Pacific Islander'
data$ethnicity[data$ethnicity == 6] <- 'Other'

data$household.size[data$household.size == 1] <- '1'
data$household.size[data$household.size == 2] <- '2-3'
data$household.size[data$household.size == 3] <- '4-5'
data$household.size[data$household.size == 4] <- '6+'

data$exercise[data$exercise == 1] <- 'Several Times a week'
data$exercise[data$exercise == 2] <- 'Once or twice a week'
data$exercise[data$exercise == 3] <- 'Once or twice a month'
data$exercise[data$exercise == 4] <- 'Less often than once a month'
data$exercise[data$exercise == 5] <- 'Never'

data$bmi <- (data$weight * 0.45) / (data$height * 0.025)**2

#############################################################################################################################

summary(data$age)
summary(data$bmi)
summary(data$income)
summary(data$annual.income)

#############################################################################################################################

ggplot(data, aes(gender)) + geom_bar() + labs(title='Gender Distribution', x='Gender', y='Count')
ggplot(data, aes(household.size)) + geom_bar() + labs(title='Household Size Histogram', x='Size of Household', y='Count')
ggplot(data, aes(height)) + geom_histogram(binwidth=2) + labs(title='Height Histogram', x='Height (in)', y='Count')
ggplot(data, aes(age)) + geom_histogram(binwidth=4) + labs(title='Age Histogram', x='Age', y='Count')
ggplot(data, aes('Income', annual.income)) + geom_boxplot() + labs(title='Income Distribution', x='Annual Income ($)', y='Count')

ggplot(data, aes(bmi)) + geom_histogram(binwidth=2) + geom_vline(xintercept=18.5, color='green', size=1) +
  geom_vline(xintercept=25, color='green', size=1) + geom_vline(xintercept=18.4, color='yellow', size=1) +
  geom_vline(xintercept=25.1, color='yellow', size=1) + geom_vline(xintercept=29.9, color='yellow', size=1) +
  geom_vline(xintercept=30, color='red', size=1) + labs(title='BMI Histogram', x='BMI', y='Count')
