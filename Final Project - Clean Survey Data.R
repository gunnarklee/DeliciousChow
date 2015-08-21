library(ggplot2)

#dir="C:/Users/Rich/Downloads/Questionnaire__Copy (1).csv"

#To refer back to the source file on github
#download.file(url='http://github.com/gunnarklee/DeliciousChow/blob/master/clean_data.csv',
#              destfile='clean_data.csv', method='curl')
#dir = 'clean_data.csv'


dir='/Users/gunnarkleemann/Google Drive/Berkeley assignments/241files/W241_Final_DeliciousChow'
dir2=paste(dir, '/clean_survey_data.csv', sep='')
data <- read.csv(dir2, stringsAsFactors=FALSE, sep=';')
#tabulate(data)
table(data)
summary(data)
#########################################

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
names(data)
summary(data$age); print ('In 2010, the median age was 37.2 in the US')
#ref : https://www2.census.gov/prod/cen2010/briefs/c2010br-03.pdf
summary(data$bmi)
summary(data$income)
summary(data$annual.income) ;print ('Median household income in the US was $51,939 in 2013')
# ref: https://www.census.gov/content/dam/Census/library/publications/2014/demo/p60-249.pdf
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

##############################reshape the data#######################
require (dplyr)
require (tidyr)
data_long=gather(data,  recepie, measurement, Guiltless.Asian.Glazed.Chicken.Thighs:Chicken.Tostadas, na.rm=TRUE)
head(data_long)

###aggregate private data
#data$annual.income[data$annual.income==290009] <- 29000

########################## Map classfications to recepies #######

data_tmp<-data_long
Categories <- list(Asian =c("Guiltless.Asian.Glazed.Chicken.Thighs", "Low.Cal.Asian.Glazed.Chicken.Thighs"),
                    BBQ= c("Healthy.Chicken.with.Honey.Barbecue.Sauce", "Lite.Chicken..Red.Grape..and.Pesto.Pizza"),
                    Cajun= c("Cajun.Chicken","Healthy.Cajun.Chicken"),
                    Chicken =c("Chicken.with.Quinoa", "Chicken.with.Quinoa", "Healthy.Chicken.with.Salsa"),
                    Cilantro=c("Guiltless.Chicken.Breast.Cilantro.Lime.Chicken.with..Salsa", "Chicken.Breast.Cilantro.Lime.Chicken.with.Avocado.Salsa"),      
                    Cutlets = c("Chicken.Cutlets.with.Vegetables", "Chicken.Cutlets.with.Avocado.Salsa"),
                    Pizza =c("Lite.Chicken.with.Honey.Barbecue.Sauce", "Chicken..Sausage..and.Pesto.Pizza"),
                    Sate= c("Chicken.Sate.with.Veggies", "Chicken.Sate.with.Peanut.Sauce"),
                    Strips= c("Guiltless.Chicken.Strips.with.Blue.Cheese", "Chicken.Strips.with.Blue.Cheese"),
                    Tacos= c("Lite.Beef.and.Bean.Chili.Tacos", "Lite.Beef.and.Bean.Chili.Tacos"),
                    Tostados= c("Chicken.Tostadas", "Low.Cal.Chicken.Tostadas"),
                    Goat= "Chicken.Breasts.with.Goat.Cheese.and.Pine.Nuts")

nams <- names( Categories )
nums <- sapply(Categories, length)
CatMap <- unlist( Map( rep, nams, nums ) )
names(CatMap) <- unlist( Categories )
data_tmp['recepie'] <- sapply(data_tmp['recepie'], as.character) #Tranform the factor leveles to string 
data_tmp <- transform(data_tmp, MealStyle = CatMap[ recepie ])

Categories <- list(Quinoa =  ("Chicken.with.Quinoa"),
                    Vegetable =	c("Chicken.Cutlets.with.Vegetables","Chicken.Sate.with.Veggies"),
                    Control	= c("Chicken.Tostadas","Chicken.Strips.with.Blue.Cheese","Chicken.Sate.with.Peanut.Sauce", "Chicken.Cutlets.with.Avocado.Salsa","Chicken.Breast.Cilantro.Lime.Chicken.with.Avocado.Salsa","Chicken.with.Mango.Salsa", "Cajun.Chicken","Chicken.Breasts.with.Goat.Cheese.and.Pine.Nuts","Chicken..Sausage..and.Pesto.Pizza"),
                    Guiltless =	c("Guiltless.Chicken.Strips.with.Blue.Cheese","Guiltless.Chicken.Breast.Cilantro.Lime.Chicken.with..Salsa","Guiltless.Asian.Glazed.Chicken.Thighs"),
                    Healthy =	c("Healthy.Chicken.with.Salsa","Healthy.Chicken.with.Honey.Barbecue.Sauce","Healthy.Cajun.Chicken"),
                    Lite = c("Lite.Beef.and.Bean.Chili.Tacos","Lite.Chicken.with.Honey.Barbecue.Sauce","Lite.Chicken..Red.Grape..and.Pesto.Pizza"),
                    LowCal =	c("Low.Cal.Chicken.Tostadas","Low.Cal.Beef.and.Bean.Chili.Tacos","Low.Cal.Asian.Glazed.Chicken.Thighs"))
                   
nams <- names( Categories )
nums <- sapply(Categories, length)
CatMap <- unlist( Map( rep, nams, nums ) )
names(CatMap) <- unlist( Categories )
data_tmp <- transform(data_tmp, TreatMent = CatMap[ recepie ])
data_tmp['measurement']=data_tmp['measurement']-16 # remove the constant
data_long<-data_tmp
rm(data_tmp)
write.csv(data_long 'LongCleanDT.csv')
###################### make figures #######################

pdf("D:/temp/graph4.pdf" paste(dir, '/clean_survey_data.csv', sep=''))
boxplot(write)
dev.off()


