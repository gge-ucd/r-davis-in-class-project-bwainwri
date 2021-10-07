?set.seed
# The set. seed() function sets the starting number used to 
# generate a sequence of random numbers – it ensures that you 
# get the same result if you start with that same seed each time 
# you run the same process
set.seed(15)
?runif
# Select 50 random numbers from a uniform distribution from 4 to 50
hw2 <- runif(50, 4, 50)
?replace
# Add NAs to these positions
hw2 <- replace(hw2, c(4,12,22,27), NA)
hw2
# na.omit(), complete.cases() are other options
prob1<-hw2[!is.na(hw2)]
prob1<-prob1[prob1 >= 14 & prob1 <= 38]
times3<-prob1*3
plus10<-times3+10
?seq
final<-plus10[seq(1,length(plus10),2)]

# spelled out
odds <- seq(from = 1, to = 23, by = 2)
odds
plus10[odds]

# OR
final<-plus10[c(TRUE,FALSE)]
# this assigns the second number as true and then every other as true, so these are evens
final_trial<-plus10[c(FALSE,TRUE)]






# Reading in spreadsheets
surveys<-read.csv("data/portal_data_joined.csv")
str(surveys)
head(surveys)
summary(surveys)
length(unique(surveys$species_id))

# Subsetting a dataframe
surveys[1,2]
survey_200<-surveys[200,]
?nrow
survey_last<-surveys[nrow(surveys),]
tail(surveys)


surveys_middle<-surveys[(nrow(surveys)/2),]

# Reproduce the output of the head() function by using the - 
# notation (e.g. removal) and the nrow() function, keeping 
# just the first through 6th rows of the surveys dataset.
surveys[c(1:6),]
head(surveys)

?nrow
surveys[-which(surveys > 6),]
nrow(surveys)

surveyhead<-surveys[-(7:nrow(surveys)),]



