# Week 4 homework
library(tidyverse)
surveys<-read_csv("data/portal_data_joined.csv")
str(surveys)
surveys %>%
  filter(weight > 30 & weight < 60) %>%
  head()
# Only prints first 5 columns, how do
# we make it so it prints all columns?

# Create a new tibble showing the maximum weight for each species + sex combination 
# and name it biggest_critters. Sort the tibble to take a look at the biggest and 
# smallest species + sex combinations. HINT: it’s easier to calculate max if 
# there are no NAs in the dataframe…

biggest_critters <- surveys %>%
  filter(!is.na(weight)) %>%
  filter(!is.na(sex)) %>%      
  group_by(species_id, sex) %>%
  summarize(weight = max(weight)) %>%
  arrange(weight)
  
head(biggest_critters)
tail(biggest_critters)

# Try to figure out where the NA weights are concentrated in the data- is there a 
# particular species, taxa, plot, or whatever, where there are lots of NA values? 
# There isn’t necessarily a right or wrong answer here, but manipulate surveys a 
# few different ways to explore this. Maybe use tally and arrange here.

str(surveys)
summary(surveys)
# 3348 NAs for hindfoot length, and 2503 NAs for weight
?complete.cases
complete.cases(surveys)

NA_str <- surveys[!complete.cases(surveys),]
summary(NA_str)

NA_str <- surveys %>%
  filter(complete.cases()) 
sum(is.na(NA_str$species_id)) # species id has no NAs
sum(is.na(NA_str$sex)) # sex has 1748 NAs



NA_investigation <-NA_str %>% 
 group_by(species_id,year,plot_id) %>%
 summarize(na_ct_sex = sum(is.na(sex)),
          na_ct_wt = sum(is.na(weight)),
          na_ct_hfl = sum(is.na(hindfoot_length))) %>% 
  arrange(na_ct_sex)
head(NA_investigation)
tail(NA_investigation)
# Most sex NAs occurred in 1983, 1987, and 1985 in plots 20 and 15, and for species AH and AB

# could also arrange by weight. Would love to make a graph
NA_investigation <-NA_str %>% 
  group_by(species_id,year,plot_id) %>%
  summarize(na_ct_sex = sum(is.na(sex)),
            na_ct_wt = sum(is.na(weight)),
            na_ct_hfl = sum(is.na(hindfoot_length))) %>% 
  arrange(na_ct_wt)
head(NA_investigation)
tail(NA_investigation)

# Take surveys, remove the rows where weight is NA and add a column that contains the average 
# weight of each species+sex combination to the full surveys dataframe. Then get rid of all 
# the columns except for species, sex, weight, and your new average weight column. Save this 
# tibble as surveys_avg_weight.

weight_means<-surveys %>% 
  filter(!is.na(weight)) %>%
  filter(!is.na(sex)) %>%
  group_by(species_id,sex) %>%
  summarize(mean_wt = mean(weight))

surveys<-surveys %>% 
  filter(!is.na(weight)) %>%
  filter(!is.na(sex))

surveys_avg_weight<-merge(surveys,weight_means, by = c("species_id","sex"))
head(surveys_avg_weight)

# Take surveys_avg_weight and add a new column called above_average that contains 
# logical values stating whether or not a row’s weight is above average for its 
# species+sex combination (recall the new column we made for this tibble).
surveys_avg_weight$above_average<-surveys_avg_weight$weight > surveys_avg_weight$mean_wt

