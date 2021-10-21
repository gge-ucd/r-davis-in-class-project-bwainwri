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

str(biggest_critters)
head(biggest_critters)
tail(biggest_critters)

# Try to figure out where the NA weights are concentrated in the data- is there a 
# particular species, taxa, plot, or whatever, where there are lots of NA values? 
# There isn’t necessarily a right or wrong answer here, but manipulate surveys a 
# few different ways to explore this. Maybe use tally and arrange here.

str(surveys)
summary(surveys)

surveys %>%
  filter(is.na(hindfoot_length)) %>% # get all of the NAs in hindfoot
  group_by(species_id) %>%
  tally()

surveys %>%
  filter(is.na(hindfoot_length)) %>%
  group_by(species) %>%
  summarize(count = n(), mean = mean(weight, na.rm = T))

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

surveys %>% 
 filter(!is.na(weight)) %>% 
  mutate(weight_cat = case_when(weight > mean(weight) ~ "big",
weight < mean(weight) ~ "small")) %>% 
  select(weight, weight_cat)


# Using the iris data frame (this is built in to R), create a new variable that categorizes petal 
# length into three groups:
# small (less than or equal to the 1st quartile)
# medium (between the 1st and 3rd quartiles)
# large (greater than or equal to the 3rd quartile)
# Hint: Explore the iris data using summary(iris$Petal.Length), to see the 
# petal length distribution. Then use your function of choice: ifelse() or case_when() to 
# make a new variable named petal.length.cat based on the conditions listed above. 
# Note that in the iris data frame there are no NAs, so we don’t have to deal with them here.

data(iris)
str(iris)
summary(iris)
quantile(iris$Petal.Length, 0.25)
quantile(iris$Petal.Length, 0.75)

iris %>% 
  mutate(petal_length_cat = case_when(Petal.Length <= quantile(Petal.Length, 0.25) ~ "small",
                                Petal.Length > quantile(Petal.Length,0.25) & Petal.Length < quantile(Petal.Length, 0.75) ~ "medium",
                                Petal.Length >= quantile(Petal.Length, 0.25) ~ "large"))

iris %>%
  mutate(length_cat = ifelse(Petal.Length <= 1.6, "small",
                             ifelse(Petal.Length >= 5.1, "large",
                                    "medium")))

surveys <- read_csv("data/portal_data_joined.csv")
dim(surveys)
tail_length <- read_csv("data/tail_length.csv")
dim(tail_length)
# So join functions require same number of row entries
intersect(colnames(surveys),colnames(tail_length))
surveys_joined<-full_join(surveys,tail_length, by = intersect(colnames(surveys),colnames(tail_length)))
str(surveys_joined)

# Use pivot_wider on the surveys data frame with year as columns, plot_id as rows, and the number 
# of genera per plot as the values. You will need to summarize before reshaping, and use the function 
# n_distinct() to get the number of unique genera within a particular chunk of data. It’s a powerful 
# function! See ?n_distinct for more.

surveys_wide<-surveys %>% group_by(year,plot_id) %>% summarize(n_genera = n_distinct(genus))
pivot_wider(surveys_wide, names_from = year, values_from = n_genera)






