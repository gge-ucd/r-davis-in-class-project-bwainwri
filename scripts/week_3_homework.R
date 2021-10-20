library(tidyverse)
survey<-read.csv("data/portal_data_joined.csv")
str(survey)
surveys_base<-survey[1:60,c("species_id","weight","plot_type")]
str((surveys_base))

surveys_base<-
  survey %>%
  select(species_id,
         weight,
         plot_type
  )
surveys_base %>% str()

surveys_base$species_id.f<-as.factor(surveys_base$species_id)
surveys_base$plot_type.f<-as.factor(surveys_base$plot_type)
surveys_base<-surveys_base[!is.na(surveys_base$weight),]
challenge_base<-surveys_base[surveys_base$weight > 150,]

#1 keep only obs before 1995
str(survey)
challenge_1<-survey %>% filter(year < 1995)

challenge_2 <- challenge_1 %>%
  select(year,
         sex,
         weight)

challenge_1_2 <- survey %>% 
  filter(year < 1995) %>%
  select(year,
         sex,
         weight)

challenge_1_2 == challenge_2
# Can NAs be equal to NAs?
?mutate

str(survey)
challenge_3<- survey %>%
  mutate(hindfoot_half = hindfoot_length/2) %>%
  filter(!is.na(hindfoot_half) | hindfoot_half < 30) %>%
  select(species_id, hindfoot_half)


#Use group_by() and summarize() to find the mean, 
#min, and max hindfoot length for each species (using species_id).
challenge_4 <- survey %>%
  filter(!is.na(hindfoot_length)) %>%
  group_by(species_id) %>%
  summarize(mean_hf = mean(hindfoot_length),
            min_hf = min(hindfoot_length),
            max_hf = max(hindfoot_length))

# What was the heaviest animal measured in each year? 
# Return the columns year, genus, species_id, and weight.
challenge_5 <- survey %>%
  filter(!is.na(weight)) %>%
  group_by(year) %>%
  filter(weight == max(weight)) %>%
  select(year, genus, species, weight) %>%
  arrange(year)

# You saw above how to count the number of individuals of each sex using a 
# combination of group_by() and tally(). How could you get the same result 
# using group_by() and summarize()? Hint: see ?n.
surveys %>%
  group_by(sex) %>%
  summarize(n = n())
