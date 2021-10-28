# Brooke Wainwright
# Week 5 homework

# 1. Create a tibble named surveys from the portal_data_joined.csv file. Then manipulate surveys 
# to create a new dataframe called surveys_wide with a column for genus and a column named after 
# every plot type, with each of these columns containing the mean hindfoot length of animals in 
# that plot type and genus. So every row has a genus and then a mean hindfoot length value for 
# every plot type. The dataframe should be sorted by values in the Control plot type column. 
# This question will involve quite a few of the functions you’ve used so far, and it may be 
# useful to sketch out the steps to get to the final result.

library(tidyverse)
surveys<-read_csv("data/portal_data_joined.csv")

surveys_hf_avg<-surveys %>% 
  filter(!is.na(hindfoot_length)) %>% 
  group_by(genus,plot_type) %>% 
  summarize(mean_hf = mean(hindfoot_length))
str(surveys_hf_avg)
surveys_wide<-surveys_hf_avg %>% 
  pivot_wider(names_from = plot_type, values_from = mean_hf) %>% 
  arrange(Control)

# 2. Using the original surveys dataframe, use the two different functions we laid out for 
# conditional statements, ifelse() and case_when(), to calculate a new weight category variable 
# called weight_cat. For this variable, define the rodent weight into three categories, where 
# “small” is less than or equal to the 1st quartile of weight distribution, “medium” is between 
# (but not inclusive) the 1st and 3rd quartile, and “large” is any weight greater than or equal 
# to the 3rd quartile. (Hint: the summary() function on a column summarizes the distribution). 
# For ifelse() and case_when(), compare what happens to the weight values of NA, depending on 
# how you specify your arguments.

surveys %>% 
  filter(!is.na(weight)) %>% 
  mutate(weight_cat = case_when(weight <= quantile(weight, 0.25) ~ "small",
                                      weight > quantile(weight,0.25) & weight < quantile(weight, 0.75) ~ "medium",
                                      weight >= quantile(weight, 0.25) ~ "large"))
# You can leave your last case_when argument for everything else, and that
# forces NAs as "large" or whatever

surveys %>%
  filter(!is.na(weight)) %>%
  mutate(weight_cat = ifelse(weight <= quantile(weight, 0.25), "small",
                             ifelse(weight >= quantile(weight, 0.25), "large",
                                    "medium")))

# ifelse doesn't call all remaining values, including NAs

# Week 6 lecture - data visualization
library(ggplot2)

# Use what you just learned to create a scatter plot of weight and species_id with weight on the 
# Y-axis, and species_id on the X-axis. Have the colors be coded by plot_type. Is this a good way 
# to show this type of data? What might be a better graph?

surveys_complete <- read_csv("data/portal_data_joined.csv") %>% 
  filter(complete.cases(.))

# Change labels
# Slant Species ID
# Jitter points
# Overlay boxplot

ggplot(data = surveys_complete, mapping = aes(x = species_id, y = weight, color = plot_type)) +
  geom_point(alpha = 0.25) +
  theme_classic() + 
  geom_jitter(aes(color=plot_type))

# 1. Boxplots are useful summaries, but hide the shape of the distribution. For example, if the 
# distribution is bimodal, we would not see it in a boxplot. An alternative to the boxplot is the 
# violin plot, where the shape (of the density of points) is drawn.
# Replace the box plot with a violin plot; see geom_violin().

ggplot(data = surveys_complete, mapping = aes(x = species_id, y = weight)) +
  geom_violin() 
  #geom_boxplot()
  #geom_jitter(alpha = 0.3, color = "tomato")

# 2. In many types of data, it is important to consider the scale of the observations. For example, it 
# may be worth changing the scale of the axis to better distribute the observations in the space of 
# the plot. Changing the scale of the axes is done similarly to adding/modifying other components 
# (i.e., by incrementally adding commands). Try making these modifications:
# Represent weight on the log10 scale; see scale_y_log10().

ggplot(data = surveys_complete, mapping = aes(x = species_id, y = weight)) +
  geom_jitter(alpha = 0.3, color = "tomato") +
  geom_violin() +
  scale_y_log10()
 

# 3. Make a new plot to explore the distrubtion of hindfoot_length just for species NL and PF. Overlay 
# a jitter plot of the hindfoot lengths of each species by a boxplot. Then, color the datapoints 
# according to the plot from which the sample was taken.

# Hint: Check the class for plot_id. Consider changing the class of plot_id from integer to factor. 
# Why does this change how R makes the graph?
surveys_NL_PF<-surveys_complete %>% 
  filter(species_id == "NL" | species_id == "PF") 

surveys_NL_PF$plot_type.f<-as.factor(surveys_NL_PF$plot_type) 

# It's making one box plot per plot type, I want a box plot per species
ggplot(data = surveys_NL_PF, mapping = aes(x = species_id, y = hindfoot_length, color = plot_type.f)) +
  geom_boxplot() +
  geom_point(alpha = 0.25) +
  geom_jitter(alpha = 0.3)
  
# add labels for Species
# change legend label
library(ggplot2)
p <- ggplot(surveys_NL_PF, aes(x = species_id, y = hindfoot_length))
p <- p + geom_boxplot(width = 1, alpha = 0.5)
p <- p + theme_classic()
p <- p + geom_jitter(aes(colour = plot_type), position = position_jitter(width = 0.5), alpha = 1/4)
 p <- p + labs(x ="Species", y = "Hindfoot Length")
print(p)

str(surveys_NL_PF)








