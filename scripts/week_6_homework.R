library(tidyverse)

gapminder <- read_csv("https://gge-ucd.github.io/R-DAVIS/data/gapminder.csv") #ONLY change the "data" part of this path if necessary

str(gapminder)
summary(gapminder)

# First calculates mean life expectancy on each continent. Then create a plot that shows how life 
# expectancy has changed over time in each continent. Try to do this all in one step using pipes! 
# (aka, try not to create intermediate dataframes)
gapminder %>% 
  group_by(continent) %>% 
  summarise(mean_life_exp = mean(lifeExp))

gapminder %>% 
  group_by(continent,year) %>% 
  summarise(mean_life_exp = mean(lifeExp)) %>% 
  ggplot(mapping = aes(x = year, y = mean_life_exp, color = continent)) +
  geom_point()

# Look at the following code and answer the following questions. What do you think the 
# scale_x_log10() line of code is achieving? What about the geom_smooth() line of code?

ggplot(gapminder, aes(x = gdpPercap, y = lifeExp)) +
  geom_point(aes(color = continent), size = .25) + 
  scale_x_log10() +
  geom_smooth(method = 'lm', color = 'black', linetype = 'dashed') +
  theme_bw()
?geom_smooth
# geom_smooth adds a trend "line" through the data. In this case, it's linear model.

# Challenge! Modify the above code to size the points in proportion to the population of the 
# country. Hint: Are you translating data to a visual feature of the plot?
gapminder
ggplot(gapminder, aes(x = gdpPercap, y = lifeExp)) +
  geom_point(aes(color = continent, size = pop)) + 
  scale_x_log10() +
  geom_smooth(method = 'lm', color = 'black', linetype = 'dashed') +
  theme_bw()

# Create a boxplot that shows the life expectency for Brazil, China, El Salvador, Niger, and the 
# United States, with the data points in the backgroud using geom_jitter. Label the X and Y axis 
# with “Country” and “Life Expectancy” and title the plot “Life Expectancy of Five Countries”.

gapminder %>% 
  filter(country == "Brazil" | country == "China" | country == "El Salvador" | country == "Niger" | country == "United States") %>% 
  ggplot(aes(x = country, y = lifeExp)) +
  geom_boxplot(width = 1, alpha = 0.5) +
  theme_classic() +
  geom_jitter(position = position_jitter(width = 0.5), alpha = 1/2) +
  labs(x ="Country", y = "Life Expectancy", title = "Life Expectancy of Five Countries")



