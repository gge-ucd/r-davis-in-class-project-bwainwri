# Create faceted graph by continent, where each smaller graph is the change in population between 
# 2002 and 2007 with country on the x-axis, ordered by country size

library(tidyverse)

gapminder <- read_csv("https://gge-ucd.github.io/R-DAVIS/data/gapminder.csv") #ONLY change the "data" part of this path if necessary

str(gapminder)
summary(gapminder)

# Restrict to 2002 and 2007 and drop Oceania

year_gap <- gapminder %>% 
  filter(year == 2002 | year == 2007) %>% 
  filter(!continent == "Oceania") %>% 
  select(country,year,pop,continent) %>% 
  pivot_wider(names_from = "year", values_from = "pop", names_prefix = "pop") %>% 
  mutate(pop_diff = pop2007 - pop2002)
?arrange
summary(year_gap)

library(ggplot2)
ggplot(year_gap) + 
  geom_bar(aes(x = reorder(country, pop_diff), y = pop_diff, fill = continent), stat = 'identity') +
  facet_wrap(~ continent, nrow = 2, scales = 'free') +
  labs(x = "Country", y = "Change in Population between 2002 and 2007") + 
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "NONE") +
  scale_fill_viridis_d(option = "D")

# boom, that was so fun













