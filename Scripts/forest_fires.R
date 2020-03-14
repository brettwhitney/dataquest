# The goal of this project was to practice writing ggplot functions and using purrr's map() and map2()

library(tidyverse)

forest_fires <- read_csv('Data/forestfires.csv')

#look at the top 10 rows
forest_fires

# I will visually investigate 2 main questions:
# 1. When is fire season?
# 2. What is the relationship between the severity (area in hectares) of forest fires and the other variables in this data set?

# When is fire season? ----
# use forcats::relevel to force month and day to be in the correct order in the visualizations

month_order <- c('jan', 'feb', 'mar', 'apr',
                 'may', 'jun', 'jul', 'aug',
                 'sep', 'oct', 'nov', 'dec')

day_order <- c('sun', 'mon', 'tue', 'wed', 'thu', 'fri', 'sat')

forest_fires_ordered <- forest_fires %>% 
  mutate(month = fct_relevel(month, month_order),
         day = fct_relevel(day, day_order))

# From this chart we can see that the worst months for forest fires are August and September.
forest_fires_ordered %>% 
  ggplot() +
  geom_bar(aes(x = month)) +
  labs(title = 'Count of Forest Fires by Month',
       x = 'Month', y = 'Number of Forest Fires') +
  theme(panel.background = element_rect(fill = 'white'))

# This visualization shows that the weekend is when most forest fires occurred. 
forest_fires_ordered %>% 
  ggplot() +
  geom_bar(aes(x = day)) +
  labs(title = 'Count of Forest Fires by Day of Week',
       x = 'Day of Week', y = 'Number of Forest Fires') +
  theme(panel.background = element_rect(fill = 'white'))

# How do the other variables in this dataset vary with time?
boxplot_by_time <- function(x, y) {
  forest_fires_ordered %>% 
    ggplot(aes_string(x = x, y = y)) +
    geom_boxplot() +
    labs(title = str_c('Distribution of', y, 'by', x, sep = ' ')) +
    theme(panel.background = element_rect(fill = 'white'))
}

var_names <- names(forest_fires_ordered[,5:13])

map2('month', var_names, boxplot_by_time)
map2('day', var_names, boxplot_by_time)



# 2. What is the relationship between the severity (area in hectares) of forest fires and the other variables in this data set? ----
scatter_area_by_var <- function(x) {
  forest_fires_ordered %>% 
    ggplot(aes_string(x = x, y = 'area')) +
    geom_point(alpha = 0.3) +
    labs(title = str_c('Relationship Between Area and', x, sep = ' ')) +
    theme(panel.background = element_rect(fill = 'white'))
}

scatterplot_var_names <- names(forest_fires_ordered[,5:12])
map(scatterplot_var_names, scatter_area_by_var)

# That first set of plots isn't very informative because area has a *very* long right tail. 
# If we filter by different values of area we should hopefully see something useful.

small_area_scatter <- function(x) {
  forest_fires_ordered %>% 
    filter(area <= 100 & area > 0) %>% 
    ggplot(aes_string(x = x, y = 'area')) +
    geom_point(alpha = 0.3) +
    labs(title = str_c('Relationship Between Area and', x, sep = ' '),
         subtitle = 'Fires that burned between 1 and 100 hectares') +
    theme(panel.background = element_rect(fill = 'white'))
}

map(scatterplot_var_names, small_area_scatter)


large_area_scatter <- function(x) {
  forest_fires_ordered %>% 
    filter(area > 100) %>% 
    ggplot(aes_string(x = x, y = 'area')) +
    geom_point() +
    labs(title = str_c('Relationship Between Area and', x, sep = ' '),
         subtitle = 'Fires that burned more than 100 hectares') +
    theme(panel.background = element_rect(fill = 'white'))
}

map(scatterplot_var_names, large_area_scatter)

# There still isn't anything super obvious in these charts besides a vague hint of a sweet spot for wind.