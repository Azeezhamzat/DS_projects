library(tidyverse)
library(readxl)
library(ggtext)
library(RColorBrewer)

data()
glimpse(mpg)
?mpg
view(mpg)

?filter
filter(mpg, cty>=20)
mpg_efficient <- filter(mpg, cty>=20)
view(mpg_efficient)

mpg_ford <- filter(mpg, manufacturer=="ford")
view(mpg_ford)

# converting metric: from miles per gallon to kilometers per litre
mpg_metric <- mutate(mpg, cty_metric=0.425144 * cty)
glimpse(mpg_metric)


# piping
# Shortcut : ctrl + shft + M keys give "%>%" 
mpg_mtric <- mpg %>% mutate(cty_metric=0.425144 * cty)

# groupby
mpg %>% 
  group_by(class) %>% 
  summarise(mean(cty))

mpg %>% 
  group_by(class) %>% 
  summarise(mean(cty), 
            median(cty))

# visualization
library(ggplot2)
ggplot(mpg, aes(x = cty)) + geom_histogram()

# adding label
ggplot(mpg, aes(x = cty)) + geom_histogram() +
  labs(x = "City mileage")

ggplot(mpg, aes(x = cty)) + geom_freqpoly() +
  labs(x = "City mileage")

# layering/combining plots
ggplot(mpg, aes(x = cty)) + geom_histogram() +
  geom_freqpoly() +
  labs(x = "City mileage")

ggplot(mpg, aes(x = cty,
                y = hwy)) + geom_point()

# adding regression line

ggplot(mpg, aes(x = cty,
                y = hwy)) + geom_point() +
  geom_smooth(method = 'lm')

# adding color 
ggplot(mpg, aes(x = cty,
                y = hwy,
                color = class)) + geom_point()

ggplot(mpg, aes(x = cty,
                y = hwy,
                color = class)) + geom_point() +
  scale_color_brewer(palette = "Dark2")
