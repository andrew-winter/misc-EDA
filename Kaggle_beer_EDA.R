library(tidyverse)
library(scales)
library(skimr)

beer <- read_csv('beer_reviews.csv')

# skim(data)

beer %>%
  count(beer_style, sort = TRUE) %>%
  print(n = 104)


# Gives a simple count of beer style
beer %>%
  ggplot(aes(x = beer_style)) +
  geom_bar()

# Simple bar chart showing proportion of beer styles
# These two simple plots are lacking, hard to filter
beer %>%
  ggplot(aes(x = beer_style, y = stat(prop), group = 1)) +
  geom_bar()



# What if we do use a 'counted' data frame? Easier to filter
beer %>%
  count(beer_style) %>%
  filter(n > 31000) %>%
  ggplot(aes(x = reorder(beer_style, -n), y = n)) +
  geom_col() +
  theme(axis.text.x = element_text(angle = 35, vjust = 1, hjust = 1)) +
  labs(x = 'beer_style', y = 'count')


beer %>%
  count(beer_abv, sort = TRUE)




# Comparing abv across beer styles
top_20_styles <- beer %>%
  count(beer_style, sort = TRUE) %>%
  `$`(beer_style) %>%
  `[`(1:20)

beer %>%
  filter(beer_style %in% top_20_styles) %>%
  ggplot(aes(x = beer_style, y = beer_abv)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 35, vjust = 1, hjust = 1))



# Comparing reviews across breweries
top_10_breweries <- beer %>%
  count(brewery_name, sort = TRUE) %>%
  `$`(brewery_name) %>%
  `[`(1:10)

beer %>%
  filter(brewery_name %in% top_10_breweries) %>%
  ggplot(aes(x = brewery_name, y = review_overall)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 35, vjust = 1, hjust = 1))




beer %>%
  select(beer_style, brewery_name, beer_name, review_overall, beer_abv) %>%
  arrange(desc(beer_abv))


beer %>%
  count(brewery_name, sort = TRUE)
  



# Bar chart showing frequency of most popular beers
# Not showing descending order like intended
beer %>%
  count(beer_style, review_overall, sort = TRUE) %>%
  filter(n > 10000) %>%
  ggplot(aes(reorder(beer_style, -n), n, fill = review_overall)) +
  geom_bar(stat = 'identity') +
  theme(axis.text.x = element_text(angle = 35, vjust = 1, hjust = 1)) +
  labs(x = 'beer_style')


beer %>%
  count(review_overall, sort = TRUE) %>%
  ggplot(aes(review_overall, n)) +
  geom_col()

beer %>%
  ggplot(aes(beer_abv)) +
  geom_density()
