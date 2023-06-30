# TO DO -------------------------------------------------------------------

# country count is off because it's not finding the phrase "united states"
# could be resolved using tokens_compound()


# packages ----------------------------------------------------------------

library(tidyverse)
library(tidytext)
library(stopwords)
library(skimr)
library(wordcloud)
library(quanteda)
library(writexl)
library(here)
library(flextable)
library(reshape2)
library(textdata)

data # raw
data_words # tokenized
data_words_cleaned # stopwords removed
data_words_count # word count per speech



# overview ----------------------------------------------------------------

# plot of word count per speech
data_words_count %>%
  ggplot(aes(x = date, y = n)) +
  geom_col(position = position_dodge2(preserve = "single"), 
           show.legend = FALSE, fill = "black") +
  scale_x_date("date",
               date_breaks = "1 month",
               date_labels = "%b %y",
               minor_breaks = NULL) +
  scale_y_continuous("word count",
                     minor_breaks = NULL) +
  theme_minimal()

# frequency ---------------------------------------------------------------

# most common words
data_words_cleaned %>%
  count(word, sort = TRUE)

# plot most common words
data_words_cleaned %>%
  count(word, sort = TRUE) %>%
  slice(1:10) %>%
  ggplot(aes(x = reorder(word, -n), y = n)) +
  geom_col() +
  theme_minimal()

# plot most common words
data_words_cleaned %>%
  count(word, sort = TRUE) %>%
  ggplot(aes(x = reorder(word, -n), y = n)) +
  geom_col() +
  theme_minimal() +
  coord_flip()

# words used at least 100 times
data_words_100x <- data_words_cleaned %>%
  count(word, sort = TRUE) %>%
  filter(n >= 100) %>%
  print(n = 1000)

# sentiment ---------------------------------------------------------------

# identify positive and negative words
data_sentiment_words <- data_words_cleaned %>%
  inner_join(get_sentiments("bing"),
             by = "word",
             relationship = "many-to-many") %>%
  group_by(sentiment) %>%
  count(word) %>%
  arrange(sentiment, -n) %>%
  slice(1:10)

# plot top positive and negative words
data_sentiment_words %>%
  ggplot(aes(x = n, y = reorder(word, n), fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(vars(sentiment), scales = "free_y") +
  labs(x = "Contribution to sentiment", y = NULL) +
  theme_minimal()






# make bing library a character vector that can be searched
bing <- get_sentiments("bing") 
# %>%
#   pull(word) %>%
#   str_flatten()

# search for keywords in library
bing %>%
  str_detect("strike")

# see which top words are not in the bing library
not_bing <- data_words_100x %>%
  anti_join(bing, by = c("word" = "word"))


# 599 of the 687 words used at least 100x are not in the bing library






# Countries ---------------------------------------------------------------

# import list of countries, source UN
UN_countries <- read_delim("data/UN_countries.csv", delim = ";", 
                           escape_double = FALSE, trim_ws = TRUE) %>%
  select("Country or Area") %>%
  rename(country = "Country or Area") %>%
  add_row(country = "america") %>%
  add_row(country = "united states") %>%
  add_row(country = "united kingdom") %>%
  add_row(country = "great britain") %>%
  add_row(country = "britain") %>%
  add_row(country = "england")

# pull and change to lowercase
UN_countries <- pull(UN_countries, country) %>%
  str_to_lower()

# filter for speeches that match country list
data_countries <- data_words_cleaned %>%
  filter(word %in% UN_countries) %>%
  filter(!(word %in% c("ukraine", "russia")))

# count of countries
data_countries_counts <- data_countries %>%
  count(word, sort = TRUE)

# export for other viz tools
write_csv(data_countries_counts, file = 'data/country_counts.csv')

# plot of top 10 most common countries
data_countries %>%
  count(word, sort = TRUE) %>%
  slice(1:10) %>%
  ggplot(aes(x = reorder(word, -n), y = n)) +
  geom_col() +
  theme_minimal()

# plot of countries mentioned at least 10 times
data_countries %>%
  count(word, sort = TRUE) %>%
  filter(n >= 10) %>%
  ggplot(aes(x = reorder(word, n), y = n)) +
  geom_col() +
  theme_minimal() +
  coord_flip()

# counts above don't work for "united states" because data_words_cleaned is
# tokenized as single words
# returns 0
str_count(data_flat, pattern = "united states")



