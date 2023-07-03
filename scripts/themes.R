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

# reminder ----------------------------------------------------------------

data # raw
data_words # tokenized
data_words_cleaned # stopwords removed
data_words_count # word count per speech

# create, format, arrange -------------------------------------------------

# save as new object
data_june <- page_df_june

# split timestamp into separate date and time columns
data_june[c('date', 'time')] <- str_split_fixed(data_june$timestamp, " - ", 2)

# drop timestamp and rearrange columns
data_june <- data_june %>%
  select(!timestamp) %>%
  relocate(date, time, .before = title)

# parse date
data_june$date <- dmy(data_june$date)

# parse time
data_june$time <- hm(data_june$time)

# arrange by date
data_june <- data_june %>%
  arrange(date, time)

# inspect
str(data_june)

# add id number, represents speech number
data_june <- data_june %>%
  mutate(id = 709:753) %>%
  relocate(id, .before = date)

# add id_day number, represents speech number for a given day
data_june <- data_june %>%
  group_by(date) %>%
  mutate(id_day = 1:n()) %>%
  relocate(id_day, .after = date) %>%
  ungroup()

# combine data and data_june to create full df
data <- bind_rows(data, data_june)

# tidytext prep -----------------------------------------------------------

# unnest single words from speech and place in own row
data_words <- data %>%
  select(id, date, id_day, speech) %>%
  unnest_tokens(output = word,
                input = speech,
                token = "words",
                to_lower = TRUE,
                drop = TRUE
  )

# remove stop words
data_words_cleaned <- data_words %>%
  anti_join(get_stopwords())

# quanteda prep -----------------------------------------------------------

# create a corpusC
data_corpus <- data %>%
  select(id, date, id_day, speech) %>%
  corpus(text_field = "speech")

# verify docvars
docvars(data_corpus)

# convert corpus to tibble for joining the docvars with the KWIC tibbles
data_kwic_join <- data_corpus %>%
  convert(to = "data.frame") %>%
  as_tibble() %>%
  select(!text)

# create tokens from corpus
data_tokens <- tokens(data_corpus, remove_punct = TRUE) %>%
  tokens_tolower()

# remove stopwords
data_tokens_nostop <- tokens_remove(data_tokens, pattern = stopwords("en"))

# speech word counts ------------------------------------------------------

# word count per speech
data_words_count <- data_words %>%
  group_by(id) %>%
  count()

# create df of id, date, id_day for joining
data_id <- data %>%
  select(id, date, id_day, time)

# join and rearrange
data_words_count <- data_words_count %>%
  full_join(data_id, by = c("id" = "id")) %>%
  relocate(n, .after = time)

# export for other viz tools
write_csv(data_words_count, file = 'data/words_count.csv')

# bar plot of word count by speech
ggplot(data_words_count, aes(x = date, y = n)) +
  geom_col(position = position_dodge2(preserve = "single"), 
           show.legend = FALSE, fill = "black") +
  scale_x_date("date",
               date_breaks = "1 month",
               date_labels = "%b %y",
               minor_breaks = NULL) +
  scale_y_continuous("word count",
                     minor_breaks = NULL) +
  theme_minimal()

# scatter plot of word count by speech
ggplot(data_words_count, aes(x = date, y = n)) +
  geom_point() +
  theme_minimal()

# frequency ---------------------------------------------------------------

# most common words
data_words_cleaned %>%
  count(word, sort = TRUE)

# plot top 10 most common words
data_words_cleaned %>%
  count(word, sort = TRUE) %>%
  slice(1:10) %>%
  ggplot(aes(x = reorder(word, -n), y = n)) +
  geom_col() +
  theme_minimal()



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



