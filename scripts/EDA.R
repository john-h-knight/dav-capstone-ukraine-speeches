# Title: Analysis of President Zelenskyy's Speeches
# By: John Knight



# TO DO -------------------------------------------------------------------

# frequency over time of top words
# KWIC for top words
# KWIC over time for top words
# ukraine, russian, RoW

# compare to fires burning data from Economist

# which speeches are most similar and most different
# and what happened those days?

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

# create, format, arrange -------------------------------------------------

# save as new object
data <- page_df

# split timestamp into separate date and time columns
data[c('date', 'time')] <- str_split_fixed(data$timestamp, " - ", 2)

# drop timestamp and rearrange columns
data <- data %>%
  select(!timestamp) %>%
  relocate(date, time, .before = title)

# parse date
data$date <- dmy(data$date)

# parse time
data$time <- hm(data$time)

# arrange by date
data <- data %>%
  arrange(date, time)

# inspect
str(data)

# assign number -----------------------------------------------------------

# check speeches per day
data %>%
  count(date)

# check cumulative speeches per day
data %>%
  count(date) %>%
  count(n)

# commenting out the below because I'm reassigning id and id_day after
# removing the languages in foreign languages
# # add id number, represents speech number
# data <- data %>%
#   rowid_to_column(var = "id")
# 
# # add id_day number, represents speech number for a given day
# data <- data %>%
#   group_by(date) %>%
#   mutate(id_day = 1:n()) %>%
#   relocate(id_day, .after = date) %>%
#   ungroup()

# inspect
str(data)

# foreign languages -------------------------------------------------------

# after plotting sentiment scores below I realized 4 speeches are not in 
# english and need to be removed
# id = 655, 683, 692, 697

# filter out by id then reassign id and id_day
# nrow should drop from 712 to 708
data <- data %>%
  filter(!(id %in% c(655, 683, 692, 697))) %>%
  select(date, time, title, speech)

# add id number, represents speech number
data <- data %>%
  rowid_to_column(var = "id")

# add id_day number, represents speech number for a given day
data <- data %>%
  group_by(date) %>%
  mutate(id_day = 1:n()) %>%
  relocate(id_day, .after = date) %>%
  ungroup()

# tokenize ----------------------------------------------------------------

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

# word counts -------------------------------------------------------------

# count unique words
data_words_cleaned %>%
  count(word, sort = TRUE) %>%
  filter(n >= 100) %>%
  print(n = 1000)

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

# plot of word count
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

# plot of speeches and word count
ggplot(data_words_count, aes(x = date, y = n)) +
  geom_point() +
  theme_minimal()



ggplot(data_words_count) +
 aes(x = date, y = id_day, size = n) +
 geom_point(shape = "square", colour = "#112446") +
 theme_minimal()


# frequency ---------------------------------------------------------------

# wordcloud
data_words_cleaned %>%
  count(word) %>%
  with(wordcloud(word, n, max.words = 10))

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

# count of "ukraine" per speech
count_per_ukraine <- pull(data, speech) %>%
  str_to_lower() %>%
  str_count(pattern = "ukraine")

# create tibble, join with id and dates
data_count_ukraine <- tibble(id = 1:708, ukraine_count = count_per_ukraine) %>%
  full_join(data_id, by = c("id" = "id")) %>%
  relocate(ukraine_count, .after = time)

# plot "ukraine" count per speech over time
ggplot(data_count_ukraine, aes(x = date, y = ukraine_count)) +
  geom_col(position = position_dodge2(preserve = "single"), 
           show.legend = FALSE, fill = "black") +
  scale_x_date("date",
               date_breaks = "1 month",
               date_labels = "%b %y",
               minor_breaks = NULL) +
  scale_y_continuous("Count of 'Ukraine'",
                     minor_breaks = NULL) +
  theme_minimal()

# sentiment ---------------------------------------------------------------

# calculate net sentiment score for each speech
data_sentiment <- data_words_cleaned %>%
  inner_join(get_sentiments("bing"),
             by = "word",
             relationship = "many-to-many"
  ) %>%
  count(id, date, id_day, sentiment) %>%
  pivot_wider(names_from = sentiment, values_from = n, values_fill = 0) %>%
  mutate(sentiment = positive - negative)


# is destroyed not in the bing library?
data_sentiment_singles <- data_words_cleaned %>%
  inner_join(get_sentiments("bing"),
             by = "word",
             relationship = "many-to-many") %>%
  filter(word == "destroy")

# is destroyed not in the bing library?
get_sentiments("bing") %>%
  slice(1400:1500) %>%
  print(n = 100)


# plot showing net sentiment
data_sentiment %>%
  ggplot(aes(x = date, y = sentiment)) +
  geom_col(position = position_dodge2(preserve = "single"), 
           show.legend = FALSE, fill = "black") +
  scale_x_date("date",
               date_breaks = "1 month",
               date_labels = "%b %y",
               minor_breaks = NULL) +
  scale_y_continuous("sentiment score",
                     minor_breaks = NULL) +
  theme_minimal()

# save plot
ggsave("plots/sentiment_score.png")

# plot showing positive and negative sentiments
data_sentiment %>%
  mutate(neg_negative = negative*(-1)) %>%
  ggplot(aes(x = date, y = positive)) +
  geom_col(position = position_dodge2(preserve = "single"), 
           show.legend = FALSE, fill = "forestgreen") +
  geom_col(inherit.aes = FALSE, aes(x = date, y = neg_negative),
           position = position_dodge2(preserve = "single"), 
           show.legend = FALSE, fill = "darkred") +
  scale_x_date("date",
               date_breaks = "1 month",
               date_labels = "%b %y",
               minor_breaks = NULL) +
  scale_y_continuous("sentiment score",
                     minor_breaks = NULL) +
  theme_minimal()

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

# wordcloud of top positive and negative words
data_sentiment_words %>%
  acast(word ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("#F8766D", "#00BFC4"),
                   max.words = 100)

# calculate net sentiment score for each speech
data_emotions <- data_words_cleaned %>%
  inner_join(get_sentiments("nrc"),
             by = "word",
             relationship = "many-to-many"
  )

# number of words by emotion
data_emotions %>%
  count(sentiment, sort = TRUE)

# plot emotions
data_emotions %>%
  count(word, sentiment, sort = TRUE) %>%
  group_by(sentiment) %>%
  slice_max(n, n = 5) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(n, word, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  theme_light() +
  labs(title = "Emotional Words in Zelenskyy's Speeches",
       subtitle = "Top 5 words, NRC lexicon",
       x = NULL,
       y = NULL)


# top negative words ------------------------------------------------------

# plot "aggression" count per speech over time
ggplot(data_count_ukraine, aes(x = date, y = ukraine_count)) +
  geom_col(position = position_dodge2(preserve = "single"), 
           show.legend = FALSE, fill = "black") +
  scale_x_date("date",
               date_breaks = "1 month",
               date_labels = "%b %y",
               minor_breaks = NULL) +
  scale_y_continuous("Count of 'Ukraine'",
                     minor_breaks = NULL) +
  theme_minimal()





# concordancing -----------------------------------------------------------

# # collapse speeches into one string
# data_flat <- pull(data, speech) %>%
#   str_flatten(collapse = ",", last = NULL, na.rm = FALSE) %>%
#   str_squish()
# 
# # 5 pre/post words around ukraine, all speeches
# kwic_ukraine <- kwic(tokens(data_flat), pattern = "ukraine") %>%
#   as_tibble()
# 
# # 5 pre/post words around united states, all speeches
# kwic_united_states <- kwic(tokens(data_flat), 
#                            pattern = phrase("united states")) %>%
#   as_tibble()

# KWIC prep using quanteda ------------------------------------------------

# create a corpus using data for KWIC
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


# KWIC ukraine with sentiment and frequency -------------------------------

# # KWIC ukraine
# data_kwic_ukraine <- kwic(data_tokens_nostop, pattern = "ukraine")

# KWIC ukrain*
data_kwic_ukrainwc <- kwic(data_tokens_nostop, pattern = "ukrain*") %>%
  as_tibble() %>%
  select(!c(from, to, pattern)) %>%
  full_join(data_kwic_join, by = c("docname" = "doc_id"))

# combine pre and post words into one string
data_kwic_ukrainwc_combined <- data_kwic_ukrainwc %>%
  mutate(combined = paste(pre, post, sep = " ")) %>%
  select(id, date, id_day, combined, keyword)

# unnest single words from KIWC and place in own row
data_kwic_ukrainwc_unnest <- data_kwic_ukrainwc_combined %>%
  unnest_tokens(output = word,
                input = combined,
                token = "words",
                drop = TRUE
  )

# calculate net sentiment score for each speech
data_kwic_ukrainwc_sentiment <- data_kwic_ukrainwc_unnest %>%
  inner_join(get_sentiments("bing"),
             by = "word",
             relationship = "many-to-many"
  ) %>%
  count(id, date, id_day, sentiment) %>%
  pivot_wider(names_from = sentiment, values_from = n, values_fill = 0) %>%
  mutate(sentiment = positive - negative)

# export for other viz tools
write_csv(data_kwic_ukrainwc_sentiment, 
          file = 'data/kwic_ukraine_sentiment.csv')

# plot showing net sentiment
data_kwic_ukrainwc_sentiment %>%
  ggplot(aes(x = date, y = sentiment)) +
  geom_col(position = position_dodge2(preserve = "single"), 
           show.legend = FALSE, fill = "black") +
  scale_x_date("date",
               date_breaks = "1 month",
               date_labels = "%b %y",
               minor_breaks = NULL) +
  scale_y_continuous("sentiment score",
                     minor_breaks = NULL) +
  labs(title = "Sentiment when talking about Ukraine") +
  theme_minimal()

# most common words
data_kwic_ukrainwc_unnest %>%
  count(word, sort = TRUE)

# plot most common words
data_kwic_ukrainwc_unnest %>%
  count(word, sort = TRUE) %>%
  slice(1:50) %>%
  ggplot(aes(x = reorder(word, n), y = n)) +
  geom_col() +
  theme_minimal() +
  coord_flip()

# KWIC russia with sentiment and frequency --------------------------------

# # KWIC russia
# data_kwic_russia <- kwic(data_tokens_nostop, pattern = "russia")

# KWIC russia*
data_kwic_russiawc <- kwic(data_tokens_nostop, pattern = "russia*") %>%
  as_tibble() %>%
  select(!c(from, to, pattern)) %>%
  full_join(data_kwic_join, by = c("docname" = "doc_id"))

# combine pre and post words into one string
data_kwic_russiawc_combined <- data_kwic_russiawc %>%
  mutate(combined = paste(pre, post, sep = " ")) %>%
  select(id, date, id_day, combined, keyword)

# unnest single words from KIWC and place in own row
data_kwic_russiawc_unnest <- data_kwic_russiawc_combined %>%
  unnest_tokens(output = word,
                input = combined,
                token = "words",
                drop = TRUE
  )

# calculate net sentiment score for each speech
data_kwic_russiawc_sentiment <- data_kwic_russiawc_unnest %>%
  inner_join(get_sentiments("bing"),
             by = "word",
             relationship = "many-to-many"
  ) %>%
  count(id, date, id_day, sentiment) %>%
  pivot_wider(names_from = sentiment, values_from = n, values_fill = 0) %>%
  mutate(sentiment = positive - negative)

# export for other viz tools
write_csv(data_kwic_russiawc_sentiment, 
          file = 'data/kwic_russia_sentiment.csv')

# plot showing net sentiment
data_kwic_russiawc_sentiment %>%
  ggplot(aes(x = date, y = sentiment)) +
  geom_col(position = position_dodge2(preserve = "single"), 
           show.legend = FALSE, fill = "black") +
  scale_x_date("date",
               date_breaks = "1 month",
               date_labels = "%b %y",
               minor_breaks = NULL) +
  scale_y_continuous("sentiment score",
                     minor_breaks = NULL) +
  labs(title = "Sentiment when talking about Russia") +
  theme_minimal()

# most common words
data_kwic_russiawc_unnest %>%
  count(word, sort = TRUE)

# plot most common words
data_kwic_russiawc_unnest %>%
  count(word, sort = TRUE) %>%
  slice(1:50) %>%
  ggplot(aes(x = reorder(word, n), y = n)) +
  geom_col() +
  theme_minimal() +
  coord_flip()




# KWIC support ------------------------------------------------------------

# KWIC support
data_kwic_support <- kwic(data_tokens_nostop, pattern = "support") %>%
  as_tibble() %>%
  select(!c(from, to, pattern)) %>%
  full_join(data_kwic_join, by = c("docname" = "doc_id"))

# combine pre and post words into one string
data_kwic_support_combined <- data_kwic_support %>%
  mutate(combined = paste(pre, post, sep = " ")) %>%
  select(id, date, id_day, combined, keyword)

# unnest single words from KIWC and place in own row
data_kwic_support_unnest <- data_kwic_support_combined %>%
  unnest_tokens(output = word,
                input = combined,
                token = "words",
                drop = TRUE
  )

# top words used with support
data_kwic_support_unnest %>%
  count(word, sort = TRUE) %>%
  slice(1:50) %>%
  print(n = 50)



# KWIC killed -------------------------------------------------------------

# create tibble
data_kwic_killed <- kwic(data_tokens_nostop, pattern = "killed") %>%
  as_tibble() %>%
  select(!c(from, to, pattern)) %>%
  full_join(data_kwic_join, by = c("docname" = "doc_id"))

# combine pre and post words into one string
data_kwic_killed_combined <- data_kwic_killed %>%
  mutate(combined = paste(pre, post, sep = " ")) %>%
  select(id, date, id_day, combined, keyword)

# unnest single words from KIWC and place in own row
data_kwic_killed_unnest <- data_kwic_killed_combined %>%
  unnest_tokens(output = word,
                input = combined,
                token = "words",
                drop = TRUE
  )

# top words used with support
data_kwic_killed_unnest %>%
  count(word, sort = TRUE) %>%
  slice(1:50) %>%
  print(n = 50)

# RoW ---------------------------------------------------------------------

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

# sources -----------------------------------------------------------------

# list of countries from UN
# https://unstats.un.org/unsd/methodology/m49/overview/

# tidytext package

# quanteda package


