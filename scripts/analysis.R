# Title: Analysis of President Zelenskyy's Speeches
# By: John Knight



# TO DO -------------------------------------------------------------------

# 655 is in spanish
# 683 is in german (?)
# 692 is ???
# 697 is ???



# packages ----------------------------------------------------------------

library(tidyverse)
library(tidytext)
library(stopwords)
library(skimr)
library(wordcloud)

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

# add id number, represents speech number
data <- data %>%
  rowid_to_column(var = "id")

# add id_day number, represents speech number for a given day
data <- data %>%
  group_by(date) %>%
  mutate(id_day = 1:n()) %>%
  relocate(id_day, .after = date) %>%
  ungroup()

# inspect
str(data)

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

# explore -----------------------------------------------------------------

# skim for overview
skim(data_words_cleaned)

# most common words
data_words_cleaned %>%
  count(word, sort = TRUE)


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

# plot
data_sentiment %>%
  ggplot(aes(x = date, y = sentiment, fill = id_day)) +
  geom_col(position = position_dodge2(preserve = "single"), show.legend = FALSE)




