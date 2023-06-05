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

# explore -----------------------------------------------------------------

# skim for overview
skim(data_words_cleaned)

# most common words
data_words_cleaned %>%
  count(word, sort = TRUE)

# wordcloud
data_words_cleaned %>%
  count(word) %>%
  with(wordcloud(word, n, max.words = 100))

# # save wordcloud
# ggsave(filename = "plots/wordcloud100.png",
#        width = 1000,
#        height = 1000,
#        units = "px",
#        dpi = 300
#        )

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
