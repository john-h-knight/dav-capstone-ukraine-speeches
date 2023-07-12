library(tidyverse)
library(tidytext)
library(quanteda)
library(stopwords)

# wrangle -----------------------------------------------------------------

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

# rearrange columns then arrange by date
data <- data %>%
  select(!timestamp) %>%
  relocate(speech, .after = time) %>%
  arrange(date, time)

# inspect
str(data)

# remove rows 653, 681, 690, 695 because they aren't in english
data <- data[-c(653, 681, 690, 695),]

# add id number, represents overall speech number
data <- data %>%
  rowid_to_column(var = "id")

# add id_day number, represents speech number for a given day
data <- data %>%
  group_by(date) %>%
  mutate(id_day = 1:n()) %>%
  relocate(id_day, .after = date) %>%
  ungroup()

# export for other tools
write_csv(data, file = 'data/speeches_500_days.csv')


# corpus ------------------------------------------------------------------

# create a corpus
corpus <- data %>%
  select(id, date, id_day, speech) %>%
  corpus(text_field = "speech")

# verify docvars
docvars(corpus)

# create tokens from corpus
tokens <- tokens(corpus, remove_punct = FALSE) %>%
  tokens_tolower()

# remove stopwords
tokens_nostop <- tokens_remove(tokens, pattern = stopwords("en"))

# convert corpus to tibble for joining the docvars with the KWIC tibbles
docvars <- corpus %>%
  convert(to = "data.frame") %>%
  as_tibble() %>%
  select(!text)


# thank you ---------------------------------------------------------------

# kwic for "thank you"
thank_you_kwic <- kwic(tokens, pattern = phrase("thank you"), window = 20)

# convert kwic results to tbl and add docvars
thank_you <- thank_you_kwic %>%
  as_tibble() %>%
  select(!c(from, to, pattern)) %>%
  full_join(docvars, by = c("docname" = "doc_id"))

# export for other tools
write_csv(thank_you, file = 'data/thank_you.csv')