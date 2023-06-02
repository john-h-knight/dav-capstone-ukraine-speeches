# Title: Analysis of President Zelenskyy's Speeches
# By: John Knight
# About:

library(tidyverse)

testing <- page_1mo_df

testing[c('date', 'time')] <- str_split_fixed(testing$date, " - ", 2)

testing <- testing[c('date', 'time', 'title', 'speech')]

testing$date <- dmy(testing$date)

testing$time <- hm(testing$time)

str(testing)


# this is how the df will be after scraping
testing2 <- page_1mo_df %>%
  mutate(timestamp = date, .before = date) %>%
  select(timestamp, title, speech)

# parse timestamp, overwrite
testing2$timestamp <- dmy_hm(testing2$timestamp)

# split timestamp into separate date and time columns
testing2[c('date', 'time')] <- str_split_fixed(testing2$timestamp, " ", 2)

# rearrange columns and remove timestamp
testing2 <- testing2 %>%
  relocate(date, time, .before = title) %>%
  select(date, time, title, speech)
  
# inspect
str(testing)
