library(tidyverse)
library(tidytext)
library(quanteda)
library(stopwords)
library(writexl)

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


# tidytext ----------------------------------------------------------------

# unnest single words from speech and place in own row
data_tidy <- data %>%
  select(id, date, id_day, speech) %>%
  unnest_tokens(output = word,
                input = speech,
                token = "words",
                to_lower = TRUE,
                drop = TRUE
  )

# # remove stop words
# data_tidy_clean <- data_tidy %>%
#   anti_join(get_stopwords())
# 
# # most common words
# data_tidy_clean %>%
#   count(word, sort = TRUE)
# 
# # plot top 10 most common words
# data_tidy_clean %>%
#   count(word, sort = TRUE) %>%
#   slice(1:10) %>%
#   ggplot(aes(x = reorder(word, -n), y = n)) +
#   geom_col() +
#   theme_minimal()

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
thank_you_kwic <- kwic(tokens, pattern = phrase("thank you"), window = 30)

# convert kwic results to tbl and add docvars
thank_you <- thank_you_kwic %>%
  as_tibble() %>%
  select(!c(from, to, pattern)) %>%
  full_join(docvars, by = c("docname" = "doc_id"))

# subset rows that have "for" in the post column
thank_you_for <- thank_you[grepl("for", thank_you$post), ]

# export for other tools
write_csv(thank_you_for, file = 'data/thank_you_for.csv')

write_xlsx(thank_you_for, path = 'data/thank_you_for.xlsx')


# thank -------------------------------------------------------------------

# # kwic for thank
# thank_kwic <- kwic(tokens,
#                    pattern = "thank",
#                    window = 15)
# 
# # convert kwic results to tbl and add docvars
# # note that nrow increased due to NAs from speeches without "thank"
# thank <- thank_kwic %>%
#   as_tibble() %>%
#   select(!c(from, to, pattern)) %>%
#   full_join(docvars, by = c("docname" = "doc_id"))
# 
# # search for "for" in post
# thank_for <- thank[grepl("for", thank$post), ]
# 
# # export for other tools
# write_csv(thank_for, file = 'data/thank_for.csv')
# 
# write_xlsx(thank_for, path = 'data/thank_for.xlsx')
# 
# # kwic post "for" from "thank" kwic
# for_kwic <- kwic(tokens(thank_kwic$post),
#                                pattern = "for",
#                                window = 15)

# thank* ------------------------------------------------------------------

# # kwic for "thank" as beginning
# thank_wc_kwic <- kwic(tokens,
#                       pattern = "thank.*",
#                       valuetype = "regex",
#                       window = 10)
# 
# # convert kwic results to tbl and add docvars
# thank_wc <- thank_wc_kwic %>%
#   as_tibble() %>%
#   select(!c(from, to, pattern)) %>%
#   full_join(docvars, by = c("docname" = "doc_id"))
# 
# # forms of thank*
# thank_wc %>%
#   count(keyword, sort = TRUE)
# 
# thank_wc %>%
#   filter(keyword == "thanked")
# 
# thank_wc %>%
#   filter(keyword == "thanks")
