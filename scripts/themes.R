# TO DO -------------------------------------------------------------------

# country count is off because it's not finding the phrase "united states"
# could be resolved using tokens_compound()


# packages ----------------------------------------------------------------

library(tidyverse)
library(tidytext)
library(stopwords)
library(quanteda)
library(quanteda.textplots)
library(quanteda.textstats)
# library(skimr)
# library(wordcloud)
# library(writexl)
# library(here)
# library(flextable)
# library(reshape2)
# library(textdata)

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

# create tokens from corpus
data_tokens <- tokens(data_corpus, remove_punct = TRUE) %>%
  tokens_tolower()

# remove stopwords
data_tokens_nostop <- tokens_remove(data_tokens, pattern = stopwords("en"))

# create a document-feature matrix (DFM) from the tokens object
dfm <- dfm(data_tokens_nostop)

# inspect
print(dfm)

# # most frequent features (same as most common words)
# topfeatures(dfm)

# convert corpus to tibble for joining the docvars with the KWIC tibbles
data_corpus_tbl <- data_corpus %>%
  convert(to = "data.frame") %>%
  as_tibble() %>%
  select(!text)

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

# scatter plot of word count by speech
ggplot(data_words_count, aes(x = date, y = n)) +
  geom_line() +
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

# export for other viz tools
write_csv(data_words_100x, file = 'data/words_100x.csv')

# words used only once
data_words_cleaned %>%
  count(word, sort = FALSE) %>%
  filter(n == 1) %>%
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
# 631 of the 722 words used at least 100x are not in the bing library
not_bing <- data_words_100x %>%
  anti_join(bing, by = c("word" = "word"))

# countries ---------------------------------------------------------------

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





# networks ----------------------------------------------------------------

# create a feature co-occurrence matrix (FCM) using the DFM
fcm <- dfm %>%
  dfm_trim(min_termfreq = 100) %>% # removes words used less than 100x
  fcm()

# inspect
print(fcm)


# from quanteda tutorial
# most frequently co-occurring words
feat <- names(topfeatures(fcm, 20))

# select features for network plot using feat
fcm_select <- fcm %>%
  fcm_select(pattern = feat, selection = "keep")

# inspect
dim(fcm_select)
print(fcm_select)

size <- log(colSums(dfm_select(dfm, feat, selection = "keep")))

fcm_select %>% 
  textplot_network(min_freq = 0.5,
                   vertex_size = size / max(size)*3)



# from LADAL tutorial
fcm_select %>%
  textplot_network(min_freq = 0.1,
                   edge_alpha = 0.5,
                   edge_color = "purple",
                   vertex_labelsize = log(rowSums(fcm_select)),
                   edge_size = 2)




?textplot_network



fcm[, "killed"]

fcm[, c("killed", "wounded", "died")]



# load function for co-occurrence calculation
source("https://slcladal.github.io/rscripts/calculateCoocStatistics.R")

# define term
coocTerm <- "thank"

# calculate co-occurrence statistics
coocs <- calculateCoocStatistics(coocTerm, dfm, measure = "LOGLIK")

# inspect results
coocs[1:20]

# select from dfm using coocs and target word "thank"
dfm_thank <- dfm_select(dfm,
                        pattern = c(names(coocs)[1:20], "thank"))

# create fcm from dfm
fcm_thank <- fcm(dfm_thank)

# inspect
print(fcm_thank)

# plot
textplot_network(fcm_thank, 
                 min_freq = 5, 
                 edge_alpha = 0.1, 
                 edge_size = 5,
                 edge_color = "purple",
                 vertex_labelsize = log(colSums(fcm_thank)))


# n-grams -----------------------------------------------------------------

# create a tbl of bigrams
bigrams <- textstat_collocations(data_tokens, 
                               min_count = 10, 
                               size = 2) %>%
  as_tibble()

# search for bigrams that contain "russian"
bigrams_russian <- bigrams[grepl("russian", bigrams$collocation), ]

# create a tbl of trigrams
trigrams <- textstat_collocations(data_tokens, 
                                  min_count = 10, 
                                  size = 3) %>%
  as_tibble()

# search for trigrams that contain "killed"
trigrams_killed <- trigrams[grepl("killed", trigrams$collocation), ]

# create a tbl of quadgrams
quadgrams <- textstat_collocations(data_tokens, 
                                 min_count = 10, 
                                 size = 4) %>%
  as_tibble()

# kwic --------------------------------------------------------------------

kwic_ukraine <- kwic(data_tokens, pattern = "ukraine")

kwic_killed <- kwic(data_tokens, pattern = "killed")

kwic_wounded <- kwic(data_tokens, pattern = "wounded")

kwic_destroyed <- kwic(data_tokens, pattern = "destroyed")

kwic_peace <- kwic(data_tokens, pattern = "peace")

kwic_thankyou <- kwic(data_tokens, pattern = phrase("thank you"))

kwic_iamgratefulto <- kwic(data_tokens, pattern = phrase("i am grateful to"))

kwic_heroofukraine <- kwic(data_tokens, pattern = phrase("hero of ukraine"))

# loss --------------------------------------------------------------------


loss <- data[grepl("killed", data$speech),]

loss2 <- data_words_cleaned[grepl("killed", data_words_cleaned$word)]




# grateful ----------------------------------------------------------------

# convert kwic results to tbl and add docvars
grateful <- kwic_iamgratefulto %>%
  as_tibble() %>%
  select(!c(from, to, pattern)) %>%
  full_join(data_corpus_tbl, by = c("docname" = "doc_id"))

# # combine pre and post words into one string
# grateful_combined <- grateful %>%
#   mutate(combined = paste(pre, post, sep = " ")) %>%
#   select(id, date, id_day, combined, keyword)

# create tokens from post words
grateful_tokens <- grateful %>%
  unnest_tokens(output = word,
                input = post,
                token = "words",
                drop = TRUE
  )

# remove stop words
grateful_tokens_clean <- grateful_tokens %>%
  anti_join(get_stopwords())

# most frequent words
grateful_tokens_clean %>%
  count(word, sort = TRUE) %>%
  print(n = 100)




# thank you ---------------------------------------------------------------

# convert kwic results to tbl and add docvars
thankyou <- kwic_thankyou %>%
  as_tibble() %>%
  select(!c(from, to, pattern)) %>%
  full_join(data_corpus_tbl, by = c("docname" = "doc_id"))

# # combine pre and post words into one string
# grateful_combined <- grateful %>%
#   mutate(combined = paste(pre, post, sep = " ")) %>%
#   select(id, date, id_day, combined, keyword)

# create tokens from post words
thankyou_tokens <- thankyou %>%
  unnest_tokens(output = word,
                input = post,
                token = "words",
                drop = TRUE
  )

# remove stop words
thankyou_tokens_clean <- thankyou_tokens %>%
  anti_join(get_stopwords())

# most frequent words
thankyou_tokens_clean %>%
  count(word, sort = TRUE) %>%
  print(n = 100)


# russian -----------------------------------------------------------------

russian <- bigrams_russian %>%
  arrange(collocation) %>%
  rowid_to_column() %>%
  filter(rowid %in% c(26:81)) %>%
  select(collocation, count) %>%
  arrange(-count)
  
