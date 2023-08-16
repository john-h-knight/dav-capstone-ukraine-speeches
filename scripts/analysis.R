# packages ----------------------------------------------------------------

library(tidyverse)
library(tidytext)
library(quanteda)
library(stopwords)
library(spacyr)
library(broom)

# wrangle -----------------------------------------------------------------

# save as new object, from collection v2
data_paragraphs <- page_df_no_collapse

# split timestamp into separate date and time columns
data_paragraphs[c('date', 'time')] <- str_split_fixed(
  data_paragraphs$timestamp, " - ", 2)

# parse date
data_paragraphs$date <- dmy(data_paragraphs$date)

# parse time
data_paragraphs$time <- hm(data_paragraphs$time)

# drop timestamp and rearrange columns
data_paragraphs <- data_paragraphs %>%
  select(!timestamp) %>%
  relocate(date, time, .before = title) %>%
  arrange(date, time)

# inspect
str(data_paragraphs)

# remove speeches that aren't in english
data_paragraphs <- data_paragraphs[-c(c(19775:19820), 
                                      c(20441:20520), 
                                      c(20746:20776), 
                                      c(20891:20923)
                                      ), 
                                   ]

# assign id number to each speech
data_paragraphs <- data_paragraphs %>%
  group_by(date, time) %>%
  mutate(id = cur_group_id()) %>%
  ungroup() %>%
  relocate(id, .before = paragraph)

# collapse paragraph strings into one string per speech
# and add id_day number
data_collapsed <- data_paragraphs %>%
  group_by(id) %>%
  mutate(speech = paste0(text, collapse = " ")) %>%
  ungroup() %>%
  filter(paragraph == 2) %>%
  select(!c(paragraph, text)) %>%
  rename(text = speech) %>%
  group_by(date) %>%
  mutate(id_day = 1:n()) %>%
  relocate(id, .before = date) %>%
  relocate(id_day, .after = id) %>%
  ungroup()

# word counts -------------------------------------------------------------

# use tidytext to unnest tokens as words
data_words <- unnest_tokens(data_collapsed, 
                            output = word, 
                            input = text) 

# word count per speech
speech_length <- data_words %>%
  group_by(id, id_day) %>%
  count() %>%
  ungroup() %>%
  rename(words = n) %>%
  select(!id_day)

# corpus and parse --------------------------------------------------------

# create a corpus
corpus <- data_collapsed %>%
  select(!c(time, title)) %>%
  corpus(text_field = "text")

# convert corpus to tibble to check doc_id
corpus_tbl <- corpus %>%
  convert(to = "data.frame") %>%
  as_tibble() %>%
  select(!text)

# initialize prior to running any spacyr functions
spacy_initialize()

# tokenize and annotate
parsed <- spacy_parse(corpus,
                      pos = TRUE,
                      tag = FALSE,
                      lemma = TRUE,
                      entity = TRUE,
                      dependency = FALSE,
                      nounphrase = TRUE,
                      additional_attributes = c("is_punct", "is_stop")
                      )

# convert to tbl
parsed_tbl <- parsed %>%
  as_tibble()
  
# finalize if done with spacy
spacy_finalize()

# sentiment ---------------------------------------------------------------

# download the afinn sentiment library
afinn <- get_sentiments("afinn") %>%
  as_tibble() %>%
  rename(sentiment = value)

# calculate net sentiment
sentiment_net <- parsed_tbl %>%
  left_join(afinn, by = c("lemma"= "word")) %>%
  group_by(doc_id, sentence_id) %>%
  mutate(sentiment = ifelse(is.na(sentiment), 0, sentiment)) %>%
  summarize(sentence_sentiment = sum(sentiment, na.rm = TRUE),
            .groups = "drop") %>%
  group_by(doc_id) %>%
  summarize(score = sum(sentence_sentiment, na.rm = TRUE), 
            .groups = "drop") %>%
  mutate(sentiment = ifelse(score > 0, "positive", "not positive")) %>%
  right_join(corpus_tbl, by = "doc_id") %>%
  select(id, sentiment, score) %>%
  arrange(id)

# sentiment impact --------------------------------------------------------

# determine the impact that words have on sentiment
# based on weight of lemmatized tokens and frequency
sentiment_impact <- parsed_tbl %>%
  left_join(afinn, by = c("lemma"= "word")) %>%
  mutate(sentiment = ifelse(is.na(sentiment), 0, sentiment)) %>%
  select(!c(pos, entity, nounphrase, whitespace, is_punct, is_stop)) %>%
  count(lemma, sentiment) %>%
  rename(count = n) %>%
  mutate(impact = sentiment * count)

# isolate positive impact
positive_impact <- sentiment_impact %>%
  arrange(desc(impact)) %>%
  slice(1:10)

# isolate negative impact
negative_impact <- sentiment_impact %>%
  arrange(impact) %>%
  slice(1:10) %>%
  arrange(desc(impact))

# combine positive and negative impact
combined_impact <- positive_impact %>%
  bind_rows(negative_impact) %>%
  mutate(category = ifelse(impact > 0, "positive", "negative"))

# export for other viz tools
combined_impact %>%
  write_csv(file = 'data/sentiment_impact.csv')

# combine -----------------------------------------------------------------

# add word count and sentiment to data_collapsed
speeches <- data_collapsed %>%
  left_join(speech_length, by = "id") %>%
  left_join(sentiment_net, by = "id")

# export for other tools
speeches %>%
  write_csv(file = 'data/speeches.csv')

# speeches per day --------------------------------------------------------

# count the number of days where there was 0, 1, 2, 3, 4 speeches
speeches_per_day <- speeches %>%
  group_by(date) %>%
  count() %>%
  group_by(n) %>%
  count() %>%
  ungroup() %>%
  rename(per_day = n, count = nn) %>%
  add_row(per_day = 0, count = (500 - 283 - 144 - 44 - 14)) %>%
  arrange(per_day)

# export for other tools
speeches_per_day %>%
  write_csv(file = 'data/speeches_per_day.csv')

# sentiment path ----------------------------------------------------------

sentiment_path <- parsed_tbl %>%
  filter(!c(pos %in% c("PUNCT", "SPACE", "NUM"))) %>%
  filter(!token %in% c("%", "&", "-"))  %>%
  left_join(afinn, by = c("lemma"= "word")) %>%
  group_by(doc_id, sentence_id) %>%
  mutate(sentiment = ifelse(is.na(sentiment), 0, sentiment)) %>%
  summarise(sentiment_sum = sum(sentiment, na.rm =TRUE), .groups = "drop") %>%
  group_by(doc_id) %>%
  group_by(doc_id, sentence_id) %>%
  nest(data = -doc_id) %>%
  mutate(
    fit = map(data, ~ loess(sentiment_sum ~ sentence_id,
                            data = .x,
                            span = 0.5)),
    augmented = map(fit, augment)) %>%
  unnest(augmented)  %>%
  select(-data, - fit, - .resid) %>%
  ungroup() %>%
  left_join(corpus_tbl, by = "doc_id") %>%
  select(id, id_day, date, sentence_id, .fitted)

# export for other tools
sentiment_path %>%
  write_csv(file = 'data/sentiment_path.csv')

# identify which speeches end on a positive (high) note
sentiment_path_ends <- sentiment_path %>%
  group_by(id) %>%
  slice_max(sentence_id) %>%
  ungroup() %>%
  mutate(high_note = ifelse(.fitted > 0, TRUE, FALSE))

# export for other tools
sentiment_path_ends %>%
  write_csv(file = 'data/sentiment_path_ends.csv')

# how many end on a high note
sentiment_path_ends %>%
  count(high_note)
