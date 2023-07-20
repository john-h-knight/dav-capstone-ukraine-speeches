library(tidyverse)
library(tidytext)
library(quanteda)
library(stopwords)
library(quanteda.textplots)
library(spacyr)

# wrangle -----------------------------------------------------------------

# save as new object
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

# export for other tools
write_csv(data_paragraphs, file = 'data/speeches_as_paragraphs.csv')

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
  relocate(id_day, .after = id) %>%
  ungroup()

# top words ---------------------------------------------------------------

# use tidytext to unnest tokens as words
data_words <- unnest_tokens(data_collapsed, 
                            output = word, 
                            input = text) 

# top words
data_words %>%
  count(word, sort = TRUE) %>%
  print(n = 25)

# remove stop words
data_words_clean <- data_words %>%
  anti_join(stop_words)

# top words clean
data_words_clean %>%
  count(word, sort = TRUE) %>%
  print(n = 25)

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

# calculate sentence length
sentence_length <- parsed_tbl %>%
  filter(!c(pos %in% c("PUNCT", "SPACE", "NUM"))) %>%
  filter(!token %in% c("%", "&", "-")) %>%
  group_by(doc_id, sentence_id) %>%
  summarize(n_words = n(), .groups = "drop") %>%
  group_by(doc_id) %>%
  summarize(mean_sentence_length = mean(n_words),
            n_sentences = n())

# calculate sentence sentiment and add sentence length
sentiment <- parsed_tbl %>%
  # join sentiments
  left_join(afinn, by = c("lemma"= "word")) %>%
  group_by(doc_id, sentence_id) %>%
  # if there is no sentiment in the sentence, I make the sentiment 0.
  mutate(sentiment = ifelse(is.na(sentiment), 0, sentiment)) %>%
  # calculate the mean sentiment per sentence
  summarize(sentiment_mean = mean(sentiment, na.rm =T), .groups = "drop") %>%
  group_by(doc_id) %>%
  # split text into quintiles
  arrange(doc_id, sentence_id) %>%
  mutate(section = ntile(sentence_id, 5)) %>%
  # calculate the mean sentiment per quintiles
  group_by(doc_id, section) %>%
  summarize(mean_sep = mean(sentiment_mean)) %>%
  # spread the results from a long to a wide format
  pivot_wider(values_from  = mean_sep,
              names_from = section,
              names_prefix = "section_") %>%
  # compare the mean of the first 4 section to the last section
  mutate(diff_sec_sec1_4 = section_5 - mean(c(section_1,section_2, section_3, section_4) )) %>%
  ungroup() %>%
  # add sentence length statistics
  right_join(sentence_length, by = "doc_id") %>%
  right_join(corpus_tbl, by = "doc_id") %>%
  arrange(date) %>%
  # select columns
  select(doc_id, date, n_sentences, mean_sentence_length, section_1:diff_sec_sec1_4) %>%
  # round values
  mutate(across(where(is.numeric),  ~round(., 4)))







# countries ---------------------------------------------------------------

# use tidytext to remove stop words
parsed_clean <- unnest_tokens(parsed, output = word, input = token) %>%
  anti_join(stop_words)

# save as tibble
parsed_clean_tbl <- parsed_clean %>%
  as_tibble()

# check counts of each category of entity
parsed_clean_tbl %>%
  count(entity, sort = TRUE) %>%
  print(n = 35)

# filter for GPE entities (countries, cities, states)
GPE <- parsed_clean_tbl %>%
  filter(entity %in% c("GPE_B", "GPE_I")) %>%
  count(word, sort = TRUE)

# sentences ---------------------------------------------------------------

# initialize prior to running any spacyr functions
spacy_initialize()

# tokenize as sentences using spacy because it handles Mr. and Mrs. etc
data_sentences <- spacy_tokenize(corpus,
                                 what = "sentence", 
                                 output = "data.frame")

# finalize if done with spacy
spacy_finalize()






