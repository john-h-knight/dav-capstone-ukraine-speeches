library(tidyverse)
library(tidytext)
library(quanteda)
library(stopwords)
library(quanteda.textplots)
library(spacyr)
library(writexl)

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


# top words ---------------------------------------------------------------

# use tidytext to unnest tokens as words
data_words <- unnest_tokens(data_paragraphs, 
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

# corpus ------------------------------------------------------------------

# create a corpus
corpus_paragraphs <- data_paragraphs %>%
  select(id, paragraph, text) %>%
  corpus(text_field = "text")

# inspect docvars
docvars_corpus_paragraphs <- docvars(corpus_paragraphs)

# # create tokens from corpus
# words <- tokens(corpus_paragraphs, remove_punct = TRUE)
# 
# # remove stopwords
# words_clean <- tokens_remove(words, pattern = stopwords("en"))

# # convert corpus to tibble for joining the docvars with the KWIC tibbles
# docvars <- corpus %>%
#   convert(to = "data.frame") %>%
#   as_tibble() %>%
#   select(!text)

# isolate sentences with thank --------------------------------------------

spacy_initialize()

# pull sentences from the paragraphs
sentences <- spacy_tokenize(corpus_paragraphs,
                            what = "sentence", 
                            output = "data.frame")

# subset sentences that have "thank"
sentences_thank <- sentences[grepl("thank", sentences$token), ]

# create a corpus using sentences_thank
corpus_sentences <- sentences_thank %>%
  select(token) %>%
  corpus(text_field = "token")


# parse and extract -------------------------------------------------------

# parse sentences
parsed <- spacy_parse(corpus_sentences,
                      pos = TRUE,
                      tag = TRUE,
                      lemma = TRUE,
                      entity = TRUE,
                      dependency = FALSE,
                      nounphrase = TRUE)

parsed %>%
  count(entity)

# extract entities
extracted <- entity_extract(parsed, type = "all")

spacy_finalize()

# filter by entity --------------------------------------------------------

# use tidytext to remove stop words
parsed_clean <- unnest_tokens(parsed, output = word, input = token) %>%
  anti_join(stop_words)

# save as tibble
parsed_clean_tbl <- parsed_clean %>%
  as_tibble()

parsed_clean_tbl %>%
  count(entity, sort = TRUE) %>%
  print(n = 35)

GPE_countries_cities_states <- parsed_clean_tbl %>%
  filter(entity %in% c("GPE_B", "GPE_I")) %>%
  count(word, sort = TRUE)

ORG_companies_agencies_institutions <- parsed_clean_tbl %>%
  filter(entity %in% c("ORG_B", "ORG_I")) %>%
  count(word, sort = TRUE)

NORP_nationalities_religious_political_groups <- parsed_clean_tbl %>%
  filter(entity %in% c("NORP_B", "NORP_I")) %>%
  count(word, sort = TRUE)

PERSON_people <- parsed_clean_tbl %>%
  filter(entity %in% c("PERSON_B", "PERSON_I")) %>%
  count(word, sort = TRUE)

# thank you ---------------------------------------------------------------

# subset sentences that have "thank you"
sentences_thank_you <- sentences[grepl("thank you", sentences$token), ]

# subset sentences that have "for"
sentences_thank_you_for <- sentences_thank_you[grepl("for", 
                                                     sentences_thank_you$token)
                                               , ]

# export for other tools
write_csv(sentences_thank_you_for, 
          file = 'data/sentences_thank_you_for.csv')
