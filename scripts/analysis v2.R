library(tidyverse)
library(tidytext)
library(quanteda)
library(stopwords)
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

# export for other tools
write_csv(data_paragraphs, file = 'data/speeches_as_paragraphs.csv')

# tidytext ----------------------------------------------------------------

# unnest sentences from speech and place in own row
data_paragraphs_tidy <- data_paragraphs %>%
  unnest_tokens(output = sentence,
                input = text,
                token = "sentences",
                to_lower = FALSE,
                drop = TRUE
  )

# corpus ------------------------------------------------------------------

# create a corpus
corpus_paragraphs <- data_paragraphs %>%
  corpus(text_field = "text")

# verify docvars
docvars(corpus_paragraphs)

# # create tokens from corpus
# tokens <- tokens(corpus, remove_punct = FALSE) %>%
#   tokens_tolower()
# 
# # remove stopwords
# tokens_nostop <- tokens_remove(tokens, pattern = stopwords("en"))
# 
# # convert corpus to tibble for joining the docvars with the KWIC tibbles
# docvars <- corpus %>%
#   convert(to = "data.frame") %>%
#   as_tibble() %>%
#   select(!text)

# spacyr ------------------------------------------------------------------

spacy_initialize()

sentences <- spacy_tokenize(corpus_paragraphs,
                            what = "sentence", 
                            output = "data.frame")

# subset sentences that have "thank you"
sentences_ty <- sentences[grepl("thank you", sentences$token), ] %>%
  as_tibble()

# subset sentences that have "thank"
sentences_thank <- sentences[grepl("thank", sentences$token), ] %>%
  as_tibble()

spacy_finalize()

