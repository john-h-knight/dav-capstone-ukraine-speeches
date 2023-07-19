library(quanteda)

# create a corpus
corpus <- data %>%
  select(id, date, id_day, speech) %>%
  corpus(text_field = "speech")

# # create sentence tokens from corpus
# tokens_sentence <- tokens(corpus, 
#                           remove_punct = FALSE,
#                           what = "sentence") %>%
#   tokens_tolower()
# 
# # kwic for "thank you" from sentences
# thank_you_kwic_sent <- kwic(tokens_sentence,
#                             pattern = phrase("thank you"),
#                             window = 15)
# 
# # convert kwic results to tbl and add docvars
# thank_you_sent <- thank_you_kwic_sent %>%
#   as_tibble() %>%
#   select(!c(from, to, pattern)) %>%
#   full_join(docvars, by = c("docname" = "doc_id"))
# 
# # subset rows that have "for" in the post column
# thank_you_for_sent <- thank_you_sent[grepl("for", thank_you_sent$post), ]
# 
# # export for other tools
# write_csv(thank_you_for, file = 'data/thank_you_for.csv')
# 
# write_xlsx(thank_you_for, path = 'data/thank_you_for.xlsx')



library(spacyr)

spacy_initialize()

tokens_sent <- spacy_tokenize(corpus, 
                              what = "sentence", 
                              output = "data.frame")

# subset sentences that have "thank you"
thank_you_sent <- tokens_sent[grepl("thank you", tokens_sent$token), ]

spacy_finalize()





# library(tidytext)
# 
# # unnest sentences from speech and place in own row
# data_tidy_sent <- data %>%
#   select(id, date, id_day, speech) %>%
#   unnest_tokens(output = sentence,
#                 input = speech,
#                 token = "sentences",
#                 to_lower = TRUE,
#                 drop = TRUE
#   )



