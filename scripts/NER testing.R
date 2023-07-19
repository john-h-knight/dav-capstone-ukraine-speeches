library(spacyr)

spacy_initialize()


# thank you with for ------------------------------------------------------

# # create a corpus
# corpus_ty_for <- thank_you_for %>%
#   select(id, date, post) %>%
#   corpus(text_field = "post")
# 
# # verify docvars
# docvars(corpus_ty_for)
# 
# # parse
# parsed_ty_for <- spacy_parse(corpus_ty_for,
#                              lemma = TRUE,
#                              entity = TRUE,
#                              nounphrase = TRUE)
# 
# extract_ty_for <- entity_extract(parsed_ty_for, type = "all")
# 
# extract_ty_for %>%
#   count(entity, sort = TRUE)


# thank with for in post --------------------------------------------------

# create corpus
corpus_thank_for <- thank_for %>%
  select(id, date, post) %>%
  corpus(text_field = "post")

# verify docvars
docvars(corpus_thank_for)

# parse
parsed_thank_for <- spacy_parse(corpus_thank_for,
                             lemma = TRUE,
                             entity = TRUE,
                             nounphrase = TRUE)

extract_thank_for <- entity_extract(parsed_thank_for, type = "all")

extract_ty_for %>%
  count(entity, sort = TRUE)

# end with each session ---------------------------------------------------

spacy_finalize()
