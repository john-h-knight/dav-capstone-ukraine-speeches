# packages ----------------------------------------------------------------

library(tidyverse)
library(tidytext)
library(quanteda)
library(stopwords)
library(spacyr)
library(broom)
library(plotly)
library(sysfonts)

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

# # export for other tools
# write_csv(data_paragraphs, file = 'data/speeches_paragraphs.csv')

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

# export for other tools
write_csv(data_collapsed, file = 'data/speeches_collapsed.csv')

# meta --------------------------------------------------------------------

# use tidytext to unnest tokens as words
data_words <- unnest_tokens(data_collapsed, 
                            output = word, 
                            input = text) 

# word count per speech
speeches_meta <- data_words %>%
  group_by(id, id_day) %>%
  count() %>%
  ungroup() %>%
  select(!id_day) %>%
  right_join(data_collapsed, by = "id") %>%
  relocate(id_day, .after = id) %>%
  relocate(n, .before = text) %>%
  select(!c(title, text))

# export for other tools
write_csv(speeches_meta, file = 'data/speeches_meta.csv')

# summarize
speeches_meta %>%
  summarize(mean = mean(n),
            q25 = quantile(n, probs = 0.25),
            median = median(n),
            q75 = quantile(n, probs = 0.75)
            )

# boxplot of word count
speeches_meta %>%
  ggplot(aes(y = n)) +
  geom_boxplot()

speeches_meta %>%
  ggplot(aes(x = n)) +
  geom_density()

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

# net sentiment -----------------------------------------------------------

# download the afinn sentiment library
afinn <- get_sentiments("afinn") %>%
  as_tibble() %>%
  rename(sentiment = value)

# calculate net sentiment
sentiment_net <- parsed_tbl %>%
  # join sentiments
  left_join(afinn, by = c("lemma"= "word")) %>%
  group_by(doc_id, sentence_id) %>%
  # if there is no sentiment in the sentence, I make the sentiment 0.
  mutate(sentiment = ifelse(is.na(sentiment), 0, sentiment)) %>%
  # calculate the net sentiment per sentence
  summarize(sentence_sentiment = sum(sentiment, na.rm = T), .groups = "drop") %>%
  group_by(doc_id) %>%
  summarize(net_sentiment = sum(sentence_sentiment, na.rm = T), .groups = "drop") %>%
  # add sentence length statistics
  right_join(corpus_tbl, by = "doc_id") %>%
  arrange(date) %>%
  relocate(net_sentiment, .after = id_day)

# export for other viz tools
write_csv(sentiment_net, file = 'data/net_sentiment.csv')

# sentiment trajectory ----------------------------------------------------

# calculate sentence length
sentence_length <- parsed_tbl %>%
  filter(!c(pos %in% c("PUNCT", "SPACE", "NUM"))) %>%
  filter(!token %in% c("%", "&", "-")) %>%
  group_by(doc_id, sentence_id) %>%
  summarize(n_words = n(), .groups = "drop") %>%
  group_by(doc_id) %>%
  summarize(mean_sentence_length = mean(n_words),
            n_sentences = n()) %>%
  left_join(corpus_tbl %>% select(doc_id, id, date),
            by = "doc_id") %>%
  relocate(id, date, .after = doc_id) %>%
  arrange(id)

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
  arrange(date) %>%
  # select columns
  select(doc_id, id, date, n_sentences, mean_sentence_length, section_1:diff_sec_sec1_4) %>%
  # round values
  mutate(across(where(is.numeric),  ~round(., 4)))

# define color functions for text highlighting
colfunc_red <- colorRampPalette(c("#ff5252","#a70000"))
colfunc_green <- colorRampPalette(c("#A3D12D","#086D44"))

# get text data and use html tags to colour words green or red
text <- parsed_tbl %>%
  left_join(afinn, by = c("lemma"= "word")) %>%
  mutate(sentiment = ifelse(is.na(sentiment), 0, sentiment)) %>%
  mutate(token_colour = ifelse(sentiment < 0,
                               paste0('<span style="color:',
                                      colfunc_red(abs(min(sentiment)))[abs(sentiment)],
                                      ';"><b>',  token,'</b></span>'),
                               ifelse(sentiment > 0,
                                      paste0('<span style="color:',
                                             colfunc_green(abs(max(sentiment)))[abs(sentiment)],
                                             ';"><b>', token,'</b></span>'),
                                      token))) %>%
  mutate(nchar_token = nchar(token)) %>%
  group_by(doc_id, sentence_id) %>%
  arrange(sentence_id) %>%
  # I also want to break up long sentences
  mutate(nchar_token_cum = cumsum(nchar_token)) %>%
  mutate(split = which.min(abs(nchar_token_cum - 60))) %>%
  mutate(split2 = which.min(abs(nchar_token_cum - 120))) %>%
  mutate(split3 = which.min(abs(nchar_token_cum - 180))) %>%
  mutate(split4 = which.min(abs(nchar_token_cum - 240))) %>%
  mutate(token_colour = ifelse(token_id == split & max(nchar_token_cum) >=120 ,
                               paste0(token_colour, "<br>"),
                               token_colour)) %>%
  mutate(token_colour = ifelse(token_id == split2 & max(nchar_token_cum) >=120 ,
                               paste0(token_colour, "<br>"),
                               token_colour)) %>%
  mutate(token_colour = ifelse(token_id == split3 & max(nchar_token_cum) >=180 ,
                               paste0(token_colour, "<br>"),
                               token_colour)) %>%
  mutate(token_colour = ifelse(token_id == split4 & max(nchar_token_cum) >=240 ,
                               paste0(token_colour, "<br>"),
                               token_colour)) %>%
  # Now I have added the colours to the words I can collapse them back into sentences
  summarise(collapsed_sentence = paste0(token_colour, collapse=" ")) %>%
  mutate(# delete space before comma
    collapsed_sentence = gsub(" ,", ",", collapsed_sentence),
    # delete space before full stop
    collapsed_sentence = gsub(" \\.", ".", collapsed_sentence),
    # delete space before exclamation point
    collapsed_sentence = gsub(" !", "!", collapsed_sentence),
    # delete space around hyphens
    collapsed_sentence = gsub(" - ", "-", collapsed_sentence),
    # delete space around brackets
    collapsed_sentence = gsub("\\( ", "(", collapsed_sentence),
    # delete space around brackets
    collapsed_sentence = gsub(" \\)", ")", collapsed_sentence)) %>%
  ungroup() %>%
  # Now I just add the dates to the speeches
  left_join(sentence_length %>% select(doc_id, id, date), by = "doc_id")

dat <- parsed_tbl %>%
  filter(!c(pos %in% c("PUNCT", "SPACE", "NUM"))) %>%
  filter(!token %in% c("%", "&", "-"))  %>%
  left_join(afinn, by = c("lemma"= "word")) %>%
  group_by(doc_id, sentence_id) %>%
  mutate(sentiment = ifelse(is.na(sentiment), 0, sentiment)) %>%
  summarise(sentiment_mean = mean(sentiment, na.rm = T), .groups = "drop") %>%
  group_by(doc_id) %>%
  group_by(doc_id, sentence_id) %>%
  nest(data = -doc_id) %>%
  # use combination of map and augment to get the fitted values of the Loess regression
  mutate(
    fit = map(data, ~ loess(sentiment_mean ~ sentence_id,
                            data = .x,
                            span = 0.5)),
    augmented = map(fit, augment)) %>%
  # unnest the datasets per speech into one big dataset
  unnest(augmented)  %>%
  select(-data, - fit, - .resid) %>%
  ungroup()

# combine the text and speech data
dat2 <- left_join(dat, text, by = c("sentence_id", "doc_id" )) %>%
  mutate(doc_id = paste0(doc_id, " (", date, ")")) %>%
  # order speeches
  arrange(id, sentence_id)

# subset for testing
dat2_subset <- dat2 %>%
  filter(!(doc_id == "text746 (2023-06-28)"))

# export for other tools
dat2 %>%
  select(sentence_id, .fitted, id, date) %>%
  relocate(id, date, .before = sentence_id) %>%
  write_csv(file = 'data/speeches_sentiment.csv')

# create a color palette from the colours of the Ghanaian coat of arms
colfunc <- colorRampPalette(c("#CE1126","#FCD116","#006B3F", "#0193DD", "#000000"))

# I need this font for some of the plots
font_add_google(name = "Raleway", regular.wt = 300)

# now all of this can be combined into a plotly plot
fig3 <- plot_ly(data = dat2,
                type = 'scatter',
                mode = "lines",
                color = ~doc_id,
                sort = FALSE,
                colors = colfunc(11),
                opacity = 0.75) %>%
  add_trace(
    y = ~.fitted,
    x = ~sentence_id,
    text = ~collapsed_sentence,
    hoverinfo = 'text',
    hoverlabel = list(  bgcolor = "white",
                        opacity = 0.5,
                        size = 7,
                        font= list(family = 'Raleway'),
                        align = "left",
                        line = list(
                          opacity = 0.9,
                          width = 3)),
    showlegend = T)  %>%
  layout(title = list(text = "<b>Sentiment of Speeches by President Zelenskyy</b><br><sup>Speeches vary in length, but most end on a positive note.</sup>",
                      x =0.05,
                      xanchor = "left",
                      automargin = TRUE),
         font = list(color = '#706f6f',
                     family = 'Raleway',
                     size = 14),
         xaxis = list(title = "Sentence number",
                      showspikes = TRUE,
                      spikemode  = 'toaxis',
                      spikesnap = 'data',
                      showline = TRUE,
                      showgrid = FALSE,
                      showticklabels = TRUE,
                      linecolor = '#706f6f',
                      linewidth = 2,
                      autotick = TRUE,
                      ticks = 'outside',
                      tickcolor = '#706f6f',
                      tickwidth = 2,
                      ticklen = 5,
                      tickfont = list(family = 'Raleway',
                                      size = 12,
                                      color = '#706f6f')),
         yaxis = list(title = "Average sentence sentiment (smoothed)",
                      showline = TRUE,
                      showgrid = FALSE,
                      showticklabels = TRUE,
                      linecolor = '#706f6f',
                      linewidth = 2,
                      autotick = TRUE,
                      ticks = 'outside',
                      tickcolor = '#706f6f',
                      tickwidth = 2,
                      ticklen = 5,
                      tickfont = list(family = 'Raleway',
                                      size = 12,
                                      color = '#706f6f')),
         legend = list(title = list(text = "<b>Speeches</b><br><sup>(double click on a speech to isolate it and hover over lines to see text)</sup>"),
                       x = 0, y = -3.8,
                       xanchor = "left"),
         autosize = T,
         margin = list(t = 100),
         showlegend = TRUE) %>%
  add_annotations( x = 1,
                   y = -0.05,
                   xanchor = "left",
                   showarrow = F,
                   align = "left",
                   text=  '<span style="color:#a70000;"><b>Negative values signify<br>negative sentiment</b></span>') %>%
  add_annotations( x = 1,
                   y = 0.3,
                   xanchor = "left",
                   showarrow = F,
                   align = "left",
                   text=  '<span style="color:#A3D12D;"><b>Positive values signify<br>positive sentiment</b></span>') %>%
  config(displayModeBar = FALSE)

fig3



# sentiment as sum --------------------------------------------------------

# I tried using the sum of the sentence sentiment rather than the mean
# of the sentence sentiment. It didn't make a huge difference. The chart
# was noisier in the lower sentence numbers, but the pattern was largely the
# same. I don't think it matters because it's about comparing the speeches
# to each other so it's less important what math is applied.

# sentiment_sum <- parsed_tbl %>%
#   # join sentiments
#   left_join(afinn, by = c("lemma"= "word")) %>%
#   group_by(doc_id, sentence_id) %>%
#   # if there is no sentiment in the sentence, I make the sentiment 0.
#   mutate(sentiment = ifelse(is.na(sentiment), 0, sentiment)) %>%
#   # calculate the cumulative sentiment per sentence
#   summarize(sentiment_sum = sum(sentiment, na.rm =T), .groups = "drop") %>%
#   group_by(doc_id) %>%
#   # split text into quintiles
#   arrange(doc_id, sentence_id) %>%
#   mutate(section = ntile(sentence_id, 5)) %>%
#   # calculate the mean sentiment per quintiles
#   group_by(doc_id, section) %>%
#   summarize(mean_sep = mean(sentiment_sum)) %>%
#   # spread the results from a long to a wide format
#   pivot_wider(values_from  = mean_sep,
#               names_from = section,
#               names_prefix = "section_") %>%
#   # compare the mean of the first 4 section to the last section
#   mutate(diff_sec_sec1_4 = section_5 - mean(c(section_1,section_2, section_3, section_4) )) %>%
#   ungroup() %>%
#   # add sentence length statistics
#   right_join(sentence_length, by = "doc_id") %>%
#   arrange(date) %>%
#   # select columns
#   select(doc_id, id, date, n_sentences, mean_sentence_length, section_1:diff_sec_sec1_4) %>%
#   # round values
#   mutate(across(where(is.numeric),  ~round(., 4)))
# 
# # define color functions for text highlighting
# colfunc_red <- colorRampPalette(c("#ff5252","#a70000"))
# colfunc_green <- colorRampPalette(c("#A3D12D","#086D44"))
# 
# # get text data and use html tags to colour words green or red
# text <- parsed_tbl %>%
#   left_join(afinn, by = c("lemma"= "word")) %>%
#   mutate(sentiment = ifelse(is.na(sentiment), 0, sentiment)) %>%
#   mutate(token_colour = ifelse(sentiment < 0,
#                                paste0('<span style="color:',
#                                       colfunc_red(abs(min(sentiment)))[abs(sentiment)],
#                                       ';"><b>',  token,'</b></span>'),
#                                ifelse(sentiment > 0,
#                                       paste0('<span style="color:',
#                                              colfunc_green(abs(max(sentiment)))[abs(sentiment)],
#                                              ';"><b>', token,'</b></span>'),
#                                       token))) %>%
#   mutate(nchar_token = nchar(token)) %>%
#   group_by(doc_id, sentence_id) %>%
#   arrange(sentence_id) %>%
#   # I also want to break up long sentences
#   mutate(nchar_token_cum = cumsum(nchar_token)) %>%
#   mutate(split = which.min(abs(nchar_token_cum - 60))) %>%
#   mutate(split2 = which.min(abs(nchar_token_cum - 120))) %>%
#   mutate(split3 = which.min(abs(nchar_token_cum - 180))) %>%
#   mutate(split4 = which.min(abs(nchar_token_cum - 240))) %>%
#   mutate(token_colour = ifelse(token_id == split & max(nchar_token_cum) >=120 ,
#                                paste0(token_colour, "<br>"),
#                                token_colour)) %>%
#   mutate(token_colour = ifelse(token_id == split2 & max(nchar_token_cum) >=120 ,
#                                paste0(token_colour, "<br>"),
#                                token_colour)) %>%
#   mutate(token_colour = ifelse(token_id == split3 & max(nchar_token_cum) >=180 ,
#                                paste0(token_colour, "<br>"),
#                                token_colour)) %>%
#   mutate(token_colour = ifelse(token_id == split4 & max(nchar_token_cum) >=240 ,
#                                paste0(token_colour, "<br>"),
#                                token_colour)) %>%
#   # Now I have added the colours to the words I can collapse them back into sentences
#   summarise(collapsed_sentence = paste0(token_colour, collapse=" ")) %>%
#   mutate(# delete space before comma
#     collapsed_sentence = gsub(" ,", ",", collapsed_sentence),
#     # delete space before full stop
#     collapsed_sentence = gsub(" \\.", ".", collapsed_sentence),
#     # delete space before exclamation point
#     collapsed_sentence = gsub(" !", "!", collapsed_sentence),
#     # delete space around hyphens
#     collapsed_sentence = gsub(" - ", "-", collapsed_sentence),
#     # delete space around brackets
#     collapsed_sentence = gsub("\\( ", "(", collapsed_sentence),
#     # delete space around brackets
#     collapsed_sentence = gsub(" \\)", ")", collapsed_sentence)) %>%
#   ungroup() %>%
#   # Now I just add the dates to the speeches
#   left_join(sentence_length %>% select(doc_id, id, date), by = "doc_id")
# 
# dat_sum <- parsed_tbl %>%
#   filter(!c(pos %in% c("PUNCT", "SPACE", "NUM"))) %>%
#   filter(!token %in% c("%", "&", "-"))  %>%
#   left_join(afinn, by = c("lemma"= "word")) %>%
#   group_by(doc_id, sentence_id) %>%
#   mutate(sentiment = ifelse(is.na(sentiment), 0, sentiment)) %>%
#   summarise(sentiment_sum = sum(sentiment, na.rm =T), .groups = "drop") %>%
#   group_by(doc_id) %>%
#   group_by(doc_id, sentence_id) %>%
#   nest(data = -doc_id) %>%
#   # use combination of map and augment to get the fitted values of the Loess regression
#   mutate(
#     fit = map(data, ~ loess(sentiment_sum ~ sentence_id,
#                             data = .x,
#                             span = 0.5)),
#     augmented = map(fit, augment)) %>%
#   # unnest the datasets per speech into one big dataset
#   unnest(augmented)  %>%
#   select(-data, - fit, - .resid) %>%
#   ungroup()
# 
# # combine the text and speech data
# dat2_sum <- left_join(dat_sum, text, by = c("sentence_id", "doc_id" )) %>%
#   mutate(doc_id = paste0(doc_id, " (", date, ")")) %>%
#   # order speeches
#   arrange(id, sentence_id)
# 
# # subset for testing
# dat2_sum_subset <- dat2_sum %>%
#   filter(!(doc_id == "text746 (2023-06-28)"))
# 
# # # export for other tools
# # dat2_sum %>%
# #   select(sentence_id, .fitted, id, date) %>%
# #   relocate(id, date, .before = sentence_id) %>%
# #   write_csv(file = 'data/speeches_sentiment_sum.csv')
# 
# # create a color palette from the colours of the Ghanaian coat of arms
# colfunc <- colorRampPalette(c("#CE1126","#FCD116","#006B3F", "#0193DD", "#000000"))
# 
# # I need this font for some of the plots
# font_add_google(name = "Raleway", regular.wt = 300)
# 
# # now all of this can be combined into a plotly plot
# fig4 <- plot_ly(data = dat2_sum,
#                 type = 'scatter',
#                 mode = "lines",
#                 color = ~doc_id,
#                 sort = FALSE,
#                 colors = colfunc(11),
#                 opacity = 0.75) %>%
#   add_trace(
#     y = ~.fitted,
#     x = ~sentence_id,
#     text = ~collapsed_sentence,
#     hoverinfo = 'text',
#     hoverlabel = list(  bgcolor = "white",
#                         opacity = 0.5,
#                         size = 7,
#                         font= list(family = 'Raleway'),
#                         align = "left",
#                         line = list(
#                           opacity = 0.9,
#                           width = 3)),
#     showlegend = T)  %>%
#   layout(title = list(text = "<b>Sentiment of Speeches by President Zelenskyy</b><br><sup>Speeches vary in length, but most end on a positive note.</sup>",
#                       x =0.05,
#                       xanchor = "left",
#                       automargin = TRUE),
#          font = list(color = '#706f6f',
#                      family = 'Raleway',
#                      size = 14),
#          xaxis = list(title = "Sentence number",
#                       showspikes = TRUE,
#                       spikemode  = 'toaxis',
#                       spikesnap = 'data',
#                       showline = TRUE,
#                       showgrid = FALSE,
#                       showticklabels = TRUE,
#                       linecolor = '#706f6f',
#                       linewidth = 2,
#                       autotick = TRUE,
#                       ticks = 'outside',
#                       tickcolor = '#706f6f',
#                       tickwidth = 2,
#                       ticklen = 5,
#                       tickfont = list(family = 'Raleway',
#                                       size = 12,
#                                       color = '#706f6f')),
#          yaxis = list(title = "Average sentence sentiment (smoothed)",
#                       showline = TRUE,
#                       showgrid = FALSE,
#                       showticklabels = TRUE,
#                       linecolor = '#706f6f',
#                       linewidth = 2,
#                       autotick = TRUE,
#                       ticks = 'outside',
#                       tickcolor = '#706f6f',
#                       tickwidth = 2,
#                       ticklen = 5,
#                       tickfont = list(family = 'Raleway',
#                                       size = 12,
#                                       color = '#706f6f')),
#          legend = list(title = list(text = "<b>Speeches</b><br><sup>(double click on a speech to isolate it and hover over lines to see text)</sup>"),
#                        x = 0, y = -3.8,
#                        xanchor = "left"),
#          autosize = T,
#          margin = list(t = 100),
#          showlegend = TRUE) %>%
#   add_annotations( x = 1,
#                    y = -0.05,
#                    xanchor = "left",
#                    showarrow = F,
#                    align = "left",
#                    text=  '<span style="color:#a70000;"><b>Negative values signify<br>negative sentiment</b></span>') %>%
#   add_annotations( x = 1,
#                    y = 0.3,
#                    xanchor = "left",
#                    showarrow = F,
#                    align = "left",
#                    text=  '<span style="color:#A3D12D;"><b>Positive values signify<br>positive sentiment</b></span>') %>%
#   config(displayModeBar = FALSE)
# 
# fig4

# countries ---------------------------------------------------------------

# # initialize prior to running any spacyr functions
# spacy_initialize()
# 
# # consolidate multi-word entities into one token for easier identification
# ent_consolidated <- entity_consolidate(parsed, concatenator = " ")
# 
# # filter for GPE entities (countries, cities, states)
# GPE <- ent_consolidated %>%
#   filter(entity_type == "GPE") %>%
#   count(token, sort = TRUE)
# 
# # export for other tools
# write_csv(GPE, file = 'data/GPE.csv')
# 
# # filter for ORG entities (companies, agencies, institutions)
# ORG <- ent_consolidated %>%
#   filter(entity_type == "ORG") %>%
#   count(token, sort = TRUE)
# 
# # export for other tools
# write_csv(ORG, file = 'data/ORG.csv')
# 
# # finalize if done with spacy
# spacy_finalize()

