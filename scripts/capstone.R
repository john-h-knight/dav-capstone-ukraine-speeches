# references
# https://rvest.tidyverse.org/index.html
# https://dmi3kno.github.io/polite/
# https://www.r-bloggers.com/2020/05/intro-to-polite-web-scraping-of-soccer-data-with-r/
# https://ladal.edu.au/webcrawling.html#Introduction
# https://purrr.tidyverse.org/




library(tidyverse)
library(rvest)
# library(robotstxt)
library(polite)
# library(readtext)
# library(flextable)



# define url
# this url filters dates for 1st month of conflict
url <- "https://www.president.gov.ua/en/news/speeches?date-from=24-02-2022&date-to=24-03-2022&"

# create polite session object
session <- bow(url)

# inspect
session

# extract links from page 1
speech_links <- scrape(session) %>%
  html_elements(".item_stat_headline a") %>%
  html_attr("href")

# inspect
speech_links
















session <- bow("https://www.president.gov.ua/en/news/bez-mizhnarodnogo-tribunalu-bez-prityagnennya-do-vidpovidaln-82977")

session

date <- scrape(session) %>%
  html_elements(".article") %>%
  html_elements(".date") %>%
  html_text2()

date



speech <- scrape(session) %>%
  html_element(".article_content") %>%
  html_text2()

speech



result <- tibble(
  date = date,
  speech = speech
)

result






first <- bow("https://www.president.gov.ua/en/news/zvernennya-prezidenta-ukrayini-73137")

date_first <- scrape(first) %>%
  html_elements(".article") %>%
  html_elements(".date") %>%
  html_text2()



speech_first <- scrape(first) %>%
  html_element(".article_content") %>%
  html_text2()


result_first <- tibble(
  date = date_first,
  speech = speech_first
)

result_first

