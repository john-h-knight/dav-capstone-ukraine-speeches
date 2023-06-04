# Title: Collection of President Zelenskyy's Speeches
# By: John Knight
# About:
# This script has two main functions. The first scrapes the link to each speech
# off the speech list. The second then uses that list of links to scrape the
# title, timestamp, and body of each speech.
# Dates: 14 Feb 2022 - 31 May 2023

library(tidyverse)
library(rvest)
library(polite)

# Part I: scrape links to speeches

# functions to scrape the list of speeches from /news/speeches
list_timestamp_info <- function(session_list) {
  
  list_timestamp_info <- scrape(session_list) %>%
    html_elements(".item_stat_headline p") %>%
    html_text()
}

list_title_info <- function(session_list) {
  
  list_title_info <- scrape(session_list) %>% 
    html_elements(".item_stat_headline h3") %>%
    html_text()
}

list_link_info <- function(session_list) {
  
  list_link_info <- scrape(session_list) %>% 
    html_elements(".item_stat_headline a") %>%
    html_attr("href")
}

# vector for the number of pages in the speech list
page_numbers <- 1:72

# define base url that pages numbers will be added to
base_url <- "https://www.president.gov.ua/en/news/speeches?date-from=14-02-2022&date-to=31-05-2023&page="

# combine base and page number urls
paging_urls <- paste0(base_url, page_numbers)

# inspect
paging_urls

# create tibble for functions
paging_urls_df <- tibble("page" = c(1:72),
                         "link" = paging_urls
                         )

# function to scrape data from each page of the list
list_info <- function(link, page) {
  
  page <- rlang::enquo(page)
  ## `bow()` for every URL link
  session_list <- bow(link)
  
  ## scrape different info
  list_timestamp <- list_timestamp_info(session_list = session_list)
  
  list_title <- list_title_info(session_list = session_list)
  
  list_link <- list_link_info(session_list = session_list)
  
  ## combine info into a data frame
  results <- list(list_timestamp, list_title, list_link)
  col_names <- c("timestamp", "title", "link") 
  
  list <- results %>% 
    reduce(cbind) %>% 
    as_tibble() %>% 
    set_names(col_names)
  
  return(list)
}

# wrap function for easier debugging
safe_list_info <- safely(list_info)

# iterate across each page of the list
list_df_ALL <- map(.x = paging_urls_df$link,
                       ~ safe_list_info(link = .x))

# check to see if any failed
list_df_ALL %>% 
  map("error") %>% 
  purrr::discard(~is.null(.))

# if no errors (list length of 0), combine into one df
list_df <- list_df_ALL %>% 
  map("result") %>% 
  bind_rows()

# inspect
list_df



# Part 2: scrape speeches

# create functions to scrape info from a speech page
page_timestamp_info <- function(session_page) {
  
  page_timestamp_info <- scrape(session_page) %>%
    html_element(".article p") %>%
    html_text()
}

page_title_info <- function(session_page) {
  
  page_title_info <- scrape(session_page) %>% 
    html_element(".article h1") %>%
    html_text()
}

page_body_info <- function(session_page) {
  
  page_body_info <- scrape(session_page) %>% 
    html_elements(".article_content p") %>%
    html_text2() %>%
    str_c(collapse = ", ")
}

# function to scrape data from each speech page
page_info <- function(link_page) {
  
  ## `bow()` for every URL link
  session_page <- bow(link_page)
  
  ## scrape different info
  page_timestamp <- page_timestamp_info(session_page = session_page)
  
  page_title <- page_title_info(session_page = session_page)
  
  page_body <- page_body_info(session_page = session_page)
  
  ## combine info into a data frame
  results_page <- list(page_timestamp, page_title, page_body)
  col_names_page <- c("timestamp", "title", "speech") 
  
  page <- results_page %>% 
    reduce(cbind) %>% 
    as_tibble() %>% 
    set_names(col_names_page)
  
  return(page)
}

# wrap function for easier debugging
safe_page_info <- safely(page_info)

# iterate across each link
page_df_ALL <- map(.x = list_df$link,
                       ~ safe_page_info(link_page = .x))

# check to see if any failed
page_df_ALL %>% 
  map("error") %>% 
  purrr::discard(~is.null(.))

# if no errors (list length of 0), combine into one df
page_df <- page_df_ALL %>% 
  map("result") %>% 
  bind_rows()

# inspect
page_df

# export
write_csv(page_df, file = 'data/speeches_feb_22_to_may_23.csv')




# references
# https://rvest.tidyverse.org/index.html
# https://dmi3kno.github.io/polite/
# https://www.r-bloggers.com/2020/05/intro-to-polite-web-scraping-of-soccer-data-with-r/
# https://ladal.edu.au/webcrawling.html#Introduction
# https://purrr.tidyverse.org/
