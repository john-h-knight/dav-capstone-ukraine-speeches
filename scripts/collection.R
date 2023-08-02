# packages ----------------------------------------------------------------

library(tidyverse)
library(rvest)
library(polite)

# scrape links to each speech page ----------------------------------------

list_link_info <- function(session_list) {
  
  list_link_info <- scrape(session_list) %>% 
    html_elements(".item_stat_headline a") %>%
    html_attr("href")
}

# vector for the number of web pages containing the list of speeches
page_numbers <- 1:77

# define base url that pages numbers will be added to
base_url <- "https://www.president.gov.ua/en/news/speeches?date-from=24-02-2022&date-to=08-07-2023&page="

# combine base and page number urls
paging_urls <- paste0(base_url, page_numbers)

# inspect
paging_urls

# create tibble for functions
paging_urls_df <- tibble("page" = c(1:77),
                         "link" = paging_urls
                         )

# function to scrape data from each page of the list
list_info <- function(link, page) {
  
  page <- rlang::enquo(page)
  ## `bow()` for every URL link
  session_list <- bow(link)
  
  ## scrape different info
  list_link <- list_link_info(session_list = session_list)
  
  ## combine info into a data frame
  results <- list(list_link)
  col_names <- c("link") 
  
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



# scrape speeches using links ---------------------------------------------

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

page_speech_info <- function(session_page) {
  
  page_speech_info <- scrape(session_page) %>% 
    html_elements(".article_content p") %>%
    html_text2()
}

# function to scrape data from each speech page
page_info <- function(link_page) {
  
  ## `bow()` for every URL link
  session_page <- bow(link_page)
  
  ## scrape different info
  page_timestamp <- page_timestamp_info(session_page = session_page)
  
  page_title <- page_title_info(session_page = session_page)
  
  page_speech <- page_speech_info(session_page = session_page)
  
  ## combine info into a data frame
  page <- list(timestamp = page_timestamp,
               title = page_title,
               paragraph = 1:length(page_speech),
               text = page_speech) %>%
    as_tibble()

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
page_df_no_collapse <- page_df_ALL %>%
  map("result") %>%
  bind_rows()

# inspect
page_df_no_collapse



# references --------------------------------------------------------------

# https://rvest.tidyverse.org/index.html
# https://dmi3kno.github.io/polite/
# https://www.r-bloggers.com/2020/05/intro-to-polite-web-scraping-of-soccer-data-with-r/
# https://ladal.edu.au/webcrawling.html#Introduction
# https://purrr.tidyverse.org/
