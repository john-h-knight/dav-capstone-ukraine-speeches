# references
# https://rvest.tidyverse.org/index.html
# https://dmi3kno.github.io/polite/
# https://www.r-bloggers.com/2020/05/intro-to-polite-web-scraping-of-soccer-data-with-r/
# https://ladal.edu.au/webcrawling.html#Introduction
# https://purrr.tidyverse.org/

library(tidyverse)
library(rvest)
library(polite)



# functions to scrape the list of speeches from /news/speeches
list_date_info <- function(session_list) {
  
  list_date_info <- scrape(session_list) %>%
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

# vector for the number of pages after filtering for the 1st month
page_numbers <- 1:7

# define base url that pages numbers will be added to
base_url <- "https://www.president.gov.ua/en/news/speeches?date-from=24-02-2022&date-to=24-03-2022&page="

# combine base and page number urls
paging_urls <- paste0(base_url, page_numbers)

# inspect
paging_urls

# create tibble for functions
paging_urls_df <- tibble("page" = c(1:7),
                         "link" = paging_urls
                         )

# function to scrape data from each page of the list
list_1mo_info <- function(link, page) {
  
  page <- rlang::enquo(page)
  ## `bow()` for every URL link
  session_list <- bow(link)
  
  ## scrape different info
  list_date <- list_date_info(session_list = session_list)
  
  list_title <- list_title_info(session_list = session_list)
  
  list_link <- list_link_info(session_list = session_list)
  
  ## combine info into a data frame
  results <- list(list_date, list_title, list_link)
  col_names <- c("date", "title", "link") 
  
  list_1mo <- results %>% 
    reduce(cbind) %>% 
    as_tibble() %>% 
    set_names(col_names)
  
  return(list_1mo)
}

# wrap function for easier debugging
safe_list_1mo_info <- safely(list_1mo_info)

# iterate across each page of the list
# list_1mo_df_ALL <- map2(.x = paging_urls_df$link, .y = paging_urls_df$page,
#                                  ~ safe_list_1mo_info(link = .x, page = .y))

# iterate across each page of the list, switching to map() 
list_1mo_df_ALL <- map(.x = paging_urls_df$link,
                       ~ safe_list_1mo_info(link = .x))

# check to see if any failed
list_1mo_df_ALL %>% 
  map("error") %>% 
  purrr::discard(~is.null(.))

# if no errors (list length of 0), combine into one df
list_1mo_df <- list_1mo_df_ALL %>% 
  map("result") %>% 
  bind_rows()

# inspect
list_1mo_df





# # test new functions for scraping speech pages
# test_url <- "https://www.president.gov.ua/en/news/zvernennya-prezidenta-ukrayini-volodimira-zelenskogo-do-ucha-73801"
# 
# test_session <- bow(test_url)
# 
# test_page_date_info <- scrape(test_session) %>%
#   html_element(".article p") %>%
#   html_text()
# 
# test_page_date_info
# 
# test_page_title_info <- scrape(test_session) %>% 
#   html_element(".article h1") %>%
#   html_text()
# 
# test_page_title_info
# 
# test_page_body_info <- scrape(test_session) %>% 
#   html_elements(".article_content p") %>%
#   html_text2()
# 
# test_page_body_info
# 
# # combine separate strings into one
# collapsed <- test_page_body_info %>%
#   str_c(collapse = ", ")
# 
# collapsed





# create functions to scrape info from a speech page
page_date_info <- function(session_page) {
  
  page_date_info <- scrape(session_page) %>%
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
page_1mo_info <- function(link_page) {
  
  ## `bow()` for every URL link
  session_page <- bow(link_page)
  
  ## scrape different info
  page_date <- page_date_info(session_page = session_page)
  
  page_title <- page_title_info(session_page = session_page)
  
  page_body <- page_body_info(session_page = session_page)
  
  ## combine info into a data frame
  results_page <- list(page_date, page_title, page_body)
  col_names_page <- c("date", "title", "speech") 
  
  page_1mo <- results_page %>% 
    reduce(cbind) %>% 
    as_tibble() %>% 
    set_names(col_names_page)
  
  return(page_1mo)
}

# wrap function for easier debugging
safe_page_1mo_info <- safely(page_1mo_info)

# iterate across each link
# list_1mo_df_ALL <- map2(.x = paging_urls_df$link, .y = paging_urls_df$page,
#                                  ~ safe_list_1mo_info(link = .x, page = .y))

# iterate across each link, switching to map() 
page_1mo_df_ALL <- map(.x = list_1mo_df$link,
                       ~ safe_page_1mo_info(link_page = .x))

# check to see if any failed
page_1mo_df_ALL %>% 
  map("error") %>% 
  purrr::discard(~is.null(.))

# if no errors (list length of 0), combine into one df
page_1mo_df <- page_1mo_df_ALL %>% 
  map("result") %>% 
  bind_rows()

# inspect
page_1mo_df

# export
write_csv(page_1mo_df, file = 'data/page_1mo_df.csv')

