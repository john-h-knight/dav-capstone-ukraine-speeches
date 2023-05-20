# references
# https://rvest.tidyverse.org/index.html
# https://dmi3kno.github.io/polite/
# https://www.r-bloggers.com/2020/05/intro-to-polite-web-scraping-of-soccer-data-with-r/
# https://ladal.edu.au/webcrawling.html#Introduction
# https://purrr.tidyverse.org/

library(tidyverse)
library(rvest)
library(polite)



# create functions to scrape info from a page
speech_date_info <- function(session) {
  
  speech_date_info <- scrape(session) %>%
    html_elements(".item_stat_headline p") %>%
    html_text()
}

speech_title_info <- function(session) {
  
  speech_title_info <- scrape(session) %>% 
    html_elements(".item_stat_headline h3") %>%
    html_text()
}

speech_link_info <- function(session) {
  
  speech_link_info <- scrape(session) %>% 
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

# function to scrape data from each link
speech_1mo_info <- function(link, page) {
  
  page <- rlang::enquo(page)
  ## `bow()` for every URL link
  session <- bow(link)
  
  ## scrape different info
  speech_date <- speech_date_info(session = session)
  
  speech_title <- speech_title_info(session = session)
  
  speech_link <- speech_link_info(session = session)
  
  ## combine info into a data frame
  results <- list(speech_date, speech_title, speech_link)
  col_names <- c("date", "title", "link") 
  
  speech_1mo <- results %>% 
    reduce(cbind) %>% 
    as_tibble() %>% 
    set_names(col_names)
  
  return(speech_1mo)
}

# wrap function for easier debugging
safe_speech_1mo_info <- safely(speech_1mo_info)

# iterate across each page link
speech_1mo_df_ALL <- map2(.x = paging_urls_df$link, .y = paging_urls_df$page,
                                 ~ safe_speech_1mo_info(link = .x, page = .y))

# check to see if any failed, list is length 0, no errors
speech_1mo_df_ALL %>% 
  map("error") %>% 
  purrr::discard(~is.null(.))

# since there were no errors, combine into one df
speech_1mo_df <- speech_1mo_df_ALL %>% 
  map("result") %>% 
  bind_rows()