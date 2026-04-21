library(rvest)
library(httr)
library(dplyr)
library(stringr)
library(purrr)
library(tibble)

ua <- user_agent("Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/120.0.0.0 Safari/537.36")

# ---------------------------------------------------------
# Phase 1: Fetch Airline Names and Base URLs
# ---------------------------------------------------------
cat("Fetching airline names and links...\n")
base_url <- "https://www.airlinequality.com/review-pages/a-z-airline-reviews/"

response <- GET(base_url, ua)
page <- read_html(response)

airline_nodes <- page %>% html_elements("div[id^='a2z-ldr-'] li a")

airline_names <- airline_nodes %>% html_text(trim = TRUE)
airline_hrefs <- airline_nodes %>% html_attr("href")

# Note: We are no longer appending the page size here. We handle it in Phase 2.
airline_base_urls <- paste0("https://www.airlinequality.com", airline_hrefs)

df_airline <- tibble(
  Name = airline_names,
  Base_Link = airline_base_urls
)

cat(sprintf("Successfully found %d airlines.\n", nrow(df_airline)))

# ---------------------------------------------------------
# Phase 2: Scrape the Reviews with Pagination
# ---------------------------------------------------------
table_columns <- c("Aircraft", "Type Of Traveller", "Seat Type", "Route", "Date Flown", 
                   "Seat Comfort", "Cabin Staff Service", "Food & Beverages", 
                   "Ground Service", "Inflight Entertainment", "Wifi & Connectivity", 
                   "Value For Money", "Recommended")

scrape_airline_reviews <- function(airline_name, base_link) {
  cat(sprintf("\nScraping %s...\n", airline_name))
  
  page_num <- 1
  all_pages_data <- list()
  
  while (TRUE) {
    # Dynamically construct the URL for page 1, 2, 3, etc.
    page_url <- paste0(base_link, "/page/", page_num, "/?sortby=post_date%3ADesc&pagesize=100")
    cat(sprintf("  -> Fetching page %d\n", page_num))
    
    page <- tryCatch({
      res <- GET(page_url, ua)
      # If the page doesn't exist (e.g., 404), stop looping
      if (status_code(res) != 200) return(NULL)
      read_html(res)
    }, error = function(e) {
      cat(sprintf("    [!] Error on page %d: %s\n", page_num, e$message))
      return(NULL)
    })
    
    # Break the loop if the page failed to load
    if (is.null(page)) break
    
    reviews <- page %>% html_elements("article[itemprop='review']")
    
    # Break the loop if there are no more reviews on this page
    if (length(reviews) == 0) {
      cat("  -> No more reviews found. Moving to next airline.\n")
      break
    }
    
    review_data <- map_dfr(reviews, function(review) {
      rating <- review %>% html_element("span[itemprop='ratingValue']") %>% html_text(trim = TRUE)
      title <- review %>% html_element("h2.text_header") %>% html_text(trim = TRUE) %>% str_remove_all("\"")
      review_date <- review %>% html_element("time[itemprop='datePublished']") %>% html_text(trim = TRUE)
      
      text_content <- review %>% html_element("div.text_content") %>% html_text(trim = TRUE)
      verified <- str_detect(text_content, "(?i)Trip Verified")
      review_text <- str_remove(text_content, "^.*\\|\\s*")
      
      tab_data <- setNames(as.list(rep(NA, length(table_columns))), table_columns)
      table_rows <- review %>% html_elements("table.review-ratings tr")
      
      for (row in table_rows) {
        header <- row %>% html_element("td.review-rating-header") %>% html_text(trim = TRUE)
        if (header %in% table_columns) {
          stars_cells <- row %>% html_elements("td.review-rating-stars span.star.fill")
          value_cell <- row %>% html_element("td.review-value") %>% html_text(trim = TRUE)
          
          if (length(stars_cells) > 0) {
            tab_data[[header]] <- length(stars_cells)
          } else if (!is.na(value_cell)) {
            tab_data[[header]] <- value_cell
          }
        }
      }
      
      tibble(
        `Airline Name` = airline_name,
        Overall_Rating = rating,
        Review_Title = title,
        `Review Date` = review_date,
        Verified = verified,
        Review = review_text
      ) %>% bind_cols(as_tibble(tab_data))
    })
    
    # Store the scraped data for this page
    all_pages_data[[page_num]] <- review_data
    
    # Increment page number and pause to avoid getting blocked
    page_num <- page_num + 1
    Sys.sleep(runif(1, min = 1.5, max = 3.5)) 
  }
  
  # Combine all pages for this airline into one tibble
  return(bind_rows(all_pages_data))
}

# Iterate over all airlines (Tip: test with `head(df_airline, 2)` first)
all_reviews <- map2_dfr(df_airline$Name, df_airline$Base_Link, scrape_airline_reviews)

# ---------------------------------------------------------
# Phase 3: Export to CSV
# ---------------------------------------------------------
write.csv(all_reviews, "Airline_review_full.csv", row.names = FALSE)
cat("\nScraping completed! Data saved to Airline_review_full.csv\n")