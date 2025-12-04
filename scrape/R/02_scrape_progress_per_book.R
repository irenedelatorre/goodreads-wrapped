# Convert NA to "null" (text) for {k:v}-style formatting
null_or_num <- function(x) {
  ifelse(is.na(x), "null", as.character(x))
}

null_or_str <- function(x) {
  ifelse(
    is.na(x) | x == "",
    "null",
    paste0('"', stringr::str_replace_all(x, '"', '\\"'), '"')
  )
}

# scrape progress from individual pages

scrape_reading_progress <- function(view_url, wait_timeout = 15) {
  library(xml2); library(rvest); library(stringr)
  library(dplyr); library(purrr); library(tibble)
  
  abs_url <- function(x) ifelse(
    startsWith(x %||% character(), "http"),
    x, paste0("https://www.goodreads.com", x)
  )
  
  # Navigate to the review page (your session is already logged in)
  retry_nav(view_url)
  
  # Simple wait until the timeline renders
  t0 <- Sys.time(); nodes <- list()
  repeat {
    Sys.sleep(0.7)
    src  <- remDr$getPageSource()[[1]]
    html <- read_html(src)
    nodes <- html_elements(
      html, ".readingTimeline__row .readingTimeline__text"
    )
    if (length(nodes) > 0) break
    if (as.numeric(difftime(Sys.time(), t0, units = "secs")) >
        wait_timeout) break
  }
  
  if (length(nodes) == 0) {
    # No timeline on this page: return empty tibble
    return(tibble(
      date = character(), page = integer(), percent = numeric(),
      event = character(), status_url = character(), view = character()
    ))
  }
  
  # Small parsing helpers
  parse_date <- function(txt) {
    # take only the leading "Month DD, YYYY"
    d <- str_match(txt, "^([A-Za-z]+\\s+\\d{1,2},\\s+\\d{4})")[, 2]
    d %||% NA_character_
  }
  parse_page <- function(txt) {
    n <- str_match(txt, "page\\s+(\\d+)")[, 2]
    ifelse(is.na(n), NA_integer_, as.integer(n))
  }
  parse_pct <- function(txt) {
    p <- str_match(txt, "(\\d+(?:\\.\\d+)?)%")[, 2]
    ifelse(is.na(p), NA_real_, as.numeric(p))
  }
  parse_event <- function(txt) {
    case_when(
      str_detect(txt, regex("\\bStarted Reading\\b", ignore_case = TRUE))
      ~ "started",
      str_detect(txt, regex("\\bFinished Reading\\b", ignore_case = TRUE))
      ~ "finished",
      str_detect(txt, regex("\\bShelved as\\b", ignore_case = TRUE))
      ~ "shelved-as",
      str_detect(txt, regex("\\bShelved\\b", ignore_case = TRUE))
      ~ "shelved",
      TRUE ~ NA_character_
    )
  }
  
  # Extract each timeline row
  res <- map_dfr(nodes, function(node) {
    full <- html_text(node, trim = TRUE) %>% str_squish()
    tibble(
      date       = parse_date(full),
      page       = parse_page(full),
      percent    = parse_pct(full),
      event      = parse_event(full),
      status_url = html_attr(
        html_element(node, "a[href*='/user_status/show/']"), "href"
      ) %>% abs_url()
    )
  }) %>%
    mutate(view = view_url)
  
  res
}


# Function to scrape progress per book
# url is saved in df, in the `view` column 
# Valid JSON (array of objects with quoted keys)
format_progress_json <- function(df) {
  if (is.null(df) || nrow(df) == 0) return("[]")
  # Export only relevant columns; NA -> null
  jsonlite::toJSON(
    df %>% dplyr::select(date, page, percent, event),
    auto_unbox = TRUE,
    na = "null"
  )
}

