# R/03_scrape_book_metadata.R
# Helpers to scrape genres + language from a Goodreads book page
# Requires: xml2, rvest, stringr, purrr (and `retry_nav`, `remDr` if using Selenium fallback)

# --- JSON-LD helper ---------------------------------------------------------

# Try to extract language (and optionally genres) from JSON-LD <script> blocks
.extract_ld_metadata <- function(html) {
  nodes <- rvest::html_elements(html, xpath = "//script[@type='application/ld+json']")
  if (!length(nodes)) return(NULL)
  
  lang <- NA_character_
  genres_vec <- character()
  
  for (node in nodes) {
    txt <- rvest::html_text2(node)
    if (!nzchar(txt)) next
    
    obj <- tryCatch(
      jsonlite::fromJSON(txt),
      error = function(e) NULL
    )
    if (is.null(obj)) next
    
    # Sometimes it's a list of things, sometimes a single object
    objs <- if (is.list(obj) && !is.null(names(obj))) list(obj) else obj
    
    for (o in objs) {
      if (!is.list(o)) next
      t <- o[["@type"]]
      if (is.null(t)) next
      
      # We care about Book-like types
      t_chr <- if (is.character(t)) t else as.character(t)
      if (!any(t_chr %in% c("Book", "BookSeries", "CreativeWork"))) next
      
      # Language
      cand_lang <- o[["inLanguage"]]
      if (is.null(cand_lang)) cand_lang <- NA_character_
      cand_lang <- as.character(cand_lang)[1]
      if (.lang_candidate_ok(cand_lang)) {
        lang <- cand_lang
      }
      
      # Genres (optional, not all pages have them)
      g <- o[["genre"]]
      if (!is.null(g)) {
        g_chr <- unique(as.character(unlist(g)))
        g_chr <- g_chr[nzchar(g_chr)]
        genres_vec <- unique(c(genres_vec, g_chr))
      }
    }
  }
  
  if (is.na(lang) && !length(genres_vec)) return(NULL)
  
  list(
    language   = if (!.lang_candidate_ok(lang)) NA_character_ else lang,
    genres_vec = genres_vec
  )
}

# Remove zero-width & control formatting chars
clean_invisibles <- function(x) {
  stringr::str_replace_all(x, "\\p{Cf}", "") |>
    trimws()
}

# --- helpers just for language ---------------------------------------------

# Accept things like:
#   English
#   Spanish; Castilian
#   Portuguese (Brazil)
#   Chinese — Simplified
.lang_candidate_ok <- function(x) {
  if (is.na(x) || !nzchar(x)) return(FALSE)
  x <- stringr::str_squish(x)
  
  # Too long or contains obviously non-language punctuation => reject
  if (nchar(x) > 60) return(FALSE)
  if (grepl("[{}\\[\\]<>@#%=_~|]", x)) return(FALSE)
  
  # Allowed: letters (any script), spaces, apostrophes, hyphens/dashes,
  # semicolons, commas, parentheses.
  ok_chars <- "^[\\p{L}' ,;()–—-]+$"
  if (!grepl(ok_chars, x, perl = TRUE)) return(FALSE)
  
  # Must contain at least one letter sequence (2+)
  if (!grepl("[\\p{L}]{2,}", x, perl = TRUE)) return(FALSE)
  
  TRUE
}

# Try to read "Language" from various layouts
.extract_language <- function(html) {
  lang_labels <- c("Language", "Idioma", "Língua", "Langue", "Sprache", "Lingua")
  
  ## 0) Look for semantic markup (itemprop="inLanguage")
  lang_nodes_sem <- rvest::html_elements(
    html,
    xpath = "//*[@itemprop='inLanguage' or @itemprop='bookLanguage']"
  )
  if (length(lang_nodes_sem)) {
    cand <- lang_nodes_sem |>
      rvest::html_text2() |>
      stringr::str_squish() |>
      clean_invisibles()
    cand <- cand[nzchar(cand)]
    if (length(cand)) {
      hit <- cand[vapply(cand, .lang_candidate_ok, logical(1))][1]
      if (!is.na(hit)) return(hit)
    }
  }
  
  ## 1) Strict: EditionDetails → DescListItem where dt == Language
  xp_strict <- paste0(
    "//div[contains(@class,'EditionDetails')]",
    "//dl[contains(@class,'DescList')]",
    "//div[contains(@class,'DescListItem')][.//dt[normalize-space() = '",
    paste(lang_labels, collapse = "' or normalize-space() = '"),
    "']]//dd"
  )
  
  cand <- rvest::html_elements(html, xpath = xp_strict) |>
    rvest::html_text2() |>
    stringr::str_squish() |>
    clean_invisibles()
  cand <- cand[nzchar(cand)]
  if (length(cand)) {
    hit <- cand[vapply(cand, .lang_candidate_ok, logical(1))][1]
    if (!is.na(hit)) return(hit)
  }
  
  ## 2) New-ish layout but without EditionDetails wrapper
  xp_desc_any <- paste0(
    "//dl[contains(@class,'DescList')]",
    "//div[contains(@class,'DescListItem')][.//dt[normalize-space() = '",
    paste(lang_labels, collapse = "' or normalize-space() = '"),
    "']]//dd"
  )
  cand <- rvest::html_elements(html, xpath = xp_desc_any) |>
    rvest::html_text2() |>
    stringr::str_squish() |>
    clean_invisibles()
  cand <- cand[nzchar(cand)]
  if (length(cand)) {
    hit <- cand[vapply(cand, .lang_candidate_ok, logical(1))][1]
    if (!is.na(hit)) return(hit)
  }
  
  ## 3) Generic dt → nearest dd anywhere (but still validated)
  dts <- rvest::html_elements(html, xpath = "//dt")
  if (length(dts)) {
    for (dt in dts) {
      lbl <- dt |>
        rvest::html_text2() |>
        stringr::str_squish() |>
        clean_invisibles()
      if (tolower(lbl) %in% tolower(lang_labels)) {
        dd <- rvest::html_element(dt, xpath = "following-sibling::dd[1]")
        if (!is.null(dd)) {
          val <- rvest::html_elements(dd, xpath = ".//* | self::dd") |>
            rvest::html_text2() |>
            stringr::str_squish() |>
            clean_invisibles()
          val <- val[nzchar(val)]
          if (length(val)) {
            hit <- val[vapply(val, .lang_candidate_ok, logical(1))][1]
            if (!is.na(hit)) return(hit)
          }
        }
      }
    }
  }
  
  ## 4) Legacy boxes: bookDataBox / details
  legacy_nodes <- rvest::html_elements(
    html,
    xpath = "//*[@id='bookDataBox' or @id='details' or contains(@class,'BookDataBox')]"
  )
  if (length(legacy_nodes)) {
    legacy_txt <- legacy_nodes |>
      rvest::html_text2() |>
      paste(collapse = "  ") |>
      clean_invisibles()
    m <- stringr::str_match(
      legacy_txt,
      "(?is)\\b(Edition\\s+Language|Language)\\s*[:\\-]?\\s*([\\p{L}' ,;()–—-]{2,60})"
    )
    if (!all(is.na(m))) {
      cand <- stringr::str_squish(m[, ncol(m)]) |>
        clean_invisibles()
      if (.lang_candidate_ok(cand)) return(cand)
    }
  }
  
  ## 5) Ultra-fallback: scan full page text for "Language: ...".
  full_txt <- html |>
    rvest::html_text2() |>
    stringr::str_squish() |>
    clean_invisibles()
  
  m2 <- stringr::str_match(
    full_txt,
    "(?is)\\bLanguage\\s*[:\\-]?\\s*([\\p{L}' ,;()–—-]{2,60})"
  )
  if (!all(is.na(m2))) {
    cand <- stringr::str_squish(m2[, ncol(m2)]) |>
      clean_invisibles()
    if (.lang_candidate_ok(cand)) return(cand)
  }
  
  NA_character_
}


# Scrape genres + language from a book page (new & legacy layouts)
scrape_book_metadata <- function(book_url, wait_timeout = 15) {
  library(xml2); library(rvest); library(stringr); library(purrr)
  
  trim_vec <- function(x) x %>% rvest::html_text2() %>% trimws()
  uniq_collapse <- function(x) {
    x <- unique(x[nzchar(x)])
    if (length(x) == 0) NA_character_ else paste(x, collapse = "; ")
  }
  
  extract_from_doc <- function(html) {
    # 1) Try JSON-LD first (fast + clean)
    ld <- .extract_ld_metadata(html)
    
    # GENRES from DOM (will be overridden/augmented by JSON-LD if present)
    genres <- html %>%
      html_elements("[data-testid='genresList'] .Button__labelItem") %>%
      trim_vec()
    if (!length(genres)) {
      genres <- html %>%
        html_elements(
          ".BookPageMetadataSection__genreButton .Button__labelItem, \
         .BookPageMetadataSection__genres a.Button--tag .Button__labelItem, \
         a.bookPageGenreLink, \
         .bookPageGenres a"
        ) %>%
        trim_vec()
    }
    
    # Use language from JSON-LD if available; otherwise fall back to DOM/XPath
    language <- NA_character_
    if (!is.null(ld) && !is.na(ld$language)) {
      language <- ld$language
    } else {
      language <- .extract_language(html)
    }
    
    # Merge genres: DOM genres plus any JSON-LD genres we saw
    genres_vec <- unique(genres[nzchar(genres)])
    if (!is.null(ld) && length(ld$genres_vec)) {
      genres_vec <- unique(c(genres_vec, ld$genres_vec))
    }
    
    list(
      genres_vec = genres_vec,
      genres     = uniq_collapse(genres_vec),
      language   = if (is.na(language) || !nzchar(language)) NA_character_ else language
    )
  }
  
  # FAST PATH: simple read_html
  fast_ok <- FALSE
  res <- list(genres_vec = character(), genres = NA_character_, language = NA_character_)
  try({
    html_fast <- xml2::read_html(book_url)
    res_fast  <- extract_from_doc(html_fast)
    if (length(res_fast$genres_vec) > 0 || !is.na(res_fast$language)) {
      res <- res_fast
      fast_ok <- TRUE
    }
  }, silent = TRUE)
  if (fast_ok) return(res)
  
  # FALLBACK: Selenium (requires global retry_nav() and remDr)
  retry_nav(book_url)
  try(remDr$executeScript("window.scrollTo(0, document.body.scrollHeight);"),
      silent = TRUE)
  
  t0 <- Sys.time(); html_slow <- NULL
  repeat {
    Sys.sleep(0.7)
    src <- remDr$getPageSource()[[1]]
    if (!length(src)) next
    html_slow <- xml2::read_html(src)
    
    has_genres <- length(html_elements(
      html_slow,
      "[data-testid='genresList'], .BookPageMetadataSection__genres, \
       a.bookPageGenreLink, .bookPageGenres a"
    )) > 0
    has_details <- length(html_elements(html_slow, ".EditionDetails, dl.DescList")) > 0
    has_legacy  <- length(html_elements(html_slow, "#bookDataBox, #details")) > 0
    
    if (has_genres || has_details || has_legacy) break
    if (as.numeric(difftime(Sys.time(), t0, units = "secs")) > wait_timeout) break
  }
  if (is.null(html_slow)) {
    return(list(genres_vec = character(), genres = NA_character_, language = NA_character_))
  }
  
  extract_from_doc(html_slow)
}

# Safe wrapper
safe_metadata <- purrr::possibly(
  scrape_book_metadata,
  otherwise = list(
    genres_vec = character(),
    genres     = NA_character_,
    language   = NA_character_
  ),
  quiet = TRUE
)

# Simple in-memory cache to avoid re-scraping URLs in a single run
if (!exists(".meta_cache", inherits = FALSE)) {
  .meta_cache <- new.env(parent = emptyenv())
}

get_meta_cached <- function(url) {
  url <- url %||% ""
  if (!nzchar(url)) {
    return(safe_metadata(url))
  }
  key <- url
  if (!exists(key, envir = .meta_cache, inherits = FALSE)) {
    .meta_cache[[key]] <- safe_metadata(url)
  }
  .meta_cache[[key]]
}

# Optional: quick debug helper
test_book_metadata <- function(book_url, save_html = TRUE) {
  library(xml2); library(rvest)
  
  cat("\n=== Testing:", book_url, "===\n")
  
  ok_fast <- FALSE
  html_fast <- NULL
  try({
    html_fast <- read_html(book_url)
    g_fast <- html_elements(html_fast, "[data-testid='genresList'] .Button__labelItem")
    d_fast <- html_elements(html_fast, ".EditionDetails .DescListItem")
    cat("Fast path: genres nodes =", length(g_fast),
        "| edition items =", length(d_fast), "\n")
    ok_fast <- (length(g_fast) > 0 || length(d_fast) > 0)
  }, silent = TRUE)
  
  if (!ok_fast) {
    cat("Fast path did not find nodes. Trying Selenium…\n")
    retry_nav(book_url)
    try(remDr$executeScript(
      "window.scrollTo(0, document.body.scrollHeight);"), silent = TRUE)
    Sys.sleep(1.5)
    src <- remDr$getPageSource()[[1]]
    html_fast <- read_html(src)
    g_sel <- html_elements(html_fast, "[data-testid='genresList'] .Button__labelItem")
    d_sel <- html_elements(html_fast, ".EditionDetails .DescListItem")
    cat("Selenium path: genres nodes =", length(g_sel),
        "| edition items =", length(d_sel), "\n")
  }
  
  got <- scrape_book_metadata(book_url, wait_timeout = 12)
  print(got)
  
  if (save_html && (length(got$genres_vec) == 0 && is.na(got$language))) {
    fn <- file.path(getwd(), sprintf("debug_book_%s.html",
                                     gsub("\\W+", "_", basename(book_url))))
    writeLines(as.character(html_fast), fn, useBytes = TRUE)
    cat("Saved HTML for inspection:", fn, "\n")
  }
  
  invisible(got)
}
