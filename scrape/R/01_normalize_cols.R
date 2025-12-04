# Function to normalize columns
# df is the data frame

normalize_cols <- function(df) {
  df <- df %>% select(where(~ !all(is.na(.x))))
  names(df) <- names(df) %>% str_trim() %>% str_replace_all("\\s+", " ")
  
  # Supports Spanish and English names
  rename_map <- c(
    # títulos/autores
    "Title" = "title",
    "Book Title" = "title",
    "Título" = "title",
    "Author" = "author",
    "Autor" = "author",
    # fechas
    "Date Started" = "date_started",
    "Fecha de inicio" = "date_started",
    "Date Read" = "date_finished",
    "Fecha de lectura" = "date_finished",
    # ratings y varios
    "My Rating" = "my_rating",
    "Rating" = "my_rating",
    "Average rating" = "avg_rating",
    "Puntuación media" = "avg_rating",
    "ISBN" = "isbn",
    "ISBN13" = "isbn13",
    "Shelves" = "shelves",
    "Bookshelves" = "shelves",
    "Estanterías" = "shelves"
  )
  for (nm in names(rename_map)) {
    if (nm %in% names(df)) {
      names(df)[names(df) == nm] <- rename_map[[nm]]
    }
  }
  # Ensure that 'view' is not renamed if it already exists; 
  # if it doesn’t exist, it will be created later
  df
}

scrape_shelf_table <- function(shelf = "read",
                               max_pages = 50,
                               sleep_sec = 2,
                               wait_timeout = 20) {
  abs_url <- function(x) {
    ifelse(
      startsWith(x %||% character(), "http"),
      x,
      paste0("https://www.goodreads.com", x)
    )
  }
  nz <- function(x) if (length(x) == 0) NA_character_ else x
  
  row_selectors <- c(
    "tr.bookalike.review",        # classic
    "tr.review",                  # variant
    "tr.bookalike",               # looser
    "table#books tbody tr",       # frequent id
    "table.tableList tbody tr",   # frequent class
    "tr[id^='review_']",          # rows with id review_*
    "tr.bookRow"                  # just in case
  )
  
  pick_selector <- function(html) {
    for (sel in row_selectors) {
      n <- length(rvest::html_elements(html, sel))
      if (n > 0) return(sel)
    }
    NA_character_
  }
  
  get_txt <- function(node, selectors) {
    for (sel in selectors) {
      el <- rvest::html_element(node, sel)
      if (!rlang::is_empty(el)) {
        val <- rvest::html_text(el, trim = TRUE)
        if (length(val) == 1 && nzchar(val)) return(val)
      }
    }
    NA_character_
  }
  
  get_attr <- function(node, selectors, attr) {
    for (sel in selectors) {
      el <- rvest::html_element(node, sel)
      if (!rlang::is_empty(el)) {
        val <- rvest::html_attr(el, attr)
        if (length(val) == 1 && nzchar(val)) return(val)
      }
    }
    NA_character_
  }
  
  get_all_join <- function(node, sel, sep = "; ") {
    els <- rvest::html_elements(node, sel)
    if (length(els) == 0) return(NA_character_)
    vals <- rvest::html_text(els, trim = TRUE)
    vals <- vals[nzchar(vals)]
    if (length(vals) == 0) return(NA_character_)
    paste(unique(vals), collapse = sep)
  }
  
  parse_int <- function(txt) {
    n <- stringr::str_extract(txt %||% "", "\\d+")
    if (is.na(n)) return(NA_integer_)
    as.integer(n)
  }
  
  out <- list()
  
  for (page in seq_len(max_pages)) {
    url <- paste0(
      "https://www.goodreads.com/review/list?view=table&per_page=100",
      "&shelf=", shelf, "&page=", page
    )
    retry_nav(url)
    
    # Active wait until rows appear (or timeout)
    t0 <- Sys.time()
    rows <- NULL
    chosen_sel <- NA_character_
    repeat {
      Sys.sleep(1.0)
      src  <- remDr$getPageSource()[[1]]
      html <- xml2::read_html(src)
      chosen_sel <- pick_selector(html)
      if (!is.na(chosen_sel)) {
        rows <- rvest::html_elements(html, chosen_sel)
        if (length(rows) > 0) break
      }
      if (as.numeric(difftime(Sys.time(), t0, units = "secs")) >
          wait_timeout) break
    }
    
    # If no rows, save HTML for debugging and exit
    if (length(rows) == 0 || is.null(rows)) {
      tmp <- file.path(
        getwd(),
        sprintf("goodreads_%s_page%s.html", shelf, page)
      )
      writeLines(src %||% "<empty>", tmp, useBytes = TRUE)
      warning(sprintf(
        "No rows detected at %s (selector=%s). HTML saved to: %s",
        url, chosen_sel, tmp
      ))
      break
    }
    
    message(sprintf(
      ".. [%s p.%d] selector='%s' rows=%d",
      shelf, page, chosen_sel, length(rows)
    ))
    
    # Flexible selectors per column
    sel_title_link <- c(
      "td.field.title div.value a.bookTitle",
      "td.title a[href*='/book/']"
    )
    sel_author_a   <- "td.field.author a"
    sel_date_added <- c("td.field.date_added .value", "td.field.date_added")
    sel_date_pub   <- c(
      "td.field.date_pub .value",
      "td.field.date_pub",
      "td.field.date_published .value",
      "td.field.date_published",
      "td.field.date_pubdate .value",
      "td.field.date_pubdate"
    )
    sel_date_read  <- c(
      "td.field.date_read span.date_read_value",
      "td.field.date_read .value",
      "td.field.date_read"
    )
    sel_date_start <- c(
      "td.field.date_started span.date_started_value",
      "td.field.date_started .value",
      "td.field.date_started"
    )
    sel_num_pages  <- c(
      "td.field.num_pages div.value",
      "td.field.num_pages .value",
      "td.field.num_pages"
    )
    sel_review     <- c(
      "td.field.review div.value",
      "td.field.review .value",
      "td.field.review"
    )
    sel_review_link <- c(
      "td.field.actions a.viewLink[href*='/review/show/']",
      "a.viewLink[href*='/review/show/']",
      "a[href^='/review/show/']"
    )
    sel_shelves_a  <- "td.field.shelves div.value a.shelfLink"
    
    titles <- purrr::map_chr(
      rows, ~ get_txt(.x, sel_title_link), .progress = NULL
    )
    book_links <- purrr::map_chr(
      rows, ~ get_attr(.x, sel_title_link, "href"), .progress = NULL
    )
    authors <- purrr::map_chr(
      rows, ~ get_all_join(.x, sel_author_a), .progress = NULL
    )
    
    date_added <- purrr::map_chr(
      rows, ~ get_txt(.x, sel_date_added), .progress = NULL
    )
    date_pub <- purrr::map_chr(
      rows, ~ get_txt(.x, sel_date_pub), .progress = NULL
    )
    date_read <- purrr::map_chr(
      rows, ~ get_txt(.x, sel_date_read), .progress = NULL
    )
    date_started <- purrr::map_chr(
      rows, ~ get_txt(.x, sel_date_start), .progress = NULL
    )
    
    num_pages <- purrr::map_int(
      rows, ~ parse_int(get_txt(.x, sel_num_pages))
    )
    rating <- purrr::map_int(rows, parse_rating)
    review <- purrr::map_chr(
      rows, ~ get_txt(.x, sel_review), .progress = NULL
    )
    shelves <- purrr::map_chr(
      rows, ~ get_all_join(.x, sel_shelves_a), .progress = NULL
    )
    review_links <- purrr::map_chr(rows, function(.x) {
      lk <- get_attr(.x, sel_review_link, "href")
      if (is.na(lk) || !nzchar(lk)) {
        rid <- rvest::html_attr(.x, "id")       # e.g., "review_7443061737"
        rid_num <- stringr::str_extract(rid %||% "", "\\d+")  # "7443061737"
        if (!is.na(rid_num)) {
          paste0("/review/show/", rid_num)
        } else {
          NA_character_
        }
      } else {
        lk
      }
    })
    
    tb <- tibble::tibble(
      title         = dplyr::na_if(titles, ""),
      author        = dplyr::na_if(authors, ""),
      view          = abs_url(review_links),   # <-- your REVIEW URL
      book_url      = abs_url(book_links),     # <-- extra: book URL
      date_added    = dplyr::na_if(date_added, ""),
      date_pub      = dplyr::na_if(date_pub, ""),
      date_finished = dplyr::na_if(date_read, ""),
      date_started  = dplyr::na_if(date_started, ""),
      num_pages     = dplyr::na_if(num_pages, NA_integer_),
      my_rating     = dplyr::na_if(rating, NA_integer_),
      review        = dplyr::na_if(review, ""),
      shelves       = dplyr::na_if(shelves, ""),
      .shelf        = shelf,
      .page         = page
    ) %>%
      dplyr::mutate(
        # Title: remove odd spacing and normalize parentheses
        title = stringr::str_squish(title) %>%
          stringr::str_replace_all("\\s*\\(\\s*", " (") %>%
          stringr::str_replace_all("\\s*\\)\\s*", ")"),
        
        # Dates: strip " [edit]" and "not set"
        date_started  = date_started %>%
          stringr::str_replace("\\s*\\[edit\\]\\s*$", "") %>%
          stringr::str_squish() %>%
          dplyr::na_if("not set") %>%
          dplyr::na_if(""),
        
        date_finished = date_finished %>%
          stringr::str_replace("\\s*\\[edit\\]\\s*$", "") %>%
          stringr::str_squish() %>%
          dplyr::na_if("not set") %>%
          dplyr::na_if(""),
        
        # Shelves: remove "[edit]" and trailing separators
        shelves = shelves %>%
          stringr::str_replace_all("\\s*\\[edit\\]\\s*", "") %>%
          stringr::str_replace_all("\\s*;\\s*$", "") %>%
          stringr::str_replace_all("\\s*;\\s*;", "; ") %>%
          stringr::str_squish() %>%
          dplyr::na_if(""),
        
        # Review: if it says "Write a review" (or empty), set to NA
        review = review %>%
          stringr::str_replace("\\s*\\[edit\\]\\s*$", "") %>%
          stringr::str_squish(),
        review = dplyr::if_else(
          stringr::str_detect(tolower(review), "^write a review$") |
            review == "",
          NA_character_,
          review
        ),
        
        # Rating 0 = no rating
        my_rating = dplyr::na_if(my_rating, 0L)
      )
    
    tb <- dplyr::filter(tb, !is.na(title))
    out[[length(out) + 1]] <- tb
    
    # Probably no more pages
    if (nrow(tb) < 90) break
    Sys.sleep(sleep_sec)
  }
  
  if (length(out) == 0) return(tibble::tibble())
  dplyr::bind_rows(out)
}

# Robust rating extractor for one <tr> row
# Robust rating extractor for one <tr> row
parse_rating <- function(row) {
  cell <- rvest::html_element(row, "td.field.rating")
  if (rlang::is_empty(cell)) return(NA_integer_)
  
  # A) Best: any element with data-rating (covers <div class="stars">)
  el_with_dr <- rvest::html_element(cell, "[data-rating]")
  if (!rlang::is_empty(el_with_dr)) {
    dr <- rvest::html_attr(el_with_dr, "data-rating")
    if (!is.null(dr) && nzchar(dr) && grepl("^\\d+$", dr)) {
      return(as.integer(dr))
    }
  }
  
  # B) Count filled anchors: <a class="star on">…</a>
  on_count <- length(rvest::html_elements(cell, ".stars a.star.on"))
  if (on_count > 0) return(on_count)
  
  # C) Static stars fallback (p10 = filled)
  p10_count <- length(
    rvest::html_elements(cell, ".staticStars .staticStar.p10")
  )
  if (p10_count > 0) return(p10_count)
  
  # D) Text fallback: "[ 2 of 5 stars ]" or "2 of 5 stars"
  txt <- rvest::html_text(cell, trim = TRUE)
  m  <- stringr::str_match(txt, "\\[\\s*(\\d)\\s+of\\s+5\\s+stars\\s*\\]")
  if (!is.na(m[, 2])) return(as.integer(m[, 2]))
  m2 <- stringr::str_match(txt, "(\\d)\\s+of\\s+5\\s+stars")
  if (!is.na(m2[, 2])) return(as.integer(m2[, 2]))
  
  NA_integer_
}


# Parser flexible de fechas Goodreads
parse_gd_date <- function(x) {
  x <- str_trim(x %||% "")
  res <- ifelse(
    x == "" | is.na(x),
    NA_character_,
    x
  )
  # try different formats
  parsed <- suppressWarnings(
    parse_date_time(
      res,
      orders = c(
        "Ymd","Y-m-d","d/m/Y","m/d/Y","d.m.Y","b d Y","d b Y","b Y","Y"),
      locale = "C"
    ))
  as_date(parsed)
}
