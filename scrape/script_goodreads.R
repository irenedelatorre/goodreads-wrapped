# From ChatGPT:
# ¡Hecho! Te dejo un script en R que extrae tu biblioteca de Goodreads
# incluyendo “Date started” y “Date finished” desde tu vista de “My Books” (no
# del CSV oficial), estando logueada. Usa RSelenium para abrir un Chrome real,
# de forma que puedas iniciar sesión (incluso si tu librería no es pública).
#
# Cómo funciona:
#
# Abre Chrome con RSelenium → 2) Te lleva a Goodreads para que inicies sesión →
# 3) Recorre tus estanterías (“read”, “currently-reading” y “to-read”) en vista
# tabla y per_page=100, raspa la tabla visible (que suele incluir “Date Started”
# y “Date Read/Finished” si las tienes guardadas) → 4) Une todo y exporta un CSV
# con title, author, date_started, date_finished, etc.
#
# Requisitos previos (una vez):
# install.packages(c("RSelenium","rvest","xml2","dplyr","purrr","stringr","readr","lubridate","tidyr"))
# Asegúrate de tener Google Chrome instalado. Si RSelenium no arranca por
# versiones, mira el comentario dentro del script.

install.packages(c("RSelenium","rvest","xml2","dplyr","purrr","stringr","readr","lubridate","tidyr","binman","wdman"))

# =========================================================
# Goodreads -> CSV con fechas de lectura (R + RSelenium + Firefox)
# =========================================================
suppressPackageStartupMessages({
  library(RSelenium)
  library(rvest)
  library(xml2)
  library(dplyr)
  library(purrr)
  library(stringr)
  library(readr)
  library(lubridate)
  library(tidyr)
  library(jsonlite)
  library(tibble)
})

# -----------------------------
# 0) Utilidades
# -----------------------------
`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a

safe_onexit <- function(expr) {
  # Evita errores si ya está cerrado
  try(expr, silent = TRUE)
}

# -----------------------------
# 1) Arranque Selenium (Firefox)
# -----------------------------
message(">> Iniciando Selenium (Firefox)...")
puerto <- as.integer(4569)
rD <- rsDriver(
  browser    = "firefox",
  geckover   = "latest",     # si da guerra, usa una fija: "0.35.0"
  phantomver = NULL,
  check      = FALSE,
  verbose    = FALSE,
  port       = puerto,   # usa otro puerto libre
)
remDr <- rD$client
remDr$open()

# Cierre seguro incluso si el script falla más adelante
on.exit({
  message("\n>> Cerrando sesión y servidor Selenium...")
  safe_onexit(remDr$close())
  safe_onexit(rD$server$stop())
}, add = TRUE)

# Helpers de navegación
go <- function(u) {
  stopifnot(is.character(u), length(u) == 1L, !is.na(u), nzchar(trimws(u)))
  remDr$navigate(trimws(u))
}
retry_nav <- function(u, attempts = 3, wait = 2) {
  for (i in seq_len(attempts)) {
    ok <- try({ go(u); TRUE }, silent = TRUE)
    if (isTRUE(ok)) return(invisible(TRUE))
    Sys.sleep(wait)
  }
  stop("No se pudo navegar a: ", u)
}

# -----------------------------
# 2) Login manual
# -----------------------------
login_url <- "https://www.goodreads.com/user/sign_in"
mybooks_url <- "https://www.goodreads.com/review/list?view=table&per_page=100&shelf=all"

retry_nav(login_url)

message("\n>>> Inicia sesión en Goodreads en la ventana de Firefox (usuario/contraseña o SSO).")
message("    Cuando termines, el script detectará automáticamente que estás dentro...")

# Verificación rápida: ir a My Books (vista tabla, 100 por página)

wait_until_logged_in <- function(timeout = 300, poll = 3) {
  t0 <- Sys.time()
  repeat {
    # intenta ir a My Books (si no estás logueada, te redirige a sign_in)
    retry_nav(mybooks_url)
    Sys.sleep(2)
    
    # comprueba URL actual
    curr <- try(remDr$getCurrentUrl()[[1]], silent = TRUE)
    if (!inherits(curr, "try-error")) {
      # si NO contiene sign_in, probablemente ya estás dentro
      not_signin <- !grepl("sign_in", curr, fixed = TRUE)
    } else {
      not_signin <- FALSE
    }
    
    # comprueba si hay tabla en la página
    src <- try(remDr$getPageSource()[[1]], silent = TRUE)
    has_table <- FALSE
    if (!inherits(src, "try-error")) {
      html <- xml2::read_html(src)
      has_table <- length(rvest::html_elements(html, "table")) > 0
    }
    
    if (isTRUE(not_signin) && isTRUE(has_table)) {
      message("✔️ Sesión detectada. Continuamos.")
      break
    }
    
    if (as.numeric(difftime(Sys.time(), t0, units = "secs")) > timeout) {
      stop("No se detectó el login en el tiempo previsto (", timeout, " s).")
    }
    Sys.sleep(poll)
  }
}

wait_until_logged_in(timeout = 300, poll = 3)
Sys.sleep(2)  # pequeño margen

# -----------------------------
# 3) Normalización de columnas
# -----------------------------
normalize_cols <- function(df) {
  df <- df %>% select(where(~ !all(is.na(.x))))
  names(df) <- names(df) %>% str_trim() %>% str_replace_all("\\s+", " ")
  
  # Soportar nombres en inglés y español
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
  # Garantiza que 'view' no se renombre si ya existe; si no existe, se creará luego
  df
}

# ---------------------------------------------------------
# 4 scrape_shelf_table ROBUSTA (autodetección + espera + debug)
# ---------------------------------------------------------
scrape_shelf_table <- function(shelf = "read", max_pages = 50, sleep_sec = 2, wait_timeout = 20) {
  abs_url <- function(x) ifelse(startsWith(x %||% character(), "http"), x, paste0("https://www.goodreads.com", x))
  nz <- function(x) if (length(x) == 0) NA_character_ else x
  
  row_selectors <- c(
    "tr.bookalike.review",         # clásico
    "tr.review",                   # variante
    "tr.bookalike",                # más laxo
    "table#books tbody tr",        # id frecuente
    "table.tableList tbody tr",    # clase frecuente
    "tr[id^='review_']",           # filas con id review_*
    "tr.bookRow"                   # por si acaso
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
  parse_rating <- function(node) {
    # 1) número visible
    t1 <- get_txt(node, c("td.field.rating .value", "td.field.rating"))
    if (!is.na(t1)) {
      n <- stringr::str_extract(t1, "\\d+")
      if (!is.na(n)) return(as.integer(n))
    }
    # 2) atributo data-rating en estrellas
    dr <- get_attr(node, c("td.field.rating span.staticStars", "td.field.rating span.rating"), "data-rating")
    if (!is.na(dr) && grepl("^\\d+$", dr)) return(as.integer(dr))
    # 3) fallback
    anytxt <- get_txt(node, c("td.field.rating *"))
    n2 <- stringr::str_extract(anytxt %||% "", "\\d+")
    if (!is.na(n2)) return(as.integer(n2))
    NA_integer_
  }
  
  out <- list()
  
  for (page in seq_len(max_pages)) {
    url <- paste0(
      "https://www.goodreads.com/review/list?view=table&per_page=100&shelf=",
      shelf,
      "&page=",
      page
      )
    retry_nav(url)
    
    # Espera activa hasta que aparezcan filas (o timeout)
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
      if (as.numeric(difftime(Sys.time(), t0, units = "secs")) > wait_timeout) break
    }
    
    # Si no hay filas, guardar HTML para diagnóstico y salir
    if (length(rows) == 0 || is.null(rows)) {
      tmp <- file.path(getwd(), sprintf("goodreads_%s_page%s.html", shelf, page))
      writeLines(src %||% "<empty>", tmp, useBytes = TRUE)
      warning(sprintf(
        "No se detectaron filas en %s (selector=%s). HTML guardado en: %s",
        url,
        chosen_sel,
        tmp))
      break
    }
    
    message(sprintf(
      ".. [%s p.%d] selector='%s' filas=%d",
      shelf,
      page,
      chosen_sel,
      length(rows)
      ))
    
    # Selectores flexibles por columna
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
    
    titles <- purrr::map_chr(rows, ~ get_txt(.x, sel_title_link), .progress = NULL)
    book_links  <- purrr::map_chr(rows, ~ get_attr(.x, sel_title_link, "href"), .progress = NULL)
    authors <- purrr::map_chr(rows, ~ get_all_join(.x, sel_author_a), .progress = NULL)
    
    date_added   <- purrr::map_chr(rows, ~ get_txt(.x, sel_date_added), .progress = NULL)
    date_pub     <- purrr::map_chr(rows, ~ get_txt(.x, sel_date_pub), .progress = NULL)
    date_read    <- purrr::map_chr(rows, ~ get_txt(.x, sel_date_read), .progress = NULL)
    date_started <- purrr::map_chr(rows, ~ get_txt(.x, sel_date_start), .progress = NULL)
    
    num_pages <- purrr::map_int(rows, ~ parse_int(get_txt(.x, sel_num_pages)))
    rating    <- purrr::map_int(rows, ~ parse_rating(.x))
    review    <- purrr::map_chr(rows, ~ get_txt(.x, sel_review), .progress = NULL)
    shelves   <- purrr::map_chr(rows, ~ get_all_join(.x, sel_shelves_a), .progress = NULL)
    review_links <- purrr::map_chr(rows, function(.x) {
      lk <- get_attr(.x, sel_review_link, "href")
      if (is.na(lk) || !nzchar(lk)) {
        rid <- rvest::html_attr(.x, "id")                     # ej: "review_7443061737"
        rid_num <- stringr::str_extract(rid %||% "", "\\d+")  # "7443061737"
        if (!is.na(rid_num)) paste0("/review/show/", rid_num) else NA_character_
      } else lk
    })
    
    tb <- tibble::tibble(
      title         = dplyr::na_if(titles, ""),
      author        = dplyr::na_if(authors, ""),
      view          = abs_url(review_links),   # <-- ahora es la URL de TU REVIEW
      book_url      = abs_url(book_links),     # <-- extra: URL del libro
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
        # Título: eliminar huecos raros y normalizar paréntesis
        title = stringr::str_squish(title) %>%
          stringr::str_replace_all("\\s*\\(\\s*", " (") %>%
          stringr::str_replace_all("\\s*\\)\\s*", ")"),
        
        # Fechas: quitar " [edit]" y "not set"
        date_started  = date_started  %>% 
          stringr::str_replace("\\s*\\[edit\\]\\s*$", "") %>% 
          stringr::str_squish() %>%
          dplyr::na_if("not set") %>% 
          dplyr::na_if(""),
        
        date_finished = date_finished %>%
          stringr::str_replace("\\s*\\[edit\\]\\s*$", "") %>% 
          stringr::str_squish() %>%
          dplyr::na_if("not set") %>% dplyr::na_if(""),
        
        # Estanterías: eliminar "[edit]" y separadores sobrantes
        shelves = shelves %>% 
          stringr::str_replace_all("\\s*\\[edit\\]\\s*", "") %>% 
          stringr::str_replace_all("\\s*;\\s*$", "") %>% 
          stringr::str_replace_all("\\s*;\\s*;", "; ") %>%
          stringr::str_squish() %>%
          dplyr::na_if(""),
        
        # Review: si pone "Write a review" (o vacío), convertir a NA
        review = review %>%
          stringr::str_replace("\\s*\\[edit\\]\\s*$", "") %>%
          stringr::str_squish(),
        
        review = dplyr::if_else(
          stringr::str_detect(
            tolower(review),
            "^write a review$") | review == "", 
          NA_character_,
          review),
        
        # Rating 0 = sin rating
        my_rating = dplyr::na_if(my_rating, 0L)
      )
    
    tb <- dplyr::filter(tb, !is.na(title))
    out[[length(out) + 1]] <- tb
    
    if (nrow(tb) < 90) break  # probablemente no hay más páginas
    Sys.sleep(sleep_sec)
  }
  
  if (length(out) == 0) return(tibble::tibble())
  dplyr::bind_rows(out)
}

# -----------------------------------------------
# Extraer Reading Progress de una página de libro
# -----------------------------------------------
scrape_reading_progress <- function(view_url, wait_timeout = 15) {
  library(xml2); library(rvest); library(stringr); library(dplyr); library(purrr); library(tibble)
  
  abs_url <- function(x) ifelse(startsWith(x %||% character(), "http"), x, paste0("https://www.goodreads.com", x))
  
  # Navega a la página del libro (tu sesión ya está logueada)
  retry_nav(view_url)
  
  # Espera simple a que se renderice el timeline
  t0 <- Sys.time(); nodes <- list()
  repeat {
    Sys.sleep(0.7)
    src  <- remDr$getPageSource()[[1]]
    html <- read_html(src)
    nodes <- html_elements(html, ".readingTimeline__row .readingTimeline__text")
    if (length(nodes) > 0) break
    if (as.numeric(difftime(Sys.time(), t0, units = "secs")) > wait_timeout) break
  }
  
  if (length(nodes) == 0) {
    # No hay timeline en esta página: devuelve tibble vacío
    return(tibble(
      date = character(), page = integer(), percent = numeric(),
      event = character(), status_url = character(), view = character()
    ))
  }
  
  # Funciones auxiliares de parseo
  parse_date   <- function(txt) {
    # toma solo el "Month DD, YYYY" del inicio
    d <- str_match(txt, "^([A-Za-z]+\\s+\\d{1,2},\\s+\\d{4})")[,2]
    d %||% NA_character_
  }
  parse_page   <- function(txt) {
    n <- str_match(txt, "page\\s+(\\d+)")[,2]
    ifelse(is.na(n), NA_integer_, as.integer(n))
  }
  parse_pct    <- function(txt) {
    p <- str_match(txt, "(\\d+(?:\\.\\d+)?)%")[,2]
    ifelse(is.na(p), NA_real_, as.numeric(p))
  }
  parse_event  <- function(txt) {
    case_when(
      str_detect(txt, regex("\\bStarted Reading\\b", ignore_case = TRUE))  ~ "started",
      str_detect(txt, regex("\\bFinished Reading\\b", ignore_case = TRUE)) ~ "finished",
      str_detect(txt, regex("\\bShelved as\\b", ignore_case = TRUE))       ~ "shelved-as",
      str_detect(txt, regex("\\bShelved\\b", ignore_case = TRUE))          ~ "shelved",
      TRUE ~ NA_character_
    )
  }
  
  # Extrae cada fila del timeline
  res <- map_dfr(nodes, function(node) {
    full <- html_text(node, trim = TRUE) %>% str_squish()
    tibble(
      date       = parse_date(full),
      page       = parse_page(full),
      percent    = parse_pct(full),
      event      = parse_event(full),
      status_url = html_attr(html_element(node, "a[href*='/user_status/show/']"), "href") %>% abs_url()
    )
  }) %>%
    mutate(view = view_url)
  
  res
}

# Convierte NA a null (texto) para el formato tipo {k:v}
null_or_num <- function(x) ifelse(is.na(x), "null", as.character(x))
null_or_str <- function(x) ifelse(is.na(x) | x == "", "null", paste0('"', str_replace_all(x, '"', '\\"'), '"'))

# JSON válido (array de objetos con claves entre comillas)
format_progress_json <- function(df) {
  if (is.null(df) || nrow(df) == 0) return("[]")
  # Sólo exportamos columnas relevantes; NA -> null
  jsonlite::toJSON(
    df %>% select(date, page, percent, event),
    auto_unbox = TRUE, na = "null"
  )
}

# -----------------------------
# 5) Raspar estanterías clave
# -----------------------------
message(">> Raspando 'read' (terminados)...")
df_read    <- scrape_shelf_table("read")

# Usa df_read (o el DF consolidado que tengas con la columna 'view')
# Conviene envolver con `possibly()` por si alguna página da error puntual.
safe_progress <- purrr::possibly(df_read, otherwise = tibble(
  date = character(), page = integer(), percent = numeric(),
  event = character(), status_url = character(), view = character()
))

df_read_with_progress <- df_read %>%
  mutate(progress = map(view, ~ safe_progress(.x))) %>%
  mutate(progress_json  = map_chr(progress, format_progress_json))


readr::write_csv(
  progress_long %>%
    select(
      title,
      author,
      book_url,
      date_pub,
      date_finished,
      date_started,
      num_pages,
      my_rating,
      review,
      shelves,
      progress_json
    ),
  "goodreads_progress_logs.csv")

# message(">> Raspando 'currently-reading' (en lectura)...")
# df_current <- scrape_shelf_table("currently-reading")
# 
# message(">> Raspando 'to-read' (por leer)...")
# df_toread  <- scrape_shelf_table("to-read")

# -----------------------------
# 6) Unir, parsear fechas y ordenar
# -----------------------------
all_df <- bind_rows(df_read, df_current, df_toread) %>%
  select(any_of(c("title","author","date_started","date_finished","my_rating","avg_rating",
                  "isbn","isbn13","shelves",".shelf",".page")),
         everything()) %>%
  distinct()

# Parser flexible de fechas Goodreads
parse_gd_date <- function(x) {
  x <- str_trim(x %||% "")
  res <- ifelse(
    x == "" | is.na(x),
    NA_character_,
    x
  )
  # Intentar varios formatos comunes
  parsed <- suppressWarnings(parse_date_time(res,
                                             orders = c("Ymd","Y-m-d","d/m/Y","m/d/Y","d.m.Y","b d Y","d b Y","b Y","Y"),
                                             locale = "C"
  ))
  as_date(parsed)
}

if ("date_started" %in% names(all_df)) {
  all_df$date_started_parsed <- parse_gd_date(all_df$date_started)
}
if ("date_finished" %in% names(all_df)) {
  all_df$date_finished_parsed <- parse_gd_date(all_df$date_finished)
}

final_df <- all_df %>%
  filter(!(is.na(title) & is.na(author))) %>%
  arrange(desc(coalesce(date_finished_parsed, date_started_parsed))) %>%
  relocate(title, author, date_started, date_started_parsed,
           date_finished, date_finished_parsed,
           my_rating, avg_rating, isbn, isbn13, shelves, .shelf)

# -----------------------------
# 7) Exportar CSV
# -----------------------------
outfile <- file.path(getwd(), sprintf("goodreads_with_dates_%s.csv", format(Sys.Date(), "%Y%m%d")))
write_csv(final_df, outfile)
message(sprintf("\n✅ ¡Listo! Exportado a: %s", outfile))

# -----------------------------
# 8) Cierre (también cubierto por on.exit)
# -----------------------------
safe_onexit(remDr$close())
safe_onexit(rD$server$stop())
