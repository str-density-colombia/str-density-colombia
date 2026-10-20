#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
#   01_scrape_listings.R
#
#   Purpose   Collect long-term rental listings (houses and apartments) from
#             Fincaraíz (www.fincaraiz.com.co) for the cities in the study.
#   Paper     Section 2.2. Source of the listing-level data.
#   Input     None (live website).
#   Output    data/raw/<prefix>_<city>_scraper_data.rds and .xlsx, one pair
#             of files per city.
#   Next      03_prepare_input.R
#   Packages  rvest, chromote, stringr, dplyr, tidyr, tibble, writexl
#   Note      Each run returns the listings available on the website at that
#             moment, so a new run will not reproduce the data used in the
#             study. The files in data/raw are the output of the run used in
#             the study. Run the script from the root folder of the project.
#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~



# 1 SETTINGS -------------------------------------------------------------------

library(rvest)
library(stringr)
library(chromote)
library(dplyr)
library(tidyr)
library(writexl)

# Define working directory


# Define output directory
out_dir <- file.path("data", "raw")

# Identifier used as prefix in the output file names.
file_prefix <- "2102526"

# Cities and their URL slugs on Fincaraíz (city and department).
# Commented-out cities had already been scraped in an earlier run and were
# skipped to avoid processing them twice.
ciudades <- tibble::tribble(
  ~ciudad,        ~slug_ciudad,     ~slug_depto,
  "Popayán",      "popayan",        "cauca",
  "Bogotá",       "bogota",         "bogota-dc",
  "Medellín",     "medellin",       "antioquia",
  "Cali",         "cali",           "valledelcauca",
  "Barranquilla", "barranquilla",   "atlantico",
  "Bucaramanga",  "bucaramanga",    "santander",
  "Manizales",    "manizales",      "caldas",
  "Pasto",        "pasto",          "narino",
  "Pereira",      "pereira",        "risaralda",
  "Ibagué",       "ibague",         "tolima",
  "Cúcuta",       "cucuta",         "nortedesantander",
  "Villavicencio","villavicencio",  "meta",
  "Montería",     "monteria",       "cordoba",
  "Cartagena",    "cartagena",      "bolivar",
  "Tunja",        "tunja",          "boyaca",
  #"Florencia",   "florencia",      "caqueta",
  "Valledupar",   "valledupar",     "cesar",
  #"Quibdó",      "quibdo",         "choco",
  "Neiva",        "neiva",          "huila",
  #"Riohacha",    "riohacha",       "laguajira",
  "Santa Marta",  "santamarta",     "magdalena",
  "Armenia",      "armenia",        "quindio",
  "Sincelejo",    "sincelejo",      "sucre"
)



# 2 SCRAPE ONE LISTING ---------------------------------------------------------

# Returns a long data frame (var, value, link) with the fields of a single
# listing page.
scrape_property <- function(url_det){
  cat("   → Scraping:", url_det, "\n")
  
  # Listing pages are rendered with JavaScript, so read_html() alone returns
  # an incomplete page. A headless Chrome session loads the page and returns
  # the rendered HTML.
  b <- ChromoteSession$new()
  b$Page$navigate(url_det)
  b$Page$loadEventFired()
  
  html_rendered <- b$Runtime$evaluate(
    "document.documentElement.outerHTML"
  )$result$value
  
  b$close()
  
  page <- read_html(html_rendered)
  
  precio <- page %>%
    html_element("p.main-price") %>%
    html_text2() %>%
    str_squish()
  
  # Summary bar at the top of the listing. On the date of collection,
  # positions 2, 3 and 4 held bedrooms, bathrooms and floor area.
  all.main <- page %>%
    html_elements("span.ant-typography") %>%
    html_text2() %>%
    str_squish()
  
  hab_main   <- all.main[2]
  banos_main <- all.main[3]
  area_main  <- all.main[4]
  
  ubi.text <- page %>%
    html_element("span.property-location-tag") %>%
    html_text2() %>%
    str_squish()
  
  desc.text <- page %>%
    html_element("div.ant-typography.property-description") %>%
    html_text2() %>%
    str_squish()
  
  # Coordinates are not displayed on the page; they are embedded in the
  # page source.
  html_raw <- as.character(page)
  lat <- str_extract(html_raw, '"latitude"\\s*:\\s*(-?\\d+\\.\\d+)',  group = 1)
  lng <- str_extract(html_raw, '"longitude"\\s*:\\s*(-?\\d+\\.\\d+)', group = 1)
  
  # Detail table ("Detalles de la propiedad"). Labels and values are stored
  # in separate HTML elements and are matched by position.
  labels <- page %>%
    html_elements("span.ant-typography") %>%
    html_text2() %>%
    str_squish()
  
  # Drop navigation text, empty strings and separators, the section title,
  # and the first three spans, which belong to the summary bar and not to
  # the detail table
  labels <- labels[labels != "Volver a la búsqueda"]
  labels <- labels[labels != "" & labels != "•"]
  labels <- labels[!str_detect(labels, regex("^detalles de la propiedad$", ignore_case = TRUE))]
  labels <- labels[-c(1:3)]
  
  # Just the first 15 fields of the detail table are kept
  labels <- labels[1:15]
  
  valores <- page %>%
    html_elements("button.btnQuestion, strong") %>%
    html_text2() %>%
    str_squish()
  
  # Labels and values are laid out in two columns. Both vectors are padded
  # to an even length and truncated to the same length so that they can be
  # paired row by row
  if (length(labels) %% 2 != 0) labels <- c(labels, NA)
  if (length(valores) %% 2 != 0) valores <- c(valores, NA)
  
  if (length(labels) < length(valores)) {
    valores <- valores[1:length(labels)]
  }
  if (length(valores) < length(labels)) {
    labels <- labels[1:length(valores)]
  }
  
  m_labels <- matrix(labels,  ncol = 2, byrow = TRUE)
  m_values <- matrix(valores, ncol = 2, byrow = TRUE)
  
  v_labels <- c(t(m_labels))
  v_values <- c(t(m_values))
  
  df <- data.frame(
    var   = v_labels,
    value = v_values,
    stringsAsFactors = FALSE
  )
  
  df <- df[1:15, ]
  
  # Fields outside the detail table are appended as additional rows.
  df <- rbind(
    df,
    data.frame(var = "precio",      value = precio,     stringsAsFactors = FALSE),
    data.frame(var = "hab.main",    value = hab_main,   stringsAsFactors = FALSE),
    data.frame(var = "banos.main",  value = banos_main, stringsAsFactors = FALSE),
    data.frame(var = "area",        value = area_main,  stringsAsFactors = FALSE),
    data.frame(var = "ubicacion",   value = ubi.text,   stringsAsFactors = FALSE),
    data.frame(var = "descripcion", value = desc.text,  stringsAsFactors = FALSE),
    data.frame(var = "lat",         value = lat,        stringsAsFactors = FALSE),
    data.frame(var = "lng",         value = lng,        stringsAsFactors = FALSE)
  )
  
  df$link <- url_det
  
  df
}



# 3 LOOP OVER CITIES -----------------------------------------------------------

for (c in 1:nrow(ciudades)) {
  
  ciudad_nom  <- ciudades$ciudad[c]
  ciudad_slug <- ciudades$slug_ciudad[c]
  depto_slug  <- ciudades$slug_depto[c]
  
  cat("\n=============================\n")
  cat("CITY:", ciudad_nom, "\n")
  cat("=============================\n")
  
  # Search results for long-term rentals of houses and apartments.
  base_url <- paste0(
    "https://www.fincaraiz.com.co/arriendo/casas-y-apartamentos/",
    ciudad_slug, "/", depto_slug
  )
  
  ## 3.1 Detect the last results page ----------------------------------
  
  cat("Reading page 1 to detect the last results page...\n")
  
  page1 <- tryCatch(read_html(paste0(base_url, "/pagina1")), error = function(e) NULL)
  if (is.null(page1)) next
  
  last_page <- page1 %>%
    html_elements("a[href*='/pagina']") %>%
    html_attr("href") %>%
    str_extract("pagina[0-9]+") %>%
    str_remove("pagina") %>%
    as.numeric() %>%
    max(na.rm = TRUE)
  
  # Cities with a single results page have no pagination links.
  if(is.infinite(last_page)){
    last_page = 1
  }
  
  cat("Last page detected:", last_page, "\n")
  
  ## 3.2 Collect listing URLs ------------------------------------------
  
  all_links <- c()
  
  for (p in 1:last_page) {
    page_url <- paste0(base_url, "/pagina", p)
    cat("\nProcessing page:", p, "\n")
    
    page <- tryCatch(read_html(page_url), error = function(e) NULL)
    if (is.null(page)) next
    
    links <- page %>%
      html_elements("a.lc-data") %>%
      html_attr("href") %>%
      unique()
    
    links <- paste0("https://www.fincaraiz.com.co", links)
    all_links <- c(all_links, links)
  }
  
  # The same listing can appear on more than one results page.
  all_links <- unique(all_links)
  cat("Unique links:", length(all_links), "\n")
  
  ## 3.3 Scrape each listing -------------------------------------------
  
  # Listings that fail to load are skipped.
  resultados <- list()
  
  for (i in seq_along(all_links)) {
    
    cat("\nListing", i, "of", length(all_links), "\n")
    
    tmp <- tryCatch(
      scrape_property(all_links[i]),
      error = function(e) NULL
    )
    
    if (!is.null(tmp)) {
      resultados[[length(resultados) + 1]] <- tmp
    }
  }
  
  if (length(resultados) == 0) next
  
  df_final <- bind_rows(resultados)
  
  cat("Listings scraped:", length(unique(df_final$link)), "\n")
  
  ## 3.4 Reshape to one row per listing --------------------------------
  
  # Field names are standardized (lower case, underscores). When a field
  # appears more than once in a listing, the first occurrence is kept.
  df_wide <- df_final %>%
    mutate(
      var = str_to_lower(var),
      var = str_replace_all(var, "\\s+", "_"),
      value = str_squish(value)
    ) %>%
    distinct(link, var, .keep_all = TRUE) %>%
    pivot_wider(
      id_cols = link,
      names_from = var,
      values_from = value
    ) %>%
    mutate(
      id_propiedad = row_number(),
      lat = as.numeric(lat),
      lng = as.numeric(lng)
    ) %>%
    relocate(id_propiedad, link)
  
  ## 3.5 Save ----------------------------------------------------------
  
  writexl::write_xlsx(
    df_wide,
    file.path(out_dir, paste0(file_prefix, "_", ciudad_slug, "_scraper_data.xlsx"))
  )
  
  saveRDS(
    df_wide,
    file.path(out_dir, paste0(file_prefix, "_", ciudad_slug, "_scraper_data.RDS"))
  )
  
}
