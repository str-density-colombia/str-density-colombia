#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
#   03_prepare_input.R
#
#   Purpose   Combine the listing-level data of all cities, clean the listing
#             variables, and merge them with the city-level dataset.
#   Paper     Section 2.2. Builds the estimation sample.
#   Input     data/raw/120226_<city>_scraper_data.xlsx, one file per city.
#             data/clean/clean_2ndlevel_data.xlsx (from 02_build_city_level.R).
#   Output    data/clean/100226_multilevel_final_dataset.xlsx, one row per
#             listing.
#             data/clean/sample_flow.csv, listings retained at each step, by
#             city.
#   Next      Estimation scripts.
#   Packages  readxl, janitor, dplyr, tidyr, purrr, stringr, stringi, writexl
#   Note      Listings above 15 million COP/month are excluded in the
#             estimation scripts, not here. Run the script from the root
#             folder of the project.
#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~



# 1 SETTINGS -------------------------------------------------------------------

library(readxl)
library(janitor)
library(dplyr)
library(tidyr)
library(purrr)
library(stringr)
library(writexl)

raw_dir  <- file.path("data", "raw")
out_dir  <- file.path("data", "clean")
lvl2_file <- file.path(out_dir, "clean_2ndlevel_data.xlsx")
out_file  <- file.path(out_dir, "100226_multilevel_final_dataset.xlsx")
flow_file <- file.path(out_dir, "sample_flow.csv")

# Identifier used as prefix in the names of the scraped files.
file_prefix <- "120226"

# Cities and their URL slugs on Fincaraíz (city and department).
# Commented-out cities were not scraped.
ciudades <- tibble::tribble(
  ~ciudad,        ~slug_ciudad,     ~slug_depto,
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
  #"Popayán",     "popayan",        "cauca",
  "Valledupar",   "valledupar",     "cesar",
  #"Quibdó",      "quibdo",         "choco",
  "Neiva",        "neiva",          "huila",
  #"Riohacha",    "riohacha",       "laguajira",
  "Santa Marta",  "santamarta",     "magdalena",
  "Armenia",      "armenia",        "quindio",
  "Sincelejo",    "sincelejo",      "sucre"
)



# 2 LOAD LISTING DATA ----------------------------------------------------------

# One file per city, stacked into a single dataset.
level1_data <- ciudades %>%
  mutate(file_path = file.path(raw_dir,
                               paste0(file_prefix, "_", slug_ciudad,
                                      "_scraper_data.RDS"))) %>%
  mutate(data = map(file_path, readRDS)) %>%
  select(slug_ciudad, slug_depto, data) %>%
  unnest(data) %>%
  clean_names()

# Sample flow, step 1: listings collected.
flow <- count(level1_data, slug_ciudad, name = "collected")



# 3 SELECT VARIABLES -----------------------------------------------------------

level1_data <- level1_data %>%
  select(id_propiedad, slug_ciudad, slug_depto,
         precio, ubicacion, tipo_de_inmueble, estado,
         banos_main, hab_main, antiguedad,
         parqueaderos, area, estrato,
         piso_n, cantidad_de_pisos,
         descripcion, link) %>%
  mutate(
    banos = banos_main,
    habs  = hab_main
  )



# 4 FURNISHED DWELLINGS --------------------------------------------------------

# The listing description is converted to lower case without accents before
# searching for keywords.
level1_data <- level1_data %>%
  mutate(
    descripcion_clean = descripcion %>%
      str_to_lower() %>%
      stringi::stri_trans_general("Latin-ASCII")
  )

# THIS IS IMPORTANT: note that a dwelling is classified as furnished when the description
# states it explicitly (e.g., "apartamento amoblado", "se arrienda
# amoblado", "totalmente amoblado") and does not negate it (e.g., "no
# amoblado", "sin amoblar").
pattern_pos <- paste0(
  "\\b(apartamento|casa)\\s+amoblad[oa]s?\\b|",
  "\\bse\\s+(alquila|arrienda|renta)\\s+amoblad[oa]s?\\b|",
  "\\b(parcialmente|totalmente|full)\\s+amoblad[oa]s?\\b"
)

pattern_neg <- "no\\W{0,15}amoblad[oa]s?|sin\\s+amoblar"

level1_data <- level1_data %>%
  mutate(
    match_pos = str_detect(descripcion_clean, pattern_pos),
    match_neg = str_detect(descripcion_clean, pattern_neg),
    amoblado  = ifelse(match_pos & !match_neg, 1, 0)
  ) %>%
  select(-match_pos, -match_neg)



# 5 CLEAN LISTING VARIABLES ----------------------------------------------------

# Each variable is parsed from the text shown on the listing page. Values
# that do not match the expected format are set to missing.

## 5.1 Price -----------------------------------------------------------

# Prices are shown as "$ 2.800.000": the dollar sign and the thousands
# separators are removed.
level1_data <- level1_data %>%
  mutate(
    precio_num = precio %>%
      str_remove_all("\\$") %>%
      str_remove_all("\\.") %>%
      str_trim() %>%
      as.numeric()
  )

## 5.2 Bathrooms -------------------------------------------------------

level1_data$banos_num <- ifelse(
  str_detect(level1_data$banos, "^\\d+\\s*Baño\\s*s?$"),
  as.numeric(str_extract(level1_data$banos, "\\d+")),
  NA_real_
)

## 5.3 Bedrooms --------------------------------------------------------

level1_data$hab_num <- ifelse(
  str_detect(level1_data$habs, "^\\-?\\d+\\s*Hab\\.?s?\\.?$"),
  as.numeric(str_extract(level1_data$habs, "-?\\d+")),
  NA_real_
)

## 5.4 Floor area ------------------------------------------------------

# Floor area is shown in square meters (e.g., "87 m²").
level1_data$area_num <- ifelse(
  str_detect(level1_data$area, "^\\d+\\.?\\d*\\s*m²$"),
  as.numeric(str_extract(level1_data$area, "\\d+\\.?\\d*")),
  NA_real_
)

## 5.5 Property age ----------------------------------------------------

# The platform reports age in five categories; any other value is set to
# missing.
categorias_antig <- c(
  "menor a 1 año",
  "1 a 8 años",
  "9 a 15 años",
  "16 a 30 años",
  "más de 30 años"
)

level1_data$antiguedad_clean <- ifelse(
  level1_data$antiguedad %in% categorias_antig,
  level1_data$antiguedad,
  NA
)

## 5.6 Socioeconomic stratum -------------------------------------------

# Note that the six strata are grouped as low (1-2), medium (3-4) and
# high (5-6).
level1_data <- level1_data %>%
  mutate(
    estrato = str_trim(estrato),
    estrato_num = ifelse(
      estrato %in% as.character(1:6),
      as.numeric(estrato),
      NA_real_
    ),
    ses = case_when(
      estrato_num %in% c(1,2) ~ "Bajo",
      estrato_num %in% c(3,4) ~ "Medio",
      estrato_num %in% c(5,6) ~ "Alto",
      TRUE ~ NA_character_
    )
  )



# 6 CLEAN LISTING DATASET ------------------------------------------------------

lvl1_data_vars <- level1_data %>%
  select(id_propiedad, slug_ciudad, slug_depto,
         ubicacion, tipo_de_inmueble,
         precio = precio_num,
         banos  = banos_num,
         habs   = hab_num,
         area   = area_num,
         ses,
         antiguedad = antiguedad_clean,
         amoblado)

# Note that only listings with complete information are kept. The furnished
# indicator is not part of this condition, so listings without a
# description are not dropped.
lvl1_data_vars <- lvl1_data_vars %>%
  filter(if_all(-amoblado, ~ !is.na(.x)))

# Sample flow, step 2: listings with complete information.
flow <- flow %>%
  left_join(count(lvl1_data_vars, slug_ciudad, name = "complete"),
            by = "slug_ciudad")

# Reference categories:
lvl1_data_vars$antiguedad <- relevel(
  factor(lvl1_data_vars$antiguedad),
  ref = "1 a 8 años"
)

lvl1_data_vars$ses <- relevel(
  factor(lvl1_data_vars$ses),
  ref = "Medio"
)



# 7 CITY-LEVEL DATA ------------------------------------------------------------

level2_data <- read_excel(lvl2_file)

# City and department names are converted to the slugs used on Fincaraíz,
# which are the merge keys with the listing data.
make_slug <- function(x){
  x %>%
    stringi::stri_trans_general("Latin-ASCII") %>%
    str_to_lower() %>%
    str_replace_all("[^a-z0-9]+", "")
}

level2_data <- level2_data %>%
  mutate(
    slug_ciudad = make_slug(ciudad),
    slug_depto  = make_slug(departamento),
    slug_ciudad = ifelse(ciudad == "Bogotá, D.C.", "bogota", slug_ciudad),
    slug_depto  = case_when(
      departamento == "Bogotá, D.C."       ~ "bogota-dc",
      departamento == "Valle Del Cauca"    ~ "valledelcauca",
      departamento == "Norte De Santander" ~ "nortedesantander",
      departamento == "La Guajira"         ~ "laguajira",
      TRUE ~ slug_depto
    )
  )



# 8 MERGE ----------------------------------------------------------------------

final_dataset <- lvl1_data_vars %>%
  left_join(level2_data,
            by = c("slug_ciudad", "slug_depto"))

# Unique listing identifier: municipal code and listing number.
final_dataset$id <- paste0(final_dataset$cod.mun, "-",
                           final_dataset$id_propiedad)

# Listings whose city has no match in the city-level data. Expected: none.
anti_join(lvl1_data_vars, level2_data,
          by = c("slug_ciudad", "slug_depto")) %>%
  count(slug_ciudad)

# Sample flow, step 3: listings matched to the city-level data.
flow <- flow %>%
  left_join(count(filter(final_dataset, !is.na(ciudad)), slug_ciudad,
                  name = "matched"),
            by = "slug_ciudad")



# 9 SAMPLE OF CITIES -----------------------------------------------------------

# Note that cities with 20 or fewer listings are excluded.
counts_cities <- count(final_dataset, ciudad) %>%
  filter(n > 20)

final_dataset <- final_dataset %>%
  filter(ciudad %in% levels(as.factor(counts_cities$ciudad)))

# Sample flow, step 4: listings in the final sample.
flow <- flow %>%
  left_join(count(final_dataset, slug_ciudad, name = "final"),
            by = "slug_ciudad")



# 10 SAMPLE FLOW ---------------------------------------------------------------

# Number of listings retained at each step, by city, with a total row.
flow <- flow %>%
  mutate(across(where(is.numeric), ~ replace_na(.x, 0)))

flow <- bind_rows(
  flow,
  summarise(flow, slug_ciudad = "Total", across(where(is.numeric), sum))
)

print(flow, n = Inf)



# 11 SAVE ----------------------------------------------------------------------

write_xlsx(final_dataset, out_file)
write.csv(flow, flow_file, row.names = FALSE)
