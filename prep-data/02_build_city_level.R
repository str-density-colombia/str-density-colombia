#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
#   02_build_city_level.R
#
#   Purpose   Build the city-level (level 2) dataset from official statistics
#             and short-term rental market data.
#   Paper     Section 2.2. Source of the city-level variables.
#   Input     data/raw/raw-2nd-lvl-variables.xlsx (one sheet per source; see
#             the "sources" sheet of that file).
#   Output    data/clean/clean_2ndlevel_data.xlsx, one row per city.
#   Next      03_prepare_input.R
#   Packages  readxl, janitor, dplyr, tidyr, stringr, writexl
#   Note      The dataset keeps every variable collected for the study,
#             including variables that do not enter the final specification.
#             Run the script from the root folder of the project.
#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~



# 1 SETTINGS -------------------------------------------------------------------

library(readxl)
library(janitor)
library(dplyr)
library(tidyr)
library(stringr)
library(writexl)

in_file  <- file.path("data", "raw", "raw-2nd-lvl-variables.xlsx")
out_dir  <- file.path("data", "clean")
out_file <- file.path(out_dir, "clean_2ndlevel_data.xlsx")

# Sheet of the input file that holds each source.
sheets <- c(
  mpm        = "dane_cnpv2018_mpm",
  pop_2005   = "dane_cg2005_population",
  pop_age    = "dane_cnpv2018_age",
  pop_2018   = "dane_cnpv2018_population",
  housing    = "dane_cnpv2018_housing",
  gdp        = "dane_gdp_dept_2024",
  tenure     = "dane_ecv2024_tenure",
  labour     = "dane_geih2025_labour",
  nonres     = "mincit_nonres_2025",
  rnt        = "mincit_rnt_2019_2025",
  nonres_cty = "mincit_nonres_2025_country",
  airdna     = "airdna_str_2025"
)
# The sheet "mincit_ett_tourism" is kept in the input file for reference but
# is not used in this script.

# Each sheet ends with a note citing its source. The note and any blank row
# before it would otherwise be read as data rows, so they are dropped.
read_sheet <- function(name) {
  df    <- read_excel(in_file, sheet = sheets[[name]])
  first <- trimws(as.character(df[[1]]))
  blank <- apply(is.na(df), 1, all)
  note  <- !is.na(first) & grepl("^\\*?Source:", first)
  df[!(blank | note), ]
}

# Merges below are inner joins: a city absent from a source is dropped.
# This helper reports, after each merge, how many cities remain and which
# ones were dropped, and warns if a merge duplicated any city.
check_merge <- function(before, after, step) {
  lost <- setdiff(before$ciudad, after$ciudad)
  cat(sprintf("%-26s %2d cities", step, nrow(after)))
  if (length(lost) > 0) cat("  | dropped:", paste(lost, collapse = ", "))
  cat("\n")
  if (anyDuplicated(after$ciudad) > 0) {
    warning(step, ": some cities appear more than once.")
  }
}



# 2 CITY LIST AND MULTIDIMENSIONAL POVERTY -------------------------------------

# This sheet defines the list of cities and the department names used as
# merge keys in the rest of the script.
pm18 <- read_sheet("mpm")
data_2nd <- pm18 %>% select(-region)



# 3 POPULATION -----------------------------------------------------------------

## 3.1 Urban share, 2005 census ----------------------------------------

censo05 <- read_sheet("pop_2005")
colnames(censo05) <- c("departamento", "cod.mun", "ciudad",
                       "urban.05", "rural.05",
                       "total.05", "urb.total.05")

prev <- data_2nd
data_2nd <- merge(data_2nd, censo05, by = c("ciudad", "departamento"))
check_merge(prev, data_2nd, "Census 2005")

# The municipal code (DIVIPOLA) from this sheet is the merge key for the
# sources that do not report city names in the same form.
data_2nd$cod.mun <- as.numeric(data_2nd$cod.mun)

## 3.2 Total population, 2018 census -----------------------------------

censo18 <- read_sheet("pop_2018") %>%
  clean_names() %>%
  mutate(cod.mun  = as.numeric(codigo_divipola),
         total.18 = total)

prev <- data_2nd
data_2nd <- merge(data_2nd, censo18[c("cod.mun", "total.18")], by = "cod.mun")
check_merge(prev, data_2nd, "Census 2018 population")

## 3.3 Share of young population, 2018 census --------------------------

# Enumerated population (not adjusted for coverage). The sheet is organized
# in nested blocks: department, municipality and area appear only in the
# first row of their block and the following rows are blank. These columns
# are filled downwards so that every row carries its department,
# municipality and area. Within each area, one row per five-year age group
# follows the area total (edad == "Total").
censo_unadj18 <- read_sheet("pop_age") %>%
  clean_names() %>%
  fill(dpto, municipio, nivel, .direction = "down")

total18 <- censo_unadj18 %>%
  filter(dpto != "Total Nacional", nivel == "Total",
         edad %in% c("Total"))

# Note that young population is defined as persons aged 20 to 39.
young18 <- censo_unadj18 %>%
  filter(dpto != "Total Nacional", nivel == "Total",
         edad %in% c("20 a 24", "25 a 29",
                     "30 a 34", "35 a 39"))

# Department and municipality are stored as "code_name".
total18 <- total18 %>%
  separate(dpto,
           into = c("cod.dpto", "departamento"),
           sep = "_",
           remove = FALSE) %>%
  separate(municipio,
           into = c("cod.mun", "ciudad"),
           sep = "_",
           remove = FALSE) %>%
  mutate(cod.mun = as.numeric(cod.mun)) %>%
  select(cod.mun, valor)

young18 <- young18 %>%
  separate(dpto,
           into = c("cod.dpto", "departamento"),
           sep = "_",
           remove = FALSE) %>%
  separate(municipio,
           into = c("cod.mun", "ciudad"),
           sep = "_",
           remove = FALSE) %>%
  mutate(cod.mun = as.numeric(cod.mun)) %>%
  select(cod.mun, edad, valor) %>%
  group_by(cod.mun) %>%
  summarise(young.18 = sum(valor, na.rm = TRUE))

prop.young <- merge(total18, young18, by = "cod.mun") %>%
  mutate(prop.young = young.18 / valor)

prev <- data_2nd
data_2nd <- merge(data_2nd, prop.young[c("cod.mun", "prop.young")],
                  by = "cod.mun")
check_merge(prev, data_2nd, "Census 2018 young share")



# 4 HOUSING STOCK --------------------------------------------------------------

censo_vivi18 <- read_sheet("housing") %>%
  clean_names() %>%
  fill(dpto, municipio, nivel, .direction = "down")

# Note that the housing stock counts houses and apartments in the whole
# municipality (all areas, all occupancy conditions). It is the denominator
# of STR listing density.
vivi_18 <- censo_vivi18 %>%
  filter(dpto != "Total Nacional", tipo %in% c("Casa", "Apartamento"),
         nivel %in% c("Total")) %>%
  group_by(dpto, municipio, nivel) %>%
  summarize(casa_aptos = sum(total)) %>%
  select(-nivel)

# This sheet reports the municipal code without the department digits. The
# five-digit DIVIPOLA code is rebuilt as department code + municipal code.
vivi_18 <- vivi_18 %>%
  separate(dpto,
           into = c("cod.dpto", "departamento"),
           sep = "_",
           remove = FALSE) %>%
  separate(municipio,
           into = c("cod.mun.base", "ciudad_viv"),
           sep = "_",
           remove = FALSE) %>%
  mutate(cod.mun = paste0(as.numeric(cod.dpto), cod.mun.base)) %>%
  select(cod.mun, casa_aptos)

prev <- data_2nd
data_2nd <- merge(data_2nd, vivi_18[c("cod.mun", "casa_aptos")],
                  by = "cod.mun")
check_merge(prev, data_2nd, "Housing stock")



# 5 REGIONAL ECONOMY -----------------------------------------------------------

## 5.1 Departmental GDP ------------------------------------------------

# GDP is reported by department; each city takes the value of its department.
pib_dpto <- read_sheet("gdp") %>%
  clean_names() %>%
  filter(dpto != "COLOMBIA") %>%
  mutate(departamento = dpto)

prev <- data_2nd
data_2nd <- merge(data_2nd, pib_dpto[c("departamento", "pib")],
                  by = "departamento")
check_merge(prev, data_2nd, "Departmental GDP")

## 5.2 Homeownership ---------------------------------------------------

ownership <- read_sheet("tenure") %>%
  clean_names()

colnames(ownership) <- c("departamento", "area", "total",
                         "pagada.total", "pagada.prop",
                         "pagando.total", "pagando.prop")

# Note that the homeownership share is the sum of households in owned
# dwellings fully paid and owned dwellings being paid, department total.
# The first row of the sheet holds column labels and is dropped.
ownership <- ownership[-1, ] %>%
  filter(area == "Total") %>%
  mutate(
    pagada.prop  = as.numeric(pagada.prop),
    pagando.prop = as.numeric(pagando.prop),
    propia.prop  = rowSums(
      across(c(pagada.prop, pagando.prop)),
      na.rm = TRUE
    )
  )

prev <- data_2nd
data_2nd <- merge(data_2nd, ownership[c("departamento", "propia.prop")],
                  by = "departamento")
check_merge(prev, data_2nd, "Homeownership")



# 6 LABOUR MARKET --------------------------------------------------------------

# The sheet contains 2025 data only.
geih_cuadro <- read_sheet("labour") %>%
  clean_names()

# Converts the DANE table, which stacks one block per city with the indicators
# in rows and the rolling quarters in columns, into a long data frame with one
# row per city, period and indicator.
clean_dane_panel <- function(df, sheet = 1){
  
  names(df) <- paste0("V", seq_along(df))
  
  # A row is a city name when only its first column has content.
  df <- df %>%
    mutate(
      ciudad = if_else(
        !is.na(V1) &
          rowSums(!is.na(select(., -V1))) == 0 &
          !str_detect(V1, "Serie|Concepto"),
        V1,
        NA_character_
      )
    ) %>%
    fill(ciudad)
  
  # The year is in the row labelled "Concepto".
  df <- df %>%
    mutate(
      año = if_else(
        str_detect(V1, "Concepto"),
        as.numeric(V2),
        NA_real_
      )
    ) %>%
    fill(año)
  
  # Rows that hold the rolling-quarter labels (e.g., "Oct - Dic").
  df <- df %>%
    mutate(
      es_periodo = str_detect(V2, "Ene|Feb|Mar|Abr|May|Jun|Jul|Ago|Sep|Oct|Nov|Dic")
    )
  
  periodos_df <- df %>%
    filter(es_periodo) %>%
    select(ciudad, V2:V11) %>%
    pivot_longer(
      cols = V2:V11,
      names_to = "col",
      values_to = "periodo"
    ) %>%
    filter(!is.na(periodo))
  
  # Keep indicator rows only.
  df_clean <- df %>%
    filter(
      !str_detect(V1, "Serie"),
      !str_detect(V1, "Concepto"),
      !es_periodo,
      !is.na(V1)
    )
  
  df_long <- df_clean %>%
    pivot_longer(
      cols = V2:V11,
      names_to = "col",
      values_to = "valor"
    ) %>%
    filter(!is.na(valor))
  
  # Each value takes the period label of its column.
  df_final <- df_long %>%
    left_join(periodos_df, by = c("ciudad", "col")) %>%
    select(
      ciudad,
      año,
      periodo,
      indicador = V1,
      valor
    ) %>%
    arrange(ciudad, indicador)
  
  return(df_final)
}

geih_tidy <- clean_dane_panel(geih_cuadro)

# Note that the unemployment and labour force participation rates for the
# October-December 2025 moving quarter.
geih_tidy <- geih_tidy %>%
  filter(indicador %in%
           c("Tasa de Desocupación (TD)", "Tasa Global de Participación (TGP)"),
         periodo == "Oct - Dic")

# Note that for cities reported as metropolitan areas (A.M.), the indicator
# of the metropolitan area is assigned to the city.
geih_tidy <- geih_tidy %>%
  mutate(
    ciudad = recode(ciudad,
                    "Barranquilla A.M." = "Barranquilla",
                    "Bogotá D.C."       = "Bogotá, D.C.",
                    "Bucaramanga A.M."  = "Bucaramanga",
                    "Cali A.M."         = "Cali",
                    "Cúcuta A.M."       = "Cúcuta",
                    "Manizales A.M."    = "Manizales",
                    "Medellín A.M."     = "Medellín",
                    "Pereira A.M."      = "Pereira"
    )
  )

geih_wide <- geih_tidy %>%
  mutate(valor = as.numeric(valor)) %>%
  pivot_wider(
    id_cols     = c(ciudad, año, periodo),
    names_from  = indicador,
    values_from = valor
  ) %>%
  clean_names()

prev <- data_2nd
data_2nd <- merge(data_2nd, geih_wide, by = "ciudad")
check_merge(prev, data_2nd, "GEIH labour market")



# 7 TOURISM --------------------------------------------------------------------

## 7.1 Non-resident foreign visitors, November 2025 --------------------

arrivals <- read_sheet("nonres") %>%
  clean_names()

# Arrivals of non-resident foreign visitors by destination municipality.
# This is a single month, November 2025.
arrivals25 <- arrivals %>%
  filter(as.numeric(ano) == 2025,
         mes == "noviembre") %>%
  mutate(cod.mun = as.numeric(cod_ciudad)) %>%
  select(cod.mun, viajeros) %>%
  distinct()

prev <- data_2nd
data_2nd <- merge(data_2nd, arrivals25, by = "cod.mun")
check_merge(prev, data_2nd, "Visitors, November 2025")

## 7.2 National Tourism Registry (RNT) ---------------------------------

rnt <- read_sheet("rnt") %>%
  clean_names()

# Rows without a registration year cannot be assigned to any year column
# and are dropped.
rnt <- rnt %>%
  filter(!is.na(ano)) %>%
  mutate(cod.mun = as.numeric(codigo_municipio)) %>%
  select(ano, cod.mun, nit, categoria)

# Note that registered providers are counted as distinct tax identification
# numbers (NIT) per municipality and year, across all provider categories.
# A provider with several establishments counts once.
rnt_city <- rnt %>%
  group_by(ano, cod.mun) %>%
  summarise(
    oferta_total = n_distinct(nit),
    .groups = "drop"
  )

# One column per year (rnt2019, ..., rnt2025). Years without registered
# providers take the value 0.
rnt_wide <- rnt_city %>%
  mutate(ano = paste0("rnt", ano)) %>%
  pivot_wider(
    names_from  = ano,
    values_from = oferta_total,
    values_fill = 0
  ) %>% na.omit()

prev <- data_2nd
data_2nd <- merge(data_2nd, rnt_wide, by = "cod.mun") %>% select(-any_of("rntNA"))
check_merge(prev, data_2nd, "RNT")


## 7.3 Non-resident foreign visitors by country, annual ----------------

visitantes <- read_sheet("nonres_cty") %>%
  clean_names()

# Figures by country of residence are summed by year and destination city.
visitantes <- visitantes %>%
  group_by(ano, departamento, ciudad) %>%
  summarize(cant_extranjeros = sum(cant_extranjeros_no_residentes,
                                   na.rm = TRUE))

# Department names are harmonized with those of the city list (section 2).
visitantes$departamento[visitantes$departamento == "Valle del Cauca"]    <- "Valle Del Cauca"
visitantes$departamento[visitantes$departamento == "Norte de Santander"] <- "Norte De Santander"
visitantes$departamento[visitantes$departamento == "Quindío"]            <- "Quindio"
visitantes$departamento[visitantes$departamento == "Bogota D.C."]        <- "Bogotá, D.C."

visitantes_wide <- visitantes %>%
  mutate(ano = paste0("visit", ano)) %>%
  pivot_wider(
    names_from  = ano,
    values_from = cant_extranjeros,
    values_fill = 0
  ) %>%
  arrange(departamento, ciudad)

prev <- data_2nd
data_2nd <- merge(data_2nd, visitantes_wide, by = c("ciudad", "departamento"))
check_merge(prev, data_2nd, "Visitors, annual")



# 8 SHORT-TERM RENTAL MARKET (AirDNA) ------------------------------------------
listings_data <- read_sheet("airdna") %>%
  clean_names()

prev <- data_2nd
data_2nd <- merge(data_2nd, listings_data %>% select(-ciudad_slug),
                  by = c("ciudad", "departamento"))
check_merge(prev, data_2nd, "AirDNA")



# 9 SAVE -----------------------------------------------------------------------
write_xlsx(data_2nd, out_file)
