#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
#   05_estimation_sample.R
#
#   Purpose   Build the estimation sample: exclusions, variable
#             transformations and grand-mean centering.
#   Paper     Sections 2.2 and 2.4.
#   Input     data/clean/100226_multilevel_final_dataset.xlsx
#             (from 03_prepare_input.R).
#   Output    multilevel-models/estimation_sample.rds, centered data and the mean of
#             the log price.
#             multilevel-models/tables/sample_flow_estimation.csv
#   Next      06_null_model.R
#   Packages  readxl, dplyr, stringr
#   Note      Run the script from the root folder of the project.
#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~



# 1 SETTINGS -------------------------------------------------------------------

library(readxl)
library(dplyr)
library(stringr)

in_file <- file.path("data", "clean", "100226_multilevel_final_dataset.xlsx")
out_dir <- "multilevel-models"
tab_dir <- file.path(out_dir, "tables")


# Note that listings at or above 15 million COP/month are excluded
# (see Section 2.2).
price_cap <- 15000000



# 2 ESTIMATION SAMPLE ----------------------------------------------------------

df_input <- read_excel(in_file)
n_input  <- nrow(df_input)

df <- df_input %>%
  filter(precio > 0, precio < price_cap, area > 0)
n_capped <- nrow(df)

df2 <- df %>% mutate(
  
  # Dependent variable
  precio = as.numeric(precio),
  
  # Level-1 variables
  area_m2 = as.numeric(area),
  banos = as.numeric(banos),
  habs = as.numeric(habs),
  ses = as.factor(ses),
  antig = as.factor(antiguedad),
  tipo_inmueble = as.factor(tipo_de_inmueble),
  
  # Grouping variable
  ciudad = as.factor(slug_ciudad),
  
  # Level-2 variables. Listing density: active STR listings per 100 housing
  # units (houses and apartments, 2018 census).
  listings = as.numeric(listings),
  density_list = listings/casa_aptos*100,
  tgp = as.numeric(tasa_global_de_participacion_tgp)
  
)

# Logarithms. Listings with non-finite values are dropped.
df2 <- df2 %>% mutate(
  lprecio = log(precio),
  larea = log(area_m2),
  llist = log(listings)) %>%
  filter(
    is.finite(lprecio),
    is.finite(larea),
    is.finite(llist),
    is.finite(tgp)
  )

# English labels for the categories reported in the tables. Reference
# categories follow alphabetical order: High (SES) and Apartment (property
# type). Property age keeps its original labels; its reference category is
# "1 a 8 años".
df2$tipo_inmueble = as.character(df2$tipo_inmueble)
df2$tipo_inmueble[df2$tipo_inmueble == "Apartamento"] = "Apartment"
df2$tipo_inmueble[df2$tipo_inmueble == "Casa"] = "House"
df2$tipo_inmueble = as.factor(df2$tipo_inmueble)

df2$ses = as.character(df2$ses)
df2$ses[df2$ses == "Bajo"] = "Low"
df2$ses[df2$ses == "Medio"] = "Medium"
df2$ses[df2$ses == "Alto"] = "High"
df2$ses = as.factor(df2$ses)

df2$ciudad = str_to_sentence(df2$ciudad)

# Number of listings (N) and cities (J)
N <- nrow(df2)
J <- n_distinct(df2$ciudad)
cat("N =", N, "| J =", J, "\n")

# Grand-mean centering (Section 2.4). The quadratic term is the square of the
# centered density, so the turning point -b1 / (2 * b2) is measured from the
# mean listing density.
df2_c <- df2 %>% mutate(
  
  # Dependent variable
  lprecio_c = lprecio - mean(lprecio, na.rm = TRUE),
  
  # Level-1 variables
  larea_c = larea - mean(larea, na.rm = TRUE),
  
  # Level-2 variables
  density_list_c = density_list - mean(density_list, na.rm = TRUE),
  density_list_c_2 = density_list_c*density_list_c,
  tgp_c = tgp - mean(tgp, na.rm = TRUE)
)

# Sample flow from the final dataset to the estimation sample.
sample_flow <- data.frame(
  step     = c("Listings in the final dataset",
               "Price below the cap and positive floor area",
               "Estimation sample (finite log values)"),
  listings = c(n_input, n_capped, N),
  cities   = c(n_distinct(df_input$slug_ciudad),
               n_distinct(df$slug_ciudad), J)
)
print(sample_flow)



# 3 SAVE -----------------------------------------------------------------------

saveRDS(
  list(df2_c        = df2_c,
       mean_lprecio = mean(df2$lprecio, na.rm = TRUE)),
  file.path(out_dir, "estimation_sample.rds")
)

write.csv(sample_flow,
          file.path(tab_dir, "sample_flow_estimation.csv"), row.names = FALSE)
