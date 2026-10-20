#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
#   08_cross_level.R
#
#   Purpose   Estimate the cross-level interaction models (Models 3.1 to
#             3.4).
#   Paper     Section 3.2.3
#   Input     multilevel-models/estimation_sample.rds 
#   Requirement multilevel-models/model_functions.R
#   Output    multilevel-models/tables/table5_cross_level.csv
#             multilevel-models/models/m34.rds 
#   Next      09_figure4_random_slopes.R
#   Packages  dplyr, nlme, msm, janitor
#   Note      Estimated by maximum likelihood (method = "ML"). Run the
#             script from the root folder of the project.
#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~



# 1 SETTINGS -------------------------------------------------------------------

library(dplyr)
library(nlme)


out_dir <- "multilevel-models"
tab_dir   <- file.path(out_dir, "tables")
model_dir <- file.path(out_dir, "models")

df2_c <- readRDS(file.path(out_dir, "estimation_sample.rds"))$df2_c
# 2 MODEL 3.1: PROPERTY TYPE X LISTING DENSITY ---------------------------------


m31_cent <- lme(
  lprecio_c ~ larea_c + banos + ses + antig + tipo_inmueble +
    density_list_c + density_list_c_2 +
    tgp_c +
    
    # Interaction
    tipo_inmueble*density_list_c,
  random = ~ 1 + larea_c | ciudad,
  data   = df2_c,
  method = "ML"
)
summary(m31_cent)

icc_m31 <- as.numeric(rho_f(model = m31_cent))

tab_m31 <- table_mixed_model_rslope(m31_cent, digits = 3) %>%
  mutate(model = "M3.1")



# 3 MODEL 3.2: SES X LISTING DENSITY -------------------------------------------


m32_cent <- lme(
  lprecio_c ~ larea_c + banos + ses + antig + tipo_inmueble +
    density_list_c + density_list_c_2 +
    tgp_c +
    
    # Interaction
    ses*density_list_c,
  random = ~ 1 + larea_c | ciudad,
  data   = df2_c,
  method = "ML"
)
summary(m32_cent)

icc_m32 <- as.numeric(rho_f(model = m32_cent))

tab_m32 <- table_mixed_model_rslope(m32_cent, digits = 3) %>%
  mutate(model = "M3.2")



# 4 MODEL 3.3: PROPERTY TYPE X LFP RATE ----------------------------------------


m33_cent <- lme(
  lprecio_c ~ larea_c + banos + ses + antig + tipo_inmueble +
    density_list_c + density_list_c_2 +
    tgp_c +
    
    # Interaction
    tipo_inmueble*tgp_c,
  random = ~ 1 + larea_c | ciudad,
  data   = df2_c,
  method = "ML"
)
summary(m33_cent)

icc_m33 <- as.numeric(rho_f(model = m33_cent))

tab_m33 <- table_mixed_model_rslope(m33_cent, digits = 3) %>%
  mutate(model = "M3.3")



# 5 MODEL 3.4: ALL INTERACTIONS (PREFERRED SPECIFICATION) ----------------------


m34_cent <- lme(
  lprecio_c ~ larea_c + banos + ses + antig + tipo_inmueble +
    density_list_c + density_list_c_2 +
    tgp_c +
    
    # Interactions
    ses*density_list_c +
    tipo_inmueble*density_list_c +
    tipo_inmueble*tgp_c,
  
  random = ~ (1 + larea_c) | ciudad,
  data   = df2_c,
  method = "ML"
)
summary(m34_cent)

icc_m34 <- as.numeric(rho_f(model = m34_cent))

tab_m34 <- table_mixed_model_rslope(m34_cent, digits = 3) %>%
  mutate(model = "M3.4")



# 7 SAVE -----------------------------------------------------------------------

write.csv(bind_rows(tab_m31, tab_m32, tab_m33, tab_m34),
          file.path(tab_dir, "table5_cross_level.csv"), row.names = FALSE)

saveRDS(m34_cent, file.path(model_dir, "m34.rds"))
