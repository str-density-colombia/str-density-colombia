#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
#   07_random_intercept_slope.R
#
#   Purpose   Estimate the random-intercept model (Model 2.1) and the model
#             with a random slope for log floor area (Model 2.2).
#   Paper     Section 3.2.2
#   Input     multilevel-models/estimation_sample.rds (from 05_estimation_sample.R).
#   Requirement multilevel-models/model_functions.R
#   Output    multilevel-models/tables/table4_random_intercept_slope.csv
#             multilevel-models/models/m22.rds.
#   Next      03_cross_level.R
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
# 2 MODEL 2.1: RANDOM INTERCEPT ------------------------------------------------


m21_cent <- lme(
  # Level-1 variables
  lprecio_c ~ larea_c + banos + ses + antig + tipo_inmueble +
    
    # Level-2 variables
    density_list_c + density_list_c_2 +
    tgp_c,
  random = ~ 1 | ciudad,
  data   = df2_c,
  method = "ML"
)
summary(m21_cent)

icc_m21 <- as.numeric(rho_f(model = m21_cent))

tab_m21 <- table_mixed_model(m21_cent, digits = 3) %>%
  mutate(model = "M2.1")



# 3 MODEL 2.2: RANDOM SLOPE FOR LOG FLOOR AREA ---------------------------------


# Note that log floor area is the only level-1 variable with a random slope.
m22_cent <- lme(
  # Level-1 variables
  lprecio_c ~ larea_c + banos + ses +
    antig + tipo_inmueble +
    
    # Level-2 variables
    density_list_c + density_list_c_2 +
    tgp_c,
  random = ~ 1 + larea_c | ciudad,
  data   = df2_c,
  method = "ML"
)
summary(m22_cent)

icc_m22 <- as.numeric(rho_f(model = m22_cent))

tab_m22 <- table_mixed_model_rslope(m22_cent, digits = 3) %>%
  mutate(model = "M2.2")



# 5 SAVE -----------------------------------------------------------------------

write.csv(bind_rows(tab_m21, tab_m22),
          file.path(tab_dir, "table4_random_intercept_slope.csv"), row.names = FALSE)

saveRDS(m22_cent, file.path(model_dir, "m22.rds"))
