#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
#   06_null_model.R
#
#   Purpose   Estimate the null (intercept-only) model and the intraclass
#             correlation.
#   Paper     Section 3.2.1, Table 3.
#   Input     output/estimation_sample.rds (from 05_estimation_sample.R)
#   Output    multilevel-models/tables/table3_null_model.csv
#   Requirement:   model_functions.R
#   Next      02_random_int.R
#   Packages  dplyr, nlme, msm, janitor
#   Note      Estimated by maximum likelihood (method = "ML"). Run the
#             script from the root folder of the project.
#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~



# 1 SETTINGS -------------------------------------------------------------------

library(dplyr)
library(nlme)

out_dir <- "multilevel-models"
tab_dir <- file.path(out_dir, "tables")

df2_c <- readRDS(file.path(out_dir, "estimation_sample.rds"))$df2_c



# 2 NULL MODEL -----------------------------------------------------------------

m1_cent <- lme(
  lprecio ~ 1,
  random = ~ 1 | ciudad,
  data   = df2,
  method = "ML"
)
summary(m1_cent)

icc_m1 <- as.numeric(rho_f(model = m1_cent))

tab_m1 <- table_mixed_model(m1_cent, digits = 3) %>%
  mutate(model = "M1")

# The ICC is added to the table as a fit statistic, as in Table 3.
tab_m1 <- bind_rows(
  tab_m1,
  data.frame(type = "statistics", variables = "ICC", b = icc_m1, model = "M1")
)
print(tab_m1)



# 3 SAVE -----------------------------------------------------------------------

write.csv(tab_m1,
          file.path(tab_dir, "table3_null_model.csv"), row.names = FALSE)
