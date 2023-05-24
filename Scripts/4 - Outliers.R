rstudioapi::writeRStudioPreference("data_viewer_max_columns", 1000L)
rm(list = ls())

if(!require(pacman)){
  install.packages("pacman")
}
library(pacman)

pacman::p_load(faoutlier)

#import data
test <- read.csv("Data/Transformed_CSVs/test_anonymized_and_prepared.csv")
test_piq23 <- read.csv("Data/Transformed_CSVs/test_piq23.csv")
test_pis6 <- read.csv("Data/Transformed_CSVs/test_pis6.csv")
test_ebi6 <- read.csv("Data/Transformed_CSVs/test_ebi6.csv")
test_meq30 <- read.csv("Data/Transformed_CSVs/test_meq.csv")

### STEP 1 - Specify the factor structures -----

piq23_model <- "
  f1 =~ PIV_1 + PIV_2 + PIV_3 + PIV_4 + PIV_5 + PIV_6 + PIV_7 + PIV_8 + PIV_9 + PIV_10 + PIV_11 + PIV_12 + PIV_13 + PIV_16
  f2 =~ PIV_17 + PIV_18 + PIV_19 + PIV_20 + PIV_22 + PIV_23 + PIV_25 + PIV_27 + PIV_28
"

pis6_model <- '
  f1 =~ PIS.6_1 + PIS.6_2 + PIS.6_3 + PIS.6_4 + PIS.6_5 + PIS.6_6
'

ebi6_model <- "
  f1 =~ EBI_1 + EBI_2 + EBI_3 + EBI_5 + EBI_6 + EBI_8
"

meq30_model <- '
  MY =~ MEV_4 + MEV_5 + MEV_6 + MEV_9 + MEV_14 + MEV_15 + MEV_16 + MEV_18 + MEV_20 + MEV_2 1 + MEV_23 + MEV_24 + MEV_25 + MEV_26 + MEV_28 
  PM =~ MEV_2 + MEV_8 + MEV_12 + MEV_17 + MEV_27 + MEV_30 
  TR =~ MEV_1 + MEV_7 + MEV_11 + MEV_13 + MEV_19 + MEV_22
  IN =~ MEV_3 + MEV_10 + MEV_29
'

### STEP 2 - Calculate outliers -----
## step 2A - Residuals -----

# PIQ
res_piq <- obs.resid(test_piq23, piq23_model)
png("Figures/Descriptive stats/Outliers/hist_res_piq23.png", width=4, height=3, units="in", res=300)
hist_res_piq <- hist(res_piq$std_res, xlab = "Standardized residuals", main = "", xlim = range(-4,4)) 
dev.off()

# PIS
res_pis <- obs.resid(test_pis6, pis6_model)
png("Figures/Descriptive stats/Outliers/hist_res_pis6.png", width=4, height=3, units="in", res=300)
hist_res_pis <- hist(res_pis$std_res, xlab = "Standardized residuals", main = "", xlim = range(-4,4))
dev.off()

# EBI
res_ebi <- obs.resid(test_ebi6, ebi6_model)
png("Figures/Descriptive stats/Outliers/hist_res_ebi6.png", width=4, height=3, units="in", res=300)
hist_res_ebi <- hist(res_ebi$std_res, xlab = "Standardized residuals", main = "")
dev.off()

## step 2B - Mahalonobis distance ----
#PIQ
rMD_piq23 <- robustMD(test_piq23)
png("Figures/Descriptive stats/Outliers/hist_mah_piq23.png", width=4, height=3, units="in", res=300)
hist_mah_piq <- hist(rMD_piq23$mah, xlab = "Mahalonobis distance", main = "")
dev.off()

#PIS
rMD_pis6 <- robustMD(test_pis6)
png("Figures/Descriptive stats/Outliers/hist_mah_pis6.png", width=4, height=3, units="in", res=300)
hist_mah_pis <- hist(rMD_pis6$mah, xlab = "Mahalonobis distance", main = "")
dev.off()

#EBI
rMD_ebi6 <- robustMD(test_ebi6)
png("Figures/Descriptive stats/Outliers/hist_mah_ebi6.png", width=4, height=3, units="in", res=300)
hist_mah_ebi <- hist(rMD_ebi6$mah, xlab = "Mahalonobis distance", main = "")
dev.off()

## step 2C - Generalized Cook's Distance ----

#PIQ
gd_piq23 <- gCD(test_piq23, piq23_model)
png("Figures/Descriptive stats/Outliers/hist_gd_piq23.png", width=4, height=3, units="in", res=300)
hist_gd_piq <- hist(gd_piq23$gCD, xlab = "Generalized Cook's D", main = "")
dev.off()

#PIS
gd_pis6 <- gCD(test_pis6, pis6_model)
png("Figures/Descriptive stats/Outliers/hist_gd_pis6.png", width=4, height=3, units="in", res=300)
hist_gd_pis6 <- hist(gd_pis6$gCD, xlab = "Generalized Cook's D", main = "")
dev.off()

#EBI
gd_ebi6 <- gCD(test_ebi6, ebi6_model)
png("Figures/Descriptive stats/Outliers/hist_gd_ebi6.png", width=4, height=3, units="in", res=300)
hist_gd_ebi6 <- hist(gd_ebi6$gCD, xlab = "Generalized Cook's D", main = "")
dev.off()

