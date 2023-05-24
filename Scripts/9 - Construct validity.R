rstudioapi::writeRStudioPreference("data_viewer_max_columns", 1000L)
rm(list = ls())

if(!require(pacman)){
  install.packages("pacman")
}
library(pacman)

pacman::p_load(MASS,
               psych,
               ggplot2)


#import data
test <- read.csv("Data/Transformed_CSVs/test_anonymized_and_prepared.csv")
test_piq23 <- read.csv("Data/Transformed_CSVs/test_piq23.csv")
test_piq28 <- read.csv("Data/Transformed_CSVs/test_piq28.csv")
test_pis6 <- read.csv("Data/Transformed_CSVs/test_pis6.csv")
test_ebi6 <- read.csv("Data/Transformed_CSVs/test_ebi6.csv")
test_meq30 <- read.csv("Data/Transformed_CSVs/test_meq.csv")

#import colnames
subscale_score_colnames <- read.csv("Data/Transformed_CSVs/subscale_score_colnames.csv")
subscale_score_colnames <- subscale_score_colnames$subscale_score_colnames
all_item_names <- read.csv("Data/Transformed_CSVs/all_item_names.csv")
only_included_item_names <- read.csv("Data/Transformed_CSVs/only_included_item_colnames.csv")




##### CHAPTER 1 - PIQ




### STEP 1 - CRITERION VALIDITY -----

##PIQ23
t.test(test$piv_total_23[test$Inzicht == "Ja"], test$piv_total_23[test$Inzicht == "Nee"])

##PIQ28
t.test(test$piv_total_28[test$Inzicht == "Ja"], test$piv_total_28[test$Inzicht == "Nee"])


### STEP 2 - CONVERGENT VALIDITY -----

##PIQ28
cor(test$piv_total_28, test$pis_total_6, use = "pairwise.complete.obs")

##PIQ23
cor(test$piv_total_23, test$pis_total_6, use = "pairwise.complete.obs")


### STEP 3 - CONCURRENT VALIDITY -----

#PIQ
cor(test$piv_total_28, test$swls_difference, use = "pairwise.complete.obs")
cor(test$piv_total_23, test$swls_difference, use = "pairwise.complete.obs")

### STEP 4 - MULTIVARIATE REGRESSIONS -----

total_score_iv_sets <- list("mev_total", 
                            "piv_total_23",
                            "pis_total_6",
                            "ebi_total_6",
                            list("mev_total", "piv_total_23"),
                            list("mev_total", "pis_total_6"),
                            list("mev_total", "ebi_total_6"),
                            list("piv_total_23", "ebi_total_6"),
                            list("piv_total_23", "pis_total_6"),
                            list("ebi_total_6", "pis_total_6"),
                            list("mev_total", "piv_total_23", "pis_total_6"),
                            list("mev_total", "piv_total_23", "ebi_total_6"),
                            list("mev_total", "pis_total_6", "ebi_total_6"),
                            list("piv_total_23", "pis_total_6", "ebi_total_6"),
                            list("mev_total", "piv_total_23", "ebi_total_6", "pis_total_6")
)

# standardize independent variables
test_standardized <- as.data.frame(lapply(test[subscale_score_colnames], function(x) (x - mean(x)) / sd(x)))
test_standardized$swls_difference <- (test$swls_difference - mean(test$swls_difference, na.rm = T)) / sd(test$swls_difference, na.rm = T)
test_standardized$mev_total <- (test$mev_total - mean(test$mev_total, na.rm = T)) / sd(test$mev_total, na.rm = T)
test_standardized$piv_total_23 <- (test$piv_total_23 - mean(test$piv_total_23, na.rm = T)) / sd(test$piv_total_23, na.rm = T)
test_standardized$piv_total_28 <- (test$piv_total_28 - mean(test$piv_total_28, na.rm = T)) / sd(test$piv_total_28, na.rm = T)
test_standardized$ebi_total_6 <- (test$ebi_total_6 - mean(test$ebi_total_6, na.rm = T)) / sd(test$ebi_total_6, na.rm = T)
test_standardized$pis_total_6 <- (test$pis_total_6 - mean(test$pis_total_6, na.rm = T)) / sd(test$pis_total_6, na.rm = T)

# create an empty data frame to store results
results <- data.frame(iv = character(), estimate = numeric(), ci_lower = numeric(), ci_upper = numeric(), p_value = numeric(), adj_r_squared = numeric(), significance = character(), stringsAsFactors = FALSE)

# loop through each list of independent variables. ADJUST IV_SETS IF YOU NEEDED
for (iv in total_score_iv_sets) {
  # create formula for linear regression
  formula <- as.formula(paste0("swls_difference ~ ", paste(iv, collapse = " + ")))
  
  # run linear regression
  model <- lm(data = test_standardized, formula)
  
  # extract coefficients, p-values, confidence intervals, and adjusted R-squared
  coef_names <- names(model$coefficients)[-1] # exclude intercept
  coef <- sapply(coef_names, function(x) summary(model)$coefficients[x, "Estimate"])
  ci <- sapply(coef_names, function(x) {
    conf_int <- confint(model)[x,]
    abs(conf_int[1] - conf_int[2])/2
  })
  ci_lower <- sapply(coef_names, function(x) confint(model)[x,1])
  ci_upper <- sapply(coef_names, function(x) confint(model)[x,2])
  p_value <- sapply(coef_names, function(x) summary(model)$coefficients[x, "Pr(>|t|)"])
  adj_r_squared <- summary(model)$adj.r.squared
  
  # determine significance codes
  sig_codes <- ifelse(p_value < 0.001, "***",
                      ifelse(p_value < 0.01, "**",
                             ifelse(p_value < 0.05, "*", "")))
  
  # create new row for results data frame
  new_row <- data.frame(iv = paste(iv, collapse = ", "), estimate = paste0(round(coef,2), sig_codes, sep = ""), ci_lower = ci_lower, ci_upper = ci_upper, p_value = p_value, adj_r_squared = adj_r_squared, significance = sig_codes)
  
  # append new row to results data frame
  results <- rbind(results, new_row)
}

# round numeric variables to two decimal places
results[, c("ci_lower", "ci_upper", "adj_r_squared")] <- round(results[, c("ci_lower", "ci_upper", "adj_r_squared")], 2)
results <- results[, -5]

#remove 0s from the adj_r_squared
results[c("adj_r_squared")] <- lapply(results[c("adj_r_squared")], function(x) {
  x <- as.character(x)
  x <- substring(x, 2)
  x
})

## PIQ28 -----
total_score_iv_sets_28 <- list(
                            "piv_total_28",
                            list("mev_total", "piv_total_28"),
                            list("piv_total_28", "ebi_total_6"),
                            list("piv_total_28", "pis_total_6"),
                            list("mev_total", "piv_total_28", "pis_total_6"),
                            list("mev_total", "piv_total_28", "ebi_total_6"),
                            list("piv_total_28", "pis_total_6", "ebi_total_6"),
                            list("mev_total", "piv_total_28", "ebi_total_6", "pis_total_6")
)

# create an empty data frame to store results
results_28 <- data.frame(iv = character(), estimate = numeric(), ci_lower = numeric(), ci_upper = numeric(), p_value = numeric(), adj_r_squared = numeric(), significance = character(), stringsAsFactors = FALSE)

# loop through each list of independent variables. ADJUST IV_SETS IF YOU NEEDED
for (iv in total_score_iv_sets_28) {
  # create formula for linear regression
  formula <- as.formula(paste0("swls_difference ~ ", paste(iv, collapse = " + ")))
  
  # run linear regression
  model <- lm(data = test_standardized, formula)
  
  # extract coefficients, p-values, confidence intervals, and adjusted R-squared
  coef_names <- names(model$coefficients)[-1] # exclude intercept
  coef <- sapply(coef_names, function(x) summary(model)$coefficients[x, "Estimate"])
  ci <- sapply(coef_names, function(x) {
    conf_int <- confint(model)[x,]
    abs(conf_int[1] - conf_int[2])/2
  })
  ci_lower <- sapply(coef_names, function(x) confint(model)[x,1])
  ci_upper <- sapply(coef_names, function(x) confint(model)[x,2])
  p_value <- sapply(coef_names, function(x) summary(model)$coefficients[x, "Pr(>|t|)"])
  adj_r_squared <- summary(model)$adj.r.squared
  
  # determine significance codes
  sig_codes <- ifelse(p_value < 0.001, "***",
                      ifelse(p_value < 0.01, "**",
                             ifelse(p_value < 0.05, "*", "")))
  
  # create new row for results data frame
  new_row <- data.frame(iv = paste(iv, collapse = ", "), estimate = paste0(round(coef,2), sig_codes, sep = ""), ci_lower = ci_lower, ci_upper = ci_upper, p_value = p_value, adj_r_squared = adj_r_squared, significance = sig_codes)
  
  # append new row to results data frame
  results_28 <- rbind(results_28, new_row)
}

# round numeric variables to two decimal places
results_28[, c("ci_lower", "ci_upper", "adj_r_squared")] <- round(results_28[, c("ci_lower", "ci_upper", "adj_r_squared")], 2)
results_28 <- results_28[, -c(5,7)]

#remove 0s from the adj_r_squared
results_28[c("adj_r_squared")] <- lapply(results_28[c("adj_r_squared")], function(x) {
  x <- as.character(x)
  x <- substring(x, 2)
  x
})

### STEP 5 - DISCRIMINANT VALIDITY - correlations -----
#PIQ ~ EBI
cor(test$piv_total_28, test$ebi_total_6, use = "pairwise.complete.obs")
cor(test$piv_total_23, test$ebi_total_6, use = "pairwise.complete.obs")

#PIQ ~ MEQ
cor(test$piv_total_28, test$mev_total, use = "pairwise.complete.obs")
cor(test$piv_total_23, test$mev_total, use = "pairwise.complete.obs")




##### CHAPTER 2 - EBI -----


### STEP 1 - CRITERION VALIDITY -----

##PIQ23
t.test(test$ebi_mean_6[test$Emotionele.doorbraak == "Ja"], test$ebi_mean_6[test$Emotionele.doorbraak == "Nee"])
length(test$ebi_mean_6[test$Emotionele.doorbraak == "Ja" & !is.na(test$ebi_mean_6)])
length(test$ebi_mean_6[test$Emotionele.doorbraak == "Nee" & !is.na(test$ebi_mean_6)])

### STEP 2 - CONCURRENT VALIDITY -----
cor(test$ebi_total_6, test$swls_difference, use = "pairwise.complete.obs")

### STEP 3 - DISCRIMINANT VALIDITY - correlations -----
#EBI ~ MEQ
cor(test$ebi_total_6, test$mev_total, use = "pairwise.complete.obs")

#EBI ~ PIS
cor(test$ebi_total_6, test$pis_total_6, use = "pairwise.complete.obs")




##### CHAPTER 3 - PIS -----

### STEP 1 - CRITERION VALIDITY -----

t.test(test$pis_mean_6[test$Inzicht == "Ja"], test$pis_mean_6[test$Inzicht == "Nee"])
length(test$pis_mean_6[test$Inzicht == "Ja" & !is.na(test$pis_mean_6)])
length(test$pis_mean_6[test$Inzicht == "Nee" & !is.na(test$pis_mean_6)])

### STEP 2 - CONCURRENT VALIDITY -----
cor(test$pis_total_6, test$swls_difference, use = "pairwise.complete.obs")

### STEP 5 - DISCRIMINANT VALIDITY - correlations -----
#pis ~ MEQ
cor(test$pis_total_6, test$mev_total, use = "pairwise.complete.obs")

#pis ~ EBI
cor(test$pis_total_6, test$ebi_total_6, use = "pairwise.complete.obs")

