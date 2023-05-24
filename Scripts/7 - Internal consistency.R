rstudioapi::writeRStudioPreference("data_viewer_max_columns", 1000L)
rm(list = ls())

if(!require(pacman)){
  install.packages("pacman")
}
library(pacman)

pacman::p_load(ltm,
               psych,
               semTools)

#import data
test <- read.csv("Data/Transformed_CSVs/test_anonymized_and_prepared.csv")

test_piq28 <- read.csv('Data/Transformed_CSVs/test_piq28.csv')
test_piq23 <- read.csv("Data/Transformed_CSVs/test_piq23.csv")
test_pis6 <- read.csv("Data/Transformed_CSVs/test_pis6.csv")
test_ebi6 <- read.csv("Data/Transformed_CSVs/test_ebi6.csv")

item_names <- read.csv("Data/Transformed_CSVs/all_item_names.csv")


##### CHAPTER 1 - PIQ -----


### STEP 1 - OMEGA -----

##PIQ23
omega_piq23 <- omega(test_piq23[2:24], nfactors = 2, rotate = "promax", fm = "pc")
omega_piq23

omega_piq23_3f <- omega(test_piq23[2:24], nfactors = 3, poly = T, rotate = "promax", fm = "pc")
omega_piq23_3f

#using CFA based approach
#PIQ23 2F
piq23_bf_2f_model <- '
  f1 =~ PIV_1 + PIV_2 + PIV_3 + PIV_4 + PIV_5 + PIV_6 + PIV_7 + PIV_8 + PIV_9 + PIV_10 + PIV_11 + PIV_12 + PIV_13 
  f2 =~ PIV_16 + PIV_17 + PIV_18 + PIV_19 + PIV_20 + PIV_22 + PIV_23 + PIV_25 + PIV_27 + PIV_28
  g =~ PIV_1 + PIV_2 + PIV_3 + PIV_4 + PIV_5 + PIV_6 + PIV_7 + PIV_8 + PIV_9 + PIV_10 + PIV_11 + PIV_12 + PIV_13 + PIV_16 + PIV_17 + PIV_18 + PIV_19 + PIV_20 + PIV_22 + PIV_23 + PIV_25 + PIV_27 + PIV_28'
fit_piq23_bf_2f <- lavaan::cfa(piq23_bf_2f_model, data = test, estimator = "MLR", std.lv = T, orthogonal = T)
omega_piq23_cfa <- semTools::reliability(fit_piq23_bf_2f)
omega_piq23_cfa
omega_piq23_cfa[4,3]/omega_piq23_cfa[2,3]
omega_piq23_cfa[4,1]/omega_piq23_cfa[2,1]
omega_piq23_cfa[4,2]/omega_piq23_cfa[2,2]

#first revised model
piq23_bf_r1_model <- "
  f1 =~ PIV_1 + PIV_2 + PIV_3 + PIV_4 + PIV_5 + PIV_6 + PIV_10 + PIV_11 + PIV_12 + PIV_13 + PIV_16
  f2 =~ PIV_17 + PIV_18 + PIV_19 + PIV_20 + PIV_22 + PIV_23 + PIV_25 + PIV_27 + PIV_28
  f3 =~ PIV_7 + PIV_8 + PIV_9
  g =~ PIV_1 + PIV_2 + PIV_3 + PIV_4 + PIV_5 + PIV_6 + PIV_7 + PIV_8 + PIV_9 + PIV_10 + PIV_11 + PIV_12 + PIV_13 + PIV_16 + PIV_17 + PIV_18 + PIV_19 + PIV_20 + PIV_22 + PIV_23 + PIV_25 + PIV_27 + PIV_28
"

fit_piq23_bf_r1 <- lavaan::cfa(piq23_bf_r1_model, data = test, estimator = "MLR", orthogonal = T)
omega_piq23_cfa_r1 <- semTools::reliability(fit_piq23_bf_r1)
omega_piq23_cfa_r1

#second revised model
piq23_bf_r2_model <- "
  f1 =~ PIV_1 + PIV_2 + PIV_3 + PIV_4 + PIV_5 + PIV_6 + PIV_10 + PIV_11 + PIV_12 + PIV_13 + PIV_16
  f2 =~ PIV_17 + PIV_18 + PIV_19 + PIV_20 + PIV_22 + PIV_23 + PIV_25 + PIV_27 + PIV_28
  f3 =~ PIV_7 + PIV_8 + PIV_9
  PIV_22 ~~ PIV_23
  g =~ PIV_1 + PIV_2 + PIV_3 + PIV_4 + PIV_5 + PIV_6 + PIV_7 + PIV_8 + PIV_9 + PIV_10 + PIV_11 + PIV_12 + PIV_13 + PIV_16 + PIV_17 + PIV_18 + PIV_19 + PIV_20 + PIV_22 + PIV_23 + PIV_25 + PIV_27 + PIV_28
"

fit_piq23_bf_r2 <- lavaan::cfa(piq23_bf_r2_model, data = test, estimator = "MLR", orthogonal = T)
omega_piq23_cfa_r2 <- semTools::reliability(fit_piq23_bf_r2)
omega_piq23_cfa_r2


##PIQ28

#2F
piq28_bf_2f_model <- '
  f1 =~ PIV_1 + PIV_2 + PIV_3 + PIV_4 + PIV_5 + PIV_6 + PIV_7 + PIV_8 + PIV_9 + PIV_10 + PIV_11 + PIV_12 + PIV_13 + PIV_14 + PIV_15
  f2 =~ PIV_16 + PIV_17 + PIV_18 + PIV_19 + PIV_20 + PIV_21 + PIV_22 + PIV_23 + PIV_24 + PIV_25 + PIV_26 + PIV_27 + PIV_28
  g =~ PIV_1 + PIV_2 + PIV_3 + PIV_4 + PIV_5 + PIV_6 + PIV_7 + PIV_8 + PIV_9 + PIV_10 + PIV_11 + PIV_12 + PIV_13 + PIV_14 + PIV_15 + PIV_16 + PIV_17 + PIV_18 + PIV_19 + PIV_20 + PIV_21 + PIV_22 + PIV_23 + PIV_24 + PIV_25 + PIV_26 + PIV_27 + PIV_28'
fit_piq28_bf_2f <- lavaan::cfa(piq28_bf_2f_model, data = test, estimator = "MLR", std.lv = T, orthogonal = T)
omega_piq28_cfa <- semTools::reliability(fit_piq28_bf_2f)
omega_piq28_cfa
omega_piq28_cfa[4,3]/omega_piq28_cfa[2,3]
omega_piq28_cfa[4,1]/omega_piq28_cfa[2,1]
omega_piq28_cfa[4,2]/omega_piq28_cfa[2,2]

#3F
omega_piq28_3f <- omega(test_piq28[2:29], nfactors = 3, poly = F, rotate = "promax", fm = "pc")
omega_piq28_3f

#4F
omega_piq28_4f <- omega(test_piq28[2:29], nfactors = 4, poly = F, rotate = "promax", fm = "pc")
omega_piq28_4f

#5F
omega_piq28_5f <- omega(test_piq28[2:29], nfactors = 5, poly = F, rotate = "promax", fm = "pc")
omega_piq28_5f

##### STEP 2 - ITEM-TOTAL CORRELATIONS -----

# define a function to calculate item-total and average inter-item correlations

item_correlation_calculator <- function(items, total_score, string_to_delete){
  
  # Calculate the item-total correlations
  item_total_corr <- sapply(items, function(x) cor(x, total_score, use = "pairwise.complete.obs"))
  
  # Calculate the average inter-item correlations
  cormatrix <- as.matrix(psych::corr.test(items)$r)
  cormatrix[cormatrix == 1.0] <- NA
  avg_interitem_corr <- rowMeans(cormatrix, na.rm = T)
  
  #save in a df
  item_correlations_df <- round(data.frame(item_total_corr = item_total_corr, avg_interitem_corr = avg_interitem_corr), 2)
  rownames(item_correlations_df) <- as.numeric(sub(string_to_delete, "", rownames(item_correlations_df)))
  item_correlations_df[] <- lapply(item_correlations_df, function(x) sub("^0", "", x))
  
  return(item_correlations_df)
}


piq23_item_correlations_df <- item_correlation_calculator(test_piq23[2:24], test$piv_total_23, "PIV_")
piq28_item_correlations_df <- item_correlation_calculator(test_piq28[2:29], test$piv_total_28, "PIV_")

#Calculate the difference in scores between the two datasets

df_merger <- function(df1, df2){
  df1$row <- rownames(df1)
  df2$row <- rownames(df2)
  
  merged_df <- merge(df1, df2, by = "row")
  
  for(i in c(1:5)){
    merged_df[,i] <- as.numeric(merged_df[,i])
  }
  
  return(merged_df)
}

piq23_28_merged <- df_merger(piq23_item_correlations_df, piq28_item_correlations_df)

item_total_diff <- piq23_28_merged$item_total_corr.y - piq23_28_merged$item_total_corr.x
avg_interitem_diff <- piq23_28_merged$avg_interitem_corr.y - piq23_28_merged$avg_interitem_corr.x

# Create a new data frame with the shared rows and the subtracted column
piq23_23_diff <- data.frame(row = piq23_28_merged$row, item_total_diff, avg_interitem_diff)




##### CHAPTER 2 - EBI ------

### STEP 1 - OMEGA -----

#using CFA based approach
#EBI6
ebi6_model <- "
f1 =~ EBI_1 + EBI_2 + EBI_3 + EBI_5 + EBI_6 + EBI_8
"
fit_ebi6 <- lavaan::cfa(ebi6_model, data = test, estimator = "MLR", std.lv = T)
omega_ebi6_cfa <- semTools::reliability(fit_ebi6)
omega_ebi6_cfa
omega_ebi6_cfa[4,1]/omega_ebi6_cfa[2,1]

#first revised model

# first revision - allowing 1 and 3 to correlate
model_ebi6_r1 <-"
  f1 =~ EBI_1 + EBI_2 + EBI_3 + EBI_5 + EBI_6 + EBI_8
  EBI_1 ~~ EBI_3
"

fit_ebi6_r1 <- lavaan::cfa(model_ebi6_r1, data = test, estimator = "MLR", std.lv = T)
omega_ebi6_cfa_r1 <- semTools::reliability(fit_ebi6_r1)
omega_ebi6_cfa_r1

omega_ebi6_cfa_r1[4,1]/omega_ebi6_cfa_r1[2,1]

#second revised model
model_ebi6_r2 <-"
  f1 =~ EBI_1 + EBI_2 + EBI_3 + EBI_5 + EBI_6 + EBI_8
  EBI_1 ~~ EBI_3
  EBI_5 ~~ EBI_8
"

fit_ebi6_r2 <- lavaan::cfa(model_ebi6_r2, data = test, estimator = "MLR", std.lv = T)
omega_ebi6_cfa_r2 <- semTools::reliability(fit_ebi6_r2)
omega_ebi6_cfa_r2

omega_ebi6_cfa_r2[4,1]/omega_ebi6_cfa_r2[2,1]



### STEP 2 - ITEM-TOTAL CORRELATIONS -----

# define a function to calculate item-total and average inter-item correlations

item_correlation_calculator <- function(items, total_score, string_to_delete){
  
  # Calculate the item-total correlations
  item_total_corr <- sapply(items, function(x) cor(x, total_score, use = "pairwise.complete.obs"))
  
  # Calculate the average inter-item correlations
  cormatrix <- as.matrix(psych::corr.test(items)$r)
  cormatrix[cormatrix == 1.0] <- NA
  avg_interitem_corr <- rowMeans(cormatrix, na.rm = T)
  
  #save in a df
  item_correlations_df <- round(data.frame(item_total_corr = item_total_corr, avg_interitem_corr = avg_interitem_corr), 2)
  rownames(item_correlations_df) <- as.numeric(sub(string_to_delete, "", rownames(item_correlations_df)))
  item_correlations_df[] <- lapply(item_correlations_df, function(x) sub("^0", "", x))
  
  return(item_correlations_df)
}

ebi6_item_correlations_df <- item_correlation_calculator(test[colnames(test_ebi6[2:7])], test$ebi_total_6, "EBI_")




##### CHAPTER 3 - PIS

### STEP 1 - OMEGA -----

#using CFA based approach
#pis6
pis6_model <- '
  f1 =~ PIS.6_1 + PIS.6_2 + PIS.6_3 + PIS.6_4 + PIS.6_5 + PIS.6_6
  '
fit_pis6 <- lavaan::cfa(pis6_model, data = test, estimator = "MLR", std.lv = T)
omega_pis6_cfa <- semTools::reliability(fit_pis6)
omega_pis6_cfa

#first revised model

# first revision - allowing 3 and 6 to correlate
model_pis6_r1 <- '
  f1 =~ PIS.6_1 + PIS.6_2 + PIS.6_3 + PIS.6_4 + PIS.6_5 + PIS.6_6
  PIS.6_3 ~~ PIS.6_6
'

fit_pis6_r1 <- lavaan::cfa(model_pis6_r1, data = test, estimator = "MLR", std.lv = T)
omega_pis6_cfa_r1 <- semTools::reliability(fit_pis6_r1)
omega_pis6_cfa_r1


#second revised model
model_pis6_r2 <- '
  f1 =~ PIS.6_1 + PIS.6_2 + PIS.6_3 + PIS.6_4 + PIS.6_5 + PIS.6_6
  PIS.6_3 ~~ PIS.6_6
  PIS.6_2 ~~ PIS.6_3
'

fit_pis6_r2 <- lavaan::cfa(model_pis6_r2, data = test, estimator = "MLR", std.lv = T)
omega_pis6_cfa_r2 <- semTools::reliability(fit_pis6_r2)
omega_pis6_cfa_r2




### STEP 2 - ITEM-TOTAL CORRELATIONS -----

# define a function to calculate item-total and average inter-item correlations

item_correlation_calculator <- function(items, total_score, string_to_delete){
  
  # Calculate the item-total correlations
  item_total_corr <- sapply(items, function(x) cor(x, total_score, use = "pairwise.complete.obs"))
  
  # Calculate the average inter-item correlations
  cormatrix <- as.matrix(psych::corr.test(items)$r)
  cormatrix[cormatrix == 1.0] <- NA
  avg_interitem_corr <- rowMeans(cormatrix, na.rm = T)
  
  #save in a df
  item_correlations_df <- round(data.frame(item_total_corr = item_total_corr, avg_interitem_corr = avg_interitem_corr), 2)
  rownames(item_correlations_df) <- as.numeric(sub(string_to_delete, "", rownames(item_correlations_df)))
  item_correlations_df[] <- lapply(item_correlations_df, function(x) sub("^0", "", x))
  
  return(item_correlations_df)
}

pis6_item_correlations_df <- item_correlation_calculator(test[item_names$item_colnames[77:82]], test$pis_total_6, "PIS.6_")