rstudioapi::writeRStudioPreference("data_viewer_max_columns", 1000L)
rm(list = ls())

if(!require(pacman)){
  install.packages("pacman")
}
library(pacman)

update.packages(checkBuilt = TRUE, ask = FALSE)

pacman::p_load(dynamic,
               Cairo, #NOTE: ON MACOS THIS PACKAGE REQUIRES YOU TO HAVE INSTALLED XQUARTZ VERSION X11 OR MORE
               corrplot,
               remotes,
               lavaan,
               ggplot2,
               dplyr,
               tidyr,
               reshape2,
               remotes)


#import data
test <- read.csv("Data/Transformed_CSVs/test_anonymized_and_prepared.csv")
test_ebi6 <- read.csv('Data/Transformed_CSVs/test_ebi6.csv')
test_pis6 <- read.csv("Data/Transformed_CSVs/test_pis6.csv")
nonparty <- test[!test$Setting == "In een feest-setting",] 
only_psilo_lsd <- test[test$Welk.middel. == "LSD" | test$Welk.middel. == "Psilocybine (\"truffels\" of \"paddo's\")" ,] 
nonparty_only_psilo_lsd <- nonparty[nonparty$Welk.middel. == "LSD" | nonparty$Welk.middel. == "Psilocybine (\"truffels\" of \"paddo's\")" ,] 

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

### Running the main models -----

fit_piq23 <- lavaan::cfa(piq23_model, data = test, estimator = "MLR", std.lv = T)
summary(fit_piq23, fit.measures = TRUE, rsquare = T)
dfi_piq23 <- cfaHB(fit_piq23)
dfi_piq23

fit_pis6 <- lavaan::cfa(pis6_model, data = test_pis6, estimator = "MLR", std.lv = T)
summary(fit_pis6, fit.measures = TRUE)
dfi_pis6 <- cfaOne(fit_pis6)
dfi_pis6


fit_ebi6 <- lavaan::cfa(ebi6_model, data = test_ebi6, estimator = "MLR", std.lv = T)
summary(fit_ebi6, fit.measures = TRUE, modindices = T)
dfi_ebi6 <- cfaOne(fit_ebi6, plot = T)
dfi_ebi6



### STEP 2 - Calculate the fit measures for different inputs -----
fit_measure_loop <- function(estimators, data_names, model) {
  
  measures <- c("chisq.scaled", "df.scaled", "pvalue.scaled", "cfi.robust", "rmsea.robust", "rmsea.ci.lower.robust", "rmsea.ci.upper.robust", "srmr", "wrmr")
  
  fit_df <- data.frame(data_name = character(),
                       estimator = character(),
                       chisq.scaled = double(),
                       df.scaled = double(),
                       pvalue.scaled = double(),
                       cfi.robust = double(),
                       rmsea.robust = double(),
                       rmsea.ci.lower.robust = double(),
                       rmsea.ci.upper.robust = double(),
                       srmr = double(),
                       wrmr = double(),
                       stringsAsFactors = FALSE)
  
  #set orthogonal = T if the model is a bifactor model, as indicated by the presence of "bf" in the model name
  if(grepl("bf", deparse(substitute(model)))){
    rotation = T
  }
  else{
    rotation = F
  }
  
  for (data_name in data_names) {
    data <- get(data_name)
    
    for (estimator in estimators) {
      if(estimator == "MLR" | estimator == "MLM" | estimator == "MLMV"){
        categorical <- FALSE
      }  
      else{
        categorical <- TRUE
      }
      
      fit <- lavaan::cfa(model, data = data, estimator = estimator, ordered = categorical, std.lv = T, orthogonal = rotation)
      
      measures_result <- fitmeasures(fit, fit.measures = measures)
      
      fit_row <- tibble(data_name = data_name,
                        estimator = estimator,
                        chisq.scaled = measures_result['chisq.scaled'],
                        df.scaled = measures_result['df.scaled'],
                        pvalue.scaled = measures_result['pvalue.scaled'],
                        cfi.robust = measures_result['cfi.robust'],
                        rmsea.robust = measures_result['rmsea.robust'],
                        rmsea.ci.lower.robust = measures_result['rmsea.ci.lower.robust'],
                        rmsea.ci.upper.robust = measures_result['rmsea.ci.upper.robust'],
                        srmr = measures_result['srmr'],
                        wrmr = measures_result['wrmr'])
      
      fit_df <- bind_rows(fit_df, fit_row)
      
    }
  }
  
  fit_df <- fit_df %>% arrange(data_name, estimator)
  

  
  return(fit_df)
}

#define the parameters
piq_estimators <- c("MLR", "MLM", "MLMV", "WLSMV", "ULSMV")
ml_estimators <- c("MLR", "MLM", "MLMV")
dataframes <- c("test", "nonparty", "only_psilo_lsd", "nonparty_only_psilo_lsd")


# call the function to calculate the fit measures
piq23_fit_df <- fit_measure_loop(piq_estimators, dataframes, piq23_model)
ebi6_fit_df <- fit_measure_loop(ml_estimators, dataframes, ebi6_model)
pis6_fit_df <- fit_measure_loop(ml_estimators, dataframes, pis6_model)

### STEP 3 - Plot the results -----

generate_plots <- function(fit_df) {
  
  plot_data <- subset(fit_df, select=c("data_name", 
                                       "estimator", 
                                       "srmr",
                                       "cfi.robust", 
                                       "rmsea.robust", 
                                       "rmsea.ci.lower.robust", 
                                       "rmsea.ci.upper.robust")) 
  
  # Reshape data to long format
  plot_data_long <- reshape2::melt(plot_data, id.vars=c("data_name", "estimator"))
  
  # Create a separate column with the real data names
  plot_data_long$real_data_name <- c(1:nrow(plot_data_long))
  
  for(i in c(1:nrow(plot_data_long))){
    
    if(plot_data_long$data_name[i] == "nonparty" | plot_data_long$data_name[i] == "nonparty_only_psilo_lsd"){
      plot_data_long$real_data_name[i] <- "Excluding party settings"
    }
    
    if(plot_data_long$data_name[i] == "test" | plot_data_long$data_name[i] == "only_psilo_lsd"){
      plot_data_long$real_data_name[i] <- "Including all settings"
    }
  }
  
  # Create a separate column with the real substance names
  plot_data_long$real_substance_name <- c(1:nrow(plot_data_long))
  
  for(i in c(1:nrow(plot_data_long))){
    
    if(plot_data_long$data_name[i] == "nonparty" | plot_data_long$data_name[i] == "test"){
      plot_data_long$real_substance_name[i] <- "Including all substances"
    }
    
    if(plot_data_long$data_name[i] == "nonparty_only_psilo_lsd" | plot_data_long$data_name[i] == "only_psilo_lsd"){
      plot_data_long$real_substance_name[i] <- "Including only psilocybin and LSD"
    }
  }

  
  # Define colors
  my_colors <- c("#4E79A7", "#F28E2B")
  
  # Define red line positions
  cfi_red_line <- 0.95
  srmr_red_line <- 0.08
  rmsea_red_line <- 0.06
  
  # Create plots
  cfi_plot <- ggplot(plot_data_long[plot_data_long$variable == "cfi.robust", ], aes(x = value, y = paste(data_name, estimator), color = real_data_name)) +
    geom_point(aes(shape = real_substance_name)) +
    geom_vline(xintercept = cfi_red_line, color = "red") +
    scale_y_discrete(label = plot_data_long$estimator) +
    labs(shape="Substance", colour="Setting") +
    xlab("CFI score") +
    ylab("") + 
    theme(text = element_text(size = 10,
                              family = "Times New Roman"))
  
  # Create plots
  srmr_plot <- ggplot(plot_data_long[plot_data_long$variable == "srmr", ], aes(x = value, y = paste(data_name, estimator), color = real_data_name)) +
    geom_point(aes(shape = real_substance_name)) +
    geom_vline(xintercept = srmr_red_line, color = "red") +
    scale_y_discrete(label = plot_data_long$estimator) +
    labs(shape="Substance", colour="Setting") +
    xlab("SRMR score") +
    ylab("") + 
    theme(text = element_text(size = 10,
                              family = "Times New Roman"))
  
  # pivot the plot_data_long dataframe so that the lower and upper RMSEA CI values are in separate columns
  plot_data_long_wide <- plot_data_long %>%
    pivot_wider(names_from = variable, values_from = value)
  
  # create the plot with error bars
  rmsea_plot <- ggplot(plot_data_long_wide, aes(x = rmsea.robust, y = paste(data_name, estimator), color = real_data_name)) +
    geom_point(position = position_dodge(width = 0.2), aes(shape = real_substance_name)) +
    geom_errorbarh(aes(xmin = rmsea.ci.lower.robust, xmax = rmsea.ci.upper.robust), height = 0.2, position = position_dodge(width = 0.2)) +
    geom_vline(xintercept = rmsea_red_line, color = "red") +
    scale_y_discrete(label = plot_data_long_wide$estimator) +
    labs(shape="Substance", colour="Setting") +
    xlab("RMSEA score") +
    ylab("") +
    theme(text = element_text(size = 10,
                              family = "Times New Roman"))
  
  # Display plots
  print(srmr_plot)
  print(cfi_plot)
  print(rmsea_plot)
  
  #save plots
  ggsave(filename = paste0("Figures/CFA/rmsea_scores_", deparse(substitute(fit_df)) ,".png"), plot = rmsea_plot, width = 6, height = 4)
  ggsave(filename = paste0("Figures/CFA/cfi_scores_", deparse(substitute(fit_df)) ,".png"), plot = cfi_plot, width = 6, height = 4)
  ggsave(filename = paste0("Figures/CFA/srmr_scores_", deparse(substitute(fit_df)) ,".png"), plot = srmr_plot, width = 6, height = 4)
}

generate_plots(piq23_fit_df)
generate_plots(pis6_fit_df)
generate_plots(ebi6_fit_df)

### PART 2 - Model revision -----

### STEP 2.1 - Standardized residuals -----

# PIQ
residual_maker <- function(fit, colname_to_be_deleted){
  df <- round(resid(fit, type = "cor")$cov, 2)
  
  colnames(df) <- gsub(colname_to_be_deleted, "", colnames(df))
  rownames(df) <- gsub(colname_to_be_deleted, "", rownames(df))
  
  return(df)
}

residuals_piq23 <- residual_maker(fit_piq23, "PIV_")
residuals_pis6 <- residual_maker(fit_pis6, "PIS.6_")
residuals_ebi6 <- residual_maker(fit_ebi6, "EBI_")

residual_plot_maker <- function(residual_df){
  
  corrplot(residual_df, 
           method = "color", 
           #type = "upper",
           #col.lim = c(-0.20, 0.20), 
           is.corr = F, 
           p.mat = abs(residual_df),
           sig.level = 0.10,
           tl.col = "black", 
           tl.srt=0
  )
  recordPlot()
}

#PIQ23
Cairo(1200, 1200, file="Figures/CFA/piq23_resplot.png", pointsize = 24, type="png", bg="white")
residual_plot_piq23 <- residual_plot_maker(residuals_piq23)
dev.off()


#PIS
Cairo(1200, 1200, file="Figures/CFA/pis6_resplot.png", pointsize = 24, type="png", bg="white")
residual_plot_pis6 <- residual_plot_maker(residuals_pis6)
residual_plot_pis6
dev.off()

#EBI6
Cairo(1200, 1200, file="Figures/CFA/ebi6_resplot.png", pointsize = 24, type="png", bg="white")
residual_plot_ebi6 <- residual_plot_maker(residuals_ebi6)
residual_plot_ebi6 
dev.off()

### STEP 2.2 - Modification index and standardized expected parameter change -----

##PIQ23 ----
modindices_piq23 <- modindices(fit_piq23, sort = TRUE)
modindices_piq23 <- modindices_piq23[which(modindices_piq23$mi > 4.0),][c(1:4, 7)]

#adding PIV_1 ~~ PIV_2
piq23_r1_model <- "
  f1 =~ PIV_1 + PIV_2 + PIV_3 + PIV_4 + PIV_5 + PIV_6 + PIV_7 + PIV_8 + PIV_9 + PIV_10 + PIV_11 + PIV_12 + PIV_13 + PIV_16
  f2 =~ PIV_17 + PIV_18 + PIV_19 + PIV_20 + PIV_22 + PIV_23 + PIV_25 + PIV_27 + PIV_28
  PIV_1 ~~ PIV_2
  
"

fit_piq23_r1 <- lavaan::cfa(piq23_r1_model, data = test, estimator = "MLR")
fitmeasures(fit_piq23_r1)[c("chisq.scaled", "df.scaled", "pvalue.scaled", "cfi.robust", "rmsea.robust", "srmr")]


#letting 1 and 2 correlate, and creating a separate factor for 7, 8, and 9
piq23_r2_model <- "
  f1 =~ PIV_1 + PIV_2 + PIV_3 + PIV_4 + PIV_5 + PIV_6  + PIV_10 + PIV_11 + PIV_12 + PIV_13 + PIV_16
  f2 =~ PIV_17 + PIV_18 + PIV_19 + PIV_20 + PIV_22 + PIV_23 + PIV_25 + PIV_27 + PIV_28
  PIV_1 ~~ PIV_2
  f3 =~ PIV_7 + PIV_8 + PIV_9
"

fit_piq23_r2 <- lavaan::cfa(piq23_r2_model, data = test, estimator = "MLR")
fitmeasures(fit_piq23_r2)[c("chisq.scaled", "df.scaled", "pvalue.scaled", "cfi.robust", "rmsea.robust", "srmr")]

modindices_piq23_r2 <- modindices(fit_piq23_r2, sort = TRUE)
modindices_piq23_r2 <- modindices_piq23_r2[which(modindices_piq23_r2$mi > 4.0),]
length(modindices_piq23_r2$mi)

piq23_r2_fit_df <- fit_measure_loop(piq_estimators, dataframes, piq23_r2_model)
generate_plots(piq23_r2_fit_df)


## EBI6 -----
fit_ebi6_modindices <- lavaan::cfa(ebi6_model, data = test_ebi6/10, estimator = "MLR", std.lv = T)
modindices_ebi6 <- modindices(fit_ebi6_modindices, sort = T, na.remove = T)
View(round(modindices_ebi6[c(4,7)],2))


# first revision - allowing 1 and 3 to correlate
model_ebi6_r1 <-"
  f1 =~ EBI_1 + EBI_2 + EBI_3 + EBI_5 + EBI_6 + EBI_8
  EBI_1 ~~ EBI_3
"

fit_ebi6_r1 <- lavaan::cfa(model_ebi6_r1, data = test, estimator = "MLR", std.lv = T)
summary(fit_ebi6_r1, fit.measures = TRUE, modindices = T)

ebi6_r1_fit_df <- fit_measure_loop(ml_estimators, dataframes, model_ebi6_r1)
generate_plots(ebi6_r1_fit_df)

# second revision - allowing 5 and 8 to correlate
model_ebi6_r2 <-"
  f1 =~ EBI_1 + EBI_2 + EBI_3 + EBI_5 + EBI_6 + EBI_8
  EBI_1 ~~ EBI_3
  EBI_5 ~~ EBI_8
"

fit_ebi6_r2 <- lavaan::cfa(model_ebi6_r2, data = test, estimator = "MLR", std.lv = T)
summary(fit_ebi6_r2, fit.measures = TRUE, modindices = T)

ebi6_r2_fit_df <- fit_measure_loop(ml_estimators, dataframes, model_ebi6_r2)
generate_plots(ebi6_r2_fit_df)


## PIS6 -----
fit_pis6_modindices <- lavaan::cfa(pis6_model, data = test_pis6/10, estimator = "MLR")

modindices_pis6 <- modindices(fit_pis6_modindices, sort = T, na.remove = T)
View(round(modindices_pis6[c(4,7)],2))


# first revision - allowing 3 and 6 to correlate
model_pis6_r1 <- '
  f1 =~ PIS.6_1 + PIS.6_2 + PIS.6_3 + PIS.6_4 + PIS.6_5 + PIS.6_6
  PIS.6_3 ~~ PIS.6_6
'

fit_pis6_r1 <- lavaan::cfa(model_pis6_r1, data = test_pis6/10, estimator = "MLR", std.lv = T)
summary(fit_pis6_r1, fit.measures = TRUE, modindices = T)

pis6_r1_fit_df <- fit_measure_loop(ml_estimators, dataframes, model_pis6_r1)
generate_plots(pis6_r1_fit_df)

modindices_pis6_r1 <- modindices(fit_pis6_r1, sort = T, na.remove = T)[c(1:4, 7)]

# second revision - allowing 2 and 3 to correlate
model_pis6_r2 <- '
  f1 =~ PIS.6_1 + PIS.6_2 + PIS.6_3 + PIS.6_4 + PIS.6_5 + PIS.6_6
  PIS.6_3 ~~ PIS.6_6
  PIS.6_2 ~~ PIS.6_3
'

fit_pis6_r2 <- lavaan::cfa(model_pis6_r2, data = test, estimator = "MLR", std.lv = T)
summary(fit_pis6_r2, fit.measures = TRUE, modindices = T)

pis6_r2_fit_df <- fit_measure_loop(ml_estimators, dataframes, model_pis6_r2)
generate_plots(pis6_r2_fit_df)

### PART 3 - PLAIN CORRELATION MATRIX -----

cormatrix_maker <- function(data, colname_to_be_deleted){
  corrmatrix <- cor(data, use = "pairwise.complete.obs")
  
  colnames(corrmatrix) <- gsub(colname_to_be_deleted, "", colnames(corrmatrix))
  rownames(corrmatrix) <- gsub(colname_to_be_deleted, "", rownames(corrmatrix))
  
  corrplot(corrmatrix, 
           method = "color", 
           #type = "upper",
           is.corr = T, 
           #sig.level = 0.05,
           tl.col = "black", 
           tl.srt=0,
           number.font = 4,
           addCoef.col = "black")
  
  recordPlot()
}

#EBI6
Cairo(600, 600, file="Figures/CFA/ebi6_cormatrix.png", pointsize = 24, type="png", bg="white")
cormatrix_ebi6 <- cormatrix_maker(test[c("EBI_1", "EBI_2", "EBI_3", "EBI_5", "EBI_6", "EBI_8")], colname_to_be_deleted = "EBI_")
cormatrix_ebi6
dev.off()

#PIS6
Cairo(600, 600, file="Figures/CFA/pis6_cormatrix.png", pointsize = 24, type="png", bg="white")
cormatrix_pis6 <- cormatrix_maker(test[c("PIS.6_1", "PIS.6_2", "PIS.6_3", "PIS.6_4", "PIS.6_5", "PIS.6_6")], colname_to_be_deleted = "PIS.6_")
cormatrix_pis6
dev.off()
