rstudioapi::writeRStudioPreference("data_viewer_max_columns", 1000L)
rm(list = ls())
options(rlib_downstream_check = FALSE)

if(!require(pacman)){
  install.packages("pacman")
} #install the pacman package to load the other packages only if it hasn't been installed yet
pacman::p_load(doBy, moments, dplyr, tidyr) #load the other packages needed in this script

test <- read.csv("Data/Transformed_CSVs/test_anonymized_and_prepared.csv")
demographic_numeric_colnames <- read.csv("Data/Transformed_CSVs/demographic_numeric_colnames.csv")
demographic_numeric_colnames <- demographic_numeric_colnames$demographic_numeric_colnames
item_names <- read.csv("Data/Transformed_CSVs/all_item_names.csv")
item_colnames <- item_names$item_colnames
piv_colnames <- item_names$item_colnames[1:28]
sumscore_names <- read.csv("Data/Transformed_CSVs/sumscore_names.csv")
sumscore_colnames <- sumscore_names$sumscore_colnames
numeric_names <- read.csv("Data/Transformed_CSVs/numeric_names.csv")
numeric_colnames <- numeric_names$numeric_colnames
likert_colnames <- read.csv("Data/Transformed_CSVs/likert_colnames.csv")
likert_colnames <- likert_colnames$likert_colnames

### STEP 1 - Univariate normality tests -----
item_and_sumscore_colnames <- c(item_colnames, sumscore_colnames)
results_shapiro <- lapply(test[, item_and_sumscore_colnames], shapiro.test)
results_ks <- lapply(test[, item_and_sumscore_colnames], function(x) ks.test(x, y = "pnorm"))

# Extract the p-values and statistics from the results
shapiro_pvals <- sapply(results_shapiro, function(x) x$p.value)
shapiro_stats <- sapply(results_shapiro, function(x) x$statistic)
ks_pvals <- sapply(results_ks, function(x) x$p.value)
ks_stats <- sapply(results_ks, function(x) x$statistic)

# Bind the results together in a dataframe
normality_results <- data.frame(shapiro_pvals, shapiro_stats, ks_pvals, ks_stats)

### STEP 2 - Descriptive statistics -----

### STEP 2A - Means, SDs, kurtosis and skewness for all numeric variables

# apply summary statistics to each numeric variable
descriptives_list <- lapply(test[, numeric_colnames], function(x) c(mean = mean(x, na.rm = TRUE),
                                                                    sd = sd(x, na.rm = TRUE), 
                                                                    kurtosis = kurtosis(x, na.rm = TRUE),
                                                                    skewness = skewness(x, na.rm = TRUE)))

# convert results to a dataframe
descriptives_df <- as.data.frame(descriptives_list) %>%
  t() #transpose the axes

#round the decimals
descriptives_df[,c(3,4)] <- round(descriptives_df[,c(3,4)], 2) #round kurtosis and skewness to 2 decimals
descriptives_df[,c(1,2)] <- round(descriptives_df[,c(1,2)], 1) #round mean and sd to 1 decimal

#adjust the rownames to be the realnames
row.names(descriptives_df) <- numeric_names$numeric_realnames[numeric_names$numeric_colnames == rownames(descriptives_df)] 

#select only the descriptives of the PIQ, PIS, and EBI
descriptives_3questionnaires <- descriptives_df[c(item_names$item_realnames[1:28], item_names$item_realnames[77:83], item_names$item_realnames[29:36]),]

descriptives_total_scores <- descriptives_df[c(sumscore_names$sumscore_realnames), ]

setwd("/Users/jopflameling/Documenten/Psychology/Master/Thesis/ARQ Thesis/Thesis - PIQ Validation/Data analysis/CSVs")
write.csv(descriptives_3questionnaires, "descriptives_3questionnaires.csv")
write.csv(descriptives_total_scores, "descriptives_total_scores.csv")
setwd("/Users/jopflameling/Documenten/Psychology/Master/Thesis/ARQ Thesis/Thesis - PIQ Validation/Data analysis")

## AGE - Only demographic continuous variable
#get the range of the age variable
range(test$Leeftijd)
range(test$Leeftijd[!is.na(test$date_rt)])

# get the perc of ppl below age of 35
length(test$Leeftijd[test$Leeftijd < 36])/length(test$Leeftijd)

### STEP 2B - Descriptives for categorical variables
categorical_colnames <- c("Geslacht", "Opleiding", "Werksituatie", "Wanneer.", "Welk.middel.", "Hoogte.dosis.", "Eerdere.ervaring", "Setting", "Begeleiding", "Inzicht", "Emotionele.doorbraak")

# create an empty dataframe to store the summary
categorical_descriptives_df <- data.frame()
categorical_descriptives_df_rt <- data.frame()
categorical_descriptives_df_not_rt <- data.frame()

# iterate through the column names and summarize the data
for (col in categorical_colnames) {
  temp_df <- data.frame(percent = round(prop.table(table(test[,col])) * 100, 1))
  temp_df$column_name <- col
  categorical_descriptives_df <- rbind(categorical_descriptives_df, temp_df)
}

# let's do this separately for the retest data
for (col in categorical_colnames) {
  df <- test[!is.na(test$Progress_rt) & test$Progress_rt > 49,] #let's firsly filter out all NAs and everyone who did not complete at least the PIQ
  temp_df <- data.frame(percent = round(prop.table(table(df[,col])) * 100, 1))
  temp_df$column_name <- col
  categorical_descriptives_df_rt <- rbind(categorical_descriptives_df_rt, temp_df)
}

# let's do this separately for those who did not fill out the retest 
for (col in categorical_colnames) {
  df <- test[is.na(test$Progress_rt) | test$Progress_rt < 50,] #let's firsly filter out all NAs and everyone who did not complete at least the PIQ
  temp_df <- data.frame(percent = round(prop.table(table(df[,col])) * 100, 1))
  temp_df$column_name <- col
  categorical_descriptives_df_not_rt <- rbind(categorical_descriptives_df_not_rt, temp_df)
}

#manually add empty rows for the answer options that were never chosen by the retest sample
categorical_descriptives_df_rt <- dplyr::add_row(
  categorical_descriptives_df_rt,
  percent.Var1 = "Wil ik liever niet zeggen",
  percent.Freq = 0,
  column_name = "Geslacht",
  .before = 4
)

categorical_descriptives_df_rt <- dplyr::add_row(
  categorical_descriptives_df_rt,
  percent.Var1 = "MBO-1",
  percent.Freq = 0,
  column_name = "Opleiding",
  .before = 8
)

#manually add empty rows for the answer options that were never chosen by the not-retest sample
categorical_descriptives_df_not_rt <- dplyr::add_row(
  categorical_descriptives_df_not_rt,
  percent.Var1 = "Anders",
  percent.Freq = 0,
  column_name = "Geslacht",
  .before = 1
)

categorical_descriptives_df_not_rt <- dplyr::add_row(
  categorical_descriptives_df_not_rt,
  percent.Var1 = "Mescaline",
  percent.Freq = 0,
  column_name = "Welk.middel.",
  .before = 32
)

#give them the right colnames
colnames(categorical_descriptives_df_rt) <- c("var1", "percent.Freq_rt", "column_name")
colnames(categorical_descriptives_df_not_rt) <- c("percent.Var1", "percent.Freq_not_rt", "column_name")

#bind the test and retest and not-retest together
categorical_descriptives <- cbind(categorical_descriptives_df, categorical_descriptives_df_rt, categorical_descriptives_df_not_rt)

#get rid of the unnecessary columns
categorical_descriptives <- categorical_descriptives[c("percent.Var1", "percent.Freq", "percent.Freq_rt", "percent.Freq_not_rt")]

#now let's see if the test and retest completers are statistically different
lapply(categorical_colnames, function(x) chisq.test(test[, x], test$rt_completed))

tester <- chisq.test(test$Geslacht, test$rt_completed, simulate.p.value = T)

chisq_stats_df <- data.frame()

for(i in categorical_colnames){
  chisq_summary <- chisq.test(test[, i], test$rt_completed)
  chisq <- chisq_summary$statistic
  df <- chisq_summary$parameter
  p <- chisq_summary$p.value
  row <- c(i, chisq, df, p)
  chisq_stats_df <- rbind(chisq_stats_df, row)
}

chisq_stats_df_monte_carlo <- data.frame()

for(i in categorical_colnames){
  chisq_summary <- chisq.test(test[, i], test$rt_completed, simulate.p.value = T)
  chisq <- chisq_summary$statistic
  df <- chisq_summary$parameter
  p <- chisq_summary$p.value
  row <- c(i, chisq, df, p)
  chisq_stats_df_monte_carlo <- rbind(chisq_stats_df_monte_carlo, row)
}

## NOW REORDER THE DATAFRAME SO IT IS EASY TO COPY-PASTE INTO WORD

#manually make the "anders namelijk" specific, so we can use them to order the categoricaL_descriptives dataframe
categorical_descriptives$percent.Var1 <- as.character(categorical_descriptives$percent.Var1) #firstly turn into a string
categorical_descriptives_df$percent.Var1 <- as.character(categorical_descriptives_df$percent.Var1) #firstly turn into a string
categorical_descriptives_df_rt$var1 <- as.character(categorical_descriptives_df_rt$var1) #firstly turn into a string
categorical_descriptives_df_not_rt$percent.Var1 <- as.character(categorical_descriptives_df_not_rt$percent.Var1) #firstly turn into a string

categorical_descriptives$percent.Var1[1] <- "Anders_geslacht"
categorical_descriptives$percent.Var1[45] <- "Anders_setting"
categorical_descriptives$percent.Var1[5] <- "Anders_nml_opleiding"
categorical_descriptives$percent.Var1[13] <- "Anders_nml_werksituatie"
categorical_descriptives$percent.Var1[25] <- "Anders_nml_drug"
categorical_descriptives$percent.Var1[42] <- "Nee_eerdere_ervaring"
categorical_descriptives$percent.Var1[52] <- "Nee_begeleiding"
categorical_descriptives$percent.Var1[54] <- "Nee_inzicht"
categorical_descriptives$percent.Var1[56] <- "Nee_doorbraak"
categorical_descriptives$percent.Var1[51] <- "Ja_begeleiding"
categorical_descriptives$percent.Var1[53] <- "Ja_inzicht"
categorical_descriptives$percent.Var1[55] <- "Ja_doorbraak"

ordered_answer_options <- c("Vrouw", "Man", "Anders_geslacht", "Wil ik liever niet zeggen", "WO", "HBO", "MBO-2-4", "MBO-1", "VWO", "HAVO", "VMBO", "Anders_nml_opleiding", "Werkend", "Studerend", "Werkloos", "Anders_nml_werksituatie", "Minder dan een maand geleden", "Tussen één en zes maanden geleden", "Tussen zes en twaalf maanden geleden", "Tussen één en twee jaar geleden", "Tussen twee en vijf jaar geleden", "Tussen vijf en tien jaar geleden", "Meer dan tien jaar geleden", "Psilocybine (\"truffels\" of \"paddo's\")", "LSD", "2C-B", "Een combinatie van bovenstaande middelen", "MDMA", "Ayahuasca", "Ketamine", "DMT", "Mescaline","Anders_nml_drug", "Hoog", "Gemiddeld", "Laag", "Nee_eerdere_ervaring", "Eén keer", "Twee tot vijf keer", "Zes tot tien keer", "Elf tot twintig keer", "21 tot 50 keer", "51 tot 100 keer", "Meer dan 100 keer", "Thuis", "In de natuur", "In een feest-setting", "Tijdens een \"retreat\"", "In een therapeutische setting", "Anders_setting", "Ja_begeleiding", "Nee_begeleiding", "Ja_inzicht", "Nee_inzicht", "Ja_doorbraak", "Nee_doorbraak")

categorical_descriptives <- categorical_descriptives[match(ordered_answer_options, categorical_descriptives$percent.Var1),]





### STEP 3 - Percentage with response option -----

#let's firstly create an empty dataframe
total_0 <- rep(0, 28)
total_1 <- rep(0, 28)
total_2 <- rep(0, 28)
total_3 <- rep(0,28)
total_4 <- rep(0, 28)
total_5 <- rep(0, 28)

total_resp_options_perc <- data.frame(total_0, total_1, total_2, total_3, total_4, total_5)
rownames(total_resp_options_perc) <- piv_colnames
total_resp_options_count <- total_resp_options_perc

total_resp_options_word <- total_resp_options_perc

piv_response_options <- c(0:5)

#let's use a for-loop to count
for(colname in piv_colnames){
  
  #first we define the column from the test df we want to use, e.g. the PIV_1 column
  col <- test[colname]
  
  #then we loop through the response options, 0 to 5
  for(resp_option in piv_response_options){
    
    #we create the name of the column in the total_resp_options df, e.g. "total_0"
    resp_option_chosen <- paste("total", resp_option, sep ="_")
    
    #we calculate how often this response option was chosen in the given column, e.g. how often 0 was chosen in the PIV_1 column
    times_chosen <- length(col[!is.na(col) & col == resp_option])
    
    #let's save this in the count df
    total_resp_options_count[colname, resp_option_chosen] <- times_chosen
    
    #calculate the percentage
    perc_chosen <- (times_chosen/nrow(test[!is.na(col),]))*100 
    perc_chosen <- round(perc_chosen, 0)
    
    #let's save this in the perc df
    total_resp_options_perc[colname, resp_option_chosen] <- perc_chosen
    
    #save this in a string that can be easily copied
    answer <- paste(perc_chosen, "% (", times_chosen, ")", sep = "")
    
    # we save this to the total_resp_options dataframe, using "PIV_1" and "total_0" to select the correct row and column, respectively
    total_resp_options_word[colname, resp_option_chosen] <- answer
  }
}

#let's investigate what the most sparsely used categories are
min(total_resp_options_count)
min(total_resp_options_perc)

#let's prepare this df to put into the word file and write the csv for easy copy-pasting
rownames(total_resp_options_word) <- item_names$item_realnames[1:28]
colnames(total_resp_options_word) <- c(0:5)

### Step 4 - Perc. max/min scores ----

#create an extra column with the maximum theoretical scores per questionnaire
sumscore_names$max <- c(115, 140, 600, 800, 115, 140, 600, 800, 600, 700, 150, 35, 35)
sumscore_names$min <- c(rep(0, 11), 1, 1)

for(colname in sumscore_names$sumscore_colnames){
  col <- test[[colname]]
  col <- col[!is.na(col)]
  min <- sumscore_names$min[sumscore_names$sumscore_colnames == colname]
  max <- sumscore_names$max[sumscore_names$sumscore_colnames == colname]
  number_min <- length(col[col == min])
  perc_min <- number_min/length(col)*100
  number_max <- length(col[col == max])
  perc_max <- number_max/length(col)*100
  
  print(paste("Number min score ", colname, " :", number_min, "; perc min score: ", perc_min, sep = ""))
  
  #print(paste("Number max score ", colname, " :", number_max, "; perc max score: ", perc_max, sep = ""))
}

### STEP 5 - Difference between test and retest
summary(abs(test$test_rt_time_difference))
sd(abs(test$test_rt_time_difference), na.rm = T)

setwd("/Users/jopflameling/Documenten/Psychology/Master/Thesis/ARQ Thesis/Thesis - PIQ Validation/Data analysis/CSVs")
write.csv(total_resp_options_word, "total_resp_options_word.csv")
