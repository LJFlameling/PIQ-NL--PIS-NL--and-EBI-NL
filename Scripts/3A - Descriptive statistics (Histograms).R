
rstudioapi::writeRStudioPreference("data_viewer_max_columns", 1000L)
rm(list = ls())

if (!require(cowplot)){
  install.packages("cowplot")
}

if (!require(doBy)){
  install.packages("doBy")
}

if (!require(moments)){
  install.packages("moments")
}

if (!require(dplyr)){
  install.packages("dplyr")
}

if (!require(tidyr)){
  install.packages("tidyr")
}

library(cowplot)
library(ggplot2)
library(doBy)
library(moments) #library for kurtosis and skewness
library(dplyr)
library(tidyr)

test <- read.csv("Data/Transformed_CSVs/test_anonymized_and_prepared.csv")
demographic_numeric_colnames <- read.csv("Data/Transformed_CSVs/demographic_numeric_colnames.csv")
demographic_numeric_colnames <- demographic_numeric_colnames$demographic_numeric_colnames
item_names <- read.csv("Data/Transformed_CSVs/all_item_names.csv")
item_colnames <- item_names$item_colnames
sumscore_names <- read.csv("Data/Transformed_CSVs/sumscore_names.csv")
sumscore_colnames <- sumscore_names$sumscore_colnames
numeric_names <- read.csv("Data/Transformed_CSVs/numeric_names.csv")
numeric_colnames <- numeric_names$numeric_colnames
likert_colnames <- read.csv("Data/Transformed_CSVs/likert_colnames.csv")
likert_colnames <- likert_colnames$likert_colnames

### STEP 1 - Histograms for all items -----

#let's firstly visually inspect the data. We create histograms and save them in a separate folder

# Create a list to store the histograms
hist_list <- list()

# Iterate through the variables and create a histogram for each
for(col in item_colnames) {
  realname <- item_names$item_realnames[item_names$item_colnames == col]
  hist_list[[col]] <- ggplot(test, aes_string(col)) + 
    geom_histogram(binwidth = 1, color = "black", fill = "white") +
    ggtitle(realname) + xlab("Score") + ylab("Frequency") +
    theme(plot.title = element_text(size = 10))
}

# Determine the number of cowplots needed
num_cowplots <- ceiling(length(item_colnames)/12)

for (i in 1:num_cowplots) {
  start_index <- (i-1)*12 + 1
  end_index <- min(i*12, length(item_colnames))
  p <- plot_grid(plotlist = hist_list[start_index:end_index], ncol = 3, nrow = 4)
  print(p)
  ggsave(filename = paste0("Figures/Descriptive stats/Histograms of all items/cowplot_",i,".png"), plot = p, width = 12, height = 8)
}

##NOW LET'S INCLUDE RETEST MEASURES
item_colnames <- numeric_colnames[1:119]

# Create a list to store the histograms
hist_list <- list()

# Iterate through the variables and create a histogram for each
for(col in item_colnames) {
  realname <- numeric_names$numeric_realnames[numeric_names$numeric_colnames == col]
  hist_list[[col]] <- ggplot(test, aes_string(col)) + 
    geom_histogram(binwidth = 1, color = "black", fill = "white") +
    ggtitle(realname) + xlab("Score") + ylab("Frequency") +
    theme(plot.title = element_text(size = 10))
}

# Determine the number of cowplots needed
num_cowplots <- ceiling(length(item_colnames)/12)

for (i in 1:num_cowplots) {
  start_index <- (i-1)*12 + 1
  end_index <- min(i*12, length(item_colnames))
  p <- plot_grid(plotlist = hist_list[start_index:end_index], ncol = 3, nrow = 4)
  print(p)
  ggsave(filename = paste0("Figures/Descriptive stats/Histograms of all items/cowplot_",i,".png"), plot = p, width = 12, height = 8)
}

### STEP 2 - Histograms for all summary scores -----

#let's repeat the same trick
# Create a list to store the histograms
hist_list <- list()

sumscore_names <- sumscore_names[-c(4, 8, 10),]
sumscore_names$sumscore_realnames[3] <- "EBI-NL"
sumscore_names$sumscore_realnames[6] <- "Retest EBI-NL"
sumscore_names$sumscore_realnames[7] <- "PIS-NL"
sumscore_colnames <- sumscore_colnames[-c(4, 8, 10)]

# Iterate through the variables and create a histogram for each
for(col in sumscore_colnames) {
  realname <- sumscore_names$sumscore_realnames[sumscore_names$sumscore_colnames == col]
  hist_list[[col]] <- ggplot(test, aes_string(col)) + 
    geom_histogram(color = "black", fill = "white") +
    ggtitle(realname) + xlab("Score") + ylab("Frequency") +
    theme(plot.title = element_text(size = 12)) 
}

#add density and normal curves
for(col in sumscore_colnames) {
  realname <- sumscore_names$sumscore_realnames[sumscore_names$sumscore_colnames == col]
  hist_list[[col]] <- ggplot(test, aes_string(col)) + 
    geom_histogram(mapping = aes(y=..density..), color = "black", fill = "white") +
    geom_density(color = "red") +
    stat_function(fun = dnorm, args = list(mean = mean(test[[col]], na.rm = T), sd = sd(test[[col]], na.rm = T))) +
    ggtitle(realname) + xlab("Score") + ylab("Density") +
    theme(plot.title = element_text(size = 12)) 
}

cowplot_sumscores <- plot_grid(plotlist = hist_list)
print(cowplot_sumscores)

ggsave(filename = "Figures/Descriptive stats/histograms_totalscores.png", plot = cowplot_sumscores, width = 12, height = 8)

### STEP 3 - Histograms for age
age <- ggplot(test, aes(x = Leeftijd)) + 
  geom_histogram(binwidth = 1, color = "black", fill = "white") +
  xlab("Age") + ylab("Frequency") +
  theme(text = element_text(size = 20, family = "serif"))
age

ggsave(filename = "Figures/Descriptive stats/age.png", plot = age, width = 12, height = 8)

### STEP 4 - Histogram for how long test and retest measure was separated
test_retest_time <- ggplot(test, aes(x = abs(test_rt_time_difference))) + 
  geom_histogram(binwidth = 1, color = "black", fill = "white") +
  xlab(expression(paste("Difference between test and retest ", italic("(days)")))) + ylab("Frequency") +
  theme(text = element_text(size = 20, family = "serif"))
test_retest_time

ggsave(filename = "Figures/Descriptive stats/histogram_test_retest_time_separation.png", plot = test_retest_time, width = 12, height = 8)
