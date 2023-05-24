

if (!require(rstudioapi)){
  install.packages("rstudioapi")
}
library(rstudioapi)
rstudioapi::writeRStudioPreference("data_viewer_max_columns", 1000L)

rm(list = ls())

if (!require(ggplot2)) {
  install.packages("ggplot2")
}
library(ggplot2)
if (!require(nortest)) {
  install.packages("nortest")
}
library(nortest)

test <- read.csv("Data/Transformed_CSVs/test_anonymized_and_prepared.csv")

### STEP 1 - Find number of completers per questionnaire -----

#define cutoffs
cutoffs <- data.frame(c(71, 74, 81, 84, 97), c("PIQ", "EBI", "PIS", "MEQ", "SWLS-2"))
colnames(cutoffs) <- c("score", "name")

cutoffs_rt <- data.frame(c(50, 83), c("PIQ_rt", "EBI_rt"))
colnames(cutoffs_rt) <- c("score_rt", "name_rt")

#find how many people completed each test
for (i in 1:nrow(cutoffs)) {
  score <- cutoffs[i,1]
  testname <- cutoffs[i,2]
  completed <- length(test$Progress[test$Progress > score - 1])
  print(paste("Number of people who completed the", testname, "is ", completed))
}

#find how many people completed each retest
for (i in 1:nrow(cutoffs_rt)) {
  score <- cutoffs_rt[i,1]
  testname <- cutoffs_rt[i,2]
  completed <- length(test$Progress_rt[!is.na(test$Progress_rt) & test$Progress_rt > score - 1])
  print(paste("Number of people who completed the", testname, "is ", completed))
}

### STEP 2 - Identify careless responders -----

#define response time per item
cutoffs$number_of_completed_items <- c(43, 51, 58, 88, 98) #define the number of completed questions per number of completed questionnaires. This excludes the questions where participants are asked to give their email address and/or phone number.

cutoffs_rt$number_of_completed_items <- c(28, 36)

#create empty columns
test$number_of_completed_items <- rep(0, nrow(test)) 
test$number_of_completed_rt_items <- rep(0, nrow(test)) 

#add the number of completed items to the empty column
for(i in 1:nrow(test)){
  progress <- test$Progress[i]
  if(progress == 100){
    progress <- 97
  }
  number_of_completed_items <- cutoffs$number_of_completed_items[cutoffs$score == progress]
  test$number_of_completed_items[i] <- number_of_completed_items
} 

#the one for the rt is a bit more difficult
for(i in 1:nrow(test)){
  progress <- test$Progress_rt[i]
  
  if(!is.na(progress)){ #skip all NAs
    if(progress == 100){ #make all 100s and 83s the same
      progress <- 83
    }
    
    if(progress > 49){ # do this only for the ones with complete information
      number_of_completed_rt_items <- cutoffs_rt$number_of_completed_items[cutoffs_rt$score == progress]
    }
    
    if(progress < 50){ #make NA, because otherwise we'll have to divide by 0 in a bit
      number_of_completed_rt_items <- NA
    }
    test$number_of_completed_rt_items[i] <- number_of_completed_rt_items
  }
  else( #if it progress was an NA, keep NA, because otherwise we'll have to divide by 0 in a bit
    test$number_of_completed_rt_items[i] <- NA
  )
} 

#calculate the response time per item
test$response_time_per_item <- test$Duration..in.seconds./test$number_of_completed_items 
test$response_time_per_rt_item <- test$Duration..in.seconds._rt/test$number_of_completed_rt_items 

#find the range
min(test$response_time_per_item)
max(test$response_time_per_item)

min(test$response_time_per_rt_item, na.rm = T)
max(test$response_time_per_rt_item, na.rm = T)

#display a histogram of response time per item
hist_resp_time_per_item <- ggplot(data = test[test$response_time_per_item < 25,], aes(x = response_time_per_item)) +
  geom_histogram(binwidth = 1, boundary = 0, color = "black", fill = "white") +
  labs(
    y = expression("Frequency"),
    x = expression(paste("Response time per item ", "(", italic("s"), ")"))) +
  theme(text = element_text(size=20, family = "serif"))
hist_resp_time_per_item

#do the same for the rt measures
hist_resp_time_per_rt_item <- ggplot(data = test[test$response_time_per_rt_item < 25,], aes(x = response_time_per_rt_item)) +
  geom_histogram(binwidth = 1, boundary = 0, color = "black", fill = "white") +
  labs(
    y = expression("Frequency"),
    x = expression(paste("Response time per item ", "(", italic("s"), ")"))) +
  theme(text = element_text(size=20, family = "serif"))
hist_resp_time_per_rt_item

ggsave("Figures/Data_screening/hist_resp_time_per_item.jpg", hist_resp_time_per_item, width = 12, height = 8)
ggsave("Figures/Data_screening/hist_resp_time_per_rt_item.jpg", hist_resp_time_per_rt_item, width = 12, height = 8)

## STEP 2A - Find univariate outliers 
#create an empty column for careless responders
test$careless_responders <- rep(0, nrow(test)) 
test$careless_responders_rt <- rep(0, nrow(test)) 

#define a function that can find univariate outliers
univariate_outlier_calculator <- function(x, column){
  mdn = median(column, na.rm = T)
  mad = mad(column, na.rm = T)
  if(abs(x - mdn)/(1.438*mad) > 2.24){
    return(1)
  }
  else{
    return(0)
  }
}

#apply this function to all observations and save this in the empty column
for(i in 1:nrow(test)){
  outlier <- univariate_outlier_calculator(test$response_time_per_item[i], test$response_time_per_item)
  test$careless_responders[i] <- outlier
  
  if(!is.na(test$response_time_per_rt_item[i])){
    outlier_rt <- univariate_outlier_calculator(test$response_time_per_rt_item[i], test$response_time_per_rt_item)
  }
  else(
    outlier_rt <- NA
  )
  
  test$careless_responders_rt[i] <- outlier_rt
}

#inspect the data: are these high or low observations?
min(test$response_time_per_item[test$careless_responders == 1]) #this is seconds/item
min(test$Duration..in.seconds.[test$careless_responders == 1])/60 #this is total minutes
max(test$response_time_per_item[test$careless_responders == 1])

min(test$response_time_per_rt_item[test$careless_responders_rt == 1], na.rm = T) #this is seconds/item
min(test$Duration..in.seconds._rt[test$careless_responders_rt == 1], na.rm = T)/60 #this is total minutes
max(test$response_time_per_rt_item[test$careless_responders_rt == 1], na.rm = T)

## STEP 2B - Discard observations with a response time per item smaller than 2s

#find how many observations this applies to
length(test$response_time_per_item[test$response_time_per_item < 2])

length(test$response_time_per_rt_item[!is.na(test$response_time_per_rt_item) & test$response_time_per_rt_item < 2])

