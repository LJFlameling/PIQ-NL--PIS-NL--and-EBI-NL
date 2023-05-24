### In this file we will do all the data preparation

rm(list = ls())
options(datatable.max.print = 1000)

test <- read.csv("Data/test_anonymized.csv")

### STEP 1 - Remove observations -----

### STEP 1A - Bots
# see if there are any observations with recaptcha score < 0.5
unique(test$Q_RecaptchaScore)

#remove them
test <- test[!test$Q_RecaptchaScore < 0.5, ]

### STEP 1B - Noncompleters
#Remove everyone who is not a real observation
test <- test[test$Progress > 70, ]

### STEP 1C - Age
#remove underaged participants
test <- test[!as.numeric(test$Leeftijd) < 18, ]

### STEP 1D - NAs
test <- test[!is.na(test$EndDate), ] #these are the NAs of participants who did not have a recaptcha score

### STEP 2 - Turn into numerics -----

#firstly we need to turn all questionnaire columns into numerics. We do that using the colnames so the colnumbers could change.

#define a function to generate a list of colnames based on the colname and the number of items. The colname is something like "PIV_28"
generate_colnames <- function(base, n) {
  result <- c()
  for (i in 1:n) {
    result <- c(result, paste(base, i, sep="_"))
  }
  return(result)
}

#apply the function to generate a list of colnames
piv_colnames <- generate_colnames("PIV", 28)
ebi_colnames <- generate_colnames("EBI", 8)
mev_colnames <- generate_colnames("MEV", 30)
swls_before_colnames <- generate_colnames("SWLS...before", 5)
swls_after_colnames <- generate_colnames("SWLS...after", 5)
pis_colnames <- c(generate_colnames("PIS.6", 6), "PIS.7._1")

#save the colnames of all questionnaires
item_colnames <- c(piv_colnames, ebi_colnames, mev_colnames, swls_before_colnames, swls_after_colnames, pis_colnames)

#save some other colnames
demographic_numeric_colnames <- c("Progress", "Leeftijd")
demographic_numeric_realnames <- c("Progress", "Leeftijd")
demographic_numeric_names <- data.frame(demographic_numeric_colnames, demographic_numeric_realnames)
likert_colnames <- c(piv_colnames, swls_before_colnames, swls_after_colnames)

#define a function to generate a list of realnames based on the realname and the number of items. The realname is something like "PIQ-NL Question 1". 
generate_realnames <- function(base, n) {
  result <- c()
  for (i in 1:n) {
    result <- c(result, paste(base, "Question", i, sep=" "))
  }
  return(result)
}

#apply the function to generate a list of colnames
piv_realnames <- generate_realnames("PIQ-NL", 28)
ebi_realnames <- generate_realnames("EBI-NL", 8)
mev_realnames <- generate_realnames("Dutch MEQ30", 30)
swls_before_realnames <- generate_realnames("SWLS Before the Psychedelic Experience", 5)
swls_after_realnames <- generate_realnames("SWLS After the Psychedelic Experience", 5)
pis_realnames <- c(generate_realnames("PIS-NL", 7))

#save the realnames of all questionnaires
item_realnames <- c(piv_realnames, ebi_realnames, mev_realnames, swls_before_realnames, swls_after_realnames, pis_realnames)


#let's define a function to create retest names
generate_retest_names <- function(names_list, name_type){
  result <- c()
  for(name in names_list[1:36]){
    if(name_type == "colname"){
    retest_name <- paste(name, "rt", sep = "_")
    }
    if(name_type == "realname"){
    retest_name <- paste("Retest", name, sep = " ")
    }
    result <- c(result, retest_name)
  }
  return(result)
}

retest_item_colnames <- generate_retest_names(item_colnames, "colname")
retest_item_realnames <- generate_retest_names(item_realnames, "realname")

#split it up by questionnaire
retest_piv_colnames <- retest_item_colnames[grep("^PIV", retest_item_colnames)]
retest_ebi_colnames <- retest_item_colnames[grep("^EBI", retest_item_colnames)]

#let's also save all the actual questions
piq28_items <- c(
  "Awareness of uncomfortable or painful feelings I previously avoided",
  "Discovered I could explore uncomfortable or painful feelings I previously avoided",
  "Awareness of dysfunctional patterns in my actions, thoughts, and/or feelings",
  "Realized the nature and/or origins of my defenses or other coping strategies",
  "Realized how current feelings or perceptions are related to events from my past",
  "Discovered a clear pattern of avoidance in my life",
  "Gained resolution or clarity about past traumas or hurtful events",
  "Gained a deeper understanding of events/memories from my past",
  "Realized I could experience memories previously too difficult to experience",
  "Discovered how aspects of my life are affecting my well-being",
  "Realized ways my beliefs may be dysfunctional",
  "Discovered clear similarities between my past and present interpersonal relationships",
  "Realized how critical or judgmental views I hold towards myself are dysfunctional",
  "Realized certain actions I should take in regards to important relationships in my life",
  "Discovered new feelings or perspectives about significant relationships in my life",
  "Realized the point of view or actions of others that had been difficult to understand previously",
  "Awareness of my life purpose, goals, and/or priorities",
  "Awareness of beneficial patterns in my actions, thoughts, and/or feelings",
  "Experienced validation of my life, character, values, or beliefs",
  "Realized the importance of my life",
  "Discovered clarity or creative solutions about how to solve a problem in my life",
  "Awareness of information that helped me understand my life",
  "Discovered new actions that may help me achieve my goals",
  "Discovered ways to see my problems with more clarity",
  "Discovered new insights about my work or career",
  "Discovered specific techniques for coping with difficulties",
  "Gained a deeper understanding of previously held beliefs and/or values",
  "Discovered a vivid sense of the paradoxes in life")

piq23_items <- piq28_items[c(1:13, 16, 17:20, 22, 23, 25, 27, 28)]

pis7_items <- c(
  "I have had important new insights about how past events have influenced my current mental health and behaviour",
  "I have learned important new ways of thinking about my ‘self’ and my problems",
  "I have had important new insights about how I would like to change aspects of myself or my lifestyle",
  "I have become more conscious of aspects of my past that I used to ignore or not be fully aware of",
  "I have become more conscious of aspects of my ‘self’ that I used to ignore or not be fully aware of",
  "I have become more conscious of aspects of my lifestyle that I used to ignore or not be fully aware of",
  "I have made positive changes to my lifestyle and/or behaviour in accordance with the insights I have gained as part of my treatment/experience’"
)

pis6_items <- pis7_items[-7]

ebi8_items <- c(
  "I faced emotionally difficult feelings that I usually push aside",
  "I experienced a resolution of a personal conflict/trauma",
  "I felt able to explore challenging emotions and memories",
  "I was resisting and avoiding challenging feelings throughout, without breakthrough",
  "I had an emotional breakthrough",
  "I was able to get a sense of closure on an emotional problem",
  "I felt emotionally stuck throughout, without breakthrough",
  "I achieved an emotional release followed by a sense of relief"
)

ebi6_items <- ebi8_items[-c(4, 7)]

meq30_items <- c(
  "Loss of your usual sense of time",
  "Experience of amazement",
  "Sense that the experience cannot be described adequately in words",
  "Gain of insightful knowledge experienced at an intuitive level",
  "Feeling that you experienced eternity or infinity",
  "Experience of oneness or unity with objects and/or persons perceived in your surroundings",
  "Loss of your usual sense of space",
  "Feelings of tenderness and gentleness",
  "Certainty of encounter with ultimate reality (in the sense of being able to “know” and “see” what is really real at some point during your experience",
  "Feeling that you could not do justice to your experience by describing it in words",
  "Loss of usual awareness of where you were",
  "Feelings of peace and tranquility",
  "Sense of being “outside of” time, beyond past and future",
  "Freedom from the limitations of your personal self and feeling a unity or bond with what was felt to be greater than your personal self",
  "Sense of being at a spiritual height",
  "Experience of pure being and pure awareness (beyond the world of sense impressions)",
  "Experience of ecstasy",
  "Experience of the insight that “all is One”",
  "Being in a realm with no space boundaries",
  "Experience of oneness in relation to an “inner world” within",
  "Sense of reverence",
  "Experience of timelessness",
  "You are convinced now, as you look back on your experience, that in it you encountered ultimate reality (i.e., that you “knew” and “saw” what was really real)",
  "Feeling that you experienced something profoundly sacred and holy",
  "Awareness of the life or living presence in all things",
  "Experience of the fusion of your personal self into a larger whole",
  "Sense of awe or awesomeness",
  "Experience of unity with ultimate reality",
  "Feeling that it would be difficult to communicate your own experience to others who have not had similar experiences",
  "Feelings of joy"
)

swls_before_items <- c(
  "In most ways my life is close to my ideal",
  "The conditions of my life are excellent",
  "I am satisfied with my life",
  "So far I have gotten the important things I want in life",
  "If I could live my life over, I would change almost nothing."
)

swls_after_items <- swls_before_items

full_items <- c(piq28_items, ebi8_items, meq30_items, swls_before_items, swls_after_items, pis7_items)

#let's save this in a dataframe
item_names <- data.frame(item_colnames, item_realnames, full_items)
retest_item_names <- data.frame(retest_item_colnames, retest_item_realnames)

#turn all columns with these colnames into numerics
test[, c(item_colnames, retest_item_colnames)] <- lapply(test[, c(item_colnames, retest_item_colnames)], function(x) as.numeric(gsub("([0-9]+).*$", "\\1", x)))

#let's also do this for the demographic variables
test[, demographic_numeric_colnames] <- lapply(test[, demographic_numeric_colnames], as.numeric)

### STEP 4 - Calculate total scores per questionnaire ----- 
#now we can start creating total scores. First we exclude the items that are excluded for the piv, ebi and pis
piv_included <- setdiff(piv_colnames, c("PIV_14", "PIV_15", "PIV_21", "PIV_24", "PIV_26"))
ebi_included <- setdiff(ebi_colnames, c("EBI_4", "EBI_7"))
pis_included <- setdiff(pis_colnames, "PIS.7._1")

only_included_item_colnames <- c(piv_included, ebi_included, mev_colnames, swls_before_colnames, swls_after_colnames, pis_included)

#do the same for retest
only_included_retest_item_colnames <- setdiff(retest_item_colnames, 
                                              c("PIV_14_rt", "PIV_15_rt", "PIV_21_rt", "PIV_24_rt", "PIV_26_rt", "EBI_4_rt", "EBI_7_rt", "PIS.7._1_rt"))

#split it up by questionnaire
retest_piv_included <- only_included_retest_item_colnames[grep("^PIV", only_included_retest_item_colnames)]
retest_ebi_included <- only_included_retest_item_colnames[grep("^EBI", only_included_retest_item_colnames)]

#PIQ23R
piq23r_colnames <- c("PIV_3", "PIV_4", "PIV_5", "PIV_6", "PIV_7", "PIV_8", "PIV_9", "PIV_11", "PIV_12", "PIV_13", "PIV_14", "PIV_15", "PIV_16", "PIV_17", "PIV_19", "PIV_20", "PIV_21", "PIV_23", "PIV_24", "PIV_25", "PIV_26", "PIV_27", "PIV_28")

piq23r_rt_colnames <- c()
for (string in piq23r_colnames) {
  new_string <- paste0(string, "_rt")
  piq23r_rt_colnames <- c(piq23r_rt_colnames, new_string)
}

#then we calculate the total scores for all questionnaires
test$piv_total_23 <- rowSums(test[piv_included])
test$piv_total_28 <- rowSums(test[piv_colnames])
test$ebi_total_6 <- rowSums(test[ebi_included])
test$ebi_total_8 <- rowSums(test[ebi_colnames])
test$retest_piv_total_23 <- rowSums(test[retest_piv_included])
test$retest_piv_total_28 <- rowSums(test[retest_piv_colnames])
test$retest_ebi_total_6 <- rowSums(test[retest_ebi_included])
test$retest_ebi_total_8 <- rowSums(test[retest_ebi_colnames])
test$pis_total_6 <- rowSums(test[pis_included])
test$pis_total_7 <- rowSums(test[pis_colnames])
test$mev_total <- rowSums(test[mev_colnames])
test$swls_before_total <- rowSums(test[swls_before_colnames])
test$swls_after_total <- rowSums(test[swls_after_colnames])
test$piq23r_total <-rowSums(test[piq23r_colnames])
test$piq23r_rt_total <-rowSums(test[piq23r_rt_colnames])

#calculate component scores for each of the factor structures

#PIQ23
test$piq23_2f_EN_amp_total <- rowSums(test[item_names$item_colnames[c(1:13, 16)]])
test$piq23_2f_EN_gap_total <- rowSums(test[item_names$item_colnames[c(17:20, 22, 23, 25, 27, 28)]])
test$piq23_2f_NL_amp_total <- rowSums(test[item_names$item_colnames[c(1:13)]])
test$piq23_2f_NL_gap_total <- rowSums(test[item_names$item_colnames[c(16, 17:20, 22, 23, 25, 27, 28)]])

#2F
amp_2f <- item_names$item_colnames[1:15]
gap_2f <- item_names$item_colnames[16:28]

test$piq28_2f_amp <- rowSums(test[amp_2f])
test$piq28_2f_gap <- rowSums(test[gap_2f])

#3F
amp_3f <- amp_2f
lv_3f <- item_names$item_colnames[c(17:20, 27, 28)]
caav_3f <- item_names$item_colnames[c(21:26)]

test$piq28_3f_amp <- rowSums(test[amp_3f])
test$piq28_3f_lv <- rowSums(test[lv_3f])
test$piq28_3f_caav <- rowSums(test[caav_3f])

#4F
amp_4f <- item_names$item_colnames[1:9]
r_4f <- item_names$item_colnames[10:16]
lv_4f <- lv_3f
caav_4f <- caav_3f

test$piq28_4f_amp <- rowSums(test[amp_4f])
test$piq28_4f_r <- rowSums(test[r_4f])
test$piq28_4f_lv <- rowSums(test[lv_4f])
test$piq28_4f_caav <- rowSums(test[caav_4f])

#5F_28
amp_5f <- item_names$item_colnames[c(1:4, 6, 11, 13)]
pet_5f <- item_names$item_colnames[c(5, 7:9)]
r_5f <- item_names$item_colnames[c(10, 12, 14:16)]
lv_5f <- lv_4f
caav_5f <- caav_4f

test$piq28_5f_amp <- rowSums(test[amp_5f])
test$piq28_5f_pet <- rowSums(test[pet_5f])
test$piq28_5f_r <- rowSums(test[r_5f])
test$piq28_5f_lv <- rowSums(test[lv_5f])
test$piq28_5f_caav <- rowSums(test[caav_5f])

#5F_23r
amp_5f_23r <- item_names$item_colnames[c(3, 4, 6, 11, 13)]
pet_5f_23r <- pet_5f
r_5f_23r <- item_names$item_colnames[c(12, 14:16)]
lv_5f_23r <- item_names$item_colnames[c(17, 19, 20, 27, 28)]
caav_5f_23r <- item_names$item_colnames[c(21, 23:26)]

test$piq23r_5f_amp <- rowSums(test[amp_5f_23r])
test$piq23r_5f_pet <- rowSums(test[pet_5f_23r])
test$piq23r_5f_r <- rowSums(test[r_5f_23r])
test$piq23r_5f_lv <- rowSums(test[lv_5f_23r])
test$piq23r_5f_caav <- rowSums(test[caav_5f_23r])

#let's save these colnames of these subscale scores
subscale_score_colnames <- c("piq23_2f_EN_amp_total", 
                             "piq23_2f_EN_gap_total", 
                             "piq23_2f_NL_amp_total", 
                             "piq23_2f_NL_gap_total",
                             "piq28_2f_amp",
                             "piq28_2f_gap",
                             "piq28_3f_amp",
                             "piq28_3f_lv",
                             "piq28_3f_caav",
                             "piq28_4f_amp",
                             "piq28_4f_r",
                             "piq28_4f_lv",
                             "piq28_4f_caav",
                             "piq28_5f_amp",
                             "piq28_5f_pet",
                             "piq28_5f_r",
                             "piq28_5f_lv",
                             "piq28_5f_caav",
                             "piq23r_5f_amp",
                             "piq23r_5f_pet",
                             "piq23r_5f_r",
                             "piq23r_5f_lv",
                             "piq23r_5f_caav")

subscale_score_colnames <- data.frame(subscale_score_colnames)

#let's also save the colnames of these sumscores
sumscore_colnames <- c("piv_total_23", "piv_total_28", "ebi_total_6", "ebi_total_8", "retest_piv_total_23", "retest_piv_total_28", "retest_ebi_total_6", "retest_ebi_total_8", "pis_total_6", "pis_total_7", "mev_total", "swls_before_total", "swls_after_total")
sumscore_realnames <- c("PIQ23-NL", "PIQ28-NL", "EBI6-NL", "EBI8-NL", "Retest PIQ23-NL", "Retest PIQ28-NL", "Retest EBI6-NL", "Retest EBI8-NL", "PIS6-NL ", "PIS7-NL ", "Dutch MEQ30 ", "SWLS Before the Psychedelic Experience ", "SWLS After the Psychedelic Experience ")
sumscore_names <- data.frame(sumscore_colnames, sumscore_realnames)

numeric_colnames <- c(item_names$item_colnames, retest_item_names$retest_item_colnames, sumscore_names$sumscore_colnames, demographic_numeric_colnames)
numeric_realnames <- c(item_names$item_realnames, retest_item_names$retest_item_realnames, sumscore_names$sumscore_realnames, demographic_numeric_realnames)
numeric_names <- data.frame(numeric_colnames, numeric_realnames)

#let's also turn the date into an actual date
test$EndDate <- as.Date(substr(test$EndDate, 1, 10)) # keep only the date, not the time and turn the string into a date
test$EndDate_rt <- as.Date(substr(test$EndDate_rt, 1, 10))
test$test_rt_time_difference <- test$EndDate_rt - test$EndDate

#let's also calculate the mean scores
test$piv_mean_23 <- rowMeans(test[piv_included])
test$piv_mean_28 <- rowMeans(test[piv_colnames])
test$ebi_mean_6 <- rowMeans(test[ebi_included])
test$ebi_mean_8 <- rowMeans(test[ebi_colnames])
test$retest_piv_mean_23 <- rowMeans(test[retest_piv_included])
test$retest_piv_mean_28 <- rowMeans(test[retest_piv_colnames])
test$retest_ebi_mean_6 <- rowMeans(test[retest_ebi_included])
test$retest_ebi_mean_8 <- rowMeans(test[retest_ebi_colnames])
test$pis_mean_6 <- rowMeans(test[pis_included])
test$pis_mean_7 <- rowMeans(test[pis_colnames])
test$mev_mean <- rowMeans(test[mev_colnames])
test$swls_before_mean <- rowMeans(test[swls_before_colnames])
test$swls_after_mean <- rowMeans(test[swls_after_colnames])
test$piq23r_mean <-rowMeans(test[piq23r_colnames])
test$piq23r_rt_mean <-rowMeans(test[piq23r_rt_colnames])

#let's also calculate the swls difference score
test$swls_difference <- test$swls_after_total - test$swls_before_total

### STEP 5 - Make a column that records whether people completed the retest or not

#create an empty column with 0s of the length of the other columns
test$rt_completed <- rep(0, length(test$X))

for(i in 1:length(test$X)){
  if(!is.na(test$Progress_rt[i])){
    if(test$Progress_rt[i] > 49){
      test$rt_completed[i] <- 1
    }
  }
}

### STEP 5 - Create English shortnames -----

## create the English shortnames, such that "PIV_1" becomes "PIQ 1"

#vector for piq23r_colnames
piq23r_colnames <- piv_colnames[c(3:9, 11:17, 19:21, 23:28)]

#create function
english_shortname_maker <- function(n_items, colname_list){
  list <- c()
  if(grepl("piv", deparse(substitute(colname_list))) | grepl("piq23r", deparse(substitute(colname_list)))){
    english_acronym <- "PIQ"
  }
  if(grepl("pis", deparse(substitute(colname_list)))){
    english_acronym <- "PIS"
  }
  if(grepl("ebi", deparse(substitute(colname_list)))){
    english_acronym <- "EBI"
  }
  if(grepl("mev", deparse(substitute(colname_list)))){
    english_acronym <- "MEQ"
  }
  if(grepl("swls_before", deparse(substitute(colname_list)))){
    english_acronym <- "SWLS before"
  }
  if(grepl("swls_after", deparse(substitute(colname_list)))){
    english_acronym <- "SWLS after"
  }
  
  for(i in 1:n_items){
    current_colname <- colname_list[i]
    item_number <- gsub("[^[:digit:]]", "", current_colname ) 
    new_colname <- paste(english_acronym, " ", item_number, sep = "")
    list <- c(list, new_colname)
  }
  return(list)
  
}

#apply function
meq_english_shortnames <- english_shortname_maker(n_items = 30, mev_colnames)
ebi6_english_shortnames <- english_shortname_maker(n_items = 6, ebi_included)
ebi8_english_shortnames <- english_shortname_maker(n_items = 8, ebi_colnames)
pis6_english_shortnames <- c("PIS 1", "PIS 2", "PIS 3", "PIS 4", "PIS 5", "PIS 6")
pis7_english_shortnames <- c(pis6_english_shortnames, "PIS 7")
piq23r_english_shortnames <- english_shortname_maker(n_items = 23, piq23r_colnames)
piq28_english_shortnames <- english_shortname_maker(n_items = 28, piv_colnames)
piq23_english_shortnames <- english_shortname_maker(n_items = 23, piv_included)
swls_before_english_shortnames <- english_shortname_maker(n_items = 5, swls_before_colnames)
swls_after_english_shortnames <- english_shortname_maker(n_items = 5, swls_after_colnames)

#save
item_names$english_shortname <- c(piq28_english_shortnames, ebi8_english_shortnames, meq_english_shortnames, swls_before_english_shortnames, swls_after_english_shortnames, pis7_english_shortnames)
  
### STEP 6 - Making subsets with only the relevant questions -----

#select the colnames
piq23_colnames <- only_included_item_colnames[1:23]
pis6_colnames <- only_included_item_colnames[70:75]
ebi6_colnames <- only_included_item_colnames[24:29]
meq30_colnames <- only_included_item_colnames[30:59]

#make datasets with only the data per questionnaire
test_piq23 <- test[,piq23_colnames]
test_piq28 <- test[,piv_colnames]

test_pis6 <- test[,pis6_colnames]
test_pis6 <- subset(test_pis6, !is.na(test_pis6$PIS.6_1))
test_pis7 <- test[,pis_colnames]
test_pis7 <- subset(test_pis7, !is.na(test_pis7$PIS.6_1))

test_ebi6 <- test[,ebi6_colnames]
test_ebi6 <- subset(test_ebi6, !is.na(test_ebi6$EBI_1))
test_ebi8 <- test[,ebi_colnames]
test_ebi8 <- subset(test_ebi8, !is.na(test_ebi8$EBI_1))

test_meq30 <- test[,meq30_colnames]
test_meq30 <- subset(test_meq30, !is.na(test_meq30$MEV_1))

### STEP 7 - Saving the progress some nice new csvs -----

#let's save it in the right place

write.csv(as.data.frame(item_names), "Data/Transformed_CSVs/all_item_names.csv") #save the item names
write.csv(as.data.frame(demographic_numeric_colnames), "Data/Transformed_CSVs/demographic_numeric_colnames.csv") #save the demographic colnames
write.csv(as.data.frame(only_included_item_colnames), "Data/Transformed_CSVs/only_included_item_colnames.csv") #save only the included colnames
write.csv(as.data.frame(sumscore_names), "Data/Transformed_CSVs/sumscore_names.csv") #save the sumscore colnames
write.csv(subscale_score_colnames, "Data/Transformed_CSVs/subscale_score_colnames.csv")
write.csv(as.data.frame(likert_colnames), "Data/Transformed_CSVs/likert_colnames.csv")
write.csv(numeric_names, "Data/Transformed_CSVs/numeric_names.csv")
write.csv(test_meq30, "Data/Transformed_CSVs/test_meq.csv")
write.csv(test_piq23, "Data/Transformed_CSVs/test_piq23.csv")
write.csv(test_piq28, "Data/Transformed_CSVs/test_piq28.csv")
write.csv(test_pis6, "Data/Transformed_CSVs/test_pis6.csv")
write.csv(test_pis7, "Data/Transformed_CSVs/test_pis7.csv")
write.csv(test_ebi6, "Data/Transformed_CSVs/test_ebi6.csv")
write.csv(test_ebi8, "Data/Transformed_CSVs/test_ebi8.csv")
write.csv(test, "Data/Transformed_CSVs/test_anonymized_and_prepared.csv") #save the big boy