rstudioapi::writeRStudioPreference("data_viewer_max_columns", 1000L)
rm(list = ls())

#import data
test <- read.csv("Data/Transformed_CSVs/test_anonymized_and_prepared.csv")

if(!require(pacman)){
  install.packages("pacman")
}
library(pacman)

pacman::p_load(ggpubr,
               irr,
               tidyr,
               ggplot2,
               dplyr)

##### CHAPTER 1 - PIQ ---------

### STEP 1 - CALCULATING ICC -----
## PIQ

# Data preparation
test_sorted <- test[order(abs(test$test_rt_time_difference)), ] #order values ascendingly
test_sorted$test_rt_time_difference <- abs(test_sorted$test_rt_time_difference) # make negative values positive
test_sorted <- test_sorted[test_sorted$rt_completed == 1, ] #only include ppl who completed the PIQ_rt

#PIQ23
icc(test_sorted[,c(152, 156)], "twoway", "agreement", "single")

#PIQ28
icc(test_sorted[,c(153, 157)], "twoway", "agreement", "single")


### STEP 2 - SENSITIITY TO TIME -----

# Extract the three groups
group1 <- head(test_sorted, 50)
group2 <- slice(test_sorted, 51:100)
group3 <- slice(test_sorted, 101:151)

summary(group1$test_rt_time_difference)
sd(group1$test_rt_time_difference)
summary(group2$test_rt_time_difference)
sd(group2$test_rt_time_difference)
summary(group3$test_rt_time_difference)
sd(group3$test_rt_time_difference)

#calculating icc values and CI ranges
#PIQ23
piq23_group1_icc <- round(icc(group1[,c(152, 156)])$value, 2)
piq23_group1_lbound <- round(icc(group1[,c(152, 156)])$lbound, 2)
piq23_group1_ubound <- round(icc(group1[,c(152, 156)])$ubound, 2)
piq23_group1_ci <- paste("[", piq23_group1_lbound, ", ", piq23_group1_ubound, "]", sep = "")

piq23_group2_icc <- round(icc(group2[,c(152, 156)])$value, 2)
piq23_group2_lbound <- round(icc(group2[,c(152, 156)])$lbound, 2)
piq23_group2_ubound <- round(icc(group2[,c(152, 156)])$ubound, 2)
piq23_group2_ci <- paste("[", piq23_group2_lbound, ", ", piq23_group2_ubound, "]", sep = "")

piq23_group3_icc <- round(icc(group3[,c(152, 156)])$value, 2)
piq23_group3_lbound <- round(icc(group3[,c(152, 156)])$lbound, 2)
piq23_group3_ubound <- round(icc(group3[,c(152, 156)])$ubound, 2)
piq23_group3_ci <- paste("[", piq23_group3_lbound, ", ", piq23_group3_ubound, "]", sep = "")
                        
#PIQ28
piq28_group1_icc <- round(icc(group1[,c(153, 157)])$value, 2)
piq28_group1_lbound <- round(icc(group1[,c(153, 157)])$lbound, 2)
piq28_group1_ubound <- round(icc(group1[,c(153, 157)])$ubound, 2)
piq28_group1_ci <- paste("[", piq28_group1_lbound, ", ", piq28_group1_ubound, "]", sep = "")

piq28_group2_icc <- round(icc(group2[,c(153, 157)])$value, 2)
piq28_group2_lbound <- round(icc(group2[,c(153, 157)])$lbound, 2)
piq28_group2_ubound <- round(icc(group2[,c(153, 157)])$ubound, 2)
piq28_group2_ci <- paste("[", piq28_group2_lbound, ", ", piq28_group2_ubound, "]", sep = "")

piq28_group3_icc <- round(icc(group3[,c(153, 157)])$value, 2)
piq28_group3_lbound <- round(icc(group3[,c(153, 157)])$lbound, 2)
piq28_group3_ubound <- round(icc(group3[,c(153, 157)])$ubound, 2)
piq28_group3_ci <- paste("[", piq28_group3_lbound, ", ", piq28_group3_ubound, "]", sep = "")

group1_column <- c(piq23_group1_icc, piq23_group1_ci, piq28_group1_icc, piq28_group1_ci)
group2_column <- c(piq23_group2_icc, piq23_group2_ci, piq28_group2_icc, piq28_group2_ci)
group3_column <- c(piq23_group3_icc, piq23_group3_ci, piq28_group3_icc, piq28_group3_ci)

#turn into a word table
icc_table <- data.frame(group1_column, group2_column, group3_column)
colnames(icc_table) <- c("Group 1", "Group 2", "Group 3")
rownames(icc_table) <- c("PIQ23-NL", " ", "PIQ28-NL", "  ")

#turn into a bar chart
group1_barchart_icc <- c(piq23_group1_icc, piq28_group1_icc)
group1_barchart_lbound <- c(piq23_group1_lbound, piq28_group1_lbound)
group1_barchart_ubound <- c(piq23_group1_ubound, piq28_group1_ubound)
group1_margin_of_error <- group1_barchart_ubound - group1_barchart_lbound

group2_barchart_icc <- c(piq23_group2_icc, piq28_group2_icc)
group2_barchart_lbound <- c(piq23_group2_lbound, piq28_group2_lbound)
group2_barchart_ubound <- c(piq23_group2_ubound, piq28_group2_ubound)
group2_margin_of_error <- group2_barchart_ubound - group2_barchart_lbound

group3_barchart_icc <- c(piq23_group3_icc, piq28_group3_icc)
group3_barchart_lbound <- c(piq23_group3_lbound, piq28_group3_lbound)
group3_barchart_ubound <- c(piq23_group3_ubound, piq28_group3_ubound)
group3_margin_of_error <- group3_barchart_ubound - group3_barchart_lbound

icc_barchart_table <- data.frame(group1_barchart_icc, group1_barchart_lbound, group1_barchart_ubound, group1_margin_of_error, 
                                 group2_barchart_icc, group2_barchart_lbound, group2_barchart_ubound, group2_margin_of_error, 
                                 group3_barchart_icc, group3_barchart_lbound, group3_barchart_ubound, group3_margin_of_error)

rownames(icc_barchart_table) <- c("PIQ23-NL", "PIQ28-NL")
colnames(icc_barchart_table) <- sub("barchart_", "", colnames(icc_barchart_table))

#calculate the average margin of error
icc_barchart_table$avg_margin_of_error_gr1_gr2 <- rowMeans(icc_barchart_table[,c("group1_margin_of_error", "group2_margin_of_error")])
icc_barchart_table$avg_margin_of_error_gr2_gr3 <- rowMeans(icc_barchart_table[,c("group2_margin_of_error", "group3_margin_of_error")])
icc_barchart_table$avg_margin_of_error_gr1_gr3 <- rowMeans(icc_barchart_table[,c("group1_margin_of_error", "group3_margin_of_error")])

#calculate the  overlap
icc_barchart_table$overlap_gr1_gr2 <- icc_barchart_table$group2_ubound - icc_barchart_table$group1_lbound
icc_barchart_table$overlap_gr2_gr3 <- icc_barchart_table$group3_ubound - icc_barchart_table$group2_lbound
icc_barchart_table$overlap_gr1_gr3 <- icc_barchart_table$group3_ubound - icc_barchart_table$group1_lbound

#calculate the proportion overlap over the avg margin of error
icc_barchart_table$overlap_over_margin_gr1_gr2 <- icc_barchart_table$overlap_gr1_gr2/icc_barchart_table$avg_margin_of_error_gr1_gr2
icc_barchart_table$overlap_over_margin_gr2_gr3 <- icc_barchart_table$overlap_gr2_gr3/icc_barchart_table$avg_margin_of_error_gr2_gr3
icc_barchart_table$overlap_over_margin_gr1_gr3 <- icc_barchart_table$overlap_gr1_gr3/icc_barchart_table$avg_margin_of_error_gr1_gr3

#create a separate column with the rownames
icc_barchart_table$PIQ <- rownames(icc_barchart_table)

#turn the df into a wide form
icc_barchart_table_long <- icc_barchart_table %>%
  pivot_longer(cols = c("group1_icc", "group2_icc", "group3_icc"),
               names_to = "group", values_to = "icc") %>%
  mutate(lower_bound = ifelse(group == "group1_icc", group1_lbound,
                              ifelse(group == "group2_icc", group2_lbound, group3_lbound)),
         upper_bound = ifelse(group == "group1_icc", group1_ubound,
                              ifelse(group == "group2_icc", group2_ubound, group3_ubound)))

# Create the bar chart with error bars
barchart_icc <- ggplot(icc_barchart_table_long, aes(x = PIQ, y = icc, fill = group)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_errorbar(aes(ymin = lower_bound, ymax = upper_bound),
                position = position_dodge(width = 0.9), width = 0.2) +
  labs(x = "Questionnaire", y = "ICC value") +
  scale_fill_manual(values = c("#E69F00", "#56B4E9", "#009E73"),
                    labels = c("Group 1", "Group 2", "Group 3")) +
  theme(text = element_text(size = 20, family = "serif")) + 
  guides(fill=guide_legend(title="")) +
  geom_signif(y_position = c(1.0, 1.0, 1.1),
              xmin = c(0.70, 1.68, 1.68),
              xmax = c(1.3, 2.3, 2.0),
              annotation = c("*", "*", "*"),
              tip_length = 0, textsize = 6,
              ) +
  scale_y_continuous(limits = c(0,1.12), breaks = c(0.20, 0.40, 0.60, 0.80, 1.0), labels = c(".20", ".40", ".60", ".80", "1.00"))
barchart_icc

ggsave(filename = "Figures/Test-retest reliability/barchart_icc.png", plot = barchart_icc, width = 12, height = 8)

### STEP 3 - REPEAT EXCLUDING THE <14 INTERVALS -----
test_sorted <- test_sorted[test_sorted$test_rt_time_difference > 13, ] #only include ppl whose tests and retest were at least two weeks apart

#PIQ23
icc(test_sorted[,c(152, 156)], "twoway", "agreement", "single")

#PIQ28
icc(test_sorted[,c(153, 157)], "twoway", "agreement", "single")


# Extract the three groups
group1_sens <- head(test_sorted, 46)
group2 <- slice(test_sorted, 47:93)
group3 <- slice(test_sorted, 94:140)

summary(group1_sens$test_rt_time_difference)
sd(group1_sens$test_rt_time_difference)
summary(group2$test_rt_time_difference)
sd(group2$test_rt_time_difference)
summary(group3$test_rt_time_difference)
sd(group3$test_rt_time_difference)

#calculating icc values and CI ranges
#PIQ23
piq23_group1_sens_icc_sens <- round(icc(group1_sens[,c(152, 156)])$value, 2)
piq23_group1_sens_lbound <- round(icc(group1_sens[,c(152, 156)])$lbound, 2)
piq23_group1_sens_ubound <- round(icc(group1_sens[,c(152, 156)])$ubound, 2)
piq23_group1_sens_ci <- paste("[", piq23_group1_sens_lbound, ", ", piq23_group1_sens_ubound, "]", sep = "")

piq23_group2_icc_sens <- round(icc(group2[,c(152, 156)])$value, 2)
piq23_group2_lbound <- round(icc(group2[,c(152, 156)])$lbound, 2)
piq23_group2_ubound <- round(icc(group2[,c(152, 156)])$ubound, 2)
piq23_group2_ci <- paste("[", piq23_group2_lbound, ", ", piq23_group2_ubound, "]", sep = "")

piq23_group3_icc_sens <- round(icc(group3[,c(152, 156)])$value, 2)
piq23_group3_lbound <- round(icc(group3[,c(152, 156)])$lbound, 2)
piq23_group3_ubound <- round(icc(group3[,c(152, 156)])$ubound, 2)
piq23_group3_ci <- paste("[", piq23_group3_lbound, ", ", piq23_group3_ubound, "]", sep = "")

#PIQ28
piq28_group1_sens_icc_sens <- round(icc(group1_sens[,c(153, 157)])$value, 2)
piq28_group1_sens_lbound <- round(icc(group1_sens[,c(153, 157)])$lbound, 2)
piq28_group1_sens_ubound <- round(icc(group1_sens[,c(153, 157)])$ubound, 2)
piq28_group1_sens_ci <- paste("[", piq28_group1_sens_lbound, ", ", piq28_group1_sens_ubound, "]", sep = "")

piq28_group2_icc_sens <- round(icc(group2[,c(153, 157)])$value, 2)
piq28_group2_lbound <- round(icc(group2[,c(153, 157)])$lbound, 2)
piq28_group2_ubound <- round(icc(group2[,c(153, 157)])$ubound, 2)
piq28_group2_ci <- paste("[", piq28_group2_lbound, ", ", piq28_group2_ubound, "]", sep = "")

piq28_group3_icc_sens <- round(icc(group3[,c(153, 157)])$value, 2)
piq28_group3_lbound <- round(icc(group3[,c(153, 157)])$lbound, 2)
piq28_group3_ubound <- round(icc(group3[,c(153, 157)])$ubound, 2)
piq28_group3_ci <- paste("[", piq28_group3_lbound, ", ", piq28_group3_ubound, "]", sep = "")


group1_sens_column <- c(piq23_group1_sens_icc_sens, piq23_group1_sens_ci, piq28_group1_sens_icc_sens, piq28_group1_sens_ci)
group2_column <- c(piq23_group2_icc_sens, piq23_group2_ci, piq28_group2_icc_sens, piq28_group2_ci)
group3_column <- c(piq23_group3_icc_sens, piq23_group3_ci, piq28_group3_icc_sens, piq28_group3_ci)

#turn into a word table
icc_table <- data.frame(group1_sens_column, group2_column, group3_column)
colnames(icc_table) <- c("Group 1", "Group 2", "Group 3")
rownames(icc_table) <- c("PIQ23-NL", " ", "PIQ28-NL", "  ")

#turn into a bar chart
group1_sens_barchart_icc_sens <- c(piq23_group1_sens_icc_sens, piq28_group1_sens_icc_sens)
group1_sens_barchart_lbound <- c(piq23_group1_sens_lbound, piq28_group1_sens_lbound)
group1_sens_barchart_ubound <- c(piq23_group1_sens_ubound, piq28_group1_sens_ubound)
group1_sens_margin_of_error <- group1_sens_barchart_ubound - group1_sens_barchart_lbound

group2_barchart_icc_sens <- c(piq23_group2_icc_sens, piq28_group2_icc_sens)
group2_barchart_lbound <- c(piq23_group2_lbound, piq28_group2_lbound)
group2_barchart_ubound <- c(piq23_group2_ubound, piq28_group2_ubound)
group2_margin_of_error <- group2_barchart_ubound - group2_barchart_lbound

group3_barchart_icc_sens <- c(piq23_group3_icc_sens, piq28_group3_icc_sens)
group3_barchart_lbound <- c(piq23_group3_lbound, piq28_group3_lbound)
group3_barchart_ubound <- c(piq23_group3_ubound, piq28_group3_ubound)
group3_margin_of_error <- group3_barchart_ubound - group3_barchart_lbound

icc_barchart_table <- data.frame(group1_sens_barchart_icc_sens, group1_sens_barchart_lbound, group1_sens_barchart_ubound, group1_sens_margin_of_error, 
                                 group2_barchart_icc_sens, group2_barchart_lbound, group2_barchart_ubound, group2_margin_of_error, 
                                 group3_barchart_icc_sens, group3_barchart_lbound, group3_barchart_ubound, group3_margin_of_error)

rownames(icc_barchart_table) <- c("PIQ23-NL", "PIQ28-NL")
colnames(icc_barchart_table) <- sub("barchart_", "", colnames(icc_barchart_table))

#calculate the average margin of error
icc_barchart_table$avg_margin_of_error_gr1_gr2 <- rowMeans(icc_barchart_table[,c("group1_sens_margin_of_error", "group2_margin_of_error")])
icc_barchart_table$avg_margin_of_error_gr2_gr3 <- rowMeans(icc_barchart_table[,c("group2_margin_of_error", "group3_margin_of_error")])
icc_barchart_table$avg_margin_of_error_gr1_gr3 <- rowMeans(icc_barchart_table[,c("group1_sens_margin_of_error", "group3_margin_of_error")])

#calculate the  overlap
icc_barchart_table$overlap_gr1_gr2 <- icc_barchart_table$group2_ubound - icc_barchart_table$group1_sens_lbound
icc_barchart_table$overlap_gr2_gr3 <- icc_barchart_table$group3_ubound - icc_barchart_table$group2_lbound
icc_barchart_table$overlap_gr1_gr3 <- icc_barchart_table$group3_ubound - icc_barchart_table$group1_sens_lbound

#calculate the proportion overlap over the avg margin of error
icc_barchart_table$overlap_over_margin_gr1_gr2 <- icc_barchart_table$overlap_gr1_gr2/icc_barchart_table$avg_margin_of_error_gr1_gr2
icc_barchart_table$overlap_over_margin_gr2_gr3 <- icc_barchart_table$overlap_gr2_gr3/icc_barchart_table$avg_margin_of_error_gr2_gr3
icc_barchart_table$overlap_over_margin_gr1_gr3 <- icc_barchart_table$overlap_gr1_gr3/icc_barchart_table$avg_margin_of_error_gr1_gr3

#create a separate column with the rownames
icc_barchart_table$PIQ <- rownames(icc_barchart_table)

#turn the df into a wide form
icc_barchart_table_long <- icc_barchart_table %>%
  pivot_longer(cols = c("group1_sens_icc_sens", "group2_icc_sens", "group3_icc_sens"),
               names_to = "group", values_to = "icc") %>%
  mutate(lower_bound = ifelse(group == "group1_sens_icc_sens", group1_sens_lbound,
                              ifelse(group == "group2_icc_sens", group2_lbound, group3_lbound)),
         upper_bound = ifelse(group == "group1_sens_icc_sens", group1_sens_ubound,
                              ifelse(group == "group2_icc_sens", group2_ubound, group3_ubound)))

#create the barchart
barchart_icc_sens <- ggplot(icc_barchart_table_long, aes(x = PIQ, y = icc, fill = group)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_errorbar(aes(ymin = lower_bound, ymax = upper_bound),
                position = position_dodge(width = 0.9), width = 0.2) +
  labs(x = "Questionnaire", y = "ICC value") +
  scale_fill_manual(values = c("#E69F00", "#56B4E9", "#009E73"),
                    labels = c("Group 1", "Group 2", "Group 3")) +
  theme(text = element_text(size = 20, family = "serif")) + 
  guides(fill=guide_legend(title="")) +
  geom_signif(y_position = c(1, 
                             1.1,
                             1
                             ),
              xmin = c(0.7,
                       0.7,
                       1.68
                       ),
              xmax = c(1.3,
                       1, 
                       2.0
                       ),
              annotation = c("*", "*", "**"),
              tip_length = 0, textsize = 6,
  ) +
  scale_y_continuous(limits = c(0,1.12), breaks = c(0.20, 0.40, 0.60, 0.80, 1.0), labels = c(".20", ".40", ".60", ".80", "1.00"))
barchart_icc_sens

ggsave(filename = "Figures/Test-retest reliability/barchart_icc_sens.png", plot = barchart_icc_sens, width = 12, height = 8)




##### CHAPTER 2 - EBI -------

# Data preparation
test_sorted <- test[order(abs(test$test_rt_time_difference)), ] #order values ascendingly
test_sorted$test_rt_time_difference <- abs(test_sorted$test_rt_time_difference) # make negative values positive
test_sorted <- test_sorted[!is.na(test_sorted$retest_ebi_total_6),] #only include ppl who completed the EBI_rt

#EBI6
icc(test_sorted[,c(154, 158)], "twoway", "agreement", "single")


### STEP 2 - SENSITIITY TO TIME -----

# Extract the three groups
group1 <- head(test_sorted, 48)
group2 <- slice(test_sorted, 49:96)
group3 <- slice(test_sorted, 97:145)

summary(group1$test_rt_time_difference)
sd(group1$test_rt_time_difference)
summary(group2$test_rt_time_difference)
sd(group2$test_rt_time_difference)
summary(group3$test_rt_time_difference)
sd(group3$test_rt_time_difference)

#calculating icc values and CI ranges
#ebi6
ebi6_group1_icc <- round(icc(group1[,c(154, 158)])$value, 2)
ebi6_group1_lbound <- round(icc(group1[,c(154, 158)])$lbound, 2)
ebi6_group1_ubound <- round(icc(group1[,c(154, 158)])$ubound, 2)
ebi6_group1_ci <- paste("[", ebi6_group1_lbound, ", ", ebi6_group1_ubound, "]", sep = "")

ebi6_group2_icc <- round(icc(group2[,c(154, 158)])$value, 2)
ebi6_group2_lbound <- round(icc(group2[,c(154, 158)])$lbound, 2)
ebi6_group2_ubound <- round(icc(group2[,c(154, 158)])$ubound, 2)
ebi6_group2_ci <- paste("[", ebi6_group2_lbound, ", ", ebi6_group2_ubound, "]", sep = "")

ebi6_group3_icc <- round(icc(group3[,c(154, 158)])$value, 2)
ebi6_group3_lbound <- round(icc(group3[,c(154, 158)])$lbound, 2)
ebi6_group3_ubound <- round(icc(group3[,c(154, 158)])$ubound, 2)
ebi6_group3_ci <- paste("[", ebi6_group3_lbound, ", ", ebi6_group3_ubound, "]", sep = "")
     
group1_column <- c(ebi6_group1_icc, ebi6_group1_ci)
group2_column <- c(ebi6_group2_icc, ebi6_group2_ci)
group3_column <- c(ebi6_group3_icc, ebi6_group3_ci)

#turn into a word table
icc_table <- data.frame(group1_column, group2_column, group3_column)
colnames(icc_table) <- c("Group 1", "Group 2", "Group 3")


#turn into a bar chart
group1_margin_of_error <- ebi6_group1_ubound - ebi6_group1_lbound
group2_margin_of_error <- ebi6_group2_ubound - ebi6_group2_lbound
group3_margin_of_error <- ebi6_group3_ubound - ebi6_group3_lbound

icc_barchart_table <- data.frame(ebi6_group1_icc, ebi6_group1_lbound, ebi6_group1_ubound, group1_margin_of_error, 
                                 ebi6_group2_icc, ebi6_group2_lbound, ebi6_group2_ubound, group2_margin_of_error, 
                                 ebi6_group3_icc, ebi6_group3_lbound, ebi6_group3_ubound, group3_margin_of_error)

colnames(icc_barchart_table) <- sub("ebi6_", "", colnames(icc_barchart_table))

#calculate the average margin of error
icc_barchart_table$avg_margin_of_error_gr1_gr2 <- rowMeans(icc_barchart_table[,c("group1_margin_of_error", "group2_margin_of_error")])
icc_barchart_table$avg_margin_of_error_gr2_gr3 <- rowMeans(icc_barchart_table[,c("group2_margin_of_error", "group3_margin_of_error")])
icc_barchart_table$avg_margin_of_error_gr1_gr3 <- rowMeans(icc_barchart_table[,c("group1_margin_of_error", "group3_margin_of_error")])

#calculate the  overlap
icc_barchart_table$overlap_gr1_gr2 <- icc_barchart_table$group2_lbound - icc_barchart_table$group1_ubound
icc_barchart_table$overlap_gr2_gr3 <- icc_barchart_table$group3_lbound - icc_barchart_table$group2_ubound
icc_barchart_table$overlap_gr1_gr3 <- icc_barchart_table$group3_lbound - icc_barchart_table$group1_ubound

#calculate the proportion overlap over the avg margin of error
icc_barchart_table$overlap_over_margin_gr1_gr2 <- icc_barchart_table$overlap_gr1_gr2/icc_barchart_table$avg_margin_of_error_gr1_gr2
icc_barchart_table$overlap_over_margin_gr2_gr3 <- icc_barchart_table$overlap_gr2_gr3/icc_barchart_table$avg_margin_of_error_gr2_gr3
icc_barchart_table$overlap_over_margin_gr1_gr3 <- icc_barchart_table$overlap_gr1_gr3/icc_barchart_table$avg_margin_of_error_gr1_gr3



### STEP 3 - REPEAT EXCLUDING THE <14 INTERVALS -----
test_sorted <- test_sorted[test_sorted$test_rt_time_difference > 13, ] #only include ppl whose tests and retest were at least two weeks apart

#ebi6
icc(test_sorted[,c(154, 158)], "twoway", "agreement", "single")

# Extract the three groups
group1 <- head(test_sorted, 45)
group2 <- slice(test_sorted, 46:90)
group3 <- slice(test_sorted, 91:136)

summary(group1$test_rt_time_difference)
sd(group1$test_rt_time_difference)
summary(group2$test_rt_time_difference)
sd(group2$test_rt_time_difference)
summary(group3$test_rt_time_difference)
sd(group3$test_rt_time_difference)

#calculating icc values and CI ranges
#ebi6
ebi6_group1_icc <- round(icc(group1[,c(154, 158)])$value, 2)
ebi6_group1_lbound <- round(icc(group1[,c(154, 158)])$lbound, 2)
ebi6_group1_ubound <- round(icc(group1[,c(154, 158)])$ubound, 2)
ebi6_group1_ci <- paste("[", ebi6_group1_lbound, ", ", ebi6_group1_ubound, "]", sep = "")

ebi6_group2_icc <- round(icc(group2[,c(154, 158)])$value, 2)
ebi6_group2_lbound <- round(icc(group2[,c(154, 158)])$lbound, 2)
ebi6_group2_ubound <- round(icc(group2[,c(154, 158)])$ubound, 2)
ebi6_group2_ci <- paste("[", ebi6_group2_lbound, ", ", ebi6_group2_ubound, "]", sep = "")

ebi6_group3_icc <- round(icc(group3[,c(154, 158)])$value, 2)
ebi6_group3_lbound <- round(icc(group3[,c(154, 158)])$lbound, 2)
ebi6_group3_ubound <- round(icc(group3[,c(154, 158)])$ubound, 2)
ebi6_group3_ci <- paste("[", ebi6_group3_lbound, ", ", ebi6_group3_ubound, "]", sep = "")

group1_column <- c(ebi6_group1_icc, ebi6_group1_ci)
group2_column <- c(ebi6_group2_icc, ebi6_group2_ci)
group3_column <- c(ebi6_group3_icc, ebi6_group3_ci)

#turn into a word table
icc_table <- data.frame(group1_column, group2_column, group3_column)
colnames(icc_table) <- c("Group 1", "Group 2", "Group 3")


#turn into a bar chart
group1_margin_of_error <- ebi6_group1_ubound - ebi6_group1_lbound
group2_margin_of_error <- ebi6_group2_ubound - ebi6_group2_lbound
group3_margin_of_error <- ebi6_group3_ubound - ebi6_group3_lbound

icc_barchart_table <- data.frame(ebi6_group1_icc, ebi6_group1_lbound, ebi6_group1_ubound, group1_margin_of_error, 
                                 ebi6_group2_icc, ebi6_group2_lbound, ebi6_group2_ubound, group2_margin_of_error, 
                                 ebi6_group3_icc, ebi6_group3_lbound, ebi6_group3_ubound, group3_margin_of_error)

colnames(icc_barchart_table) <- sub("ebi6_", "", colnames(icc_barchart_table))

#calculate the average margin of error
icc_barchart_table$avg_margin_of_error_gr1_gr2 <- rowMeans(icc_barchart_table[,c("group1_margin_of_error", "group2_margin_of_error")])
icc_barchart_table$avg_margin_of_error_gr2_gr3 <- rowMeans(icc_barchart_table[,c("group2_margin_of_error", "group3_margin_of_error")])
icc_barchart_table$avg_margin_of_error_gr1_gr3 <- rowMeans(icc_barchart_table[,c("group1_margin_of_error", "group3_margin_of_error")])

#calculate the  overlap
icc_barchart_table$overlap_gr1_gr2 <- icc_barchart_table$group2_lbound - icc_barchart_table$group1_ubound
icc_barchart_table$overlap_gr2_gr3 <- icc_barchart_table$group3_lbound - icc_barchart_table$group2_ubound
icc_barchart_table$overlap_gr1_gr3 <- icc_barchart_table$group3_lbound - icc_barchart_table$group1_ubound

#calculate the proportion overlap over the avg margin of error
icc_barchart_table$overlap_over_margin_gr1_gr2 <- icc_barchart_table$overlap_gr1_gr2/icc_barchart_table$avg_margin_of_error_gr1_gr2
icc_barchart_table$overlap_over_margin_gr2_gr3 <- icc_barchart_table$overlap_gr2_gr3/icc_barchart_table$avg_margin_of_error_gr2_gr3
icc_barchart_table$overlap_over_margin_gr1_gr3 <- icc_barchart_table$overlap_gr1_gr3/icc_barchart_table$avg_margin_of_error_gr1_gr3


#turn the df into a wide form
icc_barchart_table_long <- icc_barchart_table %>%
  pivot_longer(cols = c("group1_icc", "group2_icc", "group3_icc"),
               names_to = "group", values_to = "icc") %>%
  mutate(lower_bound = ifelse(group == "group1_icc", group1_lbound,
                              ifelse(group == "group2_icc", group2_lbound, group3_lbound)),
         upper_bound = ifelse(group == "group1_icc", group1_ubound,
                              ifelse(group == "group2_icc", group2_ubound, group3_ubound)))


 

