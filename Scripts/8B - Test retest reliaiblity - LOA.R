rstudioapi::writeRStudioPreference("data_viewer_max_columns", 1000L)
rm(list = ls())

#import data
test <- read.csv("Data/Transformed_CSVs/test_anonymized_and_prepared.csv")

if(!require(pacman)){
  install.packages("pacman")
}
library(pacman)

pacman::p_load(ggExtra,
               blandr)


# Data preparation
test_sorted <- test[order(abs(test$test_rt_time_difference)), ] #order values ascendingly
test_sorted$test_rt_time_difference <- abs(test_sorted$test_rt_time_difference) # make negative values positive
test_sorted <- test_sorted[test_sorted$rt_completed == 1, ] #only include ppl who completed the PIQ_rt

tester <- blandr.statistics(test[["piv_total_23"]], test[["retest_piv_total_23"]]) #this helps inspect the blandr.statistics element


#CHAPTER 1 - PIQ -----


#STEP 1 - Make the LOA plots -----

#define a function to generate loa plots
loa_plotter <- function(testname, retestname, xlabname){
  plot <- blandr.draw(test_sorted[[testname]], test_sorted[[retestname]], plotProportionalBias = F) + 
    ggtitle("") +
    xlab(xlabname) +
    ylab("Total score difference") +
    theme(text = element_text(size = 20, family = "serif"))+ 
    geom_rug(col=rgb(.5,0,0,alpha=.2), outside = T, sides = "tr") +
    coord_cartesian(clip = "off" )+
    theme(plot.margin = margin(1, 1, 1, 1, "cm"))
}

#PIQ
piq23_loa_plot <- loa_plotter("piv_total_23", "retest_piv_total_23", "Total PIQ23-NL score")
piq23_loa_plot

piq28_loa_plot <- loa_plotter("piv_total_28", "retest_piv_total_28", "Total PIQ28-NL score")
piq28_loa_plot


ggsave(filename = "Figures/Test-retest reliabilitiy/piq23_loa_plot.png", plot = piq23_loa_plot , width = 12, height = 8)
ggsave(filename = "Figures/Test-retest reliabilitiy/piq28_loa_plot.png", plot = piq28_loa_plot , width = 12, height = 8)



#STEP 2 - Calculate the LOA stats and make the LOA plots -----

#define a function to calculate the LOA stats and save them nicely

LOA_calculator <- function(input_df, testname, retestname){
  sd_test <- sd(input_df[[testname]], na.rm = T)
  sd_retest <- sd(input_df[[retestname]], na.rm = T)
  avg_sd <- mean(c(sd_test, sd_retest))
  
  df_loa_stats <- blandr.statistics(input_df[[testname]], input_df[[retestname]])
  df_loa_stats
  
  mean_diff <- mean(abs(df_loa_stats$differences), na.rm = T)
  median_diff <- median(abs(df_loa_stats$differences), na.rm = T)
  std_mean_diff <- mean_diff/avg_sd
  std_median_diff <- median_diff/avg_sd
  
  df_means <- round(c(df_loa_stats$bias,df_loa_stats$upperLOA, df_loa_stats$lowerLOA, mean_diff, median_diff, std_mean_diff, std_median_diff ), 2)
  
  ci_maker <- function(lower, upper){
    lower <- round(lower, 2)
    upper <- round(upper, 2)
    ci <- paste("[", lower, ", ", upper, "]", sep = "")
    return(ci)
  }
  
  df_cis <- c(ci_maker(df_loa_stats$biasLowerCI, df_loa_stats$biasUpperCI),
                 ci_maker(df_loa_stats$upperLOA_lowerCI, df_loa_stats$upperLOA_upperCI),
                 ci_maker(df_loa_stats$lowerLOA_lowerCI, df_loa_stats$lowerLOA_upperCI),
              " ",
              " ",
              " ",
              " ")
  
  upper_limit_over_sd <- c(round(df_loa_stats$upperLOA/avg_sd,2), 
                           ci_maker(round(df_loa_stats$upperLOA_lowerCI/avg_sd,2), round(df_loa_stats$upperLOA_upperCI/avg_sd,2)))
  lower_limit_over_sd <- c(round(df_loa_stats$lowerLOA/avg_sd,2),
                           ci_maker(round(df_loa_stats$lowerLOA_lowerCI/avg_sd,2), round(df_loa_stats$lowerLOA_upperCI/avg_sd,2)))
  
  df_df <- t(data.frame(df_means, df_cis))
  df_df <- cbind(df_df, upper_limit_over_sd, lower_limit_over_sd)
  colnames(df_df) <- c("Mean", "Upper limit", "Lower limit", "Mean absolute difference", "Median absolute difference", "Standardized mean absolute difference", "Standardized median absolute difference", "Upper limit over SD", "Lower limit over SD")
  
  df_df <- df_df[,c(1:3, 8, 9, 4:7)]
  
  return(df_df)
}

#PIQ
piq23_loa_stats <- LOA_calculator(test_sorted, "piv_total_23", "retest_piv_total_23")
piq28_loa_stats <- LOA_calculator(test_sorted, "piv_total_28", "retest_piv_total_28")

piq_loa_df <- rbind(piq23_loa_stats, piq28_loa_stats)

### STEP 3 - SENSITIVITY TO TIME ANALYSIS -----

##STEP 3A - MAIN ANALYSIS
#split into three groups
group1 <- head(test_sorted, 50)
group2 <- slice(test_sorted, 51:100)
group3 <- slice(test_sorted, 101:151)

#group1
piq23_group1_loa_stats <- LOA_calculator(group1, "piv_total_23", "retest_piv_total_23")
piq28_group1_loa_stats <- LOA_calculator(group1, "piv_total_28", "retest_piv_total_28")

piq_group1_loa_df <- rbind(piq23_group1_loa_stats, piq28_group1_loa_stats)

#group2
piq23_group2_loa_stats <- LOA_calculator(group2, "piv_total_23", "retest_piv_total_23")
piq28_group2_loa_stats <- LOA_calculator(group2, "piv_total_28", "retest_piv_total_28")

piq_group2_loa_df <- rbind(piq23_group2_loa_stats, piq28_group2_loa_stats)

#group3
piq23_group3_loa_stats <- LOA_calculator(group3, "piv_total_23", "retest_piv_total_23")
piq28_group3_loa_stats <- LOA_calculator(group3, "piv_total_28", "retest_piv_total_28")

piq_group3_loa_df <- rbind(piq23_group3_loa_stats, piq28_group3_loa_stats)


sensitivity_analyser <- function(testname, retestname){
  
  loa_stats_maker <- function(group){
    
    N <- length(group$X)
    
    group_loa_stats <- blandr.statistics(group[[testname]], group[[retestname]])
    upperLOA <- group_loa_stats$upperLOA
    lowerLOA <- abs(group_loa_stats$lowerLOA)
    
    upperLOA_sd <- sqrt(N) * (group_loa_stats$upperLOA_upperCI - group_loa_stats$upperLOA_lowerCI) / (group_loa_stats$sig.level.convert.to.z * 2)
    lowerLOA_sd <- sqrt(N) * (group_loa_stats$lowerLOA_upperCI - group_loa_stats$lowerLOA_lowerCI) / (group_loa_stats$sig.level.convert.to.z * 2)
    
    if(upperLOA > lowerLOA){
      LOA <- upperLOA
      SD <- upperLOA_sd
    }
    else if(lowerLOA > upperLOA){
      LOA <- lowerLOA
      SD <- lowerLOA_sd
    }
    else(
      print(paste("Error. Neither upper or lower LOA is greater. Upper LOA is: ", upperLOA, ". Lower LOA is: ", lowerLOA, ".", sep = ""))
    )
    group_stats <- c(LOA, SD)
    return(group_stats)
  }
  
  group1_stats <- loa_stats_maker(group1)
  group2_stats <- loa_stats_maker(group2)
  group3_stats <- loa_stats_maker(group3)
  
  df <- data.frame(group1_stats, group2_stats, group3_stats)
  colnames(df) <- c("group1", "group2", "group3")
  rownames(df) <- c("LOA", "SD")
  
  return(df)
}

p_calculator <- function(testname, retestname, ngr1, ngr2, ngr3){
  sens_stats <- sensitivity_analyser(testname, retestname)
  
  t.test2 <- function(m1,m2,s1,s2,n1,n2,m0=0,equal.variance=FALSE)
  {
    if( equal.variance==FALSE ) 
    {
      se <- sqrt( (s1^2/n1) + (s2^2/n2) )
      # welch-satterthwaite df
      df <- ( (s1^2/n1 + s2^2/n2)^2 )/( (s1^2/n1)^2/(n1-1) + (s2^2/n2)^2/(n2-1) )
    } else
    {
      # pooled standard deviation, scaled by the sample sizes
      se <- sqrt( (1/n1 + 1/n2) * ((n1-1)*s1^2 + (n2-1)*s2^2)/(n1+n2-2) ) 
      df <- n1+n2-2
    }      
    t <- (m1-m2-m0)/se 
    dat <- c(m1-m2, se, t, 2*pt(-abs(t),df))    
    names(dat) <- c("Difference of means", "Std Error", "t", "p-value")
    return(dat) 
  }
  
  gr1_gr2 <- t.test2(m1 = sens_stats$group1[1], 
                     m2 = sens_stats$group2[1],
                     s1 = sens_stats$group1[2],
                     s2 = sens_stats$group2[2],
                     n1 = ngr1,
                     n2 = ngr2)
  gr1_gr2[["p-value"]]
  
  gr1_gr3 <- t.test2(m1 = sens_stats$group1[1], 
                     m2 = sens_stats$group3[1],
                     s1 = sens_stats$group1[2],
                     s2 = sens_stats$group3[2],
                     n1 = ngr1,
                     n2 = ngr3)
  gr1_gr3[["p-value"]]
  
  gr2_gr3 <- t.test2(m1 = sens_stats$group2[1], 
                     m2 = sens_stats$group3[1],
                     s1 = sens_stats$group2[2],
                     s2 = sens_stats$group3[2],
                     n1 = ngr2,
                     n2 = ngr3)
  gr2_gr3[["p-value"]]
  
  print(paste("Gr1 and gr2 p-value: ", gr1_gr2[["p-value"]], ". Gr1 and gr3 p-value: ", gr1_gr3[["p-value"]], ". Gr2 and gr3 p-value: ", gr2_gr3[["p-value"]], sep = ""))
}
p_calculator("piv_total_23", "retest_piv_total_23", 50, 50, 51)
p_calculator("piv_total_28", "retest_piv_total_28", 50, 50, 51)

### STEP 4 - REPEAT EXCLUDING THE <14 INTERVALS -----
test_sorted <- test_sorted[test_sorted$test_rt_time_difference > 13, ] #only

# Extract the three groups
group1 <- head(test_sorted, 46)
group2 <- slice(test_sorted, 47:93)
group3 <- slice(test_sorted, 94:140)

p_calculator("piv_total_23", "retest_piv_total_23", 46, 47, 47)
p_calculator("piv_total_28", "retest_piv_total_28", 46, 47, 47)


#### CHAPTER 2 - EBI

# Data preparation
test_sorted <- test[order(abs(test$test_rt_time_difference)), ] #order values ascendingly
test_sorted$test_rt_time_difference <- abs(test_sorted$test_rt_time_difference) # make negative values positive
test_sorted <- test_sorted[!is.na(test_sorted$retest_ebi_mean_6), ] #only include ppl who completed the EBI_rt

#STEP 1 - Make the LOA plots and calculate loa stats -----
ebi6_loa_plot <- loa_plotter("ebi_mean_6", "retest_ebi_mean_6", "Mean EBI-NL score")
ebi_loa_stats <- LOA_calculator(test_sorted, "ebi_mean_6", "retest_ebi_mean_6")

ggsave(filename = "Figures/Test-retest reliability/ebi6_loa_plot.png", plot = ebi6_loa_plot , width = 12, height = 8)

### STEP 2 - SENSITIVITY TO TIME ANALYSIS -----

##STEP 2A - MAIN ANALYSIS
#split into three groups
group1 <- head(test_sorted, 48)
group2 <- slice(test_sorted, 49:96)
group3 <- slice(test_sorted, 97:145)

#group1
ebi6_group1_loa_stats <- LOA_calculator(group1, "ebi_mean_6", "retest_ebi_mean_6")
ebi6_group2_loa_stats <- LOA_calculator(group2, "ebi_mean_6", "retest_ebi_mean_6")
ebi6_group3_loa_stats <- LOA_calculator(group3, "ebi_mean_6", "retest_ebi_mean_6")

#bind together
all_loa_stats <- rbind(ebi_loa_stats, ebi6_group1_loa_stats, ebi6_group2_loa_stats, ebi6_group3_loa_stats)
all_loa_stats <- as.data.frame(all_loa_stats)
rownames(all_loa_stats) <- c("Full sample", " ", "Group 1", "  ", "Group 2", "   ", "Group 3", "    ")

p_calculator("ebi_mean_6", "retest_ebi_mean_6", 48, 48, 49)

### STEP 4 - REPEAT EXCLUDING THE <14 INTERVALS -----
test_sorted <- test_sorted[test_sorted$test_rt_time_difference > 13, ] #only

# Extract the three groups
group1 <- head(test_sorted, 45)
group2 <- slice(test_sorted, 46:90)
group3 <- slice(test_sorted, 91:136)

p_calculator("ebi_mean_6", "retest_ebi_mean_6", 48, 48, 49)

