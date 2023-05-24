rstudioapi::writeRStudioPreference("data_viewer_max_columns", 1000L)
rm(list = ls())

if(!require(pacman)){
  install.packages("pacman")
}
library(pacman)

pacman::p_load(rempsyc,
               psych,
               EFAtools,
               ggplot2)

#import data
test <- read.csv("Data/Transformed_CSVs/test_anonymized_and_prepared.csv")

test_piq28 <- read.csv('Data/Transformed_CSVs/test_piq28.csv')
test_ebi8 <- read.csv('Data/Transformed_CSVs/test_ebi8.csv')
test_pis7 <- read.csv('Data/Transformed_CSVs/test_pis7.csv')
test_piq23 <- read.csv("Data/Transformed_CSVs/test_piq23.csv")
test_pis6 <- read.csv("Data/Transformed_CSVs/test_pis6.csv")
test_ebi6 <- read.csv("Data/Transformed_CSVs/test_ebi6.csv")
test_meq30 <- read.csv("Data/Transformed_CSVs/test_meq.csv")

item_names <- read.csv("Data/Transformed_CSVs/all_item_names.csv")

### PIQ ----

## Step 1 - KMO and Bartlett's test
EFAtools::KMO(test_piq23[,2:24])
EFAtools::BARTLETT(test_piq23[,2:24])

## Step 2 - Performing the PCA

#PIQ23

# running the PCA with all possible factors
piq23_23f <- principal(test_piq23[,2:24], nfactors=23, rotate = "promax", cor = "poly")

# computing eigenvalues
piq23_eigenvalues <- data.frame(c(1:23), piq23_23f$values)
colnames(piq23_eigenvalues) <- c("PC", "eigenvalue")

# plotting them on a scree plot
screeplot_piq23 <- ggplot(piq23_eigenvalues, aes(PC, eigenvalue)) +
  geom_point() + geom_line() +
  geom_abline(slope = 0, intercept = 1, colour = "red") +
  labs(x = "Component number", y = "Eigenvalue") +
  theme(text = element_text(size = 20))
screeplot_piq23

ggsave(filename = "Figures/EFA/screeplot_piq23.png", plot = screeplot_piq23, width = 12, height = 8)

# Running with 2 factors
piq23_2f <- principal(test_piq23[,2:24], nfactors=2, rotate = "promax", cor = "poly")
piq23_2f

pc_df_maker <- function(fit){
  
  #save the factor loadings in a df and round to 2 decimals
  df <- as.data.frame.matrix(as.table(fit$loadings))
  df <- round(df, 2)
  
  #make all factor loadings greater than 0.4 boldface
  
  df$h2 <- round(fit$communality, 2)
  df$u2 <- round(fit$uniquenesses, 2)
  df$items <- item_names$full_items[match(rownames(df), item_names$item_colnames)]
  df <- df[, c(ncol(df), 1:(ncol(df)-1))]
  
  return(df)
}

piq23_2f_df <- pc_df_maker(piq23_2f)

# Running with 3 factors
piq23_3f <- principal(test_piq23[,2:24], nfactors=3, rotate = "promax", cor = "poly")
piq23_3f

piq23_3f_df <- pc_df_maker(piq23_3f)
piq23_3f_df <- piq23_3f_df[, c("items", "RC1", "RC2", "RC3", "h2", "u2")]

#PIQ28
# running the PCA
piq28_28f <- principal(test_piq28[,2:29], nfactors=28, rotate = "promax", cor = "poly")

# computing eigenvalues
piq28_eigenvalues <- data.frame(c(1:28), piq28_28f$values)
colnames(piq28_eigenvalues) <- c("PC", "eigenvalue")

# plotting them on a scree plot
screeplot_piq28 <- ggplot(piq28_eigenvalues, aes(PC, eigenvalue)) +
  geom_point() + geom_line() +
  geom_abline(slope = 0, intercept = 1, colour = "red") +
  labs(x = "Component number", y = "Eigenvalue") +
  theme(text = element_text(size = 20)) +
  scale_x_continuous(breaks = c(0, 5, 10, 15, 20, 25), labels = c(0, 5, 10, 15, 20, 25))
screeplot_piq28

ggsave(filename = "Figures/EFA/screeplot_piq28.png", plot = screeplot_piq28, width = 12, height = 8)

# Running with 2 factors
piq28_2f <- principal(test_piq28[,2:29], nfactors=2, rotate = "promax", cor = "poly")
piq28_2f

piq28_2f_df <- pc_df_maker(piq28_2f)

# 3 factors
piq28_3f <- principal(test_piq28[,2:29], nfactors=3, rotate = "promax", cor = "poly")
piq28_3f

piq28_3f_df <- pc_df_maker(piq28_3f)
piq28_3f_df <- piq28_3f_df[, c("items", "RC1", "RC2", "RC3", "h2", "u2")]

# 4 factors
piq28_4f <- principal(test_piq28[,2:29], nfactors=4, rotate = "promax", cor = "poly")
piq28_4f

piq28_4f_df <- pc_df_maker(piq28_4f)
piq28_4f_df <- piq28_4f_df[, c("items", "RC1", "RC2", "RC3", "RC4", "h2", "u2")]

# Running with 5 factors
piq28_5f <- principal(test_piq28[,2:29], nfactors=5, rotate = "promax", cor = "poly")
piq28_5f

piq28_5f_df <- pc_df_maker(piq28_5f)
piq28_5f_df <- piq28_5f_df[, c("items", "RC1", "RC2", "RC3", "RC4", "RC5", "h2", "u2")]


#EBI6 -----

## Step 1 - KMO and Bartlett's test
EFAtools::KMO(test_ebi6[,2:7])
EFAtools::BARTLETT(test_ebi6[,2:7])

## Step 2 - Scree plot
# running the PCA with all possible factors
ebi6_6f <- principal(test_ebi6[,2:7], nfactors=6, rotate = "promax")

# computing eigenvalues
ebi6_eigenvalues <- data.frame(c(1:6), ebi6_6f$values)
colnames(ebi6_eigenvalues) <- c("PC", "eigenvalue")

# plotting them on a scree plot
screeplot_ebi6 <- ggplot(ebi6_eigenvalues, aes(PC, eigenvalue)) +
  geom_point() + geom_line() +
  geom_abline(slope = 0, intercept = 1, colour = "red") +
  labs(x = "Component number", y = "Eigenvalue") +
  theme(text = element_text(size = 20, family = "serif"))
screeplot_ebi6

ggsave(filename = "Figures/EFA/screeplot_ebi6.png", plot = screeplot_ebi6, width = 12, height = 8)



#PIS6 -----

## Step 1 - KMO and Bartlett's test
EFAtools::KMO(test_pis6[,2:7])
EFAtools::BARTLETT(test_pis6[,2:7])

## Step 2 - Scree plot
# running the PCA with all possible factors
pis6_6f <- principal(test_pis6[,2:7], nfactors=6, rotate = "promax")

# computing eigenvalues
pis6_eigenvalues <- data.frame(c(1:6), pis6_6f$values)
colnames(pis6_eigenvalues) <- c("PC", "eigenvalue")

# plotting them on a scree plot
screeplot_pis6 <- ggplot(pis6_eigenvalues, aes(PC, eigenvalue)) +
  geom_point() + geom_line() +
  geom_abline(slope = 0, intercept = 1, colour = "red") +
  labs(x = "Component number", y = "Eigenvalue") +
  theme(text = element_text(size = 20, family = "serif"))
screeplot_pis6

ggsave(filename = "Figures/EFA/screeplot_pis6.png", plot = screeplot_pis6, width = 12, height = 8)

