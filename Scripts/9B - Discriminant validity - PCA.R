rstudioapi::writeRStudioPreference("data_viewer_max_columns", 1000L)
rm(list = ls())

if(!require(pacman)){
  install.packages("pacman")
}
library(pacman)

pacman::p_load(MASS,
               psych,
               ggplot2,
               corrplot,
               Cairo,
               lavaan,
               dynamic)


#import data
test <- read.csv("Data/Transformed_CSVs/test_anonymized_and_prepared.csv")
test_piq23 <- read.csv("Data/Transformed_CSVs/test_piq23.csv")
test_piq28 <- read.csv("Data/Transformed_CSVs/test_piq28.csv")
test_pis6 <- read.csv("Data/Transformed_CSVs/test_pis6.csv")
test_ebi6 <- read.csv("Data/Transformed_CSVs/test_ebi6.csv")
test_meq30 <- read.csv("Data/Transformed_CSVs/test_meq.csv")


### STEP 1 - PIQ AND MEQ ----
## Data preparation
piq23_and_meq <- test[only_included_item_names$only_included_item_colnames[c(1:23, 30:59)]]
piq28_and_meq <- test[all_item_names$item_colnames[c(1:28, 37:66)]]

## KMO and Bartlett's tests

#PIQ23
EFAtools::KMO(piq23_and_meq)
EFAtools::BARTLETT(piq23_and_meq)

#PIQ28
EFAtools::KMO(piq28_and_meq)
EFAtools::BARTLETT(piq28_and_meq)


##PIQ23 -----
# running the PCA with all possible factors
piq23_meq <- principal(piq23_and_meq, rotate = "promax", cor = "poly")

# computing eigenvalues
piq23_meq_eigenvalues <- data.frame(c(1:53), piq23_meq$values)
colnames(piq23_meq_eigenvalues) <- c("PC", "eigenvalue")

# plotting them on a scree plot
screeplot_piq23_meq <- ggplot(piq23_meq_eigenvalues, aes(PC, eigenvalue)) +
  geom_point() + geom_line() +
  geom_abline(slope = 0, intercept = 1, colour = "red") +
  labs(x = "Component number", y = "Eigenvalue") +
  theme(text = element_text(size = 20, family = "serif"))
screeplot_piq23_meq

ggsave(filename = "Figures/EFA/screeplot_piq23_meq.png", plot = screeplot_piq23_meq, width = 12, height = 8)

# creating a function to turn the PCAs into nice Wordable dfs

pc_df_maker <- function(fit){
  
  #save the factor loadings in a df and round to 2 decimals
  df <- as.data.frame.matrix(as.table(fit$loadings))
  df <- round(df, 2)
  
  #make all factor loadings greater than 0.4 boldface
  
  df$h2 <- round(fit$communality, 2)
  df$u2 <- round(fit$uniquenesses, 2)
  #df <- df[, c(ncol(df), 1:(ncol(df)-1))]
  rownames(df) <- all_item_names$item_realnames[match(rownames(df), all_item_names$item_colnames)]
  
  return(df)
}

piq23_meq_4f <- principal(piq23_and_meq, nfactors = 4, rotate = "promax", cor = "poly")
piq23_meq_4f_df <- pc_df_maker(piq23_meq_4f)

piq23_meq_5f <- principal(piq23_and_meq, nfactors = 5, rotate = "promax", cor = "poly")
piq23_meq_5f_df <- pc_df_maker(piq23_meq_5f)

piq23_meq_6f <- principal(piq23_and_meq, nfactors = 6, rotate = "promax", cor = "poly")
piq23_meq_6f_df <- pc_df_maker(piq23_meq_6f)

piq23_meq_7f <- principal(piq23_and_meq, nfactors = 7, rotate = "promax", cor = "poly")
piq23_meq_7f_df <- pc_df_maker(piq23_meq_7f)

## PIQ28 -----
piq28_meq <- principal(piq28_and_meq, rotate = "promax", cor = "poly")

# computing eigenvalues
piq28_meq_eigenvalues <- data.frame(c(1:58), piq28_meq$values)
colnames(piq28_meq_eigenvalues) <- c("PC", "eigenvalue")

# plotting them on a scree plot
screeplot_piq28_meq <- ggplot(piq28_meq_eigenvalues, aes(PC, eigenvalue)) +
  geom_point() + geom_line() +
  geom_abline(slope = 0, intercept = 1, colour = "red") +
  labs(x = "Component number", y = "Eigenvalue") +
  theme(text = element_text(size = 20, family = "serif"))
screeplot_piq28_meq

ggsave(filename = "Figures/EFA/screeplot_piq28_meq.png", plot = screeplot_piq28_meq, width = 12, height = 8)

piq28_meq_4f <- principal(piq28_and_meq, nfactors = 4, rotate = "promax", cor = "poly")
piq28_meq_4f_df <- pc_df_maker(piq28_meq_4f)

piq28_meq_5f <- principal(piq28_and_meq, nfactors = 5, rotate = "promax", cor = "poly")
piq28_meq_5f_df <- pc_df_maker(piq28_meq_5f)

piq28_meq_6f <- principal(piq28_and_meq, nfactors = 6, rotate = "promax", cor = "poly")
piq28_meq_6f_df <- pc_df_maker(piq28_meq_6f)

piq28_meq_7f <- principal(piq28_and_meq, nfactors = 7, rotate = "promax", cor = "poly")
piq28_meq_7f_df <- pc_df_maker(piq28_meq_7f)


### STEP 2 - EBI AND MEQ ----
## Data preparation
ebi6_and_meq <- test[only_included_item_names$only_included_item_colnames[c(24:59)]]

## KMO and Bartlett's tests

#ebi6
EFAtools::KMO(ebi6_and_meq)
EFAtools::BARTLETT(ebi6_and_meq)

# running the PCA with all possible factors
ebi6_meq <- principal(ebi6_and_meq, rotate = "promax")

# computing eigenvalues
ebi6_meq_eigenvalues <- data.frame(c(1:36), ebi6_meq$values)
colnames(ebi6_meq_eigenvalues) <- c("PC", "eigenvalue")

# plotting them on a scree plot
screeplot_ebi6_meq <- ggplot(ebi6_meq_eigenvalues, aes(PC, eigenvalue)) +
  geom_point() + geom_line() +
  geom_abline(slope = 0, intercept = 1, colour = "red") +
  labs(x = "Component number", y = "Eigenvalue") +
  theme(text = element_text(size = 20, family = "serif"))
screeplot_ebi6_meq

ggsave(filename = "Figures/EFA/screeplot_ebi6_meq.png", plot = screeplot_ebi6_meq, width = 12, height = 8)


ebi6_meq_4f <- principal(ebi6_and_meq, nfactors = 4, rotate = "promax")
ebi6_meq_4f_df <- pc_df_maker(ebi6_meq_4f)

ebi6_meq_5f <- principal(ebi6_and_meq, nfactors = 5, rotate = "promax")
ebi6_meq_5f_df <- pc_df_maker(ebi6_meq_5f)

ebi6_meq_6f <- principal(ebi6_and_meq, nfactors = 6, rotate = "promax")
ebi6_meq_6f_df <- pc_df_maker(ebi6_meq_6f)

ebi6_meq_7f <- principal(ebi6_and_meq, nfactors = 7, rotate = "promax")
ebi6_meq_7f_df <- pc_df_maker(ebi6_meq_7f)



### STEP 3 - PIS AND MEQ -----

## Data preparation
pis6_and_meq <- test[only_included_item_names$only_included_item_colnames[c(70:75, 30:59 )]]

## KMO and Bartlett's tests

#pis6
EFAtools::KMO(pis6_and_meq)
EFAtools::BARTLETT(pis6_and_meq)

# running the PCA with all possible factors
pis6_meq <- principal(pis6_and_meq, rotate = "promax")

# computing eigenvalues
pis6_meq_eigenvalues <- data.frame(c(1:36), pis6_meq$values)
colnames(pis6_meq_eigenvalues) <- c("PC", "eigenvalue")

# plotting them on a scree plot
screeplot_pis6_meq <- ggplot(pis6_meq_eigenvalues, aes(PC, eigenvalue)) +
  geom_point() + geom_line() +
  geom_abline(slope = 0, intercept = 1, colour = "red") +
  labs(x = "Component number", y = "Eigenvalue") +
  theme(text = element_text(size = 20, family = "serif"))
screeplot_pis6_meq

ggsave(filename = "Figures/EFA/screeplot_pis6_meq.png", plot = screeplot_pis6_meq, width = 12, height = 8)

# creating a function to turn the PCAs into nice Wordable dfs


pis6_meq_4f <- principal(pis6_and_meq, nfactors = 4, rotate = "promax")
pis6_meq_4f_df <- pc_df_maker(pis6_meq_4f)

pis6_meq_5f <- principal(pis6_and_meq, nfactors = 5, rotate = "promax")
pis6_meq_5f_df <- pc_df_maker(pis6_meq_5f)

pis6_meq_6f <- principal(pis6_and_meq, nfactors = 6, rotate = "promax")
pis6_meq_6f_df <- pc_df_maker(pis6_meq_6f)


### STEP 4 - PIQ AND EBI -----

piq23_and_ebi <- test[only_included_item_names$only_included_item_colnames[c(1:29)]]
piq28_and_ebi <- test[all_item_names$item_colnames[c(1:31, 33, 34, 36)]]

## KMO and Bartlett's tests

#PIQ23
EFAtools::KMO(piq23_and_ebi)
EFAtools::BARTLETT(piq23_and_ebi)

#PIQ28
EFAtools::KMO(piq28_and_ebi)
EFAtools::BARTLETT(piq28_and_ebi)

## PIQ23 -----
# running the PCA with all possible factors
piq23_ebi <- principal(piq23_and_ebi, rotate = "promax")

# computing eigenvalues
piq23_ebi_eigenvalues <- data.frame(c(1:29), piq23_ebi$values)
colnames(piq23_ebi_eigenvalues) <- c("PC", "eigenvalue")

# making scree plot
screeplot_piq23_ebi <- ggplot(piq23_ebi_eigenvalues, aes(PC, eigenvalue)) +
  geom_point() + geom_line() +
  geom_abline(slope = 0, intercept = 1, colour = "red") +
  labs(x = "Component number", y = "Eigenvalue") +
  theme(text = element_text(size = 20, family = "serif"))
screeplot_piq23_ebi

ggsave(filename = "Figures/EFA/screeplot_piq23_ebi.png", plot = screeplot_piq23_ebi, width = 12, height = 8)

piq23_ebi_2f <- principal(piq23_and_ebi, nfactors = 2, rotate = "promax")
piq23_ebi_2f_df <- pc_df_maker(piq23_ebi_2f)

piq23_ebi_3f <- principal(piq23_and_ebi, nfactors = 3, rotate = "promax")
piq23_ebi_3f_df <- pc_df_maker(piq23_ebi_3f)

piq23_ebi_4f <- principal(piq23_and_ebi, nfactors = 4, rotate = "promax")
piq23_ebi_4f_df <- pc_df_maker(piq23_ebi_4f)

## PIQ28 -----
# running the PCA with all possible factors
piq28_ebi <- principal(piq28_and_ebi, rotate = "promax")

# computing eigenvalues
piq28_ebi_eigenvalues <- data.frame(c(1:34), piq28_ebi$values)
colnames(piq28_ebi_eigenvalues) <- c("PC", "eigenvalue")

# making scree plot
screeplot_piq28_ebi <- ggplot(piq28_ebi_eigenvalues, aes(PC, eigenvalue)) +
  geom_point() + geom_line() +
  geom_abline(slope = 0, intercept = 1, colour = "red") +
  labs(x = "Component number", y = "Eigenvalue") +
  theme(text = element_text(size = 20, family = "serif"))
screeplot_piq28_ebi

ggsave(filename = "Figures/EFA/screeplot_piq28_ebi.png", plot = screeplot_piq28_ebi, width = 12, height = 8)

piq28_ebi_2f <- principal(piq28_and_ebi, nfactors = 2, rotate = "promax")
piq28_ebi_2f_df <- pc_df_maker(piq28_ebi_2f)

piq28_ebi_3f <- principal(piq28_and_ebi, nfactors = 3, rotate = "promax")
piq28_ebi_3f_df <- pc_df_maker(piq28_ebi_3f)

piq28_ebi_4f <- principal(piq28_and_ebi, nfactors = 4, rotate = "promax")
piq28_ebi_4f_df <- pc_df_maker(piq28_ebi_4f)





### STEP 5 - PIS AND EBI ----

#firstly with only EBI

pis6_and_ebi6 <- test[all_item_names$item_colnames[c(29:31, 33, 34, 36, 77:82)]]
pis6_ebi6_pca <- principal(pis6_and_ebi6, rotate = "promax", cor = "cor")

# computing eigenvalues
pis6_ebi6_eigenvalues <- data.frame(c(1:12), pis6_ebi6_pca$values)
colnames(pis6_ebi6_eigenvalues) <- c("PC", "eigenvalue")

# plotting them on a scree plot
screeplot_pis6_ebi6 <- ggplot(pis6_ebi6_eigenvalues, aes(PC, eigenvalue)) +
  geom_point() + geom_line() +
  geom_abline(slope = 0, intercept = 1, colour = "red") +
  labs(x = "Component number", y = "Eigenvalue") +
  theme(text = element_text(size = 20, family = "serif"))
screeplot_pis6_ebi6

ggsave(filename = "Figures/EFA/screeplot_pis6_ebi6.png", plot = screeplot_pis6_ebi6, width = 12, height = 8)

pis6_ebi6_2f <- principal(pis6_and_ebi6, nfactors = 2, rotate = "promax", cor = "cor")
pis6_ebi6_2f_df <- pc_df_maker(pis6_ebi6_2f)


## CORPLOTS EBI AND PIS

#let's firstly adjust the colnames so we have nice rownames in the corrplots
colnames(pis6_and_ebi6) <- all_item_names$english_shortname[match(colnames(pis6_and_ebi6), all_item_names$item_colnames)]

Cairo(1200, 1200, file="Figures/EFA/pis6_ebi6_corrplot.png", pointsize = 24, type="png", bg="white")

corrplot(cor(pis6_and_ebi6, 
             use = "pairwise.complete.obs"), 
         method = "color", 
         tl.col = "black", 
         tl.cex = 0.7,
         #col.lim = c(0,1),
         number.cex = 0.7,
         addCoef.col = "black")

recordPlot()
dev.off()


