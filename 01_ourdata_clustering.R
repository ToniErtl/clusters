# 01 - Clustering algorithms

#----------- Setup

library(tidyverse)
library(ggplot2)
library(ggthemes)

#library(rgl) # 3 dimensional plots
library(factoextra)
library(crosstable) # crosstable function


# Clustering methods:
library(cluster)

# For random forest modeling and visualization:
library(rpart)
library(partykit)
library(party)

# Additional packages for html output:

library(knitr)
library(kableExtra)





# Functions used:

corrplot2 <- function(data,
                      method = "pearson",
                      sig.level = 0.05,
                      order = "original",
                      diag = FALSE,
                      type = "upper",
                      tl.srt = 90,
                      number.font = 1,
                      number.cex = 1,
                      mar = c(0, 0, 0, 0)) {
  library(corrplot)
  data_incomplete <- data
  data <- data[complete.cases(data), ]
  mat <- cor(data, method = method)
cor.mtest <- function(mat, method) {
    mat <- as.matrix(mat)
    n <- ncol(mat)
    p.mat <- matrix(NA, n, n)
    diag(p.mat) <- 0
    for (i in 1:(n - 1)) {
      for (j in (i + 1):n) {
        tmp <- cor.test(mat[, i], mat[, j], method = method)
        p.mat[i, j] <- p.mat[j, i] <- tmp$p.value
      }
    }
    colnames(p.mat) <- rownames(p.mat) <- colnames(mat)
    p.mat
  }
  p.mat <- cor.mtest(data, method = method)
  col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
  corrplot(mat,
           method = "color", col = col(200), number.font = number.font,
           mar = mar, number.cex = number.cex,
           type = type, order = order,
           addCoef.col = "black", # add correlation coefficient
           tl.col = "black", tl.srt = tl.srt, # rotation of text labels
           # combine with significance level
           p.mat = p.mat, sig.level = sig.level, insig = "blank",
           # hide correlation coefficiens on the diagonal
           diag = diag
  )
}

# -----------------------------# 

# Data setup:
data <- read.csv(".//Data in brief//Horn-Kiss-Lenard2021.csv")


data_cleaned <- data %>%  mutate(STournamentChoosed =replace_na(STournamentChoosed,0)) %>%
  dplyr::mutate(STournamentChoosed_comp =STournamentChoosed* comp) %>%
  dplyr::select(studid,
                classid,
                delta,
                beta ,# time inconsistency
                dictator, #altruism
                dictator_schoolmate, #altruism
                risk, # risk
                publicgood, # cooperativeness
                trust, # trust
                trust_return, #trust
                comp) %>%  # Competition
  drop_na() 

# Scale data (without competition, as it is a binary variable)
data_scaled <- data_cleaned %>%  dplyr::select(-studid,-classid, -comp) %>% 
  scale()
data_scaled <- as.data.frame(data_scaled)

data_scaled_id$studid <- data_cleaned %>% select(studid)

# Basic Descriptives
data_nomiss <- data %>% 
  dplyr::select(math,read,delta,
                beta,
                dictator,dictator_schoolmate,risk,publicgood,
                trust,trust_return, comp) %>% 
  drop_na()

stargazer::stargazer(data_nomiss,summary = TRUE, mean.sd = TRUE, median = TRUE,
                     iqr = TRUE)
rm(data_nomiss)
# --------------


# Define number of clusters based on consensus:
library(parameters)
set.seed(123)



# only continous variables, non-scaled:

n_clust <- parameters::n_clusters(data_nomiss,
                                  package = c("easystats", "NbClust", "mclust"),
                                  standardize = FALSE,
                                  include_factors = TRUE,
                                  nbclust_method ="pam"
)
View(n_clust)

n_clust_hclust<- parameters::n_clusters(data_nomiss,
                                  package = c("easystats", "NbClust", "mclust"),
                                  standardize = FALSE,
                                  include_factors = TRUE,
                                  nbclust_method ="hcut"
)
View(n_clust_hclust)


# only continous variables, scaled:

n_clust_scaled <- parameters::n_clusters(data_scaled,
                                  package = c("easystats", "NbClust", "mclust"),
                                  standardize = FALSE,
                                  include_factors = TRUE,
                                  nbclust_method ="pam"
)
View(n_clust_scaled)

n_clust_scaled_hclust <- parameters::n_clusters(data_scaled,
                                         package = c("easystats", "NbClust", "mclust"),
                                         standardize = FALSE,
                                         include_factors = TRUE,
                                         nbclust_method ="hcut"
)
View(n_clust_scaled_hclust)

# competition included, scaled continous variables
data_clustering <- data_scaled
data_clustering$comp <- data_cleaned$comp

set.seed(4567)

n_clust_scaled_gower <- parameters::n_clusters(data_clustering,
                                         package = c("easystats", "NbClust", "mclust"),
                                         standardize = FALSE,
                                         include_factors = TRUE,
                                         nbclust_method ="pam",
                                         distance_method="gower"
)
View(n_clust_scaled_gower)

n_clust_scaled_hclust_gower <- parameters::n_clusters(data_scaled,
                                                package = c("easystats", "NbClust", "mclust"),
                                                standardize = FALSE,
                                                include_factors = TRUE,
                                                nbclust_method ="hcut",
                                                distance_method = "gower"
)
View(n_clust_scaled_hclust_gower)

# based on this, we can opt for 2 or 3 clusters

# --------------------------------------------#



















