
#clear all environment
rm(list=ls())
# Read libraries


library(foreign)
library(tidyverse)
library(ggplot2)
library(ggthemes)
library(factoextra)
library(crosstable) # crosstable function

# Clustering methods:
library(cluster)

#Register parallel computing:
cores <- parallel::detectCores()-1
doParallel::registerDoParallel(cores = cores)

# ---------------------
#Second critique: how robust are the results to various clustering methods?


# Load packages

library(foreign)


library(tidyverse)
#library(tidymodels)
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



#Register parallel computing:

cores <- parallel::detectCores()-1
doParallel::registerDoParallel(cores = cores)

# ---------------------

#----- First do the original clustering excercise with k = 2 and k =.3


data <- read.dta(".//chowdhurry data//Data Archive//ConstructedData//children_familyAggregate_stat12.dta",
                 convert.factors = F)

#Data preparation including NAs

id <- data.frame(data$slno, data$mid)
data <- data.frame(data$patient_choicesOffspringMean, data$patient_choices_father, data$patient_choices_mother,
                   data$binswangerOffspringMean, data$binswanger_father, data$binswanger_mother,
                   data$spitefulOffspringMean, data$spiteful_father, data$spiteful_mother,
                   data$altruisticOffspringMean, data$altruistic_father, data$altruistic_mother,
                   data$egalitarianOffspringMean, data$egalitarian_father, data$egalitarian_mother, 
                   data$selfishOffspringMean, data$selfish_father, data$selfish_mother)

id <- id[complete.cases(data), ]
data <- data[complete.cases(data), ] # only pamx works with NAs

mydata <- data %>% 
  scale()

#continous variables:
data_cont <-data.frame(data$data.patient_choicesOffspringMean,
                       data$data.patient_choices_father,
                       data$data.patient_choices_mother,
                       data$data.binswangerOffspringMean,
                       data$data.binswanger_father,
                       data$data.binswanger_mother
) 
#%>% 
#  scale() 

data_cont <- as.data.frame(data_cont)

data_categorical <- data.frame((data$data.spitefulOffspringMean),
                               (data$data.spiteful_father),
                               (data$data.spiteful_mother),
                               (data$data.altruisticOffspringMean),
                               (data$data.altruistic_father),
                               (data$data.altruistic_mother),
                               (data$data.egalitarianOffspringMean),
                               (data$data.egalitarian_father),
                               (data$data.egalitarian_mother),
                               (data$data.selfishOffspringMean),
                               (data$data.selfish_father),
                               (data$data.selfish_mother))


data_cont$id <- rownames(data_cont)
data_categorical$id <- rownames(data_categorical)

new_data <- merge(data_cont, data_categorical, by = "id") %>% select(-id) %>% 
  scale() %>% data.frame() %>% 
  dplyr::filter(!is.na(data.data.binswangerOffspringMean))


# Define gower-distance
gower_dist <- daisy(new_data, metric = "gower")




# Comment: in their original paper, they used k = 2. 
# we can check the consensus on the ideal number of k with this function:

library(parameters)
set.seed(123)

n_clust <- parameters::n_clusters(mydata,
                                  package = c("easystats", "NbClust", "mclust"),
                                  standardize = FALSE,
                                  include_factors = TRUE,
                                  nbclust_method ="pam"
)
View(n_clust)

n_clust_factors <- parameters::n_clusters(new_data,
                                          package = c("easystats", "NbClust", "mclust"),
                                          standardize = FALSE,
                                          include_factors = TRUE,
                                          nbclust_method = "pam"
)
View(n_clust_factors)



