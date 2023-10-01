

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


library(doParallel)


# ---------------------

# First Critique: does distance matter?




data <- read.dta(".//chowdhurry data//Data Archive//ConstructedData//children_familyAggregate_stat12.dta", convert.factors = F)

#Data preparation including NAs

id <- data.frame(data$slno, data$mid)
data <- data.frame(data$patient_choicesOffspringMean, data$patient_choices_father, data$patient_choices_mother, data$binswangerOffspringMean, data$binswanger_father, data$binswanger_mother, data$spitefulOffspringMean, data$spiteful_father, data$spiteful_mother, data$altruisticOffspringMean, data$altruistic_father, data$altruistic_mother, data$egalitarianOffspringMean, data$egalitarian_father, data$egalitarian_mother, data$selfishOffspringMean, data$selfish_father, data$selfish_mother)




id <- id[complete.cases(data), ]
data <- data[complete.cases(data), ] # only pamx works with NAs


#continous variables:
data_cont <-data.frame(data$data.patient_choicesOffspringMean,
                       data$data.patient_choices_father,
                       data$data.patient_choices_mother,
                       data$data.binswangerOffspringMean,
                       data$data.binswanger_father,
                       data$data.binswanger_mother,
                       data$data.spitefulOffspringMean) %>% 
  scale() 

data_cont <- as.data.frame(data_cont)

data_categorical <- data.frame(factor(data$data.spiteful_father),
                               factor(data$data.spiteful_mother),
                               factor(data$data.altruisticOffspringMean),
                               factor(data$data.altruistic_father),
                               factor(data$data.altruistic_mother),
                               factor(data$data.egalitarianOffspringMean),
                               factor(data$data.egalitarian_father),
                               factor(data$data.egalitarian_mother),
                               factor(data$data.selfishOffspringMean),
                               factor(data$data.selfish_father),
                               factor(data$data.selfish_mother))


data_cont$id <- rownames(data_cont)
data_categorical$id <- rownames(data_categorical)

new_data <- merge(data_cont, data_categorical, by = "id") %>% select(-id)


rm(data_cont,data_categorical)
# the column names of the new_data are weird due to reformating: correct them:

for ( col in 1:ncol(new_data)){
  colnames(new_data)[col] <-  sub("factor.data.data.", "", colnames(new_data)[col])
}


for ( col in 1:ncol(new_data)){
  colnames(new_data)[col] <-  sub("data.data.", "", colnames(new_data)[col])
  colnames(new_data)[col] <-  sub("Mean.", "Mean", colnames(new_data)[col])
  colnames(new_data)[col] <-  sub("father.", "father", colnames(new_data)[col])
  colnames(new_data)[col] <-  sub("mother.", "mother", colnames(new_data)[col])
}





#-------

#We will also need the original data to use the scaled dummies:



data <- read.dta(".//chowdhurry data//Data Archive//ConstructedData//children_familyAggregate_stat12.dta", convert.factors = F)

#Data preparation including NAs

id <- data.frame(data$slno, data$mid)
data <- data.frame(data$patient_choicesOffspringMean, data$patient_choices_father, data$patient_choices_mother, data$binswangerOffspringMean, data$binswanger_father, data$binswanger_mother, data$spitefulOffspringMean, data$spiteful_father, data$spiteful_mother, data$altruisticOffspringMean, data$altruistic_father, data$altruistic_mother, data$egalitarianOffspringMean, data$egalitarian_father, data$egalitarian_mother, data$selfishOffspringMean, data$selfish_father, data$selfish_mother)

# Data preparation when removing NAs

  id <- id[complete.cases(data), ]
  data <- data[complete.cases(data), ] # only pamx works with NAs


data <- scale(data)
mydata <- data
rm(data)






dist <- daisy(new_data, metric = "gower")

#----------
#
# 01: lets accept the scaling of dummy variables, and use the original "mydata"
# and let us also accept that k-medoid is an adequate measurement
#Check whether various distance measures matter
#
#----------

cores <- detectCores()-1
doParallel::registerDoParallel(cores = cores)


# Distance measure 1: euclidean
#silhouette:
silhouette_euc <- fviz_nbclust(mydata, FUN = pam, method = "silhouette")
#elbow method:
elbow_euc <- fviz_nbclust(mydata, FUN = pam, method = "wss")

#gap stat:
set.seed(123)
gap_stat_euc<- clusGap(mydata, FUN = pam, diss=TRUE, nstart = 25, K.max = 15, 
                         B = 50) 
fviz_gap_stat(gap_stat_euc)


# Distance measure 2: Gower


silhouette_gow <- fviz_nbclust(mydata, FUN = pam, method = "silhouette", diss = dist)
#elbow method:
elbow_gow <- fviz_nbclust(mydata, FUN = pam, method = "wss", diss = dist)
#gap stat:
set.seed(123)
gap_stat_gow <- clusGap(mydata, FUN = pam, diss=dist, nstart = 25, K.max = 15, 
                         B = 50) 
fviz_gap_stat(gap_stat_gow)





########

# Alternative: find the consensus:

library(parameters)

n_clust <- parameters::n_clusters(mydata,
                      package = c("easystats", "NbClust", "mclust"),
                      standardize = FALSE,
                      include_factors = TRUE
)
n_clust

n_clust_factors <- parameters::n_clusters(new_data,
                                          package = c("easystats", "NbClust", "mclust"),
                                          standardize = FALSE,
                                          include_factors = TRUE
)
n_clust_factors





