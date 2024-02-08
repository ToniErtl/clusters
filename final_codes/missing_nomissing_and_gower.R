#Missing + missing_gower + nomissing
rm(list=ls())
# First to parts can be found in the _03_table1_missing_nomissing.R file

# Finally: do the table with missing values, using Gower-distance



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





#Recreate their results:
data <- read.dta(".//chowdhurry data//Data Archive//ConstructedData//children_familyAggregate_stat12.dta",
                 convert.factors = F)

data <- data.frame(data$patient_choicesOffspringMean, data$patient_choices_father, 
                   data$patient_choices_mother, data$binswangerOffspringMean, 
                   data$binswanger_father, data$binswanger_mother, data$spitefulOffspringMean,
                   data$spiteful_father, data$spiteful_mother, data$altruisticOffspringMean, 
                   data$altruistic_father, data$altruistic_mother, data$egalitarianOffspringMean, 
                   data$egalitarian_father, data$egalitarian_mother, data$selfishOffspringMean, 
                   data$selfish_father, data$selfish_mother)



data_cont <-data.frame(data$data.patient_choicesOffspringMean,
                       data$data.patient_choices_father,
                       data$data.patient_choices_mother,
                       data$data.binswangerOffspringMean,
                       data$data.binswanger_father,
                       data$data.binswanger_mother
) %>% 
  scale() 

data_cont <- as.data.frame(data_cont)

data_categorical <- data.frame(factor(data$data.spitefulOffspringMean),
                               factor(data$data.spiteful_father),
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

# 2 clusters are optimal according to average silhouette width

gower_dist <- daisy(new_data, metric = "gower")


require(cluster)
set.seed(12345)
pamx2_missing <- pam(gower_dist, 2)
set.seed(12345)
pamx3_missing <- pam(gower_dist, 3)
set.seed(12345)
pamx4_missing <- pam(gower_dist, 4)



data <- read.dta(".//chowdhurry data//Data Archive//ConstructedData//children_familyAggregate_stat12.dta",
                 convert.factors = F)

data <- data.frame(data$patient_choicesOffspringMean, data$patient_choices_father, 
                   data$patient_choices_mother, data$binswangerOffspringMean, 
                   data$binswanger_father, data$binswanger_mother, data$spitefulOffspringMean,
                   data$spiteful_father, data$spiteful_mother, data$altruisticOffspringMean, 
                   data$altruistic_father, data$altruistic_mother, data$egalitarianOffspringMean, 
                   data$egalitarian_father, data$egalitarian_mother, data$selfishOffspringMean, 
                   data$selfish_father, data$selfish_mother)

data <- data.frame(data)
# data_cont <-data.frame(data$data.patient_choicesOffspringMean,
#                        data$data.patient_choices_father,
#                        data$data.patient_choices_mother,
#                        data$data.binswangerOffspringMean,
#                        data$data.binswanger_father,
#                        data$data.binswanger_mother
# )
# 
# data_cont <- as.data.frame(data_cont)
# 
# data_categorical <- data.frame(factor(data$data.spitefulOffspringMean),
#                                factor(data$data.spiteful_father),
#                                factor(data$data.spiteful_mother),
#                                factor(data$data.altruisticOffspringMean),
#                                factor(data$data.altruistic_father),
#                                factor(data$data.altruistic_mother),
#                                factor(data$data.egalitarianOffspringMean),
#                                factor(data$data.egalitarian_father),
#                                factor(data$data.egalitarian_mother),
#                                factor(data$data.selfishOffspringMean),
#                                factor(data$data.selfish_father),
#                                factor(data$data.selfish_mother))
# 
# 
# data_cont$id <- rownames(data_cont)
# data_categorical$id <- rownames(data_categorical)
# 
# data <- merge(data_cont, data_categorical, by = "id") %>% select(-id)
# rm(data_cont,data_categorical)

data$gower2 <- as.numeric(pamx2_missing$clustering)


# For the PAM calculated in the original paper:

for ( col in 1:ncol(data)){
  colnames(data)[col] <-  sub("factor.data.", "", colnames(data)[col])
  colnames(data)[col] <-  sub("data.", "", colnames(data)[col])
  colnames(data)[col] <-  sub("Mean.", "Mean", colnames(data)[col])
  colnames(data)[col] <-  sub("father.", "father", colnames(data)[col])
  colnames(data)[col] <-  sub("mother.", "mother", colnames(data)[col])
}
summary((arsenal::tableby(gower2 ~ ., stat= c("mean"), data = data, cat.test = "chisq", total = FALSE)), 
        text = TRUE, latex = TRUE)

data <- data %>% select(-gower2)
data$gower3 <- as.numeric(pamx3_missing$clustering)
summary((arsenal::tableby(gower3 ~ ., stat= c("mean"), data = data, cat.test = "chisq", total = FALSE)), 
        text = TRUE, latex = TRUE)




data <- data %>% select(-gower3)
data$gower4 <- as.numeric(pamx4_missing$clustering)
summary((arsenal::tableby(gower4 ~ ., stat= c("mean"), data = data, cat.test = "chisq", total = FALSE)), 
        text = TRUE, latex = TRUE)



