# - To run this, you need to also run "_02_clustering_methods.R", as some of
# the comparisons require results from that syntax.


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




data <- scale(data)
# 2 clusters are optimal according to average silhouette width

require(cluster)
pamx_original <- pam(data, 2)

data <- data.frame(data)
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

data <- merge(data_cont, data_categorical, by = "id") %>% select(-id)
rm(data_cont,data_categorical)







data$kmed2_orig <- as.numeric(pamx_original$clustering)





# PAM 2 - compare missing and non-missing results


crosstable_pamx2_new<- clustered_data %>% 
  select(#-pamx2_mydata,
         -pamx3_mydata,
         -pamx2_newdata,
         -pamx3_newdata,
         -hclust2,
         -hclust3,
         -kpro2,
         -kpro3,
         -kpro4,
         -UMAP1,
         -UMAP2)


for ( col in 1:ncol(crosstable_pamx2_new)){
  colnames(crosstable_pamx2_new)[col] <-  sub("factor.data.", "", colnames(crosstable_pamx2_new)[col])
}


for ( col in 1:ncol(crosstable_pamx2_new)){
  colnames(crosstable_pamx2_new)[col] <-  sub("data.", "", colnames(crosstable_pamx2_new)[col])
  colnames(crosstable_pamx2_new)[col] <-  sub("Mean.", "Mean", colnames(crosstable_pamx2_new)[col])
  colnames(crosstable_pamx2_new)[col] <-  sub("father.", "father", colnames(crosstable_pamx2_new)[col])
  colnames(crosstable_pamx2_new)[col] <-  sub("mother.", "mother", colnames(crosstable_pamx2_new)[col])
}

summary((arsenal::tableby(pamx2_mydata ~ ., stat= c("mean"), data = crosstable_pamx2_new, cat.test = "chisq", total = FALSE)), 
        text = TRUE, latex = TRUE)


# For the PAM calculated in this syntax:

for ( col in 1:ncol(data)){
  colnames(data)[col] <-  sub("factor.data.", "", colnames(data)[col])
}


for ( col in 1:ncol(data)){
  colnames(data)[col] <-  sub("data.", "", colnames(data)[col])
  colnames(data)[col] <-  sub("Mean.", "Mean", colnames(data)[col])
  colnames(data)[col] <-  sub("father.", "father", colnames(data)[col])
  colnames(data)[col] <-  sub("mother.", "mother", colnames(data)[col])
}

data$spitefulOffspringMean = factor(data$spitefulOffspringMean)

summary((arsenal::tableby(kmed2_orig ~ ., stat= c("mean"), data = data, cat.test = "chisq", total = FALSE)), 
        text = TRUE, latex = TRUE)







# UMAP cannot be used for missing data








# Additional tables:

table_tech<-  clustered_data %>% 
  select(-pamx2_mydata,
    -pamx3_mydata,
    -pamx2_newdata,
    -pamx3_newdata,
    -hclust2,
    #-hclust3,
    -kpro2,
    -kpro3,
    -kpro4,
    -UMAP1,
    -UMAP2) 



summary((arsenal::tableby(hclust3 ~ ., data=table_tech, stat= c("mean"), cat.test = "chisq", total = FALSE)), 
          text = TRUE, latex = TRUE)


table_tech<-  clustered_data %>% 
  select(-pamx2_mydata,
         -pamx3_mydata,
         -pamx2_newdata,
         -pamx3_newdata,
         -hclust2,
         -hclust3,
         #-kpro2,
         -kpro3,
         -kpro4,
         -UMAP1,
         -UMAP2) 
summary((arsenal::tableby(kpro2 ~ ., data=table_tech, stat= c("mean"), cat.test = "chisq", total = FALSE)), 
        text = TRUE, latex = TRUE)



table_tech<-  clustered_data %>% 
  select(-pamx2_mydata,
         -pamx3_mydata,
         -pamx2_newdata,
         -pamx3_newdata,
         -hclust2,
         -hclust3,
         -kpro2,
         -kpro3,
         #-kpro4,
         -UMAP1,
         -UMAP2) 



summary((arsenal::tableby(kpro4 ~ ., data=table_tech, stat= c("mean"), cat.test = "chisq", total = FALSE)), 
        text = TRUE, latex = TRUE)





