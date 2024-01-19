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




# Run code _02_clustering_methods for the clustering

source("./_02_clustering_methods.R")

#Separate data to continous and categorical variables

#--------------
# IMPORTANT!!!!
#--------------
#here, I removed the "scale" command, so we will get the non-standardized values 
#for these tables 

data <- read.dta(".//chowdhurry data//Data Archive//ConstructedData//children_familyAggregate_stat12.dta",
                 convert.factors = F)

id <- data.frame(data$slno, data$mid)
data <- data.frame(data$patient_choicesOffspringMean, data$patient_choices_father, 
                   data$patient_choices_mother, data$binswangerOffspringMean, 
                   data$binswanger_father, data$binswanger_mother, data$spitefulOffspringMean,
                   data$spiteful_father, data$spiteful_mother, data$altruisticOffspringMean, 
                   data$altruistic_father, data$altruistic_mother, data$egalitarianOffspringMean, 
                   data$egalitarian_father, data$egalitarian_mother, data$selfishOffspringMean, 
                   data$selfish_father, data$selfish_mother)

data <- data[complete.cases(data), ] # only pamx works with NAs


#continous variables:
data_cont <-data.frame(data$data.patient_choicesOffspringMean,
                       data$data.patient_choices_father,
                       data$data.patient_choices_mother,
                       data$data.binswangerOffspringMean,
                       data$data.binswanger_father,
                       data$data.binswanger_mother
) 

# HERE, I REMOVED THE SCALING!
#%>% 
#  scale() 

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
rm(data)
data <- merge(data_cont, data_categorical, by = "id") %>% select(-id)
rm(data_cont,data_categorical)



# add our version of many specifications found in _02_clustering_methods.R
data$pamx2_mydata <- clustered_data$pamx2_mydata
data$pamx3_mydata <- clustered_data$pamx3_mydata  
data$pamx2_newdata <- clustered_data$pamx2_newdata
data$pamx3_newdata <- clustered_data$pamx3_newdata
data$hclust2 <- clustered_data$hclust2
data$hclust3 <- clustered_data$hclust3
data$kpro2 <- clustered_data$kpro2
data$kpro3 <- clustered_data$kpro3
data$kpro4 <- clustered_data$kpro4
data$UMAP1 <- clustered_data$UMAP1
data$UMAP2 <- clustered_data$UMAP2

#dealing with some text issues...:

for ( col in 1:ncol(data)){
  colnames(data)[col] <-  sub("factor.data.", "", colnames(data)[col])
}


for ( col in 1:ncol(data)){
  colnames(data)[col] <-  sub("data.", "", colnames(data)[col])
  colnames(data)[col] <-  sub("Mean.", "Mean", colnames(data)[col])
  colnames(data)[col] <-  sub("father.", "father", colnames(data)[col])
  colnames(data)[col] <-  sub("mother.", "mother", colnames(data)[col])
}



# PAM 2 - compare missing and non-missing results
crosstable_pamx2_new<- data %>% 
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



#---------
#TABLE 2 : part1
#---------

summary((arsenal::tableby(pamx2_mydata ~ ., stat= c("mean"), data = crosstable_pamx2_new, cat.test = "chisq", total = FALSE)), 
        text = TRUE, latex = TRUE)


# For the PAM calculated in this syntax:


data$spitefulOffspringMean = factor(data$spitefulOffspringMean)
#---------
#TABLE 2 : part2
#---------
table_tech<- data %>% 
  select(-pamx2_mydata,
    -pamx3_mydata,
    #-pamx2_newdata,
    -pamx3_newdata,
    -hclust2,
    -hclust3,
    -kpro2,
    -kpro3,
    -kpro4,
    -UMAP1,
    -UMAP2)
    #-kmed2_orig)


summary((arsenal::tableby(pamx2_newdata ~ ., stat= c("mean"), data = table_tech, cat.test = "chisq", total = FALSE)), 
        text = TRUE, latex = TRUE)


# -------------------------#


# Additional tables:


#-----------
#Table 4
#----------- 



table_tech<-  data %>% 
  select(-pamx2_mydata,
         -pamx3_mydata,
         -pamx2_newdata,
         -pamx3_newdata,
         #-hclust2,
         -hclust3,
         -kpro2,
         -kpro3,
         -kpro4,
         -UMAP1,
         -UMAP2) 

#Table 4 : part 1
summary((arsenal::tableby(hclust2 ~ ., data=table_tech, stat= c("mean"), cat.test = "chisq", total = FALSE)), 
        text = TRUE, latex = TRUE)




table_tech<-  data %>% 
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

#Table 4 : part 2
summary((arsenal::tableby(hclust3 ~ ., data=table_tech, stat= c("mean"), cat.test = "chisq", total = FALSE)), 
        text = TRUE, latex = TRUE)

#--------------#
#TABLE 5
#--------------#




table_tech<-  data %>% 
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
#table 5: part 1
summary((arsenal::tableby(kpro2 ~ ., data=table_tech, stat= c("mean"), cat.test = "chisq", total = FALSE)), 
        text = TRUE, latex = TRUE)


table_tech<-  data %>% 
  select(-pamx2_mydata,
         -pamx3_mydata,
         -pamx2_newdata,
         -pamx3_newdata,
         -hclust2,
         -hclust3,
         -kpro2,
         #-kpro3,
         -kpro4,
         -UMAP1,
         -UMAP2)
#table 5: part 1
summary((arsenal::tableby(kpro3 ~ ., data=table_tech, stat= c("mean"), cat.test = "chisq", total = FALSE)), 
        text = TRUE, latex = TRUE)





#-------------
#Table 6
#------------


table_tech<-  data %>% 
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
#table 6: part 1
summary((arsenal::tableby(kpro2 ~ ., data=table_tech, stat= c("mean"), cat.test = "chisq", total = FALSE)), 
        text = TRUE, latex = TRUE)

table_tech<-  data %>% 
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
         -UMAP2,)


#table 6: part 2
summary((arsenal::tableby(kpro4 ~ ., data=table_tech, stat= c("mean"), cat.test = "chisq", total = FALSE)), 
        text = TRUE, latex = TRUE)


#---------------

# Rand index

library(fossil)

rand_index_results <- matrix(nrow = 8, ncol = 8, NA) %>% as.data.frame()
colnames(rand_index_results) <- c("Hclust2","Kmed_euc2","Kmed2","Kproto2",
                                  "Hclust3","Kmed_euc3","Kmed3","Kproto3")
rownames(rand_index_results) <- c("Hclust2","Kmed_euc2","Kmed2","Kproto2",
                                  "Hclust3","Kmed_euc3","Kmed3","Kproto3")



technical_data <- data %>% 
  select(pamx2_mydata,
         pamx3_mydata,
         pamx2_newdata,
         pamx3_newdata,
         hclust2,
         hclust3,
         kpro2,
         kpro3,
         kpro4) %>% 
  mutate(across(.cols=everything(),.fns=~as.numeric(.x)))


rand_index_results["Hclust2","Hclust2"]<- fossil::rand.index(technical_data$hclust2,technical_data$hclust2)
rand_index_results["Hclust2","Kmed_euc2"]<- fossil::rand.index(technical_data$hclust2,technical_data$pamx2_mydata)
rand_index_results["Hclust2","Kmed2"]<- fossil::rand.index(technical_data$hclust2,technical_data$pamx2_newdata)
rand_index_results["Hclust2","Kproto2"]<- fossil::rand.index(technical_data$hclust2,technical_data$kpro2)

rand_index_results["Kmed_euc2","Hclust2"]<- fossil::rand.index(technical_data$pamx2_mydata,technical_data$hclust2)
rand_index_results["Kmed_euc2","Kmed_euc2"]<- fossil::rand.index(technical_data$pamx2_mydata,technical_data$pamx2_mydata)
rand_index_results["Kmed_euc2","Kmed2"]<- fossil::rand.index(technical_data$pamx2_mydata,technical_data$pamx2_newdata)
rand_index_results["Kmed_euc2","Kproto2"]<- fossil::rand.index(technical_data$pamx2_mydata,technical_data$kpro2)

rand_index_results["Kmed2","Hclust2"]<- fossil::rand.index(technical_data$pamx2_newdata,technical_data$hclust2)
rand_index_results["Kmed2","Kmed_euc2"]<- fossil::rand.index(technical_data$pamx2_newdata,technical_data$pamx2_mydata)
rand_index_results["Kmed2","Kmed2"]<- fossil::rand.index(technical_data$pamx2_newdata,technical_data$pamx2_newdata)
rand_index_results["Kmed2","Kproto2"]<- fossil::rand.index(technical_data$pamx2_newdata,technical_data$kpro2)

rand_index_results["Kproto2","Hclust2"]<- fossil::rand.index(technical_data$kpro2,technical_data$hclust2)
rand_index_results["Kproto2","Kmed_euc2"]<- fossil::rand.index(technical_data$kpro2,technical_data$pamx2_mydata)
rand_index_results["Kproto2","Kmed2"]<- fossil::rand.index(technical_data$kpro2,technical_data$pamx2_newdata)
rand_index_results["Kproto2","Kproto2"]<- fossil::rand.index(technical_data$kpro2,technical_data$kpro2)

rand_index_results["Hclust3","Hclust3"]<- fossil::rand.index(technical_data$hclust3,technical_data$hclust3)
rand_index_results["Hclust3","Kmed_euc3"]<- fossil::rand.index(technical_data$hclust3,technical_data$pamx3_mydata)
rand_index_results["Hclust3","Kmed3"]<- fossil::rand.index(technical_data$hclust3,technical_data$pamx3_newdata)
rand_index_results["Hclust3","Kproto3"]<- fossil::rand.index(technical_data$hclust3,technical_data$kpro3)

rand_index_results["Kmed_euc3","Hclust3"]<- fossil::rand.index(technical_data$pamx3_mydata,technical_data$hclust3)
rand_index_results["Kmed_euc3","Kmed_euc3"]<- fossil::rand.index(technical_data$pamx3_mydata,technical_data$pamx3_mydata)
rand_index_results["Kmed_euc3","Kmed3"]<- fossil::rand.index(technical_data$pamx3_mydata,technical_data$pamx3_newdata)
rand_index_results["Kmed_euc3","Kproto3"]<- fossil::rand.index(technical_data$pamx3_mydata,technical_data$kpro3)

rand_index_results["Kmed3","Hclust3"]<- fossil::rand.index(technical_data$pamx3_newdata,technical_data$hclust3)
rand_index_results["Kmed3","Kmed_euc3"]<- fossil::rand.index(technical_data$pamx3_newdata,technical_data$pamx3_mydata)
rand_index_results["Kmed3","Kmed3"]<- fossil::rand.index(technical_data$pamx3_newdata,technical_data$pamx3_newdata)
rand_index_results["Kmed3","Kproto3"]<- fossil::rand.index(technical_data$pamx3_newdata,technical_data$kpro3)

rand_index_results["Kproto3","Hclust3"]<- fossil::rand.index(technical_data$kpro3,technical_data$hclust3)
rand_index_results["Kproto3","Kmed_euc3"]<- fossil::rand.index(technical_data$kpro3,technical_data$pamx3_mydata)
rand_index_results["Kproto3","Kmed3"]<- fossil::rand.index(technical_data$kpro3,technical_data$pamx3_newdata)
rand_index_results["Kproto3","Kproto3"]<- fossil::rand.index(technical_data$kpro3,technical_data$kpro3)

# get latex output:
stargazer::stargazer(as.matrix(rand_index_results), digits = 2)

















