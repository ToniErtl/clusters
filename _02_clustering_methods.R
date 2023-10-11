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


#continous variables:
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

# Data preparation when removing NAs

id <- id[complete.cases(data), ]
data <- data[complete.cases(data), ] # only pamx works with NAs


data <- scale(data)
mydata <- data
rm(data)


# Define gower-distance
gower_dist <- daisy(new_data, metric = "gower")






#Method 1: recreate their results


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
n_clust

n_clust_factors <- parameters::n_clusters(new_data,
                                          package = c("easystats", "NbClust", "mclust"),
                                          standardize = FALSE,
                                          include_factors = TRUE,
                                          nbclust_method = "pam"
)
n_clust_factors


# n_clust_factors <- parameters::n_clusters(gower_dist,
#                                           package = c("easystats", "NbClust", "mclust"),
#                                           standardize = FALSE,
#                                           include_factors = TRUE,
#                                           nbclust_method = "pam"
# )
# n_clust_factors

# for a number of results, the amount of groups is too large to construct meaningful groups, however, 
# two or three groups are viable.

#orignal_results

pamx2_mydata <- pam(mydata, 2)
pamx3_mydata <- pam(mydata, 3)

#with Gower-distance
pamx2_new_data <- pam(gower_dist, 2)
pamx3_new_data <- pam(gower_dist, 3)


#--------------------------------------------------
# Hierarchical clustering:



#Let us do the same thing with hierarchical clustering;
library(pvclust)

n_clust_factors_hier <- parameters::n_clusters(new_data,
                                          package = c("easystats", "NbClust", "mclust"),
                                          standardize = FALSE,
                                          include_factors = TRUE,
                                          nbclust_method = "hcut",
                                          distance_method = "gower"
)
n_clust_factors_hier


# out of the 7 methods, 3 says 1 cluster, 2 says 2 cluster and 1 says 3 cluster is the optimal.
# again, stay with clust = 2 and clust = 3

dist <- daisy(new_data, metric = "gower")
cls <- hclust(dist)
dendagram<- as.dendrogram(cls)

LAB = rep("", nobs(dendagram))
dendagram = dendextend::set(dendagram, "labels", LAB)

plot(dendextend::color_branches(dendagram, k = 2),
     main="Hierarchical Clustering with Gower-distance", sub ="Using k = 2 based on scree-plot and silhouette method",
     leaflab = "none", horiz = F)

plot(dendextend::color_branches(dendagram, k = 3), 
main="Hierarchical Clustering with Gower-distance", sub ="Using k = 3 based on Gap-method",
leaflab = "none", horiz = F)


hclust_2<- cutree(cls, k = 2)
hclust_3<- cutree(cls, k = 3)



#-------------------------------
#Finally, use k-prototype

# K-prototype is closer to k-medoid, but it uses gower-distance for binary, and eucledian distance for
#continous variables

# unfortunately, k-prototype is not implemented in the previously used packages, 
# we have to test manually

library(clustMixType)

set.seed(13456)

proto_silh <- clustMixType::validation_kproto(method = "silhouette", data = new_data, k = 2:15, verbose = FALSE, nstart = 50) 
proto_gamma <- clustMixType::validation_kproto(method = "gamma", data = new_data, k = 2:15, verbose = FALSE,nstart = 50) 
proto_tau <- clustMixType::validation_kproto(method = "tau", data = new_data, k = 2:15, verbose = FALSE,nstart = 50) 
proto_cindex <- clustMixType::validation_kproto(method = "cindex", data = new_data, k = 2:15, verbose = FALSE,nstart = 50) 
proto_gplus <- clustMixType::validation_kproto(method = "gplus", data = new_data, k = 2:15, verbose = FALSE,nstart = 50) 
proto_dunn <- clustMixType::validation_kproto(method = "dunn", data = new_data, k = 2:15, verbose = FALSE,nstart = 50) 
proto_mcclain <- clustMixType::validation_kproto(method = "mcclain", data = new_data, k = 2:15, verbose = FALSE,nstart = 50) 

proto_silh$index_opt
proto_gamma$index_opt
proto_tau$index_opt
proto_cindex$index_opt
proto_gplus$index_opt
proto_dunn$index_opt
proto_mcclain$index_opt

# 2, 3 and 4 were all adequate with different criteriae

# note: all of the above tests used lambda = 3.17 as a result for optimization


kpro2 <- kproto(new_data, k = 2, method = "gower", nstart = 50, verbose = FALSE)
kpro3 <- kproto(new_data, k = 3, method = "gower", nstart = 50, verbose = FALSE)
kpro4 <- kproto(new_data, k = 4, method = "gower", nstart = 50, verbose = FALSE)




# save the clusters to the original dataset:

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
# Data preparation when removing NAs
id <- id[complete.cases(data), ]
data <- data[complete.cases(data), ]





#ADAT FRAMET ÁTNÉZNI MELYIKRE RAKJAM AZ UMAPOT



clustered_data <- new_data
rm(data, id)

clustered_data$pamx2_mydata <- pamx2_mydata$clustering
clustered_data$pamx3_mydata <- pamx3_mydata$clustering
  
clustered_data$pamx2_newdata <- factor(pamx2_new_data$clustering) # with gower distance
clustered_data$pamx3_newdata <- factor(pamx3_new_data$clustering) #with gower distance

clustered_data$hclust2 <- hclust_2
clustered_data$hclust3 <- hclust_3

clustered_data$kpro2 <- kpro2$cluster
clustered_data$kpro3 <- kpro3$cluster
clustered_data$kpro4 <- kpro4$cluster


#-------------
# Data visualization
#-------------
#-------------------
 

# First, define UMAP dimensions
# We will use these

library(umap)
#convert all factor data into numeric encoding
umap_data <- new_data 
umap_data <- data.frame(lapply(umap_data, as.numeric))

# fit umap
set.seed(12345)
umap_fit <- umap::umap(umap_data)

# get UMAP dimensions into the dataset
umap_dimensions <- umap_fit$layout %>%
  as.data.frame()%>%
  rename(UMAP1="V1",
         UMAP2="V2")

clustered_data$UMAP1 <- umap_dimensions$UMAP1
clustered_data$UMAP2 <- umap_dimensions$UMAP2
rm(umap_fit, umap_dimensions)



# Create ggplots:

hc_graph2 <- clustered_data %>% 
  mutate(hclust_2 = as.factor(hclust_2)) %>% 
  ggplot(aes(UMAP1,UMAP2, col = hclust_2, alpha=0.9))+
  geom_point()+
  theme(legend.position='none')+
  labs(title = "Hierarchical Clustering",
       subtitle = "k=2",
       col = NULL)+
  xlab("UMAP1")+
  ylab("UMAP2")+
  theme_minimal()+
scale_color_colorblind()

hc_graph3 <- clustered_data %>% 
  mutate(hclust_3 = as.factor(hclust_3)) %>% 
  ggplot(aes(UMAP1,UMAP2, col = hclust_3))+
  geom_point()+
  theme(legend.position='none')+
  labs(title = "Hierarchical Clustering",
       subtitle = "k=2",
       col = NULL)+
  xlab("UMAP1")+
  ylab("UMAP2")+
  theme_minimal()+
  scale_color_colorblind()

# Chowdhury method:

medoid_euc2 <- clustered_data %>% 
  mutate(pamx2_mydata = as.factor(pamx2_mydata)) %>% 
  ggplot(aes(UMAP1,UMAP2, col = pamx2_mydata))+
  geom_point()+
  theme(legend.position='none')+
  labs(title = "K-medoid",
       subtitle = "Eucledian distance, k=2",
       col = NULL)+
  xlab("UMAP1")+
  ylab("UMAP2")+
  theme_minimal()+
  scale_color_colorblind()

medoid_euc3 <- clustered_data %>% 
  mutate(pamx3_mydata = as.factor(pamx3_mydata)) %>% 
  ggplot(aes(UMAP1,UMAP2, col = pamx3_mydata))+
  geom_point()+
  theme(legend.position='none')+
  labs(title = "K-medoid",
       subtitle = "Eucledian distance, k=3",
       col = NULL)+
  xlab("UMAP1")+
  ylab("UMAP2")+
  theme_minimal()+
  scale_color_colorblind()

# K-medoid with gower distance

medoid_gower2 <- clustered_data %>% 
  mutate(pamx2_newdata = as.factor(pamx2_newdata)) %>% 
  ggplot(aes(UMAP1,UMAP2, col = factor(pamx2_newdata)))+
  geom_point()+
  theme(legend.position='none')+
  labs(title = "K-medoid",
       subtitle = "Gower distance, k=3",
       col = NULL)+
  xlab("UMAP1")+
  ylab("UMAP2")+
  theme_minimal()+
  scale_color_colorblind()

medoid_gower3 <- clustered_data %>% 
  mutate(pamx3_newdata = as.factor(pamx3_newdata)) %>% 
  ggplot(aes(UMAP1,UMAP2, col = pamx3_newdata))+
  geom_point()+
  theme(legend.position='none')+
  labs(title = "K-medoid",
       subtitle = "Gower distance, k=3",
       col = NULL)+
  xlab("UMAP1")+
  ylab("UMAP2")+
  theme_minimal()+
  scale_color_colorblind()



# K-prototype

kpro2_graph <- clustered_data %>% 
  mutate(kpro2 = as.factor(kpro2)) %>% 
  ggplot(aes(UMAP1,UMAP2, col = kpro2))+
  geom_point()+
  theme(legend.position='none')+
  labs(title = "K-prototype",
       subtitle = "k=2",
       col = NULL)+
  xlab("UMAP1")+
  ylab("UMAP2")+
  theme_minimal()+
  scale_color_colorblind()

kpro3_graph <- clustered_data %>% 
  mutate(kpro3 = as.factor(kpro3)) %>% 
  ggplot(aes(UMAP1,UMAP2, col = kpro3))+
  geom_point()+
  theme(legend.position='none')+
  labs(title = "K-prototype",
       subtitle = "k=3",
       col = NULL)+
  xlab("UMAP1")+
  ylab("UMAP2")+
  theme_minimal()+
  scale_color_colorblind()

kpro4_graph <- clustered_data %>% 
  mutate(kpro4 = as.factor(kpro4)) %>% 
  ggplot(aes(UMAP1,UMAP2, col = kpro4))+
  geom_point()+
  theme(legend.position='none')+
  labs(title = "K-prototype",
       subtitle = "k=4",
       col = NULL)+
  xlab("UMAP1")+
  ylab("UMAP2")+
  theme_minimal()+
  scale_color_colorblind()





ggsave("./comment_clustering_plots/all_clusters.pdf",ggpubr::ggarrange(hc_graph2, medoid_euc2, medoid_gower2,  kpro2_graph,
                  hc_graph3, medoid_euc3, medoid_gower3,  kpro3_graph ,ncol=4, nrow = 2), width = 12, height = 8)


ggsave("./comment_clustering_plots/kproto_clusters.pdf",ggpubr::ggarrange(kpro2_graph,kpro3_graph,kpro4_graph, ncol =3),
       width = 10, height = 4)


# new_data %>% select(patient_choicesOffspringMean,patient_choices_father,patient_choices_mother,
#                             binswangerOffspringMean,binswanger_father,binswanger_mother) %>% 
#   GGally::ggpairs()







# finally: datatable:

crosstable_kpro2 <- clustered_data %>% 
  select(-pamx2_mydata,
         -pamx3_mydata,
         -pamx2_newdata,
         -pamx3_newdata,
         -hclust2,
         -hclust3,
         -kpro3,
         -kpro4,
         -UMAP1,
         -UMAP2)

crosstable_kpro3 <- clustered_data %>% 
  select(-pamx2_mydata,
         -pamx3_mydata,
         -pamx2_newdata,
         -pamx3_newdata,
         -hclust2,
         -hclust3,
         -kpro2,
         -kpro4,
         -UMAP1,
         -UMAP2)



for ( col in 1:ncol(crosstable_kpro2)){
  colnames(crosstable_kpro2)[col] <-  sub("factor.data.", "", colnames(crosstable_kpro2)[col])
}


for ( col in 1:ncol(crosstable_kpro2)){
  colnames(crosstable_kpro2)[col] <-  sub("data.", "", colnames(crosstable_kpro2)[col])
  colnames(crosstable_kpro2)[col] <-  sub("Mean.", "Mean", colnames(crosstable_kpro2)[col])
  colnames(crosstable_kpro2)[col] <-  sub("father.", "father", colnames(crosstable_kpro2)[col])
  colnames(crosstable_kpro2)[col] <-  sub("mother.", "mother", colnames(crosstable_kpro2)[col])
}

for ( col in 1:ncol(crosstable_kpro3)){
  colnames(crosstable_kpro3)[col] <-  sub("factor.data.", "", colnames(crosstable_kpro3)[col])
}


for ( col in 1:ncol(crosstable_kpro2)){
  colnames(crosstable_kpro3)[col] <-  sub("data.", "", colnames(crosstable_kpro3)[col])
  colnames(crosstable_kpro3)[col] <-  sub("Mean.", "Mean", colnames(crosstable_kpro3)[col])
  colnames(crosstable_kpro3)[col] <-  sub("father.", "father", colnames(crosstable_kpro3)[col])
  colnames(crosstable_kpro3)[col] <-  sub("mother.", "mother", colnames(crosstable_kpro3)[col])
}



#stargazer::stargazer(summary(arsenal::tableby(kpro2 ~ ., stat= c("mean"), data = crosstable_kpro2)), text = NULL, row.names = FALSE)


summary((arsenal::tableby(kpro2 ~ ., stat= c("mean"), data = crosstable_kpro2, cat.test = "chisq", total = FALSE)), 
        text = TRUE, latex = TRUE)



summary((arsenal::tableby(kpro3 ~ ., stat= c("mean"), data = crosstable_kpro3, cat.test = "chisq", total = FALSE)), 
        text = TRUE, latex = TRUE)


summary((arsenal::tableby(kpro2 ~ ., stat= c("mean"), data = crosstable_kpro2, cat.test = "chisq", total = FALSE)), 
        text = "latex", latex = TRUE)


summary((arsenal::tableby(kpro3 ~ ., stat= c("mean"), data = crosstable_kpro3, cat.test = "chisq", total = FALSE)), 
        text = "latex", latex = TRUE)


