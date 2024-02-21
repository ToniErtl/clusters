# Load packages
library(foreign)

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
#Register parallel computing:

cores <- parallel::detectCores()-1
doParallel::registerDoParallel(cores = cores)

# ---------------------


#Based on _02_clustering.methods.R, we will separately make the PAM clustering.
# slight difference: the data preparation is the same code for both cases, but 
# for the first part, I do not do the refactoring of the variables and will
# rescale all variables

#-----




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

old_data <- merge(data_cont, data_categorical, by = "id") %>% select(-id) %>% 
  scale() %>% data.frame() %>% 
  dplyr::filter(!is.na(data.data.binswangerOffspringMean))


rm(data_cont,data_categorical)

# 
# 
# data <- scale(data) %>% data.frame()
# 
# data <- data %>% dplyr::filter(!is.na(data.binswangerOffspringMean))

require(cluster)
original_eucl_clusters<- pam(old_data, 2)

#-----------------------------
# GOWER DISTANCE
#-----------------------------


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

# Define gower-distance
gower_dist <- daisy(new_data, metric = "gower")


#with Gower-distance (and using the "correct" variables)
set.seed(12345)
new_gower_clusters <- pam(gower_dist, 2)


figure_dist_table <- new_data

figure_dist_table$original_eucl_clusters <- original_eucl_clusters$clustering
figure_dist_table$new_gower_clusters <- new_gower_clusters$clustering

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

figure_dist_table$UMAP1 <- umap_dimensions$UMAP1
figure_dist_table$UMAP2 <- umap_dimensions$UMAP2
rm(umap_fit, umap_dimensions)



# Create ggplots:

part1 <- figure_dist_table %>% 
  mutate(original_eucl_clusters = as.factor(original_eucl_clusters)) %>% 
  ggplot(aes(UMAP1,UMAP2, col = original_eucl_clusters))+
  geom_point()+
  theme(legend.position='none')+
  labs(title = "K-medoid based on the original method",
       subtitle = "Euclidean distance, k=2",
       col = NULL)+
  xlab("UMAP1")+
  ylab("UMAP2")+
  theme_minimal()+
  scale_color_colorblind()

part2 <- figure_dist_table %>% 
  mutate(new_gower_clusters = as.factor(new_gower_clusters)) %>% 
  ggplot(aes(UMAP1,UMAP2, col = new_gower_clusters))+
  geom_point()+
  theme(legend.position='none')+
  labs(title = "K-medoid with binary variables",
       subtitle = "Gower distance, k=2",
       col = NULL)+
  xlab("UMAP1")+
  ylab("UMAP2")+
  theme_minimal()+
  scale_color_colorblind()


# ggsave("./comment_clustering_plots/orig_eucl_vs_gower.pdf",ggpubr::ggarrange(part1,part2,
#                                                                         ncol=2), width = 8, height = 4)





