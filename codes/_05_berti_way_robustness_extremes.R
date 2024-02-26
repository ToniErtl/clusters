
#Run the first clustering:

source("./codes/_02_clustering_methods.R")

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
data <- data[complete.cases(data), ]

for ( col in 1:ncol(data)){
  colnames(data)[col] <-  sub("factor.data.data.", "", colnames(data)[col])
  colnames(data)[col] <-  sub("data.data.", "", colnames(data)[col])
  colnames(data)[col] <-  sub("data.", "", colnames(data)[col])
  colnames(data)[col] <-  sub("Mean.", "Mean", colnames(data)[col])
  colnames(data)[col] <-  sub("father.", "father", colnames(data)[col])
  colnames(data)[col] <-  sub("mother.", "mother", colnames(data)[col])
}



data_allfactors <- data %>% 
  mutate(patient_choicesOffspringMean1=case_when(patient_choicesOffspringMean==min(data$patient_choicesOffspringMean)~1,
                                                 TRUE~0),
         patient_choicesOffspringMean2=case_when(patient_choicesOffspringMean==max(data$patient_choicesOffspringMean)~1,
                                                 TRUE~0), 
         patient_choicesOffspringMean3=case_when(patient_choicesOffspringMean1==1 | patient_choicesOffspringMean2 == 1~0,
                                                 TRUE~1),
         #father:
         patient_choices_father1=case_when(patient_choices_father==min(data$patient_choices_father)~1,
                                                 TRUE~0),
         patient_choices_father2=case_when(patient_choices_father==max(data$patient_choices_father)~1,
                                                 TRUE~0),
         patient_choices_father3=case_when(patient_choices_father1==1 | patient_choices_father2 == 1~0,
                                                 TRUE~1),
         #mother:
         patient_choices_mother1=case_when(patient_choices_mother==min(data$patient_choices_mother)~1,
                                                 TRUE~0),
         patient_choices_mother2=case_when(patient_choices_mother==max(data$patient_choices_mother)~1,
                                                 TRUE~0),
         patient_choices_mother3=case_when(patient_choices_mother1==1 | patient_choices_mother2 == 1~0,
                                                 TRUE~1),
         
         #risk preferences:
        
         binswangerOffspringMean1=case_when(binswangerOffspringMean>0 & binswangerOffspringMean<=2~1,
                                                 TRUE~0),
         binswangerOffspringMean2=case_when(binswangerOffspringMean>2 & binswangerOffspringMean<=4~1,
                                                 TRUE~0),
         binswangerOffspringMean3=case_when(binswangerOffspringMean>4 & binswangerOffspringMean<=6~1,
                                            TRUE~0),
         
         binswanger_father1=case_when(binswanger_father>0 & binswanger_father<=2~1,
                                            TRUE~0),
         binswanger_father2=case_when(binswanger_father>2 & binswanger_father<=4~1,
                                            TRUE~0),
         binswanger_father3=case_when(binswanger_father>4 & binswanger_father<=6~1,
                                            TRUE~0),
         
         
         binswanger_mother1=case_when(binswanger_mother>0 & binswanger_mother<=2~1,
                                            TRUE~0),
         binswanger_mother2=case_when(binswanger_mother>2 & binswanger_mother<=4~1,
                                            TRUE~0),
         binswanger_mother3=case_when(binswanger_mother>4 & binswanger_mother<=6~1,
                                            TRUE~0)
  ) %>%
  # get rid of the continuous variables:
  select(-patient_choicesOffspringMean,
           -patient_choices_father,
           -patient_choices_mother,
           -binswangerOffspringMean,
           -binswanger_father,
           -binswanger_mother)

# make factors from all variables

data_allfactors <- data.frame(lapply(data_allfactors, as.factor))




# Define gower-distance
gower_dist <- daisy(data_allfactors, metric = "gower")


# Check the number of clusters:


library(parameters)
set.seed(123)
factor_n_clusters <- parameters::n_clusters(data_allfactors,
                                            package = c("easystats", "NbClust", "mclust"),
                                            standardize = FALSE,
                                            include_factors = TRUE,
                                            nbclust_method = "pam"
)
factor_n_clusters


#------------------- Do the clustering:

#with Gower-distance (and using the "correct" variables)
set.seed(12345)
pamx2_allfactors <- pam(gower_dist, 2)
pamx3_allfactors <- pam(gower_dist, 3)
pamx4_allfactors <- pam(gower_dist, 4)



#Gower distance with hclust
cls <- hclust(dist)
hclust_2_gower_allfactor<- cutree(cls, k = 2)
hclust_3_gower_allfactor<- cutree(cls, k = 3)

# get results from code "02":




clustered_data$pamx2_allfactors <- factor(pamx2_allfactors$clustering) 
clustered_data$pamx3_allfactors <- factor(pamx3_allfactors$clustering) 
clustered_data$pamx4_allfactors <- factor(pamx4_allfactors$clustering)

clustered_data$hclust_2_gower_allfactor <- as.factor(hclust_2_gower_allfactor) 
clustered_data$hclust_3_gower_allfactor <- as.factor(hclust_3_gower_allfactor)
#-------------
# Data visualization
#-------------
#-------------------


# Create ggplots:

# add this all_factor method:

medoid_allfactors2 <- clustered_data %>%
  ggplot(aes(UMAP1,UMAP2, col = pamx2_allfactors))+
  geom_point()+
  theme(legend.position='none')+
  labs(title = "K-medoid using only factors",
       subtitle = "Gower distance, k=2",
       col = NULL)+
  xlab("UMAP1")+
  ylab("UMAP2")+
  theme_minimal()+
  scale_color_colorblind()

medoid_allfactors3 <- clustered_data %>%
  ggplot(aes(UMAP1,UMAP2, col = pamx3_allfactors))+
  geom_point()+
  theme(legend.position='none')+
  labs(title = "K-medoid using only factors",
       subtitle = "Gower distance, k=3",
       col = NULL)+
  xlab("UMAP1")+
  ylab("UMAP2")+
  theme_minimal()+
  scale_color_colorblind()

medoid_allfactors4 <- clustered_data %>%
  ggplot(aes(UMAP1,UMAP2, col = pamx4_allfactors))+
  geom_point()+
  theme(legend.position='none')+
  labs(title = "K-medoid using only factors",
       subtitle = "Gower distance, k=4",
       col = NULL)+
  xlab("UMAP1")+
  ylab("UMAP2")+
  theme_minimal()+
  scale_color_colorblind()

ggpubr::ggarrange(medoid_allfactors2,medoid_allfactors3,medoid_allfactors4,
                  ncol =3, nrow= 1)

# new_data %>% select(patient_choicesOffspringMean,patient_choices_father,patient_choices_mother,
#                             binswangerOffspringMean,binswanger_father,binswanger_mother) %>%
#   GGally::ggpairs()



#these for hclusts

hier_factors2 <- clustered_data %>%
  ggplot(aes(UMAP1,UMAP2, col = hclust_2_gower_allfactor))+
  geom_point()+
  theme(legend.position='none')+
  labs(title = "Hierarchical clustering \nusing only factors",
       subtitle = "Gower distance, k=2",
       col = NULL)+
  xlab("UMAP1")+
  ylab("UMAP2")+
  theme_minimal()+
  scale_color_colorblind()

hier_factors3 <- clustered_data %>%
  ggplot(aes(UMAP1,UMAP2, col = hclust_3_gower_allfactor))+
  geom_point()+
  theme(legend.position='none')+
  labs(title = "Hierarchical clustering \nusing only factors",
       subtitle = "Gower distance, k=3",
       col = NULL)+
  xlab("UMAP1")+
  ylab("UMAP2")+
  theme_minimal()+
  scale_color_colorblind()

ggpubr::ggarrange(hier_factors2,hier_factors3,
                  ncol =2, nrow= 1)


# finally: datatable:

medoid_allfactors2_tab <- clustered_data %>%
  select(-pamx2_mydata,
         -pamx3_mydata,
         -pamx2_newdata,
         -pamx3_newdata,
         -hclust2,
         -hclust3,
         -kpro2,
         -kpro3,
         -kpro4,
         -UMAP1,
         -UMAP2,
         -pamx3_allfactors,
         -pamx4_allfactors)

medoid_allfactors3_tab <- clustered_data %>%
  select(-pamx2_mydata,
         -pamx3_mydata,
         -pamx2_newdata,
         -pamx3_newdata,
         -hclust2,
         -hclust3,
         -kpro2,
         -kpro3,
         -kpro4,
         -UMAP1,
         -UMAP2,
         -pamx2_allfactors,
         -pamx4_allfactors)

medoid_allfactors4_tab <- clustered_data %>%
  select(-pamx2_mydata,
         -pamx3_mydata,
         -pamx2_newdata,
         -pamx3_newdata,
         -hclust2,
         -hclust3,
         -kpro2,
         -kpro3,
         -kpro4,
         -UMAP1,
         -UMAP2,
         -pamx2_allfactors,
         -pamx3_allfactors)



for ( col in 1:ncol(medoid_allfactors2_tab)){
  colnames(medoid_allfactors2_tab)[col] <-  sub("factor.data.", "", colnames(medoid_allfactors2_tab)[col])
  colnames(medoid_allfactors2_tab)[col] <-  sub("data.", "", colnames(medoid_allfactors2_tab)[col])
  colnames(medoid_allfactors2_tab)[col] <-  sub("Mean.", "Mean", colnames(medoid_allfactors2_tab)[col])
  colnames(medoid_allfactors2_tab)[col] <-  sub("father.", "father", colnames(medoid_allfactors2_tab)[col])
  colnames(medoid_allfactors2_tab)[col] <-  sub("mother.", "mother", colnames(medoid_allfactors2_tab)[col])
}

for ( col in 1:ncol(medoid_allfactors3)){
  colnames(medoid_allfactors3)[col] <-  sub("factor.data.", "", colnames(medoid_allfactors3)[col])
}


for ( col in 1:ncol(medoid_allfactors3)){
  colnames(medoid_allfactors3)[col] <-  sub("data.", "", colnames(medoid_allfactors3)[col])
  colnames(medoid_allfactors3)[col] <-  sub("Mean.", "Mean", colnames(medoid_allfactors3)[col])
  colnames(medoid_allfactors3)[col] <-  sub("father.", "father", colnames(medoid_allfactors3)[col])
  colnames(medoid_allfactors3)[col] <-  sub("mother.", "mother", colnames(medoid_allfactors3)[col])
}


for ( col in 1:ncol(medoid_allfactors4)){
  colnames(medoid_allfactors4)[col] <-  sub("factor.data.", "", colnames(medoid_allfactors4)[col])
}


for ( col in 1:ncol(medoid_allfactors4)){
  colnames(medoid_allfactors4)[col] <-  sub("data.", "", colnames(medoid_allfactors4)[col])
  colnames(medoid_allfactors4)[col] <-  sub("Mean.", "Mean", colnames(medoid_allfactors4)[col])
  colnames(medoid_allfactors4)[col] <-  sub("father.", "father", colnames(medoid_allfactors4)[col])
  colnames(medoid_allfactors4)[col] <-  sub("mother.", "mother", colnames(medoid_allfactors4)[col])
}




summary((arsenal::tableby(pamx2_allfactors ~ ., stat= c("mean"), data = medoid_allfactors2_tab, cat.test = "chisq", total = FALSE)),
        text = TRUE, latex = TRUE)

summary((arsenal::tableby(pamx3_allfactors ~ ., stat= c("mean"), data = medoid_allfactors3_tab, cat.test = "chisq", total = FALSE)),
        text = TRUE, latex = TRUE)

summary((arsenal::tableby(pamx4_allfactors ~ ., stat= c("mean"), data = medoid_allfactors4_tab, cat.test = "chisq", total = FALSE)),
        text = TRUE, latex = TRUE)




source("./version_8_codes/data_table_nonstandardized.R")

table_tech <- data
table_tech$pamx2_allfactors <- clustered_data$pamx2_allfactors

summary((arsenal::tableby(pamx2_allfactors ~ ., data=table_tech, stat= c("mean"), cat.test = "chisq", total = FALSE)), 
        text = TRUE, latex = TRUE)

