
#Run the first clustering:

source("./_02_clustering_methods.R")
source("./_03_figure1.R")

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

# get results from code "02":




clustered_data$pamx2_allfactors <- factor(pamx2_allfactors$clustering) 
clustered_data$pamx3_allfactors <- factor(pamx3_allfactors$clustering) 
clustered_data$pamx4_allfactors <- factor(pamx4_allfactors$clustering)

#-------------
# Data visualization
#-------------
#-------------------


# Create ggplots:

#Eucliedan distance: from _03_figure1.R code
# part1




# add this all_factor method:
medoid_allfactors2 <- clustered_data %>%
  ggplot(aes(UMAP1,UMAP2, col = pamx2_allfactors))+
  geom_point()+
  theme(legend.position='none')+
  labs(title = "K-medoid using only categorical",
       subtitle = "Gower distance, k=2",
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
  labs(title = "K-medoid (mixed categorical and cont.)",
       subtitle = "Gower distance, k=2",
       col = NULL)+
  xlab("UMAP1")+
  ylab("UMAP2")+
  theme_minimal()+
  scale_color_colorblind()

ggsave("./comment_clustering_plots/eucl_kmed_kmed_categorical_comparison.pdf",
       ggpubr::ggarrange( part1,  medoid_gower2, medoid_allfactors2,
          ncol=3, nrow = 1), width = 12, height = 4)

# finally: datatables:


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


for ( col in 1:ncol(data)){
  colnames(data)[col] <-  sub("factor.data.", "", colnames(data)[col])
}


for ( col in 1:ncol(data)){
  colnames(data)[col] <-  sub("data.", "", colnames(data)[col])
  colnames(data)[col] <-  sub("Mean.", "Mean", colnames(data)[col])
  colnames(data)[col] <-  sub("father.", "father", colnames(data)[col])
  colnames(data)[col] <-  sub("mother.", "mother", colnames(data)[col])
}


medoid_allfactors2_tab <- data
medoid_allfactors2_tab$pamx2_allfactors <- clustered_data$pamx2_allfactors



for ( col in 1:ncol(medoid_allfactors2_tab)){
  colnames(medoid_allfactors2_tab)[col] <-  sub("factor.data.", "", colnames(medoid_allfactors2_tab)[col])
  colnames(medoid_allfactors2_tab)[col] <-  sub("data.", "", colnames(medoid_allfactors2_tab)[col])
  colnames(medoid_allfactors2_tab)[col] <-  sub("Mean.", "Mean", colnames(medoid_allfactors2_tab)[col])
  colnames(medoid_allfactors2_tab)[col] <-  sub("father.", "father", colnames(medoid_allfactors2_tab)[col])
  colnames(medoid_allfactors2_tab)[col] <-  sub("mother.", "mother", colnames(medoid_allfactors2_tab)[col])
}

summary((arsenal::tableby(pamx2_allfactors ~ ., stat= c("mean"), data = medoid_allfactors2_tab, cat.test = "chisq", total = FALSE)),
        text = TRUE, latex = TRUE)



eucl2_tab <- medoid_allfactors2_tab %>% select(-pamx2_allfactors)
eucl2_tab$original_eucl_clusters <- figure_dist_table$original_eucl_clusters

summary((arsenal::tableby(original_eucl_clusters ~ ., stat= c("mean"), data = eucl2_tab, cat.test = "chisq", total = FALSE)),
        text = TRUE, latex = TRUE)


# kmeodid categorical + continous 


kmed2_cat_and_cont <- medoid_allfactors2_tab %>% select(-pamx2_allfactors)
kmed2_cat_and_cont$pamx2_newdata <- clustered_data$pamx2_newdata
summary((arsenal::tableby(pamx2_newdata ~ ., stat= c("mean"), data = kmed2_cat_and_cont, cat.test = "chisq", total = FALSE)),
        text = TRUE, latex = TRUE)


