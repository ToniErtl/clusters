
# Berti way:
#Run the first clustering:

source("./_02_clustering_methods.R")

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

# make factors from all variables

data_allfactors <- data
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
factor_n_clusters # note: results are same even if we use hierarchical clustering




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

ggsave("./comment_clustering_plots/medoid_allfactors234.pdf",ggpubr::ggarrange(medoid_allfactors2,medoid_allfactors3,medoid_allfactors4,
                  ncol =3, nrow= 1), width = 12, height = 4)



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

ggsave("./comment_clustering_plots/hierarch_allfactors234.pdf", ggpubr::ggarrange(hier_factors2,hier_factors3,
                  ncol =2, nrow= 1), width = 8, height = 4)

# finally: datatable:

# These values are with standardized values: for nonstandardized values, go to _03_table.R


# 
# 
# for ( col in 1:ncol(clustered_data)){
#   colnames(clustered_data)[col] <-  sub("factor.data.", "", colnames(clustered_data)[col])
#   colnames(clustered_data)[col] <-  sub("data.", "", colnames(clustered_data)[col])
#   colnames(clustered_data)[col] <-  sub("Mean.", "Mean", colnames(clustered_data)[col])
#   colnames(clustered_data)[col] <-  sub("father.", "father", colnames(clustered_data)[col])
#   colnames(clustered_data)[col] <-  sub("mother.", "mother", colnames(clustered_data)[col])
# }
# 
# 
# 
# medoid_allfactors2_tab <- clustered_data %>%
#   select(-pamx2_mydata,
#          -pamx3_mydata,
#          -pamx2_newdata,
#          -pamx3_newdata,
#          -pamx4_newdata,
#          -hclust2,
#          -hclust3,
#          -kpro2,
#          -kpro3,
#          -kpro4,
#          -UMAP1,
#          -UMAP2,
#          -pamx3_allfactors,
#          -pamx4_allfactors,
#          -hclust_2_gower_allfactor,
#          -hclust_3_gower_allfactor)
# 
# medoid_allfactors3_tab <- clustered_data %>%
#   select(-pamx2_mydata,
#          -pamx3_mydata,
#          -pamx2_newdata,
#          -pamx3_newdata,
#          -pamx4_newdata,
#          -hclust2,
#          -hclust3,
#          -kpro2,
#          -kpro3,
#          -kpro4,
#          -UMAP1,
#          -UMAP2,
#          -pamx2_allfactors,
#          -pamx4_allfactors,
#          -hclust_2_gower_allfactor,
#          -hclust_3_gower_allfactor)
# 
# medoid_allfactors4_tab <- clustered_data %>%
#   select(-pamx2_mydata,
#          -pamx3_mydata,
#          -pamx2_newdata,
#          -pamx3_newdata,
#          -pamx4_newdata,
#          -hclust2,
#          -hclust3,
#          -kpro2,
#          -kpro3,
#          -kpro4,
#          -UMAP1,
#          -UMAP2,
#          -pamx2_allfactors,
#          -pamx3_allfactors,
#          -hclust_2_gower_allfactor,
#          -hclust_3_gower_allfactor)
# 
# 
# 
# summary((arsenal::tableby(pamx2_allfactors ~ ., stat= c("mean"), data = medoid_allfactors2_tab, cat.test = "chisq", total = FALSE)),
#         text = TRUE, latex = TRUE)
# 
# summary((arsenal::tableby(pamx3_allfactors ~ ., stat= c("mean"), data = medoid_allfactors3_tab, cat.test = "chisq", total = FALSE)),
#         text = TRUE, latex = TRUE)
# 
# summary((arsenal::tableby(pamx4_allfactors ~ ., stat= c("mean"), data = medoid_allfactors4_tab, cat.test = "chisq", total = FALSE)),
#         text = TRUE, latex = TRUE)
# 
# 
# 
# 
# #--------
# 
# 
# hierallfactors2_tab <- clustered_data %>%
#   select(-pamx2_mydata,
#          -pamx3_mydata,
#          -pamx2_newdata,
#          -pamx3_newdata,
#          -pamx4_newdata,
#          -hclust2,
#          -hclust3,
#          -kpro2,
#          -kpro3,
#          -kpro4,
#          -UMAP1,
#          -UMAP2,
#          -pamx2_allfactors,
#          -pamx3_allfactors,
#          -pamx4_allfactors,
#          -hclust_3_gower_allfactor)
# 
# 
# hierallfactors3_tab <- clustered_data %>%
#   select(-pamx2_mydata,
#          -pamx3_mydata,
#          -pamx2_newdata,
#          -pamx3_newdata,
#          -pamx4_newdata,
#          -hclust2,
#          -hclust3,
#          -kpro2,
#          -kpro3,
#          -kpro4,
#          -UMAP1,
#          -UMAP2,
#          -pamx2_allfactors,
#          -pamx3_allfactors,
#          -pamx4_allfactors,
#          -hclust_2_gower_allfactor
#          )
# 
# 
# 
# summary((arsenal::tableby(hclust_2_gower_allfactor ~ ., stat= c("mean"), data = hierallfactors2_tab, cat.test = "chisq", total = FALSE)),
#         text = TRUE, latex = TRUE)
# 
# summary((arsenal::tableby(hclust_3_gower_allfactor ~ ., stat= c("mean"), data = hierallfactors3_tab, cat.test = "chisq", total = FALSE)),
#         text = TRUE, latex = TRUE)







