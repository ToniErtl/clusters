
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

# list of continous variables
variable_list= c("data$data.patient_choicesOffspringMean",
                 "data$data.patient_choices_father",
                 "data$data.patient_choices_mother",
                 "data$data.binswangerOffspringMean",
                 "data$data.binswanger_father",
                 "data$data.binswanger_mother")



# Create dummy variables:
# data$patient_choicesOffspringMean1<- ifelse(data$data.patient_choicesOffspringMean<quantile(data$data.patient_choicesOffspringMean, probs = c(0.33)),1,0)
# data$patient_choicesOffspringMean2<- ifelse(data$data.patient_choicesOffspringMean>quantile(data$data.patient_choicesOffspringMean, probs = c(0.33)) &
#                                               data$data.patient_choicesOffspringMean<quantile(data$data.patient_choicesOffspringMean, probs = c(0.66)) ,1,0)
# data$patient_choicesOffspringMean3<- ifelse(data$data.patient_choicesOffspringMean>quantile(data$data.patient_choicesOffspringMean, probs = c(0.66)),1,0)


# Do the creation of variables for terciles defined as factors (as in the above, commented code)



variable_list <- c("patient_choicesOffspringMean",
                   "patient_choices_father",
                   "patient_choices_mother",
                   "binswangerOffspringMean",
                   "binswanger_father",
                   "binswanger_mother")

# Assuming 'data' is your data frame, replace it with your actual data frame name

# For each variable in the list
for (variable in variable_list) {
  # Create binary variables for each tercile
  for (i in 1:3) {
    lower_bound <- as.numeric(quantile(data[[paste0("data.", variable)]], probs = (i - 1) / 3))
    upper_bound <- as.numeric(quantile(data[[paste0("data.", variable)]], probs = i / 3))

    new_var <- paste0(variable, i)

    data[[new_var]] <- ifelse(data[[paste0("data.", variable)]] < upper_bound &
                                data[[paste0("data.", variable)]] >= lower_bound, 1, 0)
  }
}





# Alternative for robustness: just make them one variable and use them as factor variables

#Unfortunately, makes the clustering worse:


# variable_list <- c("patient_choicesOffspringMean",
#                    "patient_choices_father",
#                    "patient_choices_mother",
#                    "binswangerOffspringMean",
#                    "binswanger_father",
#                    "binswanger_mother")
# 
# for (variable in variable_list) {
#   # Create binary variables for each tercile
#     lower_bound <- as.numeric(quantile(data[[paste0("data.", variable)]], probs = (2 - 1) / 3))
#     middle_bound <- as.numeric(quantile(data[[paste0("data.", variable)]], probs = (3 - 1) / 3))
#     upper_bound <- as.numeric(quantile(data[[paste0("data.", variable)]], probs = 3 / 3))
#     
#     new_var <- paste0(variable,"factored")
#     
#     data[[new_var]] <- case_when(data[[paste0("data.", variable)]] <lower_bound~1,
#                                  data[[paste0("data.", variable)]] >=lower_bound & data[[paste0("data.", variable)]] <middle_bound  ~2,
#                                  TRUE~3
#     )
#   
# }








# get rid of the continuous variables:

data_allfactors <- data %>%
  select(-data.patient_choicesOffspringMean,
         -data.patient_choices_father,
         -data.patient_choices_mother,
         -data.binswangerOffspringMean,
         -data.binswanger_father,
         -data.binswanger_mother)

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



for ( col in 1:ncol(medoid_allfactors2)){
  colnames(medoid_allfactors2)[col] <-  sub("factor.data.", "", colnames(medoid_allfactors2)[col])
}


for ( col in 1:ncol(medoid_allfactors2)){
  colnames(medoid_allfactors2)[col] <-  sub("data.", "", colnames(medoid_allfactors2)[col])
  colnames(medoid_allfactors2)[col] <-  sub("Mean.", "Mean", colnames(medoid_allfactors2)[col])
  colnames(medoid_allfactors2)[col] <-  sub("father.", "father", colnames(medoid_allfactors2)[col])
  colnames(medoid_allfactors2)[col] <-  sub("mother.", "mother", colnames(medoid_allfactors2)[col])
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
