rm(list= ls())

source("./_02_clustering_methods.R")

#pamx2_newdata, pamx3_newdata, pamx4_newdata from clustering_methods file
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


medoid_gower3 <- clustered_data %>% 
  mutate(pamx3_newdata = as.factor(pamx3_newdata)) %>% 
  ggplot(aes(UMAP1,UMAP2, col = factor(pamx3_newdata)))+
  geom_point()+
  theme(legend.position='none')+
  labs(title = "K-medoid (mixed categorical and cont.)",
       subtitle = "Gower distance, k=3",
       col = NULL)+
  xlab("UMAP1")+
  ylab("UMAP2")+
  theme_minimal()+
  scale_color_colorblind()

medoid_gower4 <- clustered_data %>% 
  mutate(pamx4_newdata = as.factor(pamx4_newdata)) %>% 
  ggplot(aes(UMAP1,UMAP2, col = factor(pamx4_newdata)))+
  geom_point()+
  theme(legend.position='none')+
  labs(title = "K-medoid (mixed categorical and cont.)",
       subtitle = "Gower distance, k=4",
       col = NULL)+
  xlab("UMAP1")+
  ylab("UMAP2")+
  theme_minimal()+
  scale_color_colorblind()

ggsave("./comment_clustering_plots/kmed_2_3_4.pdf",
       ggpubr::ggarrange( medoid_gower2,  medoid_gower3, medoid_gower4,
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


pamx2_newdata_tab <- data
pamx2_newdata_tab$pamx2_newdata <- clustered_data$pamx2_newdata



for ( col in 1:ncol(pamx2_newdata_tab)){
  colnames(pamx2_newdata_tab)[col] <-  sub("factor.data.", "", colnames(pamx2_newdata_tab)[col])
  colnames(pamx2_newdata_tab)[col] <-  sub("data.", "", colnames(pamx2_newdata_tab)[col])
  colnames(pamx2_newdata_tab)[col] <-  sub("Mean.", "Mean", colnames(pamx2_newdata_tab)[col])
  colnames(pamx2_newdata_tab)[col] <-  sub("father.", "father", colnames(pamx2_newdata_tab)[col])
  colnames(pamx2_newdata_tab)[col] <-  sub("mother.", "mother", colnames(pamx2_newdata_tab)[col])
}

summary((arsenal::tableby(pamx2_newdata ~ ., stat= c("mean"), data = pamx2_newdata_tab, cat.test = "chisq", total = FALSE)),
        text = TRUE, latex = TRUE)



pamx3_newdata_tab <- pamx2_newdata_tab %>% select(-pamx2_newdata)
pamx3_newdata_tab$pamx3_newdata <- clustered_data$pamx3_newdata

summary((arsenal::tableby(pamx3_newdata ~ ., stat= c("mean"), data = pamx3_newdata_tab, cat.test = "chisq", total = FALSE)),
        text = TRUE, latex = TRUE)





pamx4_newdata_tab <- pamx2_newdata_tab %>% select(-pamx2_newdata)
pamx4_newdata_tab$pamx4_newdata <- clustered_data$pamx4_newdata

summary((arsenal::tableby(pamx4_newdata ~ ., stat= c("mean"), data = pamx4_newdata_tab, cat.test = "chisq", total = FALSE)),
        text = TRUE, latex = TRUE)

