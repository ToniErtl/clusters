#Table 1 :
rm(list=ls())
source("./version_8_codes/robustness_base_code.R")
source("./version_8_codes/Figure_01.R")
source("./version_8_codes/data_table_nonstandardized.R")


table_tech <- data
#table_tech$original_eucl_clusters <- clustered_data$original_eucl_clusters
table_tech$original_eucl_clusters <- figure_dist_table$original_eucl_clusters
      
summary((arsenal::tableby(original_eucl_clusters ~ ., data=table_tech, stat= c("mean"), cat.test = "chisq", total = FALSE)), 
        text = TRUE, latex = TRUE)
table_tech <- table_tech %>% select(-original_eucl_clusters)


table_tech$pamx2_newdata <- clustered_data$pamx2_newdata
summary((arsenal::tableby(pamx2_newdata ~ ., data=table_tech, stat= c("mean"), cat.test = "chisq", total = FALSE)), 
        text = TRUE, latex = TRUE)
table_tech <- table_tech %>% select(-pamx2_newdata)


table_tech$pamx2_allfactors <- clustered_data$pamx2_allfactors
summary((arsenal::tableby(pamx2_allfactors ~ ., data=table_tech, stat= c("mean"), cat.test = "chisq", total = FALSE)), 
        text = TRUE, latex = TRUE)
table_tech <- table_tech %>% select(-pamx2_allfactors)


