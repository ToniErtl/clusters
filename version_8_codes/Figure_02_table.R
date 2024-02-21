#Table 2 :
rm(list=ls())
#source("./version_8_codes/robustness_base_code.R")
source("./version_8_codes/Figure_02.R")
source("./version_8_codes/data_table_nonstandardized.R")



# figure_dist_table$original_eucl_clusters2 <- original_eucl_clusters2$clustering
# figure_dist_table$original_eucl_clusters3 <- original_eucl_clusters3$clustering
# figure_dist_table$original_eucl_clusters4 <- original_eucl_clusters4$clustering


table_tech <- data
#table_tech$original_eucl_clusters <- clustered_data$original_eucl_clusters
table_tech$original_eucl_clusters2 <- figure_dist_table$original_eucl_clusters2 

summary((arsenal::tableby(original_eucl_clusters2 ~ ., data=table_tech, stat= c("mean"), cat.test = "chisq", total = FALSE)), 
        text = TRUE, latex = TRUE)
table_tech <- table_tech %>% select(-original_eucl_clusters2)


table_tech$original_eucl_clusters3 <- figure_dist_table$original_eucl_clusters3 
summary((arsenal::tableby(original_eucl_clusters3 ~ ., data=table_tech, stat= c("mean"), cat.test = "chisq", total = FALSE)), 
        text = TRUE, latex = TRUE)
table_tech <- table_tech %>% select(-original_eucl_clusters3)


table_tech$original_eucl_clusters4 <- figure_dist_table$original_eucl_clusters4 
summary((arsenal::tableby(original_eucl_clusters4 ~ ., data=table_tech, stat= c("mean"), cat.test = "chisq", total = FALSE)), 
        text = TRUE, latex = TRUE)
table_tech <- table_tech %>% select(-original_eucl_clusters4)





