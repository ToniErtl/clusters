#Table 2 :
rm(list=ls())
source("./codes/Figure_02.R")
source("./codes/data_table_nonstandardized.R")




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





