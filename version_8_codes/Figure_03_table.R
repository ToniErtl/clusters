#Table 3 :
rm(list=ls())
#source("./version_8_codes/robustness_base_code.R")
source("./version_8_codes/Figure_03.R")
source("./version_8_codes/data_table_nonstandardized.R")



pamx2_newdata_tab <- data #load data from data_table_nonstandardized
#table_tech$original_eucl_clusters <- clustered_data$original_eucl_clusters
pamx2_newdata_tab$pamx2_newdata <- clustered_data$pamx2_newdata
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