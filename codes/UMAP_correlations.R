rm(list= ls())

source("./codes/robustness_base_code.R")
source("./codes/technical_code.R")

umap_comp <- umap_data
umap_comp$UMAP1 <- clustered_data$UMAP1
umap_comp$UMAP2 <- clustered_data$UMAP2


# Compute correlation matrix between original variables and UMAP1, UMAP2
correlation_matrix <- cor(umap_data, 
                          umap_comp %>% select(UMAP1, UMAP2), use = "pairwise.complete.obs")

# Display the correlation matrix
stargazer::stargazer(print(correlation_matrix))



#Cross table of Figure 1: 

table(original_eucl_clusters$clustering,
      clustered_data$pamx2_newdata)


