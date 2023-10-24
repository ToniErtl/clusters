

# Tests on model-based clustering:

library(T4cluster)
# DP-means using gower-distance (does not work on m1 mac):

T4cluster::dpmeans(dist, lambda = 0.1)




#----------


#Mclust trial with Gower-distance:

library(mclust)

n_clust_mclust1 <- parameters::n_clusters(new_data,
                                          package = c("easystats", "NbClust", "mclust"),
                                          standardize = FALSE,
                                          include_factors = TRUE,
                                          nbclust_method = "mclust"
)
n_clust_mclust1

n_clust_mclust2 <- parameters::n_clusters(dist,
                                          package = c("easystats", "NbClust", "mclust"),
                                          standardize = FALSE,
                                          include_factors = TRUE,
                                          nbclust_method = "mclust"
)
n_clust_mclust2

set.seed(123)

optimal_MC <- Mclust(new_data)
summary(optimal_MC)

# optimal_MC <- Mclust(dist)
# summary(optimal_MC)


test_clustered_data <- clustered_data
test_clustered_data$mclust3 <- optimal_MC$classification
  
test_clustered_data %>% 
  mutate(mclust3 = as.factor(mclust3)) %>% 
  ggplot(aes(UMAP1,UMAP2, col = mclust3))+
  geom_point()+
  theme(legend.position='none')+
  labs(title = "Mclust -- model-based clustering",
       subtitle = "k=3",
       col = NULL)+
  xlab("UMAP1")+
  ylab("UMAP2")+
  theme_minimal()+
  scale_color_colorblind()








