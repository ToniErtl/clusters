

# PvClust - Testing for hierarchical cluster robustness

library(pvclust)

bootstrap_cluster <- pvclust::pvclust(mydata, nboot =1000, r=seq(.5,1.4,by=.1))
pvclust::pvpick(bootstrap_cluster)





# Test with Gower-distance

# Compute the eigenvalues
x <- cmdscale(dist,1,eig=T) # dist: Gower distance matrix calculated as a daisy object
# Plot the eigenvalues and choose the correct number of dimensions (eigenvalues close to 0)
# plot(x$eig, 
#      type="h", lwd=5, las=1, 
#      xlab="Number of dimensions", 
#      ylab="Eigenvalues")

# Recover the coordinates that give the same distance matrix with the correct number of dimensions    
x <- cmdscale(dist,nb_dimensions)

# As mentioned by Stéphane, pvclust() clusters columns
pvclust(t(x))




bootstrap_cluster <- pvclust::pvclust(dist, nboot =1000, method.dist = dist,r=seq(.5,1.4,by=.1))
pvclust::pvpick(bootstrap_cluster)




test_clustered_data <- clustered_data
test_clustered_data$pvclust <- dpmeans_result$cluster

test_clustered_data %>%
  mutate(dpmeans = as.factor(dpmeans)) %>%
  ggplot(aes(UMAP1,UMAP2, col = dpmeans))+
  geom_point()+
  theme(legend.position='none')+
  labs(title = "Mclust -- model-based clustering",
       subtitle = "k=3",
       col = NULL)+
  xlab("UMAP1")+
  ylab("UMAP2")+
  theme_minimal()+
  scale_color_colorblind()




# Tests on model-based clustering:

library(T4cluster)
# DP-means using gower-distance (does not work on m1 mac):


# DOES NOT REALLY WORK IN OUR CASE

# dpmeans_result <- T4cluster::dpmeans(mydata, lambda = 40)
# table(dpmeans_result$cluster)
# test_clustered_data <- clustered_data
# test_clustered_data$dpmeans <- dpmeans_result$cluster
# 
# test_clustered_data %>% 
#   mutate(dpmeans = as.factor(dpmeans)) %>% 
#   ggplot(aes(UMAP1,UMAP2, col = dpmeans))+
#   geom_point()+
#   theme(legend.position='none')+
#   labs(title = "Mclust -- model-based clustering",
#        subtitle = "k=3",
#        col = NULL)+
#   xlab("UMAP1")+
#   ylab("UMAP2")+
#   theme_minimal()+
#   scale_color_colorblind()

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








