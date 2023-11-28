### Clustering with PCA

pr.out <- data_scaled %>% prcomp(., scale = TRUE)


scree_plot_data <- data_scaled %>% psych::principal( nfactors = 4, rotate= "varimax")

pve <- 100 * pr.out$sdev^2 / sum(pr.out$sdev ^2)
par(mfrow = c(1, 2))
plot(pve , type = "o", ylab = "PVE", main = "Scree-plot for Principal Component Analysis", 
     frame = FALSE, xlab = "Principal Component", col = "blue")



# Alternatively, look at the number of Principal Components to attaint using Parallel Analysis.
# 
# Parallel Analysis: specify how many components to retain using simulation
# 
# Here, we get the critical values for each component. If the eigenvalues from the PCA are 
# greater than the value at 0.95 confint, we can retain given component.
# 
# We have two rules:
# 1) Kaiser's rule: Eigenvalue has to be >1 (satisfied up to 4 factors, with the 4th being barely above)
# 2) Eigenvalues being greater than the estimates gathered from Parallel Analysis (Horn, 1965)



library(hornpa)
hornpa(k = 4, #test for number of factors
       size = nrow(data_scaled), # size of dataset
       reps = 500, # number of simulations
       seed = 1234) #set seed


scree_plot_data <- data_scaled  %>% psych::principal( nfactors = 4, rotate= "varimax")
scree_plot_data



rm(scree_plot_data,pve)



#Overall, no clear indication on the number of components; still,
#in order to make it comparable with the non-demeaned data, I calculate with 4 factors in the PCA.


#### 4 Factor PCA 



pca_varimax<- data_scaled %>%  psych::principal( nfactors = 4, rotate= "varimax")


pca_varimax$loadings


pca_data <- data_scaled
pca_data$studid <- data_cleaned$studid
pca_data$comp <- data_cleaned$comp

pca_data$pca_socpref_interact <- pca_varimax$scores[,"RC1"]
pca_data$pca_timepref <- pca_varimax$scores[,"RC2"]
pca_data$pca_socpref_nointeract <- pca_varimax$scores[,"RC3"]
pca_data$pca_riskpref <- pca_varimax$scores[,"RC4"]


pca_graph <- data.frame(head(pca_varimax$loadings, n=nrow(pca_varimax$loadings))) %>%
  mutate(variable = rownames(.)) %>%
  gather(component,loading,-variable) %>%
  mutate(component = case_when(component == "RC3" ~ "Social Pref. without interactions",
                               component == "RC2" ~ "Time Preference",
                               component == "RC4" ~ "Risk, risky choice",
                               component == "RC1" ~ "Social Pref. with interactions"
                               )) %>%
  ggplot(aes(loading,variable))+
  geom_col(fill = "midnightblue")+
  facet_wrap(~component,nrow=1)+
    labs(color = NULL,
       title = "Factor Loadings for each Component",
      subtitle ="With 4 factors")+
  ylab(NULL)+
  xlab(NULL)+
  theme_minimal()+
  theme(text = element_text(size = 14))   

pca_graph

ggsave("./clustering_ourdata_plots/PCA.pdf",
            pca_graph+
            theme(text = element_text(size = 16)),
       width = 12, height = 6)




# Clustering with only 4 dimensions - with the 4 principal components

data_clustering_PCA <- pca_data %>% 
  select(pca_socpref_interact, pca_timepref, pca_socpref_nointeract ,pca_riskpref)

#check for optimal number of clusters

hclust_nclust_PCA <- parameters::n_clusters(data_clustering_PCA,
                                         package = c("easystats", "NbClust", "mclust"),
                                         standardize = FALSE,
                                         include_factors = TRUE,
                                         nbclust_method ="hclust"
)

view(hclust_nclust_PCA)


# 1-2-5 all viable options


# Hierarchical clustering, from k = 2 to k = 8

cls <- hclust(dist(data_clustering_PCA), method = "complete" )
dendagram <- as.dendrogram(cls)
LAB = rep("", nobs(dendagram))
dendagram = dendextend::set(dendagram, "labels", LAB)

# plot(dendextend::color_branches(dendagram, k = 2), main="Hierarchical Clustering with Gower-distance",
#      sub ="Using k = 2 based on scree-plot and silhouette method", leaflab = "none", horiz = F)

cut_complete <- cutree(cls, k = 2)
clustered_data$hierarch2pca <- cut_complete


# plot(dendextend::color_branches(dendagram, k = 3), main="Hierarchical Clustering with Gower-distance",
#      sub ="Using k = 2 based on scree-plot and silhouette method", leaflab = "none", horiz = F)
cut_complete <- cutree(cls, k = 3)
clustered_data$hierarch3pca <- cut_complete

cut_complete <- cutree(cls, k = 4)
clustered_data$hierarch4pca <- cut_complete

cut_complete <- cutree(cls, k = 5)
clustered_data$hierarch5pca <- cut_complete

cut_complete <- cutree(cls, k = 6)
clustered_data$hierarch6pca <- cut_complete

cut_complete <- cutree(cls, k = 7)
clustered_data$hierarch7pca <- cut_complete

cut_complete <- cutree(cls, k = 8)
clustered_data$hierarch8pca <- cut_complete


hc_graph2pca <- clustered_data %>%
  mutate(hierarch2pca = as.factor(hierarch2pca)) %>%
  ggplot(aes(UMAP1,UMAP2, col = hierarch2pca))+
  geom_point()+
  theme(legend.position='none')+
  labs(title = "Hierarchical Clustering (PCA)(k=4)",
       col = NULL)+
  xlab("UMAP1")+
  ylab("UMAP2")+
  theme_minimal()+
  scale_colour_colorblind()

hc_graph3pca <- clustered_data %>%
  mutate(hierarch3pca = as.factor(hierarch3pca)) %>%
  ggplot(aes(UMAP1,UMAP2, col = hierarch3pca))+
  geom_point()+
  theme(legend.position='none')+
  labs(title = "Hierarchical Clustering (PCA)(k=4)",
       col = NULL)+
  xlab("UMAP1")+
  ylab("UMAP2")+
  theme_minimal()+
  scale_colour_colorblind()

hc_graph4pca <- clustered_data %>%
  mutate(hierarch4pca = as.factor(hierarch4pca)) %>%
  ggplot(aes(UMAP1,UMAP2, col = hierarch4pca))+
  geom_point()+
  theme(legend.position='none')+
  labs(title = "Hierarchical Clustering (PCA)(k=4)",
       col = NULL)+
  xlab("UMAP1")+
  ylab("UMAP2")+
  theme_minimal()+
  scale_colour_colorblind()

hc_graph5pca <- clustered_data %>%
  mutate(hierarch5pca = as.factor(hierarch5pca)) %>%
  ggplot(aes(UMAP1,UMAP2, col = hierarch5pca))+
  geom_point()+
  theme(legend.position='none')+
  labs(title = "Hierarchical Clustering (PCA)(k=5)",
       col = NULL)+
  xlab("UMAP1")+
  ylab("UMAP2")+
  theme_minimal()+
  scale_colour_colorblind()

hc_graph6pca <- clustered_data %>%
  mutate(hierarch6pca = as.factor(hierarch6pca)) %>%
  ggplot(aes(UMAP1,UMAP2, col = hierarch6pca))+
  geom_point()+
  theme(legend.position='none')+
  labs(title = "Hierarchical Clustering (PCA)(k=6)",
       col = NULL)+
  xlab("UMAP1")+
  ylab("UMAP2")+
  theme_minimal()+
  scale_colour_colorblind()

hc_graph7pca <- clustered_data %>%
  mutate(hierarch7pca = as.factor(hierarch7pca)) %>%
  ggplot(aes(UMAP1,UMAP2, col = hierarch7pca))+
  geom_point()+
  theme(legend.position='none')+
  labs(title = "Hierarchical Clustering (PCA)(k=7)",
       col = NULL)+
  xlab("UMAP1")+
  ylab("UMAP2")+
  theme_minimal()+
  scale_colour_colorblind()


hc_graph8pca <- clustered_data %>%
  mutate(hierarch8pca = as.factor(hierarch8pca)) %>%
  ggplot(aes(UMAP1,UMAP2, col = hierarch8pca))+
  geom_point()+
  theme(legend.position='none')+
  labs(title = "Hierarchical Clustering (PCA)(k=8)",
       col = NULL)+
  xlab("UMAP1")+
  ylab("UMAP2")+
  theme_minimal()+
  scale_colour_colorblind()


ggsave("./clustering_ourdata_plots/hierarchical_2_to_8pca.pdf",
       ggpubr::ggarrange(hc_graph2pca, hc_graph3pca, hc_graph4pca, hc_graph5pca,
          hc_graph6pca, hc_graph7pca, hc_graph8pca,
         ncol = 4, nrow = 2),width = 12, height = 8)



rm(cls, dendagram, cut_complete)




# Just to check it: PCA with PAM

pam_nclust_PCA <- parameters::n_clusters(data_clustering_PCA,
                                         package = c("easystats", "NbClust", "mclust"),
                                         standardize = FALSE,
                                         include_factors = TRUE,
                                         nbclust_method ="pam"
)

view(pam_nclust_PCA)







