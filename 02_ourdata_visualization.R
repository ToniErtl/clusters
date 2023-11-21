# Data visualization-- after running code01



hc_graph2 <- clustered_data %>% 
  mutate(hierarch2 = as.factor(hierarch2)) %>% 
  ggplot(aes(UMAP1,UMAP2, col = hierarch2))+
  geom_point()+
  theme(legend.position='none')+
  labs(title = "Hierarchical Clustering (k=2)",
       col = NULL)+
  xlab("UMAP1")+
  ylab("UMAP2")+
  theme_minimal()+
  scale_colour_colorblind()

kproto_graph2 <- clustered_data %>% 
  mutate(kproto2 = as.factor(kproto2)) %>% 
  ggplot(aes(UMAP1,UMAP2, col = kproto2))+ 
  geom_point()+
  theme(legend.position='none')+
  labs(title = "K-prototype clustering (k=2)",
       col = NULL)+
  xlab("UMAP1")+
  ylab("UMAP2")+
  theme_minimal()+
  scale_colour_colorblind()

kmed_graph2 <- clustered_data %>% 
  mutate(kmedoid2 = as.factor(kmedoid2)) %>% 
  ggplot(aes(UMAP1,UMAP2, col = kmedoid2))+
  geom_point()+
  theme(legend.position='none')+
  labs(title = "K-medoid clustering (k=2)",
       col = NULL)+
  xlab("UMAP1")+
  ylab("UMAP2")+
  theme_minimal()+
  scale_colour_colorblind()


hc_graph3 <- clustered_data %>% 
  mutate(hierarch3 = as.factor(hierarch3)) %>% 
  ggplot(aes(UMAP1,UMAP2, col = hierarch3))+
  geom_point()+
  theme(legend.position='none')+
  labs(title = "Hierarchical Clustering (k=3)",
       col = NULL)+
  xlab("UMAP1")+
  ylab("UMAP2")+
  theme_minimal()+
  scale_colour_colorblind()

kproto_graph3 <- clustered_data %>% 
  mutate(kproto3 = as.factor(kproto3)) %>% 
  ggplot(aes(UMAP1,UMAP2, col = kproto3))+
  geom_point()+
  theme(legend.position='none')+
  labs(title = "K-prototype clustering (k=3)",
       col = NULL)+
  xlab("UMAP1")+
  ylab("UMAP2")+
  theme_minimal()+
  scale_colour_colorblind()

kmed_graph3 <- clustered_data %>% 
  mutate(kmedoid3 = as.factor(kmedoid3)) %>% 
  ggplot(aes(UMAP1,UMAP2, col = kmedoid3))+
  geom_point()+
  theme(legend.position='none')+
  labs(title = "K-medoid clustering (k=3)",
       col = NULL)+
  xlab("UMAP1")+
  ylab("UMAP2")+
  theme_minimal()+
  scale_colour_colorblind()



ggpubr::ggarrange(hc_graph2, kproto_graph2, kmed_graph2,
                  hc_graph3, kproto_graph3, kmed_graph3,
                  ncol = 3, nrow = 2)




hc_graph2_nocomp <- clustered_data %>%
  mutate(hierarch2_nocomp = as.factor(hierarch2_nocomp)) %>%
  ggplot(aes(UMAP1,UMAP2, col = hierarch2_nocomp))+
  geom_point()+
  theme(legend.position='none')+
  labs(title = "Hierarchical Clustering (nocomp)(k=2)",
       col = NULL)+
  xlab("UMAP1")+
  ylab("UMAP2")+
  theme_minimal()+
  scale_colour_colorblind()

kproto_graph2 <- clustered_data %>%
  mutate(kproto2 = as.factor(kproto2)) %>%
  ggplot(aes(UMAP1,UMAP2, col = kproto2))+
  geom_point()+
  theme(legend.position='none')+
  labs(title = "K-prototype clustering (k=2)",
       col = NULL)+
  xlab("UMAP1")+
  ylab("UMAP2")+
  theme_minimal()+
  scale_colour_colorblind()

kmed_graph2 <- clustered_data %>% 
  mutate(kmedoid2 = as.factor(kmedoid2)) %>% 
  ggplot(aes(UMAP1,UMAP2, col = kmedoid2))+
  geom_point()+
  theme(legend.position='none')+
  labs(title = "K-medoid clustering (k=2)",
       col = NULL)+
  xlab("UMAP1")+
  ylab("UMAP2")+
  theme_minimal()+
  scale_colour_colorblind()


hc_graph3_nocomp <- clustered_data %>% 
  mutate(hierarch3_nocomp = as.factor(hierarch3_nocomp)) %>% 
  ggplot(aes(UMAP1,UMAP2, col = hierarch3_nocomp))+
  geom_point()+
  theme(legend.position='none')+
  labs(title = "Hierarchical Clustering (nocomp)(k=3)",
       col = NULL)+
  xlab("UMAP1")+
  ylab("UMAP2")+
  theme_minimal()+
  scale_colour_colorblind()

kproto_graph3 <- clustered_data %>% 
  mutate(kproto3 = as.factor(kproto3)) %>% 
  ggplot(aes(UMAP1,UMAP2, col = kproto3))+
  geom_point()+
  theme(legend.position='none')+
  labs(title = "K-prototype clustering (k=3)",
       col = NULL)+
  xlab("UMAP1")+
  ylab("UMAP2")+
  theme_minimal()+
  scale_colour_colorblind()

kmed_graph3 <- clustered_data %>% 
  mutate(kmedoid3 = as.factor(kmedoid3)) %>% 
  ggplot(aes(UMAP1,UMAP2, col = kmedoid3))+
  geom_point()+
  theme(legend.position='none')+
  labs(title = "K-medoid clustering (k=3)",
       col = NULL)+
  xlab("UMAP1")+
  ylab("UMAP2")+
  theme_minimal()+
  scale_colour_colorblind()

kmed_graph2_nocomp <- clustered_data %>% 
  mutate(kmedoid2_nocomp = as.factor(kmedoid2_nocomp)) %>% 
  ggplot(aes(UMAP1,UMAP2, col = kmedoid2_nocomp))+
  geom_point()+
  theme(legend.position='none')+
  labs(title = "K-medoid clustering nocomp (k=2)",
       col = NULL)+
  xlab("UMAP1")+
  ylab("UMAP2")+
  theme_minimal()+
  scale_colour_colorblind()


kmed_graph3_nocomp <- clustered_data %>% 
  mutate(kmedoid3_nocomp = as.factor(kmedoid3_nocomp)) %>% 
  ggplot(aes(UMAP1,UMAP2, col = kmedoid3_nocomp))+
  geom_point()+
  theme(legend.position='none')+
  labs(title = "K-medoid clustering nocomp (k=3)",
       col = NULL)+
  xlab("UMAP1")+
  ylab("UMAP2")+
  theme_minimal()+
  scale_colour_colorblind()




ggpubr::ggarrange(hc_graph2_nocomp, kmed_graph2_nocomp, kproto_graph2, kmed_graph2,
                  hc_graph3_nocomp, kmed_graph3_nocomp, kproto_graph3, kmed_graph3,
                  ncol = 4, nrow = 2)

ggpubr::ggarrange(kmed_graph2_nocomp, kproto_graph2, kmed_graph2,
                  kmed_graph3_nocomp, kproto_graph3, kmed_graph3,
                  ncol = 3, nrow = 2)
