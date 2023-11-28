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

ggsave("./clustering_ourdata_plots/all_clusters01.pdf",ggpubr::ggarrange(hc_graph2_nocomp, kmed_graph2_nocomp, kproto_graph2, kmed_graph2,
                  hc_graph3_nocomp, kmed_graph3_nocomp, kproto_graph3, kmed_graph3,
                  ncol = 4, nrow = 2),width = 12, height = 8)

ggsave("./clustering_ourdata_plots/all_clusters02.pdf",ggpubr::ggarrange(kmed_graph2_nocomp, kproto_graph2, kmed_graph2,
                  kmed_graph3_nocomp, kproto_graph3, kmed_graph3,
                  ncol = 3, nrow = 2),width = 12, height = 8)

ggsave("./clustering_ourdata_plots/all_clusters03.pdf",ggpubr::ggarrange(hc_graph2, kmed_graph2_nocomp, kproto_graph2, kmed_graph2,
                  hc_graph3, kmed_graph3_nocomp, kproto_graph3, kmed_graph3,
                  ncol = 4, nrow = 2),width = 12, height = 8)




# New visualization: hiearchical evolution

hc_graph4 <- clustered_data %>% 
  mutate(hierarch4 = as.factor(hierarch4)) %>% 
  ggplot(aes(UMAP1,UMAP2, col = hierarch4))+
  geom_point()+
  theme(legend.position='none')+
  labs(title = "Hierarchical Clustering (k=4)",
       col = NULL)+
  xlab("UMAP1")+
  ylab("UMAP2")+
  theme_minimal()+
  scale_colour_colorblind()

hc_graph5 <- clustered_data %>% 
  mutate(hierarch5 = as.factor(hierarch5)) %>% 
  ggplot(aes(UMAP1,UMAP2, col = hierarch5))+
  geom_point()+
  theme(legend.position='none')+
  labs(title = "Hierarchical Clustering (k=5)",
       col = NULL)+
  xlab("UMAP1")+
  ylab("UMAP2")+
  theme_minimal()+
  scale_colour_colorblind()

hc_graph6 <- clustered_data %>% 
  mutate(hierarch6 = as.factor(hierarch6)) %>% 
  ggplot(aes(UMAP1,UMAP2, col = hierarch6))+
  geom_point()+
  theme(legend.position='none')+
  labs(title = "Hierarchical Clustering (k=6)",
       col = NULL)+
  xlab("UMAP1")+
  ylab("UMAP2")+
  theme_minimal()+
  scale_colour_colorblind()

hc_graph7 <- clustered_data %>% 
  mutate(hierarch7 = as.factor(hierarch7)) %>% 
  ggplot(aes(UMAP1,UMAP2, col = hierarch7))+
  geom_point()+
  theme(legend.position='none')+
  labs(title = "Hierarchical Clustering (k=7)",
       col = NULL)+
  xlab("UMAP1")+
  ylab("UMAP2")+
  theme_minimal()+
  scale_colour_colorblind()


hc_graph8 <- clustered_data %>% 
  mutate(hierarch8 = as.factor(hierarch8)) %>% 
  ggplot(aes(UMAP1,UMAP2, col = hierarch8))+
  geom_point()+
  theme(legend.position='none')+
  labs(title = "Hierarchical Clustering (k=8)",
       col = NULL)+
  xlab("UMAP1")+
  ylab("UMAP2")+
  theme_minimal()+
  scale_colour_colorblind()



ggsave("./clustering_ourdata_plots/hierarchical_2_to_8.pdf",ggpubr::ggarrange(hc_graph2, hc_graph3, hc_graph4, hc_graph5,
                  hc_graph6, hc_graph7, hc_graph8,
                  ncol = 4, nrow = 2),width = 12, height = 8)




#---------

hc_graph4_nocomp <- clustered_data %>%
  mutate(hierarch4_nocomp = as.factor(hierarch4_nocomp)) %>%
  ggplot(aes(UMAP1,UMAP2, col = hierarch4_nocomp))+
  geom_point()+
  theme(legend.position='none')+
  labs(title = "Hierarchical Clustering (nocomp)(k=4)",
       col = NULL)+
  xlab("UMAP1")+
  ylab("UMAP2")+
  theme_minimal()+
  scale_colour_colorblind()

hc_graph5_nocomp <- clustered_data %>%
  mutate(hierarch5_nocomp = as.factor(hierarch5_nocomp)) %>%
  ggplot(aes(UMAP1,UMAP2, col = hierarch5_nocomp))+
  geom_point()+
  theme(legend.position='none')+
  labs(title = "Hierarchical Clustering (nocomp)(k=5)",
       col = NULL)+
  xlab("UMAP1")+
  ylab("UMAP2")+
  theme_minimal()+
  scale_colour_colorblind()

hc_graph6_nocomp <- clustered_data %>%
  mutate(hierarch6_nocomp = as.factor(hierarch6_nocomp)) %>%
  ggplot(aes(UMAP1,UMAP2, col = hierarch6_nocomp))+
  geom_point()+
  theme(legend.position='none')+
  labs(title = "Hierarchical Clustering (nocomp)(k=6)",
       col = NULL)+
  xlab("UMAP1")+
  ylab("UMAP2")+
  theme_minimal()+
  scale_colour_colorblind()

hc_graph7_nocomp <- clustered_data %>%
  mutate(hierarch7_nocomp = as.factor(hierarch7_nocomp)) %>%
  ggplot(aes(UMAP1,UMAP2, col = hierarch7_nocomp))+
  geom_point()+
  theme(legend.position='none')+
  labs(title = "Hierarchical Clustering (nocomp)(k=7)",
       col = NULL)+
  xlab("UMAP1")+
  ylab("UMAP2")+
  theme_minimal()+
  scale_colour_colorblind()


hc_graph8_nocomp <- clustered_data %>%
  mutate(hierarch8_nocomp = as.factor(hierarch8_nocomp)) %>%
  ggplot(aes(UMAP1,UMAP2, col = hierarch8_nocomp))+
  geom_point()+
  theme(legend.position='none')+
  labs(title = "Hierarchical Clustering (nocomp)(k=8)",
       col = NULL)+
  xlab("UMAP1")+
  ylab("UMAP2")+
  theme_minimal()+
  scale_colour_colorblind()


ggsave("./clustering_ourdata_plots/hierarchical_2_to_8_nocomp.pdf",ggpubr::ggarrange(hc_graph2_nocomp, hc_graph3_nocomp, hc_graph4_nocomp, hc_graph5_nocomp,
                  hc_graph6_nocomp, hc_graph7_nocomp, hc_graph8_nocomp,
                  ncol = 4, nrow = 2),width = 12, height = 8)











