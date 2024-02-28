rm(list= ls())

source("./version_8_codes/robustness_base_code.R")

#pamx2_newdata, pamx3_newdata, pamx4_newdata from robustness_base_code file



# Again, as in code Figure_02.R, one modification: for k = 4 , 
# just to be in line with k = 3, I switch clusters 3 for 4;
# this is for only visualization purposes
 
clustered_data <- clustered_data %>%
  mutate(pamx4_newdata=case_when(pamx4_newdata==1~1,
                                           pamx4_newdata==2~2,
                                           pamx4_newdata==3~4,
                                           pamx4_newdata==4~3))



medoid_gower2 <- clustered_data %>% 
  mutate(pamx2_newdata = as.factor(pamx2_newdata)) %>% 
  ggplot(aes(UMAP1,UMAP2, col = factor(pamx2_newdata)))+
  geom_point()+
  theme(legend.position='none')+
  labs(title = "K-medoid (mixed categorical and cont.)",
       subtitle = "Gower distance, k=2",
       col = NULL)+
  xlab("UMAP1")+
  ylab("UMAP2")+
  theme_minimal()+
  scale_color_colorblind()


medoid_gower3 <- clustered_data %>% 
  mutate(pamx3_newdata = as.factor(pamx3_newdata)) %>% 
  ggplot(aes(UMAP1,UMAP2, col = factor(pamx3_newdata)))+
  geom_point()+
  theme(legend.position='none')+
  labs(title = "K-medoid (mixed categorical and cont.)",
       subtitle = "Gower distance, k=3",
       col = NULL)+
  xlab("UMAP1")+
  ylab("UMAP2")+
  theme_minimal()+
  scale_color_colorblind()

medoid_gower4 <- clustered_data %>% 
  mutate(pamx4_newdata = as.factor(pamx4_newdata)) %>% 
  ggplot(aes(UMAP1,UMAP2, col = factor(pamx4_newdata)))+
  geom_point()+
  theme(legend.position='none')+
  labs(title = "K-medoid (mixed categorical and cont.)",
       subtitle = "Gower distance, k=4",
       col = NULL)+
  xlab("UMAP1")+
  ylab("UMAP2")+
  theme_minimal()+
  scale_color_colorblind()

ggsave("./comment_clustering_plots/kmed_2_3_4.pdf",
       ggpubr::ggarrange( medoid_gower2,  medoid_gower3, medoid_gower4,
                          ncol=3, nrow = 1), width = 12, height = 4)





