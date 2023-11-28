# Analysis -- are these groups heterogeneous?
# run after running 01_ourdata_clustering.R
library(ggpubr)
library(tidyverse)

# since the order of the observations did not change when creating the clustering 
# database, we can get these ID'd.
data_tech <- data_cleaned
data_tech$kproto3 <- clustered_data$kproto3
data_tech$kproto2 <- clustered_data$kproto2

data_tech <- data_tech %>% 
            select(studid,kproto2,kproto3)

data_hetero <- left_join(data,data_tech, by = "studid") %>% 
  mutate(d_female = ifelse(female=="female",1,0),
         kproto2 = as.factor(kproto2),
         kproto3 = as.factor(kproto3)
         ) %>% 
  filter(!is.na(kproto2))
rm(data_tech)

# use data_hetero dataset in this file for analysis!


m1 <- (lm(grade_math ~ kproto3, data = data_hetero))
m2 <- (lm(math ~ kproto3, data = data_hetero))
m3 <- (lm(read ~ kproto3, data = data_hetero))
m4 <- (lm(grade_hun ~ kproto3, data = data_hetero))
m5 <- (lm(time_total ~ kproto3, data = data_hetero))
m6 <- (lm(pared_d3 ~ kproto3, data = data_hetero))
m7 <- (lm(d_female ~ kproto3, data = data_hetero))

stargazer::stargazer(m1,m2,m3,m4,m5,m6,m7)



library(broom)

# Create a list of models
model_list <- list(m1, m2, m3, m4, m5, m6, m7)

# Create an empty dataframe to store coefficients and confidence intervals
result_df <- data.frame()

# Loop through each model
for (i in seq_along(model_list)) {
  # Use the tidy function from broom to extract coefficients and confidence intervals
  model_summary <- tidy(model_list[[i]], conf.int = TRUE)
  
  # Remove the intercept from the results
  model_summary <- subset(model_summary, term != "(Intercept)")
  
  # Add a column to identify the model
  model_summary$model <- paste0("m", i)
  
  # Combine the results into the result_df dataframe
  result_df <- rbind(result_df, model_summary)
}

# Print the combined dataframe

y_variables <-  c( "grade_math","grade_math",
         "math","math",
         "read", "read", 
         "grade_hun", "grade_hun",
         "time_total","time_total",
         "pared_d3", "pared_d3",
         "d_female", "d_female"
)
result_df$y_variable <- y_variables

rm(y_variables, model_list)
print(result_df)


#plot 

confint_kproto <- ggplot(result_df, aes(x = reorder(term, estimate),
                      y = estimate, ymin = conf.low, ymax = conf.high)) +
  geom_point(position = position_dodge(width = 0.5)) +
  geom_errorbar(position = position_dodge(width = 0.5), width = 0.2) +
  coord_flip() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Coefficient Estimates and 95% Confidence Intervals",
       x = "Coefficient",
       y = "Estimate") +
  facet_wrap(~y_variable, scales = "free")+
  theme_minimal()

ggsave("./clustering_ourdata_plots/confint_kproto.pdf",confint_kproto, width = 8, height =6)

# model_list <- c(m1,"m2","m3","m4","m5","m6","m7")
# 
# for(i in model_list){
#   paste0(i)$coefficients[2]
#   confint(i)[2,]
# }





#look at whether there are systematic differences
# in schools with distributions in schools

table(data_hetero$schoolid, data_hetero$kproto3)


# Alternative: use MatchIt

# https://cran.r-project.org/web/packages/cobalt/vignettes/cobalt.html

library(MatchIt)
library(WeightIt)
library(cobalt)
#Using WeightIt to generate weights with multinomial
#logistic regression
data_hetero2 <-  data_hetero %>% mutate(schoolid = as.factor(schoolid))

W.out.mn <- WeightIt::weightit(kproto3 ~ pared_d3 + grade_math + grade_hun + grade_lit +
                                 books8 + time_total + d_female+math+read+schoolid, data = data_hetero2,
                               method = "energy",
                               use.mlogit = FALSE)

cobalt::bal.tab(W.out.mn, un = TRUE,
        disp = "means",
        which.treat = .all)

txt_size = 16

g1 <- cobalt::bal.plot(W.out.mn, "math", which = "unadjusted")+
  labs(title="",x='Math',fill="Nr. of Cluster")+
  theme( strip.background = element_blank(),
         strip.text = element_blank(),
         text = element_text(size = txt_size ) )

g2 <- cobalt::bal.plot(W.out.mn, "read", which = "unadjusted")+
  labs(title="",x='Read', fill="Nr. of Cluster") +
  theme( strip.background = element_blank(),
         strip.text = element_blank(),
         text = element_text(size = txt_size ) )

g3 <- cobalt::bal.plot(W.out.mn, "d_female", which = "unadjusted")+
  labs(title="",x='Female',fill="Nr. of Cluster") +
  theme( strip.background = element_blank(),
         strip.text = element_blank(),
         text = element_text(size = txt_size ) )

g4 <- cobalt::bal.plot(W.out.mn, "schoolid", which = "unadjusted")+
  labs(title="",x='School ID',fill="Nr. of Cluster") +
  theme( strip.background = element_blank(),
         strip.text = element_blank(),
         text = element_text(size = txt_size ) )

g5 <- cobalt::bal.plot(W.out.mn, "time_total", which = "unadjusted")+
  labs(title="",x='Time total',fill="Nr. of Cluster") +
  theme( strip.background = element_blank(),
         strip.text = element_blank(),
         text = element_text(size = txt_size ) )

g6 <- cobalt::bal.plot(W.out.mn, "pared_d3", which = "unadjusted")+
  labs(title="",x='Parents have \nuniversity education',fill="Nr. of Cluster") +
  theme( strip.background = element_blank(),
         strip.text = element_blank(),
         text = element_text(size = txt_size ) )  
  
  
ggarrange(g1,g2,g3,g4, g5, g6, common.legend = T)+
  labs(fill="Nr. of Cluster")











