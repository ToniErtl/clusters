### Test how the other variables in the analysis change

# step 0: clear the environment
rm(list = ls())
#


# Load packages

library(foreign)


library(tidyverse)
library(ggplot2)
library(ggthemes)
library(factoextra)
library(crosstable) # crosstable function

# Clustering methods:
library(cluster)

#Register parallel computing:

cores <- parallel::detectCores()-1
doParallel::registerDoParallel(cores = cores)


# For this code, the code in "_03_table1_missing_nomissing.R" is included to get
# clustering results including missing values






data <- read.dta(".//chowdhurry data//Data Archive//ConstructedData//children_familyAggregate_stat12.dta",
                 convert.factors = F)


# Data for probit model
data_model <- data %>% select(inc_per_cap_per_month_2016,
                              hhsize,
                              age_father,
                              age_mother,
                              schooling_father,
                              schooling_mother,
                              FSIQ_std_father,
                              FSIQ_std_mother)





# Data for clustering: MISSING VALUES INCLUDED
data <- data.frame(data$patient_choicesOffspringMean, data$patient_choices_father, 
                   data$patient_choices_mother, data$binswangerOffspringMean, 
                   data$binswanger_father, data$binswanger_mother, data$spitefulOffspringMean,
                   data$spiteful_father, data$spiteful_mother, data$altruisticOffspringMean, 
                   data$altruistic_father, data$altruistic_mother, data$egalitarianOffspringMean, 
                   data$egalitarian_father, data$egalitarian_mother, data$selfishOffspringMean, 
                   data$selfish_father, data$selfish_mother)




data <- scale(data)
# 2 clusters are optimal according to average silhouette width

require(cluster)
set.seed(1345)
pamx_original2 <- pam(data, 2)
set.seed(1345)
pamx_original3 <- pam(data, 3)
set.seed(1345)
pamx_original4 <- pam(data, 4)



data_model$clustering2 <- pamx_original2$cluster
data_model$cluster2_2 <- ifelse(data_model$clustering2==2,1,0)

data_model$clustering3 <- pamx_original3$cluster
data_model$cluster3_2 <- ifelse(data_model$clustering3==3,1,0)

data_model$clustering4 <- pamx_original4$cluster
data_model$cluster4_2 <- ifelse(data_model$clustering4==2,1,0)




library(fixest)
# Probit model -- with constant
probit_m2 <- feglm(cluster2_2~inc_per_cap_per_month_2016+
                   hhsize+age_father+age_mother+schooling_father+schooling_mother+FSIQ_std_father+FSIQ_std_mother,
                   data = data_model, family = binomial(link = 'probit'), vcov = 'hetero')

probit_m3 <- feglm(cluster3_2~inc_per_cap_per_month_2016+
                     hhsize+age_father+age_mother+schooling_father+schooling_mother+FSIQ_std_father+FSIQ_std_mother,
                   data = data_model, family = binomial(link = 'probit'), vcov = 'hetero')

etable(probit_m2,probit_m3)




probit_m2 <- glm(cluster2_2~inc_per_cap_per_month_2016+
                     hhsize+age_father+age_mother+schooling_father+schooling_mother+FSIQ_std_father+FSIQ_std_mother,
                   data = data_model, family = binomial(link = 'probit'))

probit_m3 <- glm(cluster3_2~inc_per_cap_per_month_2016+
                     hhsize+age_father+age_mother+schooling_father+schooling_mother+FSIQ_std_father+FSIQ_std_mother,
                   data = data_model, family = binomial(link = 'probit'))
#get marginal effects
library(margins)
mem_probit_m2 <- margins(probit_m2)
mem_probit_m3 <- margins(probit_m3)

etable(mem_probit_m2,mem_probit_m3)


# Probit model -- without constant

probit_m2_noconst <- feglm(cluster2_2~inc_per_cap_per_month_2016+
                     hhsize+age_father+age_mother+schooling_father+schooling_mother+FSIQ_std_father+FSIQ_std_mother-1,
                   data = data_model, family = binomial(link = 'probit'), vcov = 'hetero')

probit_m3_noconst <- feglm(cluster3_2~inc_per_cap_per_month_2016+
                     hhsize+age_father+age_mother+schooling_father+schooling_mother+FSIQ_std_father+FSIQ_std_mother-1,
                   data = data_model, family = binomial(link = 'probit'), vcov = 'hetero')


etable(probit_m2_noconst,probit_m3_noconst)


