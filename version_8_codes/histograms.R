# Histograms

rm(list=ls())


library(tidyverse)

data <- read.dta(".//chowdhurry data//Data Archive//ConstructedData//children_familyAggregate_stat12.dta",
                 convert.factors = F)

id <- data.frame(data$slno, data$mid)
data <- data.frame(data$patient_choicesOffspringMean, data$patient_choices_father, 
                   data$patient_choices_mother, data$binswangerOffspringMean, 
                   data$binswanger_father, data$binswanger_mother, data$spitefulOffspringMean,
                   data$spiteful_father, data$spiteful_mother, data$altruisticOffspringMean, 
                   data$altruistic_father, data$altruistic_mother, data$egalitarianOffspringMean, 
                   data$egalitarian_father, data$egalitarian_mother, data$selfishOffspringMean, 
                   data$selfish_father, data$selfish_mother)

data <- data[complete.cases(data), ] # only pamx works with NAs


#continous variables:
data_cont <-data.frame(data$data.patient_choicesOffspringMean,
                       data$data.patient_choices_father,
                       data$data.patient_choices_mother,
                       data$data.binswangerOffspringMean,
                       data$data.binswanger_father,
                       data$data.binswanger_mother
) 

data_cont <- as.data.frame(data_cont)

data_categorical <- data.frame(factor(data$data.spitefulOffspringMean),
                               factor(data$data.spiteful_father),
                               factor(data$data.spiteful_mother),
                               factor(data$data.altruisticOffspringMean),
                               factor(data$data.altruistic_father),
                               factor(data$data.altruistic_mother),
                               factor(data$data.egalitarianOffspringMean),
                               factor(data$data.egalitarian_father),
                               factor(data$data.egalitarian_mother),
                               factor(data$data.selfishOffspringMean),
                               factor(data$data.selfish_father),
                               factor(data$data.selfish_mother))


data_cont$id <- rownames(data_cont)
data_categorical$id <- rownames(data_categorical)
rm(data)
data <- merge(data_cont, data_categorical, by = "id") %>% select(-id)
rm(data_cont,data_categorical)


for ( col in 1:ncol(data)){
  colnames(data)[col] <-  sub("factor.data.", "", colnames(data)[col])
}


for ( col in 1:ncol(data)){
  colnames(data)[col] <-  sub("data.", "", colnames(data)[col])
  colnames(data)[col] <-  sub("data.", "", colnames(data)[col])
  colnames(data)[col] <-  sub("Mean.", "Mean", colnames(data)[col])
  colnames(data)[col] <-  sub("father.", "father", colnames(data)[col])
  colnames(data)[col] <-  sub("mother.", "mother", colnames(data)[col])
}


data <- data.frame(lapply(data, function(x) type.convert(as.character(x), as.is = TRUE)))


data_cont <-data.frame(data$patient_choicesOffspringMean,
                       data$patient_choices_father,
                       data$patient_choices_mother,
                       data$binswangerOffspringMean,
                       data$binswanger_father,
                       data$binswanger_mother
) 






p1 <- ggplot(data, aes(x = patient_choicesOffspringMean)) +
  geom_histogram(aes(y = ..density..), binwidth = 1, fill = "midnightblue", color = "black") +
  labs(title = "Children (Mean)", x = "Number of Patient Choices", y = "Density") +
  theme_minimal()

# Histogram for patient_choices_father
p2 <- ggplot(data, aes(x = patient_choices_father)) +
  geom_histogram(aes(y = ..density..), binwidth = 1, fill = "midnightblue", color = "black") +
  labs(title = "Father", x = "Number of Patient Choices", y = "Density") +
  theme_minimal()

# Histogram for patient_choices_mother
p3 <- ggplot(data, aes(x = patient_choices_mother)) +
  geom_histogram(aes(y = ..density..), binwidth = 1, fill = "midnightblue", color = "black") +
  labs(title = "Mother", x = "Number of Patient Choices", y = "Density") +
  theme_minimal()


ggsave("./comment_clustering_plots/hist_patience.pdf",
       ggpubr::ggarrange(p1, p2, p3, ncol = 3, nrow = 1),
       width = 12, height = 4)


p4 <- ggplot(data, aes(x = binswangerOffspringMean)) +
  geom_histogram(aes(y = ..density..), binwidth = 1, fill = "midnightblue", color = "black") +
  labs(title = "Children (Mean)", x = "Gamble Number picked", y = "Density") +
  theme_minimal()

p5 <- ggplot(data, aes(x = binswanger_father)) +
  geom_histogram(aes(y = ..density..), binwidth = 1, fill = "midnightblue", color = "black") +
  labs(title = "Father", x = "Gamble Number picked", y = "Density") +
  theme_minimal()


p6 <- ggplot(data, aes(x = binswanger_mother)) +
  geom_histogram(aes(y = ..density..), binwidth = 1, fill = "midnightblue", color = "black") +
  labs(title = "Mother", x = "Gamble Number picked", y = "Density") +
  theme_minimal()


ggsave("./comment_clustering_plots/hist_risk.pdf",
       ggpubr::ggarrange(p4, p5, p6, ncol = 3, nrow = 1),
       width = 12, height = 4)



