# This is the replication code for our comment on Chowdhurry et. al (2022)

#clear all environment
rm(list=ls())
# Read libraries


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

# ---------------------

#----- First do the original clustering excercise with k = 2 and k =.3


