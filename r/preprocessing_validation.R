#preprocessing file
#load data and create separate validation set
#make sure to run this file first
#depends on: setwd() to folder above data/


#data ------------------------------------------
train <- read.csv("../data/train.csv", header=TRUE)
test <- read.csv("../data/test.csv", header=TRUE)


#library --------------------------------------
library(dplyr)
library(tidyr)
library(ggplot2)

#split into train/validate -------------------
set.seed(503503) #reproduce

train %>%
  group_by(label) %>%
  sample_frac(1/3) -> validation

train %>%
  anti_join(validation) -> train

train %>% 
  mutate_each(funs(as.numeric), starts_with("pixel")) -> train

test %>%
  mutate(label = paste0("fake", 1:n())) -> test #fake label for grouping


  



