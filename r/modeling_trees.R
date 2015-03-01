#modeling file
#tree models
#make sure to run validation and features first
#depends on: preprocessing_validation.R, preprocessing_features.R

## random forest -----------------------------------
library(randomForest)
rf <- randomForest(factor(label) ~ ., data = train_features)

rf$importance %>% 
  data.frame() %>%
  mutate(variable = factor(rownames(.), levels = rownames(.)[order(MeanDecreaseGini)])) %>%
  ggplot() +
  geom_point(aes(MeanDecreaseGini, variable))

## boosted trees ------------------------------------