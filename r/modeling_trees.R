#modeling file
#tree models
#make sure to run validation and features first
#depends on: preprocessing_validation.R, preprocessing_features.R

## random forest -----------------------------------
library(randomForest)
rf <- randomForest(factor(label) ~ ., 
                   data = train_features %>% select(-id),
                   ntree = 1000)

rf$importance %>% 
  data.frame() %>%
  mutate(variable = factor(rownames(.), levels = rownames(.)[order(MeanDecreaseGini)])) %>%
  ggplot() +
  geom_point(aes(MeanDecreaseGini, variable))

rf.predict <- predict(rf, validate_features, type = "class")

#8.7% validation error 
#mean(as.numeric(as.character(rf.predict)) != validate_features$label)

## boosted trees ------------------------------------
library(gbm)
expand.grid(shrinkage = .1, depth = 1:10) %>%
  group_by(shrinkage, depth) %>%
  do(model = gbm(formula = factor(label) ~ .,
                 data = train_features %>% select(-id),
                 distribution = "multinomial",
                 shrinkage = .$shrinkage,
                 interaction.depth = .$depth,
                 n.trees = 500)) -> boosted.trees


## save models because they take a long time to run ----------------------------
save(rf, boosted.trees, file = "written_results/models.RData")
