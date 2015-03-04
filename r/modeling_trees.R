#modeling file
#tree models
#make sure to run validation and features first
#depends on: preprocessing_validation.R, preprocessing_features.R

## random forest ------------------------------------------
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

#7.95% validation error 
#mean(as.numeric(as.character(rf.predict)) != validate_features$label)

## boosted trees ------------------------------------------------
library(gbm)

boosted.results <- function(shrinkage, depth) {
  expand.grid(shrinkage = shrinkage, depth = depth) %>%
    group_by(shrinkage, depth) %>%
    do(model = gbm(formula = factor(label) ~ .,
                   data = train_features %>% select(-id),
                   distribution = "multinomial",
                   shrinkage = .$shrinkage,
                   interaction.depth = .$depth,
                   n.trees = 500)) -> boosted.trees
  
  for(i in 1:nrow(boosted.trees)){
    boosted.trees[i, "train_error"] <- boosted.trees[i,]$model[[1]]$train.error[boosted.trees[i,]$model[[1]]$n.trees]
  }
  boosted.trees %>%
    group_by(shrinkage, depth, train_error) %>%
    do(pred = predict(.$model[[1]], validate_features %>% select(-id, -label), n.trees = .$model[[1]]$n.trees, type = "response")) -> gbm.predict
  
  gbm.predict %>%
    group_by(shrinkage, depth, train_error) %>%
    do(prediction = apply(.$pred[[1]][,,1],1, which.max)) %>%
    group_by(shrinkage, depth, train_error) %>%
    mutate(test_error = mean(as.character(prediction[[1]] - 1) != validate_features$label)) -> gbm.results
  
  return(list(model = boosted.trees, preds = gbm.predict, res = gbm.results))  
}

boosted_1_5 <- boosted.results(.1, 1:5)
boosted_6_10 <- boosted.results(.1, 6:10)
boosted_11_15 <- boosted.results(.1, 11:15)
boosted_16_20 <- boosted.results(.1, 16:20)

## boosted depth 16, test_error 5.698%
#round(prop.table(table(boosted_16_20$res[1,]$prediction[[1]] - 1, validate_features$label), margin = 2), 3)

## PPtree -------------------------------------------
library(PPtree)
#must remove variables with no variation
pptree <- LDA.Tree(train_features$label,  
                   train_features %>% 
                     ungroup() %>% 
                     select(-id, -label, -col_1, -row_1, -row_2, -col_2, -col_27, -col_28))

pptree.predict <- PP.classify(validate_features %>% 
                              ungroup() %>% 
                              select(-id, -label, -col_1, -row_1, -row_2, -col_2, -col_27, -col_28),
                            validate_features$label,
                            pptree)

#31% validation error 
#mean(as.character(pptree.predict$predict.class) != validate_features$label)

## submodels 3, 5, 9, 4 ----------------------------
rf_35 <- randomForest(factor(label) ~ ., 
                   data = train_features %>% select(-id) %>% filter(label %in% c("3", "5")),
                   ntree = 1000)

rf_49 <- randomForest(factor(label) ~ ., 
                      data = train_features %>% select(-id) %>% filter(label %in% c("4", "9")),
                      ntree = 1000)


## chain random forests
rf.pred_35 <- predict(rf_35, validate_features[which(as.character(rf.predict) %in% c("3", "5")),])
rf.pred_49 <- predict(rf_49, validate_features[which(as.character(rf.predict) %in% c("4", "9")),])

rf.pred_combo <- rf.predict
rf.pred_combo[which(as.character(rf.predict) %in% c("3", "5"))] <- rf.pred_35
rf.pred_combo[which(as.character(rf.predict) %in% c("4", "9"))] <- rf.pred_49

#7.65% validation error 
#mean(as.character(rf.pred_combo) != validate_features$label)

## save models because they take a long time to run ----------------------------
save(rf, rf_35, rf_49, file = "written_results/models.RData")
save(rf.predict, rf.pred_combo, file = "written_results/predict.RData")
save(boosted_16_20, file = "written_results/boosted.RData")

