#K nearest neighbors 
#need train_features and validate_features
#output is predictions on validate_features 

#notes: k = 11 is best so far, with an error rate of approx .111, which is not great. 
library(kknn)

#start with 5 nearest neighbors. 

knn5 <- kknn(as.factor(label)~., train = train_features[,-1], test = validate_features[,-1], k = 5)

plot_res_knn5 <- ggplot(data=NULL, aes(x = as.factor(validate_features$label), y = as.factor(knn5$fitted.values))) + 
  geom_tile(alpha = .01, fill = 'blue') + theme_bw() + labs(x="True Value", y="Value Classified as", title = "K Nearest Neighbors, k = 5") + 
  theme(aspect.ratio = 1, panel.grid.minor = element_blank(),panel.grid.major = element_blank())
#proportion misclassified
prop_mclass_knn5 <- 1 - length(which(knn5$fitted.values == validate_features$label)) / nrow(validate_features)

# confusion matrix
cmat_knn5 <- table(validate_features$label, knn5$fitted.values)

#next, do with 7 nearest neighbors. 

knn7 <- kknn(as.factor(label)~., train = train_features[,-1], test = validate_features[,-1], k = 7)

plot_res_knn7 <- ggplot(data=NULL, aes(x = as.factor(validate_features$label), y = as.factor(knn7$fitted.values))) + 
  geom_tile(alpha = .01, fill = 'blue') + theme_bw() + labs(x="True Value", y="Value Classified as", title = "K Nearest Neighbors, k = 7") + 
  theme(aspect.ratio = 1, panel.grid.minor = element_blank(),panel.grid.major = element_blank())

#proportion misclassified (slightly lower than k = 5)
prop_mclass_knn7 <- 1 - length(which(knn7$fitted.values == validate_features$label)) / nrow(validate_features)

# confusion matrix
cmat_knn7 <- table(validate_features$label, knn7$fitted.values)

#next, do with 9 nearest neighbors. 

knn9 <- kknn(as.factor(label)~., train = train_features[,-1], test = validate_features[,-1], k = 9)

plot_res_knn9 <- ggplot(data=NULL, aes(x = as.factor(validate_features$label), y = as.factor(knn9$fitted.values))) + 
  geom_tile(alpha = .01, fill = 'blue') + theme_bw() + labs(x="True Value", y="Value Classified as", title = "K Nearest Neighbors, k = 9") + 
  theme(aspect.ratio = 1, panel.grid.minor = element_blank(),panel.grid.major = element_blank())

#proportion misclassified. same as k = 7
prop_mclass_knn9 <- 1 - length(which(knn9$fitted.values == validate_features$label)) / nrow(validate_features)

# confusion matrix
cmat_knn9 <- table(validate_features$label, knn9$fitted.values)

#next, do with 11 nearest neighbors. 

knn11 <- kknn(as.factor(label)~., train = train_features[,-1], test = validate_features[,-1], k = 11)

plot_res_knn11 <- ggplot(data=NULL, aes(x = as.factor(validate_features$label), y = as.factor(knn11$fitted.values))) + 
  geom_tile(alpha = .01, fill = 'blue') + theme_bw() + labs(x="True Value", y="Value Classified as", title = "K Nearest Neighbors, k = 11") + 
  theme(aspect.ratio = 1, panel.grid.minor = element_blank(),panel.grid.major = element_blank())

#proportion misclassified. about .001 smaller than k = 9
prop_mclass_knn11 <- 1 - length(which(knn11$fitted.values == validate_features$label)) / nrow(validate_features)

# confusion matrix
cmat_knn11 <- table(validate_features$label, knn11$fitted.values)

#next, do with 3 nearest neighbors. 

knn3 <- kknn(as.factor(label)~., train = train_features[,-1], test = validate_features[,-1], k = 3)

plot_res_knn3 <-ggplot(data=NULL, aes(x = as.factor(validate_features$label), y = as.factor(knn3$fitted.values))) + 
  geom_tile(alpha = .01, fill = 'blue') + theme_bw() + labs(x="True Value", y="Value Classified as", title = "K Nearest Neighbors, k = 3") + 
  theme(aspect.ratio = 1, panel.grid.minor = element_blank(),panel.grid.major = element_blank())

#proportion misclassified. same as k = 5 
prop_mclass_knn3 <- 1 - length(which(knn3$fitted.values == validate_features$label)) / nrow(validate_features)

# confusion matrix
cmat_knn3 <- table(validate_features$label, knn3$fitted.values)

#next, do with 12 nearest neighbors. 

knn12 <- kknn(as.factor(label)~., train = train_features[,-1], test = validate_features[,-1], k = 12)

plot_res_knn12 <-ggplot(data=NULL, aes(x = as.factor(validate_features$label), y = as.factor(knn12$fitted.values))) + 
  geom_tile(alpha = .01, fill = 'blue') + theme_bw() + labs(x="True Value", y="Value Classified as", title = "K Nearest Neighbors, k = 12") + 
  theme(aspect.ratio = 1, panel.grid.major = element_blank())

#proportion misclassified. same as k = 13
prop_mclass_knn12 <- 1 - length(which(knn12$fitted.values == validate_features$label)) / nrow(validate_features)

# confusion matrix
cmat_knn12 <- table(validate_features$label, knn12$fitted.values)

#just trying some ridiculously high number for shiggles
knn35 <- kknn(as.factor(label)~., train = train_features[,-1], test = validate_features[,-1], k = 35)

plot_res_knn35 <-ggplot(data=NULL, aes(x = as.factor(validate_features$label), y = as.factor(knn35$fitted.values))) + 
  geom_tile(alpha = .01, fill = 'blue') + theme_bw() + labs(x="True Value", y="Value Classified as", title = "K Nearest Neighbors, k = 35") + 
  theme(aspect.ratio = 1, panel.grid.major = element_blank())

#proportion misclassified. same as k = 13
prop_mclass_knn35 <- 1 - length(which(knn35$fitted.values == validate_features$label)) / nrow(validate_features)

# confusion matrix
cmat_knn35 <- table(validate_features$label, knn35$fitted.values)

#output best predictions. 
knn11$fitted.values

#Making predictors for just 4&9 and 3&5
idx.val <- which(knn11$fitted.values %in% c(4,9)) 
knn11.49 <- kknn(as.factor(label)~., train = subset(train_features[,-1], label %in% c(4,9)), test = validate_features[idx.val,-1], k = 11)

1 - length(which(knn11.49$fitted.values == validate_features$label[idx.val])) / length(idx.val)


ggplot(data=NULL, aes(x = as.factor(validate_features$label[idx.val]), y = as.factor(knn7.49$fitted.values))) + 
  geom_tile(alpha = .01, fill = 'blue') + theme_bw() + labs(x="True Value", y="Value Classified as", title = "K Nearest Neighbors, k = 7") + 
  theme(aspect.ratio = 1, panel.grid.major = element_blank())

idx.val2 <- which(knn11$fitted.values %in% c(3,5)) 

knn11.35 <- kknn(as.factor(label)~., train = subset(train_features[,-1], label %in% c(3,5)), test = validate_features[idx.val2,-1], k = 11)


