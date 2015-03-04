#K nearest neighbors 
#need train_features and validate_features
#output is predictions on validate_features 

#notes: k = 11 is best so far, with an error rate of approx .111, which is not great. 
library(kknn)
library(ggplot2)
#start with 5 nearest neighbors. 

knn5 <- kknn(as.factor(label)~., train = train_features[,-1], test = validate_features[,-1], k = 5)

plot_res_knn5 <- ggplot(data=NULL, aes(x = as.factor(validate_features$label), y = as.factor(knn5$fitted.values))) + 
  geom_tile(alpha = .01, fill = 'blue') + theme_bw() + labs(x="True Value", y="Value Classified as", title = "K Nearest Neighbors, k = 5") + 
  theme(aspect.ratio = 1, panel.grid.minor = element_blank(),panel.grid.major = element_blank())
#proportion misclassified. at 0.106651 as of 8:04 pm Mon Mar 2
prop_mclass_knn5 <- 1 - length(which(knn5$fitted.values == validate_features$label)) / nrow(validate_features)

# confusion matrix
cmat_knn5 <- table(validate_features$label, knn5$fitted.values)

#next, do with 7 nearest neighbors. 

knn7 <- kknn(as.factor(label)~., train = train_features[,-1], test = validate_features[,-1], k = 7)

plot_res_knn7 <- ggplot(data=NULL, aes(x = as.factor(validate_features$label), y = as.factor(knn7$fitted.values))) + 
  geom_tile(alpha = .01, fill = 'blue') + theme_bw() + labs(x="True Value", y="Value Classified as", title = "K Nearest Neighbors, k = 7") + 
  theme(aspect.ratio = 1, panel.grid.minor = element_blank(),panel.grid.major = element_blank())

#proportion misclassified (slightly lower than k = 5) at 0.09729961 as of 8:06pm Mon Mar 2
prop_mclass_knn7 <- 1 - length(which(knn7$fitted.values == validate_features$label)) / nrow(validate_features)

# confusion matrix
cmat_knn7 <- table(validate_features$label, knn7$fitted.values)

#next, do with 9 nearest neighbors. 

knn9 <- kknn(as.factor(label)~., train = train_features[,-1], test = validate_features[,-1], k = 9)

plot_res_knn9 <- ggplot(data=NULL, aes(x = as.factor(validate_features$label), y = as.factor(knn9$fitted.values))) + 
  geom_tile(alpha = .01, fill = 'blue') + theme_bw() + labs(x="True Value", y="Value Classified as", title = "K Nearest Neighbors, k = 9") + 
  theme(aspect.ratio = 1, panel.grid.minor = element_blank(),panel.grid.major = element_blank())

#proportion misclassified. 0.09551364 as of 8:08pm Mon Mar 2
prop_mclass_knn9 <- 1 - length(which(knn9$fitted.values == validate_features$label)) / nrow(validate_features)

# confusion matrix
cmat_knn9 <- table(validate_features$label, knn9$fitted.values)

#next, do with 11 nearest neighbors. 

knn11 <- kknn(as.factor(label)~., train = train_features[,-1], test = validate_features[,-1], k = 11)

plot_res_knn11 <- ggplot(data=NULL, aes(x = as.factor(validate_features$label), y = as.factor(knn11$fitted.values))) + 
  geom_tile(alpha = .01, fill = 'blue') + theme_bw() + labs(x="True Value", y="Value Classified as", title = "K Nearest Neighbors, k = 11") + 
  theme(aspect.ratio = 1, panel.grid.minor = element_blank(),panel.grid.major = element_blank())

#proportion misclassified. 0.09544221 as of 8:10pm  Mon Mar 2
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

#now do for k=1

knn1 <- kknn(as.factor(label)~., train = train_features[,-1], test = validate_features[,-1], k = 1)

plot_res_knn1 <-ggplot(data=NULL, aes(x = as.factor(validate_features$label), y = as.factor(knn1$fitted.values))) + 
  geom_tile(alpha = .01, fill = 'blue') + theme_bw() + labs(x="True Value", y="Value Classified as", title = "K Nearest Neighbors, k = 1") + 
  theme(aspect.ratio = 1, panel.grid.major = element_blank())

#proportion misclassified. 0.1066581
prop_mclass_knn1 <- 1 - length(which(knn1$fitted.values == validate_features$label)) / nrow(validate_features)

# confusion matrix
cmat_knn1 <- table(validate_features$label, knn1$fitted.values)


#output best predictions. 
knn11$fitted.values

#Making predictors for just 4&9 and 3&5
idx.val <- which(knn11$fitted.values %in% c(4,9)) 
knn11.49 <- kknn(as.factor(label)~., train = subset(train_features[,-1], label %in% c(4,9)), test = validate_features[idx.val,-1], k = 11)

1 - length(which(knn11.49$fitted.values == validate_features$label[idx.val])) / length(idx.val)


ggplot(data=NULL, aes(x = as.factor(validate_features$label[idx.val]), y = as.factor(knn11.49$fitted.values))) + 
  geom_tile(alpha = .01, fill = 'blue') + theme_bw() + labs(x="True Value", y="Value Classified as", title = "K Nearest Neighbors, k = 11") + 
  theme(aspect.ratio = 1, panel.grid.major = element_blank())

idx.val2 <- which(knn11$fitted.values %in% c(3,5)) 

knn11.35 <- kknn(as.factor(label)~., train = subset(train_features[,-1], label %in% c(3,5)), test = validate_features[idx.val2,-1], k = 11)

ggplot(data=NULL, aes(x = as.factor(validate_features$label[idx.val2]), y = as.factor(knn11.35$fitted.values))) + 
  geom_tile(alpha = .005, fill = 'blue') + theme_bw() + labs(x="True Value", y="Value Classified as", title = "K Nearest Neighbors, k = 11") + 
  theme(aspect.ratio = 1, panel.grid.major = element_blank())
1 - length(which(knn11.35$fitted.values == validate_features$label[idx.val2])) / length(idx.val2)

#next, do with 11 nearest neighbors and a different kernel. 

knn11.tri <- kknn(as.factor(label)~., train = train_features[,-1], test = validate_features[,-1], k = 11, kernel = 'triangular')

plot_res_knn11.tri <- ggplot(data=NULL, aes(x = as.factor(validate_features$label), y = as.factor(knn11.tri$fitted.values))) + 
  geom_tile(alpha = .01, fill = 'blue') + theme_bw() + labs(x="True Value", y="Value Classified as", title = "K Nearest Neighbors, k = 11, triangle kernel") + 
  theme(aspect.ratio = 1, panel.grid.minor = element_blank(),panel.grid.major = element_blank())

#proportion misclassified. 0.094299 as of 10:38pm  Mon Mar 2
prop_mclass_knn11.tri <- 1 - length(which(knn11.tri$fitted.values == validate_features$label)) / nrow(validate_features)

# confusion matrix
cmat_knn11.tri<- table(validate_features$label, knn11.tri$fitted.values)

knn11.rank <- kknn(as.factor(label)~., train = train_features[,-1], test = validate_features[,-1], k = 11, kernel = 'rank')

plot_res_knn11.rank <- ggplot(data=NULL, aes(x = as.factor(validate_features$label), y = as.factor(knn11.rank$fitted.values))) + 
  geom_tile(alpha = .01, fill = 'blue') + theme_bw() + labs(x="True Value", y="Value Classified as", title = "K Nearest Neighbors, k = 11, rank kernel") + 
  theme(aspect.ratio = 1, panel.grid.minor = element_blank(),panel.grid.major = element_blank())

#proportion misclassified. 0.094299 as of 10:38pm  Mon Mar 2
prop_mclass_knn11.rank <- 1 - length(which(knn11.rank$fitted.values == validate_features$label)) / nrow(validate_features)

# confusion marankx
cmat_knn11.rank<- table(validate_features$label, knn11.rank$fitted.values)

knn11.epan <- kknn(as.factor(label)~., train = train_features[,-1], test = validate_features[,-1], k = 11, kernel = "epanechnikov")

plot_res_knn11.epan <- ggplot(data=NULL, aes(x = as.factor(validate_features$label), y = as.factor(knn11.epan$fitted.values))) + 
  geom_tile(alpha = .01, fill = 'blue') + theme_bw() + labs(x="True Value", y="Value Classified as", title = "K Nearest Neighbors, k = 11, epan kernel") + 
  theme(aspect.ratio = 1, panel.grid.minor = element_blank(),panel.grid.major = element_blank())

#proportion misclassified. 0.09472782 as of 10:38pm  Mon Mar 2
prop_mclass_knn11.epan <- 1 - length(which(knn11.epan$fitted.values == validate_features$label)) / nrow(validate_features)

# confusion maepanx
cmat_knn11.epan<- table(validate_features$label, knn11.epan$fitted.values)

knn11.rect <- kknn(as.factor(label)~., train = train_features[,-1], test = validate_features[,-1], k = 11, kernel = "rectangular")

plot_res_knn11.rect <- ggplot(data=NULL, aes(x = as.factor(validate_features$label), y = as.factor(knn11.rect$fitted.values))) + 
  geom_tile(alpha = .01, fill = 'blue') + theme_bw() + labs(x="True Value", y="Value Classified as", title = "K Nearest Neighbors, k = 11, rect kernel") + 
  theme(aspect.ratio = 1, panel.grid.minor = element_blank(),panel.grid.major = element_blank())

#proportion misclassified. 0.09472782 as of 10:38pm  Mon Mar 2
prop_mclass_knn11.rect <- 1 - length(which(knn11.rect$fitted.values == validate_features$label)) / nrow(validate_features)

# confusion marectx
cmat_knn11.rect<- table(validate_features$label, knn11.rect$fitted.values)
