#PCA on our features. Didn't really improve KNN.  :( 

train.pc <- princomp(train[,-1])
plot(train.pc)
biplot(train.pc)

str(train.pc$loadings)
print(train.pc$loadings,sort=T)[1:5]
dim(train.pc$scores)

str(summary(train.pc))

etrain <- eigen(t(as.matrix(train[,-1]))%*% as.matrix(train[,-1])/nrow(train))
summary(etrain$values)

train.feat.pc <- princomp(train_features[,-c(1,2)])
plot(train.feat.pc)
summary(train.feat.pc)
dim(train.feat.pc$loadings)

pcs.train.feat <- train.feat.pc$loadings[,1:10]

pcs.train.feat2 <- as.matrix(train_features[,-c(1,2)]) %*% as.matrix(pcs.train.feat)
pcs.train.feat2 <- data.frame(label = train_features$label, pcs.train.feat2)
pcs.val.feat <- as.matrix(validate_features[,-c(1,2)]) %*% as.matrix(pcs.train.feat)
pcs.val.feat <- data.frame(pcs.val.feat)

#KNN on the PCs
library(kknn)
knn.pc.11 <- kknn(as.factor(label)~., train = pcs.train.feat2, test = pcs.val.feat, k = 11)
ggplot(data=NULL, aes(x = as.factor(validate_features$label), y = as.factor(knn.pc.11$fitted.values))) + 
  geom_tile(alpha = .01, fill = 'blue') + theme_bw() + labs(x="True Value", y="Value Classified as", title = "K Nearest Neighbors on PCs, k = 11") + 
  theme(aspect.ratio = 1, panel.grid.minor = element_blank(),panel.grid.major = element_blank())
#proportion misclassified. at 0.106651 as of 8:04 pm Mon Mar 2
1 - length(which(knn.pc.11$fitted.values == validate_features$label)) / nrow(validate_features)

# confusion matrix
cmat_knn5 <- table(validate_features$label, knn5$fitted.values)

knn.pc.3 <- kknn(as.factor(label)~., train = pcs.train.feat2, test = pcs.val.feat, k = 3)
ggplot(data=NULL, aes(x = as.factor(validate_features$label), y = as.factor(knn.pc.3$fitted.values))) + 
  geom_tile(alpha = .01, fill = 'blue') + theme_bw() + labs(x="True Value", y="Value Classified as", title = "K Nearest Neighbors on PCs, k = 3") + 
  theme(aspect.ratio = 1, panel.grid.minor = element_blank(),panel.grid.major = element_blank())
#proportion misclassified. at 0.106651 as of 8:04 pm Mon Mar 2
1 - length(which(knn.pc.3$fitted.values == validate_features$label)) / nrow(validate_features)

knn.pc.15 <- kknn(as.factor(label)~., train = pcs.train.feat2, test = pcs.val.feat, k = 15)
ggplot(data=NULL, aes(x = as.factor(validate_features$label), y = as.factor(knn.pc.15$fitted.values))) + 
  geom_tile(alpha = .01, fill = 'blue') + theme_bw() + labs(x="True Value", y="Value Classified as", title = "K Nearest Neighbors on PCs, k = 15") + 
  theme(aspect.ratio = 1, panel.grid.minor = element_blank(),panel.grid.major = element_blank())
#proportion misclassified. at 0.106651 as of 8:04 pm Mon Mar 2
1 - length(which(knn.pc.15$fitted.values == validate_features$label)) / nrow(validate_features)

knn.pc.31 <- kknn(as.factor(label)~., train = pcs.train.feat2, test = pcs.val.feat, k = 31)
ggplot(data=NULL, aes(x = as.factor(validate_features$label), y = as.factor(knn.pc.11$fitted.values))) + 
  geom_tile(alpha = .01, fill = 'blue') + theme_bw() + labs(x="True Value", y="Value Classified as", title = "K Nearest Neighbors on PCs, k = 31") + 
  theme(aspect.ratio = 1, panel.grid.minor = element_blank(),panel.grid.major = element_blank())
#proportion misclassified. at 0.106651 as of 8:04 pm Mon Mar 2
1 - length(which(knn.pc.31$fitted.values == validate_features$label)) / nrow(validate_features)
