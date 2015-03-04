#New features, KNN 

library(kknn)

knn.ma11 <- kknn(as.factor(label) ~ ., train =train_features_ma[,-2], test = validate_features_ma[,-c(1,2)], k = 11)

ggplot(data=NULL, aes(x = as.factor(validate_features_ma$label), y = as.factor(knn.ma11$fitted.values))) + 
  geom_tile(alpha = .01, fill = 'blue') + theme_bw() + labs(x="True Value", y="Value Classified as", title = "K Nearest Neighbors, k = 11") + 
  theme(aspect.ratio = 1, panel.grid.minor = element_blank(),panel.grid.major = element_blank())
#proportion misclassified. at 0.106651 as of 8:04 pm Mon Mar 2
1 - length(which(knn.ma11$fitted.values == validate_features$label)) / nrow(validate_features)

# confusion matrix
table(validate_features_ma$label, knn.ma11$fitted.values)

knn.ma11.tri <- kknn(as.factor(label) ~ ., train =train_features_ma[,-2], test = validate_features_ma[,-c(1,2)], k = 11,kernel = 'triangular')

ggplot(data=NULL, aes(x = as.factor(validate_features_ma$label), y = as.factor(knn.ma11.tri$fitted.values))) + 
  geom_tile(alpha = .01, fill = 'blue') + theme_bw() + labs(x="True Value", y="Value Classified as", title = "K Nearest Neighbors, tri kernel, k = 11") + 
  theme(aspect.ratio = 1, panel.grid.minor = element_blank(),panel.grid.major = element_blank())
#proportion misclassified. at 0.106651 as of 8:04 pm Mon Mar 2
1 - length(which(knn.ma11.tri$fitted.values == validate_features$label)) / nrow(validate_features)

# confusion matrix
table(validate_features_ma$label, knn.ma11.tri$fitted.values)

knn.ma3 <- kknn(as.factor(label) ~ ., train =train_features_ma[,-2], test = validate_features_ma[,-c(1,2)], k = 3)

ggplot(data=NULL, aes(x = as.factor(validate_features_ma$label), y = as.factor(knn.ma3$fitted.values))) + 
  geom_tile(alpha = .01, fill = 'blue') + theme_bw() + labs(x="True Value", y="Value Classified as", title = "K Nearest Neighbors, k = 3") + 
  theme(aspect.ratio = 1, panel.grid.minor = element_blank(),panel.grid.major = element_blank())
#proportion misclassified. at 0.106651 as of 8:04 pm Mon Mar 2
1 - length(which(knn.ma3$fitted.values == validate_features$label)) / nrow(validate_features)

# confusion matrix
table(validate_features_ma$label, knn.ma3$fitted.values)

knn.ma3.tri <- kknn(as.factor(label) ~ ., train =train_features_ma[,-2], test = validate_features_ma[,-c(1,2)], k = 3,kernel = 'triangular')

ggplot(data=NULL, aes(x = as.factor(validate_features_ma$label), y = as.factor(knn.ma3.tri$fitted.values))) + 
  geom_tile(alpha = .01, fill = 'blue') + theme_bw() + labs(x="True Value", y="Value Classified as", title = "K Nearest Neighbors, tri kernel, k = 3") + 
  theme(aspect.ratio = 1, panel.grid.minor = element_blank(),panel.grid.major = element_blank())
#proportion misclassified. at 0.106651 as of 8:04 pm Mon Mar 2
1 - length(which(knn.ma3.tri$fitted.values == validate_features$label)) / nrow(validate_features)

# confusion matrix
table(validate_features_ma$label, knn.ma3.tri$fitted.values)


knn.ma9 <- kknn(as.factor(label) ~ ., train =train_features_ma[,-2], test = validate_features_ma[,-c(1,2)], k = 9)

ggplot(data=NULL, aes(x = as.factor(validate_features_ma$label), y = as.factor(knn.ma9$fitted.values))) + 
  geom_tile(alpha = .01, fill = 'blue') + theme_bw() + labs(x="True Value", y="Value Classified as", title = "K Nearest Neighbors, k = 9") + 
  theme(aspect.ratio = 1, panel.grid.minor = element_blank(),panel.grid.major = element_blank())
#proportion misclassified. at 0.106651 as of 8:04 pm Mon Mar 2
1 - length(which(knn.ma9$fitted.values == validate_features$label)) / nrow(validate_features)

# confusion matrix
table(validate_features_ma$label, knn.ma9$fitted.values)

knn.ma9.tri <- kknn(as.factor(label) ~ ., train =train_features_ma[,-2], test = validate_features_ma[,-c(1,2)], k = 9,kernel = 'triangular')

ggplot(data=NULL, aes(x = as.factor(validate_features_ma$label), y = as.factor(knn.ma9.tri$fitted.values))) + 
  geom_tile(alpha = .01, fill = 'blue') + theme_bw() + labs(x="True Value", y="Value Classified as", title = "K Nearest Neighbors, tri kernel, k = 9") + 
  theme(aspect.ratio = 1, panel.grid.minor = element_blank(),panel.grid.major = element_blank())
#proportion misclassified. at 0.106651 as of 8:04 pm Mon Mar 2
1 - length(which(knn.ma9.tri$fitted.values == validate_features$label)) / nrow(validate_features)

# confusion matrix
table(validate_features_ma$label, knn.ma9.tri$fitted.values)

knn.ma9.rect <- kknn(as.factor(label) ~ ., train =train_features_ma[,-2], test = validate_features_ma[,-c(1,2)], k = 9,kernel = 'rectangular')

ggplot(data=NULL, aes(x = as.factor(validate_features_ma$label), y = as.factor(knn.ma9.rect$fitted.values))) + 
  geom_tile(alpha = .01, fill = 'blue') + theme_bw() + labs(x="True Value", y="Value Classified as", title = "K Nearest Neighbors, rect kernel, k = 9") + 
  theme(aspect.ratio = 1, panel.grid.minor = element_blank(),panel.grid.major = element_blank())
#proportion misclassified. at 0.106651 as of 8:04 pm Mon Mar 2
1 - length(which(knn.ma9.rect$fitted.values == validate_features$label)) / nrow(validate_features)

# confusion marectx
table(validate_features_ma$label, knn.ma9.rect$fitted.values)


knn.ma11.rect <- kknn(as.factor(label) ~ ., train =train_features_ma[,-2], test = validate_features_ma[,-c(1,2)], k = 11,kernel = 'rectangular')

ggplot(data=NULL, aes(x = as.factor(validate_features_ma$label), y = as.factor(knn.ma11.rect$fitted.values))) + 
  geom_tile(alpha = .01, fill = 'blue') + theme_bw() + labs(x="True Value", y="Value Classified as", title = "K Nearest Neighbors, rect kernel, k = 11") + 
  theme(aspect.ratio = 1, panel.grid.minor = element_blank(),panel.grid.major = element_blank())
#proportion misclassified. at 0.106651 as of 8:04 pm Mon Mar 2
1 - length(which(knn.ma11.rect$fitted.values == validate_features$label)) / nrow(validate_features)

# confusion marectx
table(validate_features_ma$label, knn.ma11.rect$fitted.values)


