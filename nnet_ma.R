#Neural networks on new _ma data

library(nnet)
library(caret)

nnet.ma.10 <- nnet(as.factor(label) ~ ., data = train_features_ma[,-2], size = 10)

p.nnet.ma.10 <- predict(nnet.ma.10, newdata=validate_features_ma[, -c(1,2)], type = 'class')

table(validate_features_ma$label, p.nnet.ma.10)

length(which(validate_features_ma$label == p.nnet.ma.10)) / nrow(validate_features_ma)

nnet.ma.100 <- nnet(as.factor(label) ~ ., data = train_features_ma[,-2], size = 100, MaxNWts = 5000)

p.nnet.ma.100 <- predict(nnet.ma.100, newdata=validate_features_ma[, -c(1,2)], type = 'class')

table(validate_features_ma$label, p.nnet.ma.100)

1- length(which(validate_features_ma$label == p.nnet.ma.100)) / nrow(validate_features_ma)

nnet.ma.200 <- nnet(as.factor(label) ~ ., data = train_features_ma[,-2], size = 200, MaxNWts = 10000, maxit = 1000)

p.nnet.ma.200 <- predict(nnet.ma.200, newdata=validate_features_ma[, -c(1,2)], type = 'class')

table(validate_features_ma$label, p.nnet.ma.200)

length(which(validate_features_ma$label == p.nnet.ma.200)) / nrow(validate_features_ma)
