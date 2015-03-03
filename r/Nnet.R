#Neural networks 
#need train_features and validate_features
#output is predictions on validate_features 

library(nnet)
library(caret)

#try 10 hidden nodes for the 10 different classes? 
set.seed(503503503)
time.start <- proc.time()
#nnet110 <- nnet(as.factor(label) ~ ., data = train_features[,-1], newdata = validate_features[,-c(1,2)], size = 10, maxiter = 100, trace = T)
#pred.nnet110 <- predict(nnet110,newdata= validate_features[,-c(1,2)],type = 'class')
avnnet10 <-avNNet(as.factor(label) ~ ., data = train_features[,-1], newdata = validate_features[,-c(1,2)],size = 10, allowParallel = F, maxiter = 1000, repeats = 10, trace = T, abstol = 1e-10)
pred.avnnet10 <- predict.avNNet(avnnet10,newdata = validate_features[,-c(1,2)],type = 'class')
time.end <- proc.time() - time.start

1 - length(which(pred.avnnet10 == validate_features$label)) / nrow(validate_features)
table(validate_features$label, pred.avnnet10)

# try 5 hidden nodes
time.start <- proc.time()
avnnet5 <-avNNet(as.factor(label) ~ ., data = train_features[,-1], size = 5, allowParallel = T, maxiter = 10000, repeats = 10, trace = F)
pred.avnnet5 <- predict.avNNet(avnnet5,newdata = validate_features[,-1],type = 'class')
time.end <- proc.time() - time.start

1 - length(which(pred.avnnet5 == validate_features$label)) / nrow(validate_features)
table(validate_features$label, pred.avnnet5)

#1 hidden network
nnet110 <- nnet(as.factor(label) ~ ., data = train_features[,-1], newdata = validate_features[,-c(1,2)], size = 10, maxiter = 10000, trace = T, abstol = 1e-10)
pred.nnet110 <- predict(nnet110,newdata= validate_features[,-c(1,2)],type = 'class')
table(validate_features$label, pred.nnet110)

#convolution nn
