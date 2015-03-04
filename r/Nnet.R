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
avnnet10 <-avNNet(as.factor(label) ~ ., data = train_features[,-1], newdata = validate_features[,-c(1,2)],size = 10, allowParallel = F, maxit = 10000, repeats = 10, trace = T, abstol = 1e-10)
pred.avnnet10 <- predict.avNNet(avnnet10,newdata = validate_features[,-c(1,2)],type = 'class')
time.end <- proc.time() - time.start

1 - length(which(pred.avnnet10 == validate_features$label)) / nrow(validate_features)
table(validate_features$label, pred.avnnet10)

# try 5 hidden nodes
time.start <- proc.time()
avnnet5 <-avNNet(as.factor(label) ~ ., data = train_features[,-1], size = 5, allowParallel = T, maxiter = 10000, repeats = 10, trace = F,abstol=1e-10)
pred.avnnet5 <- predict.avNNet(avnnet5,newdata = validate_features[,-1],type = 'class')
time.end <- proc.time() - time.start

1 - length(which(pred.avnnet5 == validate_features$label)) / nrow(validate_features)
table(validate_features$label, pred.avnnet5)

#1 hidden network
nnet110 <- nnet(as.factor(label) ~ ., data = train_features[,-1], newdata = validate_features[,-c(1,2)], size = 10, maxit = 100000, trace = T, abstol = 1e-10)
pred.nnet110 <- predict(nnet110,newdata= validate_features[,-c(1,2)],type = 'class')
table(validate_features$label, pred.nnet110)
1 - length(which(pred.nnet110 == validate_features$label)) / nrow(validate_features)


#convolution nn
nnet15.c <- nnet(as.factor(label) ~ ., data = train_features[,-1], newdata = validate_features[,-c(1,2)], size = 5, maxit = 1000, trace = T,skip=T, MaxNWts = 2000)
pred.nnet15.c <- predict(nnet15.c,newdata= validate_features[,-c(1,2)],type = 'class')
table(validate_features$label, pred.nnet15.c)
1 - length(which(pred.nnet15.c == validate_features$label)) / nrow(validate_features)

nnet110.c <- nnet(as.factor(label) ~ ., data = train_features[,-1], newdata = validate_features[,-c(1,2)], size = 10, maxit = 1000, trace = T,skip=T, MaxNWts = 2000)
pred.nnet110.c <- predict(nnet110.c,newdata= validate_features[,-c(1,2)],type = 'class')
table(validate_features$label, pred.nnet110.c)
1 - length(which(pred.nnet110.c == validate_features$label)) / nrow(validate_features)

av.20nnet.c10 <- avNNet(as.factor(label) ~ ., data = train_features[,-1], newdata = validate_features[,-c(1,2)], size = 10, maxit = 1000, trace = T,skip=T, MaxNWts = 2000, repeats = 20)
pred.avnnet20.c10 <- predict.avNNet(av.20nnet.c10,newdata = validate_features[,-c(1,2)],type = 'class')
table(validate_features$label, pred.avnnet20.c10)
1 - length(which(pred.nnet110.c == validate_features$label)) / nrow(validate_features)

nnet110.c2 <- nnet(as.factor(label) ~ ., data = train_features[,-1], newdata = validate_features[,-c(1,2)], size = 10, maxit = 10000, trace = T,skip=T, MaxNWts = 2000)
pred.nnet110.c2 <- predict(nnet110.c2,newdata= validate_features[,-c(1,2)],type = 'class')
table(validate_features$label, pred.nnet110.c2)
1 - length(which(pred.nnet110.c2 == validate_features$label)) / nrow(validate_features)


av.20nnet.c102 <- avNNet(as.factor(label) ~ ., data = train_features[,-1], newdata = validate_features[,-c(1,2)], size = 10, maxit = 10000, trace = T,skip=T, MaxNWts = 2000, repeats = 20)
pred.avnnet20.c102 <- predict.avNNet(av.20nnet.c102,newdata = validate_features[,-c(1,2)],type = 'class')
table(validate_features$label, pred.avnnet20.c102)
1 - length(which(predavnnet20.c102 == validate_features$label)) / nrow(validate_features)
