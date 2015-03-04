#Neural Nets on Raw Data

dim(train)
dim(validation)

library(nnet)

nn.all2 <- nnet(as.factor(label)~., data = train, size = 2, MaxNWts = 2000)
pred.n.all2 <- predict(nn.all2, newdata = validation[-1],type='class')
