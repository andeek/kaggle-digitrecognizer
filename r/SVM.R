#svmlight #linear kernel default

library(klaR)

svm1 <- svmlight(as.factor(label)~., data = train_features[,-1], type='C', pathsvm = "~/svm_light/")

pred.svm1 <- predict(svm1,newdata= validate_features[,-c(1,2)])

table(validate_features$label, pred.svm1$class)

1 - length(which(validate_features$label == pred.svm1$class)) / nrow(validate_features)

svm2 <- svmlight(as.factor(label)~., data = train_features[,-1], type='C', pathsvm = "~/svm_light/", svm.options = "-t 1 -d 2")

pred.svm2 <- predict(svm2,newdata= validate_features[,-c(1,2)])
table(validate_features$label, pred.svm2$class)
1 - length(which(validate_features$label == pred.svm2$class)) / nrow(validate_features)

svm3 <- svmlight(as.factor(label)~., data = train_features[,-1], type='C', pathsvm = "~/svm_light/", svm.options = "-w .01")
pred.svm3 <- predict(svm3,newdata= validate_features[,-c(1,2)])
table(validate_features$label, pred.svm3$class)
1 - length(which(validate_features$label == pred.svm3$class)) / nrow(validate_features)
