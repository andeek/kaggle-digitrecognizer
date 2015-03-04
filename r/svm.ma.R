library(klaR)

svm.ma <- svmlight(as.factor(label)~., data = train_features_ma[,-2], type='C', pathsvm = "~/svm_light/",svm.options = "-t 2 -g .001")
pred.svm.ma <- predict(svm.ma,newdata= validate_features_ma[,-c(1,2)])
