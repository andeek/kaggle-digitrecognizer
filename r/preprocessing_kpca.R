#preprocessing file
#feature creation through kernel pca
#make sure to run validation first
#depends on: preprocessing_validation.R

train %>% 
  select(-label) %>% 
  mutate_each(funs(./255), starts_with("pixel")) %>% #scale to [0,1]
  mutate_each(funs(scale(., scale = FALSE)), starts_with("pixel")) -> train.std #center

means <- train %>% 
  select(-label) %>% 
  mutate_each(funs(./255), starts_with("pixel")) %>%
  summarise_each(funs(mean), starts_with("pixel"))

validation %>%
  ungroup() %>%
  select(-label) %>%
  mutate_each(funs(./255), starts_with("pixel")) -> validate.std

validate.std <- sapply(1:ncol(validate.std), function(i) validate.std[,i] - as.numeric(means[i]))

library(kernlab)
kpc <- kpca(~ .,
            data = train.std,
            kernel = "rbfdot",
            kpar = list(sigma=0.2),
            features = 5)

train_kpc <- rotated(kpc)
validate_kpc <- predict(kpc, validate.std)

library(class)
kpca_knn <- knn(train = train_kpc, test = validate_kpc, cl = factor(train$label), k = 17)
mean(as.numeric(as.character(kpca_knn)) != validation$label)

library(e1071)
lin.svm <- svm(label ~ ., data.frame(label = as.factor(train$label), train_kpc), kernel = "linear")
lin.svm.pred <- predict(lin.svm, validate_kpc)
mean(as.numeric(as.character(lin.svm.pred)) != validation$label)

rad.svm <- svm(label ~ ., data.frame(label = as.factor(train$label), train_kpc))
rad.svm.pred <- predict(rad.svm, validate_kpc)
mean(as.numeric(as.character(rad.svm.pred)) != validation$label)


