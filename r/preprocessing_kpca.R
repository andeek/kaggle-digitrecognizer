#preprocessing file
#feature creation through kernel pca
#make sure to run validation first
#depends on: preprocessing_validation.R

train %>% 
  select(-label)

library(kernlab)
kpc <- kpca(~ .,
            data = train %>% select(-label),
            kernel = "rbfdot",
            kpar = list(sigma=0.2),
            features = 5)