#preprocessing file
#summaries and exploratory data analysis
#make sure to run validation first
#depends on: preprocessing_validation.R

#some small summary statistics --------------------
train %>%
  gather(pixel, value, -label) %>%
  mutate(value = as.numeric(value)) %>%
  group_by(label) %>%
  summarise_each(funs(mean(.), max(.), min(.), median(.), sum(. > 0)/n()), value) -> summaries

