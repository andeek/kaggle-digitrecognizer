#preprocessing file
#feature creation
#make sure to run validation first
#depends on: preprocessing_validation.R

train %>%
  mutate(id = 1:n()) %>%
  gather(location, value, -label, -id) %>%
  mutate(location = as.numeric(gsub("pixel", "", location))) %>% 
  cbind(expand.grid(row = 1:28, column = 1:28)) -> train.rc

train.rc %>%
  group_by(id, row, label) %>%
  summarise(row_num = sum(value > 0)) %>%
  ungroup() %>%
  mutate(row = paste0("row_", row)) %>%
  spread(row, row_num) -> row_summary

train.rc %>%
  group_by(id, column, label) %>%
  summarise(col_num = sum(value > 0)) %>%
  ungroup() %>%
  mutate(column = paste0("col_", column)) %>%
  spread(column, col_num) -> column_summary

train.rc %>%
  group_by(id, label) %>%
  summarise(tot_num = sum(value > 0)) -> tot_summary


train.rc %>%
  filter(row %in% 14:15, column %in% 14:15) %>%
  group_by(id, label) %>%
  summarise(num_2 = sum(value > 0)) -> num_2_summary

train.rc %>%
  filter(row %in% 13:16, column %in% 13:16) %>%
  group_by(id, label) %>%
  summarise(num_4 = sum(value > 0)) -> num_4_summary

train.rc %>%
  filter(row %in% 11:18, column %in% 11:18) %>%
  group_by(id, label) %>%
  summarise(num_8 = sum(value > 0)) -> num_8_summary

train.rc %>%
  filter(row %in% 7:22, column %in% 7:22) %>%
  group_by(id, label) %>%
  summarise(num_16 = sum(value > 0)) -> num_16_summary

inner_join(tot_summary, row_summary) %>%
  inner_join(column_summary) %>%
  inner_join(num_2_summary) %>%
  inner_join(num_4_summary) %>%
  inner_join(num_8_summary) %>%
  inner_join(num_16_summary) -> train_features







