#preprocessing file
#feature creation
#make sure to run validation first
#depends on: preprocessing_validation.R

#reusible function for creating features
create_features <- function(data) {
  data %>%
    ungroup() %>%
    mutate(id = 1:n()) %>%
    gather(location, value, -label, -id) %>%
    mutate(location = as.numeric(gsub("pixel", "", location))) %>% 
    arrange(id, location) %>%
    cbind(matrix(1, nrow = nrow(data), ncol = 1) %x% data.matrix(expand.grid(row = 1:28, column = 1:28))) -> data.rc
  
  names(data.rc)[c(ncol(data.rc) - 1, ncol(data.rc))] <- c("row", "column")
  
  data.rc %>%
    filter(row > 2, row < 27) %>% #get rid of empty rows
    group_by(label, id, row) %>%
    summarise(row_num = sum(value > 0)) %>%
    ungroup() %>%
    mutate(row = paste0("row_", row)) %>%
    spread(row, row_num) -> row_summary
  
  data.rc %>%
    filter(column > 2, column < 27) %>% #get rid of empty columns
    group_by(id, column, label) %>%
    summarise(col_num = sum(value > 0)) %>%
    ungroup() %>%
    mutate(column = paste0("col_", column)) %>%
    spread(column, col_num) -> column_summary
  
  data.rc %>%
    group_by(id, label) %>%
    summarise(tot_num = sum(value > 0)) -> tot_summary

  data.rc %>%
    filter(row %in% 14:15, column %in% 14:15) %>%
    group_by(id, label) %>%
    summarise(num_2 = sum(value > 0)) -> num_2_summary
  
  data.rc %>%
    filter(row %in% 13:16, column %in% 13:16) %>%
    group_by(id, label) %>%
    summarise(num_4 = sum(value > 0)) -> num_4_summary
  
  data.rc %>%
    filter(row %in% 11:18, column %in% 11:18) %>%
    group_by(id, label) %>%
    summarise(num_8 = sum(value > 0)) -> num_8_summary
  
  data.rc %>%
    filter(row %in% 7:22, column %in% 7:22) %>%
    group_by(id, label) %>%
    summarise(num_16 = sum(value > 0)) -> num_16_summary
  
  data.rc %>%
    filter(row %in% 1:8, column %in% 1:8) %>%
    group_by(id, label) %>%
    summarise(num_top_left = sum(value > 0)) -> num_top_left_summary
  
  data.rc %>%
    filter(row %in% 21:28, column %in% 1:8) %>%
    group_by(id, label) %>%
    summarise(num_bottom_left = sum(value > 0)) -> num_bottom_left_summary
  
  data.rc %>%
    filter(row %in% 1:8, column %in% 21:28) %>%
    group_by(id, label) %>%
    summarise(num_top_right = sum(value > 0)) -> num_top_right_summary
  
  data.rc %>%
    filter(row %in% 21:28, column %in% 21:28) %>%
    group_by(id, label) %>%
    summarise(num_bottom_right = sum(value > 0)) -> num_bottom_right_summary
  
  inner_join(tot_summary, row_summary) %>%
    inner_join(column_summary) %>%
    inner_join(num_2_summary) %>%
    inner_join(num_4_summary) %>%
    inner_join(num_8_summary) %>%
    inner_join(num_16_summary) %>% 
    inner_join(num_top_right_summary) %>% 
    inner_join(num_top_left_summary) %>% 
    inner_join(num_bottom_right_summary) %>% 
    inner_join(num_bottom_left_summary) %>% 
    return()
}

#create features for 3 datasets
train_features <- create_features(train)
validate_features <- create_features(validation)
test_features <- create_features(test)








