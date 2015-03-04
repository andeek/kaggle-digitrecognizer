#preprocessing file
#feature ma creation
#make sure to run validation first
#depends on: preprocessing_validation.R

data %>%
  ungroup() %>%
  mutate(id = 1:n()) %>%
  gather(location, value, -label, -id) %>%
  mutate(location = as.numeric(gsub("pixel", "", location))) %>% 
  arrange(id, location) %>%
  cbind(matrix(1, nrow = nrow(data), ncol = 1) %x% data.matrix(expand.grid(row = 1:28, column = 1:28))) -> data.rc

names(data.rc)[c(ncol(data.rc) - 1, ncol(data.rc))] <- c("row", "column")

step <- 8
start_r <- 1
finish_r <- start_r + step - 1
start_c <- 1
finish_c <- start_c + step - 1
features <- data.rc %>% select(id, label)

while(finish_c <= 28) {
  while(finish_r <= 28) {
    data.rc %>%
      filter(row %in% start_r:finish_r, column %in% start_c:finish_c) %>%
      group_by(label, id) %>%
      summarise(mean(value)) ->> means
    
    names(means)[ncol(means)] <- paste("mean", start_r, start_c, sep = "_")
    
    means %>%
      right_join(features, by = c("id", "label")) ->> features
    
    start_r <- start_r + 4
    finish_r <- start_r + step - 1
  }
  
  start_r <- 1
  finish_r <- start_r + step - 1
  
  start_c <- start_c + 4
  finish_c <- start_c + step - 1
}

