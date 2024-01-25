
########################
filewd = getwd()
setwd("..")
source("A1_load_data.R") 
setwd(filewd)
########################

################
### Datasets ###
################

### training and testing datasets
train_set = data_full %>% filter(train_play)
test_set = data_full %>% filter(test_play)

### check
print(paste("there are", dim(data_full)[1], "plays, with", 
            dim(train_set)[1], "training plays and", 
            dim(test_set)[1], "testing plays. It is",
            dim(train_set)[1] + dim(test_set)[1] == dim(data_full)[1], 
            "that they sum up properly."))




