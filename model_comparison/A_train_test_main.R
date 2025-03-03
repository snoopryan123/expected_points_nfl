
########################
filewd = getwd()
setwd("..")
source("A1_load_data.R") 
setwd(filewd)
########################

if(!exists("SEED")) { SEED = 99 } # Aaron Donald! 

all_epochs = unique(data_full_0$epoch)
set.seed(SEED) 
HOLD_OUT_EPOCHS = sort(sample(all_epochs, size = round(0.25*length(all_epochs)), replace = FALSE))
TRAIN_EPOCHS = setdiff(all_epochs, HOLD_OUT_EPOCHS)
VAL_EPOCHS = sort(sample(TRAIN_EPOCHS, size = round(0.5*length(TRAIN_EPOCHS)), replace = FALSE)) ### for xgb param tuning

length(HOLD_OUT_EPOCHS)
length(TRAIN_EPOCHS)
length(VAL_EPOCHS)

data_full = 
  data_full %>%
  mutate(
    train_play = epoch %in% TRAIN_EPOCHS,
    val_play = epoch %in% VAL_EPOCHS,
    test_play = epoch %in% HOLD_OUT_EPOCHS
  )

### training and testing datasets
train_set = data_full %>% filter(train_play)
test_set = data_full %>% filter(test_play)

### check
print(paste("there are", dim(data_full)[1], "plays, with", 
            dim(train_set)[1], "training plays and", 
            dim(train_set %>% filter(val_play))[1], "validation plays for XGB and", 
            dim(test_set)[1], "testing plays. It is",
            dim(train_set)[1] + dim(test_set)[1] == dim(data_full)[1], 
            "that they sum up properly."))
