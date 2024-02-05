
########################
filewd = getwd()
setwd("..")
source("A1_load_data.R") 
setwd(filewd)
########################

if(!exists("SEED")) {
  SEED = 99 # Aaron Donald!
} 

#################################
#### Create train test split ####
#################################

if (!PRE_LOADED_TrainTestSplitAndTeamQualities) {
  all_epochs = unique(data_full_0$epoch)
  set.seed(SEED) 
  HOLD_OUT_EPOCHS = sort(sample(all_epochs, size = round(0.25*length(all_epochs)), replace = FALSE))
  TRAIN_EPOCHS = setdiff(all_epochs, HOLD_OUT_EPOCHS)
  VAL_EPOCHS = sort(sample(TRAIN_EPOCHS, size = round(0.5*length(TRAIN_EPOCHS)), replace = FALSE)) ### for xgb param tuning
  
  length(HOLD_OUT_EPOCHS)
  length(TRAIN_EPOCHS)
  length(VAL_EPOCHS)
  
  data_full_0 = 
    data_full_0 %>%
    mutate(
      train_play = epoch %in% TRAIN_EPOCHS,
      val_play = epoch %in% VAL_EPOCHS,
      test_play = epoch %in% HOLD_OUT_EPOCHS
    )
}

###############################
#### Create team qualities ####
###############################

if (!PRE_LOADED_TrainTestSplitAndTeamQualities) {
  USE_ALREADY_DEFINED_EP0_MODEL = TRUE
  fit_ep0_regression_model <- function(dataset) {
    r.ep0 = lm(
      pts_next_score ~ 
        down_3or4 +
        yardline_100 +
        log(ydstogo),
      data = dataset
    )
    r.ep0
  }
  
  source("../A2_createTeamQualityMetricsEpa0.R") ### transforms `data_full_0` into `dataTQ`
  data_full_0 = standardize_team_qualities(dataTQ)
}

################
### Datasets ###
################

### final dataset: keep just recent data
data_full = keep_just_recent_data(data_full_0)

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




