
args <- commandArgs(trailingOnly = TRUE)
if (length(args) == 2) {
  N_train = 100 #FIXME # num. train sets in which we randomly draw 1 play per epoch
  N_test = 100 #FIXME # num. test sets in which we randomly draw 1 play per epoch
  epoch_based_EP = as.logical(args[1])
  b = as.numeric(args[2])-1 ### index of the boot. training dataset (b == 0 means use the original training set) (for parallelization)
  B = as.numeric(args[3])-1 ### num. bootstrapped datasets
} else {
  ### local machine
  N_train = 3 #FIXME # num. train sets in which we randomly draw 1 play per epoch
  N_test = 10 #FIXME # num. test sets in which we randomly draw 1 play per epoch
  epoch_based_EP = FALSE #FIXME
  b = 1 #FIXME
  B = 3 #FIXME
}
drive_based_EP = !epoch_based_EP
group_var = if (drive_based_EP) "Drive" else if (epoch_based_EP) "epoch" 

print(paste0("drive_based_EP = ", drive_based_EP))
print(paste0("epoch_based_EP = ", epoch_based_EP))
print(paste0("group_var = ", group_var))
print(paste0("b = ", b))
print(paste0("B = ", B))

# SEED = 99 #FIXME
# PRE_LOADED_TrainTestSplitAndTeamQualities = FALSE
# source("A_train_test_main.R")

PRE_LOADED_TrainTestSplitAndTeamQualities = TRUE
source("A_train_test_main.R")
source("models_XGB.R")

###
map_drive_outcome_to_value = data_full %>% distinct(outcome_drive, pts_end_of_drive) %>% arrange(outcome_drive)
map_drive_outcome_to_value$outcome_drive_str = drive_EP_outcomes
map_epoch_outcome_to_value = data_full %>% distinct(outcome_epoch, pts_next_score) %>% arrange(outcome_epoch)
map_epoch_outcome_to_value$outcome_epoch_str = epoch_EP_outcomes

######################
### EP MODEL NAMES ###
######################

### get list of names of models
if (epoch_based_EP) {
  xgb_model_names_list <- list(
    xgb_C_epochEP_nflFastR_1_model_name,
    xgb_C_epochEP_s_1_model_name,
    xgb_C_epochEP_s_1_weightByEpoch_model_name,
    xgb_C_epochEP_oq2xdq2x_1_model_name,
    xgb_C_epochEP_oq2xdq2x_1_weightByEpoch_model_name
  )
} else if (drive_based_EP) {
  # xgb_model_names_list <- list(
  #   xgb_C_driveEP_s_1_model_name,
  #   xgb_C_driveEP_s_1_weightByDrive_model_name,
  #   xgb_C_driveEP_s_1_randomlyDrawOnePlayPerGroup_model_name,
  #   xgb_C_driveEP_oq2xdq2x_1_model_name,
  #   xgb_C_driveEP_oq2xdq2x_1_weightByDrive_model_name,
  #   xgb_C_driveEP_oq2xdq2x_1_randomlyDrawOnePlayPerGroup_model_name
  # )
  xgb_model_names_list <- list(
    xgb_C_driveEP_s_1_model_name,
    xgb_C_driveEP_s_1_weightByDrive_model_name,
    xgb_C_driveEP_s_1_randomlyDrawOnePlayPerGroup_model_name
  )
} else {
  stop(paste0("Either `epoch_based_EP` or `drive_based_EP` must be TRUE."))
}
print(xgb_model_names_list)

