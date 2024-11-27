
args <- commandArgs(trailingOnly = TRUE)
if (length(args) > 0) {
  N_train = 100 #FIXME # num. train sets in which we randomly draw 1 play per epoch
  N_test = 100 #FIXME # num. test sets in which we randomly draw 1 play per epoch
  train_test = as.logical(args[1])
  drive_based_EP = as.logical(args[2])
  accuracy_only = as.logical(args[3])
  b = as.numeric(args[4])-1 ### index of the boot. training dataset (b == 0 means use the original training set) (for parallelization)
  B = as.numeric(args[5])-1 ### num. bootstrapped datasets
} else {
  ### local machine
  N_train = 5 #FIXME # num. train sets in which we randomly draw 1 play per epoch
  N_test = 3 #FIXME # num. test sets in which we randomly draw 1 play per epoch
  # train_test = FALSE #FIXME
  train_test = TRUE #FIXME
  drive_based_EP = TRUE #FIXME
  
  accuracy_only = TRUE #FIXME
  b = 0 #FIXME
  B = 0 #FIXME
  
  # accuracy_only = FALSE #FIXME
  # b = 1 #FIXME
  # B = 3 #FIXME
}
epoch_based_EP = !drive_based_EP
group_var = if (drive_based_EP) "Drive" else if (epoch_based_EP) "epoch" 

print(paste0("train_test = ", train_test))
print(paste0("drive_based_EP = ", drive_based_EP))
print(paste0("epoch_based_EP = ", epoch_based_EP))
print(paste0("group_var = ", group_var))
print(paste0("accuracy_only = ", accuracy_only))
print(paste0("b = ", b))
print(paste0("B = ", B))

# SEED = 99 #FIXME
# PRE_LOADED_TrainTestSplitAndTeamQualities = FALSE
# source("A_train_test_main.R")

PRE_LOADED_TrainTestSplitAndTeamQualities = TRUE
source("A_train_test_main.R")
source("models_XGB.R")
source("models_MLR.R")
source("models_catalytic.R")

######################
### EP MODEL NAMES ###
######################

### get list of names of models
if (drive_based_EP) {
  if (!train_test) {
    xgb_model_names_list <- list(
      xgb_C_driveEP_s_1_weightByDrive_model_name
    )
  } else if (accuracy_only) {
    ### catalytic model params
    # phi_delta = 0.1
    # phis = seq(phi_delta, 1, by=phi_delta)
    # phis = c(phis)
    phis = c(0.05, 0.1, 0.15, 0.2, 0.25)
    M = 5e5
    xgb_model_names_list <- 
      make_catalytic_model_name(
        target_model_name = xgb_C_driveEP_s_1_weightByDrive_model_name, 
        prior_model_name = "mlr_driveEP_yurko_s3dE_weightByDrive",
        M=M, phi=phis
      )
    # xgb_model_names_list <- list(
    #   "mlr_driveEP_yurko_s3dE",
    #   "mlr_driveEP_yurko_s3dE_weightByDrive",
    #   xgb_R_driveEP_s_1_weightByDrive_model_name,
    #   xgb_C_driveEP_s_1_weightByDrive_model_name
    # )
  } else { ### train_test and !accuracy_only
    xgb_model_names_list <- list(
      xgb_C_driveEP_s_1_model_name,
      xgb_C_driveEP_s_1_weightByDrive_model_name,
      xgb_C_driveEP_s_1_randomlyDrawOnePlayPerGroup_model_name
    )
  }
  
  # xgb_model_names_list <- list(
  #   xgb_C_driveEP_s_1_model_name,
  #   xgb_C_driveEP_s_1_weightByDrive_model_name,
  #   xgb_C_driveEP_s_1_randomlyDrawOnePlayPerGroup_model_name,
  #   xgb_C_driveEP_oq2xdq2x_1_model_name,
  #   xgb_C_driveEP_oq2xdq2x_1_weightByDrive_model_name,
  #   xgb_C_driveEP_oq2xdq2x_1_randomlyDrawOnePlayPerGroup_model_name
  # )
  # xgb_model_names_list <- list(
  #   xgb_C_driveEP_s_1_model_name,
  #   xgb_C_driveEP_s_1_weightByDrive_model_name
  # )
} else if (epoch_based_EP) {
  # xgb_model_names_list <- list(
  #   xgb_C_epochEP_nflFastR_1_model_name,
  #   xgb_C_epochEP_s_1_model_name,
  #   xgb_C_epochEP_s_1_weightByEpoch_model_name,
  #   xgb_C_epochEP_oq2xdq2x_1_model_name,
  #   xgb_C_epochEP_oq2xdq2x_1_weightByEpoch_model_name
  # )
} else {
  stop(paste0("Either `epoch_based_EP` or `drive_based_EP` must be TRUE."))
}
print(xgb_model_names_list)

