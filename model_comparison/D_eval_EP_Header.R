
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
  N_train = 2 #FIXME # num. train sets in which we randomly draw 1 play per epoch
  N_test = 100
  train_test = TRUE #FIXME
  drive_based_EP = FALSE #FIXME
  # accuracy_only = TRUE #FIXME
  # b = 0 #FIXME
  # B = 0 #FIXME
  accuracy_only = FALSE #FIXME
  b = 1 #FIXME
  B = 3 #FIXME
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

source("A_train_test_main.R")
source("B_models_XGB.R")
source("B_models_MLR.R")
# source("B_models_catalytic.R")

######################
### EP MODEL NAMES ###
######################

### get list of names of models
if (epoch_based_EP) {
  if (!train_test) {
    model_names_list <- list(
      "mlr_epochEP_yurko_plus_tq_weightByEpoch",
      xgb_C_epochEP_oq2xdq2x_1_weightByEpoch_model_name
    )
  } else if (accuracy_only) {
    ### catalytic model params
    # phi_delta = 0.1
    # phis = seq(phi_delta, 1, by=phi_delta)
    # phis = c(phis)
    # phis = c(0.05, 0.15, 0.25)
    phis = c(0.05, 0.1, 0.15, 0.2, 0.25)
    M = 5e5
    model_names_list <- 
      make_catalytic_model_name(
        target_model_name = xgb_C_epochEP_oq2xdq2x_1_weightByEpoch_model_name, 
        prior_model_name = "mlr_epochEP_yurko_plus_tq_weightByEpoch",
        M=M, phi=phis
      )
  } else { ### train_test and !accuracy_only
    model_names_list <- list(
      # "mlr_epochEP_yurko_paper",
      # "mlr_epochEP_yurko_plus",
      # "mlr_epochEP_yurko_plus_weightByEpoch",
      # "mlr_epochEP_yurko_plus_tq",
      # "mlr_epochEP_yurko_plus_tq_weightByEpoch",
      # ###
      # xgb_C_epochEP_nflFastR_1_model_name,
      # xgb_C_epochEP_1_model_name,
      # xgb_C_epochEP_1_weightByEpoch_model_name,
      # xgb_C_epochEP_oq2xdq2x_1_model_name,
      # xgb_C_epochEP_oq2xdq2x_1_weightByEpoch_model_name,
      # ###
      # "mlr_epochEP_yurko_plus_randomlyDrawOnePlayPerGroup",
      # "mlr_epochEP_yurko_plus_tq_randomlyDrawOnePlayPerGroup",
      xgb_C_epochEP_1_randomlyDrawOnePlayPerEpoch_model_name,
      xgb_C_epochEP_oq2xdq2x_1_randomlyDrawOnePlayPerEpoch_model_name
    )
  }
} else if (drive_based_EP) {
  #FIXME
} else {
  stop(paste0("Either `epoch_based_EP` or `drive_based_EP` must be TRUE."))
}
print(model_names_list)
k
