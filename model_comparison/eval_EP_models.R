
args <- commandArgs(trailingOnly = TRUE)
if (length(args) == 2) {
  N_train = 100 #FIXME # num. train sets in which we randomly draw 1 play per epoch
  N_test = 100 #FIXME # num. test sets in which we randomly draw 1 play per epoch
  epoch_based_EP = as.logical(args[1])
  retrain_models_even_if_saved = as.logical(args[2])
} else {
  ### local machine
  N_train = 3 #FIXME # num. train sets in which we randomly draw 1 play per epoch
  N_test = 10 #FIXME # num. test sets in which we randomly draw 1 play per epoch
  epoch_based_EP = FALSE #FIXME
  retrain_models_even_if_saved = FALSE #FIXME
}
drive_based_EP = !epoch_based_EP

print(paste0("drive_based_EP = ", drive_based_EP))
print(paste0("epoch_based_EP = ", epoch_based_EP))
print(paste0("retrain_models_even_if_saved = ", retrain_models_even_if_saved))

# SEED = 99 #FIXME
# PRE_LOADED_TrainTestSplitAndTeamQualities = FALSE
# source("A_train_test_main.R")

PRE_LOADED_TrainTestSplitAndTeamQualities = TRUE
source("A_train_test_main.R")
source("models_XGB.R")

###
map_drive_outcome_to_value = data_full %>% distinct(outcome_drive, pts_end_of_drive) %>% arrange(outcome_drive)
map_epoch_outcome_to_value = data_full %>% distinct(outcome_epoch, pts_next_score) %>% arrange(outcome_epoch)

#######################
### TRAIN EP MODELS ###
#######################

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
  xgb_model_names_list <- list(
    xgb_C_driveEP_s_1_model_name,
    xgb_C_driveEP_s_1_weightByDrive_model_name,
    xgb_C_driveEP_s_1_randomlyDrawOnePlayPerGroup_model_name,
    xgb_C_driveEP_oq2xdq2x_1_model_name,
    xgb_C_driveEP_oq2xdq2x_1_weightByDrive_model_name,
    xgb_C_driveEP_oq2xdq2x_1_randomlyDrawOnePlayPerGroup_model_name
  )
} else {
  stop(paste0("Either `epoch_based_EP` or `drive_based_EP` must be TRUE."))
}
print(xgb_model_names_list)

### load models
filename = paste0("xgb_",if (drive_based_EP) "driveEP" else "epochEP","_trained_models_for_testing.rds")
if (!retrain_models_even_if_saved & file.exists(filename)) {
  EP_models_lst = readRDS(filename)
} else {
  EP_models_lst = list()
  for (j in 1:length(xgb_model_names_list)) {
    print(paste0("j=",j,"/",length(xgb_model_names_list)))
    xgb_model_name <- xgb_model_names_list[[j]]
    
    xgb_features <- get(paste0(xgb_model_name, "_features"))
    xgb_is_weightedByEpoch = str_detect(xgb_model_name, "weightByEpoch")
    xgb_is_weightedByDrive = str_detect(xgb_model_name, "weightByDrive")
    xgb_is_randomlyDrawOnePlayPerGroup = str_detect(xgb_model_name, "randomlyDrawOnePlayPerGroup")
    xgb_params <- get(paste0(xgb_model_name, "_params"))
    xgb_nrounds <- get(paste0(xgb_model_name, "_nrounds"))
    
    print(paste0("training ", xgb_model_name))
    if (!xgb_is_randomlyDrawOnePlayPerGroup) {
      xgb <- train_xgb(
        xgb_features, train_set, xgb_params, xgb_nrounds, watchSet=test_set, 
        epoch_based_EP=epoch_based_EP, drive_based_EP=drive_based_EP, 
        weight_by_epoch=xgb_is_weightedByEpoch, weight_by_drive=xgb_is_weightedByDrive
      )
      EP_models_lst[[xgb_model_name]] = xgb
    } else {
      xgb_lst = train_xgb_randomlyDrawnPlayPerGroup(
        xgb_model_name, xgb_features, train_set, xgb_params, xgb_nrounds, test_set, drive_based_EP=TRUE, N=N_train
      ) 
      EP_models_lst[[xgb_model_name]] = xgb_lst
    }
  }
  saveRDS(EP_models_lst, filename)
}

##########################
### EVALUATE EP MODELS ###
##########################

### randomly sample one play per epoch drive, then within the sampled test set calculate:
# RMSE(EP_hat, value(y))
# LOGLOSS(p_hat, y)

##############################################################
### EVALUATE BY RANDOMLY SAMPLING ONE PLAY PER EPOCH/DRIVE ###
##############################################################

group_var = if (drive_based_EP) "Drive" else if (epoch_based_EP) "epoch" 

test_sets_lst = randomlyDrawOnePlayPerGroup(test_set, seed=98296, drive_based_EP=drive_based_EP, N=N_test)

################################
### EVALUATE the predictions ###
################################

MAT_rmse_EPHat_ValueY = matrix(nrow = length(test_sets_lst), ncol = length(EP_models_lst))
MAT_logloss_PHat_Y = matrix(nrow = length(test_sets_lst), ncol = length(EP_models_lst))

colnames(MAT_rmse_EPHat_ValueY) = names(EP_models_lst)
colnames(MAT_logloss_PHat_Y) = names(EP_models_lst)

for (j in 1:length(EP_models_lst)) {
  xgb_model_name <- names(EP_models_lst)[j]
  xgb <- EP_models_lst[[xgb_model_name]]
  xgb_features <- get(paste0(xgb_model_name, "_features"))
  xgb_is_randomlyDrawOnePlayPerGroup = str_detect(xgb_model_name, "randomlyDrawOnePlayPerGroup")
  
  for (i in 1:length(test_sets_lst)) {
    print(paste0("Evaluating j=",j,"/",length(EP_models_lst),"th model and i=",i,"/",length(test_sets_lst),"th test set."))
    
    ### test set outcomes
    test_set_i = test_sets_lst[[i]]
    value_y_i = if (drive_based_EP) test_set_i$pts_end_of_drive else if (epoch_based_EP) test_set_i$pts_next_score
    y_i = if (drive_based_EP) test_set_i$outcome_drive else if (epoch_based_EP) test_set_i$outcome_epoch
    
    if (!xgb_is_randomlyDrawOnePlayPerGroup) {
      ### EP_hat
      EP_hat_ij = predict_ep_xgb(xgb, test_set_i, xgb_features, xgb_model_name, 
                                 epoch_based_EP=epoch_based_EP, drive_based_EP=drive_based_EP)$pred
      ### p_hat
      p_hat_mat_ij = predict_probs_xgb(xgb, test_set_i, xgb_features, 
                                       epoch_based_EP=epoch_based_EP, drive_based_EP=drive_based_EP)
      p_hat_times_y_ij = p_hat_mat_ij[cbind(1:nrow(p_hat_mat_ij), y_i+1)]
    } else {
      ### EP_hat
      EP_hat_ij = predict_xgb_randomlyDrawnPlayPerGroup(
        xgb, test_set_i, xgb_features, xgb_model_name, epoch_based_EP=epoch_based_EP, drive_based_EP=drive_based_EP, EP=TRUE
      ) 
      ### p_hat
      p_hat_mat_ij = predict_xgb_randomlyDrawnPlayPerGroup(
        xgb, test_set_i, xgb_features, xgb_model_name, epoch_based_EP=epoch_based_EP, drive_based_EP=drive_based_EP, EP=FALSE
      ) 
    }

    # RMSE(EP_hat, value(y))
    MAT_rmse_EPHat_ValueY[i,j] = RMSE(EP_hat_ij, value_y_i)
    # LOGLOSS(p_hat, y)
    MAT_logloss_PHat_Y[i,j] = -mean(log(p_hat_times_y_ij))
  }
}

EVAL_ARRAY <- abind( 
  MAT_rmse_EPHat_ValueY, 
  MAT_logloss_PHat_Y,
  along=3 
)
dimnames(EVAL_ARRAY)[[3]] = c(
  "rmse_EPHat_ValueY", 
  "logloss_PHat_Y"
)
# EVAL_ARRAY

loss_mat = apply(EVAL_ARRAY, MARGIN=3, FUN=colMeans)
loss_se_mat = apply(EVAL_ARRAY, MARGIN=3, FUN=function(x) apply(x, MARGIN=2, FUN=function(x) 2*sd(x)/sqrt(length(x))))
loss_L_mat = loss_mat - 2*loss_se_mat
loss_U_mat = loss_mat + 2*loss_se_mat

df_losses = tibble()
for (l in 1:ncol(loss_mat)) {
  print(paste0("l=",l,"/",ncol(loss_mat)))
  
  df_loss_l = 
    tibble(
      model = rownames(loss_mat),
      loss_metric = colnames(loss_mat)[l],
      loss_L = loss_L_mat[,l],
      loss = loss_mat[,l],
      loss_U = loss_U_mat[,l]
    ) %>%
    arrange(loss)
  df_loss_l
  
  df_losses = bind_rows(df_losses, df_loss_l)
}

print(data.frame(df_losses))
write_csv(
  df_losses, 
  paste0("losses_",if (drive_based_EP) "driveEP" else if (epoch_based_EP) "epochEP",".csv")
)

gt::gtsave(
  gt::gt(df_losses %>% group_by(loss_metric)) #%>% gt::fmt_number(n_sigfig = 3)
  ,paste0("losses_",if (drive_based_EP) "driveEP" else if (epoch_based_EP) "epochEP",".png")
)



