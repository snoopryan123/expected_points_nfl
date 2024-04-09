
N = 100 #FIXME # num. test sets in which we randomly draw 1 play per epoch
N3 = 100 #FIXME # num. random draws of y_hat from p_hat per test set and model

args <- commandArgs(trailingOnly = TRUE)
if (length(args) == 2) {
  epoch_based_EP = as.logical(args[1])
  retrain_models_even_if_saved = as.logical(args[2])
} else {
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
    xgb_C_driveEP_oq2xdq2x_1_model_name,
    xgb_C_driveEP_oq2xdq2x_1_weightByDrive_model_name
  )
} else {
  stop(paste0("Either `epoch_based_EP` or `drive_based_EP` must be TRUE."))
}
print(xgb_model_names_list)


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
    xgb_params <- get(paste0(xgb_model_name, "_params"))
    xgb_nrounds <- get(paste0(xgb_model_name, "_nrounds"))
    
    print(paste0("training ", xgb_model_name))
    xgb <- train_xgb(
      xgb_features, train_set, xgb_params, xgb_nrounds, watchSet=test_set, 
      epoch_based_EP=epoch_based_EP, drive_based_EP=drive_based_EP, 
      weight_by_epoch=xgb_is_weightedByEpoch, weight_by_drive=xgb_is_weightedByDrive
    )
    EP_models_lst[[xgb_model_name]] = xgb
  }
  saveRDS(EP_models_lst, filename)
}

##########################
### EVALUATE EP MODELS ###
##########################

### randomly sample one play per epoch drive, then within the sampled test set calculate:
# RMSE(EP_hat, value(y))
# LOGLOSS(p_hat, y)
# RMSE(value(y_hat), value(y)) 
# FRACTIONSAME(y_hat, y)
# CALIBRATION(p_hat, y)

################################################
### RANDOMLY SAMPLE ONE PLAY PER EPOCH/DRIVE ###
################################################

group_var = if (drive_based_EP) "Drive" else if (epoch_based_EP) "epoch" 
test_sets_lst = list()
for (i in 1:N) {
  print(paste0("randomly sampling 1 play per ", if (drive_based_EP) "drive" else "epoch"," for the i=",i," of ",N,"^th time."))
  
  set.seed(98296+i*238)
  test_set_i = 
    test_set %>%
    group_by_at(group_var) %>%
    slice_sample(n=1) %>%
    ungroup()
  
  test_sets_lst[[i]] = test_set_i
}

################################
### EVALUATE the predictions ###
################################

MAT_rmse_EPHat_ValueY = matrix(nrow = length(test_sets_lst), ncol = length(EP_models_lst))
MAT_logloss_PHat_Y = matrix(nrow = length(test_sets_lst), ncol = length(EP_models_lst))
ARR_rmse_ValueYHat_ValueY = array(dim = c(length(test_sets_lst), length(EP_models_lst), N3))
ARR_fractionSame_YHat_Y = array(dim = c(length(test_sets_lst), length(EP_models_lst), N3))
# MAT_calibration_PHat_Y = matrix(nrow = length(test_sets_lst), ncol = length(EP_models_lst))

colnames(MAT_rmse_EPHat_ValueY) = names(EP_models_lst)
colnames(MAT_logloss_PHat_Y) = names(EP_models_lst)
colnames(ARR_rmse_ValueYHat_ValueY) = names(EP_models_lst)
colnames(ARR_fractionSame_YHat_Y) = names(EP_models_lst)
# colnames(MAT_calibration_PHat_Y) = names(EP_models_lst)

for (j in 1:length(EP_models_lst)) {
  xgb_model_name <- names(EP_models_lst)[j]
  xgb <- EP_models_lst[[xgb_model_name]]
  xgb_features <- get(paste0(xgb_model_name, "_features"))
  
  for (i in 1:length(test_sets_lst)) {
    print(paste0("Evaluating j=",j,"/",length(EP_models_lst),"th model and i=",i,"/",length(test_sets_lst),"th test set."))
    
    ### test set outcomes
    test_set_i = test_sets_lst[[i]]
    value_y_i = if (drive_based_EP) test_set_i$pts_end_of_drive else if (epoch_based_EP) test_set_i$pts_next_score
    y_i = if (drive_based_EP) test_set_i$outcome_drive else if (epoch_based_EP) test_set_i$outcome_epoch
    
    ### EP_hat
    EP_hat_ij = predict_ep_xgb(
      xgb, test_set_i, xgb_features, xgb_model_name, 
      epoch_based_EP=epoch_based_EP, drive_based_EP=drive_based_EP
    )$pred
    ### p_hat
    p_hat_mat_ij = predict_probs_xgb(
      xgb, test_set_i, xgb_features, 
      epoch_based_EP=epoch_based_EP, drive_based_EP=drive_based_EP
    )
    p_hat_times_y_ij = p_hat_mat_ij[cbind(1:nrow(p_hat_mat_ij), y_i+1)]
    
    # RMSE(EP_hat, value(y))
    MAT_rmse_EPHat_ValueY[i,j] = RMSE(EP_hat_ij, value_y_i)
    # LOGLOSS(p_hat, y)
    MAT_logloss_PHat_Y[i,j] = -mean(log(p_hat_times_y_ij))
    
    ### sample outcome y_hat from p_hat_mat
    for (k in 1:N3) {
      if (k%%10==0) print(paste("k=",k,"/",N3))
      
      set.seed(97152+i*304+j*134+k*27)
      y_hat_ik = apply(p_hat_mat_ij, MARGIN=1, FUN=function(x) { sample(1:ncol(p_hat_mat_ij), size=1, prob=p_hat_mat_ij[1,], replace=TRUE) - 1 })
      if (drive_based_EP) {
        value_y_hat_ik = map_drive_outcome_to_value[y_hat_ik+1,]$pts_end_of_drive
      } else if (epoch_based_EP) {
        value_y_hat_ik = map_epoch_outcome_to_value[y_hat_ik+1,]$pts_next_score
      }
      # RMSE(value(y_hat), value(y)) 
      ARR_rmse_ValueYHat_ValueY[i,j,k] = RMSE(value_y_hat_ik , value_y_i)
      # FRACTIONSAME(y_hat, y)
      ARR_fractionSame_YHat_Y[i,j,k] = mean(y_hat_ik == y_i)
    }
    
    # # CALIBRATION(p_hat, y)
    # MAT_calibration_PHat_Y #FIXME
  }
}

MAT_rmse_ValueYHat_ValueY = apply(ARR_rmse_ValueYHat_ValueY, MARGIN=c(1,2), FUN=mean)
MAT_fractionSame_YHat_Y = apply(ARR_fractionSame_YHat_Y, MARGIN=c(1,2), FUN=mean)

EVAL_ARRAY <- abind( 
  MAT_rmse_EPHat_ValueY, 
  MAT_logloss_PHat_Y,
  MAT_rmse_ValueYHat_ValueY, 
  MAT_fractionSame_YHat_Y, 
  along=3 
)
dimnames(EVAL_ARRAY)[[3]] = c(
  "rmse_EPHat_ValueY", 
  "logloss_PHat_Y",
  "rmse_ValueYHat_ValueY", 
  "fractionSame_YHat_Y"
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





