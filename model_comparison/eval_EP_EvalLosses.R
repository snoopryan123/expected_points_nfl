
source("eval_EP_Header.R")

#FIXME

#################################################
### EVALUATE the predictions of the EP MODELS ###
#################################################

### randomly sample one play per epoch drive, then within the sampled test set calculate:
# RMSE(EP_hat, value(y))
# LOGLOSS(p_hat, y)
# COVG(pred_set(p_hat), y)

### EVALUATE BY RANDOMLY SAMPLING ONE PLAY PER EPOCH/DRIVE
test_sets_lst = randomlyDrawOnePlayPerGroup(test_set, seed=98296, drive_based_EP=drive_based_EP, N=N_test)

### evaluate the predictions
MAT_rmse_EPHat_ValueY = matrix(nrow = length(test_sets_lst), ncol = length(EP_models_lst))
MAT_logloss_PHat_Y = matrix(nrow = length(test_sets_lst), ncol = length(EP_models_lst))
MAT_covg_PHat_Y = matrix(nrow = length(test_sets_lst), ncol = length(EP_models_lst))

colnames(MAT_rmse_EPHat_ValueY) = names(EP_models_lst)
colnames(MAT_logloss_PHat_Y) = names(EP_models_lst)
colnames(MAT_covg_PHat_Y) = names(EP_models_lst)

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
    y_i_str = (tibble(outcome_drive = y_i) %>% left_join(map_drive_outcome_to_value, by="outcome_drive"))$outcome_drive_str
    
    if (!xgb_is_randomlyDrawOnePlayPerGroup) {
      ### EP_hat
      EP_hat_ij = predict_ep_xgb(xgb, test_set_i, xgb_features, xgb_model_name, 
                                 epoch_based_EP=epoch_based_EP, drive_based_EP=drive_based_EP)$pred
      ### p_hat
      p_hat_mat_ij = predict_probs_xgb(xgb, test_set_i, xgb_features, 
                                       epoch_based_EP=epoch_based_EP, drive_based_EP=drive_based_EP)
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
    p_hat_times_y_ij = p_hat_mat_ij[cbind(1:nrow(p_hat_mat_ij), y_i+1)]
    MAT_logloss_PHat_Y[i,j] = -mean(log(p_hat_times_y_ij))
    ### pred_set(p_hat)
    pred_sets_95_ij = get_pred_sets(p_hat_mat_ij, q_=0.95)
    # COVG(pred_set(p_hat), y)
    covered_ij = sapply(1:length(y_i_str), function(l) y_i_str[l] %in% pred_sets_95_ij[[l]])
    MAT_covg_PHat_Y[i,j] = mean(covered_ij)
  }
}

EVAL_ARRAY <- abind( 
  MAT_rmse_EPHat_ValueY, 
  MAT_logloss_PHat_Y,
  MAT_covg_PHat_Y,
  along=3 
)
dimnames(EVAL_ARRAY)[[3]] = c(
  "rmse_EPHat_ValueY", 
  "logloss_PHat_Y",
  "covg_PredSetPHat_Y"
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

#################################################
### BOOT ###
#################################################


