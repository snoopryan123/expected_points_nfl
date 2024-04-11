
source("eval_EP_Header.R")

#################################################
### EVALUATE the predictions of the EP MODELS ###
#################################################

### randomly sample one play per epoch drive, then within the sampled test set calculate:
# RMSE(EP_hat, value(y))
# LOGLOSS(p_hat, y)
# COVG(pred_set(p_hat), y)
# BOOTCOVG(pred_set(y_hat_mat), y)

### EVALUATE BY RANDOMLY SAMPLING ONE PLAY PER EPOCH/DRIVE
test_sets_lst = randomlyDrawOnePlayPerGroup(test_set, seed=98296, drive_based_EP=drive_based_EP, N=N_test)

### evaluate the predictions
MAT_rmse_EPHat_ValueY = matrix(nrow = length(test_sets_lst), ncol = length(xgb_model_names_list))
MAT_logloss_PHat_Y = matrix(nrow = length(test_sets_lst), ncol = length(xgb_model_names_list))
MAT_covg_PHat_Y = matrix(nrow = length(test_sets_lst), ncol = length(xgb_model_names_list))
MAT_boot_covg_PHat_Y = matrix(nrow = length(test_sets_lst), ncol = length(xgb_model_names_list))

colnames(MAT_rmse_EPHat_ValueY) = as.character(xgb_model_names_list)
colnames(MAT_logloss_PHat_Y) = as.character(xgb_model_names_list)
colnames(MAT_covg_PHat_Y) = as.character(xgb_model_names_list)
colnames(MAT_boot_covg_PHat_Y) = as.character(xgb_model_names_list)

eval_losses <- function(test_sets_lst, xgb_model_name) {
  
  ### load xgb model variables
  xgb_features <- get(paste0(xgb_model_name, "_features"))
  xgb_is_randomlyDrawOnePlayPerGroup = str_detect(xgb_model_name, "randomlyDrawOnePlayPerGroup")
  
  ### load trained xgb model
  xgb <- readRDS(paste0("fitted_models/trainedModel_",xgb_model_name,"_b",0,".rds"))
  
  ### vectors of losses accross the N_test test sets
  VEC_rmse_EPHat_ValueY = numeric(length(test_sets_lst))
  VEC_logloss_PHat_Y = numeric(length(test_sets_lst))
  VEC_covg_PHat_Y = numeric(length(test_sets_lst))

  for (i in 1:length(test_sets_lst)) {
    print(paste0("Evaluating ",xgb_model_name," on i=",i,"/",length(test_sets_lst),"th test set."))
    # print(paste0("Evaluating i=",i,"/",length(test_sets_lst),"th test set."))
    
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
    VEC_rmse_EPHat_ValueY[i] = RMSE(EP_hat_ij, value_y_i)
    # LOGLOSS(p_hat, y)
    p_hat_times_y_ij = p_hat_mat_ij[cbind(1:nrow(p_hat_mat_ij), y_i+1)]
    VEC_logloss_PHat_Y[i] = -mean(log(p_hat_times_y_ij))
    ### pred_set(p_hat)
    pred_sets_95_ij = get_pred_sets(p_hat_mat_ij, q_=0.95)
    # COVG(pred_set(p_hat), y)
    covered_ij = sapply(1:length(y_i_str), function(l) y_i_str[l] %in% pred_sets_95_ij[[l]])
    VEC_covg_PHat_Y[i] = mean(covered_ij)
  }
  
  ### results
  results <- tibble(VEC_rmse_EPHat_ValueY, VEC_logloss_PHat_Y, VEC_covg_PHat_Y)
  names(results) = str_remove_all(names(results), "VEC_")
  df_results = 
    results %>% 
    pivot_longer(everything(), names_to = "metric", values_to = "value") %>%
    group_by(metric) %>%
    summarise(
      value_Med = median(value),
      se_value = 2*sd(value)/sqrt(length(value)),
      .groups = "drop"
    ) %>%
    mutate(
      value_L = value_Med - 2*se_value,
      value_U = value_Med + 2*se_value,
    ) %>%
    relocate(value_Med, .after=value_L)
  df_results = df_results %>% mutate(xgb_model_name = xgb_model_name) %>% relocate(xgb_model_name, .before=metric)
  df_results
}

yhat_boot_covg <- function(test_sets_lst, xgb_model_name, b) {
  if (!(1 <= b & b <= B)) stop(paste0("b=",b," needs to be in 1...B=",B))
  
  ### load xgb model variables
  xgb_features <- get(paste0(xgb_model_name, "_features"))
  xgb_is_randomlyDrawOnePlayPerGroup = str_detect(xgb_model_name, "randomlyDrawOnePlayPerGroup")
  
  ### vectors of boot covgs across the N_test test sets
  n_test_sets = length(test_sets_lst)
  n_test = nrow(test_sets_lst[[1]]) #FIXME
  MAT_yhat_j = matrix(nrow=n_test, ncol=n_test_sets)
  colnames(MAT_yhat_j) = paste0("i=",1:n_test_sets)
  
  ### eval bootstrap prediction set covg
  for (i in 1:n_test_sets) {
    
    ### test set outcomes
    test_set_i = test_sets_lst[[i]]
    value_y_i = if (drive_based_EP) test_set_i$pts_end_of_drive else if (epoch_based_EP) test_set_i$pts_next_score
    y_i = if (drive_based_EP) test_set_i$outcome_drive else if (epoch_based_EP) test_set_i$outcome_epoch
    y_i_str = (tibble(outcome_drive = y_i) %>% left_join(map_drive_outcome_to_value, by="outcome_drive"))$outcome_drive_str
    
    ### load b^th trained bootstrapped model
    print(paste0("Evaluating ",xgb_model_name," on i=",i,"/",n_test_sets,"th test set for b=",b,"/",B,"th bootstrap."))
    xgb_b <- readRDS(paste0("fitted_models/trainedModel_",xgb_model_name,"_b",b,".rds"))
    
    ### b^th predictions
    if (!xgb_is_randomlyDrawOnePlayPerGroup) {
      ### p_hat
      p_hat_mat_ijb = predict_probs_xgb(
        xgb_b, test_set_i, xgb_features, epoch_based_EP=epoch_based_EP, drive_based_EP=drive_based_EP
      )
    } else {
      ### p_hat
      p_hat_mat_ijb = predict_xgb_randomlyDrawnPlayPerGroup(
        xgb_b, test_set_i, xgb_features, xgb_model_name, epoch_based_EP=epoch_based_EP, drive_based_EP=drive_based_EP, EP=FALSE
      ) 
    }
    
    ### sample ane outcome y for each row of p_hat
    set.seed(975*b+2948*i)
    yhat_ijb = sapply(1:nrow(p_hat_mat_ijb), function(i) sample(colnames(p_hat_mat_ijb), size=1, prob=p_hat_mat_ijb[i,]))
    MAT_yhat_j[,i] = yhat_ijb
  }
  
  ### results
  results =
    tibble(melt(MAT_yhat_j)) %>% 
    rename(i=Var2, play_idx=Var1, y_hat=value) %>% 
    mutate(b=b, i = as.numeric(str_remove_all(i, "i=")), xgb_model_name=xgb_model_name) 
  results
}

results = tibble()
for (j in 1:length(xgb_model_names_list)) {
  print(paste0("eval model j=",j,"/",length(xgb_model_names_list)))
  xgb_model_name <- xgb_model_names_list[[j]]
  if (b == 0) {
    results_j = eval_losses(test_sets_lst, xgb_model_name)
    results_j$b = 0
  } else {
    results_j = yhat_boot_covg(test_sets_lst, xgb_model_name, b)
  }
  results = results %>% bind_rows(results_j)
}

if (b == 0) {
  results = results %>% arrange(metric, xgb_model_name)
}

# print(data.frame(results))
print(results)
filename = paste0("eval_models/results_eval_b",b,"_", if (drive_based_EP) "driveEP" else if (epoch_based_EP) "epochEP",".csv")
filename
write_csv(results, filename)

