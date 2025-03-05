
source("eval_EP_Header.R")

#################################################
### EVALUATE the predictions of the EP MODELS ###
#################################################

evalFilename <- function(b) {
  paste0("eval_models/results_eval_b",b,"_", if (drive_based_EP) "driveEP" else if (epoch_based_EP) "epochEP",".csv")
}

### load all the generated y_hat's
df_eval_lst = list()
for (b in 1:B) {
  print(paste0("eval b=",b,"/B=",B))
  
  df_eval_b = read_csv(evalFilename(b=b), show_col_types = F)
  df_eval_lst[[b]] = df_eval_b
}
print("about to do.call(rbind(.")
df_eval = do.call(rbind, df_eval_lst)
df_eval = df_eval %>% arrange(i, b)
df_eval

### EVALUATE BY RANDOMLY SAMPLING ONE PLAY PER EPOCH/DRIVE
#FIXME # make sure the seed is the same as in the file `eval_EP_EvalLosses_AJ.R`
test_sets_lst = randomlyDrawOnePlayPerGroup(test_set, seed=98296, drive_based_EP=drive_based_EP, N=N_test)

### cmpute Boot Covg metric
eval_boot_covg <- function(test_sets_lst, xgb_model_name_j) {
  
  ### vectors of boot covgs across the N_test test sets
  VEC_boot_covg_PHat_Y = numeric(length(test_sets_lst))
  
  
  for (ii in 1:N_test) {
    print(paste0("j=",xgb_model_name_j,", i=",ii,"/",N_test))
    
    df_eval_ij = df_eval %>% filter(i == ii & xgb_model_name == xgb_model_name_j)
    df_eval_ij
    
    ### 
    test_set_i = test_sets_lst[[ii]]
    y_i = if (drive_based_EP) test_set_i$outcome_drive else if (epoch_based_EP) test_set_i$outcome_epoch
    y_i_str = (tibble(outcome_drive = y_i) %>% left_join(map_drive_outcome_to_value, by="outcome_drive"))$outcome_drive_str
    
    ### matrix of y_hat predictions across the B bootstrapped models
    MAT0_yhat_ij =
      df_eval_ij %>% 
      select(play_idx, b, y_hat) %>%
      pivot_wider(names_from = b, values_from = y_hat, names_prefix = "b") %>%
      select(-play_idx)
    MAT0_yhat_ij
    MAT_yhat_ij = as.matrix(MAT0_yhat_ij)
    
    ### empirical distribution of each row
    categories <- unique(c(MAT_yhat_ij))
    empirical_dist_mat_ij <- matrix(0, nrow = nrow(MAT_yhat_ij), ncol = length(categories))
    colnames(empirical_dist_mat_ij) = categories
    for (k in 1:nrow(MAT_yhat_ij)) {
      row_table <- table(MAT_yhat_ij[k, ])
      empirical_dist_mat_ij[k, names(row_table)] <- row_table / sum(row_table)
    }
    head(empirical_dist_mat_ij)
    
    # BOOTCOVG(pred_set(y_hat_mat), y)
    boot_pred_sets_95_ij = get_pred_sets(empirical_dist_mat_ij, q_=0.95)
    boot_covered_ij = sapply(1:length(y_i_str), function(l) y_i_str[l] %in% boot_pred_sets_95_ij[[l]])
    VEC_boot_covg_PHat_Y[ii] = mean(boot_covered_ij)
  }
  
  ### results
  results <- tibble(VEC_boot_covg_PHat_Y)
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
  df_results = df_results %>% mutate(xgb_model_name = xgb_model_name_j) %>% relocate(xgb_model_name, .before=metric)
  df_results
}

### get results
results = tibble()
for (j in 1:length(xgb_model_names_list)) {
  print(paste0("eval model j=",j,"/",length(xgb_model_names_list)))
  xgb_model_name_j <- xgb_model_names_list[[j]]
  results_boot_covg_j = eval_boot_covg(test_sets_lst, xgb_model_name_j)
  results = results %>% bind_rows(results_boot_covg_j)
}
results = results %>% arrange(metric, xgb_model_name)

print(data.frame(results))
write_csv(
  results, 
  paste0("results_bootCovg_",if (drive_based_EP) "driveEP" else if (epoch_based_EP) "epochEP",".csv")
)

### eval Losses
df_eval_0 = read_csv(evalFilename(b=0), show_col_types = F)
print(df_eval_0)
write_csv(
  df_eval_0, 
  paste0("results_losses_",if (drive_based_EP) "driveEP" else if (epoch_based_EP) "epochEP",".csv")
)

### view results
# library(tidyverse)
# temp1 = read_csv("results_losses_driveEP.csv")
# temp2 = read_csv("results_bootCovg_driveEP.csv")
# temp1
# temp2

