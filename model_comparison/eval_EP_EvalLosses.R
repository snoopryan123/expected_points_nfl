
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

eval_losses <- function(test_sets_lst, model_name) {
  
  ### model's attributes
  model_is_catalytic <- str_detect(model_name, "catalytic")
  if (model_is_catalytic) {
    catalytic_sub_model_names = get_catalytic_sub_model_names(model_name)
    target_model_name = catalytic_sub_model_names$target
    prior_model_name = catalytic_sub_model_names$prior
    M = catalytic_sub_model_names$M
    phi = catalytic_sub_model_names$phi
    model_type <- if (str_detect(target_model_name, "xgb")) "XGB" else stop()
  } else {
    target_model_name = model_name
    prior_model_name = NULL
    M = NULL
    phi = NULL
    model_type <- if (str_detect(model_name, "xgb")) "XGB" else if (str_detect(model_name, "mlr")) "MLR" else stop()
  }
  
  if (model_type == "XGB") {
    xgb_features <- get(paste0(target_model_name, "_features"))
  } 
  xgb_is_randomlyDrawOnePlayPerGroup = str_detect(target_model_name, "randomlyDrawOnePlayPerGroup")
  xgb_is_regression = str_detect(target_model_name, "_R_")
  xgb_is_boundedRegression = str_detect(target_model_name, "_BR_")
  
  ### load trained xgb model
  fit <- readRDS(paste0("fitted_models/trainedModel_",model_name,"_b",0,".rds"))
  
  ### vectors of losses accross the N_test test sets
  VEC_rmse_EPHat_ValueY = numeric(length(test_sets_lst))
  VEC_logloss_PHat_Y = numeric(length(test_sets_lst))
  VEC_covg_PHat_Y = numeric(length(test_sets_lst))

  for (i in 1:length(test_sets_lst)) {
    print(paste0("Evaluating ",model_name," on i=",i,"/",length(test_sets_lst),"th test set."))
    # print(paste0("Evaluating i=",i,"/",length(test_sets_lst),"th test set."))
    
    ### test set outcomes
    test_set_i = test_sets_lst[[i]]
    value_y_i = if (drive_based_EP) test_set_i$pts_end_of_drive else if (epoch_based_EP) test_set_i$pts_next_score
    y_i = if (drive_based_EP) test_set_i$outcome_drive else if (epoch_based_EP) test_set_i$outcome_epoch
    y_i_str = (tibble(outcome_drive = y_i) %>% left_join(map_drive_outcome_to_value, by="outcome_drive"))$outcome_drive_str
    
    if (!xgb_is_randomlyDrawOnePlayPerGroup) {
      if (model_type == "XGB") {
        ### EP_hat
        EP_hat_ij = predict_ep_xgb(fit, test_set_i, xgb_features, model_name, 
                                   epoch_based_EP=epoch_based_EP, drive_based_EP=drive_based_EP, 
                                   Regression=xgb_is_regression, BoundedRegression=xgb_is_boundedRegression)$pred
        if (!xgb_is_regression) {
          ### p_hat
          p_hat_mat_ij = predict_probs_xgb(fit, test_set_i, xgb_features, 
                                           epoch_based_EP=epoch_based_EP, drive_based_EP=drive_based_EP)
        }
      } else if (model_type == "MLR") {
        ### EP_hat
        EP_hat_ij = predict_mlr_ep(fit, test_set_i, model=model_name, 
                                   epoch_based_EP=epoch_based_EP, drive_based_EP=drive_based_EP)$pred
        if (!xgb_is_regression) {
          ### p_hat
          p_hat_mat_ij = get_mlr_probs(fit, test_set_i, epoch_based_EP=epoch_based_EP, drive_based_EP=drive_based_EP)
        }
      } else {
        stop()
      }
    } else {
      if (model_type == "XGB") {
        ### EP_hat
        EP_hat_ij = predict_xgb_randomlyDrawnPlayPerGroup(
          fit, test_set_i, xgb_features, model_name, epoch_based_EP=epoch_based_EP, drive_based_EP=drive_based_EP, EP=TRUE
        ) 
        if (!xgb_is_regression) {
          ### p_hat
          p_hat_mat_ij = predict_xgb_randomlyDrawnPlayPerGroup(
            fit, test_set_i, xgb_features, model_name, epoch_based_EP=epoch_based_EP, drive_based_EP=drive_based_EP, EP=FALSE
          ) 
        }
      } else if (model_type == "MLR") {
        stop()
      } else {
        stop()
      }
    }
    
    # RMSE(EP_hat, value(y))
    VEC_rmse_EPHat_ValueY[i] = RMSE(EP_hat_ij, value_y_i)
    if (!xgb_is_regression) {
      # LOGLOSS(p_hat, y)
      p_hat_times_y_ij = p_hat_mat_ij[cbind(1:nrow(p_hat_mat_ij), y_i+1)]
      VEC_logloss_PHat_Y[i] = -mean(log(p_hat_times_y_ij))
      ### pred_set(p_hat)
      pred_sets_95_ij = get_pred_sets(p_hat_mat_ij, q_=0.95)
      # COVG(pred_set(p_hat), y)
      covered_ij = sapply(1:length(y_i_str), function(l) y_i_str[l] %in% pred_sets_95_ij[[l]])
      VEC_covg_PHat_Y[i] = mean(covered_ij) 
    }
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
      value_L = value_Med - se_value,
      value_U = value_Med + se_value,
    ) %>%
    relocate(value_Med, .after=value_L)
  df_results = df_results %>% mutate(model_name = model_name) %>% relocate(model_name, .before=metric)
  df_results
}

eval_boot_covg <- function(test_sets_lst, model_name) {
  
  ### load xgb model variables
  xgb_features <- get(paste0(model_name, "_features"))
  xgb_is_randomlyDrawOnePlayPerGroup = str_detect(model_name, "randomlyDrawOnePlayPerGroup")
  
  ### vectors of boot covgs across the N_test test sets
  VEC_boot_covg_PHat_Y = numeric(length(test_sets_lst))
  
  ### eval bootstrap prediction set covg
  for (i in 1:length(test_sets_lst)) {
    
    ### test set outcomes
    test_set_i = test_sets_lst[[i]]
    value_y_i = if (drive_based_EP) test_set_i$pts_end_of_drive else if (epoch_based_EP) test_set_i$pts_next_score
    y_i = if (drive_based_EP) test_set_i$outcome_drive else if (epoch_based_EP) test_set_i$outcome_epoch
    y_i_str = (tibble(outcome_drive = y_i) %>% left_join(map_drive_outcome_to_value, by="outcome_drive"))$outcome_drive_str
    
    ### matrix of y_hat predictions across the B bootstrapped models
    MAT_yhat_ij = matrix(nrow = nrow(test_set_i), ncol=B)
    colnames(MAT_yhat_ij) = paste0("b",1:B)
    
    for (b in 1:B) {
      print(paste0("Evaluating ",model_name," on i=",i,"/",length(test_sets_lst),"th test set for b=",b,"/",B,"th bootstrap."))
      
      ### load b^th trained bootstrapped model
      xgb_b <- readRDS(paste0("fitted_models/trainedModel_",model_name,"_b",b,".rds"))
      
      ### b^th predictions
      if (!xgb_is_randomlyDrawOnePlayPerGroup) {
        ### p_hat
        p_hat_mat_ijb = predict_probs_xgb(
          xgb_b, test_set_i, xgb_features, epoch_based_EP=epoch_based_EP, drive_based_EP=drive_based_EP
        )
      } else {
        ### p_hat
        p_hat_mat_ijb = predict_xgb_randomlyDrawnPlayPerGroup(
          xgb_b, test_set_i, xgb_features, model_name, epoch_based_EP=epoch_based_EP, drive_based_EP=drive_based_EP, EP=FALSE
        ) 
      }
      
      ### sample ane outcome y for each row of p_hat
      set.seed(975*b+2948*i)
      yhat_ijb = sapply(1:nrow(p_hat_mat_ijb), function(i) sample(colnames(p_hat_mat_ijb), size=1, prob=p_hat_mat_ijb[i,]))
      MAT_yhat_ij[,b] = yhat_ijb
    }
    
    ### empirical distribution of each row
    categories <- unique(c(MAT_yhat_ij))
    empirical_dist_mat_ij <- matrix(0, nrow = nrow(MAT_yhat_ij), ncol = length(categories))
    colnames(empirical_dist_mat_ij) = categories
    for (k in 1:nrow(MAT_yhat_ij)) {
      row_table <- table(MAT_yhat_ij[k, ])
      empirical_dist_mat_ij[k, names(row_table)] <- row_table / sum(row_table)
    }
    
    # BOOTCOVG(pred_set(y_hat_mat), y)
    boot_pred_sets_95_ij = get_pred_sets(empirical_dist_mat_ij, q_=0.95)
    boot_covered_ij = sapply(1:length(y_i_str), function(l) y_i_str[l] %in% boot_pred_sets_95_ij[[l]])
    VEC_boot_covg_PHat_Y[i] = mean(boot_covered_ij)
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
  df_results = df_results %>% mutate(model_name = model_name) %>% relocate(model_name, .before=metric)
  df_results
}

results = tibble()
for (j in 1:length(xgb_model_names_list)) {
  print(paste0("eval model j=",j,"/",length(xgb_model_names_list)))
  model_name <- xgb_model_names_list[[j]]
  results_losses_j = eval_losses(test_sets_lst, model_name)
  if (!accuracy_only) {
    results_boot_covg_j = eval_boot_covg(test_sets_lst, model_name)
  } else {
    results_boot_covg_j = tibble()
  }
  results = results %>% bind_rows(results_losses_j) %>% bind_rows(results_boot_covg_j)
}
results = results %>% arrange(metric, model_name)

print(data.frame(results))
write_csv(
  results, 
  paste0("results_evallosses_",if (drive_based_EP) "driveEP" else if (epoch_based_EP) "epochEP",".csv")
)

# gt::gtsave(
#   gt::gt(df_losses %>% group_by(loss_metric)) #%>% gt::fmt_number(n_sigfig = 3)
#   ,paste0("results_",if (drive_based_EP) "driveEP" else if (epoch_based_EP) "epochEP",".png")
# )

### view catalytic results
if (all(str_detect(results$model_name, "catalytic"))) {
  plot_df_cat_losses = 
    results %>%
    filter(!str_detect(metric, "covg")) %>%
    rowwise() %>%
    mutate(
      M = get_catalytic_sub_model_names(model_name)$M,
      phi = get_catalytic_sub_model_names(model_name)$phi,
      metric_ = case_when(
        # str_detect(metric, "logloss") ~ "\U0070\U0302p̂",
        # str_detect(metric, "logloss") ~ "logloss(P, y)",
        # str_detect(metric, "rmse") ~ "rmse(EP, pts(y))",
        str_detect(metric, "logloss") ~ "logloss",
        str_detect(metric, "rmse") ~ "rmse",
        TRUE ~ NA_character_
      )
    )
  for (M_ in unique(plot_df_cat_losses$M)) {
    plot_cat_losses = 
      plot_df_cat_losses %>% 
      filter(M == M_) %>%
      ggplot(aes(x = phi)) +
      facet_wrap(~ metric_, scale="free_y") +
      geom_ribbon(aes(ymin = value_L, ymax = value_U), fill="gray90") +
      geom_point(aes(y = value_Med), size=3) +
      geom_line(aes(y = value_Med), linewidth=1) +
      scale_x_continuous(breaks=seq(0,1,by=0.2), name="\U03D5") +
      ylab("") + labs(title=paste0("M = ", M_))
    # plot_cat_losses
    ggsave(paste0("results_plot_cat_losses_M",M_,".png"), plot_cat_losses, width=10, height=4)
  }
}

