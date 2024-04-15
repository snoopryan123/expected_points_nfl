
########################
### Helper Functions ###
########################

load_params <- function(target_model_name) {
  params = list.load(paste0("param_tuning_results_FINAL/", target_model_name, ".yaml"))
  nrounds = params$nrounds
  catalytic = params$CATALYTIC
  suppressWarnings({ params = within(params, rm(target_model_name)) })
  suppressWarnings({ params = within(params, rm(model_name)) })
  suppressWarnings({ params = within(params, rm(CATALYTIC)) })
  suppressWarnings({ params = within(params, rm(phi)) })
  suppressWarnings({ params = within(params, rm(test_loss)) })
  suppressWarnings({ params = within(params, rm(nrounds)) })
  params = lapply(params, function(x) { ifelse(x == "NA", NA, x) }) # replace "NA" with NA
  return(list(params, nrounds, catalytic))
}

map_n77_to_01 <- function(x) {
  ### input vector x includes points of the next score, with values from -7 to 7
  (x + 7)/14
}

map_01_to_n77 <- function(y) {
  ### input vector yincludes points of the next score, already mapped into 0 to 1
  14*y - 7
}

get_xgb_train_DMatrix <- function(xgb_features, train_set, params, 
                                  epoch_based_EP=FALSE, drive_based_EP=FALSE, wp=FALSE,
                                  weight_by_epoch=FALSE, weight_by_drive=FALSE,
                                  Regression=FALSE, BoundedRegression=FALSE, 
                                  catalytic=FALSE, catalytic_model_name="") {
  
  # ### add fake catalytic data to xgboost
  # if (catalytic) {
  #   ### get catalytic train set.  Catalytic params (M,tau) are included in `params`
  #   if (catalytic_model_name == "") { stop("FIXME:", "should specify the catalytic model in train_xgb") }
  #   catalytic_model_type = case_when(
  #     str_detect(catalytic_model_name, "mlr") ~ "MLR",
  #     str_detect(catalytic_model_name, "gam") ~ "GAM",
  #     str_detect(catalytic_model_name, "lm") ~ "OLS",
  #   )
  #   catalytic_outcome = if (BoundedRegression | Regression | wp) "numeric" else "categorical"
  #   train_set = get_catalytic_set(params$M, params$tau, train_set, catalytic_model_type, 
  #                                 catalytic_model_name, catalytic_outcome, params$U, params$MU)
  #   params = within(params, rm(M))
  #   params = within(params, rm(tau))
  #   w = TRUE ### catalytic xgboost must be weighted
  # }
  
  ### row-weights for xgboost
  if (weight_by_epoch) {
    xgb_weights = train_set$epoch_weight
  } else if (weight_by_drive) {
    xgb_weights = train_set$drive_weight
  } else {
    xgb_weights = rep(1, nrow(train_set))
  } 
  
  ### create response column for xgboost
  if (epoch_based_EP) {
    if (Regression) { ### numeric expected points 
      train_labels_xgb = train_set$pts_next_score
    } else if (BoundedRegression) { ### bounded numeric expected points in [-7,7], mapped to [0,1]
      train_labels_xgb = map_n77_to_01(train_set$pts_next_score)
    } else { ### categorical expected points {-7,-3,...,3,7}
      train_labels_xgb = train_set$outcome_epoch
    }
  } else if (drive_based_EP) {
    if (Regression) { ### numeric expected points 
      train_labels_xgb = train_set$pts_end_of_drive
    } else if (BoundedRegression) { ### bounded numeric expected points
      train_labels_xgb = map_n77_to_01(train_set$pts_end_of_drive)
    } else { ### categorical expected points 
      train_labels_xgb = train_set$outcome_drive
    }
  } else if (wp) {
    train_labels_xgb = train_set$label_win
  } else {
    stop(paste0("must have one of `epoch_based_EP`, `drive_based_EP`, or `wp`be TRUE."))
  }
  
  ### get the xgboost training matrix
  train_features_xgb = train_set %>% select(all_of(xgb_features))
  train_set_xgbDM = xgboost::xgb.DMatrix(
    model.matrix(~ . + 0, data = train_features_xgb), 
    label = train_labels_xgb,
    weight = xgb_weights,
  )
  
  return(train_set_xgbDM)
}

train_xgb <- function(xgb_features, train_set, params, nrounds, watchSet=FALSE, 
                      epoch_based_EP=FALSE, drive_based_EP=FALSE, wp=FALSE,
                      weight_by_epoch=FALSE, weight_by_drive=FALSE,
                      Regression=FALSE, BoundedRegression=FALSE,
                      catalytic=FALSE,   catalytic_model_name="",
                      param_tuning=FALSE, print_every_n=50) {
  
  train_set_xgbDM =  get_xgb_train_DMatrix(
    xgb_features, train_set, params, 
    epoch_based_EP=epoch_based_EP, drive_based_EP=drive_based_EP, wp=wp,
    weight_by_epoch=weight_by_epoch, weight_by_drive=weight_by_drive,
    Regression=Regression, BoundedRegression=BoundedRegression,
    catalytic=catalytic, catalytic_model_name=catalytic_model_name
  ) 
    
  if (is.data.frame(watchSet)) { ### validation set evaluation
    val_set_xgbDM = get_xgb_train_DMatrix(
      xgb_features, watchSet, params, 
      epoch_based_EP=epoch_based_EP, drive_based_EP=drive_based_EP, wp=wp,
      weight_by_epoch=weight_by_epoch, weight_by_drive=weight_by_drive,
      Regression=Regression, BoundedRegression=BoundedRegression,
      catalytic=catalytic, catalytic_model_name=catalytic_model_name
    ) 
    watchlist <- list(train=train_set_xgbDM, validation=val_set_xgbDM)
  } else { ### no validation set
    watchlist <- list(train=train_set_xgbDM)
  }
  
  set.seed(34362649) #########################
  if (param_tuning) { ### if tuning parameters
    xgb <- xgb.train( 
      data = train_set_xgbDM, 
      watchlist = watchlist,
      params = params, 
      nrounds = 15000,
      early_stopping_rounds = 50,
      print_every_n = print_every_n,
      verbose = 2
    )
  } else {
    xgb <- xgb.train( 
      data = train_set_xgbDM, 
      watchlist = watchlist,
      params = params, 
      nrounds = nrounds, 
      print_every_n = print_every_n,
      verbose = 2
    )
  }
  return(xgb)
}

epoch_EP_outcomes = c("Touchdown","Opp_Touchdown","Field_Goal","Opp_Field_Goal","Safety","Opp_Safety","No_Score")
drive_EP_outcomes = c("Touchdown","Field_Goal","No_Score","Opp_Safety","Opp_Touchdown")

predict_probs_xgb <-  function(xgb, test_set, xgb_features, 
                               epoch_based_EP=FALSE, drive_based_EP=FALSE, wp=FALSE) {
  
  test_features_xgb =  test_set %>% select(all_of(xgb_features))
  test_set_xgbDM = xgboost::xgb.DMatrix(as.matrix(test_features_xgb))
  xgb_pred = predict(xgb, test_set_xgbDM)
  if (epoch_based_EP) {
    # print(data_full %>% distinct(outcome_epoch, pts_next_score) %>% arrange(outcome_epoch))
    pred_matrix = matrix(xgb_pred, ncol=7, nrow=length(xgb_pred)/7, byrow=TRUE)
    colnames(pred_matrix) = epoch_EP_outcomes
  } else if (drive_based_EP) {
    # print(data_full %>% distinct(outcome_drive, pts_end_of_drive) %>% arrange(outcome_drive))
    pred_matrix = matrix(xgb_pred, ncol=5, nrow=length(xgb_pred)/5, byrow=TRUE)
    colnames(pred_matrix) = drive_EP_outcomes
  } else if (wp) { ### win probability model
    pred_matrix = xgb_pred
  } else {
    stop(paste0("must have one of `epoch_based_EP`, `drive_based_EP`, or `wp`be TRUE."))
  }
  return(pred_matrix)
}

predict_ep_xgb <- function(xgb, test_set, xgb_features, model_name, 
                           epoch_based_EP=FALSE, drive_based_EP=FALSE,
                           Regression=FALSE, BoundedRegression=FALSE) {
  ### get expected points predictions
  if (Regression) { ### numeric expected points 
    test_features_xgb =  test_set %>% select(all_of(xgb_features))
    test_set_xgbDM = xgboost::xgb.DMatrix(as.matrix(test_features_xgb))
    pred_ep = predict(xgb, test_set_xgbDM)
  } else if (BoundedRegression) { ### bounded numeric expected points in [-7,7], mapped to [0,1]
    test_features_xgb =  test_set %>% select(all_of(xgb_features))
    test_set_xgbDM = xgboost::xgb.DMatrix(as.matrix(test_features_xgb))
    pred_ep = map_01_to_n77(predict(xgb, test_set_xgbDM))
  } else { ### categorical expected points 
    pred_cg = predict_probs_xgb(xgb, test_set, xgb_features, 
                                epoch_based_EP=epoch_based_EP, drive_based_EP=drive_based_EP)
    if (epoch_based_EP) {
      # data_full %>% distinct(outcome_epoch, pts_next_score) %>% arrange(outcome_epoch)
      pred_ep = pred_cg[,1]*(7) + pred_cg[,2]*(-7) + pred_cg[,3]*(3) + pred_cg[,4]*(-3) + pred_cg[,5]*(2) + pred_cg[,6]*(-2) + pred_cg[,7]*(0)
    } else if (drive_based_EP) {
      # print(data_full %>% distinct(outcome_drive, pts_end_of_drive) %>% arrange(outcome_drive))
      pred_ep = pred_cg[,1]*(7) + pred_cg[,2]*(3) + pred_cg[,3]*(0) + pred_cg[,4]*(-2) + pred_cg[,5]*(-7) 
    } else {
      stop(paste0("Either `epoch_based_EP` or `drive_based_EP` must be TRUE."))
    }
  }
  return(tibble(pred=pred_ep, model=model_name))
}

randomlyDrawOnePlayPerGroup <- function(dataset, seed, drive_based_EP=TRUE, N=100) {
  if (!drive_based_EP) {
    epoch_based_EP = TRUE
  }
  group_var = if (drive_based_EP) "Drive" else if (epoch_based_EP) "epoch" 
  
  datasets_lst = list()
  for (i in 1:N) {
    print(paste0("randomly sampling 1 play per ", tolower(group_var)," for the i=",i," of ",N,"^th time."))
    set.seed(seed + i*seed*3)
    dataset_i = dataset %>% group_by_at(group_var) %>% slice_sample(n=1) %>% ungroup()
    datasets_lst[[i]] = dataset_i
  }
  datasets_lst
}

get_pred_sets <- function(p_hat_mat, q_=0.95) {
  if (is.null(colnames(p_hat_mat))) stop()
  
  get_pred_set <- function(i) {
    p_vec = p_hat_mat[i,]
    last_idx_in_pred_set = unname(which(cumsum(sort(p_vec, decreasing = T)) >= q_)[1])
    names(p_vec[1:last_idx_in_pred_set])
  }
  
  pred_set_lst = sapply(1:nrow(p_hat_mat), FUN = get_pred_set)
  pred_set_lst
}

###########################
### Bootstrap Functions ###
###########################

get_clustered_bootstrap_dataset <- function(dataset, group_var) {
  ### cluster bootstrap: sample clusters (DRIVE or EPOCH, given by `group_var`) with replacement
  all_group_ids = sort(unique(dataset[[group_var]]))
  num_resample_cb = round(length(all_group_ids))
  group_ids_boot = tibble(
    g = sort(sample(all_group_ids, size=num_resample_cb, replace=TRUE))
  ) %>% mutate(ii = 1:n()) 
  group_ids_boot[[group_var]] = group_ids_boot$g
  group_ids_boot = group_ids_boot %>% select(-g)
  # df_cb = left_join(group_ids_boot, dataset)
  df_cb = left_join(group_ids_boot, dataset, relationship = "many-to-many")
  
  return(df_cb)
}

get_iid_bootstrap_dataset <- function(dataset) {
  row_idxs = 1:nrow(dataset)
  resampled_idxs = sort(sample(row_idxs, size=nrow(dataset), replace=TRUE))
  dataset_boot = dataset[resampled_idxs, ]
  dataset_boot
}

###########################################################
### XGBoost epoch-EP Classification Models (unweighted) ###
###########################################################

####
xgb_C_epochEP_nflFastR_1_model_name = "xgb_C_epochEP_nflFastR_1"
xgb_C_epochEP_nflFastR_1_features = 
  c(
    "yardline_100", "down1", "down2", "down4", "down4", "ydstogo", "half_seconds_remaining",
    "era0", "era1", "era2", "era3", "era4", 
    "season", "home", "posteam_timeouts_remaining", "defteam_timeouts_remaining", "retractable", "dome"
  )
xgb_C_epochEP_nflFastR_1_params = load_params(xgb_C_epochEP_nflFastR_1_model_name)[[1]]
xgb_C_epochEP_nflFastR_1_nrounds = load_params(xgb_C_epochEP_nflFastR_1_model_name)[[2]]
xgb_C_epochEP_nflFastR_1_catalytic = load_params(xgb_C_epochEP_nflFastR_1_model_name)[[3]]

####
xgb_C_epochEP_s_1_model_name = "xgb_C_epochEP_s_1"
xgb_C_epochEP_s_1_features = 
   c("yardline_100", "down1", "down2", "down3", "down4", "ydstogo", "half_seconds_remaining", 
     "era_A", "posteam_timeouts_remaining",  "defteam_timeouts_remaining", 
     "score_differential", "posteam_spread"
   )
xgb_C_epochEP_s_1_params = load_params(xgb_C_epochEP_s_1_model_name)[[1]]
xgb_C_epochEP_s_1_nrounds = load_params(xgb_C_epochEP_s_1_model_name)[[2]]
xgb_C_epochEP_s_1_catalytic = load_params(xgb_C_epochEP_s_1_model_name)[[3]]

####
xgb_C_epochEP_oq2xdq2x_1_model_name = "xgb_C_epochEP_oq2xdq2x_1"
xgb_C_epochEP_oq2xdq2x_1_features =
   c("yardline_100", "down1", "down2", "down3", "down4", "ydstogo", "half_seconds_remaining",
     "era_A", "posteam_timeouts_remaining",  "defteam_timeouts_remaining",
     "score_differential",
     "qbq_ot_0_sum", "oq_rot_0_total_sum",
     "qbq_dt_0_sum", "oq_rdt_0_sum",
     "dq_dt_0_againstPass_sum", "dq_ot_0_againstPass_sum",
     "dq_dt_0_againstRun_sum", "dq_ot_0_againstRun_sum"
   )
xgb_C_epochEP_oq2xdq2x_1_params = load_params(xgb_C_epochEP_oq2xdq2x_1_model_name)[[1]]
xgb_C_epochEP_oq2xdq2x_1_nrounds = load_params(xgb_C_epochEP_oq2xdq2x_1_model_name)[[2]]
xgb_C_epochEP_oq2xdq2x_1_catalytic = load_params(xgb_C_epochEP_oq2xdq2x_1_model_name)[[3]]

#########################################################################
### XGBoost epoch-EP Classification Models that are weighted by epoch ###
#########################################################################

####
xgb_C_epochEP_s_1_weightByEpoch_model_name = "xgb_C_epochEP_s_1_weightByEpoch"
xgb_C_epochEP_s_1_weightByEpoch_features = xgb_C_epochEP_s_1_features
xgb_C_epochEP_s_1_weightByEpoch_params = load_params(xgb_C_epochEP_s_1_weightByEpoch_model_name)[[1]]
xgb_C_epochEP_s_1_weightByEpoch_nrounds = load_params(xgb_C_epochEP_s_1_weightByEpoch_model_name)[[2]]
xgb_C_epochEP_s_1_weightByEpoch_catalytic = load_params(xgb_C_epochEP_s_1_weightByEpoch_model_name)[[3]]

####
xgb_C_epochEP_oq2xdq2x_1_weightByEpoch_model_name = "xgb_C_epochEP_oq2xdq2x_1_weightByEpoch"
xgb_C_epochEP_oq2xdq2x_1_weightByEpoch_features = xgb_C_epochEP_oq2xdq2x_1_features
xgb_C_epochEP_oq2xdq2x_1_weightByEpoch_params = load_params(xgb_C_epochEP_oq2xdq2x_1_weightByEpoch_model_name)[[1]]
xgb_C_epochEP_oq2xdq2x_1_weightByEpoch_nrounds = load_params(xgb_C_epochEP_oq2xdq2x_1_weightByEpoch_model_name)[[2]]
xgb_C_epochEP_oq2xdq2x_1_weightByEpoch_catalytic = load_params(xgb_C_epochEP_oq2xdq2x_1_weightByEpoch_model_name)[[3]]

###########################################################
### XGBoost drive-EP Classification Models (unweighted) ###
###########################################################

####
xgb_C_driveEP_s_1_model_name = "xgb_C_driveEP_s_1"
xgb_C_driveEP_s_1_features = 
  c("yardline_100", "down1", "down2", "down3", "down4", "ydstogo", "half_seconds_remaining", 
    "era_A", "posteam_timeouts_remaining",  "defteam_timeouts_remaining", 
    "score_differential", "posteam_spread"
  )
xgb_C_driveEP_s_1_params = load_params(xgb_C_driveEP_s_1_model_name)[[1]]
xgb_C_driveEP_s_1_nrounds = load_params(xgb_C_driveEP_s_1_model_name)[[2]]
xgb_C_driveEP_s_1_catalytic = load_params(xgb_C_driveEP_s_1_model_name)[[3]]

####
xgb_C_driveEP_oq2xdq2x_1_model_name = "xgb_C_driveEP_oq2xdq2x_1"
xgb_C_driveEP_oq2xdq2x_1_features =
  c("yardline_100", "down1", "down2", "down3", "down4", "ydstogo", "half_seconds_remaining",
    "era_A", "posteam_timeouts_remaining",  "defteam_timeouts_remaining",
    "score_differential",
    "qbq_ot_0_sum", "oq_rot_0_total_sum",
    "qbq_dt_0_sum", "oq_rdt_0_sum",
    "dq_dt_0_againstPass_sum", "dq_ot_0_againstPass_sum",
    "dq_dt_0_againstRun_sum", "dq_ot_0_againstRun_sum"
  )
xgb_C_driveEP_oq2xdq2x_1_params = load_params(xgb_C_driveEP_oq2xdq2x_1_model_name)[[1]]
xgb_C_driveEP_oq2xdq2x_1_nrounds = load_params(xgb_C_driveEP_oq2xdq2x_1_model_name)[[2]]
xgb_C_driveEP_oq2xdq2x_1_catalytic = load_params(xgb_C_driveEP_oq2xdq2x_1_model_name)[[3]]

#########################################################################
### XGBoost drive-EP Classification Models that are weighted by drive ###
#########################################################################

####
xgb_C_driveEP_s_1_weightByDrive_model_name = "xgb_C_driveEP_s_1_weightByDrive"
xgb_C_driveEP_s_1_weightByDrive_features = xgb_C_driveEP_s_1_features
xgb_C_driveEP_s_1_weightByDrive_params = load_params(xgb_C_driveEP_s_1_weightByDrive_model_name)[[1]]
xgb_C_driveEP_s_1_weightByDrive_nrounds = load_params(xgb_C_driveEP_s_1_weightByDrive_model_name)[[2]]
xgb_C_driveEP_s_1_weightByDrive_catalytic = load_params(xgb_C_driveEP_s_1_weightByDrive_model_name)[[3]]

####
xgb_C_driveEP_oq2xdq2x_1_weightByDrive_model_name = "xgb_C_driveEP_oq2xdq2x_1_weightByDrive"
xgb_C_driveEP_oq2xdq2x_1_weightByDrive_features = xgb_C_driveEP_oq2xdq2x_1_features
xgb_C_driveEP_oq2xdq2x_1_weightByDrive_params = load_params(xgb_C_driveEP_oq2xdq2x_1_weightByDrive_model_name)[[1]]
xgb_C_driveEP_oq2xdq2x_1_weightByDrive_nrounds = load_params(xgb_C_driveEP_oq2xdq2x_1_weightByDrive_model_name)[[2]]
xgb_C_driveEP_oq2xdq2x_1_weightByDrive_catalytic = load_params(xgb_C_driveEP_oq2xdq2x_1_weightByDrive_model_name)[[3]]



###############################################################################################
### XGBoost drive-EP Classification Models trained from randomly drawing one play per drive ###
###############################################################################################

####
xgb_C_driveEP_s_1_randomlyDrawOnePlayPerGroup_model_name = "xgb_C_driveEP_s_1_randomlyDrawOnePlayPerGroup"
xgb_C_driveEP_s_1_randomlyDrawOnePlayPerGroup_model_name_RAW = str_remove(xgb_C_driveEP_s_1_randomlyDrawOnePlayPerGroup_model_name, "_randomlyDrawOnePlayPerGroup")
xgb_C_driveEP_s_1_randomlyDrawOnePlayPerGroup_features = get(paste0(xgb_C_driveEP_s_1_randomlyDrawOnePlayPerGroup_model_name_RAW, "_features"))
xgb_C_driveEP_s_1_randomlyDrawOnePlayPerGroup_params = load_params(xgb_C_driveEP_s_1_randomlyDrawOnePlayPerGroup_model_name_RAW)[[1]]
xgb_C_driveEP_s_1_randomlyDrawOnePlayPerGroup_nrounds = load_params(xgb_C_driveEP_s_1_randomlyDrawOnePlayPerGroup_model_name_RAW)[[2]]
xgb_C_driveEP_s_1_randomlyDrawOnePlayPerGroup_catalytic = load_params(xgb_C_driveEP_s_1_randomlyDrawOnePlayPerGroup_model_name_RAW)[[3]]

####
xgb_C_driveEP_oq2xdq2x_1_randomlyDrawOnePlayPerGroup_model_name = "xgb_C_driveEP_oq2xdq2x_1_randomlyDrawOnePlayPerGroup"
xgb_C_driveEP_oq2xdq2x_1_randomlyDrawOnePlayPerGroup_model_name_RAW = str_remove(xgb_C_driveEP_oq2xdq2x_1_randomlyDrawOnePlayPerGroup_model_name, "_randomlyDrawOnePlayPerGroup")
xgb_C_driveEP_oq2xdq2x_1_randomlyDrawOnePlayPerGroup_features = get(paste0(xgb_C_driveEP_oq2xdq2x_1_randomlyDrawOnePlayPerGroup_model_name_RAW, "_features"))
xgb_C_driveEP_oq2xdq2x_1_randomlyDrawOnePlayPerGroup_params = load_params(xgb_C_driveEP_oq2xdq2x_1_randomlyDrawOnePlayPerGroup_model_name_RAW)[[1]]
xgb_C_driveEP_oq2xdq2x_1_randomlyDrawOnePlayPerGroup_nrounds = load_params(xgb_C_driveEP_oq2xdq2x_1_randomlyDrawOnePlayPerGroup_model_name_RAW)[[2]]
xgb_C_driveEP_oq2xdq2x_1_randomlyDrawOnePlayPerGroup_catalytic = load_params(xgb_C_driveEP_oq2xdq2x_1_randomlyDrawOnePlayPerGroup_model_name_RAW)[[3]]

#################################################################
### Randomly draw one play per drive or epoch, train N models ###
#################################################################

train_xgb_randomlyDrawnPlayPerGroup <- function(
    xgb_model_name, xgb_features, train_set, xgb_params, xgb_nrounds, test_set, 
    Regression, BoundedRegression, drive_based_EP=TRUE, N=100
) {
  if (!drive_based_EP) { epoch_based_EP = TRUE }
  group_var = if (drive_based_EP) "Drive" else if (epoch_based_EP) "epoch" 
  
  ### get N train sets; each is formed by randomly sampling one play per group (drive or epoch)
  train_sets_lst = randomlyDrawOnePlayPerGroup(train_set, seed=273442, drive_based_EP=drive_based_EP, N=N)
  
  ### train the model from each train set
  xgb_lst = list()
  for (i in 1:N) {
    print(paste0("training ", xgb_model_name," on training set i=",i,"/",N))
    train_set_i = train_sets_lst[[i]]
    xgb_i <- train_xgb(
      xgb_features, train_set_i, xgb_params, xgb_nrounds, watchSet=test_set, 
      epoch_based_EP=epoch_based_EP, drive_based_EP=drive_based_EP, 
      weight_by_epoch=FALSE, weight_by_drive=FALSE,
      Regression = Regression, BoundedRegression = BoundedRegression
    )
    xgb_lst[[i]] = xgb_i
  }
  xgb_lst
}

predict_xgb_randomlyDrawnPlayPerGroup <- function(
    xgb_lst, test_set, xgb_features, xgb_model_name, epoch_based_EP, drive_based_EP, EP=TRUE
) {

  preds_lst = list()
  for (i in 1:length(xgb_lst)) {
    # print("eval xgb i=",i,"/",)
    xgb_i <- xgb_lst[[i]]
    if (EP) {
      ### EP hat
      EP_hat_i = predict_ep_xgb(xgb_i, test_set, xgb_features, xgb_model_name, 
                                epoch_based_EP=epoch_based_EP, drive_based_EP=drive_based_EP)$pred
      preds_lst[[i]] = EP_hat_i
    } else {
      ### p_hat
      p_hat_mat_i = predict_probs_xgb(xgb_i, test_set, xgb_features, 
                                      epoch_based_EP=epoch_based_EP, drive_based_EP=drive_based_EP)
      preds_lst[[i]] = p_hat_mat_i
    }
  }
  
  ### average across each of the models
  if (EP) {
    ### EP hat
    preds = apply(do.call(rbind, preds_lst), 2, mean)
  } else {
    ### p_hat
    # Convert the list of matrices to a 3D array
    preds_lst_3d <- array(unlist(preds_lst), dim = c(nrow(preds_lst[[1]]), ncol(preds_lst[[1]]), length(preds_lst)))
    preds = apply(preds_lst_3d, MARGIN=c(1,2), FUN=mean)
    colnames(preds) = colnames(preds_lst[[1]])
  }
  
  preds
}

#################################################################################
### XGBoost drive-EP (Monotonic) Regression Models that are weighted by drive ###
#################################################################################

####
xgb_R_driveEP_s_1_weightByDrive_model_name = "xgb_R_driveEP_s_1_weightByDrive"
xgb_R_driveEP_s_1_weightByDrive_features = 
  c("yardline_100", "down1", "down2", "down3", "down4", 
    "ydstogo", "half_seconds_remaining", "era_A", 
    "posteam_timeouts_remaining",  "defteam_timeouts_remaining", 
    "score_differential", "posteam_spread"
  )
xgb_R_driveEP_s_1_weightByDrive_monotonicities =  c(-1,0,0,0,0, -1,0,1, 1,-1, 0,-1)
xgb_R_driveEP_s_1_weightByDrive_params = load_params(xgb_R_driveEP_s_1_weightByDrive_model_name)[[1]]
xgb_R_driveEP_s_1_weightByDrive_nrounds = load_params(xgb_R_driveEP_s_1_weightByDrive_model_name)[[2]]
xgb_R_driveEP_s_1_weightByDrive_catalytic = load_params(xgb_R_driveEP_s_1_weightByDrive_model_name)[[3]]

####
xgb_R_driveEP_s_2_weightByDrive_model_name = "xgb_R_driveEP_s_2_weightByDrive"
xgb_R_driveEP_s_2_weightByDrive_features = 
  c("yardline_100", "down",
    "ydstogo", "half_seconds_remaining", "era_A", 
    "posteam_timeouts_remaining",  "defteam_timeouts_remaining", 
    "score_differential", "posteam_spread"
  )
xgb_R_driveEP_s_2_weightByDrive_monotonicities =  c(-1,-1, -1,0,1, 1,-1, 0,-1)
xgb_R_driveEP_s_2_weightByDrive_params = load_params(xgb_R_driveEP_s_2_weightByDrive_model_name)[[1]]
xgb_R_driveEP_s_2_weightByDrive_nrounds = load_params(xgb_R_driveEP_s_2_weightByDrive_model_name)[[2]]
xgb_R_driveEP_s_2_weightByDrive_catalytic = load_params(xgb_R_driveEP_s_2_weightByDrive_model_name)[[3]]

#################################################################
#################################################################
# ##################################################################
# ### OLS Models fit from All Downs with Random Effect for Epoch ###
# library(lme4)
# 
# fit_lm_s2dE_R <- function(dataset, w=FALSE) {
#   if (w) { w = dataset$w } else { w = rep(1, nrow(dataset)) }
#   fit = lmer(pts_next_score ~
#                bs(half_seconds_remaining, df=3, knots=c(30,120))*bs(yardline_100, df=5):factor(down) +
#                log(ydstogo):factor(down) +
#                utm:as.numeric(posteam_timeouts_remaining==0) +
#                I((score_differential <= -11)) + ### need a TD
#                I((score_differential <= -4)*(game_seconds_remaining <= 900)) + ### need a TD   ### note:: fourth_quarter == game_seconds_remaining <= 900
#                I((-3 <= score_differential & score_differential <= 0)*(game_seconds_remaining <= 900)) + ### ok with a field goal
#                I((1 <= score_differential & score_differential <= 3)*(game_seconds_remaining <= 900)) + ### prefer a TD but ok with a field goal
#                I((4 <= score_differential & score_differential <= 10)*(game_seconds_remaining <= 900)) + ### ok with a field goal but game is still close
#                I((score_differential >= 11)) + ### comfortable, field goal is fine
#                factor(era_A) +
#                posteam_spread + posteam_spread:yardline_100 +
#                (1 | epoch),
#              weights = w, data = dataset)
#   # clean_lm(fit)
#   fit
# }
