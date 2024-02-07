
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

predict_probs_xgb <-  function(xgb, test_set, xgb_features, 
                               epoch_based_EP=FALSE, drive_based_EP=FALSE, wp=FALSE) {
  
  test_features_xgb =  test_set %>% select(all_of(xgb_features))
  test_set_xgbDM = xgboost::xgb.DMatrix(as.matrix(test_features_xgb))
  xgb_pred = predict(xgb, test_set_xgbDM)
  if (epoch_based_EP) {
    # print(data_full %>% distinct(outcome_epoch, pts_next_score) %>% arrange(outcome_epoch))
    pred_matrix = matrix(xgb_pred, ncol=7, nrow=length(xgb_pred)/7, byrow=TRUE)
    colnames(pred_matrix) = c("Touchdown","Opp_Touchdown","Field_Goal","Opp_Field_Goal","Safety","Opp_Safety","No_Score")
  } else if (drive_based_EP) {
    # print(data_full %>% distinct(outcome_drive, pts_end_of_drive) %>% arrange(outcome_drive))
    pred_matrix = matrix(xgb_pred, ncol=5, nrow=length(xgb_pred)/5, byrow=TRUE)
    colnames(pred_matrix) = c("Touchdown","Field_Goal","No_Score","Opp_Safety","Opp_Touchdown")
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

