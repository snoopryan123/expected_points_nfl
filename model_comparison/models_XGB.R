
# modelswd = getwd()
# setwd("..")
# source("catalytic_110_main.R")
# setwd(modelswd)

########################
### Helper Functions ###
########################

map_n77_to_01 <- function(x) {
  ### input vector x includes points of the next score, with values from -7 to 7
  (x + 7)/14
}

map_01_to_n77 <- function(y) {
  ### input vector yincludes points of the next score, already mapped into 0 to 1
  14*y - 7
}

get_xgb_train_DMatrix <- function(xgb_features, train_set, params, w=FALSE, catalytic=FALSE, 
                                  Regression=FALSE, BoundedRegression=FALSE, wp=FALSE, catalytic_model_name="") {
  
  ### add fake catalytic data to xgboost
  if (catalytic) {
    ### get catalytic train set.  Catalytic params (M,tau) are included in `params`
    if (catalytic_model_name == "") { stop("FIXME:", "should specify the catalytic model in train_xgb") }
    catalytic_model_type = case_when(
      str_detect(catalytic_model_name, "mlr") ~ "MLR",
      str_detect(catalytic_model_name, "gam") ~ "GAM",
      str_detect(catalytic_model_name, "lm") ~ "OLS",
    )
    catalytic_outcome = if (BoundedRegression | Regression | wp) "numeric" else "categorical"
    train_set = get_catalytic_set(params$M, params$tau, train_set, catalytic_model_type, 
                                  catalytic_model_name, catalytic_outcome, params$U, params$MU)
    params = within(params, rm(M))
    params = within(params, rm(tau))
    w = TRUE ### catalytic xgboost must be weighted
  }
  
  ### row-weights for xgboost
  if (!w) {
    xgb_weights = rep(1, nrow(train_set))
  } else {
    xgb_weights = train_set$w
  }
  
  ### create response column (label) for xgboost
  if (Regression) { ### numeric expected points 
    train_labels_xgb = train_set$pts_next_score
  } else if (BoundedRegression) { ### bounded numeric expected points in [-7,7], mapped to [0,1]
    train_labels_xgb = map_n77_to_01( train_set$pts_next_score )
  }
  else if (wp) { ### win probability {0,1}
    train_labels_xgb = train_set$label_win
  } else { ### categorical expected points {-7,-3,...,3,7}
    train_labels_xgb = train_set$label
  }
  
  # browser()
  
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
                      w=FALSE, catalytic=FALSE, Regression=FALSE, BoundedRegression=FALSE, wp=FALSE, catalytic_model_name="", 
                      param_tuning=FALSE, print_every_n=50, weight_by_epoch=FALSE, weight_by_game=FALSE) {
  # browser()
  if (weight_by_epoch) {
    train_set = train_set %>% group_by(epoch) %>% mutate(w = 1/n()) %>% ungroup()
    w = TRUE
  } else if (weight_by_game) {
    train_set = train_set %>% group_by(game_id) %>% mutate(w = 1/n()) %>% ungroup()
    w = TRUE
  } else {
    train_set = train_set %>% mutate(w = 1) 
  }
  
  train_set_xgbDM =  get_xgb_train_DMatrix(
    xgb_features, train_set, params, w=w, catalytic=catalytic, 
    Regression=Regression, BoundedRegression=BoundedRegression, wp=wp, catalytic_model_name=catalytic_model_name
  ) 
    
  if (is.data.frame(watchSet)) { ### validation set evaluation
    val_features_xgb = watchSet %>% select(all_of(xgb_features))
    
    ### create watchlist (val set) response column (label) for xgboost
    if (Regression) { ### numeric expected points 
      val_labels_xgb = watchSet$pts_next_score
    } else if (BoundedRegression) { ### bounded numeric expected points in [-7,7], mapped to [0,1]
      val_labels_xgb = map_n77_to_01( watchSet$pts_next_score )
    }
    else if (wp) { ### win probability {0,1}
      val_labels_xgb = watchSet$label_win
    } else { ### categorical expected points {-7,-3,...,3,7}
      val_labels_xgb = watchSet$label
    }
    
    val_set_xgbDM = xgboost::xgb.DMatrix(
      model.matrix(~ . + 0, data = val_features_xgb),
      label = val_labels_xgb
    )
    watchlist <- list(train=train_set_xgbDM, validation=val_set_xgbDM)
  } else { ### just train set for watchlist
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

predict_probs_xgb <-  function(xgb, test_set, xgb_features, wp=FALSE) {
  test_features_xgb =  test_set %>% select(all_of(xgb_features))
  test_set_xgbDM = xgboost::xgb.DMatrix(as.matrix(test_features_xgb))
  xgb_pred = predict(xgb, test_set_xgbDM)
  if (!wp) { ### expected points model
    pred_matrix = matrix(xgb_pred, ncol=7, nrow=length(xgb_pred)/7, byrow=TRUE)
    colnames(pred_matrix) = c("Touchdown","Opp_Touchdown","Field_Goal","Opp_Field_Goal","Safety","Opp_Safety","No_Score")
    return(pred_matrix)
  } else { ### win probability model
    return(xgb_pred)
  }
}

predict_ep_xgb <- function(xgb, test_set, xgb_features, model_name, Regression=FALSE, BoundedRegression=FALSE) {
  ### get expected points predictions
  if (Regression) { ### numeric expected points 
    test_features_xgb =  test_set %>% select(all_of(xgb_features))
    test_set_xgbDM = xgboost::xgb.DMatrix(as.matrix(test_features_xgb))
    pred_ep = predict(xgb, test_set_xgbDM)
  } else if (BoundedRegression) { ### bounded numeric expected points in [-7,7], mapped to [0,1]
    test_features_xgb =  test_set %>% select(all_of(xgb_features))
    test_set_xgbDM = xgboost::xgb.DMatrix(as.matrix(test_features_xgb))
    pred_ep = map_01_to_n77(predict(xgb, test_set_xgbDM))
  } else { ### categorical expected points {-7,-3,...,3,7}
    pred_cg = predict_probs_xgb(xgb, test_set, xgb_features)
    pred_ep = pred_cg[,1]*(7) + pred_cg[,2]*(-7) + pred_cg[,3]*(3) + pred_cg[,4]*(-3) + pred_cg[,5]*(2) + pred_cg[,6]*(-2) + pred_cg[,7]*(0)
  }
  return( tibble(pred = pred_ep,  model=model_name) )
}

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

###############################################################
### XGBoost Tree-Classification Models Trained on All Downs ###
###############################################################

##############################
xgb_C_nflFastR_model_name = "xgb_C_nflFastR"
xgb_C_nflFastR_features = c(
  "yardline_100", "down1", "down2", "down4", "down4", "ydstogo", "half_seconds_remaining", 
  "era0", "era1", "era2", "era3", "era4", 
  "season", "home", "posteam_timeouts_remaining", "defteam_timeouts_remaining", "retractable", "dome"
)
# determined by nflFastR's parameter tuning, which is appropriate with their row weights and their dataset.
xgb_C_nflFastR_params <-  list(booster = "gbtree",
                               objective = "multi:softprob",
                               eval_metric = c("mlogloss"),
                               num_class = 7,
                               ################################
                               eta = 0.025,
                               gamma = 1,
                               max_depth = 5,
                               subsample = 0.8,
                               colsample_bytree = 0.8,
                               min_child_weight = 1
)
xgb_C_nflFastR_nrounds = 525
xgb_C_nflFastR_catalytic = FALSE

##############################
xgb_C_nflFastR_1_model_name = "xgb_C_nflFastR_1"
xgb_C_nflFastR_1_features = c(
  "yardline_100", "down1", "down2", "down4", "down4", "ydstogo", "half_seconds_remaining",
  "era0", "era1", "era2", "era3", "era4", 
  "season", "home", "posteam_timeouts_remaining", "defteam_timeouts_remaining", "retractable", "dome"
)
xgb_C_nflFastR_1_params = load_params(xgb_C_nflFastR_1_model_name)[[1]]
xgb_C_nflFastR_1_nrounds = load_params(xgb_C_nflFastR_1_model_name)[[2]]
xgb_C_nflFastR_1_catalytic = load_params(xgb_C_nflFastR_1_model_name)[[3]]

##############################
xgb_C_s_1_model_name = "xgb_C_s_1"
xgb_C_s_1_features = c("yardline_100", "half_seconds_remaining", "era_A",
                       "posteam_timeouts_remaining",  "half", "score_differential",
                       "down1", "down2", "down3", "down4", "ydstogo",
                       "posteam_spread"
)
xgb_C_s_1_params = load_params(xgb_C_s_1_model_name)[[1]]
xgb_C_s_1_nrounds = load_params(xgb_C_s_1_model_name)[[2]]
xgb_C_s_1_catalytic = load_params(xgb_C_s_1_model_name)[[3]]

# ##############################
# xgb_C_oq2xdq2x_1_model_name = "xgb_C_oq2xdq2x_1"
# xgb_C_oq2xdq2x_1_features = c("yardline_100", "half_seconds_remaining", "era_A",
#                               "posteam_timeouts_remaining",  "half", "score_differential",
#                               "down1", "down2", "down3", "down4", "ydstogo",
#                               "qbq_ot_0_sum", "oq_rot_0_total_sum", 
#                               "qbq_dt_0_sum", "oq_rdt_0_sum", 
#                               "dq_dt_0_againstPass_sum", "dq_ot_0_againstPass_sum",
#                               "dq_dt_0_againstRun_sum", "dq_ot_0_againstRun_sum"
# )
# xgb_C_oq2xdq2x_1_params = load_params(xgb_C_oq2xdq2x_1_model_name)[[1]]
# xgb_C_oq2xdq2x_1_nrounds = load_params(xgb_C_oq2xdq2x_1_model_name)[[2]]
# xgb_C_oq2xdq2x_1_catalytic = load_params(xgb_C_oq2xdq2x_1_model_name)[[3]]

# ##############################
# xgb_C_nflFastR_oq2xdq2x_1_model_name = "xgb_C_nflFastR_oq2xdq2x_1"
# xgb_C_nflFastR_oq2xdq2x_1_features = c(
#   "yardline_100", "down1", "down2", "down4", "down4", "ydstogo", "half_seconds_remaining",
#   "qbq_ot_0_sum", "oq_rot_0_total_sum", 
#   "qbq_dt_0_sum", "oq_rdt_0_sum", 
#   "dq_dt_0_againstPass_sum", "dq_ot_0_againstPass_sum",
#   "dq_dt_0_againstRun_sum", "dq_ot_0_againstRun_sum",
#   "era0", "era1", "era2", "era3", "era4", 
#   "season", "home", "posteam_timeouts_remaining", "defteam_timeouts_remaining", "retractable", "dome"
# )
# xgb_C_nflFastR_oq2xdq2x_1_params = load_params(xgb_C_nflFastR_oq2xdq2x_1_model_name)[[1]]
# xgb_C_nflFastR_oq2xdq2x_1_nrounds = load_params(xgb_C_nflFastR_oq2xdq2x_1_model_name)[[2]]
# xgb_C_nflFastR_oq2xdq2x_1_catalytic = load_params(xgb_C_nflFastR_oq2xdq2x_1_model_name)[[3]]

# ##############################
# xgb_C_nflFastR_s_1_model_name = "xgb_C_nflFastR_s_1"
# xgb_C_nflFastR_s_1_features = c(
#   "yardline_100", "down1", "down2", "down4", "down4", "ydstogo", "half_seconds_remaining",
#   "posteam_spread",
#   "era0", "era1", "era2", "era3", "era4", 
#   "season", "home", "posteam_timeouts_remaining", "defteam_timeouts_remaining", "retractable", "dome"
# )
# xgb_C_nflFastR_s_1_params = load_params(xgb_C_nflFastR_s_1_model_name)[[1]]
# xgb_C_nflFastR_s_1_nrounds = load_params(xgb_C_nflFastR_s_1_model_name)[[2]]
# xgb_C_nflFastR_s_1_catalytic = load_params(xgb_C_nflFastR_s_1_model_name)[[3]]

# ##############################
# xgb_C_nflFastR_oq2xdq2x_2_model_name = "xgb_C_nflFastR_oq2xdq2x_2"
# xgb_C_nflFastR_oq2xdq2x_2_features = c(
#   "yardline_100", "half_seconds_remaining",
#   "posteam_timeouts_remaining", "defteam_timeouts_remaining",  "half", "score_differential",
#   "down", "ydstogo",
#   "qbq_ot_0_sum", "oq_rot_0_total_sum", 
#   "qbq_dt_0_sum", "oq_rdt_0_sum", 
#   "dq_dt_0_againstPass_sum", "dq_ot_0_againstPass_sum",
#   "dq_dt_0_againstRun_sum", "dq_ot_0_againstRun_sum",
#   "era0", "era1", "era2", "era3", "era4", 
#   "season", "home", "posteam_timeouts_remaining", "defteam_timeouts_remaining", "retractable", "dome"
# )
# xgb_C_nflFastR_oq2xdq2x_2_params = load_params(xgb_C_nflFastR_oq2xdq2x_2_model_name)[[1]]
# xgb_C_nflFastR_oq2xdq2x_2_nrounds = load_params(xgb_C_nflFastR_oq2xdq2x_2_model_name)[[2]]
# xgb_C_nflFastR_oq2xdq2x_2_catalytic = load_params(xgb_C_nflFastR_oq2xdq2x_2_model_name)[[3]]

# ##############################
# xgb_C_nflFastR_s_2_model_name = "xgb_C_nflFastR_s_2"
# xgb_C_nflFastR_s_2_features = c(
#   "yardline_100", "half_seconds_remaining",
#   "posteam_timeouts_remaining", "defteam_timeouts_remaining",  "half", "score_differential",
#   "down", "ydstogo",
#   "posteam_spread",
#   "era0", "era1", "era2", "era3", "era4", 
#   "season", "home", "posteam_timeouts_remaining", "defteam_timeouts_remaining", "retractable", "dome"
# )
# xgb_C_nflFastR_s_2_params = load_params(xgb_C_nflFastR_s_2_model_name)[[1]]
# xgb_C_nflFastR_s_2_nrounds = load_params(xgb_C_nflFastR_s_2_model_name)[[2]]
# xgb_C_nflFastR_s_2_catalytic = load_params(xgb_C_nflFastR_s_2_model_name)[[3]]

#################################################
### XGBoost Models that are weighted by epoch ###
#################################################

##############################
xgb_C_s_1_wbe_model_name = "xgb_C_s_1_wbe"
xgb_C_s_1_wbe_features = xgb_C_s_1_features
xgb_C_s_1_wbe_params = load_params(xgb_C_s_1_wbe_model_name)[[1]]
xgb_C_s_1_wbe_nrounds = load_params(xgb_C_s_1_wbe_model_name)[[2]]
xgb_C_s_1_wbe_catalytic = load_params(xgb_C_s_1_wbe_model_name)[[3]]

# ################################
# ### Catalytic XGBoost Models ###
# ################################
#
# ##############################
# xgb_BR_110_s_1_catalytic_model_name = "xgb_BR_110_s_1_catalytic" ###
# xgb_BR_110_s_1_catalytic_prior_name = "mlr_110_s2dE" ###
# xgb_BR_110_s_1_catalytic_paramFile = paste0(xgb_BR_110_s_1_catalytic_model_name, "_prior_", xgb_BR_110_s_1_catalytic_prior_name) ###
# xgb_BR_110_s_1_catalytic_features = get(paste0(str_remove(xgb_BR_110_s_1_catalytic_model_name, "_catalytic"), "_features"))
# # determined by testing on a validation set
# xgb_BR_110_s_1_catalytic_params = load_params(xgb_BR_110_s_1_catalytic_paramFile)[[1]]
# xgb_BR_110_s_1_catalytic_nrounds = load_params(xgb_BR_110_s_1_catalytic_paramFile)[[2]]
# xgb_BR_110_s_1_catalytic_catalytic = load_params(xgb_BR_110_s_1_catalytic_paramFile)[[3]]
# 
# ##############################
# xgb_C_110_s_1_catalytic_model_name = "xgb_C_110_s_1_catalytic" ###
# xgb_C_110_s_1_catalytic_prior_name = "mlr_110_s2dE" ###
# xgb_C_110_s_1_catalytic_paramFile = paste0(xgb_C_110_s_1_catalytic_model_name, "_prior_", xgb_C_110_s_1_catalytic_prior_name) ###
# xgb_C_110_s_1_catalytic_features = get(paste0(str_remove(xgb_C_110_s_1_catalytic_model_name, "_catalytic"), "_features"))
# # determined by testing on a validation set
# xgb_C_110_s_1_catalytic_params = load_params(xgb_C_110_s_1_catalytic_paramFile)[[1]]
# xgb_C_110_s_1_catalytic_nrounds = load_params(xgb_C_110_s_1_catalytic_paramFile)[[2]]
# xgb_C_110_s_1_catalytic_catalytic = load_params(xgb_C_110_s_1_catalytic_paramFile)[[3]]
# 
# ##############################
# xgb_C_s_1_catalytic_model_name = "xgb_C_s_1_catalytic" ###
# xgb_C_s_1_catalytic_prior_name = "mlr_yurko_s2dE" ###
# xgb_C_s_1_catalytic_full_name = paste0(xgb_C_s_1_catalytic_model_name, "_prior_", xgb_C_s_1_catalytic_prior_name)
# xgb_C_s_1_catalytic_paramFile = paste0(xgb_C_s_1_catalytic_model_name, "_prior_", xgb_C_s_1_catalytic_prior_name) ###
# xgb_C_s_1_catalytic_features = get(paste0(str_remove(xgb_C_s_1_catalytic_model_name, "_catalytic"), "_features"))
# # determined by testing on a validation set
# xgb_C_s_1_catalytic_params = load_params(xgb_C_s_1_catalytic_paramFile)[[1]]
# xgb_C_s_1_catalytic_nrounds = load_params(xgb_C_s_1_catalytic_paramFile)[[2]]
# xgb_C_s_1_catalytic_catalytic = load_params(xgb_C_s_1_catalytic_paramFile)[[3]]
# 
# # ##############################
# # xgb_C_s_1A_catalytic_model_name = "xgb_C_s_1_catalytic" ###
# # xgb_C_s_1A_catalytic_prior_name = "mlr_yurko_s2dE_w" ###
# # xgb_C_s_1A_catalytic_full_name = paste0(xgb_C_s_1A_catalytic_model_name, "_prior_", xgb_C_s_1A_catalytic_prior_name) ###
# # xgb_C_s_1A_catalytic_paramFile = paste0(xgb_C_s_1A_catalytic_model_name, "_prior_", xgb_C_s_1A_catalytic_prior_name) ###
# # xgb_C_s_1A_catalytic_features = get(paste0(str_remove(xgb_C_s_1A_catalytic_model_name, "_catalytic"), "_features"))
# # # determined by testing on a validation set
# # xgb_C_s_1A_catalytic_params = load_params(xgb_C_s_1A_catalytic_paramFile)[[1]]
# # xgb_C_s_1A_catalytic_nrounds = load_params(xgb_C_s_1A_catalytic_paramFile)[[2]]
# # xgb_C_s_1A_catalytic_catalytic = load_params(xgb_C_s_1A_catalytic_paramFile)[[3]]
# 
# ##############################
# xgb_C_s_1_wbe_catalytic_model_name = "xgb_C_s_1_wbe_catalytic" ###
# xgb_C_s_1_wbe_catalytic_prior_name = "mlr_yurko_s2dE_w" ###
# xgb_C_s_1_wbe_catalytic_full_name = paste0(xgb_C_s_1_wbe_catalytic_model_name, "_prior_", xgb_C_s_1_wbe_catalytic_prior_name) ###
# xgb_C_s_1_wbe_catalytic_paramFile = paste0(xgb_C_s_1_wbe_catalytic_model_name, "_prior_", xgb_C_s_1_wbe_catalytic_prior_name) ###
# xgb_C_s_1_wbe_catalytic_features = get(paste0(str_remove(xgb_C_s_1_wbe_catalytic_model_name, "_catalytic"), "_features"))
# # determined by testing on a validation set
# xgb_C_s_1_wbe_catalytic_params = load_params(xgb_C_s_1_wbe_catalytic_paramFile)[[1]]
# xgb_C_s_1_wbe_catalytic_nrounds = load_params(xgb_C_s_1_wbe_catalytic_paramFile)[[2]]
# xgb_C_s_1_wbe_catalytic_catalytic = load_params(xgb_C_s_1_wbe_catalytic_paramFile)[[3]]


