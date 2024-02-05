
### for testing this file
# j = 1
# CATALYTIC=FALSE

args <- commandArgs(trailingOnly = TRUE)
j <- as.numeric(args[1]) ### index of `the`model_names_list`, to choose which model to tune, for parallelism
CATALYTIC = as.logical(args[2])
TUNE_ALL_6_PARAMS_AT_ONCE = TRUE

### load data
PRE_LOADED_TrainTestSplitAndTeamQualities = TRUE
source("A_train_test_main.R")
source("models_XGB.R")

##################
### file setup ###
##################

print("$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$")
print(paste0("CATALYTIC = ", CATALYTIC, " and j = ", j))
print("$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$")

if (!CATALYTIC) {
  print_every_n = 50 
  nrounds = 15000
  nrow_grid_full = 100 #250
} else { ### Catalytic
  print_every_n = 25 
  nrounds = 15000 
  
  M_vec = c(10**5)
  tau_vec = 10**c(5) 
  M_tau_df = expand.grid(M = M_vec, tau = tau_vec) %>% mutate(w = tau/M)
  nrow_grid_1 = 40 # number of combos of (max_depth, min_child_weight) to tune, nrow(grid1) == nrow_grid_1 *  nrow(M_tau_df)
  nrow_grid_2 = 18 # number of combos of (subsample, colsample_bytree) to tune, nrow(grid2) == (2 * nrow_grid_2 + 3) *  nrow(M_tau_df)
  nrow_grid_3 = 40 # number of combos of (eta, gamma) to tune, nrow(grid3) == nrow_grid_3 *  nrow(M_tau_df)
}

##################################################################
if (!CATALYTIC) { 
  model_names_list <- list(
    xgb_C_nflFastR_1_model_name,
    xgb_C_s_1_model_name,
    xgb_C_oq2xdq2x_1_model_name,
    xgb_C_s_1_wbe_model_name,
    xgb_C_oq2xdq2x_1_wbe_model_name
  )
} else { ### Catalytic
  model_names_list <- list(
    xgb_C_s_1_model_name
  )
  ### make sure the indices of the below list match the indices of the above list...
  catalytic_model_names_list <- list(
    "mlr_yurko_s1d"
  )
}
##################################################################

num_models = length(model_names_list)
num_models
print(model_names_list)

###########################
### Tune the parameters ###
###########################

model_name <- model_names_list[[j]]
catalytic_model_name = if (CATALYTIC) catalytic_model_names_list[[j]]
xgb_features <- get(paste0(model_name, "_features"))
xgb_is_Regression = str_detect(model_name, "xgb_R_")
xgb_is_BoundedRegression = str_detect(model_name, "xgb_BR_")
xgb_is_weightedByEpoch = str_detect(model_name, "_wbe")
xgb_is_weightedByGame = str_detect(model_name, "_wbg")
xgb_monotonicities <- if (xgb_is_Regression | xgb_is_BoundedRegression) get(paste0(model_name, "_monotonicities"))  
xgb_fit_on_110_data = str_detect(model_name, "110")
num_features = length(xgb_features)

if (xgb_is_BoundedRegression | xgb_is_Regression) {
  xgb_MINmaxdepth = case_when(num_features <= 3 ~ 1, TRUE ~ round(num_features*0.5))
  xgb_MAXmaxdepth = num_features
  mtry_range = c( ifelse(round(num_features*0.4) < 1, 1, round(num_features*0.4)),   num_features )
} else { ### xgb classification
  xgb_MINmaxdepth = case_when(num_features <= 3 ~ 1, TRUE ~ round(num_features*0.225))
  xgb_MAXmaxdepth = case_when(
    num_features <= 4 ~ round(num_features*1),
    4 < num_features & num_features <= 8 ~ round(num_features*0.75),
    8 < num_features & num_features <= 15 ~ round(num_features*0.6),
    num_features >= 16 ~ round(num_features*0.45),
  )
  mtry_range = c( ifelse(round(num_features*0.55) < 1, 1, round(num_features*0.55)),   num_features )
}

TRAIN_SET = if (xgb_fit_on_110_data) train_set_110 else train_set

#########################################################
### Create XGBoost parameter combos:: Latin Hypercube ###
#########################################################

### https://datascience.stackexchange.com/questions/108233/recommendations-for-tuning-xgboost-hyperparams

### tune all params at once
create_grid_full <- function() {
  grid_latin_hypercube(
    tree_depth(range = c(xgb_MINmaxdepth, xgb_MAXmaxdepth)),
    min_n(),
    mtry(range = mtry_range),
    learn_rate(range = c(-1.5, -1), trans = scales::log10_trans()),
    loss_reduction(),
    sample_size = sample_prop(),
    size = nrow_grid_full
  ) %>% mutate(
    sample_size = round(sample_size,2),
    mtry = round(mtry, 2),
    mtry = mtry / num_features
  ) %>% rename(
    eta = learn_rate,
    gamma = loss_reduction,
    subsample = sample_size,
    colsample_bytree = mtry,
    max_depth = tree_depth,
    min_child_weight = min_n
  )
}
# create_grid_full()

### first, tune max_depth and min_child_weight
create_grid_1 <- function() {
  grid_latin_hypercube(
    # tree_depth(),
    tree_depth(range = c(xgb_MINmaxdepth, xgb_MAXmaxdepth)),
    min_n(),
    size = nrow_grid_1
  ) %>% mutate(
    eta = 0.1,
    gamma = 0.0,
    subsample = 1.0,
    colsample_bytree = 1.0
  ) %>% rename(
    max_depth = tree_depth,
    min_child_weight = min_n
  ) %>% relocate(
    max_depth, .after = colsample_bytree
  ) %>% relocate(
    min_child_weight, .after = max_depth
  )
}
# data.frame(create_grid_1())

### second, tune subsample and colsample_bytree
create_grid_2 <- function(max_depth, min_child_weight) {
  grid_2 = bind_rows(
    grid_latin_hypercube(
      sample_size = sample_prop(range = c(0.7, 1)), # hunch is that this range is better
      finalize(mtry(), TRAIN_SET), # because mtry depends on # of columns in data
      size = nrow_grid_2
    ),
    grid_latin_hypercube(
      sample_size = sample_prop(),
      finalize(mtry(), TRAIN_SET), # because mtry depends on # of columns in data
      size = nrow_grid_2
    )
  ) %>% mutate(
    sample_size = round(sample_size,2),
    mtry = round(mtry, 2),
    mtry = mtry / length(TRAIN_SET),
    eta = 0.1,
    gamma = 0.0,
    max_depth = max_depth,
    min_child_weight = min_child_weight
  ) %>% rename(
    subsample = sample_size,
    colsample_bytree = mtry
  ) %>% relocate(
    subsample, .after = gamma
  ) %>% relocate(
    colsample_bytree, .after = subsample
  )
  if (!CATALYTIC) {
    grid_2 = bind_rows(
      tibble(eta = 0.1, gamma = 0, subsample = 1, colsample_bytree=1, max_depth=max_depth, min_child_weight=min_child_weight), 
      tibble(eta = 0.1, gamma = 0, subsample = seq(0.5,0.9,by=0.1), colsample_bytree=1, max_depth=max_depth, min_child_weight=min_child_weight), 
      tibble(eta = 0.1, gamma = 0, subsample = 1, colsample_bytree=seq(0.5,0.9,by=0.1), max_depth=max_depth, min_child_weight=min_child_weight), 
      grid_2
    )
  } else {
    grid_2 = bind_rows(
      tibble(eta = 0.1, gamma = 0, subsample = 1, colsample_bytree=1, max_depth=max_depth, min_child_weight=min_child_weight), 
      tibble(eta = 0.1, gamma = 0, subsample = seq(0.9,0.9,by=0.1), colsample_bytree=1, max_depth=max_depth, min_child_weight=min_child_weight), 
      tibble(eta = 0.1, gamma = 0, subsample = 1, colsample_bytree=seq(0.9,0.9,by=0.1), max_depth=max_depth, min_child_weight=min_child_weight), 
      grid_2
    )
  }
  grid_2
}
# data.frame(create_grid_2(9, 3))

### third, tune eta and gamma
create_grid_3 <- function(subsample, colsample_bytree, max_depth, min_child_weight) {
  grid_3 = grid_latin_hypercube(
    # learn_rate(), 
    learn_rate(range = c(-2.5, -0.5), trans = scales::log10_trans()),
    loss_reduction(),
    size = nrow_grid_3
  ) %>% mutate(
    subsample = subsample,
    colsample_bytree = colsample_bytree,
    max_depth = max_depth,
    min_child_weight = min_child_weight
  ) %>% rename(
    eta = learn_rate,
    gamma = loss_reduction
  ) %>% relocate(
    gamma, .before = subsample
  ) %>% relocate(
    eta, .before = gamma
  )
  grid_3 = bind_rows(
    tibble(eta = 0.1, gamma = 0, subsample = subsample, colsample_bytree=colsample_bytree, max_depth=max_depth, min_child_weight=min_child_weight), 
    grid_3
  )
  grid_3
}
# data.frame(create_grid_3(0.95, 0.85, 9, 3))

### add M and tau to parameter list
add_M_tau_toGrid <- function(M_tau_df, grid) {
  ### for Catalytic models, add M and tau to the parameter grid
  grid_ = tibble()
  for (ii in 1:nrow(M_tau_df)) {
    grid_ = bind_rows(
      grid_,
      grid %>% mutate(M = M_tau_df$M[ii], tau = M_tau_df$tau[ii],)
    )
  }
  grid = grid_
  return(grid)
}
# add_M_tau_toGrid(M_tau_df, create_grid_1())
# add_M_tau_toGrid( tibble(M=c(100,200),tau=c(10,5)), create_grid_1())

grid_to_ParamList <- function(grid) {
  ### put into format suitable for my code
  parameters_list = list()
  for (iter in 1:nrow(grid)){
    if (xgb_is_BoundedRegression) {
      param <- list(booster = "gbtree",  
                    objective = "reg:logistic",
                    eval_metric = c("mae"))
    } else if (xgb_is_Regression) {
      param <- list(booster = "gbtree",  
                    objective = "reg:squarederror",
                    eval_metric = c("mae"))
    } else {
      param <- list(booster = "gbtree",  
                    objective = "multi:softprob",
                    eval_metric = c("mlogloss"),
                    num_class = 7)
    }
    param <- c(param,
               list(
                 eta = grid$eta[iter],
                 gamma = grid$gamma[iter],
                 subsample = grid$subsample[iter],
                 colsample_bytree = grid$colsample_bytree[iter],
                 max_depth = grid$max_depth[iter],
                 min_child_weight = grid$min_child_weight[iter]
               ))
    if (xgb_is_BoundedRegression | xgb_is_Regression) {
      param$monotone_constraints = xgb_monotonicities
    }
    if (CATALYTIC) {
      param$M = grid$M[iter]
      param$tau = grid$tau[iter]
    }
    parameters_list[[iter]] <- param
  }
  return(parameters_list)
}
# grid_to_ParamList(create_grid_1())

plot_xgb_tune_results <- function(losses, grid_) {
  grid_$loss = losses
  plot_ =  grid_ %>%
    dplyr::select(loss, eta, gamma, subsample, colsample_bytree, max_depth, min_child_weight) %>%
    tidyr::pivot_longer(eta:min_child_weight,
                        values_to = "value",
                        names_to = "parameter"
    ) %>%
    ggplot(aes(value, loss, color = parameter)) +
    geom_point(alpha = 0.8, show.legend = FALSE, size = 3) +
    facet_wrap(~parameter, scales = "free_x") +
    labs(x = NULL, y = "loss") +
    theme_bw()
  ggsave(paste0("job_output/plot_tuning_", model_name, ".png"), plot_, width=8, height=6)
}

#######################
### Get Best Params ###
#######################

evaluate_param_combo <- function(params) {
  
  VAL_SET_FORTUNING = TRAIN_SET %>% filter(val_play) 
  TRAIN_SET_FORTUNING = TRAIN_SET %>% filter(!val_play) 
  
  ### catalytic training data is made in here too!
  xgb <- train_xgb(xgb_features, TRAIN_SET_FORTUNING, params,
                   nrounds=1500, ## dummy, not used, since param_tuning=TRUE
                   watchSet=VAL_SET_FORTUNING,
                   w=FALSE, ## dummy, the value of catalytic will determine whether w is T or F
                   catalytic=CATALYTIC, 
                   Regression=xgb_is_Regression, BoundedRegression=xgb_is_BoundedRegression, wp=FALSE,
                   catalytic_model_name=catalytic_model_name, param_tuning=TRUE, print_every_n=print_every_n,
                   weight_by_epoch=xgb_is_weightedByEpoch, weight_by_game=xgb_is_weightedByGame
  )
  
  # bundle up the results together for returning
  output <- params
  output$nrounds <- xgb$best_iteration
  output$test_loss <- xgb$best_score
  
  ### tune the xgboost fit using w_RMSE, not logloss
  if (xgb_is_BoundedRegression | !xgb_is_Regression) { 
    output$test_loss <- RMSE(
      VAL_SET_FORTUNING$pts_next_score, 
      predict_ep_xgb(xgb, VAL_SET_FORTUNING, xgb_features, model_name, 
                     Regression=xgb_is_Regression, BoundedRegression=xgb_is_BoundedRegression)$pred,
      if (xgb_is_weightedByEpoch) VAL_SET_FORTUNING$w else rep(1, nrow(VAL_SET_FORTUNING))
    )
    # output$test_loss <- MAE(
    #   VAL_SET_FORTUNING$pts_next_score, 
    #   predict_ep_xgb(xgb, VAL_SET_FORTUNING, xgb_features, model_name, 
    #                  Regression=xgb_is_Regression, BoundedRegression=xgb_is_BoundedRegression)$pred
    # )
  } 
  
  print(paste("Loss for this param combo is", output$test_loss))
  return(output)
}

evaluate_param_list <- function(paramList, print_phase_numStr="") {
  best_loss = Inf
  best_params = NA
  num_param_combos = length(paramList)
  losses = c()
  for (i in 1:num_param_combos) {
    print("")
    print(paste("*****", "model", paste0(model_name, if (CATALYTIC) "_catalytic" else ""), 
                "phase",  print_phase_numStr, "tuning param combo", i, "of", num_param_combos, "*****"))
    print("")
    result_i = evaluate_param_combo(paramList[[i]])
    losses = c(losses, result_i$test_loss)
    if (result_i$test_loss < best_loss) {
      best_loss = result_i$test_loss
      best_params = result_i
    }
  }
  
  if (TUNE_ALL_6_PARAMS_AT_ONCE) { ### tune all 6 params at once
    return(list(best_params, losses))
  } else { ### tune params in 3 groups of 2
    return(best_params)
  }
}

get_best_params <- function() {
  
  if (TUNE_ALL_6_PARAMS_AT_ONCE) { ### tune all 6 params at once
    print("tune all six params at once")
    grid_ = create_grid_full()
    if (CATALYTIC) { grid_ = add_M_tau_toGrid(M_tau_df, grid_) }
    paramList = grid_to_ParamList(grid_)
    e_ = evaluate_param_list(paramList, print_phase_numStr="ONLY")
    best_params = e_[[1]]
    losses = e_[[2]]
    plot_xgb_tune_results(losses, grid_) 
  } else { ### tune params in 3 groups of 2
    print("first, tune max_depth and min_child_weight")
    grid1 = create_grid_1()
    if (CATALYTIC) { grid1 = add_M_tau_toGrid(M_tau_df, grid1) }
    paramList1 = grid_to_ParamList(grid1)
    best_params_1 = evaluate_param_list(paramList1, print_phase_numStr="1")
    best_max_depth = best_params_1$max_depth
    best_min_child_weight = best_params_1$min_child_weight
    
    print("second, tune subsample and colsample_bytree")
    grid2 = create_grid_2(best_max_depth, best_min_child_weight)
    if (CATALYTIC) { grid2 = add_M_tau_toGrid(M_tau_df, grid2) }
    paramList2 = grid_to_ParamList(grid2)
    best_params_2 = evaluate_param_list(paramList2, print_phase_numStr="2")
    best_subsample = best_params_2$subsample
    best_colsample_bytree = best_params_2$colsample_bytree
    
    print("third, tune eta and gamma")
    grid3 = create_grid_3(best_subsample, best_colsample_bytree, best_max_depth, best_min_child_weight)
    if (CATALYTIC) { grid3 = add_M_tau_toGrid(M_tau_df, grid3) }
    paramList3 = grid_to_ParamList(grid3)
    best_params_3 = evaluate_param_list(paramList3, print_phase_numStr="3")
    best_params = best_params_3
  }
  
  return(best_params)
}

best_params = get_best_params()

######################
### HPrint Results ###
######################

### print results
print("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~")
print(paste0("in this param tuning session, CATALYTIC = ", CATALYTIC))
print(paste("best params for model", paste0(model_name, if (CATALYTIC) "_catalytic" else ""), "is"))
print(best_params)

###
tuning_results = c(list(
    model_name = model_name,
    CATALYTIC = CATALYTIC
  ),best_params
)
list.save(tuning_results, paste0("param_tuning_results/", paste0(model_name, if (CATALYTIC) "_catalytic" else ""  ), ".yaml"))






