
source("B_models_XGB.R")
source("B_models_MLR.R")

### catalytic model CACHE
fitted_catalytic_prior_models = list()

################################
### Catalytic XGBoost Models ###
################################

make_catalytic_model_name <- function(target_model_name, prior_model_name, M, phi) {
  paste0("catalytic_", target_model_name, "_prior_", prior_model_name, "_M_", M, "_phi_", phi)
}

get_catalytic_sub_model_names <- function(catalytic_model_name) {
  catalytic_model_names = str_split(catalytic_model_name, "_prior_|_M_|_phi_")[[1]]
  list(
    target = str_remove(catalytic_model_names[1], "catalytic_"),
    prior = catalytic_model_names[2],
    M = as.numeric(catalytic_model_names[3]),
    phi = as.numeric(catalytic_model_names[4])
  )
}

###
# catalytic_weightByDrive_1_model_name = make_catalytic_model_name(
#   xgb_C_driveEP_s_1_weightByDrive_model_name, "mlr_driveEP_yurko_s3dE_weightByDrive", M=1e5, phi=0.2
# )
# catalytic_weightByDrive_1_model_names = get_catalytic_sub_model_names(catalytic_weightByDrive_1_model_name)

####################################
### Fit the models and test them ###
####################################

### acquire catalytic model
get_catalytic_prior_model <- function(
    catalytic_prior_model_name, train_set, catalytic_prior_model_type="MLR"
) {
  print(paste("FITTING CATALYTIC PRIOR MODEL", catalytic_prior_model_name))
  if (catalytic_prior_model_type=="MLR") {
    fit_mlr_func = get(paste0("fit_",str_remove(catalytic_prior_model_name, "_weightByDrive|_weightByEpoch")))
    if (str_detect(model_name, "_weightByDrive")) {
      fit = fit_mlr_weightedByDrive(train_set, fit_model_func=fit_mlr_func)
    } else if (str_detect(model_name, "_weightByEpoch")) {
      fit = fit_mlr_weightedByEpoch(train_set, fit_model_func=fit_mlr_func)
    } else {
      fit = fit_mlr_func(train_set)
    }
  }
  fit
}

# ### check
# catpriormodel = get_catalytic_prior_model(
#   catalytic_prior_model_name = catalytic_weightByDrive_1_model_names$prior, 
#   train_set = data_full, catalytic_prior_model_type = "MLR"
# )

### generate synthetic X
generate_synthetic_X <- function(train_set, M, phi, weight_by_drive=FALSE, weight_by_epoch=FALSE) {
  ### M is the number of synthetic X rows (plays) we will generate
  ### phi is the fraction of the observed dataset's total row weight allocated to the synthetic data
  
  # ### re-sample observations (rows) from the X space with replacement(ignoring the drive/epoch clustering)
  # row_idxs = 1:nrow(train_set)
  # set.seed(2018) # Go Rams!
  # resampled_idxs = sort(sample(row_idxs, size=M, replace=TRUE))
  # df_synthetic_X = train_set[resampled_idxs,]
  # df_synthetic_X = df_synthetic_X %>% select(-c(all_of(starts_with(
  #   c("outcome_", "pts_", "drive_weight", "epoch_weight", "w")
  # ))))
  
  # ### uniformly sample across the X space (ignoring the drive/epoch clustering)
  # df_synthetic_X0 = train_set
  # df_synthetic_X0 = df_synthetic_X0 %>% select(-c(all_of(starts_with(
  #   c("outcome_", "pts_", "drive_weight", "epoch_weight", "w")
  # ))))
  # set.seed(2018) # Go Rams!
  # df_synthetic_X = as_tibble(lapply(df_synthetic_X0, function(x) sample(x, replace = TRUE, size = M)))
  
  ### re-sample drives with replacement until we hit M rows
  df_synthetic_X0 = train_set
  df_synthetic_X0 = df_synthetic_X0 %>% select(-c(all_of(starts_with(
    c("drive_weight", "epoch_weight", "w")
  ))))
  set.seed(2018) # Go Rams!
  #########
  df_synthetic_X1 = tibble()
  while (nrow(df_synthetic_X1) < M) {
    df_synthetic_X_new = get_clustered_bootstrap_dataset(df_synthetic_X0, group_var)
    df_synthetic_X1 = bind_rows(df_synthetic_X1, df_synthetic_X_new)
  } 
  last_grp = df_synthetic_X1[[group_var]][M]
  df_synthetic_X = df_synthetic_X1 %>% filter(row_number() <= M | .data[[group_var]] == last_grp)
  df_synthetic_X = df_synthetic_X %>% select(-c(all_of(starts_with(
    c("outcome_", "pts_", "ii")
  ))))
  ### check
  nrow(df_synthetic_X)
  sum(df_synthetic_X1[[group_var]] == last_grp)
  sum(df_synthetic_X[[group_var]] == last_grp)
  sum(df_synthetic_X1[[group_var]] == last_grp) == sum(df_synthetic_X[[group_var]] == last_grp) 
  
  ### row weights
  if (weight_by_drive) {
    total_OG_weight = sum(train_set$drive_weight)
  } else if (weight_by_epoch) {
    total_OG_weight = sum(train_set$epoch_weight)
  } else {
    total_OG_weight = nrow(train_set)
  }
  total_synthetic_weight = total_OG_weight*phi
  total_synthetic_weight_per_synthetic_row = total_synthetic_weight/M
  df_synthetic_X$w = total_synthetic_weight_per_synthetic_row
  df_synthetic_X$drive_weight = total_synthetic_weight_per_synthetic_row
  df_synthetic_X$epoch_weight = total_synthetic_weight_per_synthetic_row
  
  df_synthetic_X
}

# ### check
# tempX = generate_synthetic_X(data_full, M=1e5, phi=0.2, weight_by_drive=TRUE)
# dim(tempX)
# sum(tempX$drive_weight) / sum(data_full$drive_weight)

### generate synthetic Y
generate_synthetic_Y_mlr <- function(
    catalytic_prior_model, df_synthetic_X, 
    epoch_based_EP = FALSE, drive_based_EP = FALSE, 
    catalytic_prior_outcome="categorical"
) {
  
  if (catalytic_prior_outcome == "categorical") {
    probs = get_mlr_probs(catalytic_prior_model, df_synthetic_X, epoch_based_EP=epoch_based_EP, drive_based_EP=drive_based_EP)
    set.seed(2022) # Go Rams!
    if (epoch_based_EP) {
      df_generated_Y = tibble(outcome_epoch = generate_mlr_outcomes(probs))
      df_generated_Y = df_generated_Y %>% left_join(
        map_epoch_outcome_to_value %>% select(-all_of(ends_with("_str")))
      )
    } else if (drive_based_EP) {
      df_generated_Y = tibble(outcome_drive = generate_mlr_outcomes(probs))
      df_generated_Y = df_generated_Y %>% left_join(
        map_drive_outcome_to_value %>% select(-all_of(ends_with("_str")))
      )
    } else {
      stop()
    }
  } else {
    stop("FIXME: need to specify `catalytic_outcome` as categorical or numeric")
  }
  
  # df_synthetic_XY = bind_cols(df_synthetic_X, df_generated_Y)
  # df_synthetic_XY
  df_generated_Y
}

# ### check
# tempY = generate_synthetic_Y_mlr(catpriormodel, tempX, drive_based_EP=T)

### combine catalytic train set with observed train set
get_catalytic_set <- function(
  train_set, M, phi, catalytic_prior_model_name, catalytic_prior_model_type,
  epoch_based_EP, drive_based_EP, weight_by_drive=FALSE, weight_by_epoch=FALSE
) {
  
  ### obtain the catalytic model (either fit it, or get it from cache)
  cat_model_hash = serialize(list(catalytic_prior_model_type, catalytic_prior_model_name, dim(train_set)), NULL) #FIXME #train_set_name, not dim(train_set) ??
  cat_model_hash = paste(as.character(cat_model_hash), sep="", collapse="")
  if ( is.null(fitted_catalytic_prior_models[[cat_model_hash]]) ) {
    ### fit the catalytic model
    catpriormodel = get_catalytic_prior_model(
      catalytic_prior_model_name, train_set, catalytic_prior_model_type
    )
    fitted_catalytic_prior_models[[cat_model_hash]] <<- catpriormodel ### update global variable
  } else {
    ### get CACHED catalytic model
    catpriormodel = fitted_catalytic_prior_models[[cat_model_hash]]
  }
  
  # browser()
  ### generate synthetic dataset
  df_synthetic_X = generate_synthetic_X(train_set, M, phi, weight_by_drive, weight_by_epoch)
  df_synthetic_Y = generate_synthetic_Y_mlr(
    catpriormodel, df_synthetic_X, epoch_based_EP=epoch_based_EP, drive_based_EP=drive_based_EP
  )
  df_synthetic_XY = bind_cols(df_synthetic_X, df_synthetic_Y)
  
  ### generate combined dataset that combines synthetic catalytic data with observed data 
  df_synthetic_XY$catalytic_data = TRUE
  train_set$catalytic_data = FALSE
  if (drive_based_EP) {
    train_set$w = train_set$drive_weight
  } else if (epoch_based_EP) {
    train_set$w = train_set$epoch_weight
  } else {
    train_set$w = 1
  }
  # browser()
  catalytic_train_set = bind_rows(
    df_synthetic_XY,
    train_set %>% select(colnames(df_synthetic_XY)) 
  )
  catalytic_train_set = catalytic_train_set[sample(nrow(catalytic_train_set)),] ### reshuffle rows
  catalytic_train_set
}

# ### check
# tempXY = get_catalytic_set(
#     train_set=data_full, M=1e5, phi=0.2, 
#     catalytic_prior_model_name=catalytic_weightByDrive_1_model_names$prior, 
#     catalytic_prior_model_type="MLR",
#     epoch_based_EP=F, drive_based_EP=T, weight_by_drive=T, weight_by_epoch=F
# )


