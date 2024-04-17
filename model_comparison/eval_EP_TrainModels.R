
source("eval_EP_Header.R")

#######################
### TRAIN EP MODELS ###
#######################

for (j in 1:length(xgb_model_names_list)) {
# for (j in 3:4) {
  print(paste0("bootstrapped dataset b=", b, "/", B, " for model", " j=",j,"/",length(xgb_model_names_list)))

  ### model j's attributes
  model_name <- xgb_model_names_list[[j]]
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
    xgb_is_Regression = str_detect(target_model_name, "xgb_R_")
    xgb_is_BoundedRegression = str_detect(target_model_name, "xgb_BR_")
    xgb_monotonicities <- if (xgb_is_Regression | xgb_is_BoundedRegression) get(paste0(target_model_name, "_monotonicities"))  
    xgb_params <- get(paste0(target_model_name, "_params"))
    xgb_nrounds <- get(paste0(target_model_name, "_nrounds"))
  } else if (model_type == "MLR") {
    mlr_model_name = str_remove_all(target_model_name, "_weightByDrive|_weightByEpoch")
    fit_mlr_func = get(paste0("fit_", mlr_model_name))
  }
  xgb_is_weightedByEpoch = str_detect(target_model_name, "weightByEpoch")
  xgb_is_weightedByDrive = str_detect(target_model_name, "weightByDrive")
  xgb_is_randomlyDrawOnePlayPerGroup = str_detect(target_model_name, "randomlyDrawOnePlayPerGroup")
  
  if (train_test) {
    dataset = train_set
    filename_prefix = "trainedModel_"
  } else {
    dataset = data_full
    filename_prefix = "trainedFullModel_"
  }
  filename = paste0("fitted_models/", filename_prefix, model_name,"_b",b,".rds")
  print(paste0("filename = ", filename))
  
  ### bootstrapped training dataset
  if (b == 0) { ### use original training dataset
    train_set_b = dataset
  } else { ### bootstrapped training dataset
    set.seed(22+b*3788)
    if (xgb_is_weightedByDrive | xgb_is_weightedByEpoch | xgb_is_randomlyDrawOnePlayPerGroup) {
      ### re-sample drives or epochs with replacement, according to group_var
      train_set_b = get_clustered_bootstrap_dataset(dataset, group_var)
    } else {
      ### re-sample plays with replacement
      train_set_b = get_iid_bootstrap_dataset(dataset)
    }
  }
  
  ### train the EP model on this dataset
  if (!xgb_is_randomlyDrawOnePlayPerGroup) {
    if (model_type == "XGB") {
      fit <- train_xgb(
        xgb_features, train_set_b, xgb_params, xgb_nrounds, watchSet=test_set, 
        epoch_based_EP=epoch_based_EP, drive_based_EP=drive_based_EP, 
        weight_by_epoch=xgb_is_weightedByEpoch, weight_by_drive=xgb_is_weightedByDrive,
        Regression=xgb_is_Regression, BoundedRegression=xgb_is_BoundedRegression,
        catalytic=model_is_catalytic, M=M, phi=phi, catalytic_prior_model_name=prior_model_name
      )
    } else if (model_type == "MLR") {
      if (xgb_is_weightedByDrive) {
        fit = fit_mlr_weightedByDrive(train_set_b, fit_model_func=fit_mlr_func)
      } else if (xgb_is_weightedByEpoch) {
        fit = fit_mlr_weightedByEpoch(train_set_b, fit_model_func=fit_mlr_func)
      } else {
        fit = fit_mlr_func(train_set_b)
      }
    } else {
      stop()
    }
  } else {
    if (model_type == "XGB") {
      fit = train_xgb_randomlyDrawnPlayPerGroup(
        target_model_name, xgb_features, train_set_b, xgb_params, xgb_nrounds, test_set, 
        Regression=xgb_is_Regression, BoundedRegression=xgb_is_BoundedRegression, drive_based_EP=TRUE, N=N_train
      ) 
    } else if (model_type == "MLR") {
      stop()
    } else {
      stop()
    }
  }
  
  ### save the model
  saveRDS(fit, filename)
}

