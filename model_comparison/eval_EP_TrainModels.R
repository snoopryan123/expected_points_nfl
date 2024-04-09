
source("eval_EP_Header.R")

#######################
### TRAIN EP MODELS ###
#######################

for (j in 1:length(xgb_model_names_list)) {
  print(paste0("bootstrapped dataset b=", b, "/", B, " for model", " j=",j,"/",length(xgb_model_names_list), " ", xgb_model_name))
  # print(paste0("model j=",j,"/",length(xgb_model_names_list)))
  xgb_model_name <- xgb_model_names_list[[j]]
  
  xgb_features <- get(paste0(xgb_model_name, "_features"))
  xgb_is_weightedByEpoch = str_detect(xgb_model_name, "weightByEpoch")
  xgb_is_weightedByDrive = str_detect(xgb_model_name, "weightByDrive")
  xgb_is_randomlyDrawOnePlayPerGroup = str_detect(xgb_model_name, "randomlyDrawOnePlayPerGroup")
  xgb_params <- get(paste0(xgb_model_name, "_params"))
  xgb_nrounds <- get(paste0(xgb_model_name, "_nrounds"))
  
  ### bootstrapped training dataset
  if (b == 0) { ### use original training dataset
    train_set_b = train_set
  } else { ### bootstrapped training dataset
    set.seed(22+b*3788)
    if (xgb_is_weightedByDrive | xgb_is_weightedByEpoch | xgb_is_randomlyDrawOnePlayPerGroup) {
      ### re-sample drives or epochs with replacement, according to group_var
      train_set_b = get_clustered_bootstrap_dataset(train_set, group_var)
    } else {
      ### re-sample plays with replacement
      train_set_b = get_iid_bootstrap_dataset(train_set)
    }
  }
    
  ### train the EP model on this dataset
  if (!xgb_is_randomlyDrawOnePlayPerGroup) {
    xgb <- train_xgb(
      xgb_features, train_set_b, xgb_params, xgb_nrounds, watchSet=test_set, 
      epoch_based_EP=epoch_based_EP, drive_based_EP=drive_based_EP, 
      weight_by_epoch=xgb_is_weightedByEpoch, weight_by_drive=xgb_is_weightedByDrive
    )
  } else {
    xgb = train_xgb_randomlyDrawnPlayPerGroup(
      xgb_model_name, xgb_features, train_set_b, xgb_params, xgb_nrounds, test_set, drive_based_EP=TRUE, N=N_train
    ) 
  }
    
  ### save the model
  filename = paste0("fitted_models/trainedModel_",xgb_model_name,"_b=",b,".rds")
  saveRDS(xgb, filename)
}

