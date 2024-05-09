
args <- commandArgs(trailingOnly = TRUE)
j <- as.numeric(args[1])

PRE_LOADED_TrainTestSplitAndTeamQualities = TRUE #FIXME
########################
filewd = getwd()
setwd("..")
source("A1_load_data.R") 
setwd("model_comparison/")
source("models_XGB.R")
source("models_MLR.R")
source("models_catalytic.R")
setwd(filewd)
########################

### models
# model_names_list <- list(
#   # xgb_C_driveEP_oq2xdq2x_1_weightByDrive_model_name,
#   xgb_C_driveEP_s_1_weightByDrive_model_name,
#   xgb_R_driveEP_s_1_weightByDrive_model_name
# )
# phis = 0.5; M = 5e5;
phis = c(0.5,1); M = 5e5;
model_names_list <- 
  make_catalytic_model_name(
    target_model_name = xgb_C_driveEP_s_1_weightByDrive_model_name, 
    prior_model_name = "mlr_driveEP_yurko_s3dE_weightByDrive",
    M=M, phi=phis
  )
model_names_list

###########################################
### train XGBoost model on full dataset ###
###########################################

for (j in 1:length(model_names_list)) {
# for (j in 5:5) {
  print(paste0("j=",j,"/",length(model_names_list)))
# {
  
  ### model j's attributes
  model_name <- model_names_list[[j]]
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
  xgb_is_epochBasedEP = str_detect(target_model_name, "epochEP")
  xgb_is_driveBasedEP = str_detect(target_model_name, "driveEP")
  xgb_is_weightedByEpoch = str_detect(target_model_name, "weightByEpoch")
  xgb_is_weightedByDrive = str_detect(target_model_name, "weightByDrive")
  xgb_is_randomlyDrawOnePlayPerGroup = str_detect(target_model_name, "randomlyDrawOnePlayPerGroup")
  
  xgb_fit = train_xgb(
    xgb_features, data_full, xgb_params, xgb_nrounds, watchSet="none", 
      epoch_based_EP=xgb_is_epochBasedEP, drive_based_EP=xgb_is_driveBasedEP, wp=FALSE,
      weight_by_epoch=xgb_is_weightedByEpoch, weight_by_drive=xgb_is_weightedByDrive,
      Regression=xgb_is_Regression, BoundedRegression=xgb_is_BoundedRegression,
      catalytic=model_is_catalytic, M=M, phi=phi, catalytic_prior_model_name=prior_model_name
    )
  xgb.save(xgb_fit, paste0("trainedFullModel_", model_name, ".xgb"))
}



# predict_ep_xgb(xgb_fit, tibble(yardline_100=1:99), xgb_features, model_name, Regression=xgb_regression) %>%
#   mutate(i = row_number()) %>%
#   ggplot() +
#   geom_line(aes(x=i,y=pred))


