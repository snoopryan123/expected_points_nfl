
args <- commandArgs(trailingOnly = TRUE)
j <- as.numeric(args[1])

PRE_LOADED_TrainTestSplitAndTeamQualities = TRUE #FIXME
########################
filewd = getwd()
setwd("..")
source("A1_load_data.R") 
setwd(filewd)
########################

source("models_XGB.R")
source("models_MLR.R")
model_names_list <- list(
  xgb_C_driveEP_s_1_weightByDrive_model_name,
  # xgb_C_driveEP_oq2xdq2x_1_weightByDrive_model_name,
  xgb_R_driveEP_s_1_weightByDrive_model_name
)
model_names_list

###########################################
### train XGBoost model on full dataset ###
###########################################

# for (j in 5:5) {
{
  xgb_model_name <- model_names_list[[j]]
  xgb_features = get(paste0(xgb_model_name, "_features"))
  xgb_params = get(paste0(xgb_model_name, "_params"))
  xgb_nrounds = get(paste0(xgb_model_name, "_nrounds"))
  xgb_catalytic = if (exists(paste0(xgb_model_name, "_catalytic"))) { get(paste0(xgb_model_name, "_catalytic")) } else { FALSE }
  xgb_regression = str_detect(xgb_model_name, "xgb_R_")
  xgb_BoundedRegression = str_detect(xgb_model_name, "xgb_BR_")
  xgb_is_epochBasedEP = str_detect(xgb_model_name, "epochEP")
  xgb_is_driveBasedEP = str_detect(xgb_model_name, "driveEP")
  xgb_is_weightedByEpoch = str_detect(model_name, "weightByEpoch")
  xgb_is_weightedByDrive = str_detect(model_name, "weightByDrive")
  xgb_is_randomlyDrawOnePlayPerGroup = str_detect(model_name, "randomlyDrawOnePlayPerGroup")
  xgb_train_set = data_full
  catalytic_model_name = if (xgb_catalytic) get(paste0(xgb_model_name, "_prior_name"))
  xgb_fit = train_xgb(xgb_features, xgb_train_set, xgb_params, xgb_nrounds, watchSet="none", 
                      epoch_based_EP=xgb_is_epochBasedEP, drive_based_EP=xgb_is_driveBasedEP, wp=FALSE,
                      weight_by_epoch=xgb_is_weightedByEpoch, weight_by_drive=xgb_is_weightedByDrive,
                      Regression=xgb_regression, BoundedRegression=xgb_BoundedRegression,
                      catalytic=xgb_catalytic, catalytic_model_name=catalytic_model_name, 
                      )
  xgb.save(xgb_fit, paste0("trainedFullModel_", xgb_model_name, ".xgb"))
}



# predict_ep_xgb(xgb_fit, tibble(yardline_100=1:99), xgb_features, xgb_model_name, Regression=xgb_regression) %>%
#   mutate(i = row_number()) %>%
#   ggplot() +
#   geom_line(aes(x=i,y=pred))


