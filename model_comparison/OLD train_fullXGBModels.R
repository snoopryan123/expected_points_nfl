
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

model_names_list <- list(
  xgb_C_s_1_model_name,
  xgb_C_s_1_wbe_model_name,
  xgb_C_oq2xdq2x_1_wbe_model_name
)

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
  xgb_is_weightedByEpoch = str_detect(xgb_model_name, "_wbe")
  xgb_is_weightedByGame = str_detect(xgb_model_name, "_wbg")
  xgb_wp = str_detect(xgb_model_name, "wp")
  xgb_train_set = data_full
  catalytic_model_name = if (xgb_catalytic) get(paste0(xgb_model_name, "_prior_name"))
  xgb_fit = train_xgb(xgb_features, xgb_train_set, xgb_params, xgb_nrounds, watchSet="none", 
                      catalytic=xgb_catalytic, Regression=xgb_regression, BoundedRegression=xgb_BoundedRegression,
                      catalytic_model_name=catalytic_model_name, wp=xgb_wp,
                      weight_by_epoch=xgb_is_weightedByEpoch, weight_by_game=xgb_is_weightedByGame)
  xgb.save(xgb_fit, paste0(xgb_model_name, ".xgb"))
}



# predict_ep_xgb(xgb_fit, tibble(yardline_100=1:99), xgb_features, xgb_model_name, Regression=xgb_regression) %>%
#   mutate(i = row_number()) %>%
#   ggplot() +
#   geom_line(aes(x=i,y=pred))


