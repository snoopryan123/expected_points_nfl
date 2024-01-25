
args <- commandArgs(trailingOnly = TRUE)
j <- as.numeric(args[1])
WP = as.logical(args[2])

# WP = TRUE
# WP = FALSE

###################
filewd = getwd()
setwd("../..")
source("00_main.R") ### get training and validation set
setwd(filewd)
source("models.R")
###################

#FIXME
model_names_list <- list(
  xgb_C_s_1_model_name,
  xgb_R_s_1_model_name
)

# model_names_list <- list(
#   xgb_C_110_s_1_model_name,
#   xgb_BR_110_s_1_model_name,
#   xgb_C_110_s_1_catalytic_model_name,
#   ###########################
#   xgb_wp_110_3_model_name,
#   xgb_wp_110_6_catalytic_model_name,
#   xgb_wp_Baldwin_model_name,
#   ###########################
#   xgb_C_s_1_catalytic_model_name
# )

# j = 7

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
  xgb_wp = str_detect(xgb_model_name, "wp")
  xgb_train_set = 
    if (xgb_wp & str_detect(xgb_model_name, "110")) {
      data_full_110_WP
    } else if (xgb_wp) {
      data_full_WP
    } else if (!xgb_wp & str_detect(xgb_model_name, "110")) {
      data_full_110_EP
    } else {
      data_full_EP
    }
  catalytic_model_name = if (xgb_catalytic) get(paste0(xgb_model_name, "_prior_name"))
  xgb_fit = train_xgb(xgb_features, xgb_train_set, xgb_params, xgb_nrounds, watchSet="none", 
                      catalytic=xgb_catalytic, Regression=xgb_regression, BoundedRegression=xgb_BoundedRegression,
                      catalytic_model_name=catalytic_model_name, wp=xgb_wp)
  xgb.save(xgb_fit, paste0(xgb_model_name, ".xgb"))
}



# predict_ep_xgb(xgb_fit, tibble(yardline_100=1:99), xgb_features, xgb_model_name, Regression=xgb_regression) %>%
#   mutate(i = row_number()) %>%
#   ggplot() +
#   geom_line(aes(x=i,y=pred))


