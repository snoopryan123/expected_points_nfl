
# SEED = 99 #FIXME
# PRE_LOADED_TrainTestSplitAndTeamQualities = FALSE
# source("A_train_test_main.R")

PRE_LOADED_TrainTestSplitAndTeamQualities = TRUE
source("A_train_test_main.R")

source("models_XGB.R")

################################################################################

print(""); print(xgb_C_nflFastR_1_model_name); print("");
xgb_C_nflFastR_1 = train_xgb(xgb_C_nflFastR_1_features, train_set, xgb_C_nflFastR_1_params, xgb_C_nflFastR_1_nrounds, watchSet=test_set, catalytic=xgb_C_nflFastR_1_catalytic)
pred_ep_xgb_C_nflFastR_1 = predict_ep_xgb(xgb_C_nflFastR_1, test_set, xgb_C_nflFastR_1_features, xgb_C_nflFastR_1_model_name)

# print(""); print(xgb_C_s_1_model_name); print("");
# xgb_C_s_1 = train_xgb(xgb_C_s_1_features, train_set, xgb_C_s_1_params, xgb_C_s_1_nrounds, watchSet=test_set, catalytic=xgb_C_s_1_catalytic)
# pred_ep_xgb_C_s_1 = predict_ep_xgb(xgb_C_s_1, test_set, xgb_C_s_1_features, xgb_C_s_1_model_name)

# print(""); print(xgb_C_oq2xdq2x_1_model_name); print("");
# xgb_C_oq2xdq2x_1 = train_xgb(xgb_C_oq2xdq2x_1_features, train_set, xgb_C_oq2xdq2x_1_params, xgb_C_oq2xdq2x_1_nrounds, watchSet=test_set, catalytic=xgb_C_oq2xdq2x_1_catalytic)
# pred_ep_xgb_C_oq2xdq2x_1 = predict_ep_xgb(xgb_C_oq2xdq2x_1, test_set, xgb_C_oq2xdq2x_1_features, xgb_C_oq2xdq2x_1_model_name)

print(""); print(xgb_C_s_1_wbe_model_name); print("");
xgb_C_s_1_wbe = train_xgb(xgb_C_s_1_wbe_features, train_set, xgb_C_s_1_wbe_params, xgb_C_s_1_wbe_nrounds, 
                          watchSet=test_set, catalytic=xgb_C_s_1_wbe_catalytic, weight_by_epoch = TRUE)
pred_ep_xgb_C_s_1_wbe = predict_ep_xgb(xgb_C_s_1_wbe, test_set, xgb_C_s_1_wbe_features, xgb_C_s_1_wbe_model_name)

print(""); print(xgb_C_oq2xdq2x_1_wbe_model_name); print("");
xgb_C_oq2xdq2x_1_wbe = train_xgb(xgb_C_oq2xdq2x_1_wbe_features, train_set, xgb_C_oq2xdq2x_1_wbe_params, xgb_C_oq2xdq2x_1_wbe_nrounds, 
                                 watchSet=test_set, catalytic=xgb_C_oq2xdq2x_1_wbe_catalytic, weight_by_epoch = TRUE)
pred_ep_xgb_C_oq2xdq2x_1_wbe = predict_ep_xgb(xgb_C_oq2xdq2x_1_wbe, test_set, xgb_C_oq2xdq2x_1_wbe_features, xgb_C_oq2xdq2x_1_wbe_model_name)

################################################################################

preds_XGB = bind_rows(
  pred_ep_xgb_C_nflFastR_1,
  # pred_ep_xgb_C_s_1,
  # pred_ep_xgb_C_oq2xdq2x_1,
  pred_ep_xgb_C_s_1_wbe,
  pred_ep_xgb_C_oq2xdq2x_1_wbe
)

get_loss <- function(preds_XGB, test_set) {
  preds_XGB %>% 
    group_by(model) %>%
    summarise(
      w_rmse = RMSE(test_set$pts_next_score, pred, test_set$w)
    ) %>% 
    arrange(w_rmse)
}

results_XGB = get_loss(preds_XGB, test_set)
# print(data.frame(results_XGB))

idxs_to_keep = (test_set %>% mutate(ii = 1:n()) %>% filter(down!=4 & half_seconds_remaining > 3*60))$ii
test_set_truncated = test_set[idxs_to_keep,]
preds_XGB_truncated = preds_XGB %>% group_by(model) %>% filter(row_number() %in% idxs_to_keep)

results_XGB_truncated = get_loss(preds_XGB_truncated, test_set_truncated)

print(data.frame(results_XGB))
print(data.frame(results_XGB_truncated))

write_csv(results_XGB, "test_results_predPerf_XGB.csv")
write_csv(results_XGB_truncated, "test_results_tuncated_predPerf_XGB.csv")

