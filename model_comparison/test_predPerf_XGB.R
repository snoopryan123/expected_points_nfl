
source("A_train_test_main.R")

source("models_XGB.R")

# print(""); print(xgb_C_nflFastR_model_name); print("");
# xgb_C_nflFastR = train_xgb(xgb_C_nflFastR_features, train_set, xgb_C_nflFastR_params, xgb_C_nflFastR_nrounds, watchSet=test_set, catalytic=xgb_C_nflFastR_catalytic)
# pred_ep_xgb_C_nflFastR = predict_ep_xgb(xgb_C_nflFastR, test_set, xgb_C_nflFastR_features, xgb_C_nflFastR_model_name)

print(""); print(xgb_C_nflFastR_1_model_name); print("");
xgb_C_nflFastR_1 = train_xgb(xgb_C_nflFastR_1_features, train_set, xgb_C_nflFastR_1_params, xgb_C_nflFastR_1_nrounds, watchSet=test_set, catalytic=xgb_C_nflFastR_1_catalytic)
pred_ep_xgb_C_nflFastR_1 = predict_ep_xgb(xgb_C_nflFastR_1, test_set, xgb_C_nflFastR_1_features, xgb_C_nflFastR_1_model_name)

print(""); print(xgb_C_s_1_model_name); print("");
xgb_C_s_1 = train_xgb(xgb_C_s_1_features, train_set, xgb_C_s_1_params, xgb_C_s_1_nrounds, watchSet=test_set, catalytic=xgb_C_s_1_catalytic)
pred_ep_xgb_C_s_1 = predict_ep_xgb(xgb_C_s_1, test_set, xgb_C_s_1_features, xgb_C_s_1_model_name)

print(""); print(xgb_C_s_1_wbe_model_name); print("");
xgb_C_s_1_wbe = train_xgb(xgb_C_s_1_wbe_features, train_set, xgb_C_s_1_wbe_params, xgb_C_s_1_wbe_nrounds, 
                          watchSet=test_set, catalytic=xgb_C_s_1_wbe_catalytic, weight_by_epoch = TRUE)
pred_ep_xgb_C_s_1_wbe = predict_ep_xgb(xgb_C_s_1_wbe, test_set, xgb_C_s_1_wbe_features, xgb_C_s_1_wbe_model_name)

print(""); print(xgb_C_oq2xdq2x_1_model_name); print("");
xgb_C_oq2xdq2x_1 = train_xgb(xgb_C_oq2xdq2x_1_features, train_set, xgb_C_oq2xdq2x_1_params, xgb_C_oq2xdq2x_1_nrounds, watchSet=test_set, catalytic=xgb_C_oq2xdq2x_1_catalytic)
pred_ep_xgb_C_oq2xdq2x_1 = predict_ep_xgb(xgb_C_oq2xdq2x_1, test_set, xgb_C_oq2xdq2x_1_features, xgb_C_oq2xdq2x_1_model_name)

print(""); print(xgb_C_oq2xdq2x_1_wbe_model_name); print("");
xgb_C_oq2xdq2x_1_wbe = train_xgb(xgb_C_oq2xdq2x_1_wbe_features, train_set, xgb_C_oq2xdq2x_1_wbe_params, xgb_C_oq2xdq2x_1_wbe_nrounds, 
                                 watchSet=test_set, catalytic=xgb_C_oq2xdq2x_1_wbe_catalytic, weight_by_epoch = TRUE)
pred_ep_xgb_C_oq2xdq2x_1_wbe = predict_ep_xgb(xgb_C_oq2xdq2x_1_wbe, test_set, xgb_C_oq2xdq2x_1_wbe_features, xgb_C_oq2xdq2x_1_wbe_model_name)

##########################################

preds_C = bind_rows(
  # pred_ep_xgb_C_nflFastR,
  pred_ep_xgb_C_nflFastR_1,
  pred_ep_xgb_C_s_1,
  pred_ep_xgb_C_s_1_wbe,
  pred_ep_xgb_C_oq2xdq2x_1,
  pred_ep_xgb_C_oq2xdq2x_1_wbe
)

results_C = 
  preds_C %>% 
  group_by(model) %>%
  summarise(
    w_rmse = RMSE(test_set$pts_next_score, pred, test_set$w)
  ) %>% 
  arrange(w_rmse)
print(data.frame(results_C))

