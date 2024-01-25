
source("A_train_test_main.R")

source("models_MLR.R")

#################################
### Fit the unweighted models ###
#################################

print(""); print("mlr_yurko_paper"); print("");
mlr_yurko_paper = fit_mlr_yurko_paper(train_set)
pred_ep_mlr_yurko_paper = predict_mlr_ep(mlr_yurko_paper, test_set, "mlr_yurko_paper")

################################################################################

# print(""); print("mlr_yurko_1"); print("");
# mlr_yurko_1 = fit_mlr_yurko_1(train_set)
# pred_ep_mlr_yurko_1 = predict_mlr_ep(mlr_yurko_1, test_set, "mlr_yurko_1")
# 
# print(""); print("mlr_yurko_1d"); print("");
# mlr_yurko_1d = fit_mlr_yurko_1d(train_set)
# pred_ep_mlr_yurko_1d = predict_mlr_ep(mlr_yurko_1d, test_set, "mlr_yurko_1d")
# 
# print(""); print("mlr_yurko_s1"); print("");
# mlr_yurko_s1 = fit_mlr_yurko_s1(train_set)
# pred_ep_mlr_yurko_s1 = predict_mlr_ep(mlr_yurko_s1, test_set, "mlr_yurko_s1")

# print(""); print("mlr_yurko_s1d"); print("");
# mlr_yurko_s1d = fit_mlr_yurko_s1d(train_set)
# pred_ep_mlr_yurko_s1d = predict_mlr_ep(mlr_yurko_s1d, test_set, "mlr_yurko_s1d")

# print(""); print("mlr_yurko_s2d"); print("");
# mlr_yurko_s2d = fit_mlr_yurko_s2d(train_set)
# pred_ep_mlr_yurko_s2d = predict_mlr_ep(mlr_yurko_s2d, test_set, "mlr_yurko_s2d")
#
# print(""); print("mlr_yurko_s2dE"); print("");
# mlr_yurko_s2dE = fit_mlr_yurko_s2dE(train_set)
# pred_ep_mlr_yurko_s2dE = predict_mlr_ep(mlr_yurko_s2dE, test_set, "mlr_yurko_s2dE")

# print(""); print("mlr_yurko_oq4xdq4x_1d"); print("");
# mlr_yurko_oq4xdq4x_1d = fit_mlr_yurko_oq4xdq4x_1d(train_set)
# pred_ep_mlr_yurko_oq4xdq4x_1d = predict_mlr_ep(mlr_yurko_oq4xdq4x_1d, test_set, "mlr_yurko_oq4xdq4x_1d")

# print(""); print("mlr_yurko_oq4xdq4x_2d"); print("");
# mlr_yurko_oq4xdq4x_2d = fit_mlr_yurko_oq4xdq4x_2d(train_set)
# pred_ep_mlr_yurko_oq4xdq4x_2d = predict_mlr_ep(mlr_yurko_oq4xdq4x_2d, test_set, "mlr_yurko_oq4xdq4x_2d")

# ### overfits in the plots...

###############################
### Fit the weighted models ###
###############################

print(""); print("mlr_yurko_s1d_w"); print("");
mlr_yurko_s1d_w = fit_mlr_yurko_s1d(train_set, weight_me=T)
pred_ep_mlr_yurko_s1d_w = predict_mlr_ep(mlr_yurko_s1d_w, test_set, "mlr_yurko_s1d_w")

print(""); print("mlr_yurko_oq4xdq4x_1d_w"); print("");
mlr_yurko_oq4xdq4x_1d_w = fit_mlr_yurko_oq4xdq4x_1d(train_set, weight_me=T)
pred_ep_mlr_yurko_oq4xdq4x_1d_w = predict_mlr_ep(mlr_yurko_oq4xdq4x_1d_w, test_set, "mlr_yurko_oq4xdq4x_1d_w")

print(""); print("mlr_yurko_s1d1_w"); print("");
mlr_yurko_s1d1_w = fit_mlr_yurko_s1d1(train_set, weight_me=T)
pred_ep_mlr_yurko_s1d1_w = predict_mlr_ep(mlr_yurko_s1d1_w, test_set, "mlr_yurko_s1d1_w")

# print(""); print("mlr_yurko_s1d2_w"); print("");
# mlr_yurko_s1d2_w = fit_mlr_yurko_s1d2(train_set, weight_me=T)
# pred_ep_mlr_yurko_s1d2_w = predict_mlr_ep(mlr_yurko_s1d2_w, test_set, "mlr_yurko_s1d2_w")

###################
### test losses ###
###################

preds = bind_rows(
  # pred_ep_mlr_yurko_paper,
  # pred_ep_mlr_yurko_1, pred_ep_mlr_yurko_1d, pred_ep_mlr_yurko_s1,
  # pred_ep_mlr_yurko_s1d, #pred_ep_mlr_yurko_s2d, #pred_ep_mlr_yurko_s2dE,
  # # pred_ep_mlr_yurko_oq4xdq4x_1d, #pred_ep_mlr_yurko_oq4xdq4x_2d
  #############################################################
  pred_ep_mlr_yurko_paper,
  # pred_ep_mlr_yurko_oq4xdq4x_1d,
  # pred_ep_mlr_yurko_s1d,
  pred_ep_mlr_yurko_s1d_w,
  pred_ep_mlr_yurko_s1d1_w,
  pred_ep_mlr_yurko_oq4xdq4x_1d_w
)

results = 
  preds %>% 
  group_by(model) %>%
  summarise(
    w_rmse = RMSE(test_set$pts_next_score, pred, test_set$w)
  ) %>% 
  arrange(w_rmse)
print(data.frame(results))



