
source("A_train_test_main.R")

source("models_MLR.R")

######################
### Fit the models ###
######################

print(""); print("mlr_yurko_paper"); print("");
mlr_yurko_paper = fit_mlr_yurko_paper(train_set)
pred_ep_mlr_yurko_paper = predict_mlr_ep(mlr_yurko_paper, test_set, "mlr_yurko_paper")

# print(""); print("mlr_yurko_1_w"); print("");
# mlr_yurko_1_w = fit_mlr_yurko_1(train_set, weight_me=T)
# pred_ep_mlr_yurko_1_w = predict_mlr_ep(mlr_yurko_1_w, test_set, "mlr_yurko_1_w")
# 
# print(""); print("mlr_yurko_2_w"); print("");
# mlr_yurko_2_w = fit_mlr_yurko_2(train_set, weight_me=T)
# pred_ep_mlr_yurko_2_w = predict_mlr_ep(mlr_yurko_2_w, test_set, "mlr_yurko_2_w")
# 
# print(""); print("mlr_yurko_3_w"); print("");
# mlr_yurko_3_w = fit_mlr_yurko_3(train_set, weight_me=T)
# pred_ep_mlr_yurko_3_w = predict_mlr_ep(mlr_yurko_3_w, test_set, "mlr_yurko_3_w")
# 
# print(""); print("mlr_yurko_4_w"); print("");
# mlr_yurko_4_w = fit_mlr_yurko_4(train_set, weight_me=T)
# pred_ep_mlr_yurko_4_w = predict_mlr_ep(mlr_yurko_4_w, test_set, "mlr_yurko_4_w")
# 
# print(""); print("mlr_yurko_5_w"); print("");
# mlr_yurko_5_w = fit_mlr_yurko_5(train_set, weight_me=T)
# pred_ep_mlr_yurko_5_w = predict_mlr_ep(mlr_yurko_5_w, test_set, "mlr_yurko_5_w")

print(""); print("mlr_yurko_1x_w"); print("");
mlr_yurko_1x_w = fit_mlr_yurko_1x(train_set, weight_me=T)
pred_ep_mlr_yurko_1x_w = predict_mlr_ep(mlr_yurko_1x_w, test_set, "mlr_yurko_1x_w")

# print(""); print("mlr_yurko_sx1_w"); print("");
# mlr_yurko_sx1_w = fit_mlr_yurko_sx1(train_set, weight_me=T)
# pred_ep_mlr_yurko_sx1_w = predict_mlr_ep(mlr_yurko_sx1_w, test_set, "mlr_yurko_sx1_w")
# 
# print(""); print("mlr_yurko_sx2_w"); print("");
# mlr_yurko_sx2_w = fit_mlr_yurko_sx2(train_set, weight_me=T)
# pred_ep_mlr_yurko_sx2_w = predict_mlr_ep(mlr_yurko_sx2_w, test_set, "mlr_yurko_sx2_w")
# 
# print(""); print("mlr_yurko_sx3_w"); print("");
# mlr_yurko_sx3_w = fit_mlr_yurko_sx3(train_set, weight_me=T)
# pred_ep_mlr_yurko_sx3_w = predict_mlr_ep(mlr_yurko_sx3_w, test_set, "mlr_yurko_sx3_w")

print(""); print("mlr_yurko_sx4_w"); print("");
mlr_yurko_sx4_w = fit_mlr_yurko_sx4(train_set, weight_me=T)
pred_ep_mlr_yurko_sx4_w = predict_mlr_ep(mlr_yurko_sx4_w, test_set, "mlr_yurko_sx4_w")

print(""); print("mlr_yurko_oq4xdq4x_1_w"); print("");
mlr_yurko_oq4xdq4x_1_w = fit_mlr_yurko_oq4xdq4x_1(train_set, weight_me=T)
pred_ep_mlr_yurko_oq4xdq4x_1_w = predict_mlr_ep(mlr_yurko_oq4xdq4x_1_w, test_set, "mlr_yurko_oq4xdq4x_1_w")

###################
### test losses ###
###################

# preds = bind_rows(
#   pred_ep_mlr_yurko_paper,
#   pred_ep_mlr_yurko_1_w, pred_ep_mlr_yurko_2_w, pred_ep_mlr_yurko_3_w, 
#   pred_ep_mlr_yurko_4_w, pred_ep_mlr_yurko_5_w, pred_ep_mlr_yurko_1x_w,
#   pred_ep_mlr_yurko_sx1_w, pred_ep_mlr_yurko_sx2_w, pred_ep_mlr_yurko_sx3_w, pred_ep_mlr_yurko_sx4_w,
#   pred_ep_mlr_yurko_oq4xdq4x_1_w,
# )

preds = bind_rows(
  pred_ep_mlr_yurko_paper, pred_ep_mlr_yurko_1x_w, pred_ep_mlr_yurko_sx4_w, pred_ep_mlr_yurko_oq4xdq4x_1_w,
)

get_loss <- function(preds, test_set) {
  preds %>% 
    group_by(model) %>%
    summarise(
      w_rmse = RMSE(test_set$pts_next_score, pred, test_set$w)
    ) %>% 
    arrange(w_rmse)
}

results = get_loss(preds, test_set)
print(data.frame(results))

idxs_to_keep = (test_set %>% mutate(ii = 1:n()) %>% filter(down!=4 & half_seconds_remaining > 120))$ii
test_set_truncated = test_set[idxs_to_keep,]
preds_truncated = preds %>% group_by(model) %>% filter(row_number() %in% idxs_to_keep)

results_truncated = get_loss(preds_truncated, test_set_truncated)

print(data.frame(results))
print(data.frame(results_truncated))

# write_csv(results, "test_results_predPerf_MLR.csv")
# write_csv(results_truncated, "test_results_tuncated_predPerf_MLR.csv")



