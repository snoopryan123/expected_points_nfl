
source("A_train_test_main.R")

source("models_OLS.R")

################################################################################

m_Romer = fit_Romer(train_set)
pred_Romer = tibble(pred = get_Romer_preds(m_Romer, test_set), model = "Romer")

# # lm_d1 = fit_lm_d1(train_set)
# # pred_lm_d1 = tibble(pred = predict_lm(lm_d1, test_set), model = "lm_d1")
# # 
# # lm_d2 = fit_lm_d2(train_set)
# # pred_lm_d2 = tibble(pred = predict_lm(lm_d2, test_set), model = "lm_d2")
# # 
# # lm_d3 = fit_lm_d3(train_set)
# # pred_lm_d3 = tibble(pred = predict_lm(lm_d3, test_set), model = "lm_d3")
# # 
# # lm_d4 = fit_lm_d4(train_set)
# # pred_lm_d4 = tibble(pred = predict_lm(lm_d4, test_set), model = "lm_d4")

lm_d1_w = fit_lm_d1(train_set, weight_me=T)
pred_lm_d1_w = tibble(pred = predict_lm(lm_d1_w, test_set), model = "lm_d1_w")

lm_d2_w = fit_lm_d2(train_set, weight_me=T)
pred_lm_d2_w = tibble(pred = predict_lm(lm_d2_w, test_set), model = "lm_d2_w")

lm_d3_w = fit_lm_d3(train_set, weight_me=T)
pred_lm_d3_w = tibble(pred = predict_lm(lm_d3_w, test_set), model = "lm_d3_w")

lm_d4_w = fit_lm_d4(train_set, weight_me=T)
pred_lm_d4_w = tibble(pred = predict_lm(lm_d4_w, test_set), model = "lm_d4_w")

lm_d5_w = fit_lm_d5(train_set, weight_me=T)
pred_lm_d5_w = tibble(pred = predict_lm(lm_d5_w, test_set), model = "lm_d5_w")

lm_d6_w = fit_lm_d6(train_set, weight_me=T)
pred_lm_d6_w = tibble(pred = predict_lm(lm_d6_w, test_set), model = "lm_d6_w")

lm_d7_w = fit_lm_d7(train_set, weight_me=T)
pred_lm_d7_w = tibble(pred = predict_lm(lm_d7_w, test_set), model = "lm_d7_w")

lm_sd6_w = fit_lm_sd6(train_set, weight_me=T)
pred_lm_sd6_w = tibble(pred = predict_lm(lm_sd6_w, test_set), model = "lm_sd6_w")

lm_sd7_w = fit_lm_sd7(train_set, weight_me=T)
pred_lm_sd7_w = tibble(pred = predict_lm(lm_sd7_w, test_set), model = "lm_sd7_w")

lm_sd8_w = fit_lm_sd8(train_set, weight_me=T)
pred_lm_sd8_w = tibble(pred = predict_lm(lm_sd8_w, test_set), model = "lm_sd8_w")

lm_sd9_w = fit_lm_sd9(train_set, weight_me=T)
pred_lm_sd9_w = tibble(pred = predict_lm(lm_sd9_w, test_set), model = "lm_sd9_w")

lm_sd9 = fit_lm_sd9(train_set)
pred_lm_sd9 = tibble(pred = predict_lm(lm_sd9, test_set), model = "lm_sd9")

# lm_s2dE_w = fit_lm_weightedByEpoch(train_set, fit_model_func=fit_lm_s2dE)   
# # model_name_s2dE = "lm_s2dE_w"
# model_name_s2dE_w = "best linear model, with row-weights 1/(# plays (rows) in that epoch)"
# pred_lm_s2dE_w = tibble(pred = predict_lm(lm_s2dE_w, test_set), model = model_name_s2dE_w)

################################################################################

preds = bind_rows(
  # pred_lm_d1, pred_lm_d2, pred_lm_d3, pred_lm_d4,
  pred_lm_d1_w, pred_lm_d2_w, pred_lm_d3_w, pred_lm_d4_w,
  pred_lm_d5_w, pred_lm_d6_w, pred_lm_d7_w,
  pred_lm_sd6_w, pred_lm_sd7_w, pred_lm_sd8_w, pred_lm_sd9_w
)

# preds = bind_rows(
#   pred_Romer, pred_lm_sd9, pred_lm_sd9_w
# )

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

# write_csv(results, "test_results_predPerf_OLS.csv")
# write_csv(results_truncated, "test_results_tuncated_predPerf_OLS.csv")

