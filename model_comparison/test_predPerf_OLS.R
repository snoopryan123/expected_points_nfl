
source("A_train_test_main.R")

source("models_OLS.R")

################################################################################

m_Romer = fit_Romer(train_set)
pred_Romer = tibble(pred = get_Romer_preds(m_Romer, test_set), model = "Romer")

################################################################################

# lm_d1 = fit_lm_d1(train_set)
# pred_lm_d1 = tibble(pred = predict_lm(lm_d1, test_set), model = "lm_d1")
# 
# lm_d2 = fit_lm_d2(train_set)
# pred_lm_d2 = tibble(pred = predict_lm(lm_d2, test_set), model = "lm_d2")
# 
# lm_d3 = fit_lm_d3(train_set)
# pred_lm_d3 = tibble(pred = predict_lm(lm_d3, test_set), model = "lm_d3")
# 
# lm_d4 = fit_lm_d4(train_set)
# pred_lm_d4 = tibble(pred = predict_lm(lm_d4, test_set), model = "lm_d4")
# 
# lm_d1_w = fit_lm_d1(train_set, weight_me=T)
# pred_lm_d1_w = tibble(pred = predict_lm(lm_d1_w, test_set), model = "lm_d1_w")
# 
# lm_d2_w = fit_lm_d2(train_set, weight_me=T)
# pred_lm_d2_w = tibble(pred = predict_lm(lm_d2_w, test_set), model = "lm_d2_w")
# 
# lm_d3_w = fit_lm_d3(train_set, weight_me=T)
# pred_lm_d3_w = tibble(pred = predict_lm(lm_d3_w, test_set), model = "lm_d3_w")
# 
# lm_d4_w = fit_lm_d4(train_set, weight_me=T)
# pred_lm_d4_w = tibble(pred = predict_lm(lm_d4_w, test_set), model = "lm_d4_w")

################################################################################

lm_d4_w = fit_lm_d4(train_set, weight_me=T)
pred_lm_d4_w = tibble(pred = predict_lm(lm_d4_w, test_set), model = "lm_d4_w")

lm_s1d_w = fit_lm_s1d(train_set, weight_me=T)
pred_lm_s1d_w = tibble(pred = predict_lm(lm_s1d_w, test_set), model = "lm_s1d_w")

lm_s2d_w = fit_lm_s2d(train_set, weight_me=T)
pred_lm_s2d_w = tibble(pred = predict_lm(lm_s2d_w, test_set), model = "lm_s2d_w")

lm_s2d1_w = fit_lm_s2d1(train_set, weight_me=T)
pred_lm_s2d1_w = tibble(pred = predict_lm(lm_s2d1_w, test_set), model = "lm_s2d1_w")

################################################################################

# lm_s2dE = fit_lm_s2dE(train_set)
# pred_lm_s2dE = tibble(pred = predict_lm(lm_s2dE, test_set), model = "lm_s2dE")

# ###################
# 
# ### test methods that account for autocorrelation
# 
# lm_s2dE = fit_lm_s2dE(train_set)
# # model_name_s2dE = "lm_s2dE"
# model_name_s2dE = "best linear model "
# pred_lm_s2dE = tibble(pred = predict_lm(lm_s2dE, test_set), model = model_name_s2dE)
# 
# lm_s2dE_R = fit_lm_s2dE_R(train_set)
# # model_name_s2dE_R = "lm_s2dE_R"
# model_name_s2dE_R = "best linear model, with random effect for epoch"
# pred_lm_s2dE_R = tibble(pred = predict(lm_s2dE_R, test_set, allow.new.levels=TRUE), model = model_name_s2dE_R)
# 
# # B = 10
# B = 100
# pred_lm_s2dE_SB_0 = predict_lm_sampleBagged(train_set, test_set, fit_model_func=fit_lm_s2dE, B=B, print_every=2) 
# # model_name_s2dE_SB = "lm_s2dE_SB"
# model_name_s2dE_SB = paste0("B=",B," times sub-sample one play per epoch and fit best linear model, then bag the results")
# pred_lm_s2dE_SB = tibble(pred = pred_lm_s2dE_SB_0, model = model_name_s2dE_SB)
# 
# lm_s2dE_w = fit_lm_weightedByEpoch(train_set, fit_model_func=fit_lm_s2dE)   
# # model_name_s2dE = "lm_s2dE_w"
# model_name_s2dE_w = "best linear model, with row-weights 1/(# plays (rows) in that epoch)"
# pred_lm_s2dE_w = tibble(pred = predict_lm(lm_s2dE_w, test_set), model = model_name_s2dE_w)
#
# set.seed(23427380)
# lm_s2dE_noised = fit_lm_s2dE(train_set %>% mutate(pts_next_score = pts_next_score + rnorm(n(), mean=0, sd=0.25)))
# # model_name_s2dE = "lm_s2dE"
# model_name_s2dE_noised = "OLS with y <- y + N(0, 0.25^2)"
# pred_lm_s2dE_noised = tibble(pred = predict_lm(lm_s2dE_noised, test_set), model = model_name_s2dE_noised)

################################################################################

# preds = bind_rows(
#   pred_lm_d1, pred_lm_d2, pred_lm_d3, pred_lm_d4,
#   pred_lm_d1_w, pred_lm_d2_w, pred_lm_d3_w, pred_lm_d4_w,
# )

preds = bind_rows(
  pred_Romer, pred_lm_d4_w, pred_lm_s1d_w, pred_lm_s2d_w, pred_lm_s2d1_w
)

# pred_Romer

results = 
  preds %>% 
  group_by(model) %>%
  summarise(
    w_rmse = RMSE(test_set$pts_next_score, pred, test_set$w)
  ) %>% 
  arrange(w_rmse)
print(data.frame(results))





