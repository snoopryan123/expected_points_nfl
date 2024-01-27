
########################
### Helper Functions ###
########################

predict_lm <- function(our_lm, dataset) {
  predict(our_lm, dataset)
}

#####################################
### OLS Models fit from All Downs ###
#####################################

fit_lm_d1 <- function(dataset, weight_me=FALSE) {
  if (!weight_me) { dataset$w = 1 }
  fit = lm(pts_next_score ~  
             bs(yardline_100, df=5):factor(down),
           weights = w, data = dataset)
  clean_lm(fit)
}

fit_lm_d2 <- function(dataset, weight_me=FALSE) {
  if (!weight_me) { dataset$w = 1 }
  fit = lm(pts_next_score ~  
             bs(yardline_100, df=5):factor(down) +
             log(ydstogo),
           weights = w, data = dataset)
  clean_lm(fit)
}

fit_lm_d3 <- function(dataset, weight_me=FALSE) {
  if (!weight_me) { dataset$w = 1 }
  fit = lm(pts_next_score ~  
             factor(down):(
               bs(yardline_100, df=5): +
               log(ydstogo)
             )
           ,weights = w, data = dataset)
  clean_lm(fit)
}

fit_lm_d4 <- function(dataset, weight_me=FALSE) {
  if (!weight_me) { dataset$w = 1 }
  fit = lm(pts_next_score ~  
             factor(down):(
                 bs(half_seconds_remaining, df=3, knots=c(30,120))*bs(yardline_100, df=5) +
                 log(ydstogo)
             )
           ,weights = w, data = dataset)
  clean_lm(fit)
}

fit_lm_d5 <- function(dataset, weight_me=FALSE) {
  if (!weight_me) { dataset$w = 1 }
  fit = lm(pts_next_score ~  
             as.numeric(down_combined34!=34):factor(down_combined34):(
                 bs(half_seconds_remaining, df=3, knots=c(30,120))*bs(yardline_100, df=5) +
                 log(ydstogo)
             ) + 
             as.numeric(down_combined34==34):(
               bs(half_seconds_remaining, df=3)*bs(yardline_100, df=4) +
               log(ydstogo)
             ) +
             I(down==4)
           ,weights = w, data = dataset)
  clean_lm(fit)
}

fit_lm_d6 <- function(dataset, weight_me=FALSE) {
  if (!weight_me) { dataset$w = 1 }
  fit = lm(pts_next_score ~  
             as.numeric(down!=3&down!=4):factor(down):(
               log(ydstogo) +
               bs(half_seconds_remaining, df=3, knots=c(30,120))*bs(yardline_100, df=5) #+
                 # log(ydstogo)
             ) + 
             as.numeric(down==3):(
               log(ydstogo) +
               bs(half_seconds_remaining, df=2, degree=2)*bs(yardline_100, df=4)
               # bs(half_seconds_remaining, df=3, knots=c(120))*bs(yardline_100, df=3, knots=c(33,67))# +
                 # log(ydstogo)
             ) +
             # log(ydstogo) +
             as.numeric(down==4):(
               log(ydstogo) +
               I(half_seconds_remaining >= 5*60):half_seconds_remaining:bs(yardline_100, df=4)
               # bs(half_seconds_remaining, df=2, degree=1)*bs(yardline_100, df=4)
               # half_seconds_remaining:yardline_100 +
               # bs(, df=3)*bs(yardline_100, df=4) +
               # bs(half_seconds_remaining, df=3)*bs(yardline_100, df=4) +
             ) 
           ,weights = w, data = dataset)
  clean_lm(fit)
}

fit_lm_sd6 <- function(dataset, weight_me=FALSE) {
  if (!weight_me) { dataset$w = 1 }
  fit = lm(pts_next_score ~  
             as.numeric(down_combined34!=34):factor(down_combined34):(
               bs(half_seconds_remaining, df=3, knots=c(30,120))*bs(yardline_100, df=5) +
               I(ydstogo==1) + I(ydstogo!=1):log(ydstogo) 
             ) + 
             as.numeric(down_combined34==34):(
               bs(half_seconds_remaining, df=3)*bs(yardline_100, df=4) +
               I(ydstogo==1) + I(ydstogo!=1):log(ydstogo) 
             ) +
             I(down==4) +
             posteam_spread + posteam_spread:yardline_100
           ,weights = w, data = dataset)
  clean_lm(fit)
}

fit_lm_sd7 <- function(dataset, weight_me=FALSE) {
  if (!weight_me) { dataset$w = 1 }
  fit = lm(pts_next_score ~  
               as.numeric(down_combined34!=34):factor(down_combined34):(
               bs(half_seconds_remaining, df=3, knots=c(30,120))*bs(yardline_100, df=5) +
               factor(era_A):(I(ydstogo==1) + I(ydstogo!=1):log(ydstogo) )
             ) + 
               as.numeric(down_combined34==34):(
               bs(half_seconds_remaining, df=3)*bs(yardline_100, df=4) +
               factor(era_A):(I(ydstogo==1) + I(ydstogo!=1):log(ydstogo) )
             ) +
             I(down==4) + factor(era_A) +
             posteam_spread + posteam_spread:yardline_100
           ,weights = w, data = dataset)
  clean_lm(fit)
}

fit_lm_sd8 <- function(dataset, weight_me=FALSE) {
  if (!weight_me) { dataset$w = 1 }
  fit = lm(pts_next_score ~  
             as.numeric(down_combined34!=34):factor(down_combined34):(
               utm:as.numeric(posteam_timeouts_remaining==0) +
               utm:as.numeric(posteam_timeouts_remaining==1) +
               bs(half_seconds_remaining, df=3, knots=c(30,120))*bs(yardline_100, df=5) +
               factor(era_A):(I(ydstogo==1) + I(ydstogo!=1):log(ydstogo) )
             ) + 
             as.numeric(down_combined34==34):(
               utm:as.numeric(posteam_timeouts_remaining==0) +
               utm:as.numeric(posteam_timeouts_remaining==1) +
               bs(half_seconds_remaining, df=3)*bs(yardline_100, df=4) +
               factor(era_A):(I(ydstogo==1) + I(ydstogo!=1):log(ydstogo) )
             ) +
             I(down==4) + factor(era_A) +
             posteam_spread + posteam_spread:yardline_100
           ,weights = w, data = dataset)
  clean_lm(fit)
}

fit_lm_sd9 <- function(dataset, weight_me=FALSE) {
  if (!weight_me) { dataset$w = 1 }
  fit = lm(pts_next_score ~  
             as.numeric(down_combined34!=34):factor(down_combined34):(
                 I((score_differential <= -11)) + ### need a TD
                 I((score_differential >= 11)) + ### comfortable, field goal is fine
                 ### note:: fourth_quarter == game_seconds_remaining <= 900
                 I((score_differential <= -4)*(game_seconds_remaining <= 900)) + ### need a TD   
                 I((-3 <= score_differential & score_differential <= 0)*(game_seconds_remaining <= 900)) + ### ok with a field goal
                 I((1 <= score_differential & score_differential <= 3)*(game_seconds_remaining <= 900)) + ### prefer a TD but ok with a field goal
                 I((4 <= score_differential & score_differential <= 10)*(game_seconds_remaining <= 900)) + ### ok with a field goal but game is still close
                 utm:as.numeric(posteam_timeouts_remaining==0) +
                 utm:as.numeric(posteam_timeouts_remaining==1) +
                 bs(half_seconds_remaining, df=3, knots=c(30,120))*bs(yardline_100, df=5) +
                 factor(era_A):(I(ydstogo==1) + I(ydstogo!=1):log(ydstogo) )
             ) + 
             as.numeric(down_combined34==34):(
                 I((score_differential <= -11)) + ### need a TD
                 I((score_differential >= 11)) + ### comfortable, field goal is fine
                 ### note:: fourth_quarter == game_seconds_remaining <= 900
                 I((score_differential <= -4)*(game_seconds_remaining <= 900)) + ### need a TD   
                 I((-3 <= score_differential & score_differential <= 0)*(game_seconds_remaining <= 900)) + ### ok with a field goal
                 I((1 <= score_differential & score_differential <= 3)*(game_seconds_remaining <= 900)) + ### prefer a TD but ok with a field goal
                 I((4 <= score_differential & score_differential <= 10)*(game_seconds_remaining <= 900)) + ### ok with a field goal but game is still close
                 utm:as.numeric(posteam_timeouts_remaining==0) +
                 utm:as.numeric(posteam_timeouts_remaining==1) +
                 bs(half_seconds_remaining, df=3)*bs(yardline_100, df=4) +
                 factor(era_A):(I(ydstogo==1) + I(ydstogo!=1):log(ydstogo) )
             ) +
             I(down==4) + factor(era_A) +
             posteam_spread + posteam_spread:yardline_100
           ,weights = w, data = dataset)
  clean_lm(fit)
}

###############################################
### Original Romer Instrumental Vars Method ###
###############################################

create_Romer_XPD <- function(dataset) {
  EE = dataset %>%
    # select(ydl,B,S,P,P2) %>%
    rename(ydl=yardline_100,B=pos_changes_w,S=not_score,P=pts_of_play,P2=pts_next_score) 
  # browser()
  # D matrix
  change_factor_names <- function(s) {
    paste0("ydl_", str_remove(s, "factor\\(ydl\\)"))
  }
  D_ <- EE %>% modelr::model_matrix(~ factor(ydl) + 0)
  # D_ <- EE %>% modelr::model_matrix(~ 0 + factor(ydl) + factor(era_A) + oq_op_1_sum + dq_dt_1_combined_sum)
  names(D_) <- change_factor_names(names(D_))
  D_ <- D_ %>%
    relocate("ydl_2", .after = "ydl_1") %>%
    relocate("ydl_3", .after = "ydl_2") %>%
    relocate("ydl_4", .after = "ydl_3") %>%
    relocate("ydl_5", .after = "ydl_4") %>%
    relocate("ydl_6", .after = "ydl_5") %>%
    relocate("ydl_7", .after = "ydl_6") %>%
    relocate("ydl_8", .after = "ydl_7") %>%
    relocate("ydl_9", .after = "ydl_8")
  
  # matrices
  D_curr = as.matrix(D_)
  D_next = shift.up(D_curr, rows = 1, fill = 0)
  B = EE$B
  S = EE$S
  
  a = B*S #FIXME???
  X_ = D_curr - a*D_next
  P_ = EE$P #
  P2_ = EE$P2
  
  list(X=X_, P=P_, P2 = P2_, D=as.matrix(D_))
}

fit_Romer <- function(dataset) {
  # get matrices
  Romer_Matrices = create_Romer_XPD(dataset)
  X = Romer_Matrices[[1]]
  P = Romer_Matrices[[2]]
  P2 = Romer_Matrices[[3]]
  D = Romer_Matrices[[4]]
  
  # Romer instrumental variables method
  Romer_V = solve(t(D) %*% X) %*% (t(D) %*% P)
  Romer = tibble(yardline_100=1:99, v=as.numeric(Romer_V[1:99]))
  Romer = smooth_me(Romer)
  Romer$model = "Romer"
  
  return(Romer)
}

get_Romer_preds <- function(V_Romer, data_test) {
  V_Romer[data_test$yardline_100,]$v_smoothed
}

# #####################################################################
# ### OLS Models that weight each play by 1/(# plays in that epoch) ###
# 
# fit_lm_weightedByEpoch <- function(dataset, fit_model_func=fit_lm_s2dE) {
#   dataset_w = 
#     dataset %>%
#     group_by(epoch) %>%
#     mutate(w = 1/n()) %>%
#     ungroup()
#   fit = fit_model_func(dataset_w, w=TRUE)
#   clean_lm(fit)
# }
