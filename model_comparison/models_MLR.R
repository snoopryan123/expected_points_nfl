
########################
### Helper Functions ###
########################

get_mlr_probs_for_predict <- function(probs, model) {
  n = nrow(probs)
  df = tibble()
  df = bind_rows(df, tibble(p = probs[,1], i = 1:n, label = 0, outcome = "Touchdown"))
  df = bind_rows(df, tibble(p = probs[,2], i = 1:n, label = 1, outcome = "Opp_Touchdown"))
  df = bind_rows(df, tibble(p = probs[,3], i = 1:n, label = 2, outcome = "Field_Goal"))
  df = bind_rows(df, tibble(p = probs[,4], i = 1:n, label = 3, outcome = "Opp_Field_Goal"))
  df = bind_rows(df, tibble(p = probs[,5], i = 1:n, label = 4, outcome = "Safety"))
  df = bind_rows(df, tibble(p = probs[,6], i = 1:n, label = 5, outcome = "Opp_Safety"))
  df = bind_rows(df, tibble(p = probs[,7], i = 1:n, label = 6, outcome = "No_Score"))
  df$model = model
  df
}

get_mlr_probs <- function(our_mlr, dataset) {
  predict(our_mlr, dataset, "probs")
}

predict_mlr_ep <- function(our_mlr, dataset, model) {
  pred = get_mlr_probs(our_mlr, dataset)
  if (is.null(nrow(pred))) { pred = matrix(pred, nrow=1) }
  colnames(pred) = c("Touchdown","Opp_Touchdown","Field_Goal","Opp_Field_Goal","Safety","Opp_Safety","No_Score")
  ep = pred[,1]*(7) + pred[,2]*(-7) + pred[,3]*(3) + pred[,4]*(-3) + pred[,5]*(2) + pred[,6]*(-2) + pred[,7]*(0)
  return(tibble(pred = unname(ep), model=model))
}

generate_mlr_outcomes <- function(probs) {
  generated_outcomes = sapply(1:nrow(probs), FUN = function (i) { which( unname(rmultinom(1, 1, probs[i,])[,1]) == 1) - 1 } )
  generated_outcomes
}

##################################################################################################
### Multinomial Logistic Regression Models fit on All Downs that don't adjust for team quality ###
##################################################################################################

fit_mlr_yurko_paper <- function(dataset, weight_me=FALSE) {
  if (!weight_me) { dataset$w = 1 }
  fit = multinom(label ~ 
                   yardline_100 +
                   factor(down) +
                   log(ydstogo) +
                   half_seconds_remaining +
                   gtg + 
                   utm, 
                 weights = w,
                 data = dataset)
  clean_lm(fit)
}

fit_mlr_yurko_0a <- function(dataset, weight_me=FALSE) {
  if (!weight_me) { dataset$w = 1 }
  fit = multinom(label ~ 
                   bs(yardline_100, df=3) +
                   factor(down) +
                   log(ydstogo) +
                   half_seconds_remaining +
                   gtg + 
                   utm, 
                 weights = w,
                 data = dataset)
  clean_lm(fit)
}

fit_mlr_yurko_0b <- function(dataset, weight_me=FALSE) {
  if (!weight_me) { dataset$w = 1 }
  fit = multinom(label ~ 
                   yardline_100 +
                   factor(down) + down:yardline_100 +
                   log(ydstogo) +
                   half_seconds_remaining +
                   gtg + 
                   utm, 
                 weights = w,
                 data = dataset)
  clean_lm(fit)
}

fit_mlr_yurko_0c <- function(dataset, weight_me=FALSE) {
  if (!weight_me) { dataset$w = 1 }
  fit = multinom(label ~ 
                   yardline_100 +
                   factor(down) + 
                   bs(log(ydstogo), df=3) +
                   half_seconds_remaining +
                   gtg + 
                   utm, 
                 weights = w,
                 data = dataset)
  clean_lm(fit)
}

fit_mlr_yurko_0d <- function(dataset, weight_me=FALSE) {
  if (!weight_me) { dataset$w = 1 }
  fit = multinom(label ~ 
                   yardline_100 +
                   factor(down) + 
                   log(ydstogo) +
                   bs(half_seconds_remaining, knots=c(30)) +
                   gtg + 
                   utm, 
                 weights = w,
                 data = dataset)
  clean_lm(fit)
}

fit_mlr_yurko_0e <- function(dataset, weight_me=FALSE) {
  if (!weight_me) { dataset$w = 1 }
  fit = multinom(label ~ 
                   yardline_100 +
                   factor(down) + 
                   log(ydstogo) +
                   bs(half_seconds_remaining, knots=c(120)) +
                   gtg + 
                   utm, 
                 weights = w,
                 data = dataset)
  clean_lm(fit)
}

fit_mlr_yurko_0f <- function(dataset, weight_me=FALSE) {
  if (!weight_me) { dataset$w = 1 }
  fit = multinom(label ~ 
                   yardline_100 +
                   factor(down) + 
                   log(ydstogo) +
                   half_seconds_remaining +
                   gtg + 
                   utm:as.numeric(posteam_timeouts_remaining==0),
                 weights = w,
                 data = dataset)
  clean_lm(fit)
}

fit_mlr_yurko_0g <- function(dataset, weight_me=FALSE) {
  if (!weight_me) { dataset$w = 1 }
  fit = multinom(label ~ 
                   yardline_100 +
                   factor(down) + 
                   log(ydstogo) +
                   half_seconds_remaining +
                   utm,
                 weights = w,
                 data = dataset)
  clean_lm(fit)
}

fit_mlr_yurko_1 <- function(dataset, weight_me=FALSE) {
  if (!weight_me) { dataset$w = 1 }
  fit = multinom(label ~ 
                   yardline_100 +
                   # gtg + ### even though better loss, the plots show a weird fit (overfitting...)
                   factor(down) + down:yardline_100 +
                   # bs(log(ydstogo), df=3) + ### even though better loss, the plots show a weird fit (overfitting...) when use `gtg`
                   log(ydstogo) +
                   bs(half_seconds_remaining, knots=c(30)) +
                   utm:as.numeric(posteam_timeouts_remaining==0) +
                   factor(era_A),
                 weights = w,
                 data = dataset)
  clean_lm(fit)
}

fit_mlr_yurko_2 <- function(dataset, weight_me=FALSE) {
  if (!weight_me) { dataset$w = 1 }
  fit = multinom(label ~ 
                   yardline_100 +
                   factor(down) + down:yardline_100 +
                   bs(log(ydstogo), df=3) + 
                   bs(half_seconds_remaining, knots=c(30)) +
                   utm:as.numeric(posteam_timeouts_remaining==0) +
                   factor(era_A),
                 weights = w,
                 data = dataset)
  clean_lm(fit)
}

fit_mlr_yurko_1d <- function(dataset, weight_me=FALSE) {
  if (!weight_me) { dataset$w = 1 }
  fit = multinom(label ~ 
                   yardline_100 +
                   factor(down) + down:yardline_100 +
                   log(ydstogo) +
                   bs(half_seconds_remaining, knots=c(30)) +
                   utm:as.numeric(posteam_timeouts_remaining==0) +
                   I((score_differential <= -11)) + ### need a TD
                   I((score_differential >= 11)) + ### comfortable, field goal is fine
                   ### note:: fourth_quarter == game_seconds_remaining <= 900
                   I((score_differential <= -4)*(game_seconds_remaining <= 900)) + ### need a TD   
                   I((-3 <= score_differential & score_differential <= 0)*(game_seconds_remaining <= 900)) + ### ok with a field goal
                   I((1 <= score_differential & score_differential <= 3)*(game_seconds_remaining <= 900)) + ### prefer a TD but ok with a field goal
                   I((4 <= score_differential & score_differential <= 10)*(game_seconds_remaining <= 900)) + ### ok with a field goal but game is still close
                   factor(era_A),
                 weights = w,
                 data = dataset)
  clean_lm(fit)
}

################################################################################
#### Adjust for Team Quality:
#### FIRST, determine which team quality metrics. THEN, fix time remaining. ####
################################################################################

fit_mlr_yurko_oq4xdq4x_1 <- function(dataset, weight_me=FALSE) {
  if (!weight_me) { dataset$w = 1 }
  fit = multinom(label ~ 
                   yardline_100 +
                   factor(down) + down:yardline_100 +
                   log(ydstogo) +
                   # bs(half_seconds_remaining, knots=c(30)) +
                   bs(half_seconds_remaining, degree=1, df=1) +
                   utm:as.numeric(posteam_timeouts_remaining==0) +
                   factor(era_A) +
                   qbq_ot_0_sum + 
                   oq_rot_0_total_sum +
                   dq_dt_0_againstPass_sum +
                   dq_dt_0_againstRun_sum +
                   qbq_dt_0_sum + 
                   oq_rdt_0_sum +
                   dq_ot_0_againstPass_sum +
                   dq_ot_0_againstRun_sum,
                 weights = w, data = dataset)
  clean_lm(fit)
}

fit_mlr_yurko_oq4xdq4x_2 <- function(dataset, weight_me=FALSE) {
  if (!weight_me) { dataset$w = 1 }
  fit = multinom(label ~ 
                   yardline_100 +
                   factor(down) + down:yardline_100 +
                   log(ydstogo) +
                   # bs(half_seconds_remaining, knots=c(30)) +
                   bs(half_seconds_remaining, degree=1, df=1) +
                   utm:as.numeric(posteam_timeouts_remaining==0) +
                   factor(era_A) +
                   qbq_ot_0_sum + qbq_ot_0_sum:yardline_100 +
                   oq_rot_0_total_sum +  oq_rot_0_total_sum:yardline_100 +
                   dq_dt_0_againstPass_sum +  dq_dt_0_againstPass_sum:yardline_100 +
                   dq_dt_0_againstRun_sum + dq_dt_0_againstRun_sum:yardline_100 +
                   qbq_dt_0_sum +  qbq_dt_0_sum:yardline_100 +
                   oq_rdt_0_sum + oq_rdt_0_sum:yardline_100 +
                   dq_ot_0_againstPass_sum + dq_ot_0_againstPass_sum:yardline_100 +
                   dq_ot_0_againstRun_sum + dq_ot_0_againstRun_sum:yardline_100,
                 weights = w, data = dataset)
  clean_lm(fit)
}

fit_mlr_yurko_s1 <- function(dataset, weight_me=FALSE) {
  if (!weight_me) { dataset$w = 1 }
  fit = multinom(label ~ 
                   yardline_100 +
                   factor(down) + down:yardline_100 +
                   log(ydstogo) +
                   # bs(half_seconds_remaining, knots=c(30)) +
                   bs(half_seconds_remaining, degree=1, df=1) +
                   utm:as.numeric(posteam_timeouts_remaining==0) +
                   factor(era_A) +
                   posteam_spread,
                 weights = w, data = dataset)
  clean_lm(fit)
}


fit_mlr_yurko_s2 <- function(dataset, weight_me=FALSE) {
  if (!weight_me) { dataset$w = 1 }
  fit = multinom(label ~ 
                   yardline_100 +
                   factor(down) + down:yardline_100 +
                   log(ydstogo) +
                   # bs(half_seconds_remaining, knots=c(30)) +
                   bs(half_seconds_remaining, degree=1, df=1) +
                   utm:as.numeric(posteam_timeouts_remaining==0) +
                   factor(era_A) +
                   posteam_spread + posteam_spread:yardline_100,
                 weights = w, data = dataset)
  clean_lm(fit)
}

################################################################################

fit_mlr_yurko_oq4xdq4x_1d <- function(dataset, weight_me=FALSE) {
    if (!weight_me) { dataset$w = 1 }
  fit = multinom(label ~ 
                   yardline_100 +
                   factor(down) + down:yardline_100 +
                   log(ydstogo) +
                   bs(half_seconds_remaining, knots=c(30)) +
                   # bs(half_seconds_remaining, degree=1, df=1) +
                   utm:as.numeric(posteam_timeouts_remaining==0) +
                   I((score_differential <= -11)) + ### need a TD
                   I((score_differential >= 11)) + ### comfortable, field goal is fine
                   ### note:: fourth_quarter == game_seconds_remaining <= 900
                   I((score_differential <= -4)*(game_seconds_remaining <= 900)) + ### need a TD   
                   I((-3 <= score_differential & score_differential <= 0)*(game_seconds_remaining <= 900)) + ### ok with a field goal
                   I((1 <= score_differential & score_differential <= 3)*(game_seconds_remaining <= 900)) + ### prefer a TD but ok with a field goal
                   I((4 <= score_differential & score_differential <= 10)*(game_seconds_remaining <= 900)) + ### ok with a field goal but game is still close
                   factor(era_A) +
                   qbq_ot_0_sum + 
                   oq_rot_0_total_sum +
                   dq_dt_0_againstPass_sum +
                   dq_dt_0_againstRun_sum +
                   qbq_dt_0_sum + 
                   oq_rdt_0_sum +
                   dq_ot_0_againstPass_sum +
                   dq_ot_0_againstRun_sum,
                 weights = w, data = dataset)
  clean_lm(fit)
}

fit_mlr_yurko_oq4xdq4x_2d <- function(dataset, weight_me=FALSE) {
    if (!weight_me) { dataset$w = 1 }
  fit = multinom(label ~ 
                   yardline_100 +
                   factor(down) + down:yardline_100 +
                   log(ydstogo) +
                   bs(half_seconds_remaining, knots=c(30)) +
                   # bs(half_seconds_remaining, degree=1, df=1) +
                   utm:as.numeric(posteam_timeouts_remaining==0) +
                   I((score_differential <= -11)) + ### need a TD
                   I((score_differential >= 11)) + ### comfortable, field goal is fine
                   ### note:: fourth_quarter == game_seconds_remaining <= 900
                   I((score_differential <= -4)*(game_seconds_remaining <= 900)) + ### need a TD   
                   I((-3 <= score_differential & score_differential <= 0)*(game_seconds_remaining <= 900)) + ### ok with a field goal
                   I((1 <= score_differential & score_differential <= 3)*(game_seconds_remaining <= 900)) + ### prefer a TD but ok with a field goal
                   I((4 <= score_differential & score_differential <= 10)*(game_seconds_remaining <= 900)) + ### ok with a field goal but game is still close
                   factor(era_A) +
                   qbq_ot_0_sum + qbq_ot_0_sum:yardline_100 +
                   oq_rot_0_total_sum +  oq_rot_0_total_sum:yardline_100 +
                   dq_dt_0_againstPass_sum +  dq_dt_0_againstPass_sum:yardline_100 +
                   dq_dt_0_againstRun_sum + dq_dt_0_againstRun_sum:yardline_100 +
                   qbq_dt_0_sum +  qbq_dt_0_sum:yardline_100 +
                   oq_rdt_0_sum + oq_rdt_0_sum:yardline_100 +
                   dq_ot_0_againstPass_sum + dq_ot_0_againstPass_sum:yardline_100 +
                   dq_ot_0_againstRun_sum + dq_ot_0_againstRun_sum:yardline_100,
                 weights = w, data = dataset)
  clean_lm(fit)
}

fit_mlr_yurko_s1d <- function(dataset, weight_me=FALSE) {
  if (!weight_me) { dataset$w = 1 }
  fit = multinom(label ~ 
                   yardline_100 +
                   factor(down) + down:yardline_100 +
                   log(ydstogo) +
                   # bs(half_seconds_remaining, knots=c(30)) +
                   bs(half_seconds_remaining, degree=1, df=1) +
                   utm:as.numeric(posteam_timeouts_remaining==0) +
                   I((score_differential <= -11)) + ### need a TD
                   I((score_differential >= 11)) + ### comfortable, field goal is fine
                   ### note:: fourth_quarter == game_seconds_remaining <= 900
                   I((score_differential <= -4)*(game_seconds_remaining <= 900)) + ### need a TD   
                   I((-3 <= score_differential & score_differential <= 0)*(game_seconds_remaining <= 900)) + ### ok with a field goal
                   I((1 <= score_differential & score_differential <= 3)*(game_seconds_remaining <= 900)) + ### prefer a TD but ok with a field goal
                   I((4 <= score_differential & score_differential <= 10)*(game_seconds_remaining <= 900)) + ### ok with a field goal but game is still close
                   factor(era_A) +
                   posteam_spread,
                 weights = w, data = dataset)
  clean_lm(fit)
}

fit_mlr_yurko_s2d <- function(dataset, weight_me=FALSE) {
  if (!weight_me) { dataset$w = 1 }
  fit = multinom(label ~ 
                   yardline_100 +
                   factor(down) + down:yardline_100 +
                   log(ydstogo) +
                   # bs(half_seconds_remaining, knots=c(30)) +
                   bs(half_seconds_remaining, degree=1, df=1) +
                   utm:as.numeric(posteam_timeouts_remaining==0) +
                   I((score_differential <= -11)) + ### need a TD
                   I((score_differential >= 11)) + ### comfortable, field goal is fine
                   ### note:: fourth_quarter == game_seconds_remaining <= 900
                   I((score_differential <= -4)*(game_seconds_remaining <= 900)) + ### need a TD   
                   I((-3 <= score_differential & score_differential <= 0)*(game_seconds_remaining <= 900)) + ### ok with a field goal
                   I((1 <= score_differential & score_differential <= 3)*(game_seconds_remaining <= 900)) + ### prefer a TD but ok with a field goal
                   I((4 <= score_differential & score_differential <= 10)*(game_seconds_remaining <= 900)) + ### ok with a field goal but game is still close
                   factor(era_A) +
                   posteam_spread + posteam_spread:yardline_100,
                 weights = w, data = dataset)
  clean_lm(fit)
}

########################################################################################
#### NOW, given best TQ metrics, fine tune the Time Remaining and Yardline splines. ####
########################################################################################

fit_mlr_yurko_s1dE <- function(dataset, weight_me=FALSE) {
  if (!weight_me) { dataset$w = 1 }
  fit = multinom(label ~ 
                   yardline_100 +
                   factor(down) + down:yardline_100 +
                   log(ydstogo) +
                   bs(half_seconds_remaining, knots=c(30)) +
                   # bs(half_seconds_remaining, degree=1, df=1) +
                   utm:as.numeric(posteam_timeouts_remaining==0) +
                   I((score_differential <= -11)) + ### need a TD
                   I((score_differential >= 11)) + ### comfortable, field goal is fine
                   ### note:: fourth_quarter == game_seconds_remaining <= 900
                   I((score_differential <= -4)*(game_seconds_remaining <= 900)) + ### need a TD   
                   I((-3 <= score_differential & score_differential <= 0)*(game_seconds_remaining <= 900)) + ### ok with a field goal
                   I((1 <= score_differential & score_differential <= 3)*(game_seconds_remaining <= 900)) + ### prefer a TD but ok with a field goal
                   I((4 <= score_differential & score_differential <= 10)*(game_seconds_remaining <= 900)) + ### ok with a field goal but game is still close
                   factor(era_A) +
                   posteam_spread,
                 weights = w, data = dataset)
  clean_lm(fit)
}

fit_mlr_yurko_s2dE <- function(dataset, weight_me=FALSE) {
  if (!weight_me) { dataset$w = 1 }
  fit = multinom(label ~ 
                   yardline_100 +
                   factor(down) + down:yardline_100 +
                   log(ydstogo) +
                   bs(half_seconds_remaining, knots=c(30)) +
                   # bs(half_seconds_remaining, degree=1, df=1) +
                   utm:as.numeric(posteam_timeouts_remaining==0) +
                   I((score_differential <= -11)) + ### need a TD
                   I((score_differential >= 11)) + ### comfortable, field goal is fine
                   ### note:: fourth_quarter == game_seconds_remaining <= 900
                   I((score_differential <= -4)*(game_seconds_remaining <= 900)) + ### need a TD   
                   I((-3 <= score_differential & score_differential <= 0)*(game_seconds_remaining <= 900)) + ### ok with a field goal
                   I((1 <= score_differential & score_differential <= 3)*(game_seconds_remaining <= 900)) + ### prefer a TD but ok with a field goal
                   I((4 <= score_differential & score_differential <= 10)*(game_seconds_remaining <= 900)) + ### ok with a field goal but game is still close
                   factor(era_A) +
                   posteam_spread + posteam_spread:yardline_100,
                 weights = w, data = dataset)
  clean_lm(fit)
}

#####################################################################
### MLR Models that weight each play by 1/(# plays in that epoch) ###

fit_mlr_weightedByEpoch <- function(dataset, fit_model_func=fit_mlr_yurko_s2dE) {
  dataset_w = 
    dataset %>%
    group_by(epoch) %>%
    mutate(w = 1/n()) %>%
    ungroup()
  fit = fit_model_func(dataset_w, w=TRUE)
  clean_lm(fit)
}

fit_mlr_yurko_s2dE_w <- function(dataset) {
  fit_mlr_weightedByEpoch(dataset, fit_model_func=fit_mlr_yurko_s2dE)
}

