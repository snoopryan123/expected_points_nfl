
########################
### Helper Functions ###
########################

# get_mlr_probs_for_predict <- function(probs, model) {
#   n = nrow(probs)
#   df = tibble()
#   df = bind_rows(df, tibble(p = probs[,1], i = 1:n, label = 0, outcome = "Touchdown"))
#   df = bind_rows(df, tibble(p = probs[,2], i = 1:n, label = 1, outcome = "Opp_Touchdown"))
#   df = bind_rows(df, tibble(p = probs[,3], i = 1:n, label = 2, outcome = "Field_Goal"))
#   df = bind_rows(df, tibble(p = probs[,4], i = 1:n, label = 3, outcome = "Opp_Field_Goal"))
#   df = bind_rows(df, tibble(p = probs[,5], i = 1:n, label = 4, outcome = "Safety"))
#   df = bind_rows(df, tibble(p = probs[,6], i = 1:n, label = 5, outcome = "Opp_Safety"))
#   df = bind_rows(df, tibble(p = probs[,7], i = 1:n, label = 6, outcome = "No_Score"))
#   df$model = model
#   df
# }

get_mlr_probs <- function(our_mlr, dataset, epoch_based_EP=FALSE, drive_based_EP=FALSE) {
  pred_matrix = predict(our_mlr, dataset, "probs")
  if (epoch_based_EP) {
    colnames(pred_matrix) = epoch_EP_outcomes
  } else if (drive_based_EP) {
    colnames(pred_matrix) = drive_EP_outcomes
  } else {
    stop(paste0("must have one of `epoch_based_EP`, `drive_based_EP`, or `wp`be TRUE."))
  }
  pred_matrix
}

predict_mlr_ep <- function(our_mlr, dataset, model_name, epoch_based_EP=FALSE, drive_based_EP=FALSE) {
  pred_cg = get_mlr_probs(our_mlr, dataset, epoch_based_EP=epoch_based_EP, drive_based_EP=drive_based_EP)
  if (is.null(nrow(pred_cg))) { pred_cg = matrix(pred_cg, nrow=1) }
  if (epoch_based_EP) {
    # data_full %>% distinct(outcome_epoch, pts_next_score) %>% arrange(outcome_epoch)
    pred_ep = pred_cg[,1]*(7) + pred_cg[,2]*(-7) + pred_cg[,3]*(3) + pred_cg[,4]*(-3) + pred_cg[,5]*(2) + pred_cg[,6]*(-2) + pred_cg[,7]*(0)
  } else if (drive_based_EP) {
    # print(data_full %>% distinct(outcome_drive, pts_end_of_drive) %>% arrange(outcome_drive))
    pred_ep = pred_cg[,1]*(7) + pred_cg[,2]*(3) + pred_cg[,3]*(0) + pred_cg[,4]*(-2) + pred_cg[,5]*(-7) 
  } else {
    stop(paste0("Either `epoch_based_EP` or `drive_based_EP` must be TRUE."))
  }
  return(tibble(pred = unname(pred_ep), model_name=model_name))
}

generate_mlr_outcomes <- function(probs) {
  generated_outcomes = sapply(1:nrow(probs), FUN = function (i) { which( unname(rmultinom(1, 1, probs[i,])[,1]) == 1) - 1 } )
  generated_outcomes
}

#####################################################################
### MLR Models that weight each play by 1/(# plays in that group) ###
#####################################################################

fit_mlr_weightedByEpoch <- function(dataset, fit_model_func) {
  dataset_w = dataset %>% group_by(epoch) %>% mutate(w = 1/n()) %>% ungroup()
  fit = fit_model_func(dataset_w, weight_me=TRUE)
  clean_lm(fit)
}

fit_mlr_weightedByDrive <- function(dataset, fit_model_func) {
  dataset_w = dataset %>% group_by(Drive) %>% mutate(w = 1/n()) %>% ungroup()
  fit = fit_model_func(dataset_w, weight_me=TRUE)
  clean_lm(fit)
}

###############################################################
### Multinomial Logistic Regression Models fit on All Downs ###
###############################################################

fit_mlr_epochEP_yurko_paper <- function(dataset, weight_me=FALSE) {
  if (!weight_me) { dataset$w = 1 }
  fit = multinom(outcome_epoch ~ 
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

fit_mlr_epochEP_yurko_plus <- function(dataset, weight_me=FALSE) {
  if (!weight_me) { dataset$w = 1 }
  fit = multinom(outcome_drive ~ 
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
                   factor(era_A),
                 weights = w, data = dataset)
  clean_lm(fit)
}

fit_mlr_epochEP_yurko_plus_tq <- function(dataset, weight_me=FALSE) {
  if (!weight_me) { dataset$w = 1 }
  fit = multinom(outcome_drive ~ 
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
                   qbqot +
                   oqot +
                   dqdt_againstPass +
                   dqdt_againstRun +
                   qbqdt +
                   oqdt +
                   dqot_againstPass +
                   dqot_againstRun,
                 weights = w, data = dataset)
  clean_lm(fit)
}
