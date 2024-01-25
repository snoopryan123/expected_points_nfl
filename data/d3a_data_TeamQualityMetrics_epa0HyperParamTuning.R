
### tune the hyperparameters alpha,beta,gamma for our hand-crafted team quality metrics
HYPERPARAM_TUNING = TRUE

### load data 
source("../A0_header.R")
data00 <- read_csv("data2.csv")

### hyperparam grid
K = 4
K_grid = seq(1/K,1,by=1/K)
K_grid_pos = seq(1/2,1,by=1/K)
HYPERPARAM_GRID = expand.grid(
  alpha = seq(0.99,1,by=0.0025),
  gamma = K_grid
  # gamma_qb = K_grid_pos,
  # gamma_o = K_grid,
  # gamma = K_grid
  # gamma_non_qb = K_grid
)
HYPERPARAM_GRID
K_grid
K_grid_pos
nrow(HYPERPARAM_GRID)

### shared hyperparams
EPA0_FROM_TRAIN_DATA_ONLY = TRUE
N0_op = 50 ### shrinkage prior: # attempts of value 0
N0_ot = 1500 ### shrinkage prior: # attempts of value 0
N0_dt_againstPass = 500 ### shrinkage prior: # attempts of value 0
N0_dt_againstRun = 500 ### shrinkage prior: # attempts of value 0
N0_dt_total = 1500 ### shrinkage prior: # attempts of value 0

### EP model as a function of our 8 hand-crafted team quality metrics
fit_lm_oq4xdq4x_A <- function(dataset) {
  fit = lm(pts_next_score ~  
             bs(half_seconds_remaining, df=3, knots=c(30,120))*bs(yardline_100, df=5):factor(down) +
             log(ydstogo):factor(down) +
             I(half_seconds_remaining <= 120):as.numeric(posteam_timeouts_remaining==0) + 
             I((score_differential <= -11)) + ### need a TD
             I((score_differential <= -4)*(game_seconds_remaining <= 900)) + ### need a TD   ### note:: fourth_quarter == game_seconds_remaining <= 900
             I((-3 <= score_differential & score_differential <= 0)*(game_seconds_remaining <= 900)) + ### ok with a field goal
             I((1 <= score_differential & score_differential <= 3)*(game_seconds_remaining <= 900)) + ### prefer a TD but ok with a field goal
             I((4 <= score_differential & score_differential <= 10)*(game_seconds_remaining <= 900)) + ### ok with a field goal but game is still close
             I((score_differential >= 11)) + ### comfortable, field goal is fine
             qbq_ot_0_sum + 
             oq_rot_0_total_sum +
             dq_dt_0_againstPass_sum +
             dq_dt_0_againstRun_sum +
             qbq_dt_0_sum + 
             oq_rdt_0_sum +
             dq_ot_0_againstPass_sum +
             dq_ot_0_againstRun_sum,
           data = dataset)
  clean_lm(fit)
}

### loop over the grid to test each hyperparameter combo; takes about 1 min. per row
losses = rep(NA, nrow(HYPERPARAM_GRID))
for (iii in 1:nrow(HYPERPARAM_GRID)) {
  print(paste0("tuning hyperparameter combo iii=",iii,"/",nrow(HYPERPARAM_GRID)))
  print(HYPERPARAM_GRID[iii,])
  
  ### set the hyperparams
  alpha_op = HYPERPARAM_GRID$alpha[iii]
  beta_o = HYPERPARAM_GRID$alpha[iii]
  beta = HYPERPARAM_GRID$alpha[iii]
  gamma_qb = HYPERPARAM_GRID$gamma[iii]
  gamma_o = HYPERPARAM_GRID$gamma[iii]
  gamma = HYPERPARAM_GRID$gamma[iii]
  # gamma_o = HYPERPARAM_GRID$gamma_o[iii]
  # gamma = HYPERPARAM_GRID$gamma[iii]
  # gamma_qb = HYPERPARAM_GRID$gamma_qb[iii]
  # gamma_o = HYPERPARAM_GRID$gamma_non_qb[iii]
  # gamma = HYPERPARAM_GRID$gamma_non_qb[iii]
  
  ### create the team quality metrics with these hyperparams -> `data3`
  source("d3b_data_TeamQualityMetrics_epa0.R")
  
  ### fit EP model with these TQ metrics
  ep_model = fit_lm_oq4xdq4x_A(data3 %>% filter(train_play))
  ep_model
  
  ### make EP predictions 
  data3 = data3 %>% mutate(ep_hat = predict(ep_model, .))
  
  ### compute out-of-sample loss
  loss_df_iii = 
    data3 %>% 
    filter(test_play) %>%
    summarise(
      MAE = mean(abs(pts_next_score - ep_hat))
    )
  losses[iii] = loss_df_iii$MAE
  print(paste0("loss = ", losses[iii]))
}

### evaluate losses
HYPERPARAM_GRID$loss = losses
HYPERPARAM_GRID1 = HYPERPARAM_GRID %>% arrange(loss)
head(HYPERPARAM_GRID1)

# write_csv(HYPERPARAM_GRID1, "df_losses_TeamQualityMetrics_epa0HyperParamTuning.csv")
# min_loss_idx = which(losses == min(losses, na.rm=T))[1]
# min_loss_idx
# HYPERPARAM_GRID[1,]





