
library(glmnet)
PRE_LOADED_TrainTestSplitAndTeamQualities = TRUE
########################
filewd = getwd()
setwd("..")
source("A1_load_data.R") 
setwd(filewd)
########################

################################################################################

D = data_full_0 %>% filter(train_play) %>% drop_na(ep0,epa0,qb_name)

generate_ridge_X <- function(dataset) {
  model.matrix(terms( ~  
                        down_3or4 +
                        yardline_100 +
                        log(ydstogo) +
                        factor(posteam):factor(season) +
                        factor(defteam):factor(season),
                      keep.order = TRUE),
                   data = dataset)
}
X = generate_ridge_X(D)
Y = D$ep0
k = dim(X)[2]
k
penalties = c( rep(0,4), rep(1,k-4))

#perform k-fold cross-validation to find optimal lambda value
{
  # # takes 10 mins
  # cv_model <- cv.glmnet(X, Y, alpha = 0, penalty.factor =  penalties, nfolds=5) ### alpha=0 means ridge
  # #find optimal lambda value that minimizes test MSE
  # best_lambda <- cv_model$lambda.min ###
}
best_lambda = 0.1
#find coefficients of best model
best_model_ep0 <- glmnet(X, Y, alpha = 0, penalty.factor =  penalties, lambda = best_lambda)
###
clean_lm(best_model_ep0)

generate_fake_EPA0 <- function(D, ep0_sd) {
  ### generate fake_EP0
  # browser()
  # X_ = X
  X_ = generate_ridge_X(D)
  D$fake_ep0 = predict(best_model_ep0, X_) + rnorm(n=nrow(D), mean=0, sd=ep0_sd)
  # D$fake_ep0 = predict(best_model_ep0, D) + rnorm(n=nrow(D), mean=0, sd=ep0_sd)
  
  ### create fake_EP0 within each drive
  D = D %>%
    mutate(half = ifelse(qtr == 1 | qtr == 2, 1, 2)) %>%
    relocate(half, .after = game_id) %>%
    group_by(game_id, half, posteam, drive) %>%
    mutate(fake_epa0 = c(diff(fake_ep0), NA)) %>%
    ungroup()
  
  ### create fake_ep0 for scoring plays
  D = D %>%
    mutate(fake_epa0 = ifelse(pts_of_play != 0, pts_of_play - fake_ep0, fake_epa0))
  
  ### create fake_ep0 for the last play of a drive, a non-scoring plays
  D = D %>%
    mutate(fake_epa0 = ifelse(
      ### pos_changes_w is -1 if possession changes at the end of the play and it's not a score
      pos_changes_w == -1 & game_id == lead(game_id, default=""),
      -lead(fake_ep0) - fake_ep0,
      fake_epa0
    ))
  
  return(D)
}
# generate_fake_EPA0(D, ep0_sd=1/20)

keep_every_kth_row <- function(D,k) {
  D %>% filter(row_number() %% k == 0)
}

{
  ####################################
  ##### EPA0 distribution checks #####
  ####################################

  ### there is autocorrelation of team quality metrics
  acf(D$qbq_ot_0_sum, lag.max=45)

  ### there is autocorrelation of EP0
  acf(D$ep0, lag.max=5)
  
  ### not much autocorrelation of EPA0 !
  acf(D[!is.na(D$epa0),]$epa0, lag.max=5)
  
  ### even for a fixed team/player, there is little autocorrelation every 5th obs.
  acf( (D %>% filter(qb_name=="T.Brady") %>% drop_na(epa0))$epa0, lag.max=45)
  acf( (D %>% filter(qb_name=="T.Brady") %>% drop_na(epa0))$epa0, lag.max=10)
  acf( (D %>% filter(qb_name=="T.Brady") %>% drop_na(epa0))$epa0, lag.max=5)
  
  ### hist
  hist(D$epa0, breaks=seq(-20,20,by=0.5))

  ##################################################################
  ##### check assumptions of the fake_EP0 column for knockoffs #####
  ##################################################################

  # ep0_sd = summary(lm_ep0)$sigma
  ep0_sd = 1/20#1/5
  D = generate_fake_EPA0(D, ep0_sd=ep0_sd)

  sum(is.na(D$epa0))
  sum(is.na(D$fake_epa0))
  dim(D)
  Da = D %>% drop_na(fake_epa0)
  dim(Da)

  ### histogram: the distribution of EPA0 and fake_EPA0 is roughly the same
  plot_hist_epa0_fake_epa0 = Da %>%
    mutate(A="A", B="B") %>%
    ggplot() +
    geom_histogram(aes(x=epa0, fill=A), alpha=0.5, binwidth=1/2) +
    geom_histogram(aes(x=fake_epa0, fill=B), alpha=0.5, binwidth=1/2) +
    scale_fill_manual(name="", values=c("blue","red"),labels=c("EPA0","Fake EPA0")) +
    xlab("EPA0")
  plot_hist_epa0_fake_epa0
  ggsave("plot_hist_epa0_fake_epa0.png", plot_hist_epa0_fake_epa0, width=10, height=5)

  ### the random vectors EPA0 and fake_EPA0 have little autocorrelation
  plot_acf <- function(x,title,filename,lag.max = 7) {
    AutoCorrelation <- acf(x, lag.max=lag.max, plot = FALSE)
    png(filename, width=1000, height=500)
    plot(AutoCorrelation, main = title)
    dev.off()
  }
  plot_acf(Da[!is.na(Da$epa0),]$epa0, "EPA0 autocorrelation", "plot_acf_epa0.png")
  plot_acf(Da[!is.na(Da$fake_epa0),]$fake_epa0, "Fake EPA0 autocorrelation", "plot_acf_fake_epa0.png")

  ### keep every 5th row to remove all autocorrelation
  D1 = keep_every_kth_row(Da, k=5)

  ### the random vectors EPA0 and fake_EPA0 have no autocorrelation
  plot_acf(D1[!is.na(D1$epa0),]$epa0, "EPA0 autocorrelation after truncation", "plot_acf_epa0_trunc.png")
  plot_acf(D1[!is.na(D1$fake_epa0),]$fake_epa0, "Fake EPA0 autocorrelation after truncation", "plot_acf_fake_epa0_trunc.png")
}

############################################
############################################
##### create fake team quality metrics #####
############################################
############################################

##############################################
#### Offensive Quality of the Quartberack ####
##############################################

get_qbq_ot <- function(D, fake_epa0 = FALSE) {
  epa0_name = if (fake_epa0) "fake_epa0" else "epa0"
  data1a = D
  
  alpha_op = 0.995 ### 
  gamma_qb = 0.75 ###
  N0_op = 50 ### shrinkage prior: N0 attempts of value 0
  
  all_qb_names = unique( (data1a %>% drop_na(passer_player_name))$passer_player_name )
  # j=737
  
  ### quaterback quality
  qbq_ot_0 = tibble()
  for (j in 1:length(all_qb_names)) {
    if (j %% 10 == 0) print( paste0("qbq_ot_0 progress: QB j = ", j, " of n = ", length(all_qb_names), "; alpha = ", alpha_op, "; gamma = ", gamma_qb) )
  
    player_j = all_qb_names[j]
    data_player_j = data1a %>% filter(qb_play & replace_na(offensive_player_name, "") == player_j) ### all plays in which the QB was the passer or rusher
    
    nrow(data_player_j)
    
    szn_begins_player_j = c(1, diff(data_player_j$season))
    # View(tibble(season = data_player_j$season, szn_begins = szn_begins_player_j)) ### check
    ###epa0_player_j = data_player_j$epa0
    epa0_player_j = data_player_j[[epa0_name]]
    
    qbq_ot_0_j_sum = numeric(length(epa0_player_j))
    qbq_ot_0_j_mean = numeric(length(epa0_player_j))
    ### we have no information prior to the player's first play
    qbq_ot_0_j_sum[1] = 0
    qbq_ot_0_j_mean[1] = 0
    n_j = N0_op
    if (nrow(data_player_j) > 1) {
      for (k in 2:nrow(data_player_j)) {
        SBM_k = ifelse(szn_begins_player_j[k] == 1, gamma_qb, 1) # szn-begins multiplier
        
        if (!is.na(epa0_player_j[k-1])) {
          qbq_ot_0_j_sum[k] = SBM_k * alpha_op * qbq_ot_0_j_sum[k-1] + epa0_player_j[k-1]
          qbq_ot_0_j_mean[k] = SBM_k * alpha_op*(1 - 1/n_j)*qbq_ot_0_j_mean[k-1] + 1/n_j*epa0_player_j[k-1]
          n_j = n_j + 1
        } else {
          qbq_ot_0_j_sum[k] = SBM_k * qbq_ot_0_j_sum[k-1]
          qbq_ot_0_j_mean[k] = SBM_k * qbq_ot_0_j_mean[k-1]
        }
      }
    }
    data_player_j$qbq_ot_0_sum = qbq_ot_0_j_sum
    data_player_j$qbq_ot_0_mean = qbq_ot_0_j_mean
    
    ## View(data_player_j)
    
    qbq_ot_0 = bind_rows(
      qbq_ot_0, 
      data_player_j %>% select(row_idx, offensive_player_name, all_of(starts_with("qbq_ot_0")))
    )
  }

  qbq_ot_0
}

#############################################################################
#### Offensive Quality of the Offensive Team (for non-Quarterback plays) ####
#############################################################################

get_oq_ot <- function(D, fake_epa0 = FALSE) {
  epa0_name = if (fake_epa0) "fake_epa0" else "epa0"
  data1a = D
  
  beta_o = 0.995 ###
  gamma_o = 1/5 ###
  N0_ot = 1500 ### shrinkage prior: N0 attempts of value 0
  
  all_offteamnames = sort(unique( (data1a %>% drop_na(posteam))$posteam ))
  # j = 17
  
  oq_ot_0 = tibble()
  for (j in 1:length(all_offteamnames)) {
    print( paste0("oq_ot_0 progress: team j = ", j, " of n = ", length(all_offteamnames), "; beta_o = ", beta_o, "; gamma_o = ", gamma_o) )
    
    team_j = all_offteamnames[j]
    data_team_j = data1a %>% filter(posteam == team_j & !qb_play & !is.na(pass_or_rush))
    ## View(data_team_j)
    szn_begins_team_j = c(1, diff(data_team_j$season))
    # View(tibble(season = data_team_j$season, szn_begins = szn_begins_team_j)) ### check
    # epa0_team_j = data_team_j$epa0
    epa0_team_j = data_team_j[[epa0_name]]
    
    pass_or_rush_team_j = data_team_j$pass_or_rush
    pass_play_team_j = ifelse(is.na(pass_or_rush_team_j), FALSE, pass_or_rush_team_j=="pass")
  
    oq_ot_0_total_j_sum = numeric(length(epa0_team_j))
    oq_ot_0_total_j_mean = numeric(length(epa0_team_j))
    oq_rot_0_total_j_sum = numeric(length(epa0_team_j))
    oq_rot_0_total_j_mean = numeric(length(epa0_team_j))
    
    ### base case
    oq_ot_0_total_j_sum[1] = 0
    oq_ot_0_total_j_mean[1] = 0
    oq_rot_0_total_j_sum[1] = 0
    oq_rot_0_total_j_mean[1] = 0
    
    n_j = N0_ot
    nr_j = N0_ot
    if (nrow(data_team_j) > 1) {
      for (k in 2:nrow(data_team_j)) {
        if (k %% 500 == 0) print( paste0("oq_ot_0 progress: team j = ", j, " of n = ", length(all_offteamnames), 
                                         "; play k = ", k, " of n = ", nrow(data_team_j),
                                         "; beta = ", beta_o, "; gamma = ", gamma_o) )
        
        SBM_k = ifelse(szn_begins_team_j[k] == 1, gamma_o, 1) # szn-begins multiplier
        
        if (!is.na(epa0_team_j[k-1]) & !is.na(pass_or_rush_team_j[k-1])) {
          oq_ot_0_total_j_sum[k] = SBM_k * beta_o * oq_ot_0_total_j_sum[k-1] + epa0_team_j[k-1]
          oq_ot_0_total_j_mean[k] = SBM_k*beta_o*(1 - 1/n_j)*oq_ot_0_total_j_mean[k-1] + 1/n_j*epa0_team_j[k-1]
          n_j = n_j + 1
          
          if (!pass_play_team_j[k-1]) {
            oq_rot_0_total_j_sum[k] = SBM_k * beta_o * oq_rot_0_total_j_sum[k-1] + epa0_team_j[k-1]
            oq_rot_0_total_j_mean[k] = SBM_k*beta_o*(1 - 1/nr_j)*oq_rot_0_total_j_mean[k-1] + 1/nr_j*epa0_team_j[k-1]
            nr_j = nr_j + 1
          } else {
            oq_rot_0_total_j_sum[k] = SBM_k * oq_rot_0_total_j_sum[k-1]
            oq_rot_0_total_j_mean[k] = SBM_k * oq_rot_0_total_j_mean[k-1]
          }
        } else {
          oq_ot_0_total_j_sum[k] = SBM_k * oq_ot_0_total_j_sum[k-1]
          oq_ot_0_total_j_mean[k] = SBM_k * oq_ot_0_total_j_mean[k-1]
          oq_rot_0_total_j_sum[k] = SBM_k * oq_rot_0_total_j_sum[k-1]
          oq_rot_0_total_j_mean[k] = SBM_k * oq_rot_0_total_j_mean[k-1]
        }
      }
    }
    
    ###
    data_team_j$oq_ot_0_total_sum = oq_ot_0_total_j_sum
    data_team_j$oq_ot_0_total_mean = oq_ot_0_total_j_mean
    data_team_j$oq_rot_0_total_sum = oq_rot_0_total_j_sum
    data_team_j$oq_rot_0_total_mean = oq_rot_0_total_j_mean
    ## View(data_team_j)
    
    oq_ot_0 = bind_rows(
      oq_ot_0, 
      data_team_j %>% select(row_idx, posteam, all_of(starts_with("oq_ot_0")), all_of(starts_with("oq_rot_0")))
    )
  }
  
  oq_ot_0
}

#################################################
#### Defensive Quality of the Defensive Team ####
#################################################

get_dq_dt <- function(D, fake_epa0 = FALSE) {
  epa0_name = if (fake_epa0) "fake_epa0" else "epa0"
  data2a = D
  
  beta = 0.995 ###
  # gamma = 1/20 ###
  # gamma = 1/5 ###
  gamma = 1/3 ###
  N0_dt_againstPass = 500 ### shrinkage prior: N0 attempts of value 0
  N0_dt_againstRun = 500 ### shrinkage prior: N0 attempts of value 0
  N0_dt_total = 1500 ### shrinkage prior: N0 attempts of value 0
  
  
  all_defteamnames = sort(unique( (data2a %>% drop_na(defteam))$defteam ))
  # j = 17
  
  dq_dt_0 = tibble()
  for (j in 1:length(all_defteamnames)) {
    print( paste0("dq_dt_0 progress: team j = ", j, " of n = ", length(all_defteamnames), "; beta = ", beta, "; gamma = ", gamma) )
    
    team_j = all_defteamnames[j]
    data_team_j = data2a %>% filter(defteam == team_j) %>% select(any_of(c("row_idx", "season", "defteam", "pass_or_rush", "epa0", "fake_epa0")))
    ## View(data_team_j)
    szn_begins_team_j = c(1, diff(data_team_j$season))
    # View(tibble(season = data_team_j$season, szn_begins = szn_begins_team_j)) ### check
    ###epa0_team_j = data_team_j$epa0
    epa0_team_j = data_team_j[[epa0_name]]
    pass_or_rush_team_j = data_team_j$pass_or_rush
 
    ### initialize
    N_j = length(epa0_team_j)
    dq_dt_0_againstPass_j_sum = numeric(N_j)
    dq_dt_0_againstRun_j_sum = numeric(N_j)
    dq_dt_0_total_j_sum = numeric(N_j)
    dq_dt_0_againstPass_j_mean = numeric(N_j)
    dq_dt_0_againstRun_j_mean = numeric(N_j)
    dq_dt_0_total_j_mean = numeric(N_j)
    
    ### base case
    dq_dt_0_againstPass_j_sum[1] = 0
    dq_dt_0_againstRun_j_sum[1] = 0
    dq_dt_0_total_j_sum[1] = 0
    dq_dt_0_againstPass_j_mean[1] = 0
    dq_dt_0_againstRun_j_mean[1] = 0
    dq_dt_0_total_j_mean[1] = 0
    
    ### counters
    n_againstPass_j = N0_dt_againstPass
    n_againstRun_j = N0_dt_againstRun
    n_total_j = N0_dt_total
    
    for (k in 2:nrow(data_team_j)) {
      # if (k %% 500 == 0) print( paste0("dq_dt_0 progress: team j = ", j, " of n = ", length(all_defteamnames), 
      #                                  "; play k = ", k, " of n = ", nrow(data_team_j),
      #                                  "; beta = ", beta, "; gamma = ", gamma) )
      
      SBM_k = ifelse(szn_begins_team_j[k] == 1, gamma, 1) # szn-begins multiplier
      
      epa0_prevPlay = epa0_team_j[k-1] 
      passOrRush_prevPlay = pass_or_rush_team_j[k-1]
      # currPlay_isPass = !is.na(epa0_prevPlay) & !is.na(pass_or_rush_team_j[k]) & pass_or_rush_team_j[k] == "pass"
      # currPlay_isRush = !is.na(epa0_prevPlay) & !is.na(pass_or_rush_team_j[k]) & pass_or_rush_team_j[k] == "run"
      prevPlay_isPass = !is.na(epa0_prevPlay) & !is.na(passOrRush_prevPlay) & passOrRush_prevPlay == "pass"
      prevPlay_isRush = !is.na(epa0_prevPlay) & !is.na(passOrRush_prevPlay) & passOrRush_prevPlay == "run"
      
      if (prevPlay_isPass) { ### then update the Pass Def metrics for the start of the current play
        dq_dt_0_againstPass_j_sum[k] = SBM_k * beta * dq_dt_0_againstPass_j_sum[k-1] + epa0_prevPlay
        dq_dt_0_againstPass_j_mean[k] = SBM_k*beta*(1 - 1/n_againstPass_j)*dq_dt_0_againstPass_j_mean[k-1] + 
                                        1/n_againstPass_j*epa0_prevPlay
        n_againstPass_j = n_againstPass_j + 1
        
        dq_dt_0_total_j_sum[k] = SBM_k * beta * dq_dt_0_total_j_sum[k-1] + epa0_prevPlay
        dq_dt_0_total_j_mean[k] = SBM_k*beta*(1 - 1/n_total_j)*dq_dt_0_total_j_mean[k-1] + 
                                  1/n_total_j*epa0_prevPlay
        n_total_j = n_total_j + 1
        
        dq_dt_0_againstRun_j_sum[k] = SBM_k * dq_dt_0_againstRun_j_sum[k-1]
        dq_dt_0_againstRun_j_mean[k] = SBM_k * dq_dt_0_againstRun_j_mean[k-1]
      } else if (prevPlay_isRush) { ### then update the Rush Def metrics for the start of the current play
        dq_dt_0_againstRun_j_sum[k] = SBM_k * beta * dq_dt_0_againstRun_j_sum[k-1] + epa0_prevPlay
        dq_dt_0_againstRun_j_mean[k] = SBM_k*beta*(1 - 1/n_againstRun_j)*dq_dt_0_againstRun_j_mean[k-1] + 
                                       1/n_againstRun_j*epa0_prevPlay
        n_againstRun_j = n_againstRun_j + 1
        
        dq_dt_0_total_j_sum[k] = SBM_k * beta * dq_dt_0_total_j_sum[k-1] + epa0_prevPlay
        dq_dt_0_total_j_mean[k] = SBM_k*beta*(1 - 1/n_total_j)*dq_dt_0_total_j_mean[k-1] + 
                                  1/n_total_j*epa0_prevPlay
        n_total_j = n_total_j + 1
        
        dq_dt_0_againstPass_j_sum[k] = SBM_k * dq_dt_0_againstPass_j_sum[k-1]
        dq_dt_0_againstPass_j_mean[k] = SBM_k * dq_dt_0_againstPass_j_mean[k-1]
      } else {
        dq_dt_0_againstRun_j_sum[k] = SBM_k * dq_dt_0_againstRun_j_sum[k-1]
        dq_dt_0_againstPass_j_sum[k] = SBM_k * dq_dt_0_againstPass_j_sum[k-1]
        dq_dt_0_total_j_sum[k] = SBM_k * dq_dt_0_total_j_sum[k-1]
        dq_dt_0_againstRun_j_mean[k] = SBM_k * dq_dt_0_againstRun_j_mean[k-1]
        dq_dt_0_againstPass_j_mean[k] = SBM_k * dq_dt_0_againstPass_j_mean[k-1]
        dq_dt_0_total_j_mean[k] = SBM_k * dq_dt_0_total_j_mean[k-1]
      }
    }
  
    ### combined defensive quality metric
    pass_or_rush_team_j_AA = replace_na(pass_or_rush_team_j, "")
    dq_dt_0_combined_j_sum = 
      (pass_or_rush_team_j_AA == "pass")*dq_dt_0_againstPass_j_sum +
      (pass_or_rush_team_j_AA == "run")*dq_dt_0_againstRun_j_sum +
      (pass_or_rush_team_j_AA != "pass" & pass_or_rush_team_j_AA != "run")*dq_dt_0_total_j_sum
    dq_dt_0_combined_j_mean = 
      (pass_or_rush_team_j_AA == "pass")*dq_dt_0_againstPass_j_mean +
      (pass_or_rush_team_j_AA == "run")*dq_dt_0_againstRun_j_mean +
      (pass_or_rush_team_j_AA != "pass" & pass_or_rush_team_j_AA != "run")*dq_dt_0_total_j_mean
    
    ###
    data_team_j$dq_dt_0_againstRun_sum = dq_dt_0_againstRun_j_sum
    data_team_j$dq_dt_0_againstPass_sum = dq_dt_0_againstPass_j_sum
    data_team_j$dq_dt_0_total_sum = dq_dt_0_total_j_sum
    data_team_j$dq_dt_0_againstRun_mean = dq_dt_0_againstRun_j_mean
    data_team_j$dq_dt_0_againstPass_mean = dq_dt_0_againstPass_j_mean
    data_team_j$dq_dt_0_total_mean = dq_dt_0_total_j_mean
    data_team_j$dq_dt_0_combined_sum = dq_dt_0_combined_j_sum
    data_team_j$dq_dt_0_combined_mean = dq_dt_0_combined_j_mean
    ## View(data_team_j)
    
    dq_dt_0 = bind_rows(
      dq_dt_0, 
      data_team_j %>% select(row_idx, season, defteam, all_of(starts_with("dq_dt_0")))
    )
  }
  dq_dt_0
}

# ### tests
# aa=get_qbq_ot(D, fake_epa0 = T)
# aa=get_oq_ot(D, fake_epa0 = T)
# aa1=get_oq_ot(D, fake_epa0 = F)
# aaa=get_dq_dt(D, fake_epa0 = T)
# aaa1=get_dq_dt(D, fake_epa0 = F)

######################
#### Save metrics ####
######################

get_oqot_dqdt_metrics <- function(D, fake_epa0 = FALSE) {
  a1 = get_qbq_ot(D, fake_epa0 = fake_epa0)
  a2 = get_oq_ot(D, fake_epa0 = fake_epa0)
  a3 = get_dq_dt(D, fake_epa0 = fake_epa0)
  
  DD = D %>% left_join(a1) %>% left_join(a2) 
  DD = DD %>%
    group_by(season, posteam) %>%
    mutate(
      qbq_ot_0_sum = zoo::na.locf(qbq_ot_0_sum, na.rm = F),
      qbq_ot_0_mean = zoo::na.locf(qbq_ot_0_mean, na.rm = F),
      qbq_ot_0_sum = replace_na(qbq_ot_0_sum, 0),
      qbq_ot_0_mean = replace_na(qbq_ot_0_mean, 0),
      oq_rot_0_total_sum = zoo::na.locf(oq_rot_0_total_sum, na.rm = F),
      oq_rot_0_total_mean = zoo::na.locf(oq_rot_0_total_mean, na.rm = F),
      oq_rot_0_total_sum = replace_na(oq_rot_0_total_sum, 0),
      oq_rot_0_total_mean = replace_na(oq_rot_0_total_mean, 0),
    )  
  DD = DD %>% left_join(a3) %>% ungroup()
  
  return(DD)
}

#################################################
#### Offensive Quality of the Defensive Team ####
#### Defensive Quality of the Offensive Team ####
#################################################

get_oqdt_dqot_metrics <- function(DD) {
  data2b = DD

  all_teamnames = sort(unique( (data2b %>% drop_na(defteam))$defteam ))
  # j = 17; j = 14; j = 26
  
  oq_dt_0 = tibble()
  dq_ot_0 = tibble()
  for (j in 1:length(all_teamnames)) {
    print( paste0("oq_dt_0 & dq_ot_0 progress: team j = ", j, " of n = ", length(all_teamnames)) )
    
    team_j = all_teamnames[j]
    data_team_j = data2b %>% filter(posteam == team_j | defteam == team_j) %>%
      select(row_idx, season, posteam, defteam, pass_or_rush, 
             oq_ot_0_total_sum, dq_dt_0_total_sum, oq_ot_0_total_mean, dq_dt_0_total_mean,
             dq_dt_0_againstPass_sum, dq_dt_0_againstRun_sum,
             qbq_ot_0_sum, qbq_ot_0_mean, oq_rot_0_total_sum, oq_rot_0_total_mean, qb_name, 
       )
    N_j = nrow(data_team_j)
    ## data_team_j
    
    ###############################################################
    ### team_j==26: PHI
    ### PHI, PHI, PHI, PHI, PHI, ARI, ARI, ARI, ARI, PHI, PHI, PHI, 
    ### ARI, ARI, ARI, ARI, ARI, PHI, PHI, PHI, PHI, ARI, ARI, ARI, 
    ###############################################################
    ### 1,   1,   1,   1,   1,   0,   0,   0,   0,   1,   1,   1,   team_j_isOnOffense
    ### 0,   0,   0,   0,   0,   1,   1,   1,   1,   0,   0,   0,   team_j_isOnDefense
    ###
    ### 0,   0,   0,   0,   0,   5,   5,   5,   5,   0,   0,   0,   oq_dt_0_j_idx
    ### 0,   0,   0,   0,   0,   0,   0,   0,   0,   9,   9,   9,   dq_ot_0_j_idx
    ###############################################################
    
    isOnOffense_j = as.numeric(data_team_j$posteam == team_j)
    isOnDefense_j = as.numeric(data_team_j$defteam == team_j)
    Off_Becomes_Def_j = diff(isOnOffense_j)
    od_switchpoint_idx_j = c(lag((Off_Becomes_Def_j == -1)*1:(N_j-1), default=0), 0)
    do_switchpoint_idx_j = c(lag((Off_Becomes_Def_j == 1)*1:(N_j-1), default=0), 0)
    
    oq_dt_0_j_idx = numeric(N_j)
    dq_ot_0_j_idx = numeric(N_j)
    for (k in 2:N_j) {
      # print(k)
      if (isOnOffense_j[k] & do_switchpoint_idx_j[k] != 0) {
        dq_ot_0_j_idx[k] = do_switchpoint_idx_j[k]
      } else if (isOnOffense_j[k]) {
        dq_ot_0_j_idx[k] = dq_ot_0_j_idx[k-1]
      }
      else if (isOnDefense_j[k] & od_switchpoint_idx_j[k] != 0) {
        oq_dt_0_j_idx[k] = od_switchpoint_idx_j[k]
      } else if (isOnDefense_j[k]) {
        oq_dt_0_j_idx[k] = oq_dt_0_j_idx[k-1]
      }
    }
    
    ##############################
    dq_ot_0_j_sum = dq_ot_0_j_idx
    dq_ot_0_j_sum[dq_ot_0_j_sum != 0] = data_team_j$dq_dt_0_total_sum[dq_ot_0_j_idx]
    
    dq_ot_0_j_mean = dq_ot_0_j_idx
    dq_ot_0_j_mean[dq_ot_0_j_mean != 0] = data_team_j$dq_dt_0_total_mean[dq_ot_0_j_idx]
    
    dq_ot_0_j_againstPass_sum = dq_ot_0_j_idx
    dq_ot_0_j_againstPass_sum[dq_ot_0_j_againstPass_sum != 0] = data_team_j$dq_dt_0_againstPass_sum[dq_ot_0_j_idx]
    
    dq_ot_0_j_againstRun_sum = dq_ot_0_j_idx
    dq_ot_0_j_againstRun_sum[dq_ot_0_j_againstRun_sum != 0] = data_team_j$dq_dt_0_againstRun_sum[dq_ot_0_j_idx]
    
    ##############################
    oq_dt_0_j_sum = oq_dt_0_j_idx
    oq_dt_0_j_sum[oq_dt_0_j_sum != 0] = data_team_j$oq_ot_0_total_sum[oq_dt_0_j_idx]
    
    oq_dt_0_j_mean = oq_dt_0_j_idx
    oq_dt_0_j_mean[oq_dt_0_j_mean != 0] = data_team_j$oq_ot_0_total_mean[oq_dt_0_j_idx]
    
    qbq_dt_0_j_sum = oq_dt_0_j_idx
    qbq_dt_0_j_sum[qbq_dt_0_j_sum != 0] = data_team_j$qbq_ot_0_sum[oq_dt_0_j_idx]
    
    qbq_dt_0_j_mean = oq_dt_0_j_idx
    qbq_dt_0_j_mean[qbq_dt_0_j_mean != 0] = data_team_j$qbq_ot_0_mean[oq_dt_0_j_idx]
    
    oq_rdt_0_j_sum = oq_dt_0_j_idx
    oq_rdt_0_j_sum[oq_rdt_0_j_sum != 0] = data_team_j$oq_rot_0_total_sum[oq_dt_0_j_idx]
    
    oq_rdt_0_j_mean = oq_dt_0_j_idx
    oq_rdt_0_j_mean[oq_rdt_0_j_mean != 0] = data_team_j$oq_rot_0_total_mean[oq_dt_0_j_idx]
    
    qb_name_dt_j = oq_dt_0_j_idx
    qb_name_dt_j[qb_name_dt_j != 0] = data_team_j$qb_name[oq_dt_0_j_idx]
    
    ##########################
    # ### check
    # isOnOffense_j[1:12]
    # Off_Becomes_Def_j[1:12]
    # od_switchpoint_idx_j[1:12]
    # do_switchpoint_idx_j[1:12]
    # oq_dt_0_j_idx[1:12]
    # dq_ot_0_j_idx[1:12]
    # oq_dt_0_j_sum[1:12]
    # dq_ot_0_j_sum[1:12]
    ##########################
    
    data_team_j$dq_ot_0_sum = dq_ot_0_j_sum
    data_team_j$dq_ot_0_mean = dq_ot_0_j_mean
    
    data_team_j$dq_ot_0_againstPass_sum = dq_ot_0_j_againstPass_sum
    
    data_team_j$dq_ot_0_againstRun_sum = dq_ot_0_j_againstRun_sum
    
    data_team_j$oq_dt_0_sum = oq_dt_0_j_sum
    data_team_j$oq_dt_0_mean = oq_dt_0_j_mean
    
    data_team_j$qbq_dt_0_sum = qbq_dt_0_j_sum
    data_team_j$qbq_dt_0_mean = qbq_dt_0_j_mean
    
    data_team_j$oq_rdt_0_sum = oq_rdt_0_j_sum
    data_team_j$oq_rdt_0_mean = oq_rdt_0_j_mean
    
    data_team_j$qb_name_dt = qb_name_dt_j
  
    data_team_j_off = data_team_j %>% filter(posteam == team_j) %>% select(
      row_idx, posteam, all_of(starts_with("dq_ot_0"))
    )
    data_team_j_def = data_team_j %>% filter(defteam == team_j) %>% select(
      row_idx, defteam, all_of(starts_with("oq_dt_0")), all_of(starts_with("qbq_dt_0")), all_of(starts_with("oq_rdt_0")), qb_name_dt
    )
    
    oq_dt_0 = bind_rows(oq_dt_0, data_team_j_def)
    dq_ot_0 = bind_rows(dq_ot_0, data_team_j_off)
  }
  
  ### save the oq, dq metrics
  data2c = data2b %>% left_join(oq_dt_0) %>% left_join(dq_ot_0) 
  return(data2c)
}

##########################################################
#### get combined df with fake TQ and real TQ metrics ####
##########################################################

get_TQ_and_fakeTQ_df <- function(D) {
  ### cleanse of old TQ metrics
  E = D %>% select(
    -all_of(starts_with("dq_")), -all_of(starts_with("oq_")), -all_of(starts_with("qbq_")),
  )
  # E = D
  names(E)
  ### get TQ metrics
  EE = get_oqot_dqdt_metrics(E, fake_epa0 = TRUE)
  EE1 = get_oqdt_dqot_metrics(EE)
  ### change the names of TQ metrics to reflect them being "fake"
  tq_idx_start = 1 + which(names(EE1) == "fake_epa0") ### last col idx before TQ metrics
  names(EE1)[tq_idx_start:ncol(EE1)] = paste0("fake_", names(EE1)[tq_idx_start:ncol(EE1)])
  names(EE1)
  EE2 = left_join(D,EE1)
  return(EE2)
}

###############
#### LASSO ####
###############

### LASSO
fit_lasso <- function(dataset) {
  # browser()
  # dataset1 = dataset
  dataset1 = dataset %>% drop_na(
    half_seconds_remaining, yardline_100, posteam_timeouts_remaining, score_differential, era_A,
    qbq_ot_0_sum, oq_rot_0_total_sum, dq_dt_0_againstPass_sum, dq_dt_0_againstRun_sum, 
    qbq_dt_0_sum, oq_rdt_0_sum, dq_ot_0_againstPass_sum, dq_ot_0_againstRun_sum, 
    fake_qbq_ot_0_sum, fake_oq_rot_0_total_sum, fake_dq_dt_0_againstPass_sum, fake_dq_dt_0_againstRun_sum, 
    fake_qbq_dt_0_sum, fake_oq_rdt_0_sum, fake_dq_ot_0_againstPass_sum, fake_dq_ot_0_againstRun_sum,
  )
  X1 = model.matrix(terms( ~  
             bs(half_seconds_remaining, df=3, knots=c(30,120))*bs(yardline_100, df=5) + 
             utm:as.numeric(posteam_timeouts_remaining==0) + 
             I((score_differential <= -11)) + ### need a TD
             I((score_differential <= -4)*(game_seconds_remaining <= 900)) + ### need a TD   ### note:: fourth_quarter == game_seconds_remaining <= 900
             I((-3 <= score_differential & score_differential <= 0)*(game_seconds_remaining <= 900)) + ### ok with a field goal
             I((1 <= score_differential & score_differential <= 3)*(game_seconds_remaining <= 900)) + ### prefer a TD but ok with a field goal
             I((4 <= score_differential & score_differential <= 10)*(game_seconds_remaining <= 900)) + ### ok with a field goal but game is still close
             I((score_differential >= 11)) + ### comfortable, field goal is fine
             factor(era_A) +
             qbq_ot_0_sum + 
             oq_rot_0_total_sum +
             dq_dt_0_againstPass_sum +
             dq_dt_0_againstRun_sum +
             qbq_dt_0_sum + 
             oq_rdt_0_sum +
             dq_ot_0_againstPass_sum +
             dq_ot_0_againstRun_sum +
             fake_qbq_ot_0_sum + 
             fake_oq_rot_0_total_sum +
             fake_dq_dt_0_againstPass_sum +
             fake_dq_dt_0_againstRun_sum +
             fake_qbq_dt_0_sum + 
             fake_oq_rdt_0_sum +
             fake_dq_ot_0_againstPass_sum +
             fake_dq_ot_0_againstRun_sum,
            keep.order = TRUE),
           data = dataset1)
  k1 = dim(X1)[2]
  Y1 = dataset1$pts_next_score
  penalties1 = c( rep(0,k1-16), rep(1,16))
  
  #perform k-fold cross-validation to find optimal lambda value
  cv_model1 <- cv.glmnet(X1, Y1, alpha = 1, nfolds=5)
  #find optimal lambda value that minimizes test MSE
  best_lambda1 <- cv_model1$lambda.min
  #find coefficients of best model
  best_model1 <- glmnet(X1, Y1, alpha = 1, penalty.factor =  penalties1, lambda = best_lambda1)
  ###
  clean_lm(best_model1)
}

###################
#### KNOCKOFFS ####
###################

get_knockoffs <- function() {
  ### generate fake EPA0
  D = generate_fake_EPA0(D, ep0_sd=1/20)
  D = D %>% drop_na(fake_epa0)
  ### generate fake TQ
  E = get_TQ_and_fakeTQ_df(D)
  ### make the rows uncorrelated
  E1 = keep_every_kth_row(E, k=5)
  ### fit the LASSO
  m = fit_lasso(E1)
  ### LASSO coefficients
  coeffs = coef(m)
  tq_betas = coeffs[(nrow(coeffs)-16+1):nrow(coeffs),]
  ### W statistics
  W = c(
    abs(tq_betas["qbq_ot_0_sum"]) - abs(tq_betas["fake_qbq_ot_0_sum"]),
    abs(tq_betas["qbq_dt_0_sum"]) - abs(tq_betas["fake_qbq_dt_0_sum"]),
    abs(tq_betas["oq_rot_0_total_sum"]) - abs(tq_betas["fake_oq_rot_0_total_sum"]),
    abs(tq_betas["oq_rdt_0_sum"]) - abs(tq_betas["fake_oq_rdt_0_sum"]),
    abs(tq_betas["dq_dt_0_againstPass_sum"]) - abs(tq_betas["fake_dq_dt_0_againstPass_sum"]),
    abs(tq_betas["dq_ot_0_againstPass_sum"]) - abs(tq_betas["fake_dq_ot_0_againstPass_sum"]),
    abs(tq_betas["dq_dt_0_againstRun_sum"]) - abs(tq_betas["fake_dq_dt_0_againstRun_sum"]),
    abs(tq_betas["dq_ot_0_againstRun_sum"]) - abs(tq_betas["fake_dq_ot_0_againstRun_sum"])
  )
  W = as_tibble(t(W))
  return(W)
}

Ws = tibble()
num_knockoffs = 25
for (i in 1:num_knockoffs) {
  print(paste0("computing knockoff ", i, " of ", num_knockoffs))
  set.seed(2022+i) ### go Rams!
  Wi = get_knockoffs()
  Ws = bind_rows(Ws, Wi)
}
write_csv(Ws, "Ws.csv")

###
Ws = read_csv("Ws.csv")
get_tau <- function(t,W) {
  if ( sum(W >= t) == 0) {
    # NA
    0
  } else {
    sum(W <= -t) / sum(W >= t)
  }
}

# knocked_off_vars = list()
knocked_off_vars = tibble()
for (i in 1:nrow(Ws)) {
# {
  q_ = 0.05
  prop_tib = tibble(t = seq(0.01,1,by=0.001)) %>%
    rowwise() %>%
    mutate(prop = get_tau(t, Ws[i,])) %>%
    mutate(below_q = prop <= q_)
  prop_tib
  tau = (prop_tib %>% filter(below_q) %>% head(1))$t
  knocked_off_i = as_tibble( Ws[i,] < tau )
  knocked_off_vars = bind_rows(knocked_off_vars, knocked_off_i)
}

colMeans(knocked_off_vars) 
write_csv(colMeans(knocked_off_vars), "knockoffs_results.csv")














