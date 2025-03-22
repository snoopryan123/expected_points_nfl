
###################
#### load data ####
###################

### load data 
source("../A0_header.R")
data0 <- read_csv("data2.csv")

# USE_PLAY_SUCCESS = FALSE
USE_PLAY_SUCCESS = TRUE

#######################################
##### create EP0 and EPA0 columns #####
#######################################

# data0 = data0 %>% mutate(down_3or4 = down3 + down4)
# fit_ep0_regression_model <- function(dataset) {
#   r.ep0 = lm(
#     pts_next_score ~ 
#       down_3or4 +
#       yardline_100 +
#       log(ydstogo),
#     data = dataset
#   )
#   r.ep0
# }

fit_ep0_regression_model <- function(dataset) {
  ### model from Yurko 2018
  r.ep0 = multinom(label ~ 
                   yardline_100 +
                   factor(down) +
                   log(ydstogo) +
                   half_seconds_remaining +
                   gtg + 
                   utm +
                   posteam_spread ### add this term to mitigate selection bias!
                 ,data = dataset)
  clean_lm(r.ep0)
}

### fit EP0 model 
r.ep0 = fit_ep0_regression_model(data0)
print(r.ep0)

# ### visualize EP0
# {
#   p3_tib = bind_rows(
#     tibble(game_seconds_remaining=0,down_3or4=0,ydstogo=10,yardline_100=1:99),
#     tibble(game_seconds_remaining=0,down_3or4=1,ydstogo=10,yardline_100=1:99),
#   ) %>% mutate(
#     ep0_pred = predict(r.ep0, .),
#   ) 
#   plot_ep0 = p3_tib %>%
#     ggplot() +
#     geom_line(aes(x=(yardline_100), y=(ep0_pred), color=factor(down_3or4)), linewidth=1) +
#     scale_color_brewer(palette="Set1", name="3rd or 4th\ndown") +
#     xlab("yardline") + 
#     ylab(TeX("$EP^{(0)}$")) +
#     # ylab("EPA0") +
#     scale_x_continuous()
#   # plot_ep0
#   # ggsave("plot_ep0.png", plot_ep0, width=8, height=6)
# }

# ep0 <- function(df) { 
#   predict(r.ep0, df)
# }

ep0 <- function(df) {
  df = df %>% mutate(posteam_spread = 0)
  pred_cg = predict(r.ep0, df, "probs")
  pred_ep = pred_cg[,1]*(7) + pred_cg[,2]*(-7) + pred_cg[,3]*(3) + pred_cg[,4]*(-3) + pred_cg[,5]*(2) + pred_cg[,6]*(-2) + pred_cg[,7]*(0)
  unname(pred_ep)
}

### create EP0 column
data0$ep0 = ep0(data0) 

### create EPA0 within each drive
data0 = data0 %>%
  mutate(half = ifelse(qtr == 1 | qtr == 2, 1, 2)) %>%
  relocate(half, .after = game_id) %>%
  group_by(game_id, half, posteam, drive) %>%
  mutate(epa0 = c(diff(ep0), NA)) %>%
  ungroup()

### create EPA0 for scoring plays
data0 = data0 %>%
  mutate(epa0 = ifelse(pts_of_play != 0, pts_of_play - ep0, epa0))

### create EPA0 for the last play of a drive, a non-scoring plays
data0 = data0 %>%
  mutate(epa0 = ifelse(
    ### pos_changes_w is -1 if possession changes at the end of the play and it's not a score
    pos_changes_w == -1 & game_id == lead(game_id, default=""),
    -lead(ep0) - ep0,
    epa0
  ))

### play success
data0$playSuccess0 = as.numeric(data0$epa0 > 0)
# data0 %>% select(epa0, playSuccess0)

# ### check
# View(data0 %>% select(game_id, half, qtr, posteam, drive, yardline_100, ydstogo, 
#      down, passer_player_name, rusher_player_name,
#      pts_of_play, pos_changes_w, ep0, epa0))

### create more helpful columns
data0 = 
  data0 %>%
  group_by(game_id, posteam) %>%
  mutate(
    qb_name = zoo::na.locf(passer_player_name, na.rm = F),
    qb_name = zoo::na.locf(passer_player_name, fromLast = T, na.rm = F)
  ) %>%
  ungroup() %>%
  group_by(posteam) %>%
  mutate(
    kicker_name = zoo::na.locf(kicker_player_name, fromLast = T, na.rm = F),
    kicker_name = zoo::na.locf(kicker_name, na.rm = F),
    punter_name = zoo::na.locf(punter_player_name, fromLast = T, na.rm = F),
    punter_name = zoo::na.locf(punter_name, na.rm = F),
  ) %>%
  ungroup() %>%
  relocate(qb_name, .after=passer_player_name) %>% 
  relocate(kicker_name, .after=kicker_player_name) %>% 
  relocate(punter_name, .after=punter_player_name) %>% 
  mutate(offensive_player_name = case_when(
    !sapply(passer_player_name, is.na) ~ passer_player_name,
    !sapply(rusher_player_name, is.na) ~ rusher_player_name,
    TRUE~NA_character_
  )) %>%
  mutate(qb_play = replace_na(offensive_player_name == qb_name, FALSE)) %>%
  mutate(pass_or_rush = case_when(
    replace_na(pass_attempt == 1, FALSE) ~ "pass",
    replace_na(rush_attempt == 1, FALSE) ~ "run",
    TRUE~NA_character_
  )) %>%
  mutate(row_idx = row_number()) %>% relocate(row_idx, .before = game_id)

# ### check
# View(
#   data0 %>%
#     filter(row_idx %in% 709363:(709363+2000)) %>%
#     select(
#       game_id, row_idx, play_id, posteam, defteam,
#       passer_player_name, rusher_player_name, offensive_player_name, qb_name, qb_play,
#       kicker_player_name, kicker_name, punter_player_name, punter_name
#     )
# )
# View(data0 %>% #filter(row_number() %in% 10000:10200) %>%
#     select(
#       game_id, play_id, season, posteam, defteam, yardline_100, down, ydstogo, epa0,
#       offensive_player_name, pass_or_rush,
#       # passer_player_name, pass_attempt, rusher_player_name, rush_attempt,
# ))

############################################
#### Function -- running quality metric ####
############################################

create_running_quality_metric <- function(epa0_vec, szn_vec, n, N0, alpha, gamma) {
  ### pad with N0 fake zeros
  if (USE_PLAY_SUCCESS) { fake_data_val = 0.35 } else { fake_data_val = 0 }
  epa0_vec = c(rep(fake_data_val, N0), epa0_vec)
  szn_vec = c(rep(first(szn_vec), N0), szn_vec)
  n = N0 + n
  ### create running quality metric
  qbqot = numeric(n)
  for (k in 2:n) {
    epa0_jk = epa0_vec[1:(k-1)]
    alpha_k = alpha**((k-2):0)
    szn_jk = szn_vec[1:(k-1)]
    s_jk = last(szn_jk) - szn_jk
    gamma_k = gamma**s_jk
    ### weighted mean
    top_k = sum(replace_na(alpha_k * gamma_k * epa0_jk, 0))
    bot_k = sum(alpha_k * gamma_k)
    qbqot[k] = top_k / bot_k
    # ### weighted sum
    # top_k = sum(replace_na(alpha_k * gamma_k * epa0_jk, 0))
    # qbqot[k] = top_k 
  }
  ### check
  # plot(qbqot)
  ### remove padding
  qbqot = qbqot[(N0+1):n]
  ### return
  qbqot
}

###
data1a = data0

##############################################
#### Offensive Quality of the Quarterback ####
##############################################

### hyperparams
all_qb_names = unique( (data1a %>% drop_na(passer_player_name, epa0))$passer_player_name )
# j=737
alpha = 0.995
gamma = 0.9
N0 = 250

### quaterback quality
df_qbq_ot = tibble()
for (j in 1:length(all_qb_names)) {
  if (j %% 10 == 0) print( paste0("qbq_ot_0 progress: QB j = ", j, " of n = ", length(all_qb_names), "; alpha = ", alpha, "; gamma = ", gamma) )
  ### player j's data
  player_j = all_qb_names[j]
  data_player_j = data1a %>% filter(qb_play & replace_na(offensive_player_name, "") == player_j & !is.na(epa0)) ### all plays in which the QB was the passer or rusher
  nrow(data_player_j)
  ### relevant columns
  if (USE_PLAY_SUCCESS) { epa0_j = data_player_j$playSuccess0 } else { epa0_j = data_player_j$epa0 }
  szn_j = data_player_j$season
  n_j = length(epa0_j)
  ### create quality metric
  qbqot = create_running_quality_metric(epa0_j, szn_j, n_j, N0, alpha, gamma)
  ### check
  # plot(qbqot)
  ### add column
  data_player_j$qbqot = qbqot
  ### add to dataframe
  df_qbq_ot = bind_rows(
    df_qbq_ot,
    data_player_j %>% select(season, week, row_idx, offensive_player_name, all_of(starts_with("qbqot")))
  )
}
### standardize
df_qbq_ot = 
  df_qbq_ot %>%
  mutate(
    qbqot0 = qbqot,
    qbqot = std(qbqot0)
  )

### check
plot_qbs = c(
  "P.Manning",
  "L.Jackson",
  "C.Wentz",
  "M.Hasselbeck",
  "N.Peterman"
)
plot_qbq_trajs = plot_qbq_trajectories(df_qbq_ot, plot_qbs)
# plot_qbq_trajs
ggsave("../model_comparison/plotting/plot_tq_qbq_trajs.png",plot_qbq_trajs,width=10,height=4)

#########################################
#### Offensive & Defensive Qualities ####
#########################################

### hyperparams
all_teamnames = sort(unique( (data1a %>% drop_na(posteam))$posteam))
# j = 17
alpha = 0.995
gamma = 0.75
N0 = 250

### non-qb offensive & defensive qualities
df_oq_ot = tibble()
df_dq_dt_againstPass = tibble()
df_dq_dt_againstRun = tibble()
for (j in 1:length(all_teamnames)) {
  if ((j-1) %% 4 == 0) print( paste0("tq progress: team j = ", j, " of n = ", length(all_teamnames), "; alpha = ", alpha, "; gamma = ", gamma) )
  
  ### team j
  team_j = all_teamnames[j]
  
  ### create offensive quality metric 
  data_team_j = data1a %>% filter(posteam == team_j & !qb_play & !is.na(pass_or_rush) & !is.na(epa0))
  nrow(data_team_j)
  if (USE_PLAY_SUCCESS) { epa0_j = data_team_j$playSuccess0 } else { epa0_j = data_team_j$epa0 }
  szn_j = data_team_j$season
  n_j = length(epa0_j)
  oqot = create_running_quality_metric(epa0_j, szn_j, n_j, N0, alpha, gamma)
  data_team_j$oqot = oqot
  df_oq_ot = bind_rows(
    df_oq_ot,
    data_team_j %>% select(season, week, row_idx, posteam, all_of(starts_with("oqot")))
  )
  
  ### create defensive quality metric against pass
  data_team_againstPass_j = data1a %>% filter(defteam == team_j & !is.na(pass_or_rush) & !is.na(epa0) & pass_or_rush == "pass")
  nrow(data_team_againstPass_j)
  if (USE_PLAY_SUCCESS) { epa0_j = data_team_againstPass_j$playSuccess0 } else { epa0_j = data_team_againstPass_j$epa0 }
  szn_j = data_team_againstPass_j$season
  n_j = length(epa0_j)
  dqdt_againstPass = create_running_quality_metric(epa0_j, szn_j, n_j, N0, alpha, gamma)
  data_team_againstPass_j$dqdt_againstPass = dqdt_againstPass
  df_dq_dt_againstPass = bind_rows(
    df_dq_dt_againstPass,
    data_team_againstPass_j %>% select(season, week, row_idx, defteam, all_of(starts_with("dqdt")))
  )
  
  ### create defensive quality metric against run
  data_team_againstRun_j = data1a %>% filter(defteam == team_j & !is.na(pass_or_rush) & !is.na(epa0) & pass_or_rush == "run")
  nrow(data_team_againstRun_j)
  if (USE_PLAY_SUCCESS) { epa0_j = data_team_againstRun_j$playSuccess0 } else { epa0_j = data_team_againstRun_j$epa0 }
  szn_j = data_team_againstRun_j$season
  n_j = length(epa0_j)
  dqdt_againstRun = create_running_quality_metric(epa0_j, szn_j, n_j, N0, alpha, gamma)
  data_team_againstRun_j$dqdt_againstRun = dqdt_againstRun
  df_dq_dt_againstRun = bind_rows(
    df_dq_dt_againstRun,
    data_team_againstRun_j %>% select(season, week, row_idx, defteam, all_of(starts_with("dqdt")))
  )
}
### standardize
df_oq_ot = df_oq_ot %>% mutate(oqot0 = oqot, oqot = std(oqot0))
df_dq_dt_againstPass = df_dq_dt_againstPass %>% mutate(dqdt_againstPass0 = dqdt_againstPass, dqdt_againstPass = std(dqdt_againstPass0))
df_dq_dt_againstRun = df_dq_dt_againstRun %>% mutate(dqdt_againstRun0 = dqdt_againstRun, dqdt_againstRun = std(dqdt_againstRun0))

### check
plot_teams = c(
  "LA",
  "TEN"
  # "SEA"
)
plot_oqot_trajs = plot_oqot_trajectories(df_oq_ot, plot_teams, 2015)
# plot_oqot_trajs
ggsave("../model_comparison/plotting/plot_tq_oqot_trajs.png",plot_oqot_trajs,
       width=8,height=4
       # width=10,height=4
)
plot_dqdt_againstRun_trajs = plot_dqdt_againstRun_trajectories(
  df_dq_dt_againstRun, c("BAL","DEN"), min_year=2010
)
# plot_dqdt_againstRun_trajs
ggsave("../model_comparison/plotting/plot_tq_dqdt_againstRun_trajs.png",plot_dqdt_againstRun_trajs,
       width=8,height=4
       # width=10,height=4
)
plot_dqdt_againstPass_trajs = plot_dqdt_againstPass_trajectories(
  df_dq_dt_againstPass, c("SF","DEN"), min_year=2010
)
# plot_dqdt_againstPass_trajs
ggsave("../model_comparison/plotting/plot_tq_dqdt_againstPass_trajs.png",plot_dqdt_againstPass_trajs,
       width=8,height=4
       # width=10,height=4
)

#############################
#### Save OQ, DQ metrics ####
#############################

### gather the team quality metrics
df_qbq_ot %>% select(-qbqot0)
df_oq_ot %>% select(-oqot0)
df_dq_dt_againstPass %>% select(-dqdt_againstPass0)
df_dq_dt_againstRun %>% select(-dqdt_againstRun0)

### save the team quality metrics
data2a = 
  data1a %>% 
  left_join(
    df_qbq_ot %>% select(row_idx, qbqot)
  ) %>% 
  left_join(
    df_oq_ot %>% select(row_idx, oqot)
  ) %>% 
  left_join(
    df_dq_dt_againstPass %>% select(row_idx, dqdt_againstPass)
  ) %>% 
  left_join(
    df_dq_dt_againstRun %>% select(row_idx, dqdt_againstRun)
  ) 
data2a = 
  data2a %>%
  group_by(season, posteam) %>%
  mutate(
    qbqot = zoo::na.locf(qbqot, na.rm = F),
    oqot = zoo::na.locf(oqot, na.rm = F),
    qbqot = replace_na(qbqot, 0),
    oqot = replace_na(oqot, 0),
  ) %>%
  group_by(season, defteam) %>%
  mutate(
    dqdt_againstPass = zoo::na.locf(dqdt_againstPass, na.rm = F),
    dqdt_againstRun = zoo::na.locf(dqdt_againstRun, na.rm = F),
    dqdt_againstPass = replace_na(dqdt_againstPass, 0),
    dqdt_againstRun = replace_na(dqdt_againstRun, 0),
  ) %>%
  ungroup()

dim(data1a)
dim(data2a)
sum(is.na(data2a$qbqot))
sum(is.na(data2a$oqot))
sum(is.na(data2a$dqdt_againstPass))
sum(is.na(data2a$dqdt_againstRun))

# ### check OQ_OP, OQ_OT, QBQ, RBQ
# dim(data1a)
# dim(data2a)
# View(data2a %>% filter(posteam=="ARI") %>%
#        mutate(rrr = as.numeric(!qb_play & posteam=="ARI" &!is.na(pass_or_rush))) %>%
#        select(
#          game_id, play_id, defteam, yardline_100, down, ydstogo,
#          season, posteam, offensive_player_name, pass_or_rush, epa0,
#          oq_ot_0_total_sum,
#          passer_player_name, qb_play, qbq_ot_0_sum, oq_rot_0_total_sum, rrr
#          # all_of(starts_with("oq_op_0")), all_of(starts_with("oq_ot_0"))
# ))
# View(data2a %>% filter(row_number() <= 2000) %>%
#     select(
#       game_id, play_id, defteam, yardline_100, down, ydstogo,
#       season, posteam, offensive_player_name, pass_or_rush, epa0,
#       oq_ot_0_total_sum, passer_player_name, qbq_ot_0_sum, oq_rot_0_total_sum
#       # all_of(starts_with("oq_op_0")), all_of(starts_with("oq_ot_0"))
# ))
# # hist(data2a$oq_op_0_mean)
# # hist(data2a$oq_op_0_sum)
# # hist(data2a$oq_ot_0_total_mean)
# hist(data2a$oq_ot_0_total_sum)
# # data2a %>% filter(rusher_player_name=="T.Gurley" & season==2018) %>%
# #   drop_na(all_of(starts_with("oq_op_0"))) %>% drop_na(all_of(starts_with("oq_ot_0"))) %>%
# #   mutate(i = row_number()) %>% ggplot() +
# #   # geom_line(aes(x=i,y=oq_op_0_mean))
# #   geom_line(aes(x=i,y=oq_op_0_sum))
# data2a %>% filter(posteam=="LA") %>%
#   drop_na(all_of(starts_with("oq_op_0"))) %>% drop_na(all_of(starts_with("oq_ot_0"))) %>%
#   mutate(i = row_number()) %>% ggplot() +
#   # geom_line(aes(x=i,y=oq_ot_0_total_mean))
#   geom_line(aes(x=i,y=oq_ot_0_total_sum))

#################################################
#### Offensive Quality of the Defensive Team ####
#### Defensive Quality of the Offensive Team ####
#################################################

all_teamnames = sort(unique( (data2a %>% drop_na(defteam))$defteam ))
# j = 17; j = 14; j = 26

df_oqdt = tibble()
df_dqot = tibble()
for (j in 1:length(all_teamnames)) {
  print( paste0("oq_dt & dq_ot progress: team j = ", j, " of n = ", length(all_teamnames)) )
  
  team_j = all_teamnames[j]
  data_team_j = data2a %>% filter(posteam == team_j | defteam == team_j) %>%
    select(row_idx, season, posteam, defteam, pass_or_rush, 
           qbqot, oqot, dqdt_againstPass, dqdt_againstRun,
           qb_name, kicker_name, punter_name
     )
  data_team_j
  N_j = nrow(data_team_j)

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
  dqot_againstPass_j = dq_ot_0_j_idx
  dqot_againstPass_j[dqot_againstPass_j != 0] = data_team_j$dqdt_againstPass[dq_ot_0_j_idx]
  
  dqot_againstRun_j = dq_ot_0_j_idx
  dqot_againstRun_j[dqot_againstRun_j != 0] = data_team_j$dqdt_againstRun[dq_ot_0_j_idx]
  ##############################
  qbqdt_j = oq_dt_0_j_idx
  qbqdt_j[qbqdt_j != 0] = data_team_j$qbqot[oq_dt_0_j_idx]
  
  oqdt_j = oq_dt_0_j_idx
  oqdt_j[oqdt_j != 0] = data_team_j$oqot[oq_dt_0_j_idx]
  ##############################
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
  
  data_team_j$qbqdt = qbqdt_j
  data_team_j$oqdt = oqdt_j
  data_team_j$dqot_againstPass = dqot_againstPass_j
  data_team_j$dqot_againstRun = dqot_againstRun_j
  data_team_j$qb_name_dt = qb_name_dt_j

  data_team_j_off = data_team_j %>% filter(posteam == team_j) %>% select(
    row_idx, posteam, dqot_againstPass, dqot_againstRun
  )
  data_team_j_def = data_team_j %>% filter(defteam == team_j) %>% select(
    row_idx, defteam, qbqdt, oqdt, 
    #qb_name_dt
  )
  
  df_oqdt = bind_rows(df_oqdt, data_team_j_def)
  df_dqot = bind_rows(df_dqot, data_team_j_off)
}

###################################
#### Save OQ_DT, DQ_OT metrics ####
###################################

### save the oq, dq metrics
data2c = data2a %>% left_join(df_oqdt) %>% left_join(df_dqot) 

dim(data2a)
dim(data2c)
sum(is.na(data2c$qbqdt))
sum(is.na(data2c$oqdt))
sum(is.na(data2c$dqot_againstPass))
sum(is.na(data2c$dqot_againstRun))

# ### check dims
names(data2c)
# dim(data2a)
# dim(data2c)
# ### check OQ_DT
# View(data2c %>% filter(100000 <= row_number() & row_number() <= 100000+2000) %>%
#        select(
#          row_idx, game_id, play_id, yardline_100, down, ydstogo,
#          # pass_or_rush,
#          season, pass_or_rush, posteam, defteam, passer_player_name,  epa0,
#          dq_dt_0_total_sum, dq_ot_0_sum,
#          oq_ot_0_total_sum, oq_dt_0_sum,
#          oq_rot_0_total_sum, oq_rdt_0_sum,
#          qb_name, qb_name_dt, qbq_ot_0_sum, qbq_dt_0_sum,
#          fg_made, kicker_name, kicker_name_dt, kq0_sum, kq0_dt_sum, 
#          punt_attempt, punter_name, punter_name_dt, pq0_sum, pq0_dt_sum
#          # oq_ot_0_total_mean, oq_dt_0_mean, dq_dt_0_total_mean, dq_ot_0_mean
# ))

##############################################
### create variables for DRIVES and EPOCHS ###
##############################################

###
data3 = data2c 
names(data3)

### select columns relevant to creating `pts_end_of_drive` and `outcome_drive`
df1 = data3 %>% select(row_idx, game_id, qtr, posteam, defteam, drive, 
                       epoch, epoch_length, epoch_weight, label, pts_next_score, pts_of_play)
df1

### create `drive` column to index all drives throughout the dataset
df2a = 
  df1 %>% group_by(game_id, drive) %>% mutate(drive1 = cur_group_id()) %>% 
  ungroup() %>% relocate(drive1, .after=drive)
df2a
df2b = df2a %>% select(-drive) %>% rename(Drive = drive1)
df2b

### create `pts_end_of_drive`
df3 = 
  df2b %>%
  group_by(Drive) %>%
  mutate(pts_end_of_drive = pts_of_play[n()]) %>%
  ungroup()
df3

### create `df_outcome_drive` and `df_outcome_epoch`
df_outcome_epoch = 
  df3 %>% distinct(pts_next_score, label) %>% arrange(label) %>% rename(outcome_epoch = label) 
df_outcome_drive =
  df3 %>% distinct(pts_end_of_drive) %>% arrange(-pts_end_of_drive) %>% mutate(outcome_drive = 0:(n()-1))

df_outcome_epoch
df_outcome_drive

df4 = df3 %>% left_join(df_outcome_epoch) %>% left_join(df_outcome_drive)
df4

### `drive_length` and `drive_weight`
df5 = df4 %>% group_by(Drive) %>% mutate(drive_length = n(), drive_weight = 1/drive_length) %>% ungroup()
df5

### final dataset
data3b = 
  data3 %>%
  left_join(df5) %>%
  select(-label, -drive) %>%
  relocate(drive_length, .after = epoch_length) %>%
  relocate(drive_weight, .after = epoch_weight) %>%
  relocate(outcome_epoch, .after = pts_next_score) %>%
  relocate(outcome_drive, .after = outcome_epoch) %>%
  relocate(Drive, .after = epoch) %>%
  relocate(pts_end_of_drive, .after = pts_next_score) 

dim(data3)
dim(data3b)
names(data3b)

#################################
#### Save our OQ, DQ metrics ####
#################################

### save
write_csv(data3b, "data3.csv")
print("we have successfully created the Team Quality metrics and added them to the dataset `data4`")

