
source("0_clean_lm.R")
data1_a <- read_csv("data1a.csv")
# data1_a = model_data1a
# data1_a <- read_csv("data7a.csv")[,1:82]

#############################
##### create EP0 column #####
#############################

#### create a simple EP model, called EP0,
#### trained only on data from era1, to avoid data-bleed.
data1_a = data1_a %>% mutate(
  # down_1or2 = down1 + down2,
  down_3or4 = down3 + down4,
)

r.ep0 = lm(
  pts_next_score ~ 
    down_3or4 +
    # down2 + down3 + down4 + 
    # game_seconds_remaining + 
    # splines::bs(yardline_100,3) + 
    # splines::bs(log(ydstogo),3), 
    yardline_100 +
    log(ydstogo),
  data = (data1_a %>% filter(era0==1 | era1==1))
)
print(r.ep0)
save_lm(r.ep0, "ep0.rds")

p3_tib = bind_rows(
  tibble(down2=0,down3=0,down4=0,game_seconds_remaining=0,down_3or4=0,
         ydstogo=10,
         yardline_100=1:99),
  tibble(down2=1,down3=0,down4=0,game_seconds_remaining=0,down_3or4=0,
         ydstogo=10,
         yardline_100=1:99),
  tibble(down2=0,down3=1,down4=0,game_seconds_remaining=0,down_3or4=1,
         ydstogo=10,
         yardline_100=1:99),
  tibble(down2=0,down3=0,down4=1,game_seconds_remaining=0,down_3or4=1,
         ydstogo=10,
         yardline_100=1:99)
) 
p3_tib = p3_tib %>% mutate(
  ep0_pred = predict(r.ep0, p3_tib),
  down=factor(ifelse(down2==1,2,ifelse(down3==1,3,ifelse(down4==1,4,1))))
) 
plot_ep0 = p3_tib %>%
  ggplot() +
  geom_line(aes(x=(yardline_100), y=(ep0_pred), color=down)) +
  scale_x_continuous()
# plot_ep0
ggsave("plot_ep0.png", plot_ep0, width=8, height=6)

ep0 <- function(df) { 
  predict(r.ep0, df)
}

data1_a$ep0 = ep0(data1_a) ###

##############################
##### create EPA0 column #####
##############################

### create EPA0 within each drive
data1_a = data1_a %>%
  mutate(half = ifelse(qtr == 1 | qtr == 2, 1, 2)) %>%
  relocate(half, .after = game_id) %>%
  group_by(game_id, half, posteam, drive) %>%
  mutate(epa0 = c(diff(ep0), NA)) %>%
  ungroup()

### create EPA0 for scoring plays
data1_a = data1_a %>%
  mutate(epa0 = ifelse(pts_of_play != 0, pts_of_play - ep0, epa0))

### create EPA0 for the last play of a drive, a non-scoring plays
data1_a = data1_a %>%
  mutate(epa0 = ifelse(
    ### pos_changes_w is -1 if possession changes at the end of the play and it's not a score
    pos_changes_w == -1 & game_id == lead(game_id, default=""),
    -lead(ep0) - ep0,
    epa0
  ))

# ### check
# View(data1_a %>% select(game_id, half, qtr, posteam, drive, yardline_100, ydstogo, down, passer_player_name, rusher_player_name,
#                          pts_of_play, pos_changes_w, ep0, epa0))

#############################################################
##### create EP00 column (for eScoreDiff for WP models) #####
#############################################################

#### create a simple EP model, called EP00,
#### trained only on data from era1, to avoid data-bleed.
r.ep00 = lm(
  pts_next_score ~ 
    bs(yardline_100, df=5) + 
    I((half_seconds_remaining <= 120)*bs(half_seconds_remaining, degree=3)) + 
    I((half_seconds_remaining <= 120)*(posteam_timeouts_remaining==0)) + 
    down2 + down3 + down4 +
    bs(log(ydstogo), df=3),
  data = (data1_a %>% filter(era0==1 | era1==1))
)
print(r.ep00)
save_lm(clean_lm(r.ep00), "ep00.rds")

data1_a$ep00 =  predict(r.ep00, data1_a) 
data1_a = data1_a %>% relocate(ep00, .before = ep0)

###############################
##### create more columns #####
###############################

data1_a = data1_a %>%
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
#   data1_a %>%
#     filter(row_idx %in% 709363:(709363+2000)) %>%
#     select(
#       game_id, row_idx, play_id, posteam, defteam,
#       passer_player_name, rusher_player_name, offensive_player_name, qb_name, qb_play,
#       kicker_player_name, kicker_name, punter_player_name, punter_name
#     )
# )
# View(data1_a %>% #filter(row_number() %in% 10000:10200) %>%
#     select(
#       game_id, play_id, season, posteam, defteam, yardline_100, down, ydstogo, epa0,
#       offensive_player_name, pass_or_rush,
#       # passer_player_name, pass_attempt, rusher_player_name, rush_attempt,
# ))

#########################################################################
##### KICKER QUALITY via FGP0, FGPA0 (field goal probability added) #####
#########################################################################

### FG dataset
fg_df = data1_a %>% 
  drop_na(field_goal_result) %>%
  select(row_idx, field_goal_result, yardline_100, posteam, season, week, kicker_player_name) %>%
  mutate(i = 1:n()) %>% 
  mutate(fg_made = as.numeric(field_goal_result == "made")) %>%
  filter(yardline_100 <= 50) %>%
  mutate(field_goal_attempt = 1)
tail(fg_df)  

### fgp0: probability of a field goal, using data prior to 2005, ignoring kicker quality
fg_model0 = glm(fg_made ~ bs(yardline_100, df=5),     
                data = fg_df %>% filter(season <= 2005), family="binomial")
fg_df = fg_df %>% mutate(
  fgp0 = predict(fg_model0, ., type="response"), ### field goal probability
  fgpa0 = fg_made - fgp0 ### field goal probability added) 
) 

### kicker quality: weighted sum of pa0
all_kickers = unique(fg_df$kicker_player_name)
alpha = 0.995 #
kq0_df = tibble()
for (k in 1:length(all_kickers)) {
  print(paste0("kicker k = ", k, " of ", length(all_kickers)))
  
  fgs_k = fg_df %>% filter(kicker_player_name == all_kickers[k])
  kq0_k_sum = numeric(nrow(fgs_k))
  kq0_k_mean = numeric(nrow(fgs_k))
  n_k = 1 
  for (j in 1:nrow(fgs_k)) {
    if (j > 1) {
      kq0_k_sum[j] = alpha*kq0_k_sum[j-1] + fgs_k$fgpa0[j-1]
      kq0_k_mean[j] = alpha*(1 - 1/n_k)*kq0_k_mean[j-1] + 1/n_k*fgs_k$fgpa0[j-1]
      n_k = n_k + 1
    }
  }
  kq0_df = bind_rows(
    kq0_df,
    fgs_k %>% mutate(kq0_sum=kq0_k_sum, kq0_mean=kq0_k_mean)
  )
}

### plot kicker quality rankings
kq_plot = kq0_df %>% 
  filter(season >= 2006) %>%
  mutate(
    kq0_sum_std = (kq0_sum - mean(kq0_sum))/(2*sd(kq0_sum)),
    kq0_mean_std = (kq0_mean - mean(kq0_mean))/(2*sd(kq0_mean))
  ) %>% 
  group_by(kicker_player_name) %>% 
  filter(first(season)>2006) %>%
  summarise(num_kicks = n(), kq_mean = mean(kq0_sum_std), career_fg_pa0 = sum(fgpa0)) %>% 
  arrange(-kq_mean) %>%
  filter(num_kicks > 100) %>%
  ggplot(aes(y = fct_reorder(kicker_player_name, kq_mean), x = kq_mean)) +
  geom_point() +
  xlab("career mean kicker quality\n (>100 attempts, rookie season >2006)") + ylab("kicker")
# kq_plot
# ggsave("plot_kq.png", kq_plot, width=8, height=12)

### join kq to full datafame
data1aa = left_join(data1_a, kq0_df %>% select(-i))
data1aa = data1aa %>%
  mutate(field_goal_attempt = replace_na(field_goal_attempt, 0)) %>%
  group_by(kicker_name) %>%
  mutate(
    kq0_sum = zoo::na.locf(kq0_sum, fromLast = T, na.rm = F),
    kq0_mean = zoo::na.locf(kq0_mean, fromLast = T, na.rm = F),
    kq0_sum = zoo::na.locf(kq0_sum, na.rm = F),
    kq0_mean = zoo::na.locf(kq0_mean, na.rm = F)
  ) %>%
  ungroup()

# ### check
# sum(is.na(data1aa %>% select(all_of(starts_with("kq")))))
# dim(data1aa)
# View(
#   data1aa %>% select(row_idx, posteam, kicker_name, kicker_player_name, field_goal_result, field_goal_attempt, fg_made, fgp0, kq0_sum, kq0_mean)
# )

#################################################################
##### PUNTER QUALITY via NYA (next yardline above expected) #####
#################################################################

### punt dataset
punt_df = data1aa %>%
  group_by(game_id, half) %>%
  mutate(next_ydl = lead(yardline_100)) %>%
  ungroup() %>%
  drop_na(punter_player_name) %>%
  mutate(punt_attempt=1) %>%
  select(row_idx, yardline_100, next_ydl, posteam, season, week, punter_player_name, punt_attempt) %>% 
  mutate(i = 1:n()) %>%
  drop_na()
punt_df

### eny:: expected next yardline of a punt, using data prior to 2005, ignoring punter quality
punt_model0 = lm(next_ydl ~ bs(yardline_100,3), data = punt_df %>% filter(season <= 2005))
punt_df = punt_df %>%  mutate(
    eny0 = predict(punt_model0, ., type="response"), ### punt expected next yardline
    nyae0 = next_ydl - eny0     ### punt next yardine above expected
  ) 
punt_df

### punter quality: weighted sum of nyae0
all_punters = unique(punt_df$punter_player_name)
alpha = 0.995 #
pq0_df = tibble()
for (k in 1:length(all_punters)) {
  print(paste0("punter k = ", k, " of ", length(all_punters)))
  
  punts_k = punt_df %>% filter(punter_player_name == all_punters[k])
  pq0_k_sum = numeric(nrow(punts_k))
  pq0_k_mean = numeric(nrow(punts_k))
  n_k = 1 
  for (j in 1:nrow(punts_k)) {
    if (j > 1) {
      pq0_k_sum[j] = alpha*pq0_k_sum[j-1] + punts_k$nyae0[j-1]
      pq0_k_mean[j] = alpha*(1 - 1/n_k)*pq0_k_mean[j-1] + 1/n_k*punts_k$nyae0[j-1]
      n_k = n_k + 1
    }
  }
  pq0_df = bind_rows(
    pq0_df,
    punts_k %>% mutate(pq0_sum=pq0_k_sum, pq0_mean=pq0_k_mean)
  )
}

### plot punter quality rankings
pq_plot = pq0_df %>% 
  filter(season >= 2006) %>%
  mutate(
    pq0_sum_std = (pq0_sum - mean(pq0_sum))/(2*sd(pq0_sum)),
    pq0_mean_std = (pq0_mean - mean(pq0_mean))/(2*sd(pq0_mean))
  ) %>% 
  group_by(punter_player_name) %>% 
  filter(min(season)>2006) %>%
  summarise(num_kicks = n(), pq_mean = mean(pq0_sum_std), career_fg_pa0 = sum(nyae0)) %>% 
  arrange(-pq_mean) %>%
  filter(num_kicks >= 250) %>%
  ggplot(aes(y = fct_reorder(punter_player_name, pq_mean), x = pq_mean)) +
  geom_point() +
  xlab("career mean punter quality\n (>250 attempts, rookie season >2006)") + ylab("punter")
# pq_plot
# ggsave("plot_pq.png", pq_plot, width=8, height=12)

### join kq to full datafame
data1aaa = left_join(data1aa, pq0_df %>% select(-i))
data1aaa = data1aaa %>%
  mutate(punt_attempt = replace_na(punt_attempt, 0)) %>%
  group_by(punter_name) %>%
  mutate(
    pq0_sum = zoo::na.locf(pq0_sum, fromLast = T, na.rm = F),
    pq0_mean = zoo::na.locf(pq0_mean, fromLast = T, na.rm = F),
    pq0_sum = zoo::na.locf(pq0_sum, na.rm = F),
    pq0_mean = zoo::na.locf(pq0_mean, na.rm = F)
  ) %>%
  ungroup()

# ### check
# sum(is.na(data1aa %>% select(all_of(starts_with("pq")))))
# dim(data1aaa)
# View(
#   data1aaa %>% select(row_idx, posteam, punter_name, punt_attempt, punter_player_name, yardline_100, next_ydl, eny0, nyae0, pq0_sum, pq0_mean)
# )

##############################################
#### Offensive Quality of the Quartberack ####
##############################################

###
data1a = data1aaa

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
  epa0_player_j = data_player_j$epa0
  
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

#############################################################################
#### Offensive Quality of the Offensive Team (for non-Quarterback plays) ####
#############################################################################

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
  epa0_team_j = data_team_j$epa0
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

###################################
#### Save OQ_OP, OQ_OT metrics ####
###################################

### save the oq, dq metrics
# data2a = data1a %>% left_join(oq_op_0) %>% left_join(oq_ot_0)
data2a = data1a %>% left_join(oq_ot_0) %>% left_join(qbq_ot_0) 
data2a = data2a %>%
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
#### Defensive Quality of the Defensive Team ####
#################################################

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
  
  data_team_j = data2a %>% filter(defteam == team_j) %>% select(row_idx, season, defteam, pass_or_rush, epa0)
  ## View(data_team_j)
  szn_begins_team_j = c(1, diff(data_team_j$season))
  # View(tibble(season = data_team_j$season, szn_begins = szn_begins_team_j)) ### check
  epa0_team_j = data_team_j$epa0
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

############################
#### Save DQ_DT metrics ####
############################

### save the dq, dt metrics
data2b = data2a %>% left_join(dq_dt_0) %>% ungroup()

# ### check dims
# dim(dq_dt_0)
# dim(data2a)
# dim(data2b)
# ### plots
# hist(data2b$dq_dt_0_total_sum)
# data2b %>% filter(posteam=="NYJ") %>% filter(season==2018) %>%
#   drop_na(all_of(starts_with("dq_dt_0"))) %>%
#   mutate(i = row_number()) %>% ggplot() +
#   # geom_line(aes(x=i,y=dq_dt_0_againstPass_mean))
#   # geom_line(aes(x=i,y=dq_dt_0_againstPass_sum))
#   # geom_line(aes(x=i,y=dq_dt_0_againstRun_mean))
#   # geom_line(aes(x=i,y=dq_dt_0_againstRun_sum))
#   # geom_line(aes(x=i,y=dq_dt_0_total_mean))
#   geom_line(aes(x=i,y=dq_dt_0_total_sum))
# # geom_line(aes(x=i,y=oq_ot_0_total_sum))
# ### check DQ_DT_total
# View(data2b %>% filter(row_number() <= 2000) %>% select(
#   row_idx, game_id, play_id, posteam, yardline_100, down, ydstogo,
#   season, defteam, epa0, pass_or_rush,
#   dq_dt_0_total_sum, #dq_dt_0_total_mean
# ))
# # ### check DQ_DT_againstRun
# # View(data2b %>% filter(row_number() <= 2000) %>% filter(pass_or_rush=="run") %>% select(
# #        row_idx, game_id, play_id, posteam, yardline_100, down, ydstogo,
# #        season, defteam, epa0, pass_or_rush,
# #        dq_dt_0_againstRun_sum, dq_dt_0_againstRun_mean
# # ))
# # ### check DQ_DT_againstPass
# # View(data2b %>% filter(row_number() <= 2000) %>% filter(pass_or_rush=="pass") %>% select(
# #   row_idx, game_id, play_id, posteam, yardline_100, down, ydstogo,
# #   season, defteam, epa0, pass_or_rush,
# #   dq_dt_0_againstPass_sum, dq_dt_0_againstPass_mean
# # ))
# # ### check DQ_DT_combined
# # View(data2b %>% filter(row_number() <= 2000) %>% select(
# #   row_idx, game_id, play_id, posteam, yardline_100, down, ydstogo,
# #   season, defteam, epa0, pass_or_rush,
# #   dq_dt_0_combined_sum, dq_dt_0_againstPass_sum, dq_dt_0_againstRun_sum, dq_dt_0_total_sum
# #   # dq_dt_0_combined_mean, dq_dt_0_againstPass_mean, dq_dt_0_againstRun_mean
# # ))
# # ### check DQ_DT_ overall
# # View(data2b %>% filter(row_number() <= 2000) %>%
# #        select(
# #          row_idx, game_id, play_id, posteam, yardline_100, down, ydstogo,
# #          season, defteam, epa0, pass_or_rush,
# #          all_of(starts_with("dq_dt_0")))
# # )

#################################################################################################################
#################################################################################################################

#################################################
#### Offensive Quality of the Defensive Team ####
#### Defensive Quality of the Offensive Team ####
#################################################

all_teamnames = sort(unique( (data2b %>% drop_na(defteam))$defteam ))
# j = 17; j = 14; j = 26

oq_dt_0 = tibble()
dq_ot_0 = tibble()
kqpq_dt_0 = tibble()
for (j in 1:length(all_teamnames)) {
  print( paste0("oq_dt_0 & dq_ot_0 progress: team j = ", j, " of n = ", length(all_teamnames)) )
  
  team_j = all_teamnames[j]
  data_team_j = data2b %>% filter(posteam == team_j | defteam == team_j) %>%
    select(row_idx, season, posteam, defteam, pass_or_rush, 
           oq_ot_0_total_sum, dq_dt_0_total_sum, oq_ot_0_total_mean, dq_dt_0_total_mean,
           dq_dt_0_againstPass_sum, dq_dt_0_againstRun_sum,
           qbq_ot_0_sum, qbq_ot_0_mean, oq_rot_0_total_sum, oq_rot_0_total_mean,
           kq0_sum, kq0_mean, pq0_sum, pq0_mean,
           qb_name, kicker_name, punter_name
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
  
  ##############################
  kq0_dt_sum_j = oq_dt_0_j_idx
  kq0_dt_sum_j[kq0_dt_sum_j != 0] = data_team_j$kq0_sum[oq_dt_0_j_idx]
  
  pq0_dt_sum_j = oq_dt_0_j_idx
  pq0_dt_sum_j[pq0_dt_sum_j != 0] = data_team_j$pq0_sum[oq_dt_0_j_idx]
  
  ##############################
  kicker_name_dt_j = oq_dt_0_j_idx
  kicker_name_dt_j[kicker_name_dt_j != 0] = data_team_j$kicker_name[oq_dt_0_j_idx]
  
  punter_name_dt_j = oq_dt_0_j_idx
  punter_name_dt_j[punter_name_dt_j != 0] = data_team_j$punter_name[oq_dt_0_j_idx]
  
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
  
  data_team_j$kq0_dt_sum = kq0_dt_sum_j
  data_team_j$pq0_dt_sum = pq0_dt_sum_j
  
  data_team_j$kicker_name_dt = kicker_name_dt_j
  data_team_j$punter_name_dt = punter_name_dt_j
  data_team_j$qb_name_dt = qb_name_dt_j

  data_team_j_off = data_team_j %>% filter(posteam == team_j) %>% select(
    row_idx, posteam, all_of(starts_with("dq_ot_0"))
  )
  data_team_j_def = data_team_j %>% filter(defteam == team_j) %>% select(
    row_idx, defteam, all_of(starts_with("oq_dt_0")), all_of(starts_with("qbq_dt_0")), all_of(starts_with("oq_rdt_0")), 
    all_of(starts_with("kq0_dt")), all_of(starts_with("pq0_dt")), kicker_name_dt, punter_name_dt, qb_name_dt
  )
  
  oq_dt_0 = bind_rows(oq_dt_0, data_team_j_def)
  dq_ot_0 = bind_rows(dq_ot_0, data_team_j_off)
}

###################################
#### Save OQ_DT, DQ_OT metrics ####
###################################

### save the oq, dq metrics
data2c = data2b %>% left_join(oq_dt_0) %>% left_join(dq_ot_0) 

# ### check dims
# dim(data2b)
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


#################################
#### Save our OQ, DQ metrics ####
#################################

### ### remove rows that we won't use, to save space
### data5a = data2c %>% select(-all_of(ends_with("_mean")))

data7b = data2c 

### save
write_csv(data7b, "data7b.csv")
print("done.")
