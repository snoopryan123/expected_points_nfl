
source("eval_EP_Header.R")

############
### Data ###
############

# SZN = max(data_full$season)
SZN = 2022
SZN

### Data from which to calculate EPA
dfE = data_full %>% filter(season == SZN) %>% select(-ep, -ep0, -epa0) 
### Full PBP data
pbpszn = nflfastR::load_pbp(SZN)

#####################
### EPA functions ###
#####################

get_nflfastR_ep <- function(pbpdata) {
  nflfastR::calculate_expected_points(pbpdata)$ep
}

get_our_ep <- function(
    pbpdata, xgb_model_fit, xgb_model_name, 
    epoch_based_EP=FALSE, drive_based_EP=FALSE, Regression=FALSE, BoundedRegression=FALSE, strip_context=FALSE
) {
  if (strip_context) {
    # [1] "yardline_100"               "half_seconds_remaining"     "era_A"                     
    # [4] "posteam_timeouts_remaining" "half"                       "score_differential"        
    # [7] "down1"                      "down2"                      "down3"                     
    # [10] "down4"                      "ydstogo"                    "posteam_spread"
    pbpdata = pbpdata %>% mutate(
      posteam_spread = 0, score_differential = 0
    )
  } 
  predict_ep_xgb(
    xgb_model_fit, 
    pbpdata, 
    xgb_features = get(paste0(xgb_model_name, "_features")), 
    model_name = xgb_model_name,
    epoch_based_EP=epoch_based_EP, drive_based_EP=drive_based_EP,
    Regression=Regression, BoundedRegression=BoundedRegression
  )$pred
}

create_epa <- function(pbp_data, ep_str) {
  ### from Ben Baldwin's code
  
  epa_str = paste0("epa_", substr(ep_str,4,nchar(ep_str)))
  
  pbp_data %>%
    dplyr::group_by(game_id) %>%
    dplyr::mutate(# Now conditionally assign the EPA, first for possession team
      # touchdowns:
      ep = all_of(ep_str),
      tmp_posteam = .data$posteam
    ) %>%
    tidyr::fill(all_of(ep_str), .direction = "up") %>%
    # tidyr::fill(.data$tmp_posteam, .direction = "up") %>%
    tidyr::fill("tmp_posteam", .direction = "up") %>%
    dplyr::mutate(
      field_goal_made = as.numeric(field_goal_result == "made"),
      extra_point_good = as.numeric(extra_point_result == "good"),
      two_point_conv_good = as.numeric(two_point_conv_result == "good"),
      safety_team = ifelse(safety == 1, posteam, NA)
    ) %>%
    dplyr::mutate(
      # get epa for non-scoring plays
      home_ep = dplyr::if_else(.data$tmp_posteam == .data$home_team, .data[[ep_str]], - .data[[ep_str]]),
      home_epa = dplyr::lead(.data$home_ep) - .data$home_ep,
      epa = dplyr::if_else(.data$tmp_posteam == .data$home_team, .data$home_epa, -.data$home_epa),
      
      # td
      epa = dplyr::if_else(!is.na(.data$td_team),
                           dplyr::if_else(.data$td_team == .data$posteam,
                                          7 - .data[[ep_str]], -7 - .data[[ep_str]]),
                           .data$epa),
      # Offense field goal:
      epa = dplyr::if_else(is.na(.data$td_team) & .data$field_goal_made == 1,
                           3 - .data[[ep_str]], .data$epa, missing = .data$epa),
      # Offense extra-point:
      epa = dplyr::if_else(is.na(.data$td_team) & .data$field_goal_made == 0 &
                             .data$extra_point_good == 1,
                           1 - .data[[ep_str]], .data$epa, missing = .data$epa),
      # Offense two-point conversion:
      epa = dplyr::if_else(is.na(.data$td_team) & .data$field_goal_made == 0 &
                             .data$extra_point_good == 0 &
                             (.data$two_point_conv_good == 1),
                           2 - .data[[ep_str]], .data$epa, missing = .data$epa),
      # Failed PAT (both 1 and 2):
      epa = dplyr::if_else(is.na(.data$td_team) & .data$field_goal_made == 0 &
                             .data$extra_point_good == 0 &.data$two_point_conv_good == 0,
                           0 - .data[[ep_str]], .data$epa, missing = .data$epa),
      # Opponent scores defensive 2 point:
      epa = dplyr::if_else(
        .data$defensive_two_point_conv == 1, -2 - .data[[ep_str]], .data$epa, missing = .data$epa
      ),
      # Safety:
      epa = dplyr::case_when(
        !is.na(.data$safety_team) & .data$safety_team == .data$posteam ~  2 - .data[[ep_str]],
        !is.na(.data$safety_team) & .data$safety_team == .data$defteam ~ -2 - .data[[ep_str]],
        TRUE ~ .data$epa
      )
    ) %>%
    # Create columns with cumulative epa totals for both teams:
    dplyr::mutate(
      # Change epa for plays occurring at end of half with no scoring
      # plays to be just the difference between 0 and starting ep:
      epa = dplyr::if_else(((.data$qtr == 2 &
                               (dplyr::lead(.data$qtr) == 3 |
                                  dplyr::lead(.data$desc) == "END QUARTER 2")) |
                              (.data$qtr == 4 &
                                 (dplyr::lead(.data$qtr) == 5 |
                                    dplyr::lead(.data$desc) == "END QUARTER 4" |
                                    dplyr::lead(.data$desc) == "END GAME"))) &
                             .data$sp == 0 &
                             !is.na(.data$play_type),
                           0 - .data[[ep_str]], .data$epa),
      # last play of OT
      epa = dplyr::if_else(.data$qtr > 4 & dplyr::lead(.data$desc) == "END GAME" & .data$sp == 0,
                           0 - .data[[ep_str]],
                           .data$epa),
      epa = dplyr::if_else(.data$desc == "END QUARTER 2", NA_real_, .data$epa),
      epa = dplyr::if_else(.data$desc == "GAME", NA_real_, .data$epa),
      ep = dplyr::if_else(.data$desc == "END QUARTER 2", NA_real_, .data[[ep_str]]),
      ep = dplyr::if_else(.data$desc == "GAME", NA_real_, .data[[ep_str]])) %>%
    dplyr::ungroup() %>%
    rename(!!epa_str := epa) %>%
    return()
}

######################
### get EP and EPA ###
######################

### modify this dataframe
# dfE1 = dfE
dfE1 = left_join(dfE, pbpszn) %>% select(-ep, -epa) ### join full PBP data
dim(dfE)
dim(dfE1)

### get nflFastR's EP and EPA
dfE1$ep_nflfastR = get_nflfastR_ep(dfE1)
dfE1 = create_epa(dfE1, "ep_nflfastR")
dim(dfE1)
names(dfE1)

### get our bootstrapped EP predictions
model_name = xgb_model_names_list[[1]]
print(model_name)
B = 100 #FIXME
# for (b in 0:0) {
for (b in 0:B) {
  print(paste0("b=",b,"/",B))
  
  ### read b^th EP model
  filename = paste0("fitted_models/","trainedFullModel_",model_name,"_b",b,".rds")
  fit = readRDS(filename)
  
  ###  get b^th EP and EPA predictions
  ep_str = paste0("ep_b",b)
  dfE1[[ep_str]] = get_our_ep(dfE1, fit, model_name, drive_based_EP=TRUE, strip_context=TRUE)
  dfE1 = create_epa(dfE1, ep_str)
}

### check
# names(dfE1)
# names(dfE1)[endsWith(names(dfE1), "nflfastR")]
names(dfE1)[startsWith(names(dfE1), "ep_b")]
names(dfE1)[startsWith(names(dfE1), "epa_b")]
dfE_C1 = dfE1 %>% 
  select(c(
    posteam, season, qb_name, game_id, Drive, pass_or_rush, yardline_100, down, ydstogo, yards_gained, 
    #interception, #fumble_lost, ep_nflfastR, 
    any_of(c("ep_nflfastR", "epa_nflfastR", "ep_b0", "epa_b0")), 
    #all_of(starts_with("ep_b"))
  ))
dfE_C1

### final EPA df
dfEF = dfE1

#################
### Visualize ###
#################

plot_df_1 = 
  dfEF %>% 
  select(c(
    posteam, season, qb_name, pass_or_rush, all_of(starts_with("epa_b"))
  )) %>%
  pivot_longer( all_of(starts_with("epa_b")), names_to="b", values_to="epa") %>%
  mutate(
    b = as.numeric(str_remove(b, "epa_b"))
  )
plot_df_1

plot_df_nflfastR = 
  dfEF %>% 
  select(c(
    posteam, season, qb_name, pass_or_rush, all_of(ends_with("_nflfastR"))
  )) 
plot_df_nflfastR

MIN_PLAYS = 250

plot_df_QB1 = 
  plot_df_1 %>%
  drop_na(pass_or_rush, qb_name, epa) %>%
  select(-pass_or_rush, -posteam) %>%
  group_by(qb_name,season,b) %>%
  summarise(
    epa = sum(epa),
    num_plays = n(),
    epa_per_play = epa/num_plays,
    .groups = "drop"
  ) %>%
  group_by(qb_name,season) %>%
  summarise(
    epa_L = quantile(epa, probs=c(0.025)),
    epa_b0 = first(epa),
    epa_Med = quantile(epa, probs=c(0.50)),
    epa_U = quantile(epa, probs=c(0.975)),
    epa_per_play_L = quantile(epa_per_play, probs=c(0.025)),
    epa_per_play_b0 = first(epa_per_play),
    epa_per_play_Med = quantile(epa_per_play, probs=c(0.50)),
    epa_per_play_U = quantile(epa_per_play, probs=c(0.975)),
    num_plays = unique(num_plays),
    .groups = "drop"
  ) %>%
  filter(num_plays >= MIN_PLAYS) %>%
  arrange(b, -epa_per_play_Med) 
plot_df_QB1

plot_df_QB_nflfastR = 
  plot_df_nflfastR %>%
  drop_na(pass_or_rush, qb_name, epa_nflfastR) %>%
  select(-pass_or_rush, -posteam, -ep_nflfastR) %>%
  group_by(qb_name,season) %>%
  summarise(
    epa = sum(epa_nflfastR),
    num_plays = n(),
    epa_per_play = epa/num_plays,
    .groups = "drop"
  ) %>%
  filter(num_plays >= MIN_PLAYS) %>%
  arrange(-epa_per_play) 
plot_df_QB_nflfastR

plot_df_TM1 = 
  plot_df_1 %>%
  drop_na(pass_or_rush, epa) %>%
  select(-qb_name, -pass_or_rush) %>%
  group_by(posteam,season,b) %>%
  summarise(
    epa = sum(epa),
    num_plays = n(),
    epa_per_play = epa/num_plays,
    .groups = "drop"
  ) %>%
  group_by(posteam,season) %>%
  summarise(
    epa_L = quantile(epa, probs=c(0.025)),
    epa_b0 = first(epa),
    epa_Med = quantile(epa, probs=c(0.50)),
    epa_U = quantile(epa, probs=c(0.975)),
    epa_per_play_L = quantile(epa_per_play, probs=c(0.025)),
    epa_per_play_b0 = first(epa_per_play),
    epa_per_play_Med = quantile(epa_per_play, probs=c(0.50)),
    epa_per_play_U = quantile(epa_per_play, probs=c(0.975)),
    num_plays = unique(num_plays),
    .groups = "drop"
  ) %>%
  arrange(b, -epa_per_play_Med) 
plot_df_TM1

plot_df_TM_nflfastR = 
  plot_df_nflfastR %>%
  drop_na(pass_or_rush, epa_nflfastR) %>%
  select(-pass_or_rush, -qb_name, -ep_nflfastR) %>%
  group_by(posteam,season) %>%
  summarise(
    epa = sum(epa_nflfastR),
    num_plays = n(),
    epa_per_play = epa/num_plays,
    .groups = "drop"
  ) %>%
  arrange(-epa_per_play) 
plot_df_TM_nflfastR

################################################################################

plot_QB_epa_1 = 
  plot_df_QB1 %>%
  ggplot(aes(y=fct_reorder(qb_name, epa_Med))) +
  geom_vline(xintercept = 0, color="gray80", linewidth=0.5) +
  geom_point(aes(x=epa_Med)) +
  geom_errorbar(aes(xmin=epa_L, xmax=epa_U)) +
  ylab("QB") + xlab("EPA") +
  # scale_x_continuous(limits = c(-125, 225)) +
  theme(
    axis.text.y = element_text(size=12), 
    # axis.text.x = element_text(size=15),
    # plot.title = element_text(size=25),
  ) +
  labs(
    title=paste0("QB Total EPA in ", SZN), 
    # subtitle=paste0("(QB's with at least ",MIN_PLAYS," plays)")
  )
# plot_QB_epa_1
ggsave("plot_QB_epa.png", plot_QB_epa_1, width=8, height=10)

plot_QB_epa_per_play_1 = 
  plot_df_QB1 %>%
  ggplot(aes(y=fct_reorder(qb_name, epa_per_play_Med))) +
  geom_vline(xintercept = 0, color="gray80", linewidth=0.5) +
  geom_point(aes(x=epa_per_play_Med)) +
  geom_errorbar(aes(xmin=epa_per_play_L, xmax=epa_per_play_U)) +
  ylab("QB") + xlab("EPA/Play") +
  theme(
    axis.text.y = element_text(size=12), 
    # axis.text.x = element_text(size=15),
    # plot.title = element_text(size=25),
  ) +
  labs(
    title=paste0("QB EPA/Play in ", SZN), 
    # subtitle=paste0("(QB's with at least ",MIN_PLAYS," plays)")
  )
# plot_QB_epa_per_play_1
ggsave("plot_QB_epa_per_play.png", plot_QB_epa_per_play_1, width=8, height=10)

plot_TM_epa_1 = 
  plot_df_TM1 %>%
  ggplot(aes(y=fct_reorder(posteam, epa_Med))) +
  geom_vline(xintercept = 0, color="gray80", linewidth=0.5) +
  geom_point(aes(x=epa_Med)) +
  geom_errorbar(aes(xmin=epa_L, xmax=epa_U)) +
  ylab("Team") + xlab("EPA") +
  # scale_x_continuous(limits = c(-125, 225)) +
  theme(
    axis.text.y = element_text(size=12),
    # axis.text.x = element_text(size=15),
    # plot.title = element_text(size=25),
  ) +
  labs(title=paste0("Offensive Team Total EPA in ", SZN))
# plot_TM_epa_1
ggsave("plot_TM_epa.png", plot_TM_epa_1, width=8, height=10)

plot_TM_epa_per_play_1 = 
  plot_df_TM1 %>%
  ggplot(aes(y=fct_reorder(posteam, epa_per_play_Med))) +
  geom_vline(xintercept = 0, color="gray80", linewidth=0.5) +
  geom_point(aes(x=epa_per_play_Med)) +
  geom_errorbar(aes(xmin=epa_per_play_L, xmax=epa_per_play_U)) +
  ylab("Team") + xlab("EPA/Play") +
  # scale_x_continuous(breaks=seq(-10,10,by=0.1)) +
  theme(
    axis.text.y = element_text(size=12), 
    # axis.text.x = element_text(size=15),
    # plot.title = element_text(size=25),
  ) +
  labs(title=paste0("Offensive Team EPA/Play in ", SZN))
# plot_TM_epa_per_play_1
ggsave("plot_TM_epa_per_play.png", plot_TM_epa_per_play_1, width=8, height=10)

################################################################################

plot_QB_epa_nflfastR = 
  plot_df_QB_nflfastR %>%
  ggplot(aes(y=fct_reorder(qb_name, epa))) +
  geom_vline(xintercept = 0, color="gray80", linewidth=0.5) +
  geom_point(aes(x=epa)) +
  ylab("QB") + xlab("EPA") +
  # scale_x_continuous(limits = c(-125, 225)) +
  theme(
    axis.text.y = element_text(size=12), 
    # axis.text.x = element_text(size=15),
    # plot.title = element_text(size=25),
  ) +
  labs(
    title=paste0("Traditional QB Total EPA in ", SZN), 
    #subtitle=paste0("(QB's with at least ",MIN_PLAYS," plays)")
  )
# plot_QB_epa_nflfastR
ggsave("plot_QB_epa_nflfastR.png", plot_QB_epa_nflfastR, width=8, height=10)

plot_QB_epa_per_play_nflfastR = 
  plot_df_QB_nflfastR %>%
  ggplot(aes(y=fct_reorder(qb_name, epa_per_play))) +
  geom_vline(xintercept = 0, color="gray80", linewidth=0.5) +
  geom_point(aes(x=epa_per_play)) +
  ylab("QB") + xlab("EPA/Play") +
  theme(
    axis.text.y = element_text(size=12), 
    # axis.text.x = element_text(size=15),
    # plot.title = element_text(size=25),
  ) +
  labs(
    title=paste0("Traditional QB EPA/Play in ", SZN), 
    # subtitle=paste0("(QB's with at least ",MIN_PLAYS," plays)")
  )
# plot_QB_epa_per_play_nflfastR
ggsave("plot_QB_epa_per_play_nflfastR.png", plot_QB_epa_per_play_nflfastR, width=8, height=10)

plot_TM_epa_nflfastR = 
  plot_df_TM_nflfastR %>%
  ggplot(aes(y=fct_reorder(posteam, epa))) +
  geom_vline(xintercept = 0, color="gray80", linewidth=0.5) +
  geom_point(aes(x=epa)) +
  ylab("Team") + xlab("EPA") +
  # scale_x_continuous(limits = c(-125, 225)) +
  theme(
    axis.text.y = element_text(size=12), 
    # axis.text.x = element_text(size=15),
    # plot.title = element_text(size=25),
  ) +
  labs(title=paste0("Traditional Offensive Team Total EPA in ", SZN))
# plot_TM_epa_nflfastR
ggsave("plot_TM_epa_nflfastR.png", plot_TM_epa_nflfastR, width=8, height=10)

plot_TM_epa_per_play_nflfastR = 
  plot_df_TM_nflfastR %>%
  ggplot(aes(y=fct_reorder(posteam, epa_per_play))) +
  geom_vline(xintercept = 0, color="gray80", linewidth=0.5) +
  geom_point(aes(x=epa_per_play)) +
  ylab("Team") + xlab("EPA/Play") +
  theme(
    axis.text.y = element_text(size=12), 
    # axis.text.x = element_text(size=15),
    # plot.title = element_text(size=25),
  ) +
  labs(title=paste0("Traditional Offensive Team EPA/Play in ", SZN))
# plot_TM_epa_nflfastR
ggsave("plot_TM_epa_per_play_nflfastR.png", plot_TM_epa_per_play_nflfastR, width=8, height=10)



