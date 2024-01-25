
########################
filewd = getwd()
setwd("..")
source("A1_load_data.R") 
setwd(filewd)
########################

##################################
### plot Team Quality Rankings ###
##################################

rookie_qb_sznz = data_full %>%
  select(qb_name, season) %>%
  distinct(qb_name, season) %>%
  arrange(qb_name, season) %>%
  group_by(qb_name) %>%
  slice_head() %>%
  rename(qb_rookie_szn = season)
rookie_qb_sznz

plot_qbq = data_full %>%
  left_join(rookie_qb_sznz) %>%
  group_by(qb_name) %>%
  filter(qb_play) %>%
  filter(qb_rookie_szn > 2006) %>%
  summarise(mean_qbq = mean(qbq_ot_0_sum), att = n()) %>%
  filter(att >= 2000) %>%
  ggplot() +
  geom_point(aes(x=mean_qbq, y=fct_reorder(qb_name, mean_qbq))) +
  scale_x_continuous(breaks=seq(-10,10,by=0.50)) +
  # xlab("career mean quarterback quality") +
  xlab("career mean quarterback quality\n (>2000 attempts, rookie szn >2006)") +
  ylab("quarterback")
# plot_qbq
ggsave("plot_TQ_qbq.png", plot_qbq, width=7, height=11)

plot_oq_rot = data_full %>%
  filter(season == 2021) %>%
  group_by(posteam, season) %>%
  summarise(mean_oq_rot = mean(oq_rot_0_total_sum)) %>%
  ggplot() +
  geom_point(aes(x=mean_oq_rot, y=fct_reorder(posteam, mean_oq_rot))) +
  scale_x_continuous(breaks=seq(-10,10,by=0.50)) +
  xlab("mean team non-quarterback\n offensive quality in 2021") +
  ylab("team")
# plot_oq_rot
ggsave("plot_TQ_oq_rot.png", plot_oq_rot, width=6, height=9)

plot_dq_dt_againstPass = data_full %>%
  filter(season == 2021) %>%
  group_by(defteam, season) %>%
  summarise(mean_dq_dt = mean(dq_dt_0_againstPass_sum)) %>%
  ggplot() +
  geom_point(aes(x=mean_dq_dt, y=fct_reorder(defteam, -mean_dq_dt))) +
  scale_x_continuous(breaks=seq(-10,10,by=0.50)) +
  xlab("mean team defensive quality\n against the pass in 2021") +
  ylab("team")
# plot_dq_dt_againstPass
ggsave("plot_TQ_dq_dt_againstPass.png", plot_dq_dt_againstPass, width=6, height=9)

plot_dq_dt_againstRun = data_full %>%
  filter(season == 2021) %>%
  group_by(defteam, season) %>%
  summarise(mean_dq_dt = mean(dq_dt_0_againstRun_sum)) %>%
  ggplot() +
  geom_point(aes(x=mean_dq_dt, y=fct_reorder(defteam, -mean_dq_dt))) +
  scale_x_continuous(breaks=seq(-10,10,by=0.50)) +
  xlab("mean team defensive quality\n against the run in 2021") +
  ylab("team")
# plot_dq_dt_againstRun
ggsave("plot_TQ_dq_dt_againstRun.png", plot_dq_dt_againstRun, width=6, height=9)

plot_tq = plot_grid(plot_qbq, plot_oq_rot, plot_dq_dt_againstPass, plot_dq_dt_againstRun, nrow=1)
save_plot(paste0("plot_TQ.png"), plot_tq, base_width=28, base_height=10)
