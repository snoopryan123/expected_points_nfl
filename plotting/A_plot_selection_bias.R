
PRE_LOADED_TrainTestSplitAndTeamQualities = TRUE #FIXME
########################
filewd = getwd()
setwd("..")
source("A1_load_data.R") 
setwd(filewd)
########################

###########################################
### plot selection bias in team quality ###
###########################################

plot_selection_bias0a = data_full %>%
  select(pts_next_score, yardline_100, posteam_spread) %>%
  filter(yardline_100 <= 30) %>%
  # filter(posteam_spread %in% -14:14) %>%
  mutate(
    point_spread_bin = cut(posteam_spread, breaks=c(-30,-3.5,3.5,30))
    # yardline_bin = cut(yardline_100, breaks=seq(0,100,by=10)),
    # point_spread_bin = cut(posteam_spread, breaks=c(-30,-15,-7,-3,0,3,7,14,30))
  ) %>%
  group_by(point_spread_bin) %>%
  summarise(
    avg_next_pts = mean(pts_next_score)
  ) %>%
  # ggplot(aes(x = fct_reorder(point_spread_bin, -avg_next_pts), y = avg_next_pts, )) +
  ggplot(aes(x = point_spread_bin, y = avg_next_pts, )) +
  geom_col(fill="firebrick") +
  ylab("average empirical points\n of the next score") +
  xlab("point spread bin") +
  theme(
    axis.text.x = element_text(size=25, angle = 45, vjust = 1, hjust=1),
    axis.text = element_text(size=35),
    plot.title = element_text(size=40),
    axis.title = element_text(size=40)
  ) +
  labs(title = TeX("$$yardline \\in (0,30]$$"))
# plot_selection_bias0a
ggsave("plot_selection_bias0a.png", plot_selection_bias0a, width=6, height=8)

plot_selection_bias4a = 
  data_full %>%
  select(pts_next_score, yardline_100, posteam_spread) %>%
  filter(yardline_100 <= 30) %>%
  mutate(
    # yardline_bin = cut(yardline_100, breaks=c(0,30,70,100)),
    point_spread_bin = cut(posteam_spread, breaks=c(-30,-3.5,3.5,30))
  ) %>%
  group_by(point_spread_bin) %>%
  summarise(
    count = n()
  ) %>% 
  mutate(freq = count/sum(count)) %>%
  ### plot
  ggplot(aes(x = point_spread_bin, y = freq, )) +
  geom_col(fill="dodgerblue3") +
  ylab("empirical frequency") +
  xlab("point spread bin") +
  theme(
    axis.text.x = element_text(size=25, angle = 45, vjust = 1, hjust=1),
    axis.text = element_text(size=35),
    plot.title = element_text(size=40),
    axis.title = element_text(size=40)
  ) +
  labs(title = TeX("$$yardline \\in (0,30]$$"))
# plot_selection_bias4a
ggsave("plot_selection_bias4a.png", plot_selection_bias4a, width=6, height=8)

################################################################################

plot_selection_bias0 = 
  data_full %>%
  select(pts_next_score, yardline_100, posteam_spread) %>%
  filter(posteam_spread %in% -14:14) %>%
  mutate(
    yardline_bin = cut(yardline_100, breaks=seq(0,100,by=10)),
    point_spread_bin = cut(posteam_spread, breaks=c(-30,-15,-7,-3,0,3,7,14,30))
  ) %>%
  group_by(yardline_bin, point_spread_bin) %>%
  summarise(
    ep = mean(pts_next_score)
  ) %>% 
  mutate(ep_str = round(ep,2)) %>%
  ggplot(aes(x=yardline_bin, y=point_spread_bin)) +
  geom_tile(aes(fill=ep)) +
  geom_text(aes(label=ep_str), color="white", size=5) +
  theme(axis.text.x = element_text(size=13)) +
  guides(fill=guide_legend(title="EP")) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1, size=20)) +
  theme(
    # text = element_text(size=30),
    legend.text = element_text(size=30),
    legend.title = element_text(size=30),
    axis.text = element_text(size=20),
    axis.title = element_text(size=40)
  ) +
  xlab("yards from opponent's endzone") + ylab("point spread bin") 
# plot_selection_bias0
ggsave("plot_selection_bias0.png", plot_selection_bias0, width=9, height=6)


### base-rate empirical avg next pts
data_full %>%
  select(pts_next_score, yardline_100, posteam_spread) %>%
  # filter(yardline_100 <= 30) %>%
  mutate(
    point_spread_bin = cut(posteam_spread, breaks=c(-30,-3.5,3.5,30))
  ) %>%
  group_by(point_spread_bin) %>%
  summarise(
    avg_next_pts = mean(pts_next_score)
  ) %>%
  arrange(-avg_next_pts)

plot_selection_bias1 = 
  data_full %>%
  # filter(season > 2010) %>%
  select(posteam_spread, yardline_100) %>%
  filter(abs(posteam_spread) <= 20) %>%
  group_by(posteam_spread) %>%
  summarise(
    mean_ydl = mean(yardline_100)
  ) %>%
  ggplot(aes(x = posteam_spread, y = mean_ydl)) +
  geom_point() +
  # labs(title="evidence of selection bias") +
  xlab("point spread of the offensive team") +
  ylab("mean yardline") +
  geom_smooth(se=FALSE, method = "lm", color="dodgerblue2", linewidth=1)
# plot_selection_bias1
ggsave("plot_selection_bias1.png", plot_selection_bias1, width=6, height=6)

plot_selection_bias2 = 
  data_full %>%
  # filter(season > 2010) %>%
  select(posteam_spread, yardline_100) %>%
  filter(abs(posteam_spread) <= 20) %>%
  group_by(yardline_100) %>%
  summarise(
    mean_ps = mean(posteam_spread)
  ) %>%
  ggplot(aes(x = yardline_100, y = mean_ps)) +
  geom_point() +
  # labs(title="evidence of selection bias") +
  ylab("mean point spread") +
  xlab("yards from opponent's endzone") +
  geom_smooth(se=FALSE, method = "lm", color="dodgerblue2", size=1)
# plot_selection_bias2
ggsave("plot_selection_bias2.png", plot_selection_bias2, width=6, height=6)

plot_selection_bias3 = 
  data_full %>%
  select(pts_next_score, yardline_100, posteam_spread) %>%
  # filter(posteam_spread %in% -14:14) %>%
  mutate(
    yardline_bin = cut(yardline_100, breaks=seq(0,100,by=10)),
    # point_spread_bin = cut(posteam_spread, breaks=c(-30,-14.5,-6.5,-3.5,-0.5,0.5,3.5,6.5,14.5,30))
    point_spread_bin = cut(posteam_spread, breaks=c(-30,-3.5,3.5,30))
  ) %>%
  group_by(yardline_bin, point_spread_bin) %>%
  summarise(
    count = n()
  ) %>% 
  group_by(yardline_bin) %>%
  mutate(
    freq = count/sum(count)
  ) %>%
  mutate(freq_str = round(freq,2)) %>%
  ggplot(aes(x=yardline_bin, y=point_spread_bin)) +
  geom_tile(aes(fill=freq)) +
  geom_text(aes(label=freq_str), color="white") +
  theme(axis.text.x = element_text(size=13)) +
  guides(fill=guide_legend(title="freq")) +
  xlab("yards from opponent's endzone") + ylab("point spread bin") 
# plot_selection_bias3
ggsave("plot_selection_bias3.png", plot_selection_bias3, width=11, height=6)

plot_selection_bias4 = data_full %>%
  select(pts_next_score, yardline_100, posteam_spread) %>%
  # filter(posteam_spread %in% -14:14) %>%
  mutate(
    # yardline_bin = cut(yardline_100, breaks=seq(0,100,by=10)),
    yardline_bin = cut(yardline_100, breaks=c(0,30,70,100)),
    # point_spread_bin = cut(posteam_spread, breaks=c(-30,-14.5,-6.5,-3.5,-0.5,0.5,3.5,6.5,14.5,30))
    point_spread_bin = cut(posteam_spread, breaks=c(-30,-3.5,3.5,30))
  ) %>%
  group_by(yardline_bin, point_spread_bin) %>%
  summarise(
    count = n()
  ) %>% 
  group_by(yardline_bin) %>%
  mutate(
    freq = count/sum(count),
    lab = paste("yardline ", yardline_bin)
  ) %>%
  mutate(freq_str = round(freq,2)) %>%
  ggplot(aes(x=point_spread_bin, y=freq)) +
  facet_wrap(~lab) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  geom_col(fill="black") + ylab("empirical frequency") +
  theme(
    strip.text.x = element_text(size = 30),
    legend.text = element_text(size=20),
    legend.title = element_text(size=30),
    axis.text = element_text(size=30),
    axis.title = element_text(size=40)
  ) +
  xlab("point spread bin")
# plot_selection_bias4
ggsave("plot_selection_bias4.png", plot_selection_bias4, width=12, height=7)

### base-rate empirical frequency
data_full %>%
  select(pts_next_score, yardline_100, posteam_spread) %>%
  # filter(yardline_100 <= 30) %>%
  mutate(
    point_spread_bin = cut(posteam_spread, breaks=c(-30,-3.5,3.5,30))
  ) %>%
  group_by(point_spread_bin) %>%
  summarise(
    count = n()
  ) %>% 
  mutate(
    freq = count/sum(count),
  ) %>%
  arrange(-point_spread_bin)



