
PRE_LOADED_TrainTestSplitAndTeamQualities = TRUE #FIXME
########################
filewd = getwd()
setwd("..")
source("A1_load_data.R") 
setwd(filewd)
########################

###########################################
### distribution in epoch size ###
###########################################

epoch_sizes_df = 
  data_full_0A %>%
  select(game_id, play_id, half_seconds_remaining, epoch) %>%
  group_by(game_id, epoch) %>%
  summarise(
    count = n(), 
    hsr_start = max(half_seconds_remaining),
    hsr_med = median(half_seconds_remaining),
    hsr_end = min(half_seconds_remaining),
    .groups = "drop"
  ) 
epoch_sizes_df

epoch_sizes_df %>%
  ggplot() +
  geom_histogram(aes(x = count), fill="black")

epoch_sizes_df %>%
  mutate(hsr_med_bin = cut(hsr_med, breaks=seq(0,1800,by=30))) %>%
  group_by(hsr_med_bin) %>%
  summarise(mean_num_plays = mean(count)) %>%
  ggplot() +
  geom_point(aes(y = hsr_med_bin, x = mean_num_plays))



