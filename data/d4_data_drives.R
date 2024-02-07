
library(tidyverse)

data3 = read_csv("data3.csv")

##############
### DRIVES ###
##############

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
data4 = 
  data3 %>%
  left_join(df5) %>%
  select(-label, -drive)

dim(data3)
dim(data4)
names(data4)
write_csv(data4, "data4.csv")
