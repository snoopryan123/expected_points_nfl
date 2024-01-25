
source("A0_header.R")

########################
####### LOAD DATA ######
########################

### full dataset
data_full_0 <- read_csv("data/data3.csv")

### the pointspread from nflFastR is flipped, so flip it back!
data_full_0 <- data_full_0 %>% mutate(posteam_spread = -posteam_spread)

### add num plays in epoch, and epoch weight w = 1/# plays in epoch
data_full_0 <- 
  data_full_0 %>% 
  group_by(epoch) %>%
  mutate(
    epoch_length = n(),
    w = 1/epoch_length
  )

### standardize some covariates 
# std <- function(x, mu, sigma) { (x-mu) / (2*sigma) }
# std <- function(x) { (x-mean(x, na.rm=TRUE)) / (2*sd(x, na.rm=TRUE)) }
# std <- function(x) { (x-mean(x)) / (2*sd(x)) }
# std <- function(x) { (x-mean(x)) }
# std <- function(x) { x / sd(x) }

std <- function(x) { (x-mean(x)) / sd(x) }

### standardizing the columns as-is 
data_full_0A = data_full_0
data_full_0A$qbq_ot_0_sum = std(data_full_0A$qbq_ot_0_sum)
data_full_0A$oq_rot_0_total_sum = std(data_full_0A$oq_rot_0_total_sum)
data_full_0A$dq_dt_0_againstRun_sum = std(data_full_0A$dq_dt_0_againstRun_sum)
data_full_0A$dq_dt_0_againstPass_sum = std(data_full_0A$dq_dt_0_againstPass_sum)
data_full_0A$qbq_dt_0_sum = std(data_full_0A$qbq_dt_0_sum)
data_full_0A$oq_rdt_0_sum = std(data_full_0A$oq_rdt_0_sum)
data_full_0A$dq_ot_0_againstRun_sum = std(data_full_0A$dq_ot_0_againstRun_sum)
data_full_0A$dq_ot_0_againstPass_sum = std(data_full_0A$dq_ot_0_againstPass_sum)

data_full_0A = 
  data_full_0A %>% 
  mutate(
    posteam_coach = ifelse(home == 1, home_coach, away_coach),
    posteam_spread_std = std(posteam_spread),
    utm = as.numeric(half_seconds_remaining <= 120),
    gtg = (yardline_100 <= 10),
    era_A = case_when(era0==1~0, era1==1~1, era2==1~2, era3==1~3, era4==1~4, TRUE~NA_real_)
  )  

sum(is.na(data_full_0A$qbq_ot_0_sum))
sum(is.na(data_full_0A$oq_rot_0_total_sum))
sum(is.na(data_full_0A$dq_dt_0_againstRun_sum))
sum(is.na(data_full_0A$dq_dt_0_againstPass_sum))
sum(is.na(data_full_0A$qbq_dt_0_sum))
sum(is.na(data_full_0A$oq_rdt_0_sum))
sum(is.na(data_full_0A$dq_ot_0_againstRun_sum))
sum(is.na(data_full_0A$dq_ot_0_againstPass_sum))

### remove era0 and era1
data_full_AA = data_full_0A %>% filter(era0 != 1 & era1 != 1) 
### remove non-pass and non-rush plays
data_full_A = data_full_AA %>% drop_na(pass_or_rush)

### final dataset
data_full = data_full_A

### dataset descriptors
print(paste("there are",  length(unique(data_full$game_id)), "games in our dataset"))
print(paste("there are",  length(unique(data_full$epoch)), "epochs in our dataset"))
print(paste("there are",  nrow(data_full), "plays in our dataset"))
print(paste("there are",  round(nrow(data_full)/length(unique(data_full$epoch)),2), "plays per epoch in our dataset"))

