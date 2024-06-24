
source("A0_header.R")

if(!exists("PRE_LOADED_TrainTestSplitAndTeamQualities")) {
  PRE_LOADED_TrainTestSplitAndTeamQualities = FALSE
}

########################
####### LOAD DATA ######
########################

### full dataset
data_full_0 <- read_csv("data/data4.csv")

if (PRE_LOADED_TrainTestSplitAndTeamQualities) {
  data_full_0 = standardize_team_qualities(data_full_0)
} else {
  data_full_0 =
    data_full_0 %>% 
    select(-c(
      all_of(starts_with("oq")), all_of(starts_with("qbq")), all_of(starts_with("dq"))
    ))
}
names(data_full_0)

################################################################################

keep_just_recent_data <- function(data_full_0, verbose=TRUE) {
  data_full_AA = data_full_0 %>% filter(season >= 2010)
  data_full_AA

  if (verbose) {
    ### dataset descriptors
    print(table(data_full_AA$season))
    print(table(data_full_AA$era_A))
    print(paste("there are",  length(unique(data_full_AA$game_id)), "games in our dataset"))
    print(paste("there are",  length(unique(data_full_AA$epoch)), "epochs in our dataset"))
    print(paste("there are",  length(unique(data_full_AA$Drive)), "drives in our dataset"))
    print(paste("there are",  nrow(data_full_AA), "plays in our dataset"))
    print(paste("there are",  round(nrow(data_full_AA)/length(unique(data_full_AA$epoch)),2), "plays per epoch in our dataset"))
  }
  
  data_full_AA
}

### final dataset: keep just recent data
data_full = keep_just_recent_data(data_full_0)

print(paste(
  "our primary dataset of all plays from", min(data_full$season), "to", max(data_full$season),
  "consists of", nrow(data_full), "plays,",
  "and ", length(unique((data_full$epoch))), "epochs, ",
  "and ", length(unique((data_full$Drive))), "drives."
))

################################################################################

epoch_EP_outcomes = c("Touchdown","Opp_Touchdown","Field_Goal","Opp_Field_Goal","Safety","Opp_Safety","No_Score")
drive_EP_outcomes = c("Touchdown","Field_Goal","No_Score","Opp_Safety","Opp_Touchdown")

map_drive_outcome_to_value = data_full %>% distinct(outcome_drive, pts_end_of_drive) %>% arrange(outcome_drive)
map_drive_outcome_to_value$outcome_drive_str = drive_EP_outcomes
map_epoch_outcome_to_value = data_full %>% distinct(outcome_epoch, pts_next_score) %>% arrange(outcome_epoch)
map_epoch_outcome_to_value$outcome_epoch_str = epoch_EP_outcomes
