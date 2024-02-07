
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


