
# Codebase for `A statistical view of expected points models in American football`

# Get play-by-play data
* enter `data` folder
* run `d1_data_acquisition.R` -> output `data_nflFastR_pbp_1999_2022.csv`
* run `d2_data_acquisition.R` -> output `data2.csv`
* run `d3a_data_TeamQualityMetrics_epa0HyperParamTuning.R` to tune the hyperparameters for the 8 hand-crafted team quality metrics
* clear the environment workspace and run `d3b_data_TeamQualityMetrics_epa0.R` -> output `data3.csv`

# Model comparison
* enter `model_comparison` folder
* run `FIXME`

# Plotting
* enter `plotting` folder
* run `A_plot_team_quality.R` to visualize our hand-crafted team quality metrics
* run `A_plot_selection_bias.R` to visualize selection bias induced by not adjusting for team quality
* run `A_plot_summary_stats.R` to visualize some data summary statistics
* run `A_plot_EP.R` to visualize EP models
    * Before visualizing XGB models, need to train and save full XGBoost models via `model_comparison/train_full_models.R`; some of these models should already be saved in the Github

m quality knockoffs
* run `FIXME`


