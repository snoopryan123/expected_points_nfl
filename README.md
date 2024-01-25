
# Codebase for `A statistical view of expected points models in American football`

# Get play-by-play data
* enter `data` folder
* run `d1_data_acquisition.R` -> output `data_nflFastR_pbp_1999_2022.csv`
* run `d2_data_acquisition.R` -> output `data2.csv`
* run `d3a_data_TeamQualityMetrics_epa0HyperParamTuning.R` to tune the hyperparameters for the 8 hand-crafted team quality metrics
* clear the environment workspace and run `d3b_data_TeamQualityMetrics_epa0.R` -> output `data3.csv`

# Model comparison
* enter `model_comparison` folder
* run 

# Plotting
* enter `plotting` folder
* run `A_plot_team_quality.R` to visualize our hand-crafted team quality metrics
* run `A_plot_selection_bias.R` to visualize selection bias induced by not adjusting for team quality
* run `A_plot_summary_stats.R` to visualize some data summary statistics
* run `A_plot_EP.R` to visualize EP models




# OLD:

## Model and data visualization
* Visualize the EP models in `comparison_v110/2_plotting/A_plot_EP.R`
  * Before doing so, need to train and save full XGBoost models via `comparison_v110/3_model_selection/xgb_110/train_full_models.R`, but some of these models should already be saved
* selection bias plots, team quality plots, and other plots of the data in `comparison_v110/2_plotting/A_plot_TQ.R`

## Model bake-off (out-of-sample predictive performance comparison)
* for `EP Paper`:
  * test OLS models: run `comparison_v110/3_model_selection/ols_110/test_ep_allDowns.R`
  * test MLR models: run `comparison_v110/3_model_selection/mlr_110/test_ep_allDowns.R`
  * test XGB models: run `comparison_v110/3_model_selection/xgb_110/test_ep_allDowns.R`
* __XGBoost model tuning:__
  * XGBoost:
    * tune XGB models: run `comparison_v110/3_model_selection/xgb_110/param_tuning.R` via `run_param_tuning_xgb_EP_AJ.sh` and `run_param_tuning_xgb_WP_AJ.sh`
    * final tuning parameters to be used in XGBoost model fitting found in `param_tuning_results_FINAL`
    * these tuned params are already included in the github repo
  * Catalytic XGBoost:
    * tune Catalytic XGB models: run `comparison_v110/3_model_selection/xgb_110/param_tuning_catalytic.R` via `run_param_tuning_catalytic_EP_AJ.sh` and `run_param_tuning_catalytic_WP_AJ.sh`
    * final tuning parameters to be used in Catalytic XGBoost model fitting found in `param_tuning_results_FINAL`
    * these tuned params are already included in the github repo

## Team quality knockoffs in `EP Paper`
* run `comparison_v110/6_knockoffs_TQ/knockoffs_EPA0tq.R`


