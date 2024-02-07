
# Codebase for `[working title: A statistical view of expected points models in American football]`

# Get play-by-play data
* download `data4.csv` from https://upenn.box.com/s/kp43egvarlv6bsgbxuchnbc9fcj9iave and put in `data/` folder
* you can also generate the dataset from code:
  * enter `data` folder
  * run `d1_data_acquisition.R` -> output `data_nflFastR_pbp_1999_2022.csv`
  * run `d2_data_acquisition.R` -> output `data2.csv`
  * run `d3a_data_TeamQualityMetrics_epa0HyperParamTuning.R` to tune the hyperparameters for the 8 hand-crafted team quality metrics
  * clear the environment workspace and run `d3b_data_TeamQualityMetrics_epa0.R` -> output `data3.csv`, which contains one initial train/test split column and 8 hand-crafted team quality metrics built from EPA0 fit from this initial training set.
  * run `d4_data_drives.R` -> output `data4.csv`

# Model comparison
* enter `model_comparison` folder
* tune XGB (XGBoost) params: run `param_tuning.R` parallelized on a cluster via `run_param_tuning.sh`, then transfer the outputted `.yaml` files (which store the tuned params) from the folder `param_tuning_results` into the folder `param_tuning_results_FINAL`
* test models: `FIXME`
* train and save models on the full dataset: `FIXME`
 
<!--
* test XGB models: run `test_predPerf_XGB.R` (on a cluster via `run_test_XGB.sh`)
* test MLR (multinomial logistic regression) models: run `test_predPerf_MLR.R` (on a cluster via `run_test_MLR.sh`)
* train and save XGB models on the full dataset: run `train_fullXGBModels.R` parallelized on a cluster via `run_train_fullXGBModels.sh`, which outputs `.xgb` files that store the fitted the models
-->

# Plots/visualizations
* enter `plotting` folder
* run `A_plot_EP.R` to visualize EP models
    * Before visualizing XGB models, need to train and save full XGBoost models via `model_comparison/train_full_models.R`; some of these models should already be saved in the Github
* run `A_plot_team_quality.R` to visualize our hand-crafted team quality metrics
* run `A_plot_selection_bias.R` to visualize selection bias induced by not adjusting for team quality
* run `A_plot_summary_stats.R` to visualize some data summary statistics

<!--
# Team quality knockoffs
* enter `team_quality_knockoffs` folder
* run `team_quality_knockoffs.R` (on a cluster via `run_team_quality_knockoffs.sh`)
-->

