
# Codebase for `A statistical view of expected points models in American football`

# Get play-by-play data
* download `data3.csv` from https://upenn.box.com/s/kp43egvarlv6bsgbxuchnbc9fcj9iave and put in `data/` folder
* you can also generate the dataset from code:
  * enter `data` folder
  * run `d1_data_acquisition.R` -> output `data_nflFastR_pbp_1999_2022.csv`
  * run `d2_data_acquisition.R` -> output `data2.csv`
  * run `d3_data_TeamQualityMetrics.R` -> output `data3.csv`, which generates the 8 hand-crafted team quality metrics 

# Model comparison
* enter `model_comparison` folder
* tune XGBoost hyperparameters: run `C_param_tuning.R` parallelized on a cluster via `R_run_param_tuning.sh`, then transfer the outputted `.yaml` files (which store the tuned params) from the folder `param_tuning_results` into the folder `param_tuning_results_FINAL`
  * the saved `.yaml` files that store the tuned XGB hyperparameters should already be in `param_tuning_results_FINAL`
* train all the various bootstrapped models on the training set for testing: run `D_eval_EP_TrainModels.R` parallelized on a cluster via `R_run_train_epochEP_models_for_testing_AJ.sh` and store the fitted models in `fitted_models/`

<!--
* evaluate EP models (prediction accuracy): run `eval_EP_models.R` (on a cluster via `run_eval_driveEP_models.sh`) -> output
 `FIXME`
* train and save models on the full dataset: `FIXME`
* for the catalytic modeling results on local machine: set params in `eval_EP_Header.R`, train in `eval_EP_TrainModels.R`, and eval in `eval_EP_EvalLosses.R` 
 

# Plots/visualizations
* enter `plotting` folder
* run `A_plot_EP.R` to visualize EP models
    * Before visualizing XGB models, need to train and save full XGBoost models via `model_comparison/train_full_models.R`; some of these models should already be saved in the Github
* run `A_plot_team_quality.R` to visualize our hand-crafted team quality metrics
* run `A_plot_selection_bias.R` to visualize selection bias induced by not adjusting for team quality
* run `A_plot_summary_stats.R` to visualize some data summary statistics
-->

