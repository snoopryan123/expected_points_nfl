
library(tidyverse)
library(xgboost) 
library(randomForest)
library(splines)
library(matrixcalc)
library(lme4)
# library(nflfastR) 
library(nflreadr) 
library(nflplotR)
library(grid)
library(nnet)
library(gridExtra)
library(loo)
library(latex2exp)
library(RColorBrewer)
library(cowplot)
library(dials)
library(rlist)
library(gam)
library(ggnewscale)
library(abind)
library(reshape2)
#####library(gt)
#####library(webshot2)
# this should be the default u y do this R
options(scipen = 999999)
# options(scipen = 50)
output_folder = "./plots/"
theme_set(theme_bw())
# theme_update(text = element_text(size=16))
# theme_update(plot.title = element_text(hjust = 0.5))
theme_update(
  text = element_text(size=20),
  plot.title = element_text(hjust = 0.5),
  axis.title = element_text(size=20),
  axis.text = element_text(size=20),
  legend.text = element_text(size=20),
  legend.title = element_text(size=20)
) 
options(pillar.sigfig=4)

### install.packages("gt",repos = "http://cran.us.r-project.org")

# ################################################################################
# ### installing packages (on HPC3)
# myPKGs <- c(
#   "tidyverse",
#   "xgboost",
#   "randomForest",
#   "splines",
#   "matrixcalc",
#   "lme4",
###   "nflfastR",
#   "nflreadr",
#   "grid",
#   "nnet",
#   "gridExtra",
#   "loo",
#   "latex2exp",
#   "RColorBrewer",
#   "cowplot",
#   "dials",
#   "rlist",
#   "gam",
#   "ggnewscale"
# )
# # check to see if each package is installed, and if not add it to a list to install
# InstalledPKGs <- names(installed.packages()[,'Package'])
# InstallThesePKGs <- myPKGs[!myPKGs %in% InstalledPKGs]
# # install any needed packages
# if (length(InstallThesePKGs) > 0) install.packages(InstallThesePKGs)
# ################################################################################

############################################
### Saving LM, GLM, and MULTINOM objects ###
############################################

compute_scoreTimeRatio <- function(score_differential, game_seconds_remaining) {
  score_differential/(game_seconds_remaining + 0.01)
}

clean_lm <- function(cm) {
  cm$residuals = c()
  cm$fitted.values = c()
  cm$effects = c()
  cm$qr$qr = c()
  cm$linear.predictors = c()
  cm$weights = c()
  cm$prior.weights = c()
  cm$data = c()
  
  cm$family$variance = c()
  cm$family$dev.resids = c()
  cm$family$aic = c()
  cm$family$validmu = c()
  cm$family$simulate = c()
  cm$model = c()
  
  cm
}

save_lm <- function(cm, filename) {
  attr(cm$terms, ".Environment") <- NULL
  attr(cm$formula,".Environment") <- NULL
  saveRDS(cm, filename)
}

load_lm <- function(filename) {
  cm = readRDS(filename)
  attr(cm$terms, ".Environment") <- globalenv()
  if ( any(str_detect(class(cm), "glm")) ) {
    attr(cm$formula, ".Environment") <- globalenv()
  }
  return(cm)
}

###############################
### standardizing functions ###
###############################

std <- function(x) { (x-mean(x)) / sd(x) }

standardize_team_qualities <- function(dataset) {
  ### standardizing the columns as-is 
  dataset$qbq_ot_0_sum = std(dataset$qbq_ot_0_sum)
  dataset$oq_rot_0_total_sum = std(dataset$oq_rot_0_total_sum)
  dataset$dq_dt_0_againstRun_sum = std(dataset$dq_dt_0_againstRun_sum)
  dataset$dq_dt_0_againstPass_sum = std(dataset$dq_dt_0_againstPass_sum)
  dataset$qbq_dt_0_sum = std(dataset$qbq_dt_0_sum)
  dataset$oq_rdt_0_sum = std(dataset$oq_rdt_0_sum)
  dataset$dq_ot_0_againstRun_sum = std(dataset$dq_ot_0_againstRun_sum)
  dataset$dq_ot_0_againstPass_sum = std(dataset$dq_ot_0_againstPass_sum)
  
  # browser()
  sum(is.na(dataset$qbq_ot_0_sum))
  sum(is.na(dataset$oq_rot_0_total_sum))
  sum(is.na(dataset$dq_dt_0_againstRun_sum))
  sum(is.na(dataset$dq_dt_0_againstPass_sum))
  sum(is.na(dataset$qbq_dt_0_sum))
  sum(is.na(dataset$oq_rdt_0_sum))
  sum(is.na(dataset$dq_ot_0_againstRun_sum))
  sum(is.na(dataset$dq_ot_0_againstPass_sum))
  
  dataset
}

###################################
######### Plot Functions ##########
###################################

gradient1 = c(brewer.pal(name="PuRd",n=9)[3:9], rev(brewer.pal(name="Blues",n=9)[3:8]))

smooth_me <- function(df) {
  v_y = df$v[1:99]
  #v_spline = smooth.spline(99:1, v_y)
  v_spline = smooth.spline(99:1, v_y,df=8)
  rev(v_spline$y)
  temp = tibble(yardline_100=1:99,v_smoothed=rev(v_spline$y))
  left_join(df,temp, by = "yardline_100") 
}

smooth_me2 <- function(yardline_100, v, df=8) {
  v_spline = smooth.spline(yardline_100, v, df=df)
  v_spline$y
}

### plot our field goal probability model
plot_fg_prob_by_kq <- function(fg_model, kq=NULL) {
  kq_breaks = seq(-1,1,length=7)
  fg_plot_df = tibble()
  if (is.null(kq)) {
    for (kq in kq_breaks) {
      fg_plot_df = bind_rows(fg_plot_df, tibble(yardline_100 = 1:99, kq_0_sum_std = kq))
    }
  } else {
    fg_plot_df = tibble(yardline_100 = 1:99, kq_0_sum_std = kq)
  }
  
  fg_model_plot = fg_plot_df %>%
    mutate(fgd = yardline_100 + 17) %>%
    mutate(p = predict(fg_model, ., type="response")) %>%
    mutate(color_col = factor(round(kq_0_sum_std,2))) %>%
    mutate(color_col = fct_reorder(color_col, -1*kq_0_sum_std)) %>%
    ggplot(aes(x = yardline_100, y = p, color = color_col)) +
    geom_line(size=1) +
    ylab("field goal make probability") + xlab("yardline") +
    labs(color=" kicker\n quality") +
    scale_y_continuous(breaks=seq(0,1,by=0.1)) +
    scale_x_continuous(
      breaks=seq(0,100,by=10),
      sec.axis = sec_axis(~.x+17, breaks = seq(27,70,by=10), name="field goal distance")
      # sec.axis = sec_axis(~.x+17, breaks = seq(15,70,by=10), name="field goal distance")
      # sec.axis = sec_axis(~.x+17, breaks = seq(17,70,by=10))
    ) +
    theme(axis.title = element_text(size=20),
          axis.text = element_text(size=20),
          legend.text = element_text(size=20),
          legend.title = element_text(size=20)) +
    scale_colour_manual(values = brewer.pal(name="PuRd",n=9)[3:11])
  fg_model_plot
}

### plot our punt expected next yardline model
plot_punt_eny_by_pq <- function(punt_model, pq=NULL) {
  pq_breaks = seq(-1,1,length=7)
  punt_plot_df = tibble()
  if (is.null(pq)) {
    for (pq in pq_breaks) {
      punt_plot_df = bind_rows(punt_plot_df, tibble(yardline_100 = 30:99, pq_0_sum_std = pq))
    }
  } else {
    punt_plot_df = tibble(yardline_100 = 30:99, pq_0_sum_std = pq)
  }
  punt_model_plot = punt_plot_df %>%
    mutate(p = predict(punt_model, ., type="response")) %>%
    mutate(color_col = factor(round(pq_0_sum_std,2))) %>%
    mutate(color_col = fct_reorder(color_col, -1*pq_0_sum_std)) %>%
    ggplot(aes(x = yardline_100, y = p, color = color_col)) +
    geom_line(size=1) +
    ylab("expected next yardline") + xlab("yardline") +
    labs(color=" punter\n quality") +
    scale_y_continuous(breaks=seq(0,100,by=10)) +
    scale_x_continuous(breaks=seq(0,100,by=10), limits=c(30,100)) +
    theme(axis.title = element_text(size=20),
          axis.text = element_text(size=20),
          legend.text = element_text(size=20),
          legend.title = element_text(size=20)) +
    scale_colour_manual(values = brewer.pal(name="PuRd",n=9)[3:11])
  punt_model_plot
}

### conversion prob 
plot_conv_0 <- function(conv_model, qbq_ot_0_sum=0, oq_rot_0_total_sum=0, dq_dt_0_againstPass_sum=0, dq_dt_0_againstRun_sum=0) {
  plot_conv = 
    expand.grid(yardline_100 = 1:99, ydstogo=1:10) %>%
    # expand.grid(yardline_100 = 1:93, ydstogo=c(1,2,3,4,10)) %>%
    filter(0 < yardline_100 - ydstogo & yardline_100 + ydstogo < 100) %>%
    filter(0 < yardline_100 - ydstogo & yardline_100 - ydstogo < 100) %>%
    mutate(qbq_ot_0_sum=qbq_ot_0_sum, oq_rot_0_total_sum=oq_rot_0_total_sum, 
           dq_dt_0_againstPass_sum=dq_dt_0_againstPass_sum, dq_dt_0_againstRun_sum=dq_dt_0_againstRun_sum, 
           down=4) %>%
    mutate(p_conv = predict(conv_model, ., type="response")) %>%
    ggplot(aes(x = yardline_100, y=p_conv, color=factor(ydstogo))) +
    geom_line(linewidth=1) +
    ylab("conversion probability") +
    xlab("yardline") +
    scale_x_continuous(breaks=seq(0,100,by=10)) +
    scale_y_continuous(breaks=seq(0,1,by=0.05)) +
    labs(color=" yards\n to go") +
    scale_colour_manual(values = rev(c(
      rev(brewer.pal(name="Blues",n=9)[5:9]),
      # brewer.pal(name="Purples",n=9)[6:9],
      rev(brewer.pal(name="Reds",n=9)[3:7])
    ))) +
    theme(
      axis.title = element_text(size=30)
    )
  plot_conv
}

plot_conv_ <- function(conv_model, qbq_ot_0_sum=0, oq_rot_0_total_sum=0, dq_dt_0_againstPass_sum=0, dq_dt_0_againstRun_sum=0) {
  plot_conv = 
    expand.grid(yardline_100 = 1:99, ydstogo=1:15) %>%
    filter(0 < yardline_100 - ydstogo & yardline_100 + ydstogo < 100) %>%
    filter(0 < yardline_100 - ydstogo & yardline_100 - ydstogo < 100) %>%
    mutate(qbq_ot_0_sum=qbq_ot_0_sum, oq_rot_0_total_sum=oq_rot_0_total_sum, 
           dq_dt_0_againstPass_sum=dq_dt_0_againstPass_sum, dq_dt_0_againstRun_sum=dq_dt_0_againstRun_sum, 
           down=4) %>%
    mutate(p_conv = predict(conv_model, ., type="response")) %>%
    group_by(ydstogo) %>%
    summarise(
      p_conv = mean(p_conv),
    ) %>%
    ggplot() +
    # geom_col(aes(x=ydstogo,y=p_conv, fill=-p_conv), show.legend = FALSE) +
    geom_col(aes(x=ydstogo,y=p_conv), fill="black") +
    scale_fill_gradient2(low = muted("red"),
                         # mid ="white",
                         high = muted("black"),) +
    ylab("conversion probability") +
    xlab("yards to go") +
    scale_x_continuous(breaks=seq(0,100,by=2)) +
    scale_y_continuous(breaks=seq(0,1,by=0.1)) +
    scale_colour_manual(values = rev(c(
      rev(brewer.pal(name="Blues",n=9)[5:9]),
      # brewer.pal(name="Purples",n=9)[6:9],
      rev(brewer.pal(name="Reds",n=9)[3:7])
    ))) +
    theme(
      axis.title = element_text(size=30)
    )
  plot_conv
}

######################
### Loss Functions ###
######################

MAE <- function(x,y,w=NULL) {
  if (is.null(w)) { w = rep(1, length(x)) } 
  mean(w*abs(x-y))
}

RMSE <- function(x,y,w=NULL) {
  if (is.null(w)) { w = rep(1, length(x)) } 
  sqrt( mean(w*(x-y)**2)  )
}

LOGLOSS <- function(y,p,w=NULL) {
  ### if p == 0, replace p with 10^-15
  ### if p == 1, replace p with 1-10^-15
  if (is.null(w)) { w = rep(1, length(x)) } 
  p = (tibble(p) %>% rowwise() %>% mutate(p_ = max(min(p, 1-10^-15), 10^-15)) )$p_
  -1 * mean( (y*log(p) + (1-y)*log(1-p))*w )
}

compute_loss_by_time_bin <- function(model, dataset, model_name, m=2.5, xgb_features=NULL, xgb_wp=TRUE, endgame=FALSE) {
  model_type = case_when(
    str_detect(model_name, "mlr") ~ "mlr",
    str_detect(model_name, "lr") ~ "lr",
    str_detect(model_name, "gam") ~ "gam",
    str_detect(model_name, "xgb") & str_detect(model_name, "TT") ~ "xgbTT",
    str_detect(model_name, "xgb") ~ "xgb",
    TRUE~ "other"
  )
  if (model_type == "mlr") {
    predss = predict_mlr_ep(model, dataset, model_name)$pred 
  } else if (model_type == "lr") {
    predss = predict_lr(model, dataset)
  } else if (model_type == "gam") {
    predss = predict_gam(model, dataset)
  } else if (model_type == "xgb") {
    if (xgb_wp) {
      predss = predict_probs_xgb(model, dataset, xgb_features, wp=TRUE)
    } else {
      predss = predict_ep_xgb(model, dataset, xgb_features, model_name, 
                              Regression=str_detect(model_name, "_R_"), BoundedRegression = str_detect(model_name, "_BR_"))$pred
    }
  } else if (model_type == "xgbTT") {
    predss = predict_probs_xgb_wp_TT(model, dataset, xgb_features)
  } 
  df_ =  dataset %>% 
    mutate(pred = predss) %>%
    mutate(game_sec_rem_bin = cut(
      game_seconds_remaining, 
      breaks = if (!endgame) seq(0,3600,by= 60*m) else seq(0,180,by= 15), 
      include.lowest=TRUE, dig.lab = 5)
    ) %>%
    group_by(game_sec_rem_bin) %>%
    filter(!is.na(game_sec_rem_bin))
  
  if (xgb_wp) {
    df_ = df_ %>% summarise(loss = LOGLOSS(label_win, pred)) %>% mutate(model = model_name)
  } else {
    df_ = df_ %>% summarise(loss = MAE(pts_next_score, pred)) %>% mutate(model = model_name)
  }
  return(df_)
}

plot_grouped_results <- function(grouped_preds) {
  grouped_results = grouped_preds %>%
    mutate(loss = round(loss, 2)) %>%
    arrange(game_sec_rem_bin, loss) %>%
    group_by(game_sec_rem_bin) %>%
    mutate(rank = rank(loss), i =1:n(), dd = rank-i) %>%
    mutate(rank1 = ifelse(rank - round(rank+0.01) == -0.5, rank - 0.5, rank)) %>%
    mutate(rank2 = rank1 + dd/15) %>%
    group_by(game_sec_rem_bin, loss) %>%
    mutate(loss = c(loss[1], rep(NA, n()-1)))
  print(grouped_results)
  # write_csv(grouped_results, paste0("test_results_grouped_wp.csv"))
  
  plot_grouped_results = grouped_results %>%
    mutate(lll = round(loss,2)) %>%
    ggplot(aes(y = game_sec_rem_bin, x = rank2)) +
    # geom_point(aes(y = game_sec_rem_bin, x = i, shape = model, color=model)) +
    geom_point(aes(color=model), size=3) +
    geom_text(aes(label = lll), color="black", size=3, nudge_x = 0.3) +
    scale_x_continuous(breaks=seq(0,100,by=1)) +
    ylab("game seconds remaining bin") +
    scale_shape_manual(values=seq(0,25)) +
    scale_colour_manual(values = c("firebrick", "#E69F00", "#56B4E9", "#009E73",
                                   "#F0E442", "#0072B2", "#D55E00", "#CC79A7")) +
    xlab("loss rank")
  plot_grouped_results
  # ggsave("plot_grouped_results.png", plot_grouped_results, width=20, height=10)
}

round_df <- function(df, digits) {
  nums <- vapply(df, is.numeric, FUN.VALUE = logical(1))
  
  df[,nums] <- round(df[,nums], digits = digits)
  
  (df)
}


# cross_entropy <- function(preds_p, y) {
#   preds_p$y = rep(y, nrow(preds_p)/length(y))
#   ent_df = preds_p %>% filter(label == y) %>% mutate(log_q = -log(p)) %>% arrange(i)
#   (ent_df %>% summarise(e = mean(log_q)))$e 
# }
# 
# brier <- function(preds_p, y) {
#   preds_p$y = rep(y, nrow(preds_p)/length(y))
#   brier_df = preds_p %>% filter(label == y) %>% mutate(b = (1-p)**2) %>% arrange(i)
#   (brier_df %>% summarise(e = mean(b)))$e 
# }
# 
# weighted_MAE <- function(x,y,w) {
#   sum(abs(x-y)*w) / sum(w)
# }
# 
# weighted_RMSE <- function(x,y,w) {
#   sqrt( sum(w*(x-y)**2) / sum(w) )
# }
# 
# weighted_cross_entropy <- function(preds_p, y, w) {
#   preds_p$y = rep(y, nrow(preds_p)/length(y))
#   ent_df = preds_p %>% filter(label == y) %>% mutate(log_q = -log(p)) %>% arrange(i)
#   ent_df = ent_df %>% group_by(model) %>% mutate(w=w)
#   (ent_df %>% summarise(e = sum(w*log_q)))$e / sum(w)
# }
# 
# weighted_brier <- function(preds_p, y, w) {
#   preds_p$y = rep(y, nrow(preds_p)/length(y))
#   brier_df = preds_p %>% filter(label == y) %>% mutate(b = (1-p)**2) %>% arrange(i)
#   brier_df = brier_df %>% group_by(model) %>% mutate(w=w)
#   (brier_df %>% summarise(e = sum(w*b)))$e / sum(w)
# }

##########################
### Sampling Functions ###
##########################

sample_one_play_per_epoch <- function(dataset, seed=NA) {
  if (!is.na(seed)) set.seed(seed)
  dataset %>% group_by(epoch) %>% slice_sample(n=1)
}

#############################################
### plot team/player quality trajectories ###
#############################################

plot_qbq_trajectories <- function(df_qbq_ot, qb_names) {
  df_qbq_ot %>%
    rename(qb = offensive_player_name) %>%
    mutate(
      t = (season-min(season))*max(week)+week,
    ) %>%
    group_by(qb,t) %>%
    slice_head() %>%
    arrange(qb,t) %>%
    group_by(qb, season) %>%
    filter(qb %in% qb_names) %>%
    ggplot(aes(y = qbqot, x=t, color = qb)) +
    geom_vline(xintercept = seq(1,450,18*1), color="gray80", linetype="dashed") +
    geom_hline(yintercept=0, color="gray60") + 
    geom_line(linewidth=1) +
    scale_x_continuous(
      breaks = seq(1,450+1,18*2),
      labels = function(x) 1999 + (x - 1) %/% 18 ,
      name = "Time"
    ) +
    # scale_y_continuous(breaks=seq(-5,5,by=1)) +
    labs(color = "Quarterback", y="Quarterback Quality") +
    # scale_color_discrete(name = "quarterback")
    theme(axis.text.x = element_text(size=15, angle=45, 
                                     hjust=1, vjust=1.15)) 
}

plot_oqot_trajectories <- function(df_oq_ot, team_names, min_year=1999) {
  df_oq_ot %>%
    mutate(
      t = (season-min(season))*max(week)+week,
    ) %>%
    group_by(posteam,t) %>%
    slice_head() %>%
    arrange(posteam,t) %>%
    group_by(posteam, season) %>%
    filter(posteam %in% team_names) %>%
    filter(season >= min_year) %>%
    ggplot(aes(y = oqot, x=t, color = posteam)) +
    geom_vline(xintercept = seq((min_year-1999)*18+1,450,18*1), color="gray80", linetype="dashed") +
    geom_hline(yintercept=0, color="gray60") + 
    geom_line(linewidth=1) +
    scale_x_continuous(
      breaks = seq((min_year-1999)*18+1,450+1,18*2),
      labels = function(x) 1999 + (x - 1) %/% 18 ,
      name = "Time"
    ) +
    # scale_color_nfl() +
    scale_y_continuous(breaks=seq(-5,5,by=1), name = "Non-QB Offensive Quality") +
    labs(color = "Team") +
    # scale_color_discrete(name = "quarterback")
    theme(axis.text.x = element_text(size=15, angle=45, 
                                     hjust=1, vjust=1.15)) 
}

plot_dqdt_againstRun_trajectories <- function(df_dq_dt, team_names, min_year=1999) {
  df_dq_dt %>%
    mutate(
      t = (season-min(season))*max(week)+week,
    ) %>%
    group_by(defteam,t) %>%
    slice_head() %>%
    arrange(defteam,t) %>%
    group_by(defteam, season) %>%
    filter(defteam %in% team_names) %>%
    filter(season >= min_year) %>%
    ggplot(aes(y = dqdt_againstRun, x=t, color = defteam)) +
    geom_vline(xintercept = seq((min_year-1999)*18+1,450,18*1), color="gray80", linetype="dashed") +
    geom_hline(yintercept=0, color="gray60") + 
    geom_line(linewidth=1) +
    scale_x_continuous(
      breaks = seq((min_year-1999)*18+1,450+1,18*2),
      labels = function(x) 1999 + (x - 1) %/% 18 ,
      name = "Time"
    ) +
    # scale_color_nfl() +
    scale_y_continuous(breaks=seq(-5,5,by=1), name = "Defensive Quality\nAgainst The Run") +
    labs(color = "Team") +
    # scale_color_discrete(name = "quarterback")
    theme(axis.text.x = element_text(size=15, angle=45, 
                                     hjust=1, vjust=1.15)) 
}

plot_dqdt_againstPass_trajectories <- function(df_dq_dt, team_names, min_year=1999) {
  df_dq_dt %>%
    mutate(
      t = (season-min(season))*max(week)+week,
    ) %>%
    group_by(defteam,t) %>%
    slice_head() %>%
    arrange(defteam,t) %>%
    group_by(defteam, season) %>%
    filter(defteam %in% team_names) %>%
    filter(season >= min_year) %>%
    ggplot(aes(y = dqdt_againstPass, x=t, color = defteam)) +
    geom_vline(xintercept = seq((min_year-1999)*18+1,450,18*1), color="gray80", linetype="dashed") +
    geom_hline(yintercept=0, color="gray60") + 
    geom_line(linewidth=1) +
    scale_x_continuous(
      breaks = seq((min_year-1999)*18+1,450+1,18*2),
      labels = function(x) 1999 + (x - 1) %/% 18 ,
      name = "Time"
    ) +
    # scale_color_nfl() +
    scale_y_continuous(breaks=seq(-5,5,by=1), name = "Defensive Quality\nAgainst The Pass") +
    labs(color = "Team") +
    # scale_color_discrete(name = "quarterback")
    theme(axis.text.x = element_text(size=15, angle=45, 
                                     hjust=1, vjust=1.15)) 
}


