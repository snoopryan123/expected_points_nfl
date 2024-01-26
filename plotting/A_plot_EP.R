

########################
filewd = getwd()
setwd("..")
source("A1_load_data.R") 
setwd(filewd)
########################

############################
### load data and models ###
############################

# MODEL_TYPE = "MLR"
# source("../model_comparison/models_MLR.R")
# model_name = "mlr_yurko_s1d1_w"
# weighted_model = endsWith(model_name, "_w")
# dataset_to_fit = data_full
# model_fit = get(paste0("fit_",str_remove(model_name, "_w")))(dataset_to_fit, weight_me = weighted_model)

# MODEL_TYPE = "OLS"
# source("../model_comparison/models_OLS.R")
# model_name = "lm_s2d1_w"
# weighted_model = endsWith(model_name, "_w")
# dataset_to_fit = data_full
# model_fit = get(paste0("fit_",str_remove(model_name, "_w")))(dataset_to_fit, weight_me = weighted_model)

MODEL_TYPE = "XGB"
filewd = getwd()
setwd("../model_comparison")
source("models_XGB.R")
setwd(filewd)
### make sure to first train and save these XGB models in `train_full_models.R`
model_name = "xgb_C_s_1"
# model_name = "xgb_C_s_1_wbe"
# model_name = "xgb_C_oq2xdq2x_1_wbe"
model_fit = xgb.load(paste0("../model_comparison/",model_name,".xgb"))
xgb_features = get(paste0(model_name, "_features"))
xgb_is_regression = str_detect(model_name, "xgb_R_")
xgb_is_BoundedRegression = str_detect(model_name, "xgb_BR_")

print(model_fit)
no_dq = !str_detect(model_name, "dq")
is_4x = str_detect(model_name, "4x") | (str_detect(model_name, "xgb") & str_detect(model_name, "2x"))
spread_tq = str_detect(model_name, "_s")

############################
### visualize the models ###
############################
# l = -3; u = 9;
l = -3.5; u = 7;

plot_varyingTime <- function(model_fit, model_name, half_=1) {
  my_palette <- c(
    rev(brewer.pal(name="Blues",n=9)[3:9]),
    brewer.pal(name="Purples",n=9)[3:9],
    rev(brewer.pal(name="Reds",n=9)[3:9])
    # rev(brewer.pal(name="BrBG",n=9)[7:9]),
    # rev(brewer.pal(name="BrBG",n=9)[1:3]),
    # rev(brewer.pal(name="YlOrBr",n=9)[3:8])
  )
  
  # N = 16
  # time_breaks = c(seq(0,1-120/1800,length=N), 1-60/1800, 1-30/1800, 1-10/1800)
  
  time_breaks = c(10, 30, 60, seq(120, 1800, by=120))
  
  plot_set = tibble()
  for (j in length(time_breaks):1) {
    plot_set = bind_rows(
      plot_set,
      tibble(
        yardline_100 = 1:99,
        ydstogo=c(1:9, rep(10,90)), 
        qbq_ot_0_sum = 0, oq_rot_0_total_sum = 0, 
        qbq_dt_0_sum = 0, oq_rdt_0_sum = 0,   
        # dq_dt_0_total_sum = 0, dq_ot_0_sum = 0,
        dq_dt_0_againstPass_sum = 0, dq_ot_0_againstPass_sum = 0,
        dq_dt_0_againstRun_sum = 0, dq_ot_0_againstRun_sum = 0,
        posteam_spread = 0, posteam_spread_std = 0,
        down=1, down1=1, down2=0, down3=0, down4=0, 
        game_seconds_remaining = time_breaks[j] + ifelse(half_ == 1, 1800, 0), ####
        half_seconds_remaining = ifelse(game_seconds_remaining > 1800, game_seconds_remaining - 1800, game_seconds_remaining),  
        half_sec_rem_std = 1 - half_seconds_remaining/1800,
        half = ifelse(game_seconds_remaining > 1800, 1, 2),
        utm = as.numeric(half_seconds_remaining <= 120), gtg = yardline_100 <= 10,
        era0=0, era1=0, era2=0, era3=1, era4=0, era_B=3, era_A=4, season=2015, home=0,
        posteam_timeouts_remaining=3,defteam_timeouts_remaining=3,retractable=0,dome=0,
        score_differential=0,
      )
    )
  }
  
  if (MODEL_TYPE == "OLS") {
    pred_ep = bind_cols(
      tibble(pred = predict_lm(model_fit, plot_set), model=model_name),
      plot_set
    )
  } else if (MODEL_TYPE == "MLR") {
    pred_ep = bind_cols(
      predict_mlr_ep(model_fit, plot_set, model_name),
      plot_set
    )
  } else if (MODEL_TYPE == "XGB") {
    pred_ep = bind_cols(
      predict_ep_xgb(model_fit, plot_set, xgb_features, model_name, Regression=xgb_is_regression, BoundedRegression=xgb_is_BoundedRegression),
      plot_set
    )
  }
  
  # legend_title = " half\n seconds\n remaining"
  legend_title = " game\n seconds\n remaining"
  plot = pred_ep %>%
    mutate(
      color_col = fct_reorder(factor(round(game_seconds_remaining,2)), 
                                   -round(game_seconds_remaining,2)),
      # down = paste0("down = ", down)
    ) %>%
    ggplot(aes(x = yardline_100, y = pred, color = color_col)) +
    # facet_wrap(~ down) +
    labs(color=legend_title)  +
    geom_line(linewidth=1) +
    scale_x_continuous(breaks=seq(0,100,10)) +
    scale_y_continuous(limits=c(l,u),breaks=seq(-20,20,1)) +
    xlab("yard line y") +
    scale_colour_manual(values = my_palette) +
    ylab("expected points of the next score") #+
  # theme(axis.title = element_text(size=20),
  #       axis.text = element_text(size=20),
  #       legend.text = element_text(size=20),
  #       legend.title = element_text(size=20))
  
  plot
  ggsave(paste0("plot_model_",model_name,"_time.png"), plot,width=9,height=7)
  # return(plot)
}

plot_varyingTimeByDown <- function(model_fit, model_name, half_=1) {
  my_palette <- c(
    rev(brewer.pal(name="Blues",n=9)[3:9]),
    brewer.pal(name="Purples",n=9)[3:9],
    rev(brewer.pal(name="Reds",n=9)[3:9])
    # rev(brewer.pal(name="BrBG",n=9)[7:9]),
    # rev(brewer.pal(name="BrBG",n=9)[1:3]),
    # rev(brewer.pal(name="YlOrBr",n=9)[3:8])
  )
  
  # N = 16
  # time_breaks = c(seq(0,1-120/1800,length=N), 1-60/1800, 1-30/1800, 1-10/1800)
  
  time_breaks = c(10, 30, 60, seq(120, 1800, by=120))
  
  plot_set = tibble()
  for (j in length(time_breaks):1) {
    for (dd in 1:4) {
      plot_set = bind_rows(
        plot_set,
        tibble(
          yardline_100 = 1:99,
          ydstogo=c(1:9, rep(10,90)), 
          qbq_ot_0_sum = 0, oq_rot_0_total_sum = 0, 
          qbq_dt_0_sum = 0, oq_rdt_0_sum = 0,   
          # dq_dt_0_total_sum = 0, dq_ot_0_sum = 0,
          dq_dt_0_againstPass_sum = 0, dq_ot_0_againstPass_sum = 0,
          dq_dt_0_againstRun_sum = 0, dq_ot_0_againstRun_sum = 0,
          posteam_spread = 0, posteam_spread_std = 0,
          down=dd, 
          down1=as.numeric(dd==1), down2=as.numeric(dd==2), down3=as.numeric(dd==3), down4=as.numeric(dd==4), 
          game_seconds_remaining = time_breaks[j] + ifelse(half_ == 1, 1800, 0), ####
          half_seconds_remaining = ifelse(game_seconds_remaining > 1800, game_seconds_remaining - 1800, game_seconds_remaining),  
          half_sec_rem_std = 1 - half_seconds_remaining/1800,
          half = ifelse(game_seconds_remaining > 1800, 1, 2),
          utm = as.numeric(half_seconds_remaining <= 120), gtg = yardline_100 <= 10,
          era0=0, era1=0, era2=0, era3=1, era4=0, era_B=3, era_A=4, season=2015, home=0,
          posteam_timeouts_remaining=3,defteam_timeouts_remaining=3,retractable=0,dome=0,
          score_differential=0,
        )
      )
    }
  }
  
  if (MODEL_TYPE == "OLS") {
    pred_ep = bind_cols(
      tibble(pred = predict_lm(model_fit, plot_set), model=model_name),
      plot_set
    )
  } else if (MODEL_TYPE == "MLR") {
    pred_ep = bind_cols(
      predict_mlr_ep(model_fit, plot_set, model_name),
      plot_set
    )
  } else if (MODEL_TYPE == "XGB") {
    pred_ep = bind_cols(
      predict_ep_xgb(model_fit, plot_set, xgb_features, model_name, Regression=xgb_is_regression, BoundedRegression=xgb_is_BoundedRegression),
      plot_set
    )
  }
  
  # legend_title = " half\n seconds\n remaining"
  legend_title = " game\n seconds\n remaining"
  plot = pred_ep %>%
    mutate(
      color_col = fct_reorder(factor(round(game_seconds_remaining,2)), 
                              -round(game_seconds_remaining,2)),
      down = paste0("down = ", down)
    ) %>%
    ggplot(aes(x = yardline_100, y = pred, color = color_col)) +
    facet_wrap(~ down) +
    labs(color=legend_title)  +
    geom_line(linewidth=1) +
    scale_x_continuous(breaks=seq(0,100,10)) +
    scale_y_continuous(limits=c(l,u),breaks=seq(-20,20,1)) +
    xlab("yard line y") +
    scale_colour_manual(values = my_palette) +
    ylab("expected points of the next score") #+
  # theme(axis.title = element_text(size=20),
  #       axis.text = element_text(size=20),
  #       legend.text = element_text(size=20),
  #       legend.title = element_text(size=20))
  
  plot
  ggsave(paste0("plot_model_",model_name,"_timeByDown.png"), plot,width=11,height=9)
  # return(plot)
}

plot_varyingTQ <- function(model_fit, model_name, colname, N=7, keepFewSpreads=FALSE) {
  
  if (!spread_tq) {
    my_palette <- brewer.pal(name="PuRd",n=9)[3:11]
  } else if (keepFewSpreads) {
    # my_palette <- brewer.pal(name="PuRd",n=9)[c(3,5,7,9)]
    my_palette <- c("magenta", "forestgreen", "firebrick", "dodgerblue2")
  } else {
    my_palette <- c(
      rev(brewer.pal(name="Blues",n=9)[3:9]),
      brewer.pal(name="Purples",n=9)[3:9],
      rev(brewer.pal(name="Reds",n=9)[3:9])
      # rev(brewer.pal(name="BrBG",n=9)[7:9]),
      # rev(brewer.pal(name="BrBG",n=9)[1:3]),
      # rev(brewer.pal(name="YlOrBr",n=9)[4:8])
    )
  }

  if (!spread_tq) {
    tq_breaks = seq(-1,1,length=N)
    
    plot_set = tibble()
    for (j in 1:N) {
      plot_set = bind_rows(
        plot_set,
        tibble(
          yardline_100 = 1:99,
          ydstogo=c(1:9, rep(10,90)), 
          qbq_ot_0_sum = ifelse(colname == "qbq_ot_0_sum", tq_breaks[j], 0),
          oq_rot_0_total_sum = ifelse(colname == "oq_rot_0_total_sum", tq_breaks[j], 0),
          qbq_dt_0_sum = ifelse(colname == "qbq_dt_0_sum", tq_breaks[j], 0),
          oq_rdt_0_sum = ifelse(colname == "oq_rdt_0_sum", tq_breaks[j], 0),
          dq_dt_0_againstPass_sum = ifelse(colname == "dq_dt_0_againstPass_sum", tq_breaks[j], 0),
          dq_ot_0_againstPass_sum = ifelse(colname == "dq_ot_0_againstPass_sum", tq_breaks[j], 0),
          dq_dt_0_againstRun_sum = ifelse(colname == "dq_dt_0_againstRun_sum", tq_breaks[j], 0),
          dq_ot_0_againstRun_sum = ifelse(colname == "dq_ot_0_againstRun_sum", tq_breaks[j], 0),
          # dq_dt_0_total_sum = ifelse(colname == "dq_dt_0_total_sum", tq_breaks[j], 0),
          # dq_ot_0_sum = ifelse(colname == "dq_ot_0_sum", tq_breaks[j], 0), 
          # posteam_spread = ifelse(colname == "posteam_spread", tq_breaks[j], 0),
          down=1, down1=1, down2=0, down3=0, down4=0, 
          game_seconds_remaining = 3600,
          half_seconds_remaining = 1800, half_sec_rem_std = 0, 
          half = ifelse(game_seconds_remaining > 1800, 1, 2),
          utm = as.numeric(half_seconds_remaining <= 120), gtg = yardline_100 <= 10,
          era0=0, era1=0, era2=0, era3=1, era4=0, era_B=3, era_A=4, season=2020, home=0,
          posteam_timeouts_remaining=3,defteam_timeouts_remaining=3,retractable=0,dome=0,
          score_differential=0,
        )
      )
    }
    legend_title = case_when(
      colname == "qbq_ot_0_sum" ~ " offensive\n team's\n quartberack\n quality",
      colname == "oq_rot_0_total_sum" ~ " offensive\n team's\n remaining\n offensive\n quality",
      colname == "qbq_dt_0_sum" ~ " defensive\n team's\n quartberack\n quality",
      colname == "oq_rdt_0_sum" ~ " defensive\n team's\n remaining\n offensive\n quality",
      colname == "dq_dt_0_againstPass_sum" ~ " defensive\n team's\n defensive\n quality\n against\n the pass",
      colname == "dq_ot_0_againstPass_sum" ~ " offensive\n team's\n defensive\n quality\n against\n the pass",
      colname == "dq_dt_0_againstRun_sum" ~ " defensive\n team's\n defensive\n quality\n against\n the run",
      colname == "dq_ot_0_againstRun_sum" ~ " offensive\n team's\n defensive\n quality\n against\n the run",
      # colname == "dq_dt_0_total_sum" ~ " defensive\n team's\n defensive\n quality",
      # colname == "dq_ot_0_sum" ~ " offensive\n team's\n defensive\n quality",
      TRUE ~ "FIXME"
    )
  } else {
    spread_breaks = seq(-10,10,by=1)
    if (keepFewSpreads) {
      spread_breaks = c(9,7,-3,-2)
    }
    
    plot_set = tibble()
    for (j in 1:length(spread_breaks)) {
      plot_set = bind_rows(
        plot_set,
        tibble(
          yardline_100 = 1:99,
          ydstogo=c(1:9, rep(10,90)), 
          posteam_spread = spread_breaks[j],
          posteam_spread_std = (posteam_spread - mean(data_full_0$posteam_spread))/sd(data_full_0$posteam_spread),
          down=1, down1=1, down2=0, down3=0, down4=0, 
          game_seconds_remaining = 3600,
          half_seconds_remaining = 1800, half_sec_rem_std = 0, 
          half = ifelse(game_seconds_remaining > 1800, 1, 2),
          utm = as.numeric(half_seconds_remaining <= 120), gtg = yardline_100 <= 10,
          era0=0, era1=0, era2=0, era3=1, era4=0, era_B=3, era_A=4, season=2020, home=0,
          posteam_timeouts_remaining=3,defteam_timeouts_remaining=3,retractable=0,dome=0,
          score_differential=0,
        )
      )
    }
    legend_title = " point\n spread"
  }
  
  
  if (MODEL_TYPE == "OLS") {
    pred_ep = bind_cols(
      tibble(pred = predict_lm(model_fit, plot_set), model=model_name),
      plot_set
    )
  } else if (MODEL_TYPE == "MLR") {
    pred_ep = bind_cols(
      predict_mlr_ep(model_fit, plot_set, model_name),
      plot_set
    )
  } else if (MODEL_TYPE == "XGB") {
    pred_ep = bind_cols(
      predict_ep_xgb(model_fit, plot_set, xgb_features, model_name, Regression=xgb_is_regression, BoundedRegression=xgb_is_BoundedRegression),
      plot_set
    )
  }
  
  plot_title = ""
  if (spread_tq) {
    plot = pred_ep %>%
      mutate(color_col = fct_reorder(factor(posteam_spread), -posteam_spread)) 
  } else {
    plot = pred_ep %>%
      mutate(color_col = fct_reorder(factor(round(.data[[colname]],2)), -round(.data[[colname]],2))) 
  }

  plot = plot %>%
    ggplot(aes(x = yardline_100, y = pred, color = color_col)) +
    labs(color=legend_title)  +
    geom_line(linewidth=1) +
    # scale_x_continuous(trans = "reverse", breaks=seq(0,100,10)) +
    scale_x_continuous(breaks=seq(0,100,10)) +
    # scale_y_continuous(breaks=seq(-20,20,1)) +
    scale_y_continuous(limits=c(l,u),breaks=seq(-20,20,1)) +
    # labs(title=paste0("model: ", model_name)) +
    xlab("yard line y") + labs(title=plot_title) +
    ylab("expected points of the next score") +
    # theme(axis.title = element_text(size=20),
    #       axis.text = element_text(size=20),
    #       legend.text = element_text(size=20),
    #       legend.title = element_text(size=20)) +
    scale_colour_manual(values = my_palette)
  
  return(plot)
}

plot_varyingDown <- function(model_fit, model_name) {
  
  my_palette <- c("magenta", "forestgreen", "firebrick", "dodgerblue2")
    
  plot_set = tibble()
  for (dd in 1:4) {
    plot_set = bind_rows(
      plot_set,
      tibble(
        yardline_100 = 1:99,
        ydstogo=c(1:9, rep(10,90)), 
        posteam_spread = 0,
        posteam_spread_std = 0,
        down=dd, 
        down1=as.numeric(dd==1), down2=as.numeric(dd==2), down3=as.numeric(dd==3), down4=as.numeric(dd==4), 
        # game_seconds_remaining = 3600, half_seconds_remaining = 1800,
        game_seconds_remaining = 2700, half_seconds_remaining = 900,
        # half_sec_rem_std = 0, 
        half = ifelse(game_seconds_remaining > 1800, 1, 2),
        utm = as.numeric(half_seconds_remaining <= 120), gtg = yardline_100 <= 10,
        era0=0, era1=0, era2=0, era3=1, era4=0, era_B=3, era_A=4, season=2020, home=0,
        posteam_timeouts_remaining=3,defteam_timeouts_remaining=3,retractable=0,dome=0,
        score_differential=0,
      )
    )
  }
  legend_title = "down"
  
  if (MODEL_TYPE == "OLS") {
    pred_ep = bind_cols(
      tibble(pred = predict_lm(model_fit, plot_set), model=model_name),
      plot_set
    )
  } else if (MODEL_TYPE == "MLR") {
    pred_ep = bind_cols(
      predict_mlr_ep(model_fit, plot_set, model_name),
      plot_set
    )
  } else if (MODEL_TYPE == "XGB") {
    pred_ep = bind_cols(
      predict_ep_xgb(model_fit, plot_set, xgb_features, model_name, Regression=xgb_is_regression, BoundedRegression=xgb_is_BoundedRegression),
      plot_set
    )
  }
  
  plot_title = ""
  plot = pred_ep %>%
    ggplot(aes(x = yardline_100, y = pred, color = factor(down))) +
    labs(color=legend_title)  +
    geom_line(linewidth=1) +
    # scale_x_continuous(trans = "reverse", breaks=seq(0,100,10)) +
    scale_x_continuous(breaks=seq(0,100,10)) +
    # scale_y_continuous(breaks=seq(-20,20,1)) +
    scale_y_continuous(limits=c(l,u),breaks=seq(-20,20,1)) +
    # labs(title=paste0("model: ", model_name)) +
    xlab("yard line y") + labs(title=plot_title) +
    ylab("expected points of the next score") +
    # theme(axis.title = element_text(size=20),
    #       axis.text = element_text(size=20),
    #       legend.text = element_text(size=20),
    #       legend.title = element_text(size=20)) +
    scale_colour_manual(values = my_palette)
  plot
  ggsave(paste0("plot_model_",model_name,"_down.png"), plot,width=9,height=7)
  # return(plot)
}

plot_varyingScoreDiff <- function(model_fit, model_name) {
  my_palette <- brewer.pal(name="PuRd",n=9)[3:11]
  # my_palette <- c(
  #   rev(brewer.pal(name="Blues",n=9)[3:9]),
  #   brewer.pal(name="Purples",n=9)[3:9],
  #   rev(brewer.pal(name="Reds",n=9)[3:9])
  #   # rev(brewer.pal(name="BrBG",n=9)[7:9]),
  #   # rev(brewer.pal(name="BrBG",n=9)[1:3]),
  #   # rev(brewer.pal(name="YlOrBr",n=9)[3:8])
  # )
  
  # N = 16
  # time_breaks = c(seq(0,1-120/1800,length=N), 1-60/1800, 1-30/1800, 1-10/1800)
  
  # score_diffs = c(-17,-10,-6,-2,0,2,6,10,17)
  # score_diffs = seq(-24,24,by=3)
  score_diffs = seq(-24,24,by=1)
  
  plot_set = tibble()
  for (j in length(score_diffs):1) {
    # for (j in 1:length(time_breaks)) {
    plot_set = bind_rows(
      plot_set,
      tibble(
        yardline_100 = 1:99,
        ydstogo=c(1:9, rep(10,90)), 
        qbq_ot_0_sum = 0, oq_rot_0_total_sum = 0, 
        qbq_dt_0_sum = 0, oq_rdt_0_sum = 0,   
        # dq_dt_0_total_sum = 0, dq_ot_0_sum = 0,
        dq_dt_0_againstPass_sum = 0, dq_ot_0_againstPass_sum = 0,
        dq_dt_0_againstRun_sum = 0, dq_ot_0_againstRun_sum = 0,
        posteam_spread = 0, posteam_spread_std = 0,
        down=1, down1=1, down2=0, down3=0, down4=0, 
        game_seconds_remaining = 300,
        half_seconds_remaining = ifelse(game_seconds_remaining > 1800, game_seconds_remaining - 1800, game_seconds_remaining),  
        half_sec_rem_std = 1 - half_seconds_remaining/1800,
        half = ifelse(game_seconds_remaining > 1800, 1, 2),
        utm = as.numeric(half_seconds_remaining <= 120), gtg = yardline_100 <= 10,
        era0=0, era1=0, era2=0, era3=1, era4=0, era_B=3, era_A=4, season=2015, home=0,
        posteam_timeouts_remaining=3,defteam_timeouts_remaining=3,retractable=0,dome=0,
        score_differential=score_diffs[j],
      )
    )
  }
  
  if (MODEL_TYPE == "OLS") {
    pred_ep = bind_cols(
      tibble(pred = predict_lm(model_fit, plot_set), model=model_name),
      plot_set
    )
  } else if (MODEL_TYPE == "MLR") {
    pred_ep = bind_cols(
      predict_mlr_ep(model_fit, plot_set, model_name),
      plot_set
    )
  } else if (MODEL_TYPE == "XGB") {
    pred_ep = bind_cols(
      predict_ep_xgb(model_fit, plot_set, xgb_features, model_name, Regression=xgb_is_regression, BoundedRegression=xgb_is_BoundedRegression),
      plot_set
    )
  }
  
  legend_title = " score\n differetial"
  
  plot = pred_ep %>%
    group_by(score_differential) %>%
    summarise(pred = mean(pred)) %>%
    ggplot(aes(x = score_differential, y = pred)) + 
    geom_point() +
    # scale_x_continuous(breaks=score_diffs) +
    scale_x_continuous(breaks=seq(-24,24,by=6)) +
    scale_y_continuous(breaks=seq(-7,7,by=0.25)) +
    xlab("score differential") +
    ylab("expected points") +
    labs(title="5 minutes remaining in the game")
  plot
  ggsave(paste0("plot_model_",model_name,"_scoreDiff.png"), plot,width=9,height=7)
  # return(plot)
}

plot_xgbC_evidenceOfOverfitting <- function(model_fit, model_name, colname, N=7, keepFewSpreads=FALSE) {

  plot_set = tibble(
    yardline_100 = 1:99,
    ydstogo=c(1:9, rep(10,90)), 
    posteam_spread = 0,
    down=4, 
    game_seconds_remaining = 2*60,
    half_seconds_remaining = 2*60, 
    # half_sec_rem_std = 0, 
    half = ifelse(game_seconds_remaining > 1800, 1, 2),
    utm = as.numeric(half_seconds_remaining <= 120), gtg = yardline_100 <= 10,
    era0=0, era1=0, era2=0, era3=1, era4=0, era_B=3, era_A=4, season=2020, home=0,
    posteam_timeouts_remaining=2,defteam_timeouts_remaining=0,retractable=0,dome=0,
    score_differential=-4,
  ) %>%
    mutate(
      down1=as.numeric(down==1),
      down2=as.numeric(down==2),
      down3=as.numeric(down==3),
      down4=as.numeric(down==4),
    )
   
  if (MODEL_TYPE == "OLS") {
    pred_ep = bind_cols(
      tibble(pred = predict_lm(model_fit, plot_set), model=model_name),
      plot_set
    )
  } else if (MODEL_TYPE == "MLR") {
    pred_ep = bind_cols(
      predict_mlr_ep(model_fit, plot_set, model_name),
      plot_set
    )
  } else if (MODEL_TYPE == "XGB") {
    pred_ep = bind_cols(
      predict_ep_xgb(model_fit, plot_set, xgb_features, model_name, Regression=xgb_is_regression, BoundedRegression=xgb_is_BoundedRegression),
      plot_set
    )
  }
  
  # plot_title = ""
  # plot = pred_ep %>%
  #   mutate(color_col = fct_reorder(factor(round(.data[[colname]],2)), -round(.data[[colname]],2))) 
  
  plot = pred_ep %>%
    ggplot(aes(x = yardline_100, y = pred)) +
    # ggplot(aes(x = yardline_100, y = pred, color = color_col)) +
    # labs(color=legend_title)  +
    geom_line(linewidth=1) +
    scale_x_continuous(breaks=seq(0,100,10)) +
    scale_y_continuous(limits=c(l,u),breaks=seq(-20,20,1)) +
    xlab("yard line y") + 
    # labs(title=plot_title) +
    # scale_colour_manual(values = my_palette) +
    ylab("expected points of the next score") 
  plot
  
  return(plot)
}

make_plots_batch <- function(model_name, model_fit) {
  if (!spread_tq) {
    # plot_varyingTime(model_fit, model_name)
    # p1 = plot_varyingTQ(model_fit, model_name, "qbq_ot_0_sum")
    # p2 = plot_varyingTQ(model_fit, model_name, "oq_rot_0_total_sum")
    # p3 = plot_varyingTQ(model_fit, model_name, "dq_dt_0_total_sum")
    # p4 = plot_varyingTQ(model_fit, model_name, "qbq_dt_0_sum")
    # p5 = plot_varyingTQ(model_fit, model_name, "oq_rdt_0_sum")
    # p6 = plot_varyingTQ(model_fit, model_name, "dq_ot_0_sum")
    p1 = plot_varyingTQ(model_fit, model_name, "qbq_ot_0_sum")
    p2 = plot_varyingTQ(model_fit, model_name, "oq_rot_0_total_sum")
    p3 = plot_varyingTQ(model_fit, model_name, "dq_dt_0_againstPass_sum")
    p4 = plot_varyingTQ(model_fit, model_name, "dq_dt_0_againstRun_sum")
    p5 = plot_varyingTQ(model_fit, model_name, "qbq_dt_0_sum")
    p6 = plot_varyingTQ(model_fit, model_name, "oq_rdt_0_sum")
    p7 = plot_varyingTQ(model_fit, model_name, "dq_ot_0_againstPass_sum")
    p8 = plot_varyingTQ(model_fit, model_name, "dq_ot_0_againstRun_sum")
    if (no_dq) {
      p14 = plot_grid(p1,p2,p4,p5, nrow=2 )
      save_plot(paste0("plot_model_",model_name,"_4OQ.png"), p14, base_width=16, base_height=12)
    } else {
      p18 = plot_grid(p1,p2,p3,p4,p5,p6,p7,p8, nrow=2 )
      save_plot(paste0("plot_model_",model_name,"_8TQ.png"), p18, base_width=30, base_height=12)
      # p16 = plot_grid(p1,p2,p3,p4,p5,p6, nrow=2 )
      # save_plot(paste0("plot_model_",model_name,"_6TQ.png"), p16, base_width=24, base_height=12)
    }
    # save_plot(paste0("plot_model_",model_name,"_diagonalTQ.png"), pd, base_width=8, base_height=6)
    ## return(p1234)
    # p1 = plot_varyingTQ(model_fit, model_name, "oq_ot_0_total_sum")
    # p2 = plot_varyingTQ(model_fit, model_name, "dq_dt_0_total_sum")
    # p12 = plot_grid(p1,p2)
    # save_plot(paste0("plot_model_",model_name,"_2TQ.png"), p12, base_width=14, base_height=5)
    # ## return(p12)
  } else {
    p1 = plot_varyingTQ(model_fit, model_name, "")
    ggsave(paste0("plot_model_",model_name,"_sTQ.png"), p1, width=9, height=7)
    if (MODEL_TYPE=="XGB" & str_detect(model_name, "xgb_C")) {
      p2 = plot_varyingTQ(model_fit, model_name, "", keepFewSpreads=TRUE) 
      ggsave(paste0("plot_model_",model_name,"_sTQoverfitting.png"), p2, width=9, height=7)
    }
  }
}

plot_varyingTime(model_fit, model_name)
plot_varyingTimeByDown(model_fit, model_name)
plot_varyingScoreDiff(model_fit, model_name)
plot_varyingDown(model_fit, model_name)
make_plots_batch(model_name, model_fit)
 
