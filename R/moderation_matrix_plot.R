#' moderation_matrix
#'
#' Plots a moderation matrix
#' @param ... meta_ninja models all sharing a common data name
#' @param effect_size a string
#' @param moderators a list of moderators to include, order retained.
#' @param null_value a scalar indicating non-sigificance (where the dashed line will be drawn).
#' @param transf function to transform values
#' @param leading_zero when true, leading zeros are allowed on the x-axis
#' @import ggplot2 data.table
#' @export

moderation_matrix <- function(..., effect_size = "Effect size", moderators = NULL,
                              null_value = 0, transf = NULL, leading_zero = TRUE){

  models <- list(...)

  dat_list <- lapply(models, function(x) coef(x, everything()))
  #return(dat_list)
  #return(dat_list)
  DL = lapply(seq_along(dat_list), function(x){
    dat_list[[x]]$outcome = names(dat_list)[x]
    dat_list[[x]]
  }) %>% do.call(rbind, .)  %>% data.table::data.table()

  if(is.null(transf)){
    transf = function(x) x
  }

  DL = DL[type != "effect size", .(
    y = transf(est),
    lower = transf(lower),
    upper = transf(upper),
    cluster, moderation,
    outcome = factor(outcome, levels = names(models)), type,
    model_p)]

  graph_dat = DL

  if(is.null(moderators)){
  mod_levels <- unique(graph_dat$moderation)
  mod_levels <- mod_levels[mod_levels != "Baseline"]
  }else{
    mod_levels = moderators
  }
  graph_dat$moderation[graph_dat$moderation == "Baseline"] = ""
  mod_levels = c(mod_levels, "")

  graph_dat$moderation = factor(graph_dat$moderation, levels = mod_levels)
  graph_dat = graph_dat[!is.na(graph_dat$moderation),]
  graph_dat$outcome = factor(graph_dat$outcome)
  # ------- fix cluster order

  cluster_levels <- unique(graph_dat$cluster)
  cluster_levels <- cluster_levels[cluster_levels != "Baseline"]
  cluster_levels <- c(cluster_levels, "Baseline")

  graph_dat$cluster = factor(graph_dat$cluster, levels = cluster_levels)
  graph_dat$cluster = forcats::fct_rev(graph_dat$cluster)

  # prepare significance -------

  final_dat = graph_dat %>% # give baseline a p value
   dplyr:: mutate(model_p = tidyr::replace_na(model_p, 0)) %>%  # Baseline p is NA. I want it to act as sig.
    na.omit()

  setkey(final_dat, outcome, moderation)
  sig_dat  <- final_dat[CJ(outcome,moderation, unique = TRUE), .(p = mean(model_p, na.rm = T), y = 0, cluster = NA) , by = .EACHI]
  sig_dat$p[is.na(sig_dat$p)] = 1
  # sig_dat = final_dat %>% # this table summarises moderation p values
  #   dplyr::group_by(outcome, moderation) %>%
  #   dplyr::summarise(p = mean(model_p, na.rm = T), y = 0, cluster = NA) %>% # empty y and cluster vars set up for ggplot2
  #   tidyr::complete(outcome, moderation) %>%  # include zero sums so no data panels are grey
  #   data.table::data.table()

  sig_dat$cluster = factor(sig_dat$cluster, levels = levels(final_dat$cluster))

  p <- ggplot(final_dat, aes(
    x = y,
    y = cluster,
    xmin = lower,
    xmax = upper
  )) +
    ggplot2::geom_rect(data = sig_dat[sig_dat$p >= 0.05 | is.na(sig_dat$p),], # add in grey rectangles if not sig.
              fill = "black",
              xmin = -Inf, xmax = Inf,
              ymin = -Inf, ymax = Inf,
              alpha = 0.15, inherit.aes = F) + # inherit.aes caused factors to lose order.
    geom_vline(xintercept = null_value, linetype = 2) + # add in vertical line at 0
    ggplot2::geom_point() + geom_errorbarh(height = .1)+
    ggplot2::facet_grid( # grid by moderation and outcome
      rows = vars(moderation),
      cols = vars(outcome),
      scales = "free_y",
      space = "free_y"
    ) +
    labs(y = " ", x = effect_size) + theme(text = element_text(family = "serif")) +
    ggplot2::scale_y_discrete(labels = c("Baseline" = expression(bold(Baseline)), parse = T)) +
    ggplot2::theme_bw() +
    ggplot2::theme(strip.text.y = ggplot2::element_text(angle = 0),
                   strip.background.y = ggplot2::element_blank(),
                   text = ggplot2::element_text(family = "serif"))
  if(!leading_zero){
    p <- p + scale_x_continuous(labels = function(x) gsub("^0\\.",".",x))
  }
  p
}

# models <- list(
# "Path 1" = lm.lp_mod,
# "Path 2" = lm.lo_mod,
# "Path 3" = lp.lo_mod,
# "Path 4" = lp.fo_mod,
# "Path 6" = lm.fo_mod)

# library(metaSEM); library(msemtools)
# library(data.table); library(tidyverse)
#
# mod1 = meta3(drink_yi, drink_vi, study_id, data = msemtools::conigrave20) %>%
#   moderate(Gender, Age, Cohort)
#
# mod2 = meta3(risk_short_yi, risk_short_vi, study_id, data = msemtools::conigrave20) %>%
#   moderate(Gender, Age, Cohort)
#
# mod3 = meta3(risk_long_yi, risk_long_vi, study_id, data = msemtools::conigrave20) %>%
#   moderate(Gender, Age, Cohort)
#
# models = list("Current drinker" =  mod1,
#               "Short-term risk" =  mod2,
#               "Long-term risk"  =  mod3)
# #
# moderation_matrix("Current drinker" =  mod1,
#                   "Short-term risk" =  mod2,
#                   "Long-term risk"  =  mod3
#                   )
# summary(mod1)
