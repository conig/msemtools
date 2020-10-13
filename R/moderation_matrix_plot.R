#' moderation_matrix
#'
#' Plots a moderation matrix
#' @param ... meta_ninja models all sharing a common data name
#' @param effect_size a string
#' @param moderators a list of moderators to include, order retained.
#' @param null_value a scalar indicating non-sigificance (where the dashed line will be drawn).
#' @import ggplot2 data.table
#' @export

moderation_matrix <- function(..., effect_size = "Effect size", moderators = NULL,
                              null_value = 0){

  models <- list(...)

  dat_list <- lapply(models, function(x) coef(x, everything()))
  #return(dat_list)
  #return(dat_list)
  DL = lapply(seq_along(dat_list), function(x){
    dat_list[[x]]$outcome = names(dat_list)[x]
    dat_list[[x]]
  }) %>% do.call(rbind, .)  %>% data.table::data.table()

  DL = DL[type != "effect size", .(
    y = metafor::transf.ztor(est),
    lower = metafor::transf.ztor(lower),
    upper = metafor::transf.ztor(upper),
    cluster, moderation,
    outcome, type,
    model_p)]

  graph_dat = DL

  if(is.null(moderators)){
  mod_levels <- unique(graph_dat$moderation)
  mod_levels <- mod_levels[mod_levels != "Baseline"]
  }else{
    mod_levels = moderators
  }
  mod_levels = c(mod_levels, "Baseline")

  graph_dat$moderation = factor(graph_dat$moderation, levels = mod_levels)
  graph_dat = graph_dat[!is.na(graph_dat$moderation),]
  graph_dat$outcome = factor(graph_dat$outcome)
  graph_dat$cluster = factor(graph_dat$cluster)

  final_dat = graph_dat %>% # give baseline a p value
   dplyr:: mutate(model_p = tidyr::replace_na(model_p, 0)) %>%  # Baseline p is NA. I want it to act as sig.
    na.omit()


  sig_dat  <- final_dat[, .(p = mean(model_p, na.rm = T), y = 0, cluster = NA) , by = c("outcome", "moderation")]
  # sig_dat = final_dat %>% # this table summarises moderation p values
  #   dplyr::group_by(outcome, moderation) %>%
  #   dplyr::summarise(p = mean(model_p, na.rm = T), y = 0, cluster = NA) %>% # empty y and cluster vars set up for ggplot2
  #   tidyr::complete(outcome, moderation) %>%  # include zero sums so no data panels are grey
  #   data.table::data.table()

  sig_dat$cluster = factor(sig_dat$cluster, levels = levels(final_dat$cluster))

  ggplot(final_dat, aes(
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
    ggplot2::theme_bw() + geom_vline(xintercept = null_value, linetype = 2) + # add in vertical line at 0
    ggplot2::geom_point() + geom_errorbarh(height = .1)+
    ggplot2::facet_grid( # grid by moderation and outcome
      rows = vars(moderation),
      cols = vars(outcome),
      scales = "free_y",
      space = "free_y"
    ) +
    labs(y = " ", x = effect_size) + theme(text = element_text(family = "serif")) +
    scale_x_continuous(labels = function(x) gsub("0\\.",".",x)) +
    scale_y_discrete(labels = c("Baseline" = expression(bold(Baseline)), parse = T))

}

# library(metaSEM); library(msemtools)
#
# mod1 = meta3(drink_yi, drink_vi, study_id, data = msemtools::conigrave20) %>%
#   moderate(Gender, Age, Cohort)
#
# mod2 = meta3(risk_short_yi, risk_short_vi, study_id, data = msemtools::conigrave20) %>%
#   moderate(Gender, Age, Cohort)
# #
# moderation_matrix("Current drinker" =  mod1,
#                   "Short term risk" =  mod2)
# summary(mod1)
