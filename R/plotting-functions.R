#' diamond_df
#'
#' This function creates diamonds for moderators and summary statistics.
#' @param plot a ggplot2
#' @param data a data.frame
#' @param fill ggplot2 fill value
#' @param colour character. A hex code. If provided, gives diamond coloured border
#' @importFrom ggplot2 aes

add_diamond = function(plot, data, fill = "grey20", colour = NA) {
  diamond_shape = data.frame(
    x = c(data$lower, data$est, data$upper, data$est),
    y = c(data$position,data$position + .4,data$position,data$position - .4),
    names = c("xmin", "ymax", "xmax", "ymax"),
    setting = "Pooled"
  )
  plot = plot + ggplot2::geom_polygon(
    data = diamond_shape,
    aes(x = x, y = y),
    fill = fill,
    colour = colour,
    inherit.aes = F
  )
  plot
}

#' ninjaForest
#'
#' This function is used for plotting
#'
#' @param model an object belonging to the class 'meta_ninja'. These objects are created by the function 'meta3_moderation'.
#' @param ... moderators to plot character strings only
#' @param xlab a string.
#' @param effect_label a string. If provided relabels "effect size" in facetting.
#' @param transform a function. If supplied effect sizes are transformed by this function
#' @param baseline_name a string. The label for the baseline model
#' @param factor.levels  a character vector. If supplied, only the factor.levels specified will be plotted.
#' @param facet_by a colname. Facets effect sizes by a supplied variable.
#' @param vline a scalar. Dictates the x-intercept (the dashed line).
#' @param author the name of the author column
#' @param year the name of the year column
#' @param order_magnitude a bool. If true, effect sizes are ordered by magnitude
#' @param include_weights a bool. If true, effect sizes become transparent proportional to their weight
#' @param moderator.shape a scalar ggplot2 geom_point shape value
#' @param moderator.size a scalar. ggplot2 geom_point size value
#' @param summary.shape a scalar ggplot2 geom_point shape value
#' @param summary.size a scalar ggplot2 geom_point size value
#' @param diamond a bool. If true, diamond is created for summary measure
#' @param moderator_diamond a bool. If true, diamond is created for moderators
#' @param font A string. The name of a font family. Defaults to serif
#' @export forest_plot
#' @importFrom dplyr filter select %>% mutate
#' @import ggplot2

#test arguments:
#xlab = "effect size"; baseline_name = "All" ; transform = function(x) papyr::logit2prob(x) ; factor.levels = NULL; vline = 0; cluster = NULL; author = NULL; year = NULL; moderator.shape = 23;moderator.size = 3.2;summary.shape = 23;summary.size = 4;font = "serif";diamond = T; moderator_diamond = F
#TODO create method for meta3. Add heterogeneity indicies to plot with argument
forest_plot = function(model,
                       ...,
                       xlab = "Effect size",
                       effect_label = NULL,
                       transform = NULL,
                       baseline_name = "Pooled estimate",
                       factor.levels = NULL,
                       facet_by = NULL,
                       vline = NULL,
                       author = NULL,
                       year = NULL,
                       order_magnitude = FALSE,
                       include_weights = FALSE,
                       moderator.shape = 23,
                       moderator.size = 3,
                       summary.shape = 23,
                       summary.size = 4,
                       diamond = TRUE,
                       moderator_diamond = FALSE,
                       font = "serif") {

  if (!any(c("meta_ninja", "meta") %in% class(model))) {
    stop("must be provided objects of class 'meta-ninja")
  }

  mods = unlist(list(...))

  #find missing variables
  if("meta" %in% class(model)){
    data_name = model$call$data
    cluster = as.character(model$call$cluster)
    yi = model$call$y
    vi = model$call$v
  }else{
    data_name = model$data
    cluster = model$cluster
    yi = model$models[[1]]$call$y
    vi = model$models[[1]]$call$v
  }

  data = eval(parse(text = data_name))

  if (is.null(year)) {
    year = find_year(data)
  }
  if (is.null(author)) {
    author = find_author(data)
  }

  #this chunk sets up summary object
  if ("meta" %in% class(model)) {

    dat = get_effects(model, baseline = T)
    dat$order = seq_along(dat$est)

  } else{

    dat = coef(model, mods)

    if("numeric" %in% dat$type) warning("Only categorical moderators will be plotted")


  } # if statement end

  dat <- data.table::data.table(dat)
  dat[, cluster_mean := weighted.mean(est, (1/(SE^2)), na.rm = TRUE) , by = "cluster"]

  key <- data.table::data.table(data)[,c(cluster, year, author), with = FALSE]
  names(key) = c("cluster","year","author")
  key$cluster = as.character(key$cluster)

  dat <- dplyr::left_join(dat, key, by = "cluster")

  # facet shape merge in extra data ------------------------------------------------
  if (!is.null(facet_by)) {
    temp_dat = data[, c(facet_by,
                        cluster,
                        as.character(yi),
                        as.character(vi))]
    names(temp_dat)= c("effect_shape", "cluster","est","SE")
    temp_dat$est = as.numeric(temp_dat$est)
    temp_dat$cluster = as.character(temp_dat$cluster) # make types the same as in dat
    temp_dat$SE = sqrt(temp_dat$SE) # square root the sampling variance to get the SE.
    temp_dat$effect_shape = as.character(temp_dat$effect_shape)
    temp_dat$effect_shape[is.na(temp_dat$effect_shape)] = "Not reported"

    dat = dplyr::left_join(dat, temp_dat, by = c("est","SE","cluster"))
  }else{
    dat$effect_shape = "no facet"
  }

  dat$year[is.na(dat$year)] = dat$order[is.na(dat$year)]

  effects = dat[dat$type == "effect size",]

  dat$cluster[dat$type == "effect size"] = paste(effects$author, effects$year)
  dat$cluster[dat$cluster == "Baseline"] = baseline_name

  # Apply transformation
  if (!is.null(transform)) {
    dat = dat %>%
      dplyr::mutate(
        est = transform(est),
        lower = transform(lower),
        upper = transform(upper)
      )
  }

  if (is.null(vline)) {
    if (!is.null(transform)) {
      vline = transform(0)
    } else {
      vline = 0
    }
  }

  if(order_magnitude){
    dat$year[dat$setting == "Effect sizes"] <-
      order(dat$cluster_mean[dat$setting == "Effect sizes"]) + nrow(dat[setting != "Effect sizes"])
  }

  dat = dat[order(dat$year),]



  dat$position = 1:nrow(dat)

  #  include weights--------------------------------------------------

    dat$weight = 1 / dat$SE^2
    dat$weight[dat$type != "effect size"] = max(dat$weight)

  # re-label effect size.
    if(!is.null(effect_label)){

      dat$setting[dat$setting == "Effect sizes"] = effect_label
      dat$setting = forcats::fct_relevel(factor(dat$setting), "Pooled", after = Inf) # make pooled last.
    }


  # ------------------------------------------------------------------- plotting

  plot = ggplot(dat, aes(
    y = stats::reorder(cluster, as.numeric(as.character(year))),
    x = est,
    xmin = lower,
    xmax = upper,
    shape = effect_shape
  ))

  if(include_weights){
    plot = plot + aes(alpha = weight) + ggplot2::scale_alpha(range = c(0.4,1)) +
      ggplot2::guides(alpha = FALSE)
  }

  plot = plot + geom_point(color = "black") +
    geom_errorbarh(data = dat[dat$type == "effect size", ] , height = .1) +

    geom_vline(xintercept = vline,
               #add horizontal line
               color = 'black',
               linetype = 'dashed') +

    if (!diamond) {
      plot = plot + geom_point(
        #add summary points
        data = dat[dat$type == "baseline",],
        color = 'black',
        shape = summary.shape,
        size = summary.size,
        fill = "black"
      )

    }

  #add the CI error bars
  #Specify the limits of the x-axis and relabel it to something more meaningful
  plot = plot + scale_x_continuous(name = xlab) +
    ylab(NULL)

  plot = plot + facet_grid(setting ~ ., scales = 'free', space = 'free') +
    ggplot2::theme_classic()
  #if diamond ------------------------------------
  if (diamond) {
    bd = dat[dat$type == "Baseline",]
    plot = plot %>% add_diamond(bd)
  } else {
    plot = plot + geom_errorbarh(data = dat[dat$type == "baseline", ] , height = .1)
  }

  if (moderator_diamond) {
    types = dat$cluster[dat$type == "moderator level"]
    for (i in types) {
      plot = plot %>% add_diamond(dat[dat$cluster == i, ], fill = "white", colour = "grey20")
    }
  } else {
    plot = plot + geom_errorbarh(data = dat[dat$type == "moderator level", ] , height = .25) + geom_point(
      #add summary points
      data = dat[dat$type == "moderator level",],
      color = 'black',
      shape = moderator.shape,
      size = moderator.size,
      fill = "white"
    )
  }

  # change font ---------------------------------
  if (!is.null(font)) {
    plot = plot + ggplot2::theme(text = ggplot2::element_text(family = font))
  }

  if(nrow(dat[dat$setting == "Pooled",]) < 2){
  plot = plot + ggplot2::theme(
      strip.background = ggplot2::element_blank(),
      strip.text = ggplot2::element_blank()
    )
  }

  # turn off shape if not used ------------------
  if(length(unique(dat$effect_shape)) == 1){
    plot = plot + ggplot2::theme(legend.position = "none")
  }

  return(plot)

}

#' ninjaFunnel
#'
#' This function is used for plotting funnel plots
#'
#' @param model an object belonging to the class 'meta_ninja'. These objects are created by the function 'meta3_moderation'.
#' @param xlab a character string. Label for the x-axis.
#' @param ylab a character string. Label for the y-axis.
#' @param font a character string. Set's font family. Defaults to times new roman ('serif')
#' @param CI a bool. If true, error bars will be plotted. totally unnecesary, but kind of fun.
#' @param alpha a scalar. Sets alpha transparency.
#' @param shape a scalar. The ggplot2 shape for the poitns
#' @param size a scalar. The ggplot2 size value for the points
#' @param CI_linetype ggplot2 linetype for confidence intervals
#' @param CI_size ggplot2 size value for confidence intervals
#' @param pool_linetype ggplot2 linetype for pooled estimate
#' @param pool_size ggplot2 size value for pooled estimate
#' @param density a bool. If True, plots densit of points.
#' @importFrom ggplot2 aes theme xlab geom_point coord_flip scale_x_reverse geom_line geom_segment geom_errorbar element_text stat_density_2d stat scale_fill_gradient
#' @export funnel_plot

#test arguments:
funnel_plot = function(model,
                       xlab = "Estimate",
                       ylab = "Standard Error",
                       font = "serif",
                       CI = F,
                       alpha = .6,
                       shape = 19,
                       size = 1.5,
                       CI_linetype = "dashed",
                       CI_size = .6,
                       pool_linetype = "dotted",
                       pool_size = .5,
                       density = F
                       ) {
  t_model = NULL
  if ("meta_ninja" %in% class(model)) {
    t_model = model$models[[1]]
  }
  if ("meta" %in% class(model)) {
    t_model = model
  }

  requireNamespace("metafor", quietly = TRUE)

  data = t_model$data

  if (!is.null(data$x1)) {
    stop(
      "Moderated models cannot be used to create funnel plots as they have no single estimate. Please use a baseline model.",
      call. = F
    )
  }

  data$se = sqrt(data$v)
  estimate = extractData(t_model)$estimate
  se = extractData(t_model)$SE
  se.seq = seq(0, max(data$se), 0.001)
  ll95 = estimate - (1.96 * se.seq)
  ul95 = estimate + (1.96 * se.seq)
  meanll95 = estimate - 1.96 * se
  meanul95 = estimate + 1.96 * se

  data$lower = data$y  - 1.96 * data$se
  data$upper = data$y + 1.96 * data$se
  dfCI = data.frame(ll95, ul95, se.seq, estimate, meanll95, meanul95)
  reg_test = (metafor::regtest(x = data$y, vi = data$v))
  fp = ggplot2::ggplot(data, ggplot2::aes(
    x = se,
    y = y,
    ymin = lower,
    ymax = upper
  ))
  fp = fp + ggplot2::theme_classic()

  if (density) {
    fp = fp +
      ggplot2::stat_density_2d(
        ggplot2::aes(fill = stat(level)),
        bins = 7,
        colour = "white",
        geom = "polygon",
        alpha = 0.2,
        show.legend = F
      ) +
      ggplot2::scale_fill_gradient(low = "grey60", high = "grey30")
  }
  fp = fp +
    ggplot2::geom_point(alpha = alpha, shape = shape, size = size) +
    ggplot2::xlab(ylab) + ggplot2::ylab(xlab) +
    ggplot2::geom_line(
      ggplot2::aes(x = se.seq, y = ll95),
      linetype = CI_linetype,
      data = dfCI,
      size = CI_size,
      inherit.aes = FALSE
    ) +
    ggplot2::geom_line(
      ggplot2::aes(x = se.seq, y = ul95),
      linetype = CI_linetype,
      data = dfCI,
      size = CI_size,
      inherit.aes = FALSE
    ) +

    #geom_segment(aes(x = min(se.seq), y = meanll95, xend = max(se.seq), yend = meanll95), linetype='dotted', data=dfCI) +
    ggplot2::geom_segment(
      ggplot2::aes(
        x = min(se.seq),
        y = estimate,
        xend = max(se.seq),
        yend = estimate
      ),
      linetype = pool_linetype,
      data = dfCI,
      size = pool_size,
      inherit.aes = FALSE
    ) +
    #geom_segment(aes(x = min(se.seq), y = meanul95, xend = max(se.seq), yend = meanul95), linetype='dotted', data=dfCI) +
    ggplot2::scale_x_reverse() +
    ggplot2::coord_flip()
  if (CI == T) {
    fp = fp + ggplot2::geom_errorbar()
  }
  if (!is.null(font)) {
    fp = fp + ggplot2::theme(text = ggplot2::element_text(family = font))
  }

  return(list(plot = fp, reg_test = reg_test))
}

#' forest_height
#'
#' Use this function to resize funnel plots in rmarkdown.
#'
#' @param meta3_plot a moderated meta3 plot
#' @param slope the numeric value to multiple number of rows by
#' @param intercept the numeric constant
#' @export forest_height

forest_height = function(meta3_plot, slope = .12, intercept = .52){
  length(unique(meta3_plot$data$cluster)) * slope + intercept
}

#' #' plot_mods
#' #'
#' #' Plot all moderators and data points in a concise manner for multiple meta-analyses
#' #' @param model
#' #' @param ...
#'
#' plot_mods = function(model,...){
#'
#'   mod_calls = model$calls[-1]
#'
#'   calls_eval = lapply(seq_along(mod_calls), function(x){
#'     #message(x)
#'     out = eval(as.expression(mod_calls[[x]]$x), envir = tibble::as_tibble(eval(model$data)))
#'     y = eval(as.expression(mod_calls[[x]]$y), envir = tibble::as_tibble(eval(model$data)))
#'     v = eval(as.expression(mod_calls[[x]]$v), envir = tibble::as_tibble(eval(model$data)))
#'     model_name = as.character(mod_calls[[x]]$model.name)
#'     out = data.table::data.table(cbind(out, y, v))
#'     out$moderator = model_name
#'     out = reshape2::melt(out, id.vars = c("y","v","moderator"))
#'     dplyr::select(na.omit(out[out$value == 1,]), -value)
#'   })
#'
#'   effect_dat = do.call(rbind, calls_eval)
#'   effect_dat$se = sqrt(effect_dat$v)
#'   effect_dat$lower = effect_dat$y - 1.96*effect_dat$se
#'   effect_dat$upper = effect_dat$y + 1.96*effect_dat$se
#'   effect_dat$se = NULL; effect_dat$v = NULL
#'   effect_dat$type = "effect size"
#'
#'   mod_summary = dplyr::filter(coef(model, everything()), type == "moderator level")
#'   mod_summary = dplyr::select(mod_summary, y = est, moderator = moderation, variable = cluster, lower, upper, type)
#'
#'   all_dat = rbind(mod_summary, effect_dat)
#'
#'   p = ggplot2::ggplot(all_dat,
#'                       ggplot2::aes(
#'                         x = variable,
#'                         y = y,
#'                         ymin = lower,
#'                         ymax = upper,
#'                         group = variable
#'                       )) +
#'     geom_boxplot(data = all_dat[all_dat$type == "effect size", ]) +
#'     geom_point(data = all_dat[all_dat$type == "effect size", ], position =
#'                  position_dodge(width = 0.5), alpha = .5) +
#'     geom_errorbar(data = all_dat[all_dat$type == "effect size", ],
#'                   width = 0.05,
#'                   position = position_dodge(width = 0.5)) +
#'     # special ones
#'     geom_point(data = all_dat[all_dat$type == "moderator level",], shape = 5) +
#'     geom_errorbar(data = all_dat[all_dat$type == "moderator level", ],
#'                   width = 0.05,
#'                   position = position_dodge(width = 0.5))+
#'
#'
#'     facet_wrap( ~
#'                   moderator, scales = "free_y") +coord_flip() + theme_bw() + labs(y = "")
#'
#'   return(p)
#' }

