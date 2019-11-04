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
#' @param xlab a string.
#' @param transform a function. If supplied effect sizes are transformed by this function
#' @param baseline_name a string. The label for the baseline model
#' @param factor.levels  a charcater vector. If supplied, only the factor.levels specified will be plotted.
#' @param vline a scalar. Dictates the x-intercept (the dashed line).
#' @param author the name of the author column
#' @param year the name of the year column
#' @param moderator.shape a scalar ggplot2 geom_point shape value
#' @param moderator.size a scalar. ggplot2 geom_point size value
#' @param summary.shape a scalar ggplot2 geom_point shape value
#' @param summary.size a scalar ggplot2 geom_point size value
#' @param diamond a bool. If true, diamond is created for summary measure
#' @param moderator_diamond a bool. If true, diamond is created for moderators
#' @param font A string. The name of a font family. Defaults to serif
#' @export forest_plot
#' @importFrom dplyr filter select %>% mutate
#' @importFrom ggplot2 ggplot aes geom_point geom_errorbarh theme_classic scale_x_continuous facet_grid geom_vline ylab element_text

#test arguments:
#xlab = "effect size"; baseline_name = "All" ; transform = function(x) papertools::logit2prob(x) ; factor.levels = NULL; vline = 0; cluster = NULL; author = NULL; year = NULL; moderator.shape = 23;moderator.size = 3.2;summary.shape = 23;summary.size = 4;font = "serif";diamond = T; moderator_diamond = F
#TODO create method for meta3. Add heterogeneity indicies to plot with argument
forest_plot = function(model,
                       xlab = "Effect size",
                       transform = NULL,
                       baseline_name = "Pooled estimate",
                       factor.levels = NULL,
                       vline = NULL,
                       author = NULL,
                       year = NULL,
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

  #find missing variables
  if("meta" %in% class(model)){
    data_name = model$call$data
    cluster = as.character(model$call$cluster)
  }else{
    data_name = model$data
    cluster = model$cluster
  }

  data = data_name %>%
    parse(text = .) %>%
    eval

  if (is.null(year)) {
    year = find_year(data)
  }
  if (is.null(author)) {
    author = find_author(data)
  }

  #this chunk sets up summary object
  if ("meta" %in% class(model)) {
    b_model <- model
    baseline <- extractData(model)
    baseline$model.name = "Baseline"
    mod_data = NULL


  } else{
    b_model = model$models$Baseline
    df = model$table

    numeric_moderators = data.frame(df[df$type == "numeric", "model.name"])
    if (nrow(numeric_moderators) > 0) {
      warning(
        paste0(
          "Only categorical moderators are plotted. The following moderators will be ignored: '",
          paste(numeric_moderators, collapse = "', '"),
          "'."
        ),
        call. = F
      )
    }

    baseline = df %>%
      dplyr::filter(type %in% c("Baseline"))

    #check that all moderators are factor, otherwise warn user

    #### next we separte out the bits of the summary table we want to plot.

    if (is.null(factor.levels)) {
      not_present = factor.levels[!factor.levels %in% df$model.name]
      if (length(not_present) > 0) {
        stop(
          paste0(
            "The following supplied moderator levels were not found in model.name: '",
            paste0(not_present, collapse = "', '"),
            "'."
          )
        )
      }

      mod_data = df %>%
        dplyr::filter(type %in% c("moderator level")) %>%
        mutate(type = "moderator", setting = "Pooled")
    } else{
      mod_data = df %>%
        dplyr::filter(type == "moderator level" &
                        model.name %in% factor.levels)
      #prepare mod and baseline data
      mod_data =  mod_data %>%
        mutate(type = "moderator", setting = "Pooled")
    }
  } # if statement end

  baseline = baseline %>%
    mutate(type = "baseline", setting = "Pooled")

  if(is.null(mod_data)) mod_data <- baseline[NULL,]

  summary = rbind(baseline, mod_data) %>%
    dplyr::select(
      cluster = model.name,
      k,
      n,
      est = estimate,
      lower = lbound,
      upper = ubound,
      type,
      setting
    )
  rev = rev(seq_len(nrow(summary)))
  rev = rev[-length(rev)]
  summary$year = c(1, rev)
  summary$cluster[summary$cluster == "Baseline"] = baseline_name

  dat = b_model$data %>%
    dplyr::mutate(
      se = sqrt(v),
      lower = (y - 1.96 * se),
      upper = (y + 1.96 * se),
      k = NA,
      n = NA
    ) %>%
    dplyr::mutate(type = "data", setting = "Effect sizes") %>%
    dplyr::select(n, k, est = y, lower, upper, type, setting, cluster)

  dat$author = lapply(dat$cluster, function(x) {
    x = as.numeric(as.character(x))
    data[data[, cluster] == x, author] %>% unlist() %>% .[1]
  }) %>% unlist

  dat$year = lapply(dat$cluster, function(x) {
    x = as.numeric(as.character(x))
    data[data[, cluster] == x, year] %>% unlist() %>% .[1]
  }) %>% unlist %>%
    as.character %>% ## in one case year was coming out as a character
    as.numeric # so I'm converting it to numeric again to make sure.

  dat = dat[order(dat$year),]
  dat$cluster = paste(dat$author, dat$year)
  #create empty columns in summary for author and year
  fun = dat[, c("cluster",
                "k",
                "n",
                "est",
                "lower",
                "upper",
                "type",
                "setting",
                "year")]
  #need to make summary have the same column names and orderings.
  dat = rbind(fun,
              summary)
  dat = dat[order(dat$year),]

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

  dat$position = 1:nrow(dat)

  # ------------------------------------------------------------------- plotting

  plot = ggplot(dat, aes(
    y = stats::reorder(cluster, year),
    x = est,
    xmin = lower,
    xmax = upper
  )) + geom_point(color = "black") +
    geom_errorbarh(data = dat[dat$type == "data", ] , height = .1) +

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
    theme_classic()
  #if diamond ------------------------------------
  if (diamond) {
    bd = dat[dat$type == "baseline",]
    plot = plot %>% add_diamond(bd)
  } else {
    plot = plot + geom_errorbarh(data = dat[dat$type == "baseline", ] , height = .1)
  }

  if (moderator_diamond) {
    types = dat$cluster[dat$type == "moderator"]
    for (i in types) {
      plot = plot %>% add_diamond(dat[dat$cluster == i, ], fill = "white", colour = "grey20")
    }
  } else {
    plot = plot + geom_errorbarh(data = dat[dat$type == "moderator", ] , height = .25) + geom_point(
      #add summary points
      data = dat[dat$type == "moderator",],
      color = 'black',
      shape = moderator.shape,
      size = moderator.size,
      fill = "white"
    )
  }

  # change font ---------------------------------
  if (!is.null(font)) {
    plot = plot + theme(text = element_text(family = font))
  }

  if(nrow(mod_data) == 0){
  plot = plot + theme(
      strip.background = element_blank(),
      strip.text = element_blank()
    )
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
#' @importFrom metafor regtest
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
  fp = ggplot(data, aes(
    x = se,
    y = y,
    ymin = lower,
    ymax = upper
  ))
  fp = fp + theme_classic()

  if(density){
    fp = fp +
      stat_density_2d(aes(fill = stat(level)),bins = 7,colour = "white",geom = "polygon", alpha = 0.2, show.legend = F) +
      scale_fill_gradient(low = "grey60", high = "grey30")
  }
  fp = fp +
    geom_point(alpha = alpha, shape = shape, size = size) +
    xlab(ylab) + ylab(xlab) +
    geom_line(
      aes(x = se.seq, y = ll95),
      linetype = CI_linetype,
      data = dfCI,
      size = CI_size,
      inherit.aes = FALSE
    ) +
    geom_line(
      aes(x = se.seq, y = ul95),
      linetype = CI_linetype,
      data = dfCI,
      size = CI_size,
      inherit.aes = FALSE
    ) +

    #geom_segment(aes(x = min(se.seq), y = meanll95, xend = max(se.seq), yend = meanll95), linetype='dotted', data=dfCI) +
    geom_segment(
      aes(
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
    scale_x_reverse() +
    coord_flip()
  if (CI == T) {
    fp = fp + geom_errorbar()
  }
  if (!is.null(font)) {
    fp = fp + theme(text = element_text(family = font))
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

