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
#' @param cluster study_id column
#' @param author the name of the author column
#' @param year the name of the year column
#' @param moderator.shape a scalar ggplot2 geom_point shape value
#' @param moderator.size a scalar. ggplot2 geom_point size value
#' @param pooled.shape a scalar ggplot2 geom_point shape value
#' @param pooled.size a scalar ggplot2 geom_point size value
#' @param font A string. The name of a font family. Defaults to serif.
#' @export ninjaForest
#' @importFrom dplyr filter select %>% mutate
#' @importFrom ggplot2 ggplot aes geom_point geom_errorbarh theme_classic scale_x_continuous facet_grid geom_vline ylab element_text
#' @importFrom stats reorder

#test arguments:
#xlab = "effect size"; baseline_name = "All" ; transform = function(x) papertools::logit2prob(x) ; factor.levels = NULL; vline = 0; cluster = NULL; author = NULL; year = NULL

ninjaForest = function(model,
                       xlab = "Effect size",
                       transform = NULL,
                       baseline_name = "All",
                       factor.levels = NULL,
                       vline = NULL,
                       cluster = NULL,
                       author = NULL,
                       year = NULL,
                       moderator.shape = 23,
                       moderator.size = 3.2,
                       summary.shape = 23,
                       summary.size = 4,

                       font = "serif") {
  if (!"meta_ninja" %in% class(model)) {
    stop("ninjaForest must be provided objects of class 'meta-ninja")
  }

  #find missing variables
  data = model$data

  if (is.null(year)) {
    year = find_year(data)
  }
  if (is.null(author)) {
    author = find_author(data)
  }
  if (is.null(cluster)) {
    cluster = model$cluster
  }

  #this chunk sets up summary object
  df = model$table
  b_model = model$models$Baseline
  #check that all moderators are factor, otherwise warn user
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

  #### next we separte out the bits of the summary table we want to plot.
  baseline = df %>%
    dplyr::filter(type %in% c("Baseline"))

  if (is.null(factor.levels)) {
    not_present = factor.levels[!factor.levels %in% df$model.name]
    if (length(not_present) > 0) {
      stop(
        paste0(
          "The following supplied factor levels were not found in model.name: '",
          paste0(not_present, collapse = "', '"),
          "'."
        )
      )
    }

    mod_data = df %>%
      dplyr::filter(type %in% c("factor level"))
  } else{
    mod_data = df %>%
      dplyr::filter(type == "factor level" &
                      model.name %in% factor.levels)
  }

  #prepare mod and baseline data
  mod_data =  mod_data %>%
    mutate(type = "moderator", setting = "Pooled")

  baseline = baseline %>%
    mutate(type = "baseline", setting = "Pooled")

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
  summary$year = c(1,rev)
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
    data[data[,cluster] == x, author] %>% unlist() %>% .[1]
  }) %>% unlist

  dat$year = lapply(dat$cluster, function(x) {
    x = as.numeric(as.character(x))
    data[data[,cluster] == x, year] %>% unlist() %>% .[1]
  }) %>% unlist
  dat = dat[order(dat$year), ]
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
  dat = dat[order(dat$year), ]

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

  # ------------------------------------- plotting

  plot = ggplot(dat, aes(
    y = reorder(cluster, year),
    x = est,
    xmin = lower,
    xmax = upper
  )) + geom_point(color = "black") +
    geom_errorbarh(height = .1) +

    geom_vline(xintercept = vline, #add horizontal line
               color = 'black',
               linetype = 'dashed') +

    geom_point( #add summary points
      data = dat[dat$type == "moderator", ],
      color = 'black',
      shape = moderator.shape,
      size = moderator.size,
      fill = "white"
    ) +

    geom_point( #add summary points
      data = dat[dat$type == "baseline", ],
      color = 'black',
      shape = summary.shape,
      size = summary.size,
      fill = "black"
    ) +


    #add the CI error bars
    #Specify the limits of the x-axis and relabel it to something more meaningful
    scale_x_continuous(name = xlab) +
    ylab(NULL) +

    facet_grid(setting ~ ., scales = 'free', space = 'free') +
    theme_classic()

  if (!is.null(font)) {
    plot = plot + theme(text = element_text(family = font))
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
#' @param alpha a scalar. Sets alpha transparency.
#' @param font a character string. Set's font family. Defaults to times new roman ('serif')
#' @param CI a bool. If true, error bars will be plotted. totally unnecesary, but kind of fun.
#' @importFrom metafor regtest
#' @importFrom ggplot2 aes theme xlab geom_point coord_flip scale_x_reverse geom_line geom_segment geom_errorbar element_text
#' @export funnel_plot

#test arguments:
funnel_plot = function(model,
                       xlab = "Estimate",
                       ylab = "Standard Error",
                       alpha = .5,
                       font = "serif",
                       CI = F) {
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
  reg_test = (regtest(x = data$y, vi = data$v))
  fp = ggplot(data, aes(
    x = se,
    y = y,
    ymin = lower,
    ymax = upper
  ))
  fp = fp + theme_classic() +
    geom_point(alpha = alpha) +
    xlab(ylab) + ylab(xlab) +
    geom_line(
      aes(x = se.seq, y = ll95),
      linetype = 'dashed',
      data = dfCI,
      size = .8,
      inherit.aes = FALSE
    ) +
    geom_line(
      aes(x = se.seq, y = ul95),
      linetype = 'dashed',
      data = dfCI,
      size = .8,
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
      linetype = 'solid',
      data = dfCI,
      size = 1,
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
