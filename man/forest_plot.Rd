% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plotting-functions.R
\name{forest_plot}
\alias{forest_plot}
\title{ninjaForest}
\usage{
forest_plot(
  model,
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
  font = "serif"
)
}
\arguments{
\item{model}{an object belonging to the class 'meta_ninja'. These objects are created by the function 'meta3_moderation'.}

\item{...}{moderators to plot character strings only}

\item{xlab}{a string.}

\item{effect_label}{a string. If provided relabels "effect size" in facetting.}

\item{transform}{a function. If supplied effect sizes are transformed by this function}

\item{baseline_name}{a string. The label for the baseline model}

\item{factor.levels}{a character vector. If supplied, only the factor.levels specified will be plotted.}

\item{facet_by}{a colname. Facets effect sizes by a supplied variable.}

\item{vline}{a scalar. Dictates the x-intercept (the dashed line).}

\item{author}{the name of the author column}

\item{year}{the name of the year column}

\item{order_magnitude}{a bool. If true, effect sizes are ordered by magnitude}

\item{include_weights}{a bool. If true, effect sizes become transparent proportional to their weight}

\item{moderator.shape}{a scalar ggplot2 geom_point shape value}

\item{moderator.size}{a scalar. ggplot2 geom_point size value}

\item{summary.shape}{a scalar ggplot2 geom_point shape value}

\item{summary.size}{a scalar ggplot2 geom_point size value}

\item{diamond}{a bool. If true, diamond is created for summary measure}

\item{moderator_diamond}{a bool. If true, diamond is created for moderators}

\item{font}{A string. The name of a font family. Defaults to serif}
}
\description{
This function is used for plotting
}
