% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/publication_bias.r
\name{PET_PEESE}
\alias{PET_PEESE}
\title{PET_PEESE}
\usage{
PET_PEESE(
  m,
  transf = NULL,
  digits = 2,
  alpha = 0.1,
  type = c("PET-PEESE", "PET", "PEESE"),
  pattern = "{est} [95\% CI {lower}, {upper}]"
)
}
\arguments{
\item{transf}{function with which to transform results}

\item{digits}{number of digits for rounding}

\item{alpha}{the alpha threshold used in PET-PEESE. Defaults to 0.1 (10% as per Stanley 2017)}

\item{type}{Should PET-PEESE, PET or PEESE be used?}

\item{x}{a metaSEM or meta_ninja model}
}
\description{
Calculates a PET-PEESEE adjusted estimate
}
\details{
PET-PEESE uses meta-regression in order to adjust estimates for publication bias. When there is publication bias the sampling variance becomes correlated with effect size. By using the standard error (PET) or the sampling variance (PEESE) as moderators in a meta-regression, estimates (the intercept) can be made which partial out the correlation between effect size and variance. PET-PEESE first tests whether the variance component is a significant (p < 0.05) predictor of effect size. If it is, PEESE is used. Otherwise, PET is used.
}
