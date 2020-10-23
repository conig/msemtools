msemtools
================
James Conigrave
05/02/2019

The purpose of ‘msemtools’ (metaSEM tools) is to help run meta-analyses
with metaSEM efficiently. This project should be considered to be in
Beta. Please report any issues.

## Installation

To install msemtools run the following code:

``` r
#install.packages("remotes")
remotes::install_github("conig/msemtools")
```

## Running analyses

This packages is a wrapper around the metaSEM package. It does not
perform any calculations itself, but rather converts your instructions
into metaSEM syntax. Resultant models are returned along with formatted
output. Currently only meta3 is supported.

# Example

To demonstrate this software, I use the included dataset conigrave20.
This data is from a meta-analysis looking at Indigenous Australian
drinking patterns. The outcomes are the log-odds of being a current
drinker, being at short-term risk, and long-term risk from drinking.

``` r
library(msemtools)
```

## Running a basic model with metaSEM

To run a metaSEM model, you need an effect size, corresponding sampling
variance, and information about how effect sizes are clustered.

``` r
current_d.0 <- meta3(
  y = drink_yi,
  v = drink_vi,
  cluster = study_id,
  data = conigrave20,
  model.name = "Current drinker"
)

summary(current_d.0)
```

    ## 
    ## Call:
    ## meta3(y = drink_yi, v = drink_vi, cluster = study_id, data = conigrave20, 
    ##     model.name = "Current drinker")
    ## 
    ## 95% confidence intervals: z statistic approximation (robust=FALSE)
    ## Coefficients:
    ##            Estimate Std.Error    lbound    ubound z value  Pr(>|z|)    
    ## Intercept 0.3644346 0.1340253 0.1017499 0.6271193  2.7191  0.006545 ** 
    ## Tau2_2    0.4459826 0.0887495 0.2720368 0.6199284  5.0252 5.029e-07 ***
    ## Tau2_3    0.3478567 0.1764483 0.0020244 0.6936891  1.9714  0.048674 *  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Q statistic on the homogeneity of effect sizes: 5518.62
    ## Degrees of freedom of the Q statistic: 98
    ## P value of the Q statistic: 0
    ## 
    ## Heterogeneity indices (based on the estimated Tau2):
    ##                               Estimate
    ## I2_2 (Typical v: Q statistic)   0.5559
    ## I2_3 (Typical v: Q statistic)   0.4336
    ## 
    ## Number of studies (or clusters): 37
    ## Number of observed statistics: 99
    ## Number of estimated parameters: 3
    ## Degrees of freedom: 96
    ## -2 log likelihood: 244.0044 
    ## OpenMx status1: 0 ("0" or "1": The optimization is considered fine.
    ## Other values may indicate problems.)

We’ve produced the baseline model. This model shows us the pooled
log-odds of current drinkers. The intercept shows that for included
studies, the pooled log-odds of drinking was 0.36 \[95%CI 0.10 - 0.63\].

I2\_2 shows the proportion of heterogeneity within cluster (study\_id).
I2\_3 shows the heterogeneity between studies.

You can see that the total heterogeneity is extremely high \~ 1.

## Moderation with msemtools

metaSEM allows users to test for the effect of moderators. To do this, a
categorical moderator (e.g. gender) must be converted to a matrix of
dummy variables. Msemtools automates this process.

``` r
m <- msemtools::character_matrix(conigrave20$Gender)
cbind.data.frame(m, conigrave20$Gender)[1:5,]
```

    ##   Mostly male Mixed gender Mostly female conigrave20$Gender
    ## 1           0            1             0       Mixed gender
    ## 2           1            0             0        Mostly male
    ## 3           0            0             1      Mostly female
    ## 4           1            0             0        Mostly male
    ## 5           0            0             1      Mostly female

But this happens internally so you don’t need to worry about it. Just
provide the baseline mode, and tell msemtools what variable you want to
moderate by.

``` r
moderation_object = current_d.0 %>% 
  moderate(Gender)
moderation_object
```

    ## Moderation results:
    ## 
    ## I2(2): 55.6%
    ## I2(3): 43.4%
    ## -------------------------------------
    ##   moderation  k  n R2_2 R2_3  p.value
    ## -------------------------------------
    ## 1     Gender 31 93  0.6    0 < 0.001*
    ## -------------------------------------
    ## All models converged.

The p-value demonstrates that the baseline model is significantly
improved by including gender as a moderator.

To get the specific values we can call summary on this object.
Additionally, we can request a transformation to something which is
easier to interpret than log-odds.

``` r
logit2prob = function(logit){
  odds <- exp(logit)
  prob <- odds / (1 + odds)
  return(prob)
}

summary(moderation_object,
        transf = logit2prob,
        t.name = "Proportion [95% CI]")
```

    ##   Moderator       k  n  Proportion [95% CI] Estimate SE   R^2(2) R^2(3)
    ## 1 Baseline        37 99 0.59 [0.53, 0.65]   0.36     0.13 -      -     
    ## 2 Gender          31 93 -                   -        -    0.60   0.00  
    ## 3 __Mostly male   16 39 0.69 [0.61, 0.77]   0.80     0.19 -      -     
    ## 4 __Mixed gender  12 12 0.52 [0.40, 0.64]   0.09     0.25 -      -     
    ## 5 __Mostly female 19 42 0.47 [0.38, 0.56]   -0.11    0.19 -      -     
    ##   p       
    ## 1 -       
    ## 2 < 0.001*
    ## 3 -       
    ## 4 -       
    ## 5 -

We can see that samples with more males had higher proportions of
current drinkers. We can also easily plot this results.

## forest plot

``` r
plot(moderation_object, "Gender", transform = logit2prob)
```

![](README_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

Author and year were automatically detected by looking for clues like
“et al”. But you can also manually specify them.

## formatting

If we want to add more moderators we just throw them into the moderation
function as argument separated by commas.

``` r
moderation_object2 = current_d.0 %>% 
  moderate(Gender, Age, Cohort)

moderation_object2
```

    ## Moderation results:
    ## 
    ## I2(2): 55.6%
    ## I2(3): 43.4%
    ## -------------------------------------
    ##   moderation  k  n R2_2 R2_3  p.value
    ## -------------------------------------
    ## 1     Gender 31 93 0.60 0.00 < 0.001*
    ## 2        Age 18 41 0.18 0.00 < 0.001*
    ## 3     Cohort 37 99 0.03 0.09    0.15 
    ## -------------------------------------
    ## All models converged.

We can then format this table with another function

``` r
summary(moderation_object2,
        transf = logit2prob,
        t.name = "Proportion [95% CI]") %>% 
  knitr::kable()
```

| Moderator         | k  | n  | Proportion \[95% CI\] | Estimate | \(SE\) | \(R^2_{(2)}\) | \(R^2_{(3)}\) | \(p\)      |
| :---------------- | :- | :- | :-------------------- | :------- | :----- | :------------ | :------------ | :--------- |
| Baseline          | 37 | 99 | 0.59 \[0.53, 0.65\]   | 0.36     | 0.13   | \-            | \-            | \-         |
| Gender            | 31 | 93 | \-                    | \-       | \-     | 0.60          | 0.00          | \< 0.001\* |
| \_\_Mostly male   | 16 | 39 | 0.69 \[0.61, 0.77\]   | 0.80     | 0.19   | \-            | \-            | \-         |
| \_\_Mixed gender  | 12 | 12 | 0.52 \[0.40, 0.64\]   | 0.09     | 0.25   | \-            | \-            | \-         |
| \_\_Mostly female | 19 | 42 | 0.47 \[0.38, 0.56\]   | \-0.11   | 0.19   | \-            | \-            | \-         |
| Age               | 18 | 41 | \-                    | \-       | \-     | 0.18          | 0.00          | \< 0.001\* |
| \_\_15-24 years   | 9  | 17 | 0.50 \[0.38, 0.62\]   | 0.00     | 0.25   | \-            | \-            | \-         |
| \_\_25-34 years   | 11 | 29 | 0.39 \[0.28, 0.50\]   | \-0.47   | 0.24   | \-            | \-            | \-         |
| \_\_35-49 years   | 11 | 18 | 0.65 \[0.48, 0.78\]   | 0.61     | 0.34   | \-            | \-            | \-         |
| \_\_50+           | 4  | 5  | 0.49 \[0.32, 0.66\]   | \-0.03   | 0.36   | \-            | \-            | \-         |
| Cohort            | 37 | 99 | \-                    | \-       | \-     | 0.03          | 0.09          | 0.15       |

## Describing moderated tables

A method is provided to convert moderated tables to paragraph
descriptions which can be rendered in rmarkdown.

``` r
report(moderation_object2, rmarkdown = FALSE, transf = logit2prob)
```

\[1\] “Thirty-seven studies (99 effects) reported data which could be
pooled. Inspecting the Q statistic revealed significant heterogeneity
\(Q\)(98) = 5518.62, \(p\) \< 0.001. The pooled effect size was 0.59
\[95% CI 0.53, 0.65\]. The heterogeneity at level 2 was 55.59%. The
heterogeneity at level 3 was 43.36%. The covariates which significantly
moderated the baseline model were ‘Gender’ (\(R^2_{(2)}\) = 59.82%;
\(R^2_{(3)}\) = 0.00%), and ‘Age’ (\(R^2_{(2)}\) = 17.58%; \(R^2_{(3)}\)
= 0.00%).”

## Funnel plots

A function is provided to create a funnel plot from metaSEM models. An
egger’s asymmetry test is also automatically reported to try and detect
publication bias.

``` r
moderation_object2 %>%
  funnel_plot(density = T)
```

    ## $plot

![](README_files/figure-gfm/unnamed-chunk-9-1.png)<!-- -->

    ## 
    ## $reg_test
    ## 
    ## Regression Test for Funnel Plot Asymmetry
    ## 
    ## model:     mixed-effects meta-regression model
    ## predictor: standard error
    ## 
    ## test for funnel plot asymmetry: z = 0.4681, p = 0.6397

## Moderation matrix

Where there are multiple outcomes with the same moderators, it can be
useful to visually compare all outcomes and moderators simultaneously. A
moderation matrix function is supplied for this.

``` r
mods <-
  msemtools::moderation_instructions(Gender,
                                     Age,
                                     Cohort,
                                     "Australian State" = State)

mod1 <- meta3(drink_yi, drink_vi, study_id, data = conigrave20) %>% 
  moderate(moderators = mods)
mod2 <- meta3(risk_short_yi, risk_short_vi, study_id, data = conigrave20) %>% 
  moderate(moderators = mods)
mod3 <- meta3(risk_long_yi, risk_long_vi, study_id, data = conigrave20) %>% 
  moderate(moderators = mods)
```

``` r
moderation_matrix(
  "Current drinkers" = mod1,
  "Short-term risk"  = mod2,
  "Long-term risk"   = mod3,
  transf = logit2prob,
  null_value = .5,
  effect_size = "Proportion"
)
```

![](README_files/figure-gfm/unnamed-chunk-10-1.png)<!-- -->

The grey squares indicate models which did not significantly improve
upon the fit of the baseline model.
