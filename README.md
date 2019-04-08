The purpose of ‘msemtools’ (metaSEM tools) is to help run meta-analyses
with metaSEM efficiently. The project is new and is in beta. Please
report any bugs

Installation
------------

To install msemtools run the following code:

``` r
#install.packages("devtools")
#devtools::install_github("JConigrave/msemtools")
```

Running analyses
----------------

This packages is a shell around the metaSEM package. It does not perform
any calculations itself, but rather converts your instructions into
metaSEM commands, and then saves the results in a nicely formatted way.
Currently only meta3 is supported without the ability to set
constraints.

I’ll demonstrate how to use it with Marsh’s data included in the metaSEM
package

``` r
library(dplyr)
library(metaSEM)
library(msemtools)
example_data <- metaSEM::Bornmann07 %>% 
  as_tibble
head(example_data)
```

    ## # A tibble: 6 x 9
    ##      Id Study      Cluster   logOR      v  Year Type   Discipline   Country
    ##   <int> <chr>        <int>   <dbl>  <dbl> <int> <fct>  <fct>        <fct>  
    ## 1     1 Ackers (2~       1 -0.401  0.0139  1996 Fello~ Physical sc~ Europe 
    ## 2     2 Ackers (2~       1 -0.0573 0.0343  1996 Fello~ Physical sc~ Europe 
    ## 3     3 Ackers (2~       1 -0.299  0.0339  1996 Fello~ Physical sc~ Europe 
    ## 4     4 Ackers (2~       1  0.361  0.0340  1996 Fello~ Physical sc~ Europe 
    ## 5     5 Ackers (2~       1 -0.333  0.0128  1996 Fello~ Social scie~ Europe 
    ## 6     6 Ackers (2~       1 -0.0717 0.0136  1996 Fello~ Physical sc~ Europe

We need to do some basic prep for this data: In this dataset data for
‘year’ is held in its own column as well as the Study column. This will
ruin our plot. I fix this by removing everything in parentheses in the
Study column.

``` r
example_data$Study =  gsub("\\s*\\([^\\)]+\\)","",as.character(example_data$Study))
```

Running a basic model with metaSEM
----------------------------------

As the effect sizes are already set up for this dataset, we can start
running models. Here is the basic pooled effect size:

``` r
model0 <- meta3(
  y = logOR,
  v = v,
  cluster = Cluster,
  data = example_data,
  model.name = "3 level model"
)

summary(model0)
```

    ## 
    ## Call:
    ## meta3(y = logOR, v = v, cluster = Cluster, data = example_data, 
    ##     model.name = "3 level model")
    ## 
    ## 95% confidence intervals: z statistic approximation
    ## Coefficients:
    ##             Estimate  Std.Error     lbound     ubound z value Pr(>|z|)  
    ## Intercept -0.1007784  0.0401327 -0.1794371 -0.0221198 -2.5111  0.01203 *
    ## Tau2_2     0.0037965  0.0027210 -0.0015367  0.0091297  1.3952  0.16295  
    ## Tau2_3     0.0141352  0.0091445 -0.0037877  0.0320580  1.5458  0.12216  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Q statistic on the homogeneity of effect sizes: 221.2809
    ## Degrees of freedom of the Q statistic: 65
    ## P value of the Q statistic: 0
    ## 
    ## Heterogeneity indices (based on the estimated Tau2):
    ##                               Estimate
    ## I2_2 (Typical v: Q statistic)   0.1568
    ## I2_3 (Typical v: Q statistic)   0.5839
    ## 
    ## Number of studies (or clusters): 21
    ## Number of observed statistics: 66
    ## Number of estimated parameters: 3
    ## Degrees of freedom: 63
    ## -2 log likelihood: 25.80256 
    ## OpenMx status1: 0 ("0" or "1": The optimization is considered fine.
    ## Other values may indicate problems.)

The intercept tells us that the average logit was -0.1, there was some
heterogeneity between studies (I2\_3).

Moderation with msemtools
-------------------------

To run moderation with msemtools, covariates must be set up as factors.
Let’s look at Country

``` r
#set up factors
is.factor(example_data$Country)
```

    ## [1] TRUE

This is already a factor so we’re ready to go.

``` r
moderation_object = model0 %>% 
  moderate(Country)
moderation_object
```

    ## Moderation results:
    ## 
    ## I2(2): 15.7%
    ## I2(3): 58.4%
    ## 
    ##   moderation  k  n R2_2 R2_3 p.value
    ## 1    Country 21 66 0.12 0.66   0.02*
    ## 
    ## All models converged.

We can now plot easily

forest plot
-----------

``` r
moderation_object %>% plot(author = "Study")
```

    ## year was not manually specified, using: 'Year'.

![](README_files/figure-markdown_github/unnamed-chunk-5-1.png)

If author has et al in it, we would not have to manually specify it.

formatting
----------

If we want to add more moderators we just throw them into the moderation
argument with commas

``` r
moderation_object2 = model0 %>% 
  moderate(Discipline, Country, Type)

moderation_object2
```

    ## Moderation results:
    ## 
    ## I2(2): 15.7%
    ## I2(3): 58.4%
    ## 
    ##   moderation  k  n R2_2 R2_3 p.value
    ## 1 Discipline 21 66 0.00 0.50    0.13
    ## 2    Country 21 66 0.12 0.66   0.02*
    ## 3       Type 21 66 0.07 0.79 < 0.01*
    ## 
    ## All models converged.

We can then format this table with another function

``` r
moderation_object2 %>% 
  format_nicely
```

| Moderator                                            | k   | n   | Estimate (95% CI)    | SE   | R2\_2 | R2\_3 | p           | indent\_ |
|:-----------------------------------------------------|:----|:----|:---------------------|:-----|:------|:------|:------------|:---------|
| Baseline (I<sup>2</sup><sub>(2;3)</sub>: 0.16; 0.58) | 21  | 66  | -0.10 (-0.18, -0.02) | 0.04 | \-    | \-    | \-          | FALSE    |
| Discipline                                           | 21  | 66  | \-                   | \-   | 0.00  | 0.50  | 0.13        | FALSE    |
| Country                                              | 21  | 66  | \-                   | \-   | 0.12  | 0.66  | 0.02\*      | FALSE    |
| United States                                        | 4   | 12  | 0.00 (-0.11, 0.12)   | 0.06 | \-    | \-    | \-          | TRUE     |
| Canada                                               | 1   | 3   | -0.13 (-0.33, 0.07)  | 0.10 | \-    | \-    | \-          | TRUE     |
| Australia                                            | 5   | 13  | -0.02 (-0.20, 0.16)  | 0.09 | \-    | \-    | \-          | TRUE     |
| United Kingdom                                       | 4   | 10  | 0.06 (-0.10, 0.21)   | 0.08 | \-    | \-    | \-          | TRUE     |
| Europe                                               | 7   | 28  | -0.22 (-0.32, -0.12) | 0.05 | \-    | \-    | \-          | TRUE     |
| Type                                                 | 21  | 66  | \-                   | \-   | 0.07  | 0.79  | &lt; 0.01\* | FALSE    |
| Grant                                                | 13  | 40  | -0.01 (-0.08, 0.07)  | 0.04 | \-    | \-    | \-          | TRUE     |
| Fellowship                                           | 11  | 26  | -0.20 (-0.28, -0.12) | 0.04 | \-    | \-    | \-          | TRUE     |

An indent column can be used to send formatting instructions to word.

an extra column can help here msemtools::to\_apa

Describing moderated tables
---------------------------

A method is provided to convert moderated tables to paragraph
descriptions which can be rendeded in rmarkdown.

For an example we will use moderation\_object2.

``` r
as.character(moderation_object2)
```

Inspecting the Q statistic revealed significant heterogeneity (Q(df =
`r summary(moderation_object2$models$Baseline)$Q.stat$Q.df`) =
`r summary(moderation_object2$models$Baseline)$Q.stat$Q %>% papertools::digits(2)`,
*p* =
`r summary(moderation_object2$models$Baseline)$Q.stat$pval %>% papertools::round_p(2)`).
`r moderation_object2$table$k[1] %>% papertools::as_word(T)` studies
(`r moderation_object2$table$n[1] %>% papertools::as_word(F)` effects)
presented data which could be pooled. The estimated population average
and 95% Wald CI were
`r papertools::glue_bracket(moderation_object2$table$estimate[1],moderation_object2$table$lbound[1],moderation_object2$table$ubound[1])`.
The heterogeneity at level 2 was
`r moderation_object2$table$I2_2[1] %>% '*'(100) %>% papertools::digits(2)`%.
The heterogeneity at level 3 was
`r moderation_object2$table$I2_3[1] %>% '*'(100) %>% papertools::digits(2)`%.
The covariates which significantly moderated the baseline model were
‘country’(R<sup>2</sup><sub>(2)</sub> =
`r moderation_object2$table %>% filter(model.name == 'Country') %>% select(R2_2) %>% '*'(100) %>% papertools::digits(2)`%;
R<sup>2</sup><sub>(3)</sub> =
`r moderation_object2$table %>% filter(model.name == 'Country') %>% select(R2_3) %>% '*'(100) %>% papertools::digits(2)`%),
and ‘type’(R<sup>2</sup><sub>(2)</sub> =
`r moderation_object2$table %>% filter(model.name == 'Type') %>% select(R2_2) %>% '*'(100) %>% papertools::digits(2)`%;
R<sup>2</sup><sub>(3)</sub> =
`r moderation_object2$table %>% filter(model.name == 'Type') %>% select(R2_3) %>% '*'(100) %>% papertools::digits(2)`%).

Which evaluates to:

Twenty one studies (sixty six effects) presented data which could be
pooled. The estimated population average and 95% Wald CI were -0.10
(-0.18, -0.02). The heterogeneity at level 2 was 15.68%. The
heterogeneity at level 3 was 58.39%. The covariates which significantly
moderated the baseline model were ‘country’ and ‘type’. ‘Country’
explained 12.09% of heterogeneity within studies (level 2), and 66.06%
between studies (level 3). ‘Type’ explained 6.93% of heterogeneity
within studies (level 2), and 79.43% between studies (level 3).

Funnel plots
------------

Finally, a function is provided to create a funnel plot from metaSEM
models. An egger’s asymmetry test is also automatically reported to try
and detect publication bias.

``` r
model0 %>% funnel_plot(density = T)
```

    ## $plot

![](README_files/figure-markdown_github/unnamed-chunk-11-1.png)

    ## 
    ## $reg_test
    ## 
    ## Regression Test for Funnel Plot Asymmetry
    ## 
    ## model:     mixed-effects meta-regression model
    ## predictor: standard error
    ## 
    ## test for funnel plot asymmetry: z = -1.9925, p = 0.0463
