## -----------------------------------------------------------------------------
mean(mtcars$mpg)

## -----------------------------------------------------------------------------
baseline <-lm(mpg ~ 1 , data = mtcars)
coef(baseline)

## ----message = FALSE----------------------------------------------------------
library(dplyr)
mtcars %>% 
  group_by(cyl) %>% 
  summarise(avg_mpg = mean(mpg))

## -----------------------------------------------------------------------------
moderated_model <-lm(mpg ~ 1 + factor(cyl) , data = mtcars)
summary(moderated_model)

## -----------------------------------------------------------------------------
anova(baseline, moderated_model)

## -----------------------------------------------------------------------------
dat <- metaSEM::Bornmann07

## ---- message=FALSE-----------------------------------------------------------
library(metaSEM)
baseline <- meta3(y = logOR, v = v, cluster = Cluster, data = dat)
summary(baseline)

## -----------------------------------------------------------------------------
exp(coef(baseline)[1])

## -----------------------------------------------------------------------------
cyl6 <- as.numeric(mtcars$cyl == 6)
cyl8 <- as.numeric(mtcars$cyl == 8)
pred_matrix = cbind(cyl6, cyl8)

matrix <- cbind(y = mtcars$mpg, intercept = 1, cyl6, cyl8)
matrix[1:5,]

## -----------------------------------------------------------------------------
unique(dat$Discipline)

## -----------------------------------------------------------------------------
Physical <- as.numeric(dat$Discipline == 'Physical sciences')
Social <- as.numeric(dat$Discipline == 'Social sciences/humanities')
Life <- as.numeric(dat$Discipline == 'Life sciences/biology')
Multi <- as.numeric(dat$Discipline == 'Multidisciplinary')

moderator_matrix <- cbind(Social, Life, Multi)
moderator_matrix[1:5,] # I just show the first five rows.

## -----------------------------------------------------------------------------
moderated_model <- meta3(y = logOR, v = v, x = moderator_matrix, cluster = Cluster, data = dat)
summary(moderated_model)

## -----------------------------------------------------------------------------
anova(moderated_model, baseline)

## -----------------------------------------------------------------------------
moderator_matrix <- cbind(Physical, Social, Life, Multi)
moderated_model <- meta3(y = logOR, v = v, x = moderator_matrix,
                         intercept.constraints = 0, cluster = Cluster, data = dat)
summary(moderated_model)

## ----eval = FALSE-------------------------------------------------------------
#  remotes::install_github("conig/msemtools")

## -----------------------------------------------------------------------------
library(msemtools)

mod_res <- baseline %>% 
  moderate(Discipline)
mod_res

## -----------------------------------------------------------------------------
mod_res %>% 
  summary(hide.insig = FALSE)

## -----------------------------------------------------------------------------
msemtools::character_matrix(dat$Discipline)[1:5,]

## -----------------------------------------------------------------------------
res2 <- baseline %>% 
  moderate(Discipline, Type, Year, Country)
res2

## -----------------------------------------------------------------------------
summary(res2, hide.insig = FALSE)

## ----eval=FALSE---------------------------------------------------------------
#  header-includes:
#    - \usepackage{threeparttable}
#    - \usepackage{booktabs}
#    - \usepackage{float}

## -----------------------------------------------------------------------------
res2 %>% 
  format_nicely() %>% 
  to_apa(caption = "Moderator analysis results",
         note = "p-values are from likelihood ratio tests comparing the baseline model to moderated models")

