test_that("extractData works", {
  require(metaSEM)
  m = meta3(y=y, v=v, cluster=District,
                 x=scale(Year, scale=FALSE), data=Cooper03)
  testthat::expect_equal(msemtools:::extractData(m)$estimate, 0.1780268)
})

test_that("Continuous moderation works", {
  require(metaSEM)
  m = meta3(y=y, v=v, cluster=District, data=Cooper03) %>%
    moderate(Year = scale(Year, scale = FALSE))

  m2 = meta3(y=y, v=v, cluster=District,
                      x=scale(Year, scale=FALSE), data=Cooper03)

  testthat::expect_equal(m$models$Year$data.wide,m2$data.wide)
  testthat::expect_equal(m$table$estimate[3], as.numeric(coefficients(m2)[1])) #intercept
  testthat::expect_equal(m$table$estimate[4], as.numeric(coefficients(m2)[2])) #slope

})

test_that("Categorical moderation works", {
  require(metaSEM)
  #metaSEM approach
  dat = metaSEM::Bornmann07
  x = cbind(dat$Discipline == "Physical sciences",
            dat$Discipline == "Life sciences/biology",
            dat$Discipline == "Social sciences/humanities",
            dat$Discipline == "Multidisciplinary") %>%
    data.frame %>%
    sapply(as.numeric)
  m = meta3(y=logOR, v=v, cluster=Cluster, data=Bornmann07,
                               model.name="3 level model")
  m_mod = meta3(y=logOR, v=v, x = x, cluster=Cluster, data=Bornmann07,
                model.name="3 level model", intercept.constraints = 0)

  msem = m %>%
    moderate(Discipline)

  testthat::expect_equal(anova(m_mod, m)$p[2], msem$table$`anova p-value`[2]) # anova
  testthat::expect_equal(as.numeric(coefficients(m_mod)[1:4]), msem$table$estimate[3:6]) # slopes

})

