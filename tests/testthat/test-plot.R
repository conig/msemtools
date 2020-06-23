test_that("Plot works", {
  require(metaSEM)
  require(msemtools)
  dat <- msemtools::conigrave20[1:15,]
  model0 <- meta3(drink_yi, drink_vi, study_id, data = dat)
  mod_tab <- model0 %>% moderate(Gender)
  p <- suppressWarnings(plot(mod_tab))
  testthat::expect_true(all(p$data$year[-1] %in% dat$Year))
})
