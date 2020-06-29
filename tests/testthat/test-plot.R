# test_that("Plot works", {
#   require(metaSEM)
#   require(msemtools)
#   dat <- msemtools::conigrave20
#   model0 <- meta3(drink_yi, drink_vi, study_id, data = dat)
#   mod_tab <- model0 %>% moderate(Gender)
#   p <- suppressWarnings(plot(mod_tab))
#   testthat::expect_s3_class(p,"ggplot")
# })
