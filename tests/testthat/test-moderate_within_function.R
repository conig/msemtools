test_that("moderate works within functions", {
  d1 <- msemtools::conigrave20
  run_analyses_reproducible <- function(df, one_group) {
    df <-
      dplyr::filter(df, df$Remoteness == one_group) #just test for this outcome
    main_model <- metaSEM::meta3(
      y = drink_yi,
      v = drink_vi,
      cluster = study_id,
      data = df,
      model.name = "3 level model"
    )
    #summary(main_model)
    main_moderators <- msemtools::moderate(main_model,
                                           Cohort,
                                           State, call_only = FALSE)
    #summary(main_moderators)
    return(main_moderators)
  }
res <- run_analyses_reproducible(msemtools::conigrave20, "Urban")
  testthat::expect_true(!is.null(res))
})
