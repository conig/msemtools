test_that("hide.insig", {
  require(metaSEM)
  #metaSEM approach
  dat = metaSEM::Bornmann07

  m = meta3(y=logOR, v=v, cluster=Cluster, data=Bornmann07,
            model.name="3 level model")

   msem = m %>%
     moderate(Discipline, Year)

   f_msem <- format_nicely(msem, hide.insig = FALSE)
   testthat::expect_equal(sum(is.na(f_msem$k)), 0)

})
