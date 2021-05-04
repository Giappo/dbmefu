context("test-dbmefu")

testthat::test_that("match_db_intersections_with_dbmefu", {
  filename <- "prova_output.xlsx"
  file.exists(filename)
  out <- dbmefu::match_db_intersections_with_dbmefu(filename)
  testthat::expect_true(out$convalidati[1, 2] == "Rech, Michele")
  testthat::expect_true(out$mancanti[1, 2] == "Baudo, Diomadonna")
})
