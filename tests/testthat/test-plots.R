test_that("Finding accepted criteria for each genus", {

  # Test wrong input
  expect_error(get_accepted_criteria(genus = "Salmonella", criterium = "wrong"))
  expect_error(get_accepted_criteria(genus = "Wrong", criterium = "completeness"))
  # Test correct input
  expect_equal(ncol(get_accepted_criteria("Salmonella", "# contigs")), 2)
  expect_equal(ncol(get_accepted_criteria("Salmonella", "Total length")), 3)
  expect_equal(ncol(get_accepted_criteria("Salmonella", "GC (%)")), 3)

})
