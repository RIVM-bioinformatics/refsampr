test_that("Able to get the date from the folder name.", {

  expect_equal(get_run_date("juno_results/200922_NB502001_0153_AHGNLNAFX2_0007"),
    "20-09-22")

  expect_error(get_run_date("juno_results/AHGNLNAFX2_0007"))

  expect_warning(get_run_date("juno_results/500202_AHGNLNAFX2_0007"))

  expect_warning(get_run_date("juno_results/000202_AHGNLNAFX2_0007"))

})


test_that("Able to find reports for each tool in a file.", {

  expect_warning(find_tool_reports("fake_input"))
  expect_equal(find_tool_reports("fake_input", "bbtools"),
               "fake_input/bbtools/my_report.tsv")
  expect_equal(find_tool_reports("fake_input", "checkm"),
               "fake_input/checkm_report.tsv")

})

test_that("Able to read and merge reports",{

  expect_s3_class(extract_quast(testdata_dir), "data.frame")
  expect_s3_class(extract_bbtools(testdata_dir), "data.frame")
  expect_s3_class(extract_checkm(testdata_dir), "data.frame")
  expect_equal(ncol(extract_quast(testdata_dir)), 6)
  expect_equal(ncol(extract_bbtools(testdata_dir)), 4)
  expect_equal(ncol(extract_checkm(testdata_dir)), 4)
  expect_s3_class(merging_by_sample(c("test_dataset1", "test_dataset2"),
                                    run_date = get_run_date(testdata_dir)),
                  "data.frame")
  expect_equal(ncol(merging_by_sample(c("test_dataset1", "test_dataset2"),
                                    run_date = get_run_date(testdata_dir))),
                  5)

})
