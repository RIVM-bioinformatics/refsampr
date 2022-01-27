test_that("Able to get the date from the folder name.", {

  expect_equal(as.character(get_run_date("juno_results/200922_NB502001_0153_AHGNLNAFX2_0007")),
    "2020-09-22")

  expect_equal(class(get_run_date("juno_results/200922_NB502001_0153_AHGNLNAFX2_0007")),
               "Date")

  expect_error(get_run_date("juno_results/AHGNLNAFX2_0007"))

  expect_warning(get_run_date("juno_results/500202_AHGNLNAFX2_0007"))

  expect_warning(get_run_date("juno_results/000202_AHGNLNAFX2_0007"))

})

test_that("Able to get the date from the run name.", {

  expect_equal(as.character(get_run_date("200922_NB502001_0153_AHGNLNAFX2_0007")),
               "2020-09-22")

  expect_equal(class(get_run_date("200922_NB502001_0153_AHGNLNAFX2_0007")),
               "Date")

  expect_error(get_run_date("AHGNLNAFX2_0007"))

  expect_warning(get_run_date("500202_AHGNLNAFX2_0007"))

  expect_warning(get_run_date("000202_AHGNLNAFX2_0007"))

})


test_that("Able to find reports for each tool in a file.", {

  # Making fake data
  dir.create("fake_input")
  dir.create("fake_input/quast")
  dir.create("fake_input/bbtools")
  dir.create("fake_input/bbtools/bbtools_combined")
  dir.create("fake_input/checkm")
  dir.create("fake_input/QUAST2")
  # Two quast reports
  file.create("fake_input/quast/report.tsv")
  file.create("fake_input/QUAST/report.tsv")
  # Only one correct bbtools report
  file.create("fake_input/bbtools/my_report.tsv")
  file.create("fake_input/bbtools/bbtools_combined/report.tsv")

  # One checkm report
  file.create("fake_input/checkm/report.tsv")
  # One report.tsv that does not belong to any tool
  file.create("fake_input/report.tsv")
  file.create("fake_input/checkm_report.tsv")

  # Cleanup
  on.exit(unlink("fake_input", recursive = TRUE))
  expect_equal(find_tool_reports("fake_input", "bbtools"),
               "fake_input/bbtools/my_report.tsv")
  expect_equal(find_tool_reports("fake_input", "checkm"),
               "fake_input/checkm_report.tsv")
  #expect_warning(find_tool_reports("fake_input"))

})

test_that("Able to read and merge reports",{
  testdata_dir <- paste0(system.file("testdata", package = "refsamp"), "/210120_RunResults")
  test_dataset1 <- data.frame("Sample" = c("SALA1224", "BD08-00202"),
                              "Metric1" = c(1,2))
  test_dataset2 <- data.frame("Sample" = c("SALA1224", "BD08-00202"),
                              "Metric2" = c(3,4))
  on.exit(rm("testdata_dir", "test_dataset1", "test_dataset2"))

  expect_s3_class(extract_quast(testdata_dir), "data.frame")
  expect_s3_class(extract_bbtools(testdata_dir), "data.frame")
  expect_s3_class(extract_checkm(testdata_dir), "data.frame")
  expect_equal(ncol(extract_quast(testdata_dir)), 6)
  expect_equal(ncol(extract_bbtools(testdata_dir)), 4)
  expect_equal(ncol(extract_checkm(testdata_dir)), 4)
  #expect_s3_class(merging_by_sample(c("test_dataset1", "test_dataset2"),
  #                                  run_date = get_run_date(testdata_dir)),
  #                "data.frame")
  # expect_equal(ncol(merging_by_sample(c("test_dataset1", "test_dataset2"),
  #                                  run_date = get_run_date(testdata_dir))),
  #                5)

})
