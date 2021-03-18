dir.create("fake_input")
dir.create("fake_input/quast")
dir.create("fake_input/bbtools")
dir.create("fake_input/bbtools/bbtools_combined")
dir.create("fake_input/checkm")
dir.create("fake_input/QUAST2")
# Two quast reports
file.create("fake_input/quast/report.tsv")
file.create("fake_input/QUAST2/report.tsv")
# Only one correct bbtools report
file.create("fake_input/bbtools/my_report.tsv")
file.create("fake_input/bbtools/bbtools_combined/report.tsv")

# One checkm report
file.create("fake_input/checkm/report.tsv")
# One report.tsv that does not belong to any tool
file.create("fake_input/report.tsv")
file.create("fake_input/checkm_report.tsv")

# Test data
testdata_dir <- paste0(system.file("testdata", package = "refsamp"), "/210120_RunResults")
test_dataset1 <- data.frame("Sample" = c("SALA1224", "BD08-00202"),
                            "Genus" = c("Salmonella", "Shigella"),
                            "Metric1" = c(1,2))
test_dataset2 <- data.frame("Sample" = c("SALA1224", "BD08-00202"),
                            "Genus" = c("Salmonella", "Shigella"),
                            "Metric2" = c(3,4))

# Cleanup
withr::defer(rm("testdata_dir", "test_dataset1", "test_dataset2"), teardown_env())
withr::defer(unlink("fake_input", recursive = TRUE), teardown_env())
