# While being in the main project folder do:

genera_criteria <- read.csv2('data-raw/genera_accepted.csv')
usethis::use_data(
  genera_criteria, internal = TRUE, overwrite = TRUE
)
