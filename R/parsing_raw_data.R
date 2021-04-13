### Functions to get run info -------------------------------------------------

#' Get run date from iRODS collection name of Juno output.
#'
#' @param input_string Collection name of Juno output as assigned by iRODS in
#'   previous pipeline. It MUST start with the date in the format "yymmdd".
#'   Alternatively, a character vector with the form yyddmm.
#'
#' @return String with date-like format (yy-mm-dd).
#' @export
#'
#' @importFrom dplyr %>%
#'
#' @examples get_run_date("juno_results/200922_NB502001_0153_AHGNLNAFX2_0007")
#' @examples get_run_date("200922")
get_run_date <- function(input_string){
  stopifnot( stringr::str_detect(input_string, "\\d{6}") )

  if ( stringr::str_detect(input_string, "/") ){
    input_string <- basename(input_string)
  }

  date_run <- input_string %>%
    stringr::str_extract("\\d{6}") %>%
    purrr::map_chr(stringr::str_replace,
                   "(\\d{2})(\\d{2})(\\d{2})$","\\1-\\2-\\3") %>%
    as.Date(format = "%y-%m-%d")

  if( difftime(date_run,
               Sys.Date(),
               units = "days") %>%
      as.numeric() %>%
      abs() > 3650) {
    warning("The date is too far in the past or in the future.
            This might be an error in your folder name.
            Make sure that the date is included in the folder name or the provided run date.
            Make sure also that the date is provided in the format 'yymmdd'.")
  }

  date_run

}

### Functions to get and parse tool reports -----------------------------------

#' Find tool reports in Juno results.
#'
#' @param input_dir Path to directory with results from Juno pipeline.
#' @param tool c("quast", "bbtools", "checkm").
#'
#' @return The file path(s) of the desired report (tool) type in the input
#'   directory.
#'
#' @importFrom dplyr %>%
#' @importFrom stringr str_detect
#'
find_tool_reports <- function(input_dir=character(),
                              tool=c("quast", "bbtools", "checkm")){

  stopifnot( dir.exists(input_dir) )

  pattern_name <- "_report.tsv"

  if (tool == "quast"){
    pattern_path <- "quast|QUAST"
  } else if (tool == "checkm"){
    pattern_path <- "checkm|CheckM|CHECKM"
  } else if (tool == "bbtools"){
    pattern_path <- "bbtools"
  } else {
    stop("argument tool must be either 'quast', 'bbtools' or 'checkm'")
  }

  list.files( input_dir,  pattern = pattern_name, full.names = T, recursive = T) %>%
    `[`(stringr::str_detect(., pattern_path )) %>%
    `[`(! stringr::str_detect(., "per_sample"))
}



#' Read tsv report
#'
#' @param file_path Path to tsv report.
#'
#' @return Tibble containing the tsv report
#'
#' @import dplyr
#' @importFrom purrr map
#'
#'
read_tsv_report <- function(file_path){
  stopifnot(file.exists(file_path))

  if( length(file_path) > 1 ){
    warning("The input directory contains more than one report.tsv.
            The different report.tsv files will be merged in one
            single dataframe.")
    report <- suppressWarnings(suppressMessages(
      purrr::map(file_path, readr::read_tsv, col_names = TRUE) )) %>%
      dplyr::bind_rows()
  } else{
    report <- suppressWarnings( suppressMessages(readr::read_tsv(file_path, col_names = TRUE) ))
  }

  return(report)

}


#' Read information from QUAST report
#'
#' @param input_dir Path to directory with results from Juno pipeline.
#'
#' @return Tidy data frame containing information from QUAST report to be used
#'   for this QC report
#' @export
#'
#' @import dplyr
#'
extract_quast <- function(input_dir = character()){
  file_path <- find_tool_reports(input_dir, "quast")
  read_tsv_report(file_path) %>%
    dplyr::select("Assembly", "# contigs", "Total length", "GC (%)", "N50", "L50") %>%
    dplyr::rename("Sample" = "Assembly")
}

#' Read information from bbtools report
#'
#' @param input_dir Path to directory with results from Juno pipeline.
#'
#' @return Tidy data frame containing information from bbtools report to be used
#'   for this QC report.
#' @export
#'
#' @import dplyr
#'
extract_bbtools <- function(input_dir = character()){
  file_path <- find_tool_reports(input_dir, "bbtools")
  read_tsv_report(file_path) %>%
    dplyr::select("Sample",	"Percent mapped",	"Average coverage",
                  #"Reads", "Mapped reads", "Mapped bases", "Ref scaffolds", "Ref bases",	"Percent proper pairs",
                  "Percent of reference bases covered")
}

#' Read information from CheckM report
#'
#' @param input_dir Path to directory with results from Juno pipeline.
#'
#' @return Tidy data frame containing information from CheckM report to be used
#'   for this QC report
#' @export
#'
#' @import dplyr
#'
extract_checkm <- function(input_dir = character()){
  file_path <- find_tool_reports(input_dir, "checkm")
  read_tsv_report(file_path) %>%
    dplyr::rename( "Sample" = "sample" ) %>%
    dplyr::select("Sample", "completeness", "contamination",	"strain_heterogeneity") %>%
    dplyr::mutate("Sample" = stringr::str_remove(Sample, "L$"))
}



### Functions to merge datasets -----------------------------------------------

#' Merge datasets by sample
#'
#' @param vector_w_dataframes Character vector with the name of the datasets to
#'   merge. Each dataset MUST contain at least one column called "Sample" that
#'   is common between them.
#'
#' @param run_date Character or date vector with the date in the format
#'   "yy-mm-dd" as given by refsamp::get_run_date()
#'
#' @return Tibble with the different datasets provided in vector_w_dataframes
#'   bound by the 'Sample' column.
#' @export
#'
#' @importFrom purrr map
#' @import dplyr
#'
merging_by_sample <- function(vector_w_dataframes, run_date){

  run_date <- as.numeric(run_date)

  stopifnot(is.character(vector_w_dataframes))
  stopifnot(length(vector_w_dataframes) >= 2)

  # Read datasets and make sure they have a "Sample" column
  datasets_to_merge <- vector_w_dataframes %>%
    purrr::map(as.name) %>%
    purrr::map(eval)

  # Merge datasets by Sample
  merged_dataset <- datasets_to_merge[[1]]
  for(i in 2:length(datasets_to_merge)){
    merged_dataset <- dplyr::full_join(merged_dataset,
                                       datasets_to_merge[[i]],
                                       by = "Sample")
  }

  # Add genus name and run_date
  merged_dataset <- merged_dataset %>%
    left_join(genera_criteria[c("Sample", "Genus")],
              by = "Sample")  %>%
    mutate("Run_date" = as.Date(run_date, "1970-01-01"))

  return(merged_dataset)

}

