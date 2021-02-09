### Plots QUAST ---------------------------------------------------------------

#' Plot metric vs time
#'
#' @param dataset Data frame to use for plot. It must contain at least one
#'   character column named "Run_date" (yy-mm-dd) and one character column named
#'   "Sample".
#' @param metric Character vector length 1 with the name of the metric to plot
#'   (metric should be one of the column names of dataset)
#'
#' @return ggplot object containing the plot to be added
#' @export
#'
#' @import ggplot2
#'
plot_time_metrics <- function(dataset, metric){
  stopifnot("Sample" %in% colnames(dataset))
  stopifnot("Run_date" %in% colnames(dataset))
  stopifnot(metric %in% colnames(dataset))

  dataset$Run_date <- as.Date(dataset$Run_date)

  ggplot( dataset, aes(x = Run_date, y = !!as.name(metric), color = Genus))+
    geom_smooth(formula = y ~ x, show.legend = F, method = "loess")+
    geom_point(size = 3)+
    scale_color_brewer(palette = "Dark2", na.value = "gray")+
    theme_light()+
    theme(axis.text.x = element_text(angle = 90, hjust = 1))
}


#' Make reference samples report
#'
#' @param input_dir Path to directory with results from Juno pipeline.
#'   Collection name of Juno output as assigned by iRODS in previous pipeline.
#'   It MUST start with the date in the format "yymmdd"
#' @param output_report Desired file name of the output report (.html
#'   extension).
#' @param history_data Path to history_data. If not provided, a history_data.csv
#'   file will be created in the same directory where the output_report is.
#'
#' @return Html report (file name taken from output_report parameter) and a csv
#'   file with the history data.
#' @export
#'
make_refsamp_report <- function(input_dir, output_report, history_data = NULL){
  stopifnot(dir.exists(input_dir))
  stopifnot(dir.exists(dirname(output_report)))
  stopifnot(stringr::str_detect(output_report, ".html$"))

  input_dir <- normalizePath(input_dir, winslash = "/")
  output_report <- normalizePath(output_report, winslash = "/")


  if( is.null(history_data) ){
    history_data <- paste0( dirname(output_report), "/history_data.csv" )
  } else{
    stopifnot(dir.exists(dirname(history_data)))
    stopifnot(stringr::str_detect(history_data, ".csv$"))
    history_data <- normalizePath(history_data, winslash = "/")
  }

  rmarkdown::render(system.file("rmd", "Report_RefSamples.Rmd", package = "refsamp"),
                   output_file = output_report,
                   envir = parent.frame(), # Explanation: https://github.com/rstudio/rmarkdown/issues/934
                   params = list(input_dir = input_dir,
                                 history_data = history_data))
}

