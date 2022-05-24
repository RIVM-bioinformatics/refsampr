### Plots QUAST ---------------------------------------------------------------


#' Get accepted criteria for different metrics
#'
#' @param genus Character vector of length 1 with the name of a genus supported
#'   by the refsamp package. Currently accepted genera: "Bordetella",
#'   "Escherichia", "Listeria", "Neisseria", "Salmonella", "Shigella" and
#'   "Streptococcus".
#' @param criterion Character vector of length 1 with one of the included
#'   quality criteria for acceptance of a sample. The currently accepted
#'   criteria are: "avg_phred", "Total length", "# contigs", "N50", "GC (%)",
#'   "Average coverage", "completeness" and "contamination".
#'
#' @return Dataframe of 2-3 rows (depending on the criterion) including the
#'   Genus and the yintercept or the Genus and the ymin and ymax for drawing a
#'   geom_hline or geom_rect, depending on the criterion.These are the
#'   thresholds to be used for accepting samples according to a$Run_date given
#'   quality criterion.
#'
#' @import dplyr
#' @importFrom stringr str_replace
#' @importFrom stringr str_detect
#'
#'
get_accepted_criteria <- function(genus = character(), criterion = c("avg_phred",
                                                       "Total length",
                                                       "# contigs", "N50",
                                                       "GC (%)",
                                                       "Average coverage",
                                                       "completeness",
                                                       "contamination")){
  if ( ! criterion %in% c("avg_phred", "Total length", "# contigs", "N50",
                             "GC (%)",  "Average coverage", "completeness",
                            "contamination")){
    return(data.frame("Genus" = genus,
                      criterion = NA))
  }
  stopifnot(length(genus) == 1)
  stopifnot(length(criterion) == 1)
  
  criterion <- criterion %>%
    stringr::str_replace(" ", "_") %>%
    stringr::str_replace("#", "no") %>%
    stringr::str_replace("(%)", "perc")

  columns_criterion <- names(genera_criteria)[stringr::str_detect(names(genera_criteria), criterion)]

  criteria_for_genus <- genera_criteria %>%
    dplyr::filter(Genus == genus) %>%
    `[`(c("Genus", columns_criterion)) %>%
    `[`(1,)

  # If genus was not in table, the first column should not be NA
  if (is.na(criteria_for_genus[1,1])){
    criteria_for_genus[1,1] <- genus
  }
  return(criteria_for_genus)

}


#' Plot metric vs time
#'
#' @param dataset Data frame to use for plot. It must contain at least one
#'   character column named "Run_date" (yy-mm-dd) and one character column named
#'   "Sample".
#' @param metric Character vector length 1 with the name of the metric to plot
#'   (metric should be one of the column names of dataset)
#' @param scales Parameter to be passed to facet_wrap. It determines whether all
#'   axes are fixed for all panels in a plot (one panel per genus/species). It
#'   can take the values: 'fixed' (default), 'free_x' (y fixed and x different
#'   per panel), 'free_y' (x fixed and y different per panel) or 'free'(both
#'   free).
#'
#' @return ggplot object containing the plot to be added
#' @export
#'
#' @import ggplot2
#' @importFrom dplyr bind_rows
#' @importFrom purrr map
#'
plot_time_metrics <- function(dataset, metric, scales='fixed'){
  stopifnot("Sample" %in% colnames(dataset))
  stopifnot("Run_date" %in% colnames(dataset))
  stopifnot(metric %in% colnames(dataset))
  stopifnot(scales %in% c('fixed', 'free', 'free_x', 'free_y'))

  dataset$Run_date <- as.Date(dataset$Run_date)

  # Get all the criteria for the Genera contained in the dataset
  criteria <- purrr::map(unique(dataset$Genus)[!is.na(unique(dataset$Genus))],
                         get_accepted_criteria, criterion = metric)
  if(length(criteria)> 1){
    criteria <- criteria %>%
      dplyr::bind_rows(.id = "Genus")
  } else {
    criteria <- as.data.frame(criteria)
  }
  criteria <- criteria %>%
    mutate("Genus" = unique(dataset$Genus)[!is.na(unique(dataset$Genus))])

  # Only the genus ran in the last run will be coloured
  max_date <- max(dataset$Run_date)
  dataset$is_last <- factor(dataset$Run_date == max_date, levels = c(TRUE, FALSE))
  last_genus <- filter(dataset, is_last == TRUE) %>%
    `[[`("Genus") %>%
    unique()
  dataset$is_last_genus <- factor(dataset$Genus %in% last_genus, levels = c(TRUE, FALSE))

  plot_base <- ggplot( dataset, aes(x = Run_date, y = !!as.name(metric),
                                    color = is_last))
  
  if (ncol(criteria) == 3 ){
    names(criteria)[2:3] <- c("ymin", "ymax")
    criteria[,2] <- as.numeric(criteria[,2])
    criteria[,3] <- as.numeric(criteria[,3])
    plot_base <- plot_base  +
      geom_hline(data = criteria, aes(yintercept = ymax), linetype = 2) +
      geom_hline(data = criteria, aes(yintercept = ymin), linetype = 2)

  } else if (ncol(criteria) == 2 & ! is.na(criteria[1,2])){

    names(criteria)[2] <- c("yintercept")
    criteria[,2] <- as.numeric(criteria[,2])
    plot_base <- plot_base +
      geom_hline(data = criteria, aes(yintercept = yintercept), linetype = 2)
  }

  plot_base +
    geom_line(size = 1, aes(color = is_last_genus), alpha = 0.7) +
    geom_point(size = 2)+
    scale_color_manual(values = c("#f46631", "darkgray"), na.value = "gray") +
    facet_wrap( ~ Genus, scales = scales) +
    theme_light()+
    theme(axis.text.x = element_text(angle = 90, hjust = 1),
          strip.background = element_rect(fill = "#8882ae", colour = "#8882ae"),
          legend.position = "none")
}


#' Make reference samples report
#'
#' @param input_dir Path to directory with results from Juno pipeline.
#' @param output_report Desired file name of the output report (.html
#'   extension).
#' @param history_data Path to history_data. If not provided, a history_data.csv
#'   file will be created in the same directory where the output_report is.
#' @param run_name Collection name of Juno output as assigned by iRODS in
#'   previous pipeline.It MUST start with the date in the format "yymmdd".
#'
#' @return Html report (file name taken from output_report parameter) and a csv
#'   file with the history data.
#'
#' @importFrom stringr str_detect
#' @import rmarkdown
#'
#' @export
#'
make_refsamp_report <- function(
  input_dir, output_report,
  history_data = NULL,
  run_name
  ){
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
                                 history_data = history_data,
                                 run_name = run_name))
}
