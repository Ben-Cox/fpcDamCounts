#' Get laddersplit count report from FPC
#'
#' Downloads the daily ladder split report for mainstem Columbia and Snake River dams.
#' @param sdate Start date for ladder counts
#' @param edate End date for ladder counts
#' @param rpt  Report to download, either `"salmon"` (anadromous salmonids) or `"lamprey"` (all other species)
#' @importFrom dplyr filter
#' @return
#' @export
#'
#' @examples
#' \dontrun{
#' get_laddersplit(sdate="1-1-2019", edate="12-31-2019", rpt="salmon")
#' }
#
fpc_laddersplit <- function(sdate, edate, rpt=c("salmon", "lamprey")){

  rpt <- match.arg(rpt)

  url <- "https://www.fpc.org/adults/R_adultcoequeries_laddersplitreport_results_get"

  # Suppress date parsing warning. Report comes with blank and total rows with NA dates
  suppressWarnings(readr::read_csv(glue::glue("{url}{rpt}.php?sdate={sdate}&edate={edate}"),
                                   col_types=readr::cols(CountDate=readr::col_date(format="%m/%d/%Y")))) %>%
    dplyr::filter(!is.na(CountDate))

}
