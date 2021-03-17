#' Get runsum report
#'
#' @param dam Dam at which counts are desired, only available for Corps dams in this query.Dams: BON, TDA, JDA, MCN, IHR, LMN, LGS, LGR
#' @param sdate  Start date to pull counts. `character` in m-d-yyyy format.
#' @param edate  End date to pull counts. `character` in m-d-yyyy format.
#' @param rpt  Report to download, either "salmon" or "lamprey"
#'
#' @return The data frame downloaded from FPC.
#' @importFrom readr read_csv
#' @importFrom readr cols
#' @importFrom readr col_date
#' @importFrom glue glue
#' @export
#'
#' @examples
#' \dontrun{
#' get_runsum(dam="BON", sdate="1-1-2019", edate="12-31-2019", rpt="salmon")
#' }
get_runsum <- function(dam=c("BON","TDA","JDA","MCN","IHR","LMN","LGS","LGR"), sdate, edate, rpt=c("salmon", "lamprey")){
  dam <- match.arg(dam)
  rpt <- match.arg(rpt)

  url <- "https://www.fpc.org/adults/R_coeadultcount_runsum"

  readr::read_csv(glue::glue("{url}_{rpt}_getresults.php?dam={dam}&sdate={sdate}&edate={edate}"),
                  col_types=readr::cols(CountDate=readr::col_date(format="%m/%d/%Y")))

}

#' Download laddersplit count report
#'
#' @param sdate Start date
#' @param edate End date
#' @param rpt  Report to download, either "salmon" (anadromous salmonids) or "lamprey" (all other species)
#' @importFrom dplyr filter
#' @return
#' @export
#'
#' @examples
#' \dontrun{
#' get_laddersplit(sdate="1-1-2019", edate="12-31-2019", rpt="salmon")
#' }
#
get_laddersplit <- function(sdate, edate, rpt=c("salmon", "lamprey")){

    rpt <- match.arg(rpt)

  url <- "https://www.fpc.org/adults/R_adultcoequeries_laddersplitreport_results_get"

  # Suppress date parsing warning. Report comes with blank and total rows with NA dates
  suppressWarnings(readr::read_csv(glue::glue("{url}{rpt}.php?sdate={sdate}&edate={edate}"),
                  col_types=readr::cols(CountDate=readr::col_date(format="%m/%d/%Y")))) %>%
    dplyr::filter(!is.na(CountDate))

}

#' Get hourly ladder counts
#'
#' @param sdate Start date
#' @param edate End date
#' @param ladder_id  Ladders BON 1:2, TDA 3:4, JDA 5:6, MCN 7:8, IHR 9:10, LMN 11:12, LGS 13, LGR 14
#' @param species Species to get counts for
#' @import dplyr
#' @importFrom lubridate ymd_h
#' @importFrom readr parse_number
#' @importFrom tidyr pivot_longer
#' @return
#' @export
#'
#' @examples
#' \dontrun{
#' get_hourly(sdate="8-1-2019", edate="10-1-2019", ladder_id=1, species="adultchinook")
#' get_hourly(sdate="8-1-2019", edate="10-1-2019", ladder_id=1, species="jackchinook")
#' }
get_hourly <- function(sdate="8-1-2019", edate="10-1-2019", ladder_id, species="adultchinook"){
  #if(!(ladder_id %in% 1:14)){stop("Ladder id doesn't exist")}
  url <- "https://www.fpc.org/adults/R_adultcoequeries_hourlyladderspecies_getresults.php?"
  suppressWarnings(readr::read_csv(glue::glue("{url}ladderid={ladder_id}&sdate={sdate}&edate={edate}&species={species}"),
                  col_types=readr::cols(CountDate=readr::col_date(format="%b %d %Y")))) %>%
    dplyr::filter(Description!="Average") %>%
    tidyr::pivot_longer(cols=dplyr::starts_with("h"),values_to="Count") %>%
  dplyr::mutate(hr=readr::parse_number(name)-1,
                date_time=lubridate::ymd_h(paste(CountDate,hr))) %>%
    dplyr::select(-name)
}

