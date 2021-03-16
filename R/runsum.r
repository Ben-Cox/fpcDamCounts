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
#'
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

