
#' Get hourly ladder counts from FPC
#'
#' Downloads hourly counts of one species in one ladder at a mainstem Columbia or Snake River dam.
#' @param sdate Start date `character` in m-d-yyyy format. e.g., `"8-1-2019"`
#' @param edate End date `character` in m-d-yyyy format
#' @param ladder_id  BON `1:2`, TDA `3:4`, JDA `5:6`, MCN `7:8`, IHR `9:10`, LMN `11:12`, LGS `13`, LGR `14`
#' @param species Species for which to get counts. e.g., `"adultchinook"` or `"jackchinook"`
#' @import dplyr
#' @importFrom lubridate ymd_h
#' @importFrom readr parse_number
#' @importFrom tidyr pivot_longer
#' @return
#' @export
#'
#' @examples
#' \dontrun{
#' fpc_hourly(sdate="8-1-2019", edate="10-1-2019", ladder_id=1, species="adultchinook")
#' fpc_hourly(sdate="8-1-2019", edate="10-1-2019", ladder_id=1, species="jackchinook")
#' }
fpc_hourly <- function(sdate, edate, ladder_id, species="adultchinook"){
  if(!(ladder_id %in% 1:14)){stop("Ladder id doesn't exist")}
  url <- "https://www.fpc.org/adults/R_adultcoequeries_hourlyladderspecies_getresults.php?"
  suppressWarnings(readr::read_csv(glue::glue("{url}ladderid={ladder_id}&sdate={sdate}&edate={edate}&species={species}"),
                                   col_types=readr::cols(CountDate=readr::col_date(format="%b %d %Y")))) %>%
    dplyr::filter(Description!="Average") %>%
    tidyr::pivot_longer(cols=dplyr::starts_with("h"),values_to="Count") %>%
    dplyr::mutate(hr=readr::parse_number(name)-1,
                  date_time=lubridate::ymd_h(paste(CountDate,hr))) %>%
    dplyr::select(-name)
}

