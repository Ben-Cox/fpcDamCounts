% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/runsum.r
\name{fpc_runsum}
\alias{fpc_runsum}
\title{Get runsum report from FPC}
\usage{
fpc_runsum(
  dam = c("BON", "TDA", "JDA", "MCN", "IHR", "LMN", "LGS", "LGR"),
  sdate,
  edate,
  rpt = c("salmon", "lamprey")
)
}
\arguments{
\item{dam}{Dam at which counts are desired. Dams: \code{"BON"}, \code{"TDA"}, \code{"JDA"}, \code{"MCN"}, \code{"IHR"}, \code{"LMN"}, \code{"LGS"}, \code{"LGR"}}

\item{sdate}{Start date to pull counts. \code{character} in m-d-yyyy format.}

\item{edate}{End date to pull counts. \code{character} in m-d-yyyy format.}

\item{rpt}{Report to download, either \code{"salmon"} (anadromous salmonids) or \code{"lamprey"} (all other species).}
}
\value{
The data frame downloaded from FPC.
}
\description{
Downloads daily total counts at a mainstem Columbia or Snake River dam for the specified date range.
}
\examples{
\dontrun{
fpc_runsum(dam="BON", sdate="1-1-2019", edate="12-31-2019", rpt="salmon")
}
}
