% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/NGres.scrape.R
\name{NGres.scrape}
\alias{NGres.scrape}
\title{Scrape Monthly NG Residential Consumption by State}
\usage{
NGres.scrape(startdate, enddate)
}
\arguments{
\item{startdate}{a character string to indicate the starting date, in the format of "YYYY-MM"}

\item{enddate}{a character string to indicate the ending date, in the format of "YYYY-MM"}
}
\description{
This function scrapes monthly natural gas consumption (MMcf) delivered to residential end user from EIA website at a state level. The output is a dataframe containing scrapped data with "Date" as the first column, followed by the US totaland monthly data of 51 states.
}
\examples{
# scrape natural gas residential consumption from Jan 1990 to Dec 2017
NGres.scrape(startdate = "1990-01", enddate = "2017-12")
}
