% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/DDscrape.R
\name{CDD.scrape}
\alias{CDD.scrape}
\title{Scrape Monthly Cooling Degree Days State Data from NOAA}
\usage{
CDD.scrape(startdate, enddate)
}
\arguments{
\item{startdate}{a character string to indicate the starting date, in the format of "YYYY-MM-DD"}

\item{enddate}{a character string to indicate the ending date, in the format of "YYYY-MM-DD"}
}
\description{
This function scrapes monthly Cooling Degree Days (population-weighted) from NOAA website at a state level. Please note that Alaska and Hawaii are not included. The output is a dataframe containing scrapped data with "Date" as the first column, followed by the monthly data of 48 states.
}
\examples{
# scrape cooling degree days from 1990 to 2017
CDD.scrape(startdate = "1990-01-01", enddate = "2017-12-01")
}
