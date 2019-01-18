#' Scrape Monthly NG Residential Consumption by State
#'
#' This function scrapes monthly natural gas consumption (MMcf) delivered to residential end user from EIA website at a state level. The output is a dataframe containing scrapped data with "Date" as the first column, followed by the US totaland monthly data of 51 states.
#'
#' @param startdate a character string to indicate the starting date, in the format of "YYYY-MM"
#' @param enddate a character string to indicate the ending date, in the format of "YYYY-MM"
#' @return NULL
#' @examples
#' # scrape natural gas residential consumption from Jan 1990 to Dec 2017
#' NGres.scrape(startdate = "1990-01", enddate = "2017-12")
#' @export

NGres.scrape<- function(startdate, enddate){

  # feed in the website link
  URL <- "https://www.eia.gov/dnav/ng/xls/NG_SUM_LSUM_A_EPG0_VRS_MMCF_M.xls"
  temp <- tempfile() # create temporary file
  download.file(URL, temp)

  # read the downloaded file
  NG_Res <- read_excel(temp,
                     skip = 2,
                     sheet="Data 1")
  NG_Res$Date <- format(as.Date(NG_Res$Date), "%Y-%m") # format year-month

  # filter the selected periods
  result <- subset(NG_Res, Date>= startdate & Date <= enddate)
  return(result)
}
