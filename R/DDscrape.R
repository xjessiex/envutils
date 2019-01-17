#' Scrape Monthly Cooling Degree Days State Data from NOAA
#'
#' This function scrapes monthly Cooling Degree Days (population-weighted) from NOAA website at a state level. Please note that Alaska and Hawaii are not included. The output is a dataframe containing scrapped data with "Date" as the first column, followed by the monthly data of 48 states.
#'
#' @param startdate a character string to indicate the starting date, in the format of "YYYY-MM-DD"
#' @param enddate a character string to indicate the ending date, in the format of "YYYY-MM-DD"
#' @return NULL
#' @examples
#' # scrape cooling degree days from 1990 to 2017
#' CDD.scrape(startdate = "1990-01-01", enddate = "2017-12-01")
#' @export
CDD.scrape<- function(startdate, enddate){

  # create a dataspan of the query dates
  datespan <- seq(from=as.Date(startdate),
                  to=as.Date(enddate), by='months')

  # print progress
  print(paste0("Running scrape CDD: ", startdate, " to ", enddate))

  months<- seq(from=datespan[1], to=datespan[length(datespan)],by='months')

  y_1 <- as.integer(format(datespan[1], "%Y"))
  m_1 <- as.integer(format(datespan[1], "%m"))

  print(paste("Running Scrapping for: Year", y_1, "Month", m_1))

  ##### query date function (specific to NOAA CDD data) #####
  query_date_CDD <- function(year, month) {
    month<-as.numeric(month)
    monstr <- as.character(month.name[month])

    URL<-"ftp://ftp.cpc.ncep.noaa.gov/htdocs/degree_days/weighted/legacy_files/cooling/statesCONUS/"
    download_link<-paste0(URL, year, "/", monstr, ".txt")
    print(download_link)
    temp<-read.table(download_link, skip = c(15),fill = TRUE, stringsAsFactors = FALSE)


    temp<-read.table(download_link, skip = c(15),fill = TRUE)
    temp<-temp[-c(59:78),]      #delete unnecessary rows
    temp<-data.frame(lapply(temp, as.character), stringsAsFactors=FALSE)

    # Organizing the headers
    temp[27,1]<-as.character("NEW HAMPSHIRE")
    temp[27,c(2:8)]<- temp[27,c(3:9)]
    temp[27,9]<-temp[28,1]
    temp[29,1]<-as.character("NEW JERSEY")
    temp[29,c(2:8)]<- temp[29,c(3:9)]
    temp[29,9]<-temp[30,1]
    temp[31,1]<-as.character("NEW MEXICO")
    temp[31,c(2:8)]<- temp[31,c(3:9)]
    temp[31,9]<-temp[32,1]
    temp[33,1]<-as.character("NEW YORK")
    temp[33,c(2:8)]<- temp[33,c(3:9)]
    temp[33,9]<-temp[34,1]
    temp[35,1]<-as.character("NORTH CAROLINA")
    temp[35,c(2:8)]<- temp[35,c(3:9)]
    temp[35,9]<-temp[36,1]
    temp[37,1]<-as.character("NORTH DAKOTA")
    temp[37,c(2:8)]<- temp[37,c(3:9)]
    temp[37,9]<-temp[38,1]
    temp[43,1]<-as.character("RHODE ISLAND")
    temp[43,c(2:8)]<- temp[43,c(3:9)]
    temp[43,9]<-temp[44,1]
    temp[45,1]<-as.character("SOUTH CAROLINA")
    temp[45,c(2:8)]<- temp[45,c(3:9)]
    temp[45,9]<-temp[46,1]
    temp[47,1]<-as.character("SOUTH DAKOTA")
    temp[47,c(2:8)]<- temp[47,c(3:9)]
    temp[47,9]<-temp[48,1]
    temp[55,1]<-as.character("WEST VIRGINIA")
    temp[55,c(2:8)]<- temp[55,c(3:9)]
    temp[55,9]<-temp[56,1]

    temp.sort <-na.omit(temp)

    x <- as.data.frame(t(temp.sort[1:48,2]))
    colnames(x)<-temp.sort[,1]

    return(x)
  }

  # use query_date function to scrape CDD for each month
  data_1 <- query_date_CDD(year=y_1,
                           month=m_1)

  for (i in seq_along(months)[-1]) {
    y <- as.integer(format(months[i], "%Y"))
    m <- as.integer(format(months[i], "%m"))

    print(paste("Running for: Year", y, "Month", m))

    data_n <- query_date_CDD(month = m,
                             year = y)

    data_1 <- rbind(data_1, data_n)
  }

  # create dates and rearrange date column
  data_1[, "Date"] <- datespan
  data_1 <- data_1[, c(49, 1:48)]

  # make sure the elements are numeric
  for (i in (2:ncol(data_1))){
    data_1[,i] <- as.numeric(data_1[,i])
  }
  return(data_1)
}



#' Scrape Monthly Heating Degree Days State Data from NOAA
#'
#' This function scrapes monthly Hooling Degree Days (population-weighted) from NOAA website at a state level. Please note that Alaska and Hawaii are not included. The output is a dataframe containing scrapped data with "Date" as the first column, followed by the monthly data of 48 states.
#'
#' @param startdate a character string to indicate the starting date, in the format of "YYYY-MM-DD"
#' @param enddate a character string to indicate the ending date, in the format of "YYYY-MM-DD"
#' @return NULL
#' @examples
#' # scrape heating degree days from 1990 to 2017
#' HDD.scrape(startdate = "1990-01-01", enddate = "2017-12-01")
#' @export
HDD.scrape <- function(startdate, enddate){

  # create a dataspan of the query dates
  datespan <- seq(from=as.Date(startdate), to=as.Date(enddate), by='months')

  # print progress
  print(paste0("Running scrape HDD: ", startdate, " to ", enddate))

  ##### query date function (specific to NOAA HDD data) #####

  query_date_HDD <- function(year, month) {
    month<-as.numeric(month)
    monstr <- as.character(month.name[month])

    URL<-"ftp://ftp.cpc.ncep.noaa.gov/htdocs/degree_days/weighted/legacy_files/heating/statesCONUS/"
    download_link<-paste0(URL, year, "/", monstr, ".txt")
    print(download_link)
    temp<-read.table(download_link, skip = c(15),fill = TRUE)


    temp<-read.table(download_link, skip = c(15),fill = TRUE)
    temp<-temp[-c(59:258),]      #delete unnecessary rows
    temp<-data.frame(lapply(temp, as.character), stringsAsFactors=FALSE)

    # Organizing the headers
    temp[27,1]<-as.character("NEW HAMPSHIRE")
    temp[27,c(2:8)]<- temp[27,c(3:9)]
    temp[27,9]<-temp[28,1]
    temp[29,1]<-as.character("NEW JERSEY")
    temp[29,c(2:8)]<- temp[29,c(3:9)]
    temp[29,9]<-temp[30,1]
    temp[31,1]<-as.character("NEW MEXICO")
    temp[31,c(2:8)]<- temp[31,c(3:9)]
    temp[31,9]<-temp[32,1]
    temp[33,1]<-as.character("NEW YORK")
    temp[33,c(2:8)]<- temp[33,c(3:9)]
    temp[33,9]<-temp[34,1]
    temp[35,1]<-as.character("NORTH CAROLINA")
    temp[35,c(2:8)]<- temp[35,c(3:9)]
    temp[35,9]<-temp[36,1]
    temp[37,1]<-as.character("NORTH DAKOTA")
    temp[37,c(2:8)]<- temp[37,c(3:9)]
    temp[37,9]<-temp[38,1]
    temp[43,1]<-as.character("RHODE ISLAND")
    temp[43,c(2:8)]<- temp[43,c(3:9)]
    temp[43,9]<-temp[44,1]
    temp[45,1]<-as.character("SOUTH CAROLINA")
    temp[45,c(2:8)]<- temp[45,c(3:9)]
    temp[45,9]<-temp[46,1]
    temp[47,1]<-as.character("SOUTH DAKOTA")
    temp[47,c(2:8)]<- temp[47,c(3:9)]
    temp[47,9]<-temp[48,1]
    temp[55,1]<-as.character("WEST VIRGINIA")
    temp[55,c(2:8)]<- temp[55,c(3:9)]
    temp[55,9]<-temp[56,1]

    temp.sort <-na.omit(temp)

    x <- as.data.frame(t(temp.sort[1:48,2]))
    colnames(x)<-temp.sort[,1]
    return(x)
  }

  # use query_date function to scrape HDD for each month
  months<- seq(from=datespan[1], to=datespan[length(datespan)],by='months')

  y_1 <- as.integer(format(datespan[1], "%Y"))
  m_1 <- as.integer(format(datespan[1], "%m"))


  print(paste("Running for: Year", y_1, "Month", m_1))
  data_1 <- query_date_HDD(year=y_1,
                           month=m_1)

  for (i in seq_along(months)[-1]) {
    y <- as.integer(format(months[i], "%Y"))
    m <- as.integer(format(months[i], "%m"))

    print(paste("Running for: Year", y, "Month", m))

    data_n <- query_date_HDD(month = m,
                             year = y)

    data_1 <- rbind(data_1, data_n)
  }
  # create dates and rearrange date column
  data_1[, "Date"] <- datespan
  data_1 <- data_1[, c(49, 1:48)]

  # make sure the elements are numeric
  for (i in (2:ncol(data_1))){
    data_1[,i] <- as.numeric(data_1[,i])
  }
  return(data_1)
}

