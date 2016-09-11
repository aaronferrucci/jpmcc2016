
library(httr)
library(XML)
getUrl <- function(bib) {
  url <-
    paste0(
      "https://www.jpmorganchasecc.com/results/search.php",
      "?city_id=14",
      "&series_year=2016",
      "?sub_event_id=561",
      "&year=2016",
      "&search=1",
      "&bib=", bib,
      "&lname=",
      "&fname=",
      "&team_name=",
      "&gender="
    )

  return(url)
}

# input: 29:56, 1:07:48, etc
# output: time in seconds
timeToSeconds <- function(time) {
  nums <- sapply(strsplit(time, ":", fixed=T), FUN=as.numeric)
  exps <- length(nums):1 - 1
  mults <- 60^exps
  s_list <- nums * mults
  seconds <- sum(s_list)
  return(seconds)
}

getData <- function() {
  file <- "jpmcc2016.csv"
  if (file.exists(file)) {
    print(paste0("Using cached data from ", file))
    allData <- read.csv(file, stringsAsFactors=FALSE)
  } else {
    allData <- data.frame()
    # for (i in 1:12000) {
    for (i in 101:105) {
      url <- getUrl(i)
      resp <- POST(url)
      cont <- content(resp, type="application/x-www-form-urlencoded")
      html <- htmlParse(cont[96][1], asText=TRUE)
      tb <- readHTMLTable(html)
      theData = data.frame()
      if ("results" %in% names(tb)) {
        allData <- rbind(allData, tb$results)
        print(paste0(i, ": yep!"))
      } else {
        print(paste0(i, ": nope"))
      }
    }  
    allData$Name <- as.character(allData$Name)
    allData$Time <- as.character(allData$Time)
    allData$Gender <- as.character(allData$Gender)
    allData$Company <- as.character(allData$Company)

    write.csv(allData, file)
  }

  # Cast some fields to numeric
  allData$Plc <- as.numeric(allData$Plc)
  allData$GPlc <- as.numeric(allData$GPlc)
  allData$Bib <- as.numeric(allData$Bib)

  # time in seconds
  allData$Time.Seconds <- sapply(allData$Time, timeToSeconds)


  return(allData)
}

