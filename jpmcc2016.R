
library(httr)
getUrl <- function(bib) {
  url <-
    paste0(
      "https://www.jpmorganchasecc.com/results/search.php",
      "?city_id=14&series_year=2016?sub_event_id=561&year=2016&search=1",
      "&bib=", bib,
      "&lname=&fname=&team_name=&gender="
    )

  return(url)
}

getData <- function() {
  file <- "jpmcc2016.csv"
  if (file.exists(file)) {
    print(paste0("Using cached data from ", file))
    allData <- read.csv(file)
  } else {
    allData <- data.frame()
    for (i in 1:12000) {
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
  }

  return(allData)
}

