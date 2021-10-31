install.packages(c("httr", "jsonlite"))
install.packages("taskscheduleR", repos = "http://www.datatailor.be/rcube", type = 
                   "source")
library(httr)
library(jsonlite)
library(dplyr)
library(readr)

#Get Token
key <- Sys.getenv("ELECTIONS_TOKEN")

#base url
baseUrl <- "https://api.elections.org.za/api/"
electionID <- 402

#Create headers function
addHeaders <- add_headers(Authorization = paste0("bearer ", key))

#test
munis <-  GET(url = baseUrl, "LGEBallotResults?ElectoralEventID=402", add_headers(Authorization = paste0("bearer ", key))


#create URL & get provionce list
provinceURL <- paste0(baseUrl, "Delimitation/ElectoralEventID/", electionID)
provinceList <- GET(url = provinceURL, addHeaders)
provinceList <- content(provinceList, as = "text", encoding = "UTF-8")
provinceList <- fromJSON(provinceList, flatten = TRUE) %>%
  data.frame()

#create dataframe for munis
municipalList <- getmunicipalList[0, ]

#get municipalities
for ( i in provinceList$ProvinceID) {
municipalURL <- paste0(baseUrl, "/v1/Delimitation?ElectoralEventID=", electionID, "&ProvinceID=", i)
getmunicipalList <- GET(url = municipalURL, addHeaders)
getmunicipalList <- content(getmunicipalList, as = "text", encoding = "UTF-8")
getmunicipalList <- fromJSON(getmunicipalList, flatten = TRUE) %>%
  data.frame()
municipalList <- union(municipalList, getmunicipalList, id=NULL)}

#get seat results
seatresultsList <- read.csv("seatresultsList.csv")
seatresultsList <- seatresultsList [0,]
#seatresultsList <- getseatresultList2[0, ]
for (i in municipalList$MunicipalityID) {
  try({
    seatResultsURL <- paste0(baseUrl, "LGESeatCalculationResults/ElectoralEventID/", electionID, "/MunicipalityID/", i)
  getseatresultList <- GET(url = seatResultsURL, addHeaders)
  getseatresultList1 <- content(getseatresultList, as = "text", encoding = "UTF-8")
  getseatresultList2 <- fromJSON(getseatresultList1, flatten = TRUE) %>%
    data.frame()
  })

  seatresultsList <- union(getseatresultList2, seatresultsList, id=NULL)
}
write.table(seatresultsList, "seatresultsList.csv", sep = ",")

#get municipalities by province
ecMunis <- filter(municipalList, ProvinceID == 1)
fsMunis <- filter(municipalList, ProvinceID == 2)
gpMunis <- filter(municipalList, ProvinceID == 3)
kznMunis <- filter(municipalList, ProvinceID == 4)
mpMunis <- filter(municipalList, ProvinceID == 5)
ncMunis <- filter(municipalList, ProvinceID == 6)
lpMunis <- filter(municipalList, ProvinceID == 7)
nwMunis <- filter(municipalList, ProvinceID == 8)
wcMunis <- filter(municipalList, ProvinceID == 9)

#ec
ecresultsList <- read.table("ecvoteresultsList.csv")
ecresultsList <- ecresultsList[0, ]

for (i in ecMunis$MunicipalityID) {
  try({
    voteResultsURL <- paste0(baseUrl, "v1/LGEBallotResults?ElectoralEventID=", electionID, "&ProvinceID=1&MunicipalityID=", i)
    getvoteresultList <- GET(url = voteResultsURL, addHeaders)
    getvoteresultList1 <- content(getvoteresultList, as = "text", encoding = "UTF-8")
    getvoteresultList2 <- fromJSON(getvoteresultList1, flatten = TRUE) %>%
      data.frame()
  })
  
  ecresultsList <- union(getvoteresultList2, ecresultsList)
}

write.table(ecresultsList, "ecresultsList.csv", sep=",")