#install.packages(c("httr", "jsonlite", "readr", "tidyverse"))
#install.packages("taskscheduleR", repos = "http://www.datatailor.be/rcube", type = 
                   "source")
#install.packages("pivottabler")

library(httr)
library(jsonlite)
library(dplyr)
library(readr)
library(pivottabler)
library(tidyverse)

#Get Token
key <- Sys.getenv("ELECTIONS_TOKEN")

#base url
baseUrl <- "https://api.elections.org.za/api/"
electionID <- 1091

#Create headers function
addHeaders <- add_headers(Authorization = paste0("bearer ", key))

#test
munis <-  GET(url = baseUrl, "LGEBallotResults?ElectoralEventID=402", add_headers(Authorization = paste0("bearer ", key))

#get party list
partyURL <- paste0(baseUrl, "v1/ContestingParties?ElectoralEventID=", electionID)
partyList <- GET(url = partyURL, addHeaders)
partyList <- content(partyList, as = "text", encoding = "UTF-8")
partyList <- fromJSON(partyList, flatten = TRUE) %>%
  data.frame()

write_csv(partyList, "partyList.csv")


#Get province list
provinceURL <- paste0(baseUrl, "Delimitation/ElectoralEventID/", electionID)
provinceList <- GET(url = provinceURL, addHeaders)
provinceList <- content(provinceList, as = "text", encoding = "UTF-8")
provinceList <- fromJSON(provinceList, flatten = TRUE) %>%
  data.frame()

#create dataframe for munis
municipalList <- readRDS("getmunicipalList.rda")
municipalList <- municipalList[0, ]

#get municipalities
for ( i in provinceList$ProvinceID) {
municipalURL <- paste0(baseUrl, "/v1/Delimitation?ElectoralEventID=", electionID, "&ProvinceID=", i)
getmunicipalList <- GET(url = municipalURL, addHeaders)
getmunicipalList <- content(getmunicipalList, as = "text", encoding = "UTF-8")
getmunicipalList <- fromJSON(getmunicipalList, flatten = TRUE) %>%
  data.frame()
municipalList <- union(municipalList, getmunicipalList)}

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




##get seat results

#seatresultsList <- read.csv("seatresultsList.csv", stringsAsFactors = FALSE)
#seatresultsList <- getseatresultList2

seatresultsList <- readRDS("seatresultsList.rda")
seatresultsList <- seatresultsList[0, ]

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
write.table(seatresultsList, "seatresultsList.csv", sep=",", quote = TRUE, row.names = FALSE)

seatResultrunning <- seatresultsList %>% 
  pivot_wider(id_cols = c("ProvinceID", "Province", "Municipality"), names_from = "PartyResults.Name", values_from = "PartyResults.TotalPartySeats", names_sort = "FALSE")

#get seat results by province
ecSeats <- filter(seatResultrunning, ProvinceID == 1)
fsSeats <- filter(seatResultrunning, ProvinceID == 2)
gpSeats <- filter(seatResultrunning, ProvinceID == 3)
kznSeats <- filter(seatResultrunning, ProvinceID == 4)
mpSeats <- filter(seatResultrunning, ProvinceID == 5)
ncSeats <- filter(seatResultrunning, ProvinceID == 6)
lpSeats <- filter(seatResultrunning, ProvinceID == 7)
nwSeats <- filter(seatResultrunning, ProvinceID == 8)
wcSeats <- filter(seatResultrunning, ProvinceID == 9)

write.table(ecSeats, "ecSeats.csv", sep=",", quote = TRUE, row.names = FALSE)
write.table(fsSeats, "fsSeats.csv", sep=",", quote = TRUE, row.names = FALSE)
write.table(gpSeats, "gpSeats.csv", sep=",", quote = TRUE, row.names = FALSE)
write.table(kznSeats, "kznSeats.csv", sep=",", quote = TRUE, row.names = FALSE)
write.table(mpSeats, "mpSeats.csv", sep=",", quote = TRUE, row.names = FALSE)
write.table(ncSeats, "ncSeats.csv", sep=",", quote = TRUE, row.names = FALSE)
write.table(nwSeats, "nwSeats.csv", sep=",", quote = TRUE, row.names = FALSE)
write.table(wcSeats, "wcSeats.csv", sep=",", quote = TRUE, row.names = FALSE)



###Get running votes totals
#ec

#ecresultsList <- read.table("ecvoteresultsList.csv")
ecresultsList <- readRDS(file="municipalresultsList.Rda")

for (i in municipalList$MunicipalityID) {
  try({
    voteResultsURL <- paste0(baseUrl, "v1/LGEBallotResults?ElectoralEventID=", electionID, "&ProvinceID=1&MunicipalityID=", i)
    getvoteresultList <- GET(url = voteResultsURL, addHeaders)
    getvoteresultList1 <- content(getvoteresultList, as = "text", encoding = "UTF-8")
    getvoteresultList2 <- fromJSON(getvoteresultList1, flatten = TRUE) %>%
      data.frame()
  })
  
  ecresultsList <- union(getvoteresultList2, ecresultsList, ID = NULL)
}

ecResulstrunning <- ecresultsList %>% 
  pivot_wider(id_cols = c("ProvinceID", "Province", "Municipality"), names_from = "PartyBallotResults.Name", values_from = "PartyBallotResults.TotalValidVotes", names_sort = "FALSE")

write.csv(ecResulstrunning, "ecResultsrunning.csv", sep=",", row.names = FALSE)

#fs
fsresultsList <- ecresultsList[0, ]

for (i in municipalList$MunicipalityID) {
  try({
    voteResultsURL <- paste0(baseUrl, "v1/LGEBallotResults?ElectoralEventID=", electionID, "&ProvinceID=2&MunicipalityID=", i)
    getvoteresultList <- GET(url = voteResultsURL, addHeaders)
    getvoteresultList1 <- content(getvoteresultList, as = "text", encoding = "UTF-8")
    getvoteresultList2 <- fromJSON(getvoteresultList1, flatten = TRUE) %>%
      data.frame()
  })
  
  fsresultsList <- union(getvoteresultList2, fsresultsList, ID = NULL)
}

fsResultrunning <- fsresultsList %>% 
  pivot_wider(id_cols = c("ProvinceID", "Province", "Municipality"), names_from = "PartyBallotResults.Name", values_from = "PartyBallotResults.TotalValidVotes", names_sort = "FALSE")

write.csv(fsResultsrunning, "ecResultsrunning.csv", sep=",", row.names = FALSE)

#gp
gpresultsList <- ecresultsList[0, ]

for (i in municipalList$MunicipalityID) {
  try({
    voteResultsURL <- paste0(baseUrl, "v1/LGEBallotResults?ElectoralEventID=", electionID, "&ProvinceID=3&MunicipalityID=", i)
    getvoteresultList <- GET(url = voteResultsURL, addHeaders)
    getvoteresultList1 <- content(getvoteresultList, as = "text", encoding = "UTF-8")
    getvoteresultList2 <- fromJSON(getvoteresultList1, flatten = TRUE) %>%
      data.frame()
  })
  
  gpresultsList <- union(getvoteresultList2, gpresultsList, ID = NULL)
}

gpResulstrunning <- gpresultsList %>% 
  pivot_wider(id_cols = c("ProvinceID", "Province", "Municipality"), names_from = "PartyBallotResults.Name", values_from = "PartyBallotResults.TotalValidVotes", names_sort = "FALSE")

write.csv(gpResultsrunning, "gpResultsrunning.csv", sep=",", row.names = FALSE)
  
#kzn
kznresultsList <- ecresultsList[0, ]

for (i in municipalList$MunicipalityID) {
  try({
    voteResultsURL <- paste0(baseUrl, "v1/LGEBallotResults?ElectoralEventID=", electionID, "&ProvinceID=4&MunicipalityID=", i)
    getvoteresultList <- GET(url = voteResultsURL, addHeaders)
    getvoteresultList1 <- content(getvoteresultList, as = "text", encoding = "UTF-8")
    getvoteresultList2 <- fromJSON(getvoteresultList1, flatten = TRUE) %>%
      data.frame()
  })
  
  kznresultsList <- union(getvoteresultList2, kznresultsList, ID = NULL)
}

kznResulstrunning <- kznresultsList %>% 
  pivot_wider(id_cols = c("ProvinceID", "Province", "Municipality"), names_from = "PartyBallotResults.Name", values_from = "PartyBallotResults.TotalValidVotes", names_sort = "FALSE")

write.csv(kznResultsrunning, "kznResultsrunning.csv", sep=",", row.names = FALSE)

#mp
mpresultsList <- ecresultsList[0, ]

for (i in municipalList$MunicipalityID) {
  try({
    voteResultsURL <- paste0(baseUrl, "v1/LGEBallotResults?ElectoralEventID=", electionID, "&ProvinceID=5&MunicipalityID=", i)
    getvoteresultList <- GET(url = voteResultsURL, addHeaders)
    getvoteresultList1 <- content(getvoteresultList, as = "text", encoding = "UTF-8")
    getvoteresultList2 <- fromJSON(getvoteresultList1, flatten = TRUE) %>%
      data.frame()
  })
  
  mpresultsList <- union(getvoteresultList2, mpresultsList, ID = NULL)
}

mpResulstrunning <- mpresultsList %>% 
  pivot_wider(id_cols = c("ProvinceID", "Province", "Municipality"), names_from = "PartyBallotResults.Name", values_from = "PartyBallotResults.TotalValidVotes", names_sort = "FALSE")

write.csv(mpResultsrunning, "mpResultsrunning.csv", sep=",", row.names = FALSE)

#nc
ncresultsList <- ecresultsList[0, ]

for (i in municipalList$MunicipalityID) {
  try({
    voteResultsURL <- paste0(baseUrl, "v1/LGEBallotResults?ElectoralEventID=", electionID, "&ProvinceID=6&MunicipalityID=", i)
    getvoteresultList <- GET(url = voteResultsURL, addHeaders)
    getvoteresultList1 <- content(getvoteresultList, as = "text", encoding = "UTF-8")
    getvoteresultList2 <- fromJSON(getvoteresultList1, flatten = TRUE) %>%
      data.frame()
  })
  
  ncresultsList <- union(getvoteresultList2, ncresultsList, ID = NULL)
}

ncResulstrunning <- ncresultsList %>% 
  pivot_wider(id_cols = c("ProvinceID", "Province", "Municipality"), names_from = "PartyBallotResults.Name", values_from = "PartyBallotResults.TotalValidVotes", names_sort = "FALSE")

write.csv(ncResultsrunning, "ncResultsrunning.csv", sep=",", row.names = FALSE)

#lp
lpresultsList <- ecresultsList[0, ]

for (i in municipalList$MunicipalityID) {
  try({
    voteResultsURL <- paste0(baseUrl, "v1/LGEBallotResults?ElectoralEventID=", electionID, "&ProvinceID=7&MunicipalityID=", i)
    getvoteresultList <- GET(url = voteResultsURL, addHeaders)
    getvoteresultList1 <- content(getvoteresultList, as = "text", encoding = "UTF-8")
    getvoteresultList2 <- fromJSON(getvoteresultList1, flatten = TRUE) %>%
      data.frame()
  })
  
  lpresultsList <- union(getvoteresultList2, lpresultsList, ID = NULL)
}

lpResulstrunning <- lpresultsList %>% 
  pivot_wider(id_cols = c("ProvinceID", "Province", "Municipality"), names_from = "PartyBallotResults.Name", values_from = "PartyBallotResults.TotalValidVotes", names_sort = "FALSE")

write.csv(lpResultsrunning, "lpResultsrunning.csv", sep=",", row.names = FALSE)

#nw
nwresultsList <- nwresultsList[0, ]

for (i in municipalList$MunicipalityID) {
  try({
    voteResultsURL <- paste0(baseUrl, "v1/LGEBallotResults?ElectoralEventID=", electionID, "&ProvinceID=8&MunicipalityID=", i)
    getvoteresultList <- GET(url = voteResultsURL, addHeaders)
    getvoteresultList1 <- content(getvoteresultList, as = "text", encoding = "UTF-8")
    getvoteresultList2 <- fromJSON(getvoteresultList1, flatten = TRUE) %>%
      data.frame()
  })
  
  nwresultsList <- union(getvoteresultList2, nwresultsList, ID = NULL)
}

nwResulstrunning <- nwresultsList %>% 
  pivot_wider(id_cols = c("ProvinceID", "Province", "Municipality"), names_from = "PartyBallotResults.Name", values_from = "PartyBallotResults.TotalValidVotes", names_sort = "FALSE")

write.csv(nwResultsrunning, "nwResultsrunning.csv", sep=",", row.names = FALSE)

#wc
wcresultsList <- ecresultsList[0, ]

for (i in municipalList$MunicipalityID) {
  try({
    voteResultsURL <- paste0(baseUrl, "v1/LGEBallotResults?ElectoralEventID=", electionID, "&ProvinceID=9&MunicipalityID=", i)
    getvoteresultList <- GET(url = voteResultsURL, addHeaders)
    getvoteresultList1 <- content(getvoteresultList, as = "text", encoding = "UTF-8")
    getvoteresultList2 <- fromJSON(getvoteresultList1, flatten = TRUE) %>%
      data.frame()
  })
  
  wcresultsList <- union(getvoteresultList2, wcresultsList, ID = NULL)
}

wcResulstrunning <- wcresultsList %>% 
  pivot_wider(id_cols = c("ProvinceID", "Province", "Municipality"), names_from = "PartyBallotResults.Name", values_from = "PartyBallotResults.TotalValidVotes", names_sort = "FALSE")

write.csv(wcResultsrunning, "wcResultsrunning.csv", sep=",", row.names = FALSE)



#nw
nwresultsList <- nwresultsList[0, ]

for (i in municipalList$MunicipalityID) {
  try({
    voteResultsURL <- paste0(baseUrl, "v1/LGEBallotResults?ElectoralEventID=", electionID, "&ProvinceID=8&MunicipalityID=", i)
    getvoteresultList <- GET(url = voteResultsURL, addHeaders)
    getvoteresultList1 <- content(getvoteresultList, as = "text", encoding = "UTF-8")
    getvoteresultList2 <- fromJSON(getvoteresultList1, flatten = TRUE) %>%
      data.frame()
  })
  
  nwresultsList <- union(getvoteresultList2, nwresultsList, ID = NULL)
}

nwResulstrunning <- nwresultsList %>% 
  pivot_wider(id_cols = c("ProvinceID", "Province", "Municipality"), names_from = "PartyBallotResults.Name", values_from = "PartyBallotResults.TotalValidVotes", names_sort = "FALSE")

write.csv(nwResultsrunning, "nwResultsrunning.csv", sep=",", row.names = FALSE)

#wc
wcresultsList <- ecresultsList[0, ]

for (i in municipalList$MunicipalityID) {
  try({
    voteResultsURL <- paste0(baseUrl, "v1/LGEBallotResults?ElectoralEventID=", electionID, "&ProvinceID=9&MunicipalityID=", i)
    getvoteresultList <- GET(url = voteResultsURL, addHeaders)
    getvoteresultList1 <- content(getvoteresultList, as = "text", encoding = "UTF-8")
    getvoteresultList2 <- fromJSON(getvoteresultList1, flatten = TRUE) %>%
      data.frame()
  })
  
  wcresultsList <- union(getvoteresultList2, wcresultsList, ID = NULL)
}

wcResulstrunning <- wcresultsList %>% 
  pivot_wider(id_cols = c("ProvinceID", "Province", "Municipality"), names_from = "PartyBallotResults.Name", values_from = "PartyBallotResults.TotalValidVotes", names_sort = "FALSE")

write.csv(wcResultsrunning, "wcResultsrunning.csv", sep=",", row.names = FALSE)

