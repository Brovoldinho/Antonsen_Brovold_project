# LAKSEDATA FRA SSB DETTE ER EN TEST
install.packages("PxWebApiData", repos = "https://cran.r-project.org/package=PxWebApiData")
library(httr)
library(rjstat)
library(jsonlite)
library(PxWebApiData)
library(tidyr)

laksepris <- ApiData("http://data.ssb.no/api/v0/dataset/1122.json?lang=no", 
                     getDataByGET = TRUE, col_types = cols(måned = col_date(format = "%Y%m")))

# Henter ut datasettet fra laksepris
laksepris <- laksepris[[1]]

# Separerer ut U slik at datoen kan ryddes
laksepris <- separate(laksepris, uke, c("year", "week"), sep = "U")

# Legger sammen year og week
laksepris <- unite(laksepris, date, c("year", "week"), sep = "-")

# Long til wide
laksepris <- spread(laksepris, statistikkvariabel, value)

tail(laksepris)

# en test for å se om det går og pushe
