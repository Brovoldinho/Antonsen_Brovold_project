# LAKSEDATA FRA SSB DETTE ER EN TEST
install.packages("PxWebApiData", repos = "https://cran.r-project.org/package=PxWebApiData")
library(httr)
library(rjstat)
library(jsonlite)
library(PxWebApiData)
library(tidyr)
library(readr)
library(dplyr)

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
# Ikke bra, men funka

#Leser inn data fra netfonds på Salmar
salmar <- read_csv("https://www.netfonds.no/quotes/paperhistory.php?paper=SALM.OSE&csv_format=csv")

#Leser inn data fra netfonds på Marine harvest
marine_harvest <- read_csv("https://www.netfonds.no/quotes/paperhistory.php?paper=MHG.OSE&csv_format=csv")

# Ser at laksepris df er for både frossen og fersk laks. 
# Vi burde årdne to kolonner med "kilospris frossen laks" og "kilospris fersk laks".
# Samme gjelder vekt kolonnen. Unødvendig med en hel kolonne over varegruppe?
# Prøver meg litt frem og ser om jeg får til dette. 
# Om du får til mer oversiktlig kode på dette så bare erstatt den som er der!


laksepris_fersk <- laksepris[1:980,1:4]

laksepris_frosset <- laksepris[981:1960,1:4]

laksepris <- bind_cols(laksepris_fersk, laksepris_frosset)


#Endrer kolonnenavn
colnames(laksepris) <- c("varegruppe", "date", "kilospris fersk laks", "vekt fersk laks", "varegruppe1", "date1", "kilospris frossen laks", "vekt frossen laks")

#Fjerner unødvendige kolonner
laksepris[c("varegruppe","varegruppe1", "date1")] <- list(NULL)

rm(laksepris_fersk, laksepris_frosset)
