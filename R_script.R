rm(list=ls())

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

#Leser inn data fra netfonds på Salmar
salmar <- read_csv("https://www.netfonds.no/quotes/paperhistory.php?paper=SALM.OSE&csv_format=csv")

#Leser inn data fra netfonds på Marine harvest
marine_harvest <- read_csv("https://www.netfonds.no/quotes/paperhistory.php?paper=MHG.OSE&csv_format=csv")

# Ser at laksepris df er for både frossen og fersk laks. 
# Vi burde årdne to kolonner med "kilospris frossen laks" og "kilospris fersk laks".
# Samme gjelder vekt kolonnen. Unødvendig med en hel kolonne over varegruppe?
# Prøver meg litt frem og ser om jeg får til dette. 
# Om du får til mer oversiktlig kode på dette så bare erstatt den som er der!


laksepris_fersk <- filter(laksepris, varegruppe == "Fersk oppalen laks")

laksepris_frosset <- filter(laksepris, varegruppe == "Frosen oppalen laks")

laksepris <- bind_cols(laksepris_fersk, laksepris_frosset)


#Endrer kolonnenavn
colnames(laksepris) <- c("varegruppe", "date", "kilospris fersk laks", "vekt fersk laks", "varegruppe1", "date1", "kilospris frossen laks", "vekt frossen laks")

#Fjerner unødvendige kolonner
laksepris[c("varegruppe","varegruppe1", "date1")] <- list(NULL)

# Velger open price og dato fra salmar dataframe
salmar <- salmar %>% select(quote_date, open)
#Velger open price og dato fra marine_harvest dataframe
marine_harvest <- marine_harvest %>% select(quote_date, open)
# Fikser på kolonnenavn i salmar og marine_harvest
colnames(salmar) <- c("date", "salmar")
colnames(marine_harvest) <- c("date", "marine_harvest") 
# Merger marine_harvest og salmar
marine_salmar <- left_join(salmar, marine_harvest, by = "date")
# Går fra wide til long i marine_salmar filen
marine_salmar.long<- marine_salmar %>% gather(key = "variables", value = "open", -date)

# Velger varegruppe og kilopris fra laksepris_fersk
laksepris_fersk <- laksepris_fersk %>% select(date, varegruppe, `Kilopris (kr)` )
colnames(laksepris_fersk) <- c("date", "variables", "kilospris")
marine_salmar_fersk <- left_join(laksepris_fersk, marine_salmar.long, by = "date")

# Ordner på dato på marine_salmar.long (oppdateres filen?)
marine_salmar.long <- mutate(marine_salmar.long, date= as.Date(date, format= "%Y"))
nytid <- seq(as.Date("2007-05-01"), as.Date("2018-11-01"), by = "months")
repnytid <- rep(nytid, length.out = 5596)
marine_salmar.long$date <- repnytid

#Fikser dato på laksepris_fersk (filen oppdateres så må endre manuelt på length out hver måned akkurat no, kan man stoppe oppdatering?)
laksepris_fersk <- mutate(laksepris_fersk, date= as.Date(date, format= "%Y"))
nytidFersk <- seq(as.Date("2000-01-01"), as.Date("2018-11-01"), by = "months")
repnytidFersk <- rep(nytidFersk, length.out = 983)
laksepris_fersk$date <- repnytidFersk

# merger laksepris_fersk og marine_salmar.long
marine_salmar_fersk <- left_join(laksepris_fersk, marine_salmar.long, by = "date")

# fjerner først variables.x så NA fra marine_salmar_fersk
marine_salmar_fersk$variables.x = NULL

df <- na.omit(marine_salmar_fersk)

# Endrer navn på variablene
colnames(df) <- c("dato", "kilospris", "bedrifter", "open")

# Plotting
library(ggplot2)
ggplot(df, aes(x = open, y = kilospris)) + 
  geom_line(aes()) +
  scale_color_manual(values = c("#00AFBB", "#E7B800")) +
  coord_cartesian(ylim=c(15,80))

plot_density(marine_salmar_fersk)
    
