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


# Long til wide
laksepris <- spread(laksepris, statistikkvariabel, value)

colnames(laksepris) <- c("Goods", "Date", "Price_per_kg_NOK", "Volume_tons")

laksepris <- laksepris %>% 
  filter(Goods == "Fersk oppalen laks")

#Fikser datoen

newdate <- seq(as.Date("2000-01-01"), as.Date("2018-11-01"), by = "weeks")
laksepris$Date <- newdate

#---------------------------------------------------------------------------


#Leser inn data fra netfonds på Salmar
salmar <- read_csv("https://www.netfonds.no/quotes/paperhistory.php?paper=SALM.OSE&csv_format=csv")

#Leser inn data fra netfonds på Marine harvest
marine_harvest <- read_csv("https://www.netfonds.no/quotes/paperhistory.php?paper=MHG.OSE&csv_format=csv")


# Velger open price og dato fra salmar dataframe
salmar <- salmar %>% select(quote_date, open)

#Velger open price og dato fra marine_harvest dataframe
marine_harvest <- marine_harvest %>% select(quote_date, open)

# Fikser på kolonnenavn i salmar og marine_harvest
colnames(salmar) <- c("Date", "Salmar")
colnames(marine_harvest) <- c("Date", "Marine harvest") 

# Merger marine_harvest og salmar
marine_salmar <- left_join(salmar, marine_harvest, by = "Date")

# Går fra wide til long i marine_salmar filen
marine_salmar<- marine_salmar %>% gather(key = "Firm", value = "Stock_value", -Date)

#Plotter og fikser datoformatet for marine_salmar

library(ggvis)
library(lubridate)
marine_salmar$Date <- ymd(marine_salmar$Date)

marine_salmar %>% 
  group_by(Firm) %>%
  ggvis(~Date, ~Stock_value, stroke = ~Firm) %>%
  layer_paths()

#-----------------------------------------------------------

# Gjør marine_salmar om til ukentlig slik at den kan merges med laksepris
marine_salmar <- marine_salmar %>%
group_by(Date = cut(Date, "week"), Firm) %>% 
  summarise(value = mean(Stock_value))

# Gir samme datoformat til laksepris og marine_salmar

marine_salmar$Date <- ymd(marine_salmar$Date)
laksepris$Date <- ymd(laksepris$Date)

# Velger varegruppe og kilopris fra laksepris_fersk

marine_salmar_fersk <- left_join(laksepris_fersk, marine_salmar.long, by = "date")

# merger laksepris_fersk og marine_salmar.long
marine_salmar_fersk <- left_join(laksepris_fersk, marine_salmar.long, by = "date")

# fjerner først variables.x så NA fra marine_salmar_fersk
marine_salmar_fersk$variables.x = NULL

df <- na.omit(marine_salmar_fersk)

# Endrer navn på variablene
colnames(df) <- c("dato", "kilospris", "bedrifter", "open")




  
    
