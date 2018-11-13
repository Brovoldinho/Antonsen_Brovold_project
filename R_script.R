rm(list=ls())

install.packages("PxWebApiData", repos = "https://cran.r-project.org/package=PxWebApiData")
library(httr)
library(rjstat)
library(jsonlite)
library(PxWebApiData)
library(tidyr)
library(readr)
library(dplyr)
library(ggvis)
library(lubridate)

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

#Datoene strekker seg ikke like langt tilbake i tid,
# som salmar_marine så tar med relevante datoer.

laksepris <- filter(laksepris, Date > "2007-05-07")
fixeddate <- seq(as.Date("2007-05-07"), as.Date("2018-10-27"), by = "weeks")

laksepris$Date <- fixeddate

rm(newdate)

laksepris %>% ggvis(~Date, ~Price_per_kg_NOK) %>% layer_paths()

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

marine_salmar$Date <- ymd(marine_salmar$Date)

# Merger marine_salmar og laksepris

marine_salmar_laksepris <- left_join(laksepris, marine_salmar, by = "Date")

# Plotter kurs for salmar og marine, og legger inn kilosprisen.

marine_salmar_laksepris%>% 
  group_by(Firm) %>%
  ggvis(~Date, ~value, stroke = ~Firm)%>%
  layer_paths()%>%
  layer_paths(~Date, ~Price_per_kg_NOK, stroke = "Price per kg in NOK")%>%
  add_axis("y", title = "Value")
  






  
    
