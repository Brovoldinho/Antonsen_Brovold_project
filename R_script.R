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
today <- Sys.Date()

newdate <- seq(as.Date("2000-01-01"), as.Date(today), by = "weeks")

# Oppdateringen fra SSB henger en uke etter slik at vi må fjerne den siste uken fra,
# newdate før vi klistrer den inn i datokolonnen i laksepris.

newdate <- head(newdate, -1)

laksepris$Date <- newdate

# I tilfeller hvor dato matcher med laksepris datoene, betyr det at oppdatering
# fra SSB i nåværende øyeblikk ikke henger en uke etter, slik at da ser man bort ifra
# de to siste linjenene, og kjører rm(list=ls()) som står øverst i r fila.
# Deretter kjører man laksepris$Datw <- newdate alene, for å få riktig dato.


#Datoene strekker seg ikke like langt tilbake i tid,
# som salmar_marine så tar med relevante datoer.

laksepris <- filter(laksepris, Date > "2007-05-04")
fixeddate <- seq(as.Date("2007-05-07"), as.Date(today), by = "weeks")

fixeddate <- head(fixeddate, -1)

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
  
# Linear regression for verdi og pris
# Her ser vi en sterk relasjon i mellom verdi og pris, p-value < signif.lvl
verdiPris_linear <-lm(value~Price_per_kg_NOK, data=marine_salmar_laksepris)
summary(verdiPris_linear)
plotModel(verdiPris_linear)

rm(verdiPris_linear)
# Non linear regression model for verdi og pris
verdiPris_nonlinear <-lm(value~log(Price_per_kg_NOK), data=marine_salmar_laksepris)
summary(verdiPris_nonlinear)
plotModel(verdiPris_nonlinear)

rm(verdiPris_nonlinear)

# Linear regression for value og volum (Sterk relasjon, p-value < sign.nivå)
valueVolum_linear <- lm(value~Volume_tons, data=marine_salmar_laksepris)
summary(valueVolum_linear)
plotModel(valueVolum_linear)

rm(valueVolum_linear)
# Non linear regression model for value og volum
valueVolum_nonlinear <-lm(value~log(Volume_tons), data=marine_salmar_laksepris)
summary(valueVolum_nonlinear)
plotModel(valueVolum_nonlinear)

rm(valueVolum_nonlinear)

# Linear regression model for volume og pris  
prisVolum_linear <- lm(Volume_tons~Price_per_kg_NOK, data=marine_salmar_laksepris)
summary(prisVolum_linear)
plotModel(prisVolum_linear)

rm(prisVolum_linear)
# Non linear regression model for volume og pris
valueVolum_nonlinear <-lm(Price_per_kg_NOK~log(Volume_tons), data=marine_salmar_laksepris)
summary(valueVolum_nonlinear)
plotModel(valueVolum_nonlinear)

rm(valueVolum_nonlinear)

# Hva er  optimal pris mellom solgt kvantum og pris? (Etterspørsel = Tilbud)
# Ordner andregradsligning i den lineære modellen for å få en kurve med topp punkt
prisVolum <- lm(Volume_tons~Price_per_kg_NOK+I(Price_per_kg_NOK^2), data=marine_salmar_laksepris)
summary(prisVolum)
plotModel(prisVolum)
# Dette gir optimal pris
-coef(prisVolum)[2]/(2*coef(prisVolum)[3])





  
    
