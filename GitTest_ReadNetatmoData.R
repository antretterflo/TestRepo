
library(stringr)
library(tidyverse)
library(lubridate)


v.Path <- "D:/03_Privat/40_MessungenAntretter/09_Raumklima/Netatmo Download/"

# Excel Dateien in Ordner identifizieren
allFiles <- list.files(v.Path)

NetatmoData <- tibble()
str_split(allFiles[1], '_')[[1]][1]
# Schleife zum Einlesen der Daten
for(i in 1:length(allFiles)) {
  NetatmoDataSingle           <- read_excel(paste(v.Path, allFiles[i], sep=""), sheet="Worksheet", skip=2, col_names = TRUE)
  NetatmoDataSingle[['Room']] <- str_split(allFiles[i], '_')[[1]][1]
  NetatmoData                 <- bind_rows(NetatmoData, NetatmoDataSingle)
}

NetatmoData[['DateTime']]     <- ymd_hms(NetatmoData[[2]])
NetatmoData[['DateTime5min']] <- round_date(NetatmoData[['DateTime']], "5min")
NetatmoData[['AH']]           <- calcAHs_kgm3(NetatmoData[['Temperature']]) * NetatmoData[['Humidity']]/100
NetatmoData[['VP']]           <- calcVPs_Pa(NetatmoData[['Temperature']]) * NetatmoData[['Humidity']]/100
NetatmoData[['Ent']]          <- calcEnt_Jkg(NetatmoData[['Temperature']], (NetatmoData[['Humidity']]/100))


ggplot(data = NetatmoData, aes(DateTime, AH)) +
  geom_line(aes(color = Room))
NetatmoData
calcAHs_kgm3(NetatmoData[['Temperature']]) * NetatmoData[['Humidity']]/100

NetatmoData %>% 
  filter(DateTime5min == ymd_hms('2017-03-02 12:05:00')) %>% 
  filter(Room %in% c("Innenraum", "Aussenraum") %>% 
  select('Temperature')%>%
  calcCtoF()

  filter(NetatmoData, DateTime5min == ymd_hms('2017-03-02 12:05:00') & Room == "Aussenraum")
