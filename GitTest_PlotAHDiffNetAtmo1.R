load('Netatmo.RData')

ExtTe_5m  <- NetatmoData %>% 
  filter(Room =="Aussenraum") %>% 
  group_by(DateTime5min) %>% 
  summarize(mean(Temperature, na.rm=T))
names(ExtTe_5m) <- c('DateTime5min', 'Te')

ExtAH_5m  <- NetatmoData %>% 
  filter(Room =="Aussenraum") %>% 
  group_by(DateTime5min) %>% 
  summarize(mean(AH, na.rm=T))
names(ExtAH_5m) <- c('DateTime5min', 'AHe')

ExtVP_5m  <- NetatmoData %>% 
  filter(Room =="Aussenraum") %>% 
  group_by(DateTime5min) %>% 
  summarize(mean(VP, na.rm=T))
names(ExtVP_5m) <- c('DateTime5min', 'VPe')

NetAD <- full_join(NetatmoData, ExtTe_5m, "DateTime5min")
NetAD <- full_join(NetAD, ExtAH_5m, "DateTime5min")
NetAD <- full_join(NetAD, ExtVP_5m, "DateTime5min")

NetAD <- mutate(NetAD, Tdiff = Temperature - Te)#, AHdiff = AH - AHe, VPdiff = VP - VPdiff)
NetAD <- mutate(NetAD, AHdiff = AH - AHe)#, AHdiff = AH - AHe, VPdiff = VP - VPdiff)
NetAD <- mutate(NetAD, VPdiff = VP - VPe)#, AHdiff = AH - AHe, VPdiff = VP - VPdiff)

NetAD %>% #filter(Room == 'Wohnzimmer') %>% 
  ggplot(aes(Te, AHdiff*1000)) +
  geom_point(aes(color = Room, alpha = 0.2)) +
  
  facet_grid(.~Room)


# NetatmoData %>% 
#   filter(DateTime > ymd_h('2018-01-01 00') & DateTime < ymd_h('2018-01-01 03')) %>%  
#   select(Room, DateTime5min, Humidity, Temperature, AH, VP, Ent, CO2, Noise, Pressure) %>% 
#   group_by(DateTime5min) %>% 
#   group_by(Room) %>% 
#   #nest() %>% 
#   #select(Humidity, Temperature, AH, VP, Ent, CO2, Noise, Pressure) %>%
#   map(.$data, mean, na.rm=T)
#   
# 
# 
# tibble(Room = )

# NetatmoData
# calcAHs_kgm3(NetatmoData[['Temperature']]) * NetatmoData[['Humidity']]/100

# calcAHdiff_kgm3 <- function(DataTibble) {
#   
# }
# 
# AHs <- NetatmoData %>% 
#   filter(DateTime5min == ymd_hms('2017-03-02 12:05:00')) %>% 
#   filter(Room %in% c("Aussenraum")) %>% 
#   select('Temperature') %>%
#   calcAHs_kgm3()
# 
# RHs <- NetatmoData %>% 
#   filter(DateTime5min == ymd_hms('2017-03-02 12:05:00')) %>% 
#   filter(Room %in% c("Aussenraum")) %>% 
#   select('Humidity')
# 
# 
# NetatmoData[['AHes']] <- NetatmoData[['DateTime']] %>% 
#   select('Temperature') %>%
#   calcAHs_kgm3()
# 
# bbb <- NetatmoData %>% 
#   #filter(Room =="Aussenraum") %>% 
#   #head() %>% 
#   group_by(DateTime5min) %>% 
#   filter(Room =="Aussenraum") %>% 
#   select('Temperature')
#   
# NetAnest <- NetatmoData %>% nest(-DateTime5min)  
#   
# 
#   filter(NetatmoData, DateTime5min == ymd_hms('2017-03-02 12:05:00') & Room == "Aussenraum")
# 
#   AHall <- NetAnest[[100000, 'data']][['AH']]
#   AHall - AHall[1]