calcLimitTemp <- function(Tout, Slope = 0.33, Intercept = 18.8) {
  Slope * Tout + Intercept
}

require(tidyverse)
require(lubridate)
ti.DT_Tout <- NetatmoData %>% filter(Room == 'Aussenraum') %>% select(DateTime, Temperature)

calcTempDayMean <- function(ti.DT_Tout) {
  ti.DT_Tout %>% group_by(date(ti.DT_Tout[[1]])) %>% summarize(mean(Temperature, na.rm=T))
}

ti.DTTm <- calcTempDayMean(ti.DT_Tout)
names(ti.DTTm) <- c("Date", 'Temperature')

Trm <- rep(NA,6)
for(i in 7:length(ti.DTTm[[1]])) { # length(ti.DT_Tout[[1]])
  Trm[i] <-(ti.DTTm[['Temperature']][i] 
       + 0.8* ti.DTTm[['Temperature']][i-1]
       + 0.6* ti.DTTm[['Temperature']][i-2]
       + 0.5* ti.DTTm[['Temperature']][i-3]
       + 0.4* ti.DTTm[['Temperature']][i-4]
       + 0.3* ti.DTTm[['Temperature']][i-5]
       + 0.2* ti.DTTm[['Temperature']][i-6])/3.8
}

Out <- add_column(ti.DTTm, Trm)
plot(ti.DT_Tout[[1]], ti.DT_Tout[[2]], type='l')
lines(ymd_h(paste(Out[[1]], "12")), Out[[3]], col='red')


ti.hou <- mutate(NetatmoData, Date = date(NetatmoData[["DateTime"]]))

ti.all <- full_join(ti.hou, Out, by = "Date")

ti.plot <- select(ti.all, DateTime, Room, Temperature.x, Trm)

calcLimitTemp <- function(Tout, Slope = 0.33, Intercept = 18.8) {
  Slope * Tout + Intercept
}

calcLimitTemp(20)
library(ggplot2)

ggplot() +
  coord_cartesian(xlim = c(-10, 30), ylim = c(15, 30)) +
  geom_path(aes(x = c(10, 30), y = calcLimitTemp(c(10, 30), 0.31, 21.3)), color = 'red') +
  geom_path(aes(x = c(10, 30), y = calcLimitTemp(c(10, 30), 0.31, 20.3)), color = 'red2', size = 1.3) +
  geom_path(aes(x = c(10, 30), y = calcLimitTemp(c(10, 30), 0.31, 17.8)), color = 'black', size = 1.3, linetype = 2) +
  geom_path(aes(x = c(10, 30), y = calcLimitTemp(c(10, 30), 0.31, 15.3)), color = 'blue2', size = 1.3) +
  geom_path(aes(x = c(10, 30), y = calcLimitTemp(c(10, 30), 0.31, 14.3)), color = 'blue') +
  geom_path(aes(x = c(-10, 20), y = 25), color = 'red2', size = 1.3) +
  geom_path(aes(x = c(-10, 20), y = 19), color = 'blue2', size = 1.3) +
  geom_point(data = filter(ti.plot, Room == "Innenraum"), aes(Trm, Temperature.x)) +
  labs(title = "ASHRAE 55", x = "Prevailing Outdoor Temperature [°C]", y = "Indoor Operative Temperature [°C]")

