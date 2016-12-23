library(ggplot2)

fm <- read.csv("christmas_radio.csv", stringsAsFactors=F)
not_us <- c("BC", "BN", "MB", "MP", "NS", "PR", "QC", "SK", "VI", "GU", "AS")

##### States with the most Christmas stations
st <- data.frame(table(fm$State[fm$Christmas==TRUE]))
st2 <- data.frame(table(fm$State))
st <- merge(st, st2, by="Var1", all=T); rm(st2)
colnames(st) <- c("State", "HolidayStations", "TotalStations")
st$HolidayStations[is.na(st$HolidayStations)] <- 0
st$Percent <- round(st$HolidayStations/st$TotalStations,4)
st <- st[!(st$State %in% not_us), ]
st <- st[order(-st$HolidayStations), ]

##### Most & Least Successful Christmas Stations
scs <- fm[fm$Christmas==TRUE & !is.na(fm$DEC.16),]
scs <- scs[order(-scs$Share_Change), c(3,5,6,11,14,15,16,17)]

##### States with the most successful Christmas stations
sst <- data.frame(aggregate(Share_Change ~ Market, fm[fm$Christmas==TRUE,], sum))
sst$Share_Change <- round(sst$Share_Change,4)
sst <- sst[order(sst$Share_Change), ]
sst$Market <- factor(sst$Market, levels=sst$Market)
ggplot(sst, aes(Market, Share_Change)) + geom_bar(stat="identity", fill="maroon4") + 
  xlab("") + ylab("Change in Market Share") + labs(title="Market Share in Stations Switching to Holiday Format") +
  theme_bw() + coord_flip()


##### Map
library(leaflet)
fmc <- fm[fm$Christmas == TRUE, ]
m <- leaflet() %>%
   setView(lng = -97.51060, lat = 37.77779 , zoom = 4) %>%
   addProviderTiles("CartoDB.Positron") %>%
   addCircles(data=fmc, lng=fmc$transmitter_lon, lat=fmc$transmitter_lat, radius = fmc$avg.dist,
             stroke=FALSE, popup=fmc$Call, opacity=1, color="red", group="Christmas") 
m

