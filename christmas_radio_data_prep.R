library(geosphere)
setwd("/data/raw")

# source: https://www.fcc.gov/media/radio/fm-service-contour-data-points
fm <- read.csv("FM_service_contour_reduced.csv", stringsAsFactors=F)

# separate call sign and id
fm$call <- sapply(fm$call_num, function(x) strsplit(x, " ")[[1]][1])
fm$id2 <- sapply(fm$call_num, function(x) strsplit(x, " ")[[1]][2])

# keep only FM stations
fm <- fm[fm$service_type %in% "FM", ] 

# separate lat and long of transmitter site
fm$transmitter_lat <- sapply(fm$transmitter, function(x) as.numeric(strsplit(x, " ,")[[1]][1]))
fm$transmitter_lon <- sapply(fm$transmitter, function(x) as.numeric(strsplit(x, " ,")[[1]][2]))

# calculate the haversine distance between the transmitter and each of the 8 points
# determine the average of the max and min distance
for(j in seq_along(fm$app_id)){
  lat1 <- fm$transmitter_lat[j]
  lon1 <-  fm$transmitter_lon[j]
  all.dists <- NULL
  for(o in 5:13){
    lat2 <-  as.numeric(strsplit(fm[j,o], " ,")[[1]][1])
    lon2 <-  as.numeric(strsplit(fm[j,o], " ,")[[1]][2])
    dist <- distm(c(lon1, lat1), c(lon2, lat2), fun=distHaversine)[1]
    all.dists <- rbind(all.dists, dist)
  }
  max.dist <- max(all.dists)
  min.dist <- min(all.dists)
  avg.dist <- round((max.dist + min.dist)/2, 2)
  fm$avg.dist[j] <- avg.dist
}
fm <- fm[,c(1,2,14:18)]

# find and remove any duplicated transmitters
fm <- fm[!duplicated(fm[,c(3,8,9)]),]

# get state for each station
# source: https://transition.fcc.gov/fcc-bin/fmq?call=&arn=&state=&city=&freq=0.0&fre2=107.9&serv=FM&vac=&facid=&asrn=&class=&list=4&NextTab=Results+to+Next+Page%2FTab&dist=&dlat2=&mlat2=&slat2=&NS=N&dlon2=&mlon2=&slon2=&EW=W&size=9
callsigns <- read.csv("call_signs.csv", stringsAsFactors=F)

# prep FM for match
fm$Call2 <- sapply(fm$call, function(x) strsplit(x, "-")[[1]][1])

# match state to station by call sign
for(i in seq_along(fm$app_id)){
  st <- callsigns$State[grepl(fm$Call2[i], callsigns$Call)]
  if(length(st)>0){
    fm$state[i] <- gsub("\\s$", "", st)
  } else { fm$state[i] <- NA }
}

# get christmas stations
# source: http://radio-locator.com/cgi-bin/finder?format=xms&count=356&s=R&sr=1&is_ful=Y&is_lp=Y&prev=0
# date: 12/20/16
xm <- read.csv("radio-locator_xmas.csv", stringsAsFactors=F)

# identify Christmas stations
fm$Christmas <- FALSE
fm$xFormat <- NA
for(i in seq_along(xm$Call.Sign)){
  fm$Christmas[grepl(xm$Call.Sign[i], fm$Call2)] <- TRUE
  fm$xFormat[grepl(xm$Call.Sign[i], fm$Call2)] <- xm$Format[i]
}

# add in radio ratings from Nielson TLR
# source: https://tlr.nielsen.com/tlr/public/ratingsDisplay.do?method=loadRatingsForMarket
# date: 12/21/16
ratings <- read.csv("radio_rating.csv", stringsAsFactors=F, header=T, skip=1)

# separate call and type
ratings$Call <- sapply(ratings$Subscriber, function(x) strsplit(x, "-")[[1]][1])
ratings$Type <- sapply(ratings$Subscriber, function(x) strsplit(x, "-")[[1]][2])

# remove AM stations
ratings <- ratings[ratings$Type!="AM",]

# make numeric
for(i in c(5:8)) ratings[,i] <- as.numeric(ratings[,i])

# merge with origina data set
rfm <- merge(fm, ratings, by.x="Call2", by.y="Call", all.x=T)

# calculate change from Nov.
rfm$Change.Share <- rfm$DEC.16 - rfm$NOV.16

# remove excess variables 
rfm <- rfm[, c(1,2,3,5:12,15:19,21)]
rfm <- rfm[,c(4,2,1,3,11,8,5,6,7,10,12:17,9)]
rfm$state[rfm$state=="0"] <- NA

colnames(rfm) <- c("ID", "app_ID", "Call", "Service_Type", "Market", "State", "transmitter_lat", "transmitter_lon", "avg.dist", "xFormat", "Format",
                  "SEPT.16", "OCT.16", "NOV.16", "DEC.16", "Share_Change", "Christmas")

# write.csv(rfm, "christmas_radio.csv", row.names=F)

