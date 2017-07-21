#author: narendra 
#description: making dorling cartogram of obesity
#install.packages("rgdal")
#install.packages("maptools")
library(rgdal)
#library(maptools)

# info about shapefiles 
#ogrInfo("data/tl_2012_us_county/", "tl_2012_us_county")
ogrInfo("data/USCounties", "USCounties")

#gor=readShapeSpatial("data/tl_2012_us_county/tl_2012_us_county")
#head(gor)
#plot(gor)

# population data

popData <- read.csv("data/2012 Population by County.csv")
dim(popData)
head(popData)

# election data

eleData <- read.csv("data/US election data 2012.csv")
eleData <- subset(eleData, eleData$FIPS !=0)
dim(eleData)
head(eleData)

# merge both data 

data <- merge(popData,eleData,by.x = 'GEOID',by.y = 'FIPS')
dim(data)
head(data)

dim(data)
length(unique(data$GEOID)) # counting unique values 

# read in shapefiles
cesusData <- readOGR("data/USCounties/", "USCounties")

head(cesusData)
head(cesusData$GEOID)
head(cesusData$STATEFP)
head(cesusData$COUNTYFP)
head(cesusData$INTPTLAT)
head(cesusData$INTPTLON)
head(cesusData$NAME)
head(cesusData$COUNTYFP)
#plot(cesusData, axes=TRUE, border="red")
#points

dir <- data.frame(GEOID=cesusData$GEOID,x=cesusData$INTPTLAT,y=cesusData$INTPTLON)
head(dir)

data <- merge(dir,data,by.x = 'GEOID',by.y = 'GEOID')
#library(devtools)
#install_github("chxy/cartogram")



#library(cartogram)
nbrList <- nbrlist(cesusData$GEOID,cesusData$INTPTLAT,cesusData$INTPTLON)

head(nbrList)
dim(data)
head(data)

#obamaPrecent<- as.numeric(as.character( data$PercentObama)) 
obamaPrecent<- as.numeric( data$PercentObama) 
#obamaPrecent[is.na(obamaPrecent)] <- 0
bluefunc <- colorRampPalette(c("azure", "azure4"))
#data[,"bluecol"] <- bluefunc(100)[findInterval(as.numeric(data$PercentObama), seq(1:100))]
data[,"bluecol"] <- bluefunc(1000)[round(as.numeric( data$PercentObama) )]

which(is.na(data$bluecol))
head(data)
#romneyPrecent <- as.numeric(as.character( data$PercentRomney))
romneyPrecent <- as.numeric(data$PercentRomney)
#romneyPrecent[is.na(romneyPrecent)] <-0
redfunc <- colorRampPalette(c("blue", "blue4"))
#data[,"redcol"] <- redfunc(100)[findInterval(as.numeric(data$PercentRomney), seq(1:80))]
data[,"redcol"] <- redfunc(1000)[round(as.numeric(data$PercentRomney))]
data$redcol
data <- data[complete.cases(data),] # remove NA 
which(is.na(data$redcol))
rommeyData <- subset(data, as.numeric(data$PercentRomney) > as.numeric(data$PercentObama))
head(rommeyData)

rommeyData[,'finalcol'] <- rommeyData$redcol
dim(rommeyData)
head(rommeyData)
obamaData <- subset(data, as.numeric(data$PercentRomney) < as.numeric(data$PercentObama))
head(obamaData)
obamaData[,'finalcol'] <- obamaData$bluecol
dim(obamaData)
head(obamaData)

finalData <-rbind(rommeyData,obamaData)
dim(finalData)
head(finalData)
nbrs=lapply(nbrList,function(xv){xv[xv %in% data$GEOID]})
#head(nbrs)
finalData <- finalData[!duplicated(finalData[,'GEOID']),] # remove dupliactes
#res=dorling(dat$Abbr,dat$centroidx,dat$centroidy,sqrt(dat$electors),nbrs,iteration=100,name.text=TRUE,dist.ratio=1.2,frame=FALSE, col=dat$result, xlab='', ylab='',nbredge=FALSE,ggplot2=TRUE)

#res=dorling(finalData$GEOID,as.numeric(finalData$x),as.numeric(finalData$y),as.numeric(finalData$Population),nbrs,iteration=100,name.text=FALSE,dist.ratio=1.2,frame=FALSE, col=finalData$finalcol, xlab='', ylab='',nbredge=FALSE,ggplot2=TRUE)

#finalData <- data[complete.cases(finalData),]
###normlized population 

normx <- as.numeric(finalData$Population)

normalizedPopulation <- (normx-min(normx))/(max(normx)-min(normx))
res=dorling(finalData$GEOID,as.numeric(finalData$x),as.numeric(finalData$y),normalizedPopulation,nbrs,iteration=1000,name.text=FALSE,dist.ratio=10,
            frame=FALSE, col=finalData$finalcol, xlab='', ylab='',nbredge=FALSE,,ggplot2=TRUE)

#res=dorling(dat$Abbr,dat$centroidx,dat$centroidy,sqrt(dat$electors),nbrs,iteration=100,name.text=TRUE,dist.ratio=10,frame=FALSE, col=dat$result, xlab='', ylab='',nbredge=FALSE,ggplot2=TRUE)
#head(res)
#plot(res)

