setwd( "C:/Users/jdlecy/Dropbox/01 - CURRENT PROJECTS/Better Map for City Data/2012 Election Data" )

dat1 <- read.csv( "2012 Population by County.csv", stringsAsFactors=F )

dat2 <- read.csv( "US election data 2012.csv", stringsAsFactors=F )


dat <- merge( dat1, dat2, by.x="GEOID", by.y="FIPS", all.x=T )

dat$PercentObama <- as.numeric( dat$PercentObama )

dat$PercentRomney <- as.numeric( dat$PercentRomney )


sum( is.na(dat$PercentObama) )

sum( is.na(dat$PercentRomney) )


dat$PercentObama[ is.na(dat$PercentObama) ] <- mean( dat$PercentObama, na.rm=T )

dat$PercentRomney[ is.na(dat$PercentRomney) ] <- mean( dat$PercentRomney, na.rm=T )


# remove duplicates

dat <- dat[ ! duplicated( dat$GEOID ) , ]






### create a new directory for your data

dir.create( "shapefiles" )

setwd( "./shapefiles" )



### download TIGER shapefile from the census

# for information on available shapefiles:
#
# https://www.census.gov/geo/maps-data/data/tiger-line.html



download.file("ftp://ftp2.census.gov/geo/tiger/TIGER2012/COUNTY/tl_2012_us_county.zip", "us counties.zip" )

unzip( "us counties.zip" )

file.remove( "us counties.zip" )

dir()




# load and plot your shapefile

library( maptools )
library( sp )

counties <- readShapePoly( fn="tl_2012_us_county", proj4string=CRS("+proj=longlat +datum=WGS84") )

plot( counties,  border="gray10" )




counties <- counties[ counties$LSAD == "06" , ]

counties <- counties[ counties$STATEFP != "15" , ]

plot( counties,  border="gray10" )



counties <- merge( counties, dat, by.x="GEOID", by.y="GEOID" )




color.function <- colorRampPalette( c("firebrick4","blue") )

col.ramp <- color.function( 5 ) # number of groups you desire

color.vector <- cut( rank(counties$PercentObama), breaks=5, labels=col.ramp )

color.vector <- as.character( color.vector )




plot( counties, col=color.vector, border=color.vector )

