###Assigment Lesson6

###Team CrisAlex
###16/01/2017

#Library needed
library(sp)
library(rgdal)
library(rgeos)

#Download and unzip the zip files from the website
download.file("http://www.mapcruzin.com/download-shapefile/netherlands-places-shape.zip", destfile = './data/netherlands-places-shape.zip', method = "auto", mode="wb")
download.file("http://www.mapcruzin.com/download-shapefile/netherlands-railways-shape.zip", destfile = './data/netherlands-railways-shape.zip', method = "auto", mode="wb")

unzip(zipfile="./data/netherlands-places-shape.zip", exdir="./data")
unzip(zipfile="./data/netherlands-railways-shape.zip", exdir="./data")

dir.create("data", showWarnings = FALSE)

#Input the files
railways <- readOGR(dsn="data", layer = "railways")
places <- readOGR(dsn="data", layer = "places")

#Plot them
plot(railways)
plot(places, add= TRUE)

#To project both shapefiles in the same coordinates, RD
prj_string_RD <- CRS("+proj=sterea +lat_0=52.15616055555555 +lon_0=5.38763888888889 +k=0.9999079 +x_0=155000 +y_0=463000 +ellps=bessel +towgs84=565.2369,50.0087,465.658,-0.406857330322398,0.350732676542563,-1.8703473836068,4.0812 +units=m +no_defs")
railways_proj <- spTransform(railways, prj_string_RD)
places_proj <- spTransform(places, prj_string_RD)


#To see the data and take the industrial type from railways
railways@data

#Take the type ofindustrial railways
railways.ind <- railways_proj[railways_proj$type == "industrial",]
industrial <- spTransform(railways.ind, prj_string_RD)
plot(industrial)

#to do a Buffer of 1km around the industrial railways
railways.ind_buff <- gBuffer(industrial, byid=TRUE, quadsegs=5, width=1000)
plot(railways.ind_buff)


places_proj@data

#Do the intersection between industrial railways and places in Netherlands
intersection <- gIntersection(places_proj, railways.ind_buff, id=as.character(places_proj$osm_id), byid=T)

#take the data information from the place inside on the intersection
city_buffer <- places_proj[places_proj$osm_id == rownames(intersection@coords),]


#plot the result together with the name of the city
plot(railways.ind_buff, col= "orange", main = " Buffer 1km Industrial Railway", axes= TRUE)
plot(city_buffer, add=TRUE, col="blue", pch=19, cex=1.5)
text(city_buffer, "Utrecht", col="dodgerblue4", pos=1)
grid()

# View of the city name and its population
#Some variables needed for the final view
city_name <- city_buffer$name
population <- city_buffer$population

print(paste("The name of the city is", city_name, "and the population is", population, sep = " "))




