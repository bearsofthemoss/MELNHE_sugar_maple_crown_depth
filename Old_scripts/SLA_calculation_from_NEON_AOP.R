
# get LAI for 3 stands
library(sp)
library(rgdal)
library(stringr) # this is for data management
library(tidyr)
library(ggplot2)
library(raster)
library(neonUtilities)
library(rgdal)
library(rgeos)

#   plots<-readOGR(" ",Bartlett_intensive_sites_30x30")

# transform to UTM coordinates
crss <- make_EPSG()

UTM <- crss %>% dplyr::filter(grepl("WGS 84", note))%>% 
  dplyr::filter(grepl("19N", note))

# plots_UTM <- sp::spTransform(plots, CRS(SRS_string=paste0("EPSG:",UTM$code)))
plots_UTM <- sp::spTransform(plots, CRS(paste0("+init=epsg:",UTM$code)))

C7<-plots_UTM[plots_UTM$stand=="C7",]
C8<-plots_UTM[plots_UTM$stand=="C8",]
C9<-plots_UTM[plots_UTM$stand=="C9",]

old<-rbind(C7,C8, C9)

# centroids are the 'plot centers'. This script works with point data.
centroids <- as.data.frame(getSpPPolygonsLabptSlots(old))
centroids

# these are the easting and northings for the stand locations
east <- centroids[, 1]
north <-centroids[, 2]
# 
# byTileAOP(dpID="DP3.30012.001",site="BART",
#            year="2017", easting= east,
#            northing = north,
#            buffer=70, savepath = "LAI",check.size = T)



lf<-list.files(path="LAI\\DP3.30012.001\\2017\\FullSite\\D01\\2017_BART_3\\L3\\Spectrometer\\LAI", recursive = T, full.names = T)


l8<-raster(lf[1])
l9<-raster(lf[5])


par(mfrow=c(1,3))
plot(C7, main="Stand C7")
plot(l8, add=T)
plot(C7, add=T)

plot(C8, main="Stand C8")
plot(l8, add=T)
plot(C8, add=T)

plot(C9, main="Stand C9")
plot(l9, add=T)
plot(C9, add=T)



#3 
lai<-merge(l8, l9 )


plot(lai)
plot(old, add=T)

old$staplo<-paste(old$stand, old$plot)
old

old


v1 <- raster::extract( lai, old, fun=mean, na.rm=TRUE)
nom <- sapply(old@polygons , slot, "ID")
nom
v1 <- data.frame(ID = nom, Value = v1)
v1$Stand<-rep(c("C7","C8","C9"), each=4)

old@data

ggplot(v1, aes(x=Stand, y=Value))+geom_point()+
  ggtitle("LAI in C7, C8, C9")+theme_classic()



