#boreholes: cf=25; ae=26, cg=17
#r=51
#g=26
#b=13

library(raster)
library(rgdal)
library(sp)
library(mapview)
library(Orcs)

setwd('C:\\Users\\preston\\OneDrive\\Data\\PhD\\Chapter 4\\preston-vis')
#import visible light
bh17_vis=readGDAL('bh17')
bh17_vis=brick(bh17_vis)
bh25_vis=readGDAL('bh25')
bh25_vis=brick(bh25_vis)
bh26_vis=readGDAL('bh26')
bh26_vis=brick(bh26_vis)

#import tcn plots
setwd("C:\\Users\\preston\\OneDrive\\Data\\PhD\\Chapter 4\\TCN Plots")
bh17_tc=raster("BH017_filtered_tc_Predictions.tif")
bh17_tn=raster("BH017_filtered_tn_Predictions.tif")
bh25_tc=raster("BH025_filtered_tc_Predictions.tif")
bh25_tn=raster("BH025_filtered_tn_Predictions.tif")
bh26_tc=raster("BH026_filtered_tc_Predictions.tif")
bh26_tn=raster("BH026_filtered_tn_Predictions.tif")

#import clay
setwd("C:\\Users\\preston\\OneDrive\\Data\\PhD\\Chapter 4\\Clay_Plots")
bh17_clay=raster("BH017_filtered_Clay_Predictions.tif")
bh25_clay=raster("BH025_filtered_Clay_Predictions.tif")
bh26_clay=raster("BH026_filtered_Clay_Predictions.tif")




#bh17
par()
par(bty='n', mfrow=c(1,4))
par(mar = c(0, 0, 0, 0), oma = c(0, 0, 0, 0))
bh17_vis=brick(bh17_vis[[51]], bh17_vis[[26]], bh17_vis[[13]])
colors=heat.colors(15)
a=spplot(bh17_tc, col.regions=colors, at=c(0.1, 0.2, 0.4, 0.6, 0.8, 1, 2, 4, 6, 8), main="Carbon")
b=spplot(bh17_tn, col.regions=colors, at=c(0.01, 0.02, 0.04, 0.06, 0.08, 0.1, 0.2, 0.4, 0.6, 0.8), main="Nitrogen")
c=spplot(bh17_clay, col.regions=heat.colors(20), main="Clay")

plotRGB(bh17_vis, stretch='lin')

graphics.off()
print(a, position = c(0,0,.5,1),more=T)
print(b, position = c(0,0,1,1),more = T)
print(c, position = c(0,0,1.5,1))



#bh25

bh25_vis=brick(bh25_vis[[51]], bh25_vis[[26]], bh25_vis[[13]])
colors=heat.colors(15)
a=spplot(bh25_tc, col.regions=colors, at=c(0.1, 0.2, 0.4, 0.6, 0.8, 1, 2, 4, 6, 8), main="Carbon")
b=spplot(bh25_tn, col.regions=colors, at=c(0.01, 0.02, 0.04, 0.06, 0.08, 0.1, 0.2, 0.4, 0.6, 0.8), main="Nitrogen")
c=spplot(bh25_clay, col.regions=heat.colors(20), main="Clay")

plotRGB(bh25_vis, stretch='lin')

graphics.off()
print(a, position = c(0,0,.5,1),more=T)
print(b, position = c(0,0,1,1),more = T)
print(c, position = c(0,0,1.5,1))


#bh26
bh26_vis=brick(bh26_vis[[51]], bh26_vis[[26]], bh26_vis[[13]])
colors=heat.colors(15)
a=spplot(bh26_tc, col.regions=colors, at=c(0.1, 0.2, 0.4, 0.6, 0.8, 1, 2, 4, 6, 8), main="Carbon")
b=spplot(bh26_tn, col.regions=colors, at=c(0.01, 0.02, 0.04, 0.06, 0.08, 0.1, 0.2, 0.4, 0.6, 0.8), main="Nitrogen")
c=spplot(bh26_clay, col.regions=heat.colors(20), main="Clay")

plotRGB(bh26_vis, stretch='lin')

graphics.off()
print(a, position = c(0,0,.5,1),more=T)
print(b, position = c(0,0,1,1),more = T)
print(c, position = c(0,0,1.5,1))




#image scale

image.scale <- function(z, zlim, col = heat.colors(12),
                        breaks, horiz=TRUE, ylim=NULL, xlim=NULL, ...){
  if(!missing(breaks)){
    if(length(breaks) != (length(col)+1)){stop("must have one more break than colour")}
  }
  if(missing(breaks) & !missing(zlim)){
    breaks <- seq(zlim[1], zlim[2], length.out=(length(col)+1)) 
  }
  if(missing(breaks) & missing(zlim)){
    zlim <- range(z, na.rm=TRUE)
    zlim[2] <- zlim[2]+c(zlim[2]-zlim[1])*(1E-3)#adds a bit to the range in both directions
    zlim[1] <- zlim[1]-c(zlim[2]-zlim[1])*(1E-3)
    breaks <- seq(zlim[1], zlim[2], length.out=(length(col)+1))
  }
  poly <- vector(mode="list", length(col))
  for(i in seq(poly)){
    poly[[i]] <- c(breaks[i], breaks[i+1], breaks[i+1], breaks[i])
  }
  xaxt <- ifelse(horiz, "s", "n")
  yaxt <- ifelse(horiz, "n", "s")
  if(horiz){YLIM<-c(0,1); XLIM<-range(breaks)}
  if(!horiz){YLIM<-range(breaks); XLIM<-c(0,1)}
  if(missing(xlim)) xlim=XLIM
  if(missing(ylim)) ylim=YLIM
  plot(1,1,t="n",ylim=ylim, xlim=xlim, xaxt=xaxt, yaxt=yaxt, xaxs="i", yaxs="i", ...)  
  for(i in seq(poly)){
    if(horiz){
      polygon(poly[[i]], c(0,0,1,1), col=col[i], border=NA)
    }
    if(!horiz){
      polygon(c(0,0,1,1), poly[[i]], col=col[i], border=NA)
    }
  }
}
