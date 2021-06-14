#machine learning
#continous wavelet transforamtion package
library(wmtsa)
#prospectr
library(prospectr)
options(java.parameters = "-Xmx30g")
#Machine Learning
library(Cubist)
library(earth)
#Classification and Regression Training Tools
library(caret)
library(xgboost)
library(conformal)
library(extraTrees)
library(brnn)
#Parrallel Processing
library(foreach)
library(doParallel)
cl=makeCluster(4)
registerDoParallel(cl)
library(svd)
library(gbm)
library(monmlp)
library(prospectr)
#hyperspectral image processing package
library(raster)
library(rgdal)
setwd("C:\\Users\\preston\\OneDrive\\Data\\PhD\\Chapter 4")

tcn_train=read.csv("chapter_4_tcn_spectra.csv")
tcn=tcn_train[,1:3]
spectra=tcn_train[,-c(1:3)]

names=colnames(spectra)

#cwt
spectra.cwt=foreach(i=1:nrow(spectra), .combine=rbind) %dopar%
{
  library(wmtsa)
  test1=as.numeric(spectra[i,])
  test1=wavCWT(test1, n.scale=8, wavelet="Gaussian2")
  test1[,2]+test1[,3]+test1[,4]
}

colnames(spectra.cwt)=names


#band correlation
tc=tcn$tc
input.earth=data.frame(cbind(tc, spectra.cwt))
model.earth=earth(tc~., data=input.earth)
var=evimp(model.earth)

tc.a=row.names(var[1:11,])
spectra.cwt=data.frame(spectra.cwt)
spectra.cwt=as.matrix(spectra.cwt)
input.tc.a=as.matrix(spectra.cwt[,select=tc.a])
input.tc=cbind(tc, input.tc.a)

tn=tcn$tn
input.earth=data.frame(cbind(tn, spectra.cwt))
model.earth=earth(tn~., data=input.earth)
var=evimp(model.earth)

tn.a=row.names(var[1:15,])
spectra.cwt=data.frame(spectra.cwt)
spectra.cwt=as.matrix(spectra.cwt)
input.tn.a=as.matrix(spectra.cwt[,select=tn.a])
input.tn=cbind(tn, input.tn.a)


#brnn model build
model.tc=brnn(tc~., data=input.tc, neurons=5)
model.tn=brnn(tn~., data=input.tn, neurons=5)

#import boreholes
setwd('C:\\Users\\preston\\OneDrive\\Data\\PhD\\Chapter 4\\preston-swir\\Boreholes')

for (i in 17:36){
bh=paste("BH0", i, "_filtered",sep="")
bh017=readGDAL(bh)
bh017=brick(bh017)

bh017.data=bh017@data@values

bh017.cwt=foreach(i=1:nrow(bh017.data), .combine=rbind) %dopar%
{
  library(wmtsa)
  test1=as.numeric(bh017.data[i,])
  test1=wavCWT(test1, n.scale=8, wavelet="Gaussian2")
  test1[,2]+test1[,3]+test1[,4]
}

colnames(bh017.cwt)=names
input.tc.a=as.matrix(bh017.cwt[,select=tc.a])
tc=predict(model.tc, newdata=input.tc.a)
tc[tc>6.9]=6.9
tc[tc<0.2]=0.2

bh017.tc=bh017[[100]]
bh017.tc@data@values=tc

writeRaster(bh017.tc, paste(bh, "_tc_Predictions", sep=""), format="GTiff")

input.tn.a=as.matrix(bh017.cwt[,select=tn.a])
tn=predict(model.tn, newdata=input.tn.a)
tn[tn>0.52]=0.52
tn[tn<0.02]=0.02

bh017.tn=bh017[[100]]
bh017.tn@data@values=tn

writeRaster(bh017.tn, paste(bh, "_tn_Predictions", sep=""), format="GTiff")
}


