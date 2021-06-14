#hyperspectral image processing package
library(raster)
library(rgdal)
#Parrallel Processing
library(doParallel)
cl=makeCluster(4)
registerDoParallel(cl)

setwd("C:/Users/preston/OneDrive/Data/PhD/Chapter 4/preston-swir/preston_1_1m00_2m00_180606-084928")
preston_1_image=readGDAL("preston_1_1m00_2m00_180606-084928_refl.dat")
preston_1_shapes=readOGR(dsn = ".", layer = "preston_1_roi")
preston_1_image=brick(preston_1_image)
preston_1_image=flip(preston_1_image, 2)
preston_1_spectra=extract(preston_1_image, preston_1_shapes, fun=mean)
labels=data.frame(preston_1_shapes$CLASS_NAME)
preston_1_spectra=cbind(labels, preston_1_spectra)
rm(preston_1_image)


setwd("C:/Users/preston/OneDrive/Data/PhD/Chapter 4/preston-swir/preston_2_1m00_2m00_180606-085732")
preston_2_image=readGDAL("preston_2_1m00_2m00_180606-085732_refl.dat")
preston_2_shapes=readOGR(dsn = ".", layer = "preston_2_roi")
preston_2_image=brick(preston_2_image)
preston_2_image=flip(preston_2_image, 2)
preston_2_spectra=extract(preston_2_image, preston_2_shapes, fun=mean)
labels=data.frame(preston_2_shapes$CLASS_NAME)
preston_2_spectra=cbind(labels, preston_2_spectra)
rm(preston_2_image)


setwd("C:/Users/preston/OneDrive/Data/PhD/Chapter 4/preston-swir/preston_3_1m00_2m00_180606-090111")
preston_3_image=readGDAL("preston_3_1m00_2m00_180606-090111_refl.dat")
preston_3_shapes=readOGR(dsn = ".", layer = "preston_3_roi")
preston_3_image=brick(preston_3_image)
preston_3_image=flip(preston_3_image, 2)
preston_3_spectra=extract(preston_3_image, preston_3_shapes, fun=mean)
labels=data.frame(preston_3_shapes$CLASS_NAME)
preston_3_spectra=cbind(labels, preston_3_spectra)
rm(preston_3_image)

setwd("C:/Users/preston/OneDrive/Data/PhD/Chapter 4/preston-swir/preston_4_1m00_2m00_180606-090512")
preston_4_image=readGDAL("preston_4_1m00_2m00_180606-090512_refl.dat")
preston_4_shapes=readOGR(dsn = ".", layer = "preston_4_roi")
preston_4_image=brick(preston_4_image)
preston_4_image=flip(preston_4_image, 2)
preston_4_spectra=extract(preston_4_image, preston_4_shapes, fun=mean)
labels=data.frame(preston_4_shapes$CLASS_NAME)
preston_4_spectra=cbind(labels, preston_4_spectra)
rm(preston_4_image)

setwd("C:/Users/preston/OneDrive/Data/PhD/Chapter 4/preston-swir/preston_5_1m00_2m00_180606-090831")
preston_5_image=readGDAL("preston_5_1m00_2m00_180606-090831_refl.dat")
preston_5_shapes=readOGR(dsn = ".", layer = "preston_5_roi")
preston_5_image=brick(preston_5_image)
preston_5_image=flip(preston_5_image, 2)
preston_5_spectra=extract(preston_5_image, preston_5_shapes, fun=mean)
labels=data.frame(preston_5_shapes$CLASS_NAME)
preston_5_spectra=cbind(labels, preston_5_spectra)
rm(preston_5_image)

setwd("C:/Users/preston/OneDrive/Data/PhD/Chapter 4/preston-swir/preston_6_1m00_2m00_180606-091256")
preston_6_image=readGDAL("preston_6_1m00_2m00_180606-091256_refl.dat")
preston_6_shapes=readOGR(dsn = ".", layer = "preston_6_roi")
preston_6_image=brick(preston_6_image)
preston_6_image=flip(preston_6_image, 2)
preston_6_spectra=extract(preston_6_image, preston_6_shapes, fun=mean)
labels=data.frame(preston_6_shapes$CLASS_NAME)
preston_6_spectra=cbind(labels, preston_6_spectra)
rm(preston_6_image)

setwd("C:/Users/preston/OneDrive/Data/PhD/Chapter 4/preston-swir/preston_7_1m00_2m00_180606-091828")
preston_7_image=readGDAL("preston_7_1m00_2m00_180606-091828_refl.dat")
preston_7_shapes=readOGR(dsn = ".", layer = "preston_7_roi")
preston_7_image=brick(preston_7_image)
preston_7_image=flip(preston_7_image, 2)
preston_7_spectra=extract(preston_7_image, preston_7_shapes, fun=mean)
labels=data.frame(preston_7_shapes$CLASS_NAME)
preston_7_spectra=cbind(labels, preston_7_spectra)
rm(preston_7_image)


setwd("C:/Users/preston/OneDrive/Data/PhD/Chapter 4/preston-swir/preston_8_1m00_2m00_180606-092156")
preston_8_image=readGDAL("preston_8_1m00_2m00_180606-092156_refl.dat")
preston_8_shapes=readOGR(dsn = ".", layer = "preston_8_roi")
preston_8_image=brick(preston_8_image)
preston_8_image=flip(preston_8_image, 2)
preston_8_spectra=extract(preston_8_image, preston_8_shapes, fun=mean)
labels=data.frame(preston_8_shapes$CLASS_NAME)
preston_8_spectra=cbind(labels, preston_8_spectra)
rm(preston_8_image)

setwd("C:/Users/preston/OneDrive/Data/PhD/Chapter 4/preston-swir/preston_9_1m00_2m00_180606-092507")
preston_9_image=readGDAL("preston_9_1m00_2m00_180606-092507_refl.dat")
preston_9_shapes=readOGR(dsn = ".", layer = "preston_9_roi")
preston_9_image=brick(preston_9_image)
preston_9_image=flip(preston_9_image, 2)
preston_9_spectra=extract(preston_9_image, preston_9_shapes, fun=mean)
labels=data.frame(preston_9_shapes$CLASS_NAME)
preston_9_spectra=cbind(labels, preston_9_spectra)
rm(preston_9_image)

setwd("C:/Users/preston/OneDrive/Data/PhD/Chapter 4/preston-swir/preston_10_1m00_2m00_180606-092854")
preston_10_image=readGDAL("preston_10_1m00_2m00_180606-092854_refl.dat")
preston_10_shapes=readOGR(dsn = ".", layer = "preston_10_roi")
preston_10_image=brick(preston_10_image)
preston_10_image=flip(preston_10_image, 2)
preston_10_spectra=extract(preston_10_image, preston_10_shapes, fun=mean)
labels=data.frame(preston_10_shapes$CLASS_NAME)
preston_10_spectra=cbind(labels, preston_10_spectra)
rm(preston_10_image)


setwd("C:/Users/preston/OneDrive/Data/PhD/Chapter 4/preston-swir/preston_11_1m00_2m00_180606-093640")
preston_11_image=readGDAL("preston_11_1m00_2m00_180606-093640_refl.dat")
preston_11_shapes=readOGR(dsn = ".", layer = "preston_11_roi")
preston_11_image=brick(preston_11_image)
preston_11_image=flip(preston_11_image, 2)
preston_11_spectra=extract(preston_11_image, preston_11_shapes, fun=mean)
labels=data.frame(preston_11_shapes$CLASS_NAME)
preston_11_spectra=cbind(labels, preston_11_spectra)
rm(preston_11_image)

setwd("C:/Users/preston/OneDrive/Data/PhD/Chapter 4/preston-swir/preston_12_1m00_2m00_180606-094023")
preston_12_image=readGDAL("preston_12_1m00_2m00_180606-094023_refl.dat")
preston_12_shapes=readOGR(dsn = ".", layer = "preston_12_roi")
preston_12_image=brick(preston_12_image)
preston_12_image=flip(preston_12_image, 2)
preston_12_spectra=extract(preston_12_image, preston_12_shapes, fun=mean)
labels=data.frame(preston_12_shapes$CLASS_NAME)
preston_12_spectra=cbind(labels, preston_12_spectra)
rm(preston_12_image)

setwd("C:/Users/preston/OneDrive/Data/PhD/Chapter 4/preston-swir/preston_13_1m00_2m00_180606-094356")
preston_13_image=readGDAL("preston_13_1m00_2m00_180606-094356_refl.dat")
preston_13_shapes=readOGR(dsn = ".", layer = "preston_13_roi")
preston_13_image=brick(preston_13_image)
preston_13_image=flip(preston_13_image, 2)
preston_13_spectra=extract(preston_13_image, preston_13_shapes, fun=mean)
labels=data.frame(preston_13_shapes$CLASS_NAME)
preston_13_spectra=cbind(labels, preston_13_spectra)
rm(preston_13_image)

setwd("C:/Users/preston/OneDrive/Data/PhD/Chapter 4/preston-swir/preston_14_1m00_2m00_180606-094831")
preston_14_image=readGDAL("preston_14_1m00_2m00_180606-094831_refl.dat")
preston_14_shapes=readOGR(dsn = ".", layer = "preston_14_roi")
preston_14_image=brick(preston_14_image)
preston_14_image=flip(preston_14_image, 2)
preston_14_spectra=extract(preston_14_image, preston_14_shapes, fun=mean)
labels=data.frame(preston_14_shapes$CLASS_NAME)
preston_14_spectra=cbind(labels, preston_14_spectra)
rm(preston_14_image)

setwd("C:/Users/preston/OneDrive/Data/PhD/Chapter 4/preston-swir/preston_15_1m00_2m00_180606-095157")
preston_15_image=readGDAL("preston_15_1m00_2m00_180606-095157_refl.dat")
preston_15_shapes=readOGR(dsn = ".", layer = "preston_15_roi")
preston_15_image=brick(preston_15_image)
preston_15_image=flip(preston_15_image, 2)
preston_15_spectra=extract(preston_15_image, preston_15_shapes, fun=mean)
labels=data.frame(preston_15_shapes$CLASS_NAME)
preston_15_spectra=cbind(labels, preston_15_spectra)
rm(preston_15_image)

setwd("C:/Users/preston/OneDrive/Data/PhD/Chapter 4/preston-swir/preston_16_1m00_2m00_180606-095516")
preston_16_image=readGDAL("preston_16_1m00_2m00_180606-095516_refl.dat")
preston_16_shapes=readOGR(dsn = ".", layer = "preston_16_roi")
preston_16_image=brick(preston_16_image)
preston_16_image=flip(preston_16_image, 2)
preston_16_spectra=extract(preston_16_image, preston_16_shapes, fun=mean)
labels=data.frame(preston_16_shapes$CLASS_NAME)
preston_16_spectra=cbind(labels, preston_16_spectra)
rm(preston_16_image)

setwd("C:/Users/preston/OneDrive/Data/PhD/Chapter 4/preston-swir/preston_17_1m00_2m00_180606-095833")
preston_17_image=readGDAL("preston_17_1m00_2m00_180606-095833_refl.dat")
preston_17_shapes=readOGR(dsn = ".", layer = "preston_17_roi")
preston_17_image=brick(preston_17_image)
preston_17_image=flip(preston_17_image, 2)
preston_17_spectra=extract(preston_17_image, preston_17_shapes, fun=mean)
labels=data.frame(preston_17_shapes$CLASS_NAME)
preston_17_spectra=cbind(labels, preston_17_spectra)
rm(preston_17_image)


setwd("C:/Users/preston/OneDrive/Data/PhD/Chapter 4/preston-swir/preston_18_1m00_2m00_180606-100159")
preston_18_image=readGDAL("preston_18_1m00_2m00_180606-100159_refl.dat")
preston_18_shapes=readOGR(dsn = ".", layer = "preston_18_roi")
preston_18_image=brick(preston_18_image)
preston_18_image=flip(preston_18_image, 2)
preston_18_spectra=extract(preston_18_image, preston_18_shapes, fun=mean)
labels=data.frame(preston_18_shapes$CLASS_NAME)
preston_18_spectra=cbind(labels, preston_18_spectra)
rm(preston_18_image)


colnames(preston_2_spectra)=colnames(preston_1_spectra)
colnames(preston_3_spectra)=colnames(preston_1_spectra)
colnames(preston_4_spectra)=colnames(preston_1_spectra)
colnames(preston_5_spectra)=colnames(preston_1_spectra)
colnames(preston_6_spectra)=colnames(preston_1_spectra)
colnames(preston_7_spectra)=colnames(preston_1_spectra)
colnames(preston_8_spectra)=colnames(preston_1_spectra)
colnames(preston_9_spectra)=colnames(preston_1_spectra)
colnames(preston_10_spectra)=colnames(preston_1_spectra)
colnames(preston_11_spectra)=colnames(preston_1_spectra)
colnames(preston_12_spectra)=colnames(preston_1_spectra)
colnames(preston_13_spectra)=colnames(preston_1_spectra)
colnames(preston_14_spectra)=colnames(preston_1_spectra)
colnames(preston_15_spectra)=colnames(preston_1_spectra)
colnames(preston_16_spectra)=colnames(preston_1_spectra)
colnames(preston_17_spectra)=colnames(preston_1_spectra)
colnames(preston_18_spectra)=colnames(preston_1_spectra)



spectra=rbind(
  preston_1_spectra,
  preston_2_spectra,
  preston_3_spectra,
  preston_4_spectra,
  preston_5_spectra,
  preston_6_spectra,
  preston_7_spectra,
  preston_8_spectra,
  preston_9_spectra,
  preston_10_spectra,
  preston_11_spectra,
  preston_12_spectra,
  preston_13_spectra,
  preston_14_spectra,
  preston_15_spectra,
  preston_16_spectra,
  preston_17_spectra,
  preston_18_spectra
 )

lab=spectra$preston_1_shapes.CLASS_NAME

temp=data.frame(duplicated(lab))

lab=cbind(lab, temp)

lab=subset(lab, duplicated.lab.=='TRUE')


write.csv(spectra, "Chapter_4_Training_Spectra.csv")


