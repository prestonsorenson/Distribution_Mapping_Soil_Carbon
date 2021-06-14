#analysis
load("C:\\Users\\preston\\OneDrive\\Data\\PhD\\Chapter 4\\Chapter_4_Predicted_Data.RData")
load("C:\\Users\\prest\\OneDrive\\Data\\PhD\\Chapter 4\\Chapter_4_Predicted_Data.RData")
library(doParallel)
cl=makeCluster(4)
registerDoParallel(cl)
data_file=na.omit(data_file)

#average by depth
averages_file=foreach(i=17:36, .combine=rbind) %dopar%
{ 
bh=paste("BH0", i,sep="")
temp=subset(data_file, borehole==bh)
temp_vals=temp[,1:3]
temp_vals$c_n=temp_vals$tc/temp_vals$tn
aggdata=aggregate(temp_vals, by=list(temp$Var2), FUN=median)
colnames(aggdata)=c("depth", "tc", "tn", "clay", "c_n")
aggdata$borehole=bh
aggdata
}


#determine where A horizon stops
a_depth=foreach(i=17:36, .combine=rbind) %dopar%
{
bh=paste("BH0", i,sep="")
bh017=subset(averages_file, borehole==bh)
a=smooth.spline(bh017$tc, lambda=0.0001)
a=a$y
a=which(a>0.8)
b=data.frame(as.numeric(which.max(a)))
cbind(bh, b)
}

colnames(a_depth)=c("borehole", "cut")
#moran's i calculations
moran_val=foreach(i=17:36, .combine=rbind) %dopar%
{
  library(raster)
  bh=paste("BH0", i,sep="")
  temp=subset(data_file, borehole==bh)
  treat=unique(temp$treat)
  a=max(temp$Var2)-40
  results=vector('list')
  q=0
  for (j in seq(1, a, by=40)){
  q=q+1
  b=j+40
  temp1=subset(temp, Var2>=j & Var2<=b)
  carbon=mean(temp1$tc)
  nitrogen=mean(temp1$tn)
  c_n=mean(temp1$tc/temp1$tn)
  clay=mean(temp1$clay)
  temp_carbon=temp1[,c(1,4,5)]
  coordinates(temp_carbon)=~Var1+Var2
  temp_carbon=rasterFromXYZ(temp_carbon)
  temp_nitrogen=temp1[,c(2,4,5)]
  coordinates(temp_nitrogen)=~Var1+Var2
  temp_nitrogen=rasterFromXYZ(temp_nitrogen)
  temp_clay=temp1[,c(3,4,5)]
  coordinates(temp_clay)=~Var1+Var2
  temp_clay=rasterFromXYZ(temp_clay)
  moran_carbon=Moran(temp_carbon)
  moran_nitrogen=Moran(temp_nitrogen)
  moran_clay=Moran(temp_clay)
  results[[q]]=c(j, moran_carbon, moran_nitrogen, moran_clay, carbon, nitrogen, c_n, clay)
  }
  results=do.call(rbind, results)
  results=data.frame(results)
  results$borehole=i
  results$treat=treat
  colnames(results)=c("start_depth", "moran_carbon", "moran_nitrogen", "moran_clay","carbon", "nitrogen", "c_n", "clay", "borehole", "treat")
  results
}


setwd("C:\\Users\\prest\\OneDrive\\Data\\PhD\\Chapter 4")
write.csv(moran_val, "Chapter_4_Summary_Statistics_12Sept2018.csv")

#range
averages_val=foreach(i=17:36, .combine=rbind) %dopar%
{
  bh=paste("BH0", i,sep="")
  a_cut=subset(a_depth, borehole==bh)
  a_cut=a_cut$cut
  temp=subset(data_file, borehole==bh)
  temp_a=subset(temp, Var2<(a_cut))
  temp_b=subset(temp, Var2>(a_cut))
  a_carbon=mean(temp_a$tc)
  a_nitrogen=mean(temp_a$tn)
  a_clay=mean(temp_a$clay)
  b_carbon=mean(temp_b$tc)
  b_nitrogen=mean(temp_b$tn)
  b_clay=mean(temp_b$clay)
  a_carbon_sd=sd(temp_a$tc)
  a_nitrogen_sd=sd(temp_a$tn)
  a_clay_sd=sd(temp_a$clay)
  b_carbon_sd=sd(temp_b$tc)
  b_nitrogen_sd=sd(temp_b$tn)
  b_clay_sd=sd(temp_b$clay)
  table_values=cbind(a_carbon, a_nitrogen, a_clay, b_carbon, b_nitrogen, b_clay, a_carbon_sd, a_nitrogen_sd, a_clay_sd, b_carbon_sd, b_nitrogen_sd, b_clay_sd)
  table_values=data.frame(table_values)
  table_values$borehole=bh
  table_values
}


setwd("C:\\Users\\preston\\OneDrive\\Data\\PhD\\Chapter 4")
write.csv(averages_val, "Chapter_4_Average_Concentrations.csv")

#range

#average by depth
x=c("ae", "cf", "cg", "class")

moran_averages=foreach(i=1:4, .combine=rbind) %dopar%
{ 
  temp=subset(moran_val, treat==x[i])
  temp_vals=temp[,2:8]
  aggdata=aggregate(temp_vals, by=list(temp$start_depth), FUN=median)
  aggdata$treat=x[i]
  aggdata
}


#spatial analysis
library(nlme)
library(lme4)
setwd("C:\\Users\\preston\\OneDrive\\Data\\PhD\\Chapter 4")
moran.val=read.csv("Chapter_4_Summary_Statistics_12Sept2018.csv")
gps=read.csv("Breton_23May2018.csv")

depths=unique(moran.val$start_depth)

gls_results=foreach(i=1:length(depths), .combine=rbind) %dopar%
{
library(nlme)
library(lme4)
depth.sub=depths[i]
moran.val.top=subset(moran.val, start_depth==depth.sub)
moran.val.top=merge(moran.val.top, gps, by="borehole")
try({c.fit=gls(scale(carbon)~treat.x, data=moran.val.top, correlation=corExp(form=~easting+northing, nugget=T))})
try({n.fit=gls(scale(nitrogen)~treat.x, data=moran.val.top, correlation=corExp(form=~easting+northing, nugget=T))})
try({c_n.fit=gls(scale(c_n)~treat.x, data=moran.val.top, correlation=corExp(form=~easting+northing, nugget=T))})
try({mc.fit=gls(scale(moran_carbon)~treat.x, data=moran.val.top, correlation=corExp(form=~easting+northing, nugget=T))})
try({mn.fit=gls(scale(moran_nitrogen)~treat.x, data=moran.val.top, correlation=corExp(form=~easting+northing, nugget=T))})
res1=data.frame(summary(c.fit)$tTable)
res2=data.frame(summary(n.fit)$tTable)
res3=data.frame(summary(c_n.fit)$tTable)
res4=data.frame(summary(mc.fit)$tTable)
res5=data.frame(summary(mn.fit)$tTable)
res1$treat=row.names(res1)
res2$treat=row.names(res2)
res3$treat=row.names(res3)
res4$treat=row.names(res4)
res5$treat=row.names(res5)
res1$name='carbon'
res2$name='nitrogen'
res3$name='c_n'
res4$name='mc'
res5$name='mn'
res=rbind(res1, res2, res3, res4, res5)
res$depth=depth.sub
res
}

write.csv(gls_results, "Moran_GLS_Results.csv")


gls_sig=subset(gls_results, p.value<0.05)

write.csv(gls_sig, "Moran_Results_sig.csv")


depths=unique(averages_file$depth)

gps$borehole=c("BH017","BH018","BH019","BH020","BH021","BH022","BH023","BH024","BH025","BH026","BH027","BH028","BH029","BH030","BH031","BH032","BH033","BH034","BH035","BH036")

gls_results_all=foreach(i=1:length(depths), .combine=rbind) %dopar%
{
  library(nlme)
  library(lme4)
  depth.sub=depths[i]
  moran.val.top=subset(averages_file, depth==depth.sub)
  moran.val.top=merge(moran.val.top, gps, by="borehole")
  try({c.fit=gls(scale(tc)~treat, data=moran.val.top, correlation=corExp(form=~easting+northing, nugget=T))})
  try({n.fit=gls(scale(tn)~treat, data=moran.val.top, correlation=corExp(form=~easting+northing, nugget=T))})
  try({c_n.fit=gls(scale(c_n)~treat, data=moran.val.top, correlation=corExp(form=~easting+northing, nugget=T))})
  res1=data.frame(summary(c.fit)$tTable)
  res2=data.frame(summary(n.fit)$tTable)
  res3=data.frame(summary(c_n.fit)$tTable)
  res1$treat=row.names(res1)
  res2$treat=row.names(res2)
  res3$treat=row.names(res3)
  res1$name='carbon'
  res2$name='nitrogen'
  res3$name='c_n'
  res=rbind(res1, res2, res3)
  res$depth=depth.sub
  res
}

write.csv(gls_results_all, "TC_TN_GLS_Results.csv")


gls_sig=subset(gls_results_all, p.value<0.05)

write.csv(gls_sig, "TC_TN_GLS_Results_sig.csv")


gls_cf=subset(gls_sig, treat=="treatcf")
gls_cg=subset(gls_sig, treat=="treatcg")
gls_class=subset(gls_sig, treat=="treatclass")

depths_all=depths/12.4

gls_cf_carbon=subset(gls_cf, name=="carbon")
gls_cg_carbon=subset(gls_cg, name=="carbon")
gls_class_carbon=subset(gls_class, name=="carbon")

gls_cf_nitrogen=subset(gls_cf, name=="nitrogen")
gls_cg_nitrogen=subset(gls_cg, name=="nitrogen")
gls_class_nitrogen=subset(gls_class, name=="nitrogen")

gls_cf_c_n=subset(gls_cf, name=="c_n")
gls_cg_c_n=subset(gls_cg, name=="c_n")
gls_class_c_n=subset(gls_class, name=="c_n")

gls_cf_carbon
gls_cg_carbon
gls_class_carbon

gls_cf_nitrogen
gls_cg_nitrogen
gls_class_nitrogen

gls_cf_c_n
gls_cg_c_n
gls_class_c_n


#plots

#plots moran
library(gridExtra)
library(grid)
library(ggplot2)
library(lattice)

averages_file$depth=averages_file$depth/13

#ae_data=subset(averages_file, treat=='ae')
#cg_data=subset(averages_file, treat=='cg')
#cf_data=subset(averages_file, treat=='cf')
#class_data=subset(averages_file, treat=='class')

#ae_data$depth=ae_data$start_depth/12.4
#cg_data$depth=cg_data$start_depth/12.4
#cf_data$depth=cf_data$start_depth/12.0
#class_data$depth=class_data$start_depth/12.4

averages_file=merge(averages_file, gps, by="borehole")


#average by depth
x=c("ae", "cf", "cg", "class")

averages_plotting=foreach(i=1:4, .combine=rbind) %dopar%
{ 
  temp=subset(averages_file, treat==x[i])
  temp_vals=temp[,3:6]
  aggdata=aggregate(temp_vals, by=list(temp$depth), FUN=median)
  aggdata2=aggregate(temp_vals, by=list(temp$depth), FUN=function(x) quantile(x, 0.05))
  aggdata3=aggregate(temp_vals, by=list(temp$depth), FUN=function(x) quantile(x, 0.95))
  aggdata2=aggdata2[,-1]
  aggdata3=aggdata3[,-1]
  colnames(aggdata)=c("depth", "carbon", "nitrogen", "clay", "c_n")
  colnames(aggdata2)=c("q5_tc", "q5_tn","q5_clay","q5_c_n")
  colnames(aggdata3)=c("q95_tc", "q95_tn","q95_clay","q95_c_n")
  aggdata=cbind(aggdata, aggdata2, aggdata3)
  aggdata$Rotation=x[i]
  aggdata
}


averages_top=subset(averages_plotting, depth<25)
averages_sub=subset(averages_plotting, depth>25)



carbon_plot_top=ggplot(averages_top,aes(x=depth,y=carbon, group=Rotation, colour=Rotation)) + geom_point(alpha=0.25) + geom_smooth(se=FALSE, span=0.1) + coord_flip() + scale_x_reverse() + ylab('Soil Organic Carbon Concentration (%)') + scale_y_continuous(position="top", limits=c(0, 7)) + xlab("Depth (cm)") + theme(legend.position="none")
carbon_plot_top

nitrogen_plot_top=ggplot(averages_top,aes(x=depth,y=nitrogen, group=Rotation, colour=Rotation)) + geom_point(alpha=0.25) + geom_smooth(se=FALSE, span=0.1) + coord_flip() + scale_x_reverse() + ylab('Total Nitrogen Concentration (%)') + scale_y_continuous(position="top", limits=c(0, 0.5)) + xlab("Depth (cm)") + theme(legend.position="none")
nitrogen_plot_top

c_n_plot_top=ggplot(averages_top,aes(x=depth,y=c_n, group=Rotation, colour=Rotation)) + geom_point(alpha=0.25) + geom_smooth(se=FALSE, span=0.1) + coord_flip() + scale_x_reverse() + ylab('Carbon to Nitrogen Ratio') + scale_y_continuous(position="top", limits=c(4, 14)) + xlab("Depth (cm)") + theme(legend.position="none")
c_n_plot_top

carbon_plot_sub=ggplot(averages_sub,aes(x=depth,y=carbon, group=Rotation, colour=Rotation)) + geom_point(alpha=0.25) + geom_smooth(se=FALSE, span=0.1) + coord_flip() + scale_x_reverse() + ylab('Soil Organic Carbon Concentration (%)') + scale_y_continuous(position="top", limits=c(0.2, 0.5)) + xlab("Depth (cm)")+ theme(legend.position="none") 
carbon_plot_sub

nitrogen_plot_sub=ggplot(averages_sub,aes(x=depth,y=nitrogen, group=Rotation, colour=Rotation)) + geom_point(alpha=0.25) + geom_smooth(se=FALSE, span=0.1) + coord_flip() + scale_x_reverse() + ylab('Total Nitrogen Concentration (%)') + scale_y_continuous(position="top", limits=c(0.025, 0.09)) + xlab("Depth (cm)") + theme(legend.position="none")
nitrogen_plot_sub

c_n_plot_sub=ggplot(averages_sub,aes(x=depth,y=c_n, group=Rotation, colour=Rotation)) + geom_point(alpha=0.25) + geom_smooth(se=FALSE, span=0.1) + coord_flip() + scale_x_reverse() + ylab('Carbon to Nitrogen Ratio') + scale_y_continuous(position="top", limits=c(4, 11)) + xlab("Depth (cm)")+ theme(legend.position="none") 
c_n_plot_sub

#moran plots
moran_averages$depth=moran_averages$Group.1/12.4
colnames(moran_averages)=c("Group.1","moran_carbon","moran_nitrogen","moran_clay","carbon","nitrogen","c_n","clay","Rotation","depth")
moran_top=subset(moran_averages, depth<25)
moran_sub=subset(moran_averages, depth>25)

moran_carbon_plot_top=ggplot(moran_top,aes(x=depth,y=moran_carbon, group=Rotation, colour=Rotation)) + geom_point(alpha=0.25) + geom_smooth(se=FALSE, span=0.25) + coord_flip() + scale_x_reverse() + ylab('Soil Organic Carbon Moran i value') + scale_y_continuous(position="top", limits=c(0.4, 0.7)) + xlab("Depth (cm)") + theme(legend.position="none")
moran_carbon_plot_top

moran_nitrogen_plot_top=ggplot(moran_top,aes(x=depth,y=moran_nitrogen, group=Rotation, colour=Rotation)) + geom_point(alpha=0.25) + geom_smooth(se=FALSE, span=0.25) + coord_flip() + scale_x_reverse() + ylab('Total Nitrogen Moran i value') + scale_y_continuous(position="top", limits=c(0.25, 0.75)) + xlab("Depth (cm)") 
moran_nitrogen_plot_top


moran_carbon_plot_sub=ggplot(moran_sub,aes(x=depth,y=moran_carbon, group=Rotation, colour=Rotation)) + geom_point(alpha=0.25) + geom_smooth(se=FALSE, span=0.25) + coord_flip() + scale_x_reverse() + ylab('Soil Organic Carbon Moran i value') + scale_y_continuous(position="top", limits=c(0.2, 0.7)) + xlab("Depth (cm)") + theme(legend.position="none")
moran_carbon_plot_sub

moran_nitrogen_plot_sub=ggplot(moran_sub,aes(x=depth,y=moran_nitrogen, group=Rotation, colour=Rotation)) + geom_point(alpha=0.25) + geom_smooth(se=FALSE, span=0.25) + coord_flip() + scale_x_reverse() + ylab('Total Nitrogen Moran i value') + scale_y_continuous(position="top", limits=c(0.1, 0.6)) + xlab("Depth (cm)") 
moran_nitrogen_plot_sub


grid_arrange_shared_legend <- function(..., ncol = length(list(...)), nrow = 1, position = c("bottom", "right")) {
  plots <- list(...)
  position <- match.arg(position)
  g <- ggplotGrob(plots[[1]] + 
                    theme(legend.position = position))$grobs
  legend <- g[[which(sapply(g, function(x) x$name) == "guide-box")]]
  lheight <- sum(legend$height)
  lwidth <- sum(legend$width)
  gl <- lapply(plots, function(x) x +
                 theme(legend.position = "none"))
  gl <- c(gl, ncol = ncol, nrow = nrow)
  
  combined <- switch(position,
                     "bottom" = arrangeGrob(do.call(arrangeGrob, gl), 
                                            legend,ncol = 1,
                                            heights = unit.c(unit(1, "npc") - lheight, lheight)),
                     "right" = arrangeGrob(do.call(arrangeGrob, gl),
                                           legend, ncol = 2,
                                           widths = unit.c(unit(1, "npc") - lwidth, lwidth)))
  
  grid.newpage()
  grid.draw(combined)
  
  # return gtable invisibly
  invisible(combined)
}

grid_arrange_shared_legend(carbon_plot_top, nitrogen_plot_top, c_n_plot_top, moran_carbon_plot_top, moran_nitrogen_plot_top, nrow=1, ncol=5)
grid_arrange_shared_legend(carbon_plot_sub, nitrogen_plot_sub, c_n_plot_sub, moran_carbon_plot_sub, moran_nitrogen_plot_sub, nrow=1, ncol=5)


#plots moran
library(gridExtra)
library(grid)
library(ggplot2)
library(lattice)

depths_all=depths/12.4

gls_cf$depth=gls_cf$depth/12.4
gls_cg$depth=gls_cg$depth/12.4
gls_class$depth=gls_class$depth/12.4

gls_cf_carbon=subset(gls_cf, name=="carbon")
gls_cg_carbon=subset(gls_cg, name=="carbon")
gls_class_carbon=subset(gls_class, name=="carbon")

gls_cf_nitrogen=subset(gls_cf, name=="nitrogen")
gls_cg_nitrogen=subset(gls_cg, name=="nitrogen")
gls_class_nitrogen=subset(gls_class, name=="nitrogen")

gls_cf_c_n=subset(gls_cf, name=="c_n")
gls_cg_c_n=subset(gls_cg, name=="c_n")
gls_class_c_n=subset(gls_class, name=="c_n")

gls_cf_mc=subset(gls_cf, name=="mc")
gls_cg_mc=subset(gls_cg, name=="mc")
gls_class_mc=subset(gls_class, name=="mc")

gls_cf_mn=subset(gls_cf, name=="mn")
gls_cg_mn=subset(gls_cg, name=="mn")
gls_class_mn=subset(gls_class, name=="mn")


#vertical profile plots

#plots moran
library(gridExtra)
library(grid)
library(ggplot2)
library(lattice)
ae_data=subset(moran_averages, treat=='ae')
cg_data=subset(moran_averages, treat=='cg')
cf_data=subset(moran_averages, treat=='cf')
class_data=subset(moran_averages, treat=='class')

ae_data$depth=ae_data$start_depth/12.4
cg_data$depth=cg_data$start_depth/12.4
cf_data$depth=cf_data$start_depth/12.0
class_data$depth=class_data$start_depth/12.4


carbon_plot=ggplot(ae_data,aes(x=depth,y=carbon)) + geom_line() + coord_flip() + scale_x_reverse() + ylab('Soil Organic Carbon Concentration (%)') + scale_y_continuous(position="top", limits=c(0, 5.5)) + xlab("Depth (cm)") 
carbon_plot



carbon_plot=ggplot(ae_data,aes(x=depth,y=carbon)) + geom_line() 





#ggplots
setwd("C:\\Users\\preston\\OneDrive\\Papers\\PhD\\Chapter 4\\Figures")
library(gridExtra)
library(grid)
library(ggplot2)
library(lattice)
c.top=ggplot(moran.val.top, aes(x=treat, y=carbon)) + geom_boxplot() + ylab("Soil Organic Carbon Concentration (%)") + xlab("Rotation") + scale_x_discrete(labels=c("Agro-ecological", "Continuous Forage", "Continuous Grain", "Classical"))
moran.c.top=ggplot(moran.val.top, aes(x=treat, y=moran_carbon)) + geom_boxplot() + ylab("Moran's i") + xlab("Rotation") + scale_x_discrete(labels=c("Agro-ecological", "Continuous Forage", "Continuous Grain", "Classical"))
pl <- grid.arrange(c.top, moran.c.top, nrow=1, ncol=2)

c.sub=ggplot(moran.val.sub, aes(x=treat, y=carbon)) + geom_boxplot() + ylab("Soil Organic Carbon Concentration (%)") + xlab("Rotation") + scale_x_discrete(labels=c("Agro-ecological", "Continuous Forage", "Continuous Grain", "Classical"))
moran.c.sub=ggplot(moran.val.sub, aes(x=treat, y=moran_carbon)) + geom_boxplot() + ylab("Moran's i") + xlab("Rotation") + scale_x_discrete(labels=c("Agro-ecological", "Continuous Forage", "Continuous Grain", "Classical"))
p2 <- grid.arrange(c.sub, moran.c.sub, nrow=1, ncol=2)

n.top=ggplot(moran.val.top, aes(x=treat, y=nitrogen)) + geom_boxplot() + ylab("Total Nitrogen Concentration (%)") + xlab("Rotation") + scale_x_discrete(labels=c("Agro-ecological", "Continuous Forage", "Continuous Grain", "Classical"))
moran.n.top=ggplot(moran.val.top, aes(x=treat, y=moran_carbon)) + geom_boxplot() + ylab("Moran's i") + xlab("Rotation") + scale_x_discrete(labels=c("Agro-ecological", "Continuous Forage", "Continuous Grain", "Classical"))
p3 <- grid.arrange(n.top, moran.n.top, nrow=1, ncol=2)

n.sub=ggplot(moran.val.sub, aes(x=treat, y=nitrogen)) + geom_boxplot() + ylab("Total Nitrogen Concentration (%)") + xlab("Rotation") + scale_x_discrete(labels=c("Agro-ecological", "Continuous Forage", "Continuous Grain", "Classical"))
moran.n.sub=ggplot(moran.val.sub, aes(x=treat, y=moran_nitrogen)) + geom_boxplot() + ylab("Moran's i") + xlab("Rotation") + scale_x_discrete(labels=c("Agro-ecological", "Continuous Forage", "Continuous Grain", "Classical"))
p4 <- grid.arrange(n.sub, moran.n.sub, nrow=1, ncol=2)


clay.top=ggplot(moran.val.top, aes(x=treat, y=clay_carbon_r)) + geom_boxplot() + ylim(c(-0.5, 0.2)) + ylab("Topsoil Carbon Clay Content Correlation") + xlab("Rotation") + scale_x_discrete(labels=c("Agro-ecological", "Continuous Forage", "Continuous Grain", "Classical"))
clay.sub=ggplot(moran.val.sub, aes(x=treat, y=clay_carbon_r)) + geom_boxplot() + ylim(c(-0.5, 0.2)) +ylab("Subsoil Carbon Clay Content Correlation") + xlab("Rotation") + scale_x_discrete(labels=c("Agro-ecological", "Continuous Forage", "Continuous Grain", "Classical"))
p5 <- grid.arrange(clay.top, clay.sub, nrow=1, ncol=2)







