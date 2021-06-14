#aggregate size
spatial_val=foreach(i=17:36, .combine=rbind) %dopar%
{
  library(geoR)
  bh=paste("BH0", i,sep="")
  temp=subset(data_file, borehole==bh)
  treat=unique(temp$treat)
  a=max(temp$Var2)-40
  results=vector('list')
  q=0
  for (j in seq(1, a, by=40)){
    q=q+1
    b=j+100
    temp1=subset(temp, Var2>=j & Var2<=b)
    g1=as.geodata(temp1, coords.col=4:5, data.col=1)
    g2=as.geodata(temp1, coords.col=4:5, data.col=2)
    g3=as.geodata(temp1, coords.col=4:5, data.col=3)
    carbon.lag=NA
    nitrogen.lag=NA
    clay.lag=NA
    try({
    v1=variog(g1, estimator.type='modulus', trend='cte', option='bin', uvec=seq(1, 50, by=1))
    v2=variog(g2, estimator.type='modulus', trend='cte', option='bin', uvec=seq(1, 50, by=1))
    v3=variog(g3, estimator.type='modulus', trend='cte', option='bin', uvec=seq(1, 50, by=1))
    sph1=variofit(v1,cov.model="circular",weights="cressie")
    sph2=variofit(v2,cov.model="circular",weights="cressie")
    sph3=variofit(v3,cov.model="circular",weights="cressie")
    })
    carbon.lag=sph1$cov.pars[2]
    nitrogen.lag=sph2$cov.pars[2]
    clay.lag=sph3$cov.pars[2]
    res1=data.frame(cbind(carbon.lag, nitrogen.lag, clay.lag))
    res1[res1>100]=NA
    res1$treat=unique(temp1$treat)
    res1$depth=b
    results[[q]]=res1
  }
  results=do.call(rbind, results)
  results=data.frame(results)
  results$borehole=i
  results$treat=treat
  results
}


setwd("C:\\Users\\preston\\OneDrive\\Data\\PhD\\Chapter 4")

#write.csv(spatial_val, 'Variogram_results.csv')

#results=read.csv("Variogram_results.csv")

results=spatial_val
results=na.omit(results)

#analysis of results

#range
averages_val=foreach(i=17:36, .combine=rbind) %dopar%
{
  bh=paste("BH0", i,sep="")
  a_cut=subset(a_depth, borehole==bh)
  a_cut=a_cut$cut
  temp=subset(results, borehole==i)
  treat=unique(temp$treat)
  temp_a=subset(temp, depth<(a_cut))
  temp_b=subset(temp, depth>(a_cut))
  a_carbon=mean(temp_a$carbon.lag)
  a_nitrogen=mean(temp_a$nitrogen.lag)
  a_clay=mean(temp_a$clay.lag)
  b_carbon=mean(temp_b$carbon.lag)
  b_nitrogen=mean(temp_b$nitrogen.lag)
  b_clay=mean(temp_b$clay.lag)
  table_values=cbind(a_carbon, a_nitrogen, a_clay, b_carbon, b_nitrogen, b_clay)
  table_values=data.frame(table_values)
  table_values$borehole=bh
  table_values$treat=treat
  table_values
}

averages_val=na.omit(averages_val)
averages_val[18,8]="class"

variogram_averages=foreach(i=1:4, .combine=rbind) %dopar%
{ 
  x=c("ae", "cf", "cg", "class")
  temp=subset(averages_val, treat==x[i])
  temp_vals=temp[,1:6]
  aggdata=aggregate(temp_vals, by=list(temp$treat), FUN=mean)
  aggdata1=aggregate(temp_vals, by=list(temp$treat), FUN=sd)
  aggdata$treat=x[i]
  cbind(aggdata, aggdata1)
}

variogram_averages[,-c(1,8,9)]=variogram_averages[,-c(1,8,9)]/12.8
variogram_averages=variogram_averages[,-c(8,9)]

write.csv(dat, 'Variogram_results_borehole.csv')

dat=merge(averages_val, gps, by='borehole')

library(nlme)
library(lme4)

carbon_a.fit=gls(a_carbon~treat.x, data=dat, correlation=corExp(form=~easting+northing, nugget=T))
carbon_b.fit=gls(b_carbon~treat.x, data=dat, correlation=corExp(form=~easting+northing, nugget=T))
nitrogen_a.fit=gls(a_nitrogen~treat.x, data=dat, correlation=corExp(form=~easting+northing, nugget=T))
nitrogen_b.fit=gls(b_nitrogen~treat.x, data=dat, correlation=corExp(form=~easting+northing, nugget=T))


summary(carbon_a.fit)
summary(carbon_b.fit)
summary(nitrogen_a.fit)
summary(nitrogen_b.fit)



library(gridExtra)
library(grid)
library(ggplot2)
library(lattice)
colnames(variogram_averages)=c('Depth', 'Carbon.Lag','Nitrogen.Lag','Clay.Lag', 'Treat')
variogram_averages$Depth=variogram_averages$Depth/12.8



ae_carbon=subset(variogram_averages, Treat=='ae')
cf_carbon=subset(variogram_averages, Treat=='cf')
cg_carbon=subset(variogram_averages, Treat=='cg')
class_carbon=subset(variogram_averages, Treat=='class')

ae_tn=subset(tn_averages, Treat=='ae')
cf_tn=subset(tn_averages, Treat=='cf')
cg_tn=subset(tn_averages, Treat=='cg')
class_tn=subset(tn_averages, Treat=='class')

median(ae_carbon$Carbon.Lag)
median(cf_carbon$Carbon.Lag)
median(cg_carbon$Carbon.Lag)
median(class_carbon$Carbon.Lag)

median(ae_carbon$Nitrogen.Lag)
median(cf_carbon$Nitrogen.Lag)
median(cg_carbon$Nitrogen.Lag)
median(class_carbon$Nitrogen.Lag)

c.top=ggplot(moran.val.top, aes(x=treat, y=carbon)) + geom_boxplot() + ylab("Soil Organic Carbon Concentration (%)") + xlab("Rotation") + scale_x_discrete(labels=c("Agro-ecological", "Continuous Forage", "Continuous Grain", "Classical"))
pl <- grid.arrange(c.top, moran.c.top, nrow=1, ncol=2)



