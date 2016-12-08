rm(list=ls())

library(xts)
library(zyp)

in.dir <- '/home/data/projects/hydrology/small_projects/regional_climate_summaries/data/'
##stations <- c("08MG005","08MB006","08NB005","08NF001","07FB001","08JE001","07EC002","08JB002","08LA001","08LD001")  ##STATIONS HAVE VIC MODEL RESULTS
##stations <- "08MH006_Daily_Flow_ts.csv"  ## example station for testing
stations <- list.files(in.dir, pattern="_Daily_Flow_ts.csv")
clip <- '1976-01-01/2013-12-31'
start.year <- unlist(strsplit(clip, "-01-01/"))[1]
end.year <- '2013'

for(station in stations){
  ##print(station)	
  ##READ DATA and CONVERT TO XTS CLASS
  dat <- read.csv(paste0(in.dir, station), header=TRUE, as.is=TRUE)
  dat <- dat[,c(3,4)]
  dat$Date <- as.Date(dat$Date, format="%Y/%m/%d")
  dat.na.rmd <- dat[!is.na(dat$Date),]  ## Should only be removing the last few rows.
  
  ##print(head(dat.na.rmd))
  ##print(tail(dat.na.rmd))
  ##print(length(t(dat)) - length(t(dat.na.rmd)))
  print(nrow(dat) - nrow(dat.na.rmd))
  dat_xts <- as.xts(dat.na.rmd[,-1], order.by=dat.na.rmd[,1])
  dat_xts.sub <- dat_xts[clip]
  print(str(dat_xts))
  print(str(dat_xts.sub))

  ##COMPUTE ANNUAL MEAN, MIN and MAX, MONTHLY MEAN, JUN-SEP MIN, APR-JUN MAX and SEASONAL MEANS
  ann.mean <- apply.yearly(dat_xts.sub,mean)
  ann.min <- apply.yearly(dat_xts.sub,min)
  ann.max <- apply.yearly(dat_xts.sub,max)
  mon.mean <- apply.monthly(dat_xts.sub,mean)
  JAS.min <- apply.yearly(mon.mean[.indexmon(mon.mean) %in% c(7:9)],min)
  AMJ.max <- apply.yearly(mon.mean[.indexmon(mon.mean) %in% c(4:6)],max)
  DJF.mean <- apply.yearly(mon.mean[.indexmon(mon.mean) %in% c(12:2)],mean)
  MAM.mean <- apply.yearly(mon.mean[.indexmon(mon.mean) %in% c(3:5)],mean)
  JJA.mean <- apply.yearly(mon.mean[.indexmon(mon.mean) %in% c(6:8)],mean)
  SON.mean <- apply.yearly(mon.mean[.indexmon(mon.mean) %in% c(9:11)],mean)
  means <- rbind(mean(ann.mean,na.rm=TRUE),mean(ann.min,na.rm=TRUE),mean(ann.max,na.rm=TRUE),mean(JAS.min,na.rm=TRUE),mean(AMJ.max,na.rm=TRUE),mean(DJF.mean,na.rm=TRUE),mean(MAM.mean,na.rm=TRUE),mean(JJA.mean,na.rm=TRUE),mean(SON.mean,na.rm=TRUE))
  mins <- rbind(min(ann.mean,na.rm=TRUE),min(ann.min,na.rm=TRUE),min(ann.max,na.rm=TRUE),min(JAS.min,na.rm=TRUE),min(AMJ.max,na.rm=TRUE),min(DJF.mean,na.rm=TRUE),min(MAM.mean,na.rm=TRUE),min(JJA.mean,na.rm=TRUE),min(SON.mean,na.rm=TRUE))
  maxs <- rbind(max(ann.mean,na.rm=TRUE),max(ann.min,na.rm=TRUE),max(ann.max,na.rm=TRUE),max(JAS.min,na.rm=TRUE),max(AMJ.max,na.rm=TRUE),max(DJF.mean,na.rm=TRUE),max(MAM.mean,na.rm=TRUE),max(JJA.mean,na.rm=TRUE),max(SON.mean,na.rm=TRUE))  
  
  ##print(str(ann.mean))

  ##COMPUTE YUE PILON TRENDS
  trend.ann.mean <- zyp.yuepilon(ann.mean)
  trend.ann.min <- zyp.yuepilon(ann.min)
  trend.ann.max <- zyp.yuepilon(ann.max)
  trend.JAS.min <- zyp.yuepilon(JAS.min)
  trend.AMJ.max <- zyp.yuepilon(AMJ.max)
  trend.DJF.mean <- zyp.yuepilon(DJF.mean)
  trend.MAM.mean <- zyp.yuepilon(MAM.mean)
  trend.JJA.mean <- zyp.yuepilon(JJA.mean)
  trend.SON.mean <- zyp.yuepilon(SON.mean)
  trends <- rbind(trend.ann.mean,trend.ann.min,trend.ann.max,trend.JAS.min,trend.AMJ.max,trend.DJF.mean,trend.MAM.mean,trend.JJA.mean,trend.SON.mean)
  
  ##DIVIDE TOTAL TREND OVER THE PERIOD BY THE INTERCEPT TO GET THE RELATIVE TREND (see Climate Overview Methods)
  ##The total trend over the period, multiplied by the length of record, was divided by the intercept to get a percentage trend relative to the intercept. This standardized the results from the different streams making them directly comparable.
  noyear <- (trends[,"trendp"]/trends[,"trend"])
  reltrend <- (trends[,"trendp"]*noyear)/trends[,"intercept"]
  perttrend <- (trends[,"trendp"])/means*100  ## Trend expressed as a percent of the mean flow for that metric
  lowpertrend <- (trends[,"lbound"])/means*100  ## Lower bound of trend expressed as a percent of the mean flow for that metric
  upppertrend <- (trends[,"ubound"])/means*100  ## Upper bound of trend expressed as a percent of the mean flow for that metric
  trendsplus <- cbind(trends, noyear, round(reltrend,2), round(lowpertrend,2), round(perttrend,2), round(upppertrend,2), round(trends[,"sig"],2), round(means,2), round(mins,2), round(maxs,2))
  colnames(trendsplus) <- c(colnames(trends),"noyear","reltrend","lowpertr","perttrend","upppertr","sig","mean","min","max")
  
  ##The total trend over the period, multiplied by the length of record, was divided by the intercept to get a percentage trend relative to the intercept. This standardized the results from the different streams making them directly comparable.
  write.csv(trendsplus, paste0("../tables/trends/", station, "_trends_", start.year, end.year, ".csv"))
  
  ##PLOTTING - NO SEASONAL TREND PLOTS YET
  # # out.dir <- '../nobackup_plots/'
  # # postscript(file=paste0(out.dir, "RHBN_trends_", station, "_Annual_Mean_Min_Max_", start.year, "_", end.year, ".eps", sep = ""), width=6, height=8, pointsize=10)
  # # par(mfrow=c(5,1), mar=c(1,5,1,2), oma=c(5,0,5,0), bg="white")

  # # cex.axis=1.2
  # # cex.lab=1.2
  
  # # par(mfg=c(1,1))
  # # plot(ann.mean[,1], major.ticks='years', minor.ticks=FALSE, auto.grid=FALSE, main=NULL, cex.lab=cex.lab, cex.axis=cex.axis, ylab=expression("Daily Flow " (m^3 * sec^-1)), col="black", xlab="", xaxt="n", sub="Annual Mean Streamflow") 
  # # segments(x0=(.index(ann.mean)[1]),y0=(trend.ann.mean["intercept"]),x1=(.index(ann.mean)[length(ann.mean)]),y1=(trend.ann.mean["trend"]*length(ann.mean)+(trend.ann.mean["intercept"])),col="red")
  # # legend("topleft",paste0("y=",round(trend.ann.mean["trend"],2),"x+",round(trend.ann.mean["intercept"],2)," ","sig.",round(trend.ann.mean["sig"],2)),cex=cex.lab)
  # # legend("topright", paste0("Annual Mean"),box.lwd = 0)

  # # par(mfg=c(2,1))
  # # plot(ann.min[,1], major.ticks='years', minor.ticks=FALSE, auto.grid=FALSE, main=NULL, cex.lab=cex.lab, cex.axis=cex.axis, ylab=expression("Daily Flow " (m^3 * sec^-1)), col="black", xlab="", xaxt="n", sub="Annual Minimum Daily Streamflow") 
  # # segments(x0=(.index(ann.min)[1]),y0=(trend.ann.min["intercept"]),x1=(.index(ann.min)[length(ann.min)]),y1=(trend.ann.min["trend"]*length(ann.min)+(trend.ann.min["intercept"])),col="red")
  # # legend("topleft",paste0("y=",round(trend.ann.min["trend"],2),"x+",round(trend.ann.min["intercept"],2)," ","sig.",round(trend.ann.min["sig"],2)),cex=cex.lab)
  # # legend("topright", paste0("Annual Min"),box.lwd = 0)

  # # par(mfg=c(3,1))
  # # plot(ann.max[,1], major.ticks='years', minor.ticks=FALSE, auto.grid=FALSE, main=NULL, cex.lab=cex.lab, cex.axis=cex.axis, ylab=expression("Daily Flow " (m^3 * sec^-1)), col="black", xlab="", xaxt="n", sub="Annual Maximum Daily Streamflow")
  # # segments(x0=(.index(ann.max)[1]),y0=(trend.ann.max["intercept"]),x1=(.index(ann.max)[length(ann.max)]),y1=(trend.ann.max["trend"]*length(ann.max)+(trend.ann.max["intercept"])),col="red")
  # # legend("topleft",paste0("y=",round(trend.ann.max["trend"],2),"x+",round(trend.ann.max["intercept"],2)," ","sig.",round(trend.ann.max["sig"],2)),cex=cex.lab)
  # # legend("topright", paste0("Annual Max"),box.lwd = 0)
  
  # # par(mfg=c(4,1))
  # # plot(JAS.min[,1], major.ticks='years', minor.ticks=FALSE, auto.grid=FALSE, main=NULL, cex.lab=cex.lab, cex.axis=cex.axis, ylab=expression("Daily Flow " (m^3 * sec^-1)), col="black", xlab="", xaxt="n", sub="Annual Summer Minimum Streamflow") 
  # # segments(x0=(.index(JAS.min)[1]),y0=(trend.JAS.min["intercept"]),x1=(.index(JAS.min)[length(JAS.min)]),y1=(trend.JAS.min["trend"]*length(JAS.min)+(trend.JAS.min["intercept"])),col="red")
  # # legend("topleft",paste0("y=",round(trend.JAS.min["trend"],2),"x+",round(trend.JAS.min["intercept"],2)," ","sig.",round(trend.JAS.min["sig"],2)),cex=cex.lab)
  # # legend("topright", paste0("JAS Min"),box.lwd = 0)

  # # par(mfg=c(5,1))
  # # plot(AMJ.max[,1], major.ticks='years', minor.ticks=FALSE, auto.grid=FALSE, main=NULL, cex.lab=cex.lab, cex.axis=cex.axis, ylab=expression("Daily Flow " (m^3 * sec^-1)), col="black", sub="Annual Spring Maximum Streamflow")
  # # segments(x0=(.index(AMJ.max)[1]),y0=(trend.AMJ.max["intercept"]),x1=(.index(AMJ.max)[length(AMJ.max)]),y1=(trend.AMJ.max["trend"]*length(AMJ.max)+(trend.AMJ.max["intercept"])),col="red")
  # # legend("topleft",paste0("y=",round(trend.AMJ.max["trend"],2),"x+",round(trend.AMJ.max["intercept"],2)," ","sig.",round(trend.AMJ.max["sig"],2)),cex=cex.lab)
  # # legend("topright", paste0("AMJ Max"),box.lwd = 0)
    
  # # dev.off()
}
