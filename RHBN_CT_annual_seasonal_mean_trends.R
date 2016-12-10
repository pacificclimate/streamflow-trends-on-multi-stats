# Copyright 2015 Province of British Columbia
# 
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
# 
# http://www.apache.org/licenses/LICENSE-2.0
# 
# Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and limitations under the License.

rm(list=ls())

library(xts)
library(zyp)
library(lubridate)

## Centre of Timing (1/3 and 1/2 flow) are based on January to December calendar year NOT WATER YEAR

in.dir <- '../data/'
out.dir <- '../nobackup_plots/'
meta.data <- read.csv('../data/WSC_metadata_regimes.csv',header=TRUE)
stations <- list.files(in.dir, pattern="_Daily_Flow_ts.csv")
##stations <- "08MF005_Daily_Flow_ts.csv" ## FRASER AT HOPE
clip <- '1958-01-01/2012-12-31'
start.year <- unlist(strsplit(clip, "-01-01/"))[1]
end.year <- '2012'

for(station in stations){
    print(station)	
    ##READ DATA and CONVERT TO XTS CLASS
    dat <- read.csv(paste0(in.dir, station), header=TRUE, as.is=TRUE)
    dat <- dat[,c(3,4)]
    dat$Date <- as.Date(dat$Date, format="%Y/%m/%d")
    dat$PDate <- as.POSIXlt(dat$Date)
    dat$JDate <- (dat$PDate)$yday
    dat$Year <- as.numeric(format(dat$Date,'%Y'))
    dat.na.rmd <- dat[!is.na(dat$Date),]  ## Should only be removing the last few rows.
    dat.clip <- dat.na.rmd[dat.na.rmd$Date >= paste0(start.year,"-01-01") &  dat.na.rmd$Date <= paste0(end.year,"-12-31"),]  
    dat.clip$annual.cumsum <-(unlist(tapply(dat.clip$Flow, dat.clip$Year, cumsum)))
    ann.1thrdate <- by(dat.clip, dat.clip$Year, function(x) {x[which(x$annual.cumsum >= max(x$annual.cumsum,na.rm=TRUE)/3),"JDate"][1]})
    ann.1halfdate <- by(dat.clip, dat.clip$Year, function(x) {x[which(x$annual.cumsum >= max(x$annual.cumsum,na.rm=TRUE)/2),"JDate"][1]})
    
    dat_xts <- as.xts(dat.na.rmd[,2], order.by=dat.na.rmd[,1])
    dat_xts.sub <- dat_xts[clip]
    ##dat_xts.sub <- ((dat_xts.sub-mean(dat_xts.sub,na.rm=TRUE))/mean(dat_xts.sub,na.rm=TRUE))*100  ##TO STANDARDIZE
    
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
    means <- rbind(mean(ann.1thrdate,na.rm=TRUE),mean(ann.1halfdate,na.rm=TRUE),mean(ann.mean,na.rm=TRUE),mean(ann.min,na.rm=TRUE),mean(ann.max,na.rm=TRUE),mean(JAS.min,na.rm=TRUE),mean(AMJ.max,na.rm=TRUE),mean(DJF.mean,na.rm=TRUE),mean(MAM.mean,na.rm=TRUE),mean(JJA.mean,na.rm=TRUE),mean(SON.mean,na.rm=TRUE))
    mins <- rbind(min(ann.1thrdate,na.rm=TRUE),min(ann.1halfdate,na.rm=TRUE),min(ann.mean,na.rm=TRUE),min(ann.min,na.rm=TRUE),min(ann.max,na.rm=TRUE),min(JAS.min,na.rm=TRUE),min(AMJ.max,na.rm=TRUE),min(DJF.mean,na.rm=TRUE),min(MAM.mean,na.rm=TRUE),min(JJA.mean,na.rm=TRUE),min(SON.mean,na.rm=TRUE))
    maxs <- rbind(max(ann.1thrdate,na.rm=TRUE),max(ann.1halfdate,na.rm=TRUE),max(ann.mean,na.rm=TRUE),max(ann.min,na.rm=TRUE),max(ann.max,na.rm=TRUE),max(JAS.min,na.rm=TRUE),max(AMJ.max,na.rm=TRUE),max(DJF.mean,na.rm=TRUE),max(MAM.mean,na.rm=TRUE),max(JJA.mean,na.rm=TRUE),max(SON.mean,na.rm=TRUE))
    
    print(str(ann.mean))
    
    ##COMPUTE YUE PILON TRENDS
    trend.ann.1thrdate <- zyp.yuepilon(ann.1thrdate)
    trend.ann.1halfdate <- zyp.yuepilon(ann.1halfdate)
    trend.ann.mean <- zyp.yuepilon(ann.mean)
    trend.ann.min <- zyp.yuepilon(ann.min)
    trend.ann.max <- zyp.yuepilon(ann.max)
    trend.JAS.min <- zyp.yuepilon(JAS.min)
    trend.AMJ.max <- zyp.yuepilon(AMJ.max)
    trend.DJF.mean <- zyp.yuepilon(DJF.mean)
    trend.MAM.mean <- zyp.yuepilon(MAM.mean)
    trend.JJA.mean <- zyp.yuepilon(JJA.mean)
    trend.SON.mean <- zyp.yuepilon(SON.mean)
    trends <- rbind(trend.ann.1thrdate,trend.ann.1halfdate,trend.ann.mean,trend.ann.min,trend.ann.max,trend.JAS.min,trend.AMJ.max,trend.DJF.mean,trend.MAM.mean,trend.JJA.mean,trend.SON.mean)
    
    ##DIVIDE TOTAL TREND OVER THE PERIOD BY THE INTERCEPT TO GET THE RELATIVE TREND (see Climate Overview Methods)
    ##The total trend over the period, multiplied by the length of record, was divided by the intercept to get a percentage trend relative to the intercept. This standardized the results from the different streams making them directly comparable.
    noyear <- (trends[,"trendp"]/trends[,"trend"])
    reltrend <- (trends[,"trendp"]*noyear)/trends[,"intercept"]
    perttrend <- (trends[,"trendp"])/means*100  ## Trend expressed as a percent of the mean flow for that metric
    trendsplus <- cbind(trends, noyear, reltrend, perttrend, means, mins, maxs)
    colnames(trendsplus) <- c(colnames(trends),"noyear","reltrend","perttrend","means","mins","maxs")
    
    write.csv(round(trendsplus,2), paste0(out.dir, "trend_tables/", station, "_flow_trends_", start.year, end.year, ".csv"))
    
    cex.axis=1.2
    cex.lab=1.2
    
    st.name <- strsplit(station,split="_Daily_Flow_ts.csv")[[1]]
    regime <- meta.data[which(meta.data$Station == st.name),"Regime"]
    
    ## CENTRE OF TIMING
    if(regime == "Nival"){  
        
        ##PLOTTING CENTRE OF TIMING FOR 1/2 OF ANNUAL FLOW NIVAL
        postscript(file=paste0(out.dir, "plots/","trends_centre_of_timing_onehalf_", station, "_", start.year, "_", end.year, "_", "", ".eps", sep = ""), width=8, height=4, pointsize=10)
        plot(as.numeric(names(ann.1halfdate)),as.vector(ann.1halfdate),xlab="Year",ylab="1/2 Flow Day",pch=20,col="blue",cex=2,xlim=c(1912,2013),ylim=c(160,220),yaxt='n')
        
        if(trend.ann.1halfdate["sig"]<=0.05){ 
            segments(x0=(as.numeric(names(ann.1halfdate)[1])),y0=(trend.ann.1halfdate["intercept"]),x1=(as.numeric(names(ann.1halfdate)[1])+length(as.numeric(names(ann.1halfdate)))),y1=(trend.ann.1halfdate["trend"]*length(as.numeric(names(ann.1halfdate)))+(trend.ann.1halfdate["intercept"])),col="black",cex=2)
        }
        else
        { 
            segments(x0=(as.numeric(names(ann.1halfdate)[1])),y0=(trend.ann.1halfdate["intercept"]),x1=(as.numeric(names(ann.1halfdate)[1])+length(as.numeric(names(ann.1halfdate)))),y1=(trend.ann.1halfdate["trend"]*length(as.numeric(names(ann.1halfdate)))+(trend.ann.1halfdate["intercept"])),col="grey",lty=2)
        }
        yaxis.labels <- paste(month(as.Date(c(160,180,200,220)),label=TRUE),day(as.Date(c(160,180,200,220))), sep=" ")
        axis(side=2,at=c(160,180,200,220),labels=yaxis.labels,mgp=c(3, 0.5, 0),las=1)
        legend("topleft",station,cex=cex.lab)
    }
    
    else{
        ##PLOTTING CENTRE OF TIMING FOR 1/2 OF ANNUAL FLOW PLUVIAL OR HYBRID
        postscript(file=paste0(out.dir, "plots/","trends_centre_of_timing_onehalf_", station, "_", start.year, "_", end.year, "_", "", ".eps", sep = ""), width=8, height=4, pointsize=10)
        plot(as.numeric(names(ann.1halfdate)),as.vector(ann.1halfdate),xlab="Year",ylab="1/2 Flow Day",pch=20,col="blue",cex=2,xlim=c(1912,2013),ylim=c(20,160),yaxt='n')
        
        if(trend.ann.1halfdate["sig"]<=0.05){ 
            segments(x0=(as.numeric(names(ann.1halfdate)[1])),y0=(trend.ann.1halfdate["intercept"]),x1=(as.numeric(names(ann.1halfdate)[1])+length(as.numeric(names(ann.1halfdate)))),y1=(trend.ann.1halfdate["trend"]*length(as.numeric(names(ann.1halfdate)))+(trend.ann.1halfdate["intercept"])),col="black",cex=2)
        }
        else
        { 
            segments(x0=(as.numeric(names(ann.1halfdate)[1])),y0=(trend.ann.1halfdate["intercept"]),x1=(as.numeric(names(ann.1halfdate)[1])+length(as.numeric(names(ann.1halfdate)))),y1=(trend.ann.1halfdate["trend"]*length(as.numeric(names(ann.1halfdate)))+(trend.ann.1halfdate["intercept"])),col="grey",lty=2)
        }
        yaxis.labels <- paste(month(as.Date(c(20,40,60,80,100,120,140,160)),label=TRUE),day(as.Date(c(20,40,60,80,100,120,140,160))), sep=" ")
        axis(side=2,at=c(20,40,60,80,100,120,140,160),labels=yaxis.labels,mgp=c(3, 0.5, 0),las=1)
        legend("topleft",station,cex=cex.lab)
    }
    dev.off()
    
}

# # ## 1/3rd of FLOW
# # if(regime == "Nival"){  

# # ##PLOTTING TIMING FOR 1/3rd OF ANNUAL FLOW NIVAL
# # postscript(file=paste0(out.dir, "plots/","trends_timing_onethird_", station, "_", start.year, "_", end.year, "_", "", ".eps", sep = ""), width=8, height=4, pointsize=10)
# # plot(as.numeric(names(ann.1thrdate)),as.vector(ann.1thrdate),xlab="Year",ylab="1/3 Flow Day",pch=20,col="blue",cex=2,xlim=c(1912,2013),ylim=c(120,200),yaxt='n')

# # if(trend.ann.1thrdate["sig"]<=0.05){ 
# # segments(x0=(as.numeric(names(ann.1thrdate)[1])),y0=(trend.ann.1thrdate["intercept"]),x1=(as.numeric(names(ann.1thrdate)[1])+length(as.numeric(names(ann.1thrdate)))),y1=(trend.ann.1thrdate["trend"]*length(as.numeric(names(ann.1thrdate)))+(trend.ann.1thrdate["intercept"])),col="black",cex=2)
# # }
# # else
# # { 
# # segments(x0=(as.numeric(names(ann.1thrdate)[1])),y0=(trend.ann.1thrdate["intercept"]),x1=(as.numeric(names(ann.1thrdate)[1])+length(as.numeric(names(ann.1thrdate)))),y1=(trend.ann.1thrdate["trend"]*length(as.numeric(names(ann.1thrdate)))+(trend.ann.1thrdate["intercept"])),col="grey",lty=2)
# # }
# # yaxis.labels <- paste(month(as.Date(c(120,140,160,180,200)),label=TRUE),day(as.Date(c(120,140,160,180,200))), sep=" ")
# # axis(side=2,at=c(120,140,160,180,200),labels=yaxis.labels,mgp=c(3, 0.5, 0),las=1)
# # legend("topleft",station,cex=cex.lab)
# # }

# # else{
# # ##PLOTTING TIMING FOR 1/3rd OF ANNUAL FLOW PLUVIAL OR HYBRID
# # postscript(file=paste0(out.dir, "plots/","trends_timing_onethird_", station, "_", start.year, "_", end.year, "_", "", ".eps", sep = ""), width=8, height=4, pointsize=10)
# # plot(as.numeric(names(ann.1thrdate)),as.vector(ann.1thrdate),xlab="Year",ylab="1/3 Flow Day",pch=20,col="blue",cex=2,xlim=c(1912,2013),ylim=c(20,160),yaxt='n')

# # if(trend.ann.1thrdate["sig"]<=0.05){ 
# # segments(x0=(as.numeric(names(ann.1thrdate)[1])),y0=(trend.ann.1thrdate["intercept"]),x1=(as.numeric(names(ann.1thrdate)[1])+length(as.numeric(names(ann.1thrdate)))),y1=(trend.ann.1thrdate["trend"]*length(as.numeric(names(ann.1thrdate)))+(trend.ann.1thrdate["intercept"])),col="black",cex=2)
# # }
# # else
# # { 
# # segments(x0=(as.numeric(names(ann.1thrdate)[1])),y0=(trend.ann.1thrdate["intercept"]),x1=(as.numeric(names(ann.1thrdate)[1])+length(as.numeric(names(ann.1thrdate)))),y1=(trend.ann.1thrdate["trend"]*length(as.numeric(names(ann.1thrdate)))+(trend.ann.1thrdate["intercept"])),col="grey",lty=2)
# # }
# # yaxis.labels <- paste(month(as.Date(c(20,40,60,80,100,120,140,160)),label=TRUE),day(as.Date(c(20,40,60,80,100,120,140,160))), sep=" ")
# # axis(side=2,at=c(20,40,60,80,100,120,140,160),labels=yaxis.labels,mgp=c(3, 0.5, 0),las=1)
# # legend("topleft",station,cex=cex.lab)
# # }
# # dev.off()
