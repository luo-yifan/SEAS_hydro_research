library(rjags)
library(lubridate)
setwd("D:\\Desktop\\WaterBalance\\Lake Victoria with Outflow")

# LOAD DATA
# load(file.path(processDir,"Victoria_Ver4_19861990_18k_roll12.RData"))
if(is.list(jSample)) jSample = do.call(rbind,jSample)

##################################################
#### CLOSURE - FOR 1 Year
dateAll = seq.Date(as.Date(paste(startAnalysisYear,startAnalysisMonth,"01",sep = "-")),
                   as.Date(paste(endAnalysisYear,endAnalysisMonth,"01",sep = "-")),
                   by="month")
startMo = 1
endMo = length(dateAll)
uniqueYear = unique(year(dateAll))

### 1 Month Closing Assessment
VictoriaDStore = jSample[,paste('yVictoriaDStorePP[',startMo:endMo,']', sep='')]

VictoriaDStore025 = apply(as.matrix(VictoriaDStore), 2, quantile, probs=c(0.025))
VictoriaDStore975 = apply(as.matrix(VictoriaDStore), 2, quantile, probs=c(0.975))

### 6 Month Closing Assessment
rollPeriod = 6;
yVictoriaRStore_6M = rep(NA, endMo-6)

for(rp in rollPeriod:endMo){
  yVictoriaRStore_6M[rp-5] = sum(yVictoriaDStore[(rp-rollPeriod+1):rp]);
}

VictoriaR6MStore = jSample[,paste('yVictoriaR6MStorePP[',rollPeriod:endMo,']', sep='')]
VictoriaR6MStore025 = apply(as.matrix(VictoriaR6MStore), 2, quantile, probs=c(0.025))
VictoriaR6MStore975 = apply(as.matrix(VictoriaR6MStore), 2, quantile, probs=c(0.975))

### 1 Year Closing Assessment
rollPeriod = 12;
yVictoriaRStore_1Y = rep(NA, endMo-11)

for(rp in rollPeriod:endMo){
  yVictoriaRStore_1Y[rp-11] = sum(yVictoriaDStore[(rp-rollPeriod+1):rp]);
}

VictoriaR1YStore = jSample[,paste('yVictoriaR1YStorePP[',12:endMo,']', sep='')]
VictoriaRStore025_1Y = apply(as.matrix(VictoriaR1YStore), 2, quantile, probs=c(0.025), na.rm=TRUE)
VictoriaRStore975_1Y = apply(as.matrix(VictoriaR1YStore), 2, quantile, probs=c(0.975), na.rm=TRUE)

# 5 Year Closing Assessment
rollPeriod = 120;
yVictoriaRStore_5Y = rep(NA, endMo-59)
for(rp in rollPeriod:endMo){
  yVictoriaRStore_5Y[rp-59] = sum(yVictoriaDStore[(rp-rollPeriod+1):rp]);
}

VictoriaR5YStore = jSample[,paste('yVictoriaR5YStorePP[',120:endMo,']', sep='')]
VictoriaRStore025_5Y = apply(as.matrix(VictoriaR5YStore), 2, quantile, probs=c(0.025), na.rm=TRUE)
VictoriaRStore975_5Y = apply(as.matrix(VictoriaR5YStore), 2, quantile, probs=c(0.975), na.rm=TRUE)

################
# PLOTTING
################
# pdf(file.path(figDir,"5a_WBClosure_ver2_roll12_19852000.pdf"),width=12,height=7)

layout(matrix(1:3,3,1,FALSE))
par(mar = c(0,0,0,0))
par(oma = c(4,8,4,2))
### 1 MONTH CLOSURE

nMonths = endMo
storeLimits = c(-2000,2000)
# storeLimits = range(cbind(yVictoriaDStore[1:endMo],VictoriaDStore025,VictoriaDStore975),na.rm=T)
plot(yVictoriaDStore, type = "n", col = "black", lwd = 1, xaxs="i",
     xaxt='n', yaxt='n', ylim = storeLimits,xlim=c(1,endMo))
for(i in 1:nMonths){
  segments(   
    x0 = i, x1 = i, lwd = 2, col = 'gray60', lend = 2,
    y0 = VictoriaDStore025[i],
    y1 = VictoriaDStore975[i]
  );	
}
lines(yVictoriaDStore, col = "black", lwd = 1);

axis(1, at=seq(1,nMonths,12), labels=FALSE);
axis(2, at=c(-1500,0,1500));
# axis(4, labels=FALSE, at=c(-1500,0,1500))
mtext('1', 2, line = 2.5, las=2)
mtext(paste0('w = ',RollPeriod), 3, line = 1.5)
box()

### 6 MONTHS CLOSURE
nMonths = endMo - 6
# storeLimits = range(cbind(yVictoriaRStore_6M,VictoriaR6MStore025,VictoriaR6MStore975),na.rm=T)
plot(6:endMo, yVictoriaRStore_6M, type = "n", col = "black", lwd = 1,xaxs="i",
     xaxt='n', yaxt='n', ylim = storeLimits,xlim=c(1,endMo))
for(i in 1:nMonths){
  segments(   
    x0 = 5+i, x1 = 5+i, lwd = 2, col = 'gray60', lend = 2,
    y0 = VictoriaR6MStore025[i],
    y1 = VictoriaR6MStore975[i]
  );	
}
lines(6:endMo, yVictoriaRStore_6M, col = "black", lwd = 1);
axis(4)
# axis(4, labels=FALSE, at=c(-1500,0,1500))
mtext('6', 2, line = 2.5, las=2)
mtext('Length of rolling window (w months) in model simulation', 2, line = 4.5)
mtext('Cumulative changes in storage (mm)', 2, line=6.5, outer=TRUE)
axis(1, at=seq(1,endMo,12), labels=FALSE);

### 1 YEAR CLOSURE
nMonths = endMo - 11
# storeLimits = range(cbind(yVictoriaRStore_1Y,VictoriaRStore025_1Y,VictoriaRStore975_1Y),na.rm=T)
plot(12:endMo, yVictoriaRStore_1Y, type = "n", col = "black", lwd = 1,xaxs="i",
     xaxt='n', yaxt='n', ylim = storeLimits,xlim=c(1,endMo))
for(i in 1:nMonths){
  segments(   
    x0 = 11+i, x1 = 11+i, lwd = 2, col = 'gray60', lend = 2,
    y0 = VictoriaRStore025_1Y[i],
    y1 = VictoriaRStore975_1Y[i]
  );	
}
lines(12:endMo, yVictoriaRStore_1Y, col = "black", lwd = 1);
axis(2)
# axis(4, labels=FALSE, at=c(-1500,0,1500))
mtext('12', 2, line = 2.5, las=2)
axis(1, at=seq(1,endMo,12), labels=FALSE);
axis(1, at=seq(1,endMo,12)+6, labels=startAnalysisYear:endAnalysisYear, tick=FALSE);

# dev.off()

# ### 5 YEAR CLOSURE
# INSUFFICIENT DATA - NOT RUN!
# nMonths = endMo - 59 
# storeLimits = range(cbind(yVictoriaRStore_5Y,VictoriaRStore025_5Y,VictoriaRStore975_5Y),na.rm=T)
# plot(60:endMo, yVictoriaRStore_5Y, type = "n", col = "black", lwd = 1, xaxs="i",
#      xaxt='n', yaxt='n', ylim = storeLimits,xlim=c(1,endMo))
# for(i in 1:nMonths){
#   segments(   
#     x0 = 59+i+(1/2), x1 = 59+i+(1/2), lwd = 2, col = 'gray60', lend = 2,
#     y0 = VictoriaRStore025_5Y[i],
#     y1 = VictoriaRStore975_5Y[i]
#   );	
# }
# lines(60:endMo, yVictoriaRStore_5Y, col = "black", lwd = 1);
# # axis(2, at=c(-1500,0,1500));
# axis(2)
# axis(1, at=seq(1,endMo,12), labels=FALSE);
# axis(1, at=seq(1,endMo,12), labels=startAnalysisYear:endAnalysisYear, tick=FALSE, las=2);
# # axis(4, labels=FALSE, at=c(-1500,0,1500))
# mtext('60', 2, line = 2.5, las=2)

# dev.off()

############ TIME SERIES
# pdf(file.path(figDir,'5a_newEstimate_ver2_roll12_19852000.pdf'), width = 10, height = 7.5);
par(mfrow=c(5,1))
par(mar = c(0,0,0,0))
par(oma = c(3,4,1,3))

##### PRECIPITATION
# prepare data
xpol = c(startMo:endMo,endMo:startMo)
ypol = c(jSum[paste('VictoriaPrecip[',startMo:endMo,']', sep=''),3],
         jSum[paste('VictoriaPrecip[',endMo:startMo,']', sep=''),7])
yPLimit = range(c(yVictoriaPrecip1,yVictoriaPrecip2,ypol),na.rm=T)
# plotting
plot(c(0), c(0), type = "n", col = 8, lwd = 3, axes = FALSE,
     xlim = c(startMo,endMo), ylim = yPLimit*1.1);
polygon(x = xpol,y = ypol,col="grey80",border="grey80")
lines(startMo:endMo,yVictoriaPrecip1[startMo:endMo], col = "red",  lwd = 1)
lines(startMo:endMo,yVictoriaPrecip2[startMo:endMo], col = "blue",  lwd = 1,lty=2)
#lines(startMo:endMo,yVictoriaPrecip3[startMo:endMo], col = "#33A02C",  lwd = 1,lty=2)
#axes and legends
axis(2, labels=T, cex.axis = 0.9);
axis(1, at=seq(startMo,endMo,12), labels=FALSE);
mtext(paste("Precip (mm)"), side = 2, line = 2.5, cex=0.8);
legend("topleft",legend = c("FAO","NASA","L2SWBM"),bty = "n",ncol=4,
       pch = c(NA,NA,NA, 22),pt.bg = c("blue","red","#33A02C","grey80"),
       lty=c(2,2,2,0),col=c("blue","red","#33A02C", "grey80"),pt.cex = 2,
       inset=c(0,0.05))
box()

#evap
ypol = c(jSum[paste('VictoriaEvap[',startMo:endMo,']', sep=''),3],
         jSum[paste('VictoriaEvap[',endMo:startMo,']', sep=''),7])
yELimit = range(c(yVictoriaEvap,ypol),na.rm=T)
plot(c(0), c(0), type = "n", col = 8, lwd = 3, axes = FALSE, xlim = c(startMo,endMo), ylim =  c(yELimit[1]*0.95,yELimit[2]*1.05));
polygon(x = xpol,y = ypol,col="grey80",border="grey80")
lines(startMo:endMo,yVictoriaEvap[startMo:endMo], col = "#33A02C",lwd = 1,lty=2)
axis(4, at=seq(0,yELimit[2]*1.1,50), cex.axis = 0.9);
mtext(paste("Evap (mm)"), side = 2, line = 2.5, cex=0.8);
axis(1, at=seq(startMo,endMo,12), labels=FALSE);
legend("topleft",legend = c("FAO","L2SWBM"),bty = "n",ncol=2,
       pch = c(NA, 22),pt.bg = c("#33A02C", "grey80"),
       lty=c(2,0),col=c("#33A02C", "grey80"),pt.cex=2,
       inset=c(0,0.05))
box()
#Outflow
ypol = c(jSum[paste('VictoriaOutflow[',startMo:endMo,']', sep=''),3],
         jSum[paste('VictoriaOutflow[',endMo:startMo,']', sep=''),7])
yELimit = range(c(yVictoriaOutflow,ypol),na.rm=T)
plot(c(0), c(0), type = "n", col = 8, lwd = 3, axes = FALSE, xlim = c(startMo,endMo), ylim =  c(0.0001,yELimit[2])*1.1);
polygon(x = xpol,y = ypol,col="grey80",border="grey80")
lines(startMo:endMo,yVictoriaOutflow[startMo:endMo], col = "#33A02C",lwd = 1,lty=2)
axis(4, at=seq(0,yELimit[2]*1.1,3), cex.axis = 0.9);
mtext(paste("Outflow (mm)"), side = 2, line = 2.5, cex=0.8);
axis(1, at=seq(startMo,endMo,12), labels=FALSE);
legend("topleft",legend = c("AfricaOpenData","L2SWBM"),bty = "n",ncol=2,
       pch = c(NA, 22),pt.bg = c("#blue", "grey80"),
       lty=c(2,0),col=c("blue", "grey80"),pt.cex=2,
       inset=c(0,0.05))
box()

#runoff
ypol = c(jSum[paste('VictoriaRunoff[',startMo:endMo,']', sep=''),3],
         jSum[paste('VictoriaRunoff[',endMo:startMo,']', sep=''),7])
yRLimit = range(c(yVictoriaRunoff1,ypol),na.rm=T)
plot(c(0), c(0), type = "n", col = 8, lwd = 3, axes = FALSE, xlim = c(startMo,endMo), ylim = yRLimit*1);

polygon(x = xpol,y = ypol,col="grey80",border="grey80")
lines(startMo:endMo,yVictoriaRunoff1[startMo:endMo], col = "red",  lwd = 1,lty=2)
#lines(startMo:endMo,yVictoriaRunoff2[startMo:endMo], col = "blue",  lwd = 1,lty=2)
#lines(startMo:endMo,yVictoriaRunoff3[startMo:endMo], col = "#33A02C",  lwd = 1,lty=2)

axis(2, cex.axis = 0.9);
mtext(paste("Runoff (mm)"), side = 2, line = 2.5, cex=0.8);
axis(1, at=seq(startMo,endMo,12), labels=FALSE);
legend("topleft",legend = c("Dahiti","L2SWBM"),bty = "n",ncol=4,
       pch = c(NA,NA,NA, 22),pt.bg = c("red","grey80"),
       lty=c(2,2,2,0),col=c("red","grey80"),pt.cex = 2,
       inset=c(0,0.05))
box()

#Dstore
ypol = c(jSum[paste('yVictoriaDStorePP[',startMo:endMo,']', sep=''),3],
         jSum[paste('yVictoriaDStorePP[',endMo:startMo,']', sep=''),7])
yds = c(jSum[paste('VictoriaDStore[',startMo:endMo,']', sep=''),3],
         jSum[paste('VictoriaDStore[',endMo:startMo,']', sep=''),7])
# yDSLimit = range(c(yVictoriaDStore,ypol),na.rm=T)
yDSLimit = c(-200,200)
plot(yVictoriaDStore, type = "n", col = "black", lwd = 2, axes = FALSE, ylim = c(yDSLimit[1]*1,yDSLimit[2*1.1]), xlim = c(startMo,endMo));
polygon(x = xpol,y = ypol,col="grey80",border="grey80")
polygon(x = xpol,y = yds,col="grey60",border="grey60")
lines(startMo:endMo, yVictoriaDStore[startMo:endMo], col = "blue",  lwd = 1)
# abline(h=0, col = 8)
# axis(2,cex.axis = 0.9);
axis(4,cex.axis = 0.9);
axis(1, at=seq(startMo,endMo,12), labels=FALSE);
axis(1, at=seq(startMo,endMo,12)+6, labels=startAnalysisYear:endAnalysisYear, tick=FALSE);
mtext('DStore', side = 2, line = 2.5, cex = 0.8)
legend("topleft",legend = c("Hydroweb","L2SWBM"),bty = "n",ncol=2,
       pch = c(NA, 22),pt.bg = c("blue", "grey80"),
       lty=c(1,0),col=c("blue", "grey80"),pt.cex=2,
       inset=c(0,0.05))
box()

# dev.off()

colMeans(jSample[,paste0("yOutflowBias[",1:12,"]")])
apply(jSample[,paste0("yOutflowBias[",1:12,"]")],2,sd)

colMeans(jSample[,paste0("yRunoff1Bias[",1:12,"]")])
apply(jSample[,paste0("yRunoff1Bias[",1:12,"]")],2,sd)

colMeans(jSample[,paste0("VictoriaError[",1:12,"]")])
apply(jSample[,paste0("VictoriaError[",1:12,"]")],2,sd)

mean(jSample[,"yVictoriaRStorePreci"]^-0.5)
sd(jSample[,"yVictoriaRStorePreci"]^-0.5)

mean(jSample[,"yOutflowPrecision"]^-0.5)
sd(jSample[,"yOutflowPrecision"]^-0.5)

mean(jSample[,"yRunoff1Precision"]^-0.5)
sd(jSample[,"yRunoff1Precision"]^-0.5)

#mean(jSample[,"VictoriaOutflowMultBias"])
#sd(jSample[,"VictoriaOutflowMultBias"])
