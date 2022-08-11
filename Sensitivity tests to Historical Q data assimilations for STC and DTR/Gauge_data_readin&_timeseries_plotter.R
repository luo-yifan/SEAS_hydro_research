## What does this code do?
## 1) Reads in & organizes data from gauge stations along the St Clair and Detroit Rivers
## 2) Uses gauge data in stage fall discharge (SFD) equations to calculate historical channel flows
## 3) Plot time series of historical flow datasets for St Clair and Detroit Rivers
## 4) Comparison Plots of Historical Flows for St Clair vs L2S output (flow versus time)
## 5) Comparison Plots of Historical Flows for Detroit vs L2S output (flow versus time)
## 6) Comparison Plots of Historical Flows WITH DIFFERENCE PLOTS for St Clair vs L2S output
## 7) Comparison Plots of Historical Flows WITH DIFFERENCE PLOTS for Detroit vs L2S output

## Note: to-do: add read-in and sourcing of historical coefficients from "Compiled_historicalSFD_coefficients" file

## author: Sarah Katz (skatzees@umich.edu)
###########################################################################################

## 1) Read in & organizes data from gauge stations along the St Clair and Detroit Rivers
##########
setwd("/Users/skatzees/Dropbox (University of Michigan)/Hydro L2SWBM GSRA/DataFromTim/")
Gauge = read.csv('Compiled_Gauge_Data.csv', TRUE, skip=0);
#Gauge = Gauge[841:1452,]  ## Jan 1970 to Dec 2020. Note, data typically available prior to 1970-- needs to be added to spreadsheet
                          ## Not very elegant. Would be better to have a date column that could be used to select relevant time period
Gauge = Gauge[1201:1452,]  ## Just Jan 2000- Dec 2020

AL_MSL   = Gauge[, 3]    ## Algonac
#AM_MSL   = Gauge[, 4]    ## Amhurstburg. Data not yet converted from daily to monthly format
DD_MSL   = Gauge[, 5]    ## Dry Dock
FG_MSL   = Gauge[, 6]    ## Fort Gratiot
FW_MSL   = Gauge[, 7]    ## Fort Wayne
SCSP_MSL = Gauge[, 8]    ## Saint Clair State Police
WP_MSL   = Gauge[, 9]    ## Windmill Pointe
WY_MSL   = Gauge[, 10]   ## Wyandotte

TF_DTR = read.csv('rSCR_GLERL_TF.csv', TRUE, skip=0);  ## Flows calculated for Detroit River from transfer factor (Fay & Noorbakhsh 2010 report Appendix 3)
TF = TF_DTR[1201:1452,3]

startmo=1
endmo=216  ## <- Important to change this if you change the time period being plotted!!

############
## 2) Historical Flow Calculations
#########


## St Clair River SFD Equations derived from multiple linear regression method
## After Thompson et al. 2020 Table 2 (DOI: 10.1061/(ASCE)HE.1943-5584.0001904)
MLR_FG_DD = 201.03*(DD_MSL - 167)^1.732 * (FG_MSL - DD_MSL)^0.428
MLR_FG_SCSP = 262.84*(SCSP_MSL - 167)^1.523 * (FG_MSL - SCSP_MSL)^0.554
MLR_FG_AL = 270.22*(AL_MSL - 167)^1.417 * (FG_MSL - AL_MSL)^0.635


## Detroit River SFD Equations derived from multiple linear regression method
## After Thompson et al. 2020 Table 4 (DOI: 10.1061/(ASCE)HE.1943-5584.0001904)
MLR_WP_WY = 52.12*(WP_MSL - 164)^2.139 * (WP_MSL - WY_MSL)^0.416
#MLR_WP_AM = 40.15*(WP_MSL - 164)^2.14 * (WP_MSL - AM_MSL)^0.436  ## AM data not read in yet 11/18
MLR_FW_WY = 66.54*(FW_MSL - 164)^2.245 * (FW_MSL - WY_MSL)^0.419


## St Clair River SFD Equations from Fay and Noorbakhsh report
fn_FG_DD = 458.5184*(DD_MSL - 167)^1.2932 * (FG_MSL - DD_MSL)^0.3295
fn_FG_SCSP = 511.0261*(SCSP_MSL - 167)^1.1792 * (FG_MSL - SCSP_MSL)^0.4557
fn_FG_AL = 478.3803*(AL_MSL - 167)^1.1342 * (FG_MSL - AL_MSL)^0.5187


## Detroit River SFD Equations from Fay and Noorbakhsh report

fn_WP_FW = 118.1081*(WP_MSL - 164)^1.8364 * (WP_MSL - FW_MSL)^0.3624
#fn_WP_AM = 51.3625*(WP_MSL - 164)^2.0851 * (WP_MSL - AM_MSL)^0.3698  ## AM data not read in yet 11/18
fn_FW_WY = 66.2808*(FW_MSL - 164)^2.1220 * (FW_MSL - WY_MSL)^0.2943


######
## 3) Plot time series of historical flow datasets
######
par(mfrow = c(1, 1))
#pdf(paste('historical SFD equations for comparison.pdf'), width = 10, height = 8);
par(mfrow = c(2, 1))
## Historical St Clair River flows using different stage fall discharge formulations
plot(c(0), c(0), type = "n", col = 8, lwd = 3, axes = FALSE, xlim = c(startmo,endmo), ylim = c(3000,9000), xlab="", ylab="");
box()
axis(2, cex.axis = 0.9)
axis(4, cex.axis = 0.9);
axis(1, cex.axis = 0.9, labels="FALSE");
axis(1, at=seq(1,612,12), labels=c(1970:2020))
mtext(paste("St Clair River Channel Flow (cms)"), side = 2, line = 2.5, cex=1.0);
lines(x= startmo:endmo, y=MLR_FG_DD, col="orangered", lwd = 2, lty=1)
lines(x= startmo:endmo, y=MLR_FG_SCSP, col="black", lwd = 2, lty=1)
lines(x= startmo:endmo, y=MLR_FG_AL, col="dodgerblue3", lwd = 2, lty=1)
lines(x= startmo:endmo, y=fn_FG_DD, col="yellowgreen", lwd = 2, lty=1)
lines(x= startmo:endmo, y=fn_FG_SCSP, col="gold3", lwd = 2, lty=1)
lines(x= startmo:endmo, y=fn_FG_AL, col="purple", lwd = 2, lty=1)
legend(x=400,y=9000,lty=1, lwd =2, bty="n",legend = c("MLR_FG_DD","MLR_FG_SCSP", "MLR_FG_AL", "fn_FG_DD", "fn_FG_SCSP", "fn_FG_AL"),ncol=1,cex=0.8,col = c("orangered", "black", "dodgerblue3", "yellowgreen", "gold3", "purple"))


## Historical Detroit River flows using different stage fall discharge formulations
#par(mfrow = c(1, 1))
plot(c(0), c(0), type = "n", col = 8, lwd = 3, axes = FALSE, xlim = c(startmo,endmo), ylim = c(3000,9000), xlab="", ylab="");
box()
axis(2, cex.axis = 0.9)
axis(4, cex.axis = 0.9);
axis(1, at=seq(1,612,12), labels=c(1970:2020))
mtext(paste("Detroit River Channel Flow (cms)"), side = 2, line = 2.5, cex=1.0);
lines(x= startmo:endmo, y=fn_WP_FW, col="orangered", lwd = 2, lty=1)
lines(x= startmo:endmo, y=fn_FW_WY, col="black", lwd = 2, lty=1)
lines(x= startmo:endmo, y=MLR_FW_WY, col="dodgerblue3", lwd = 2, lty=1)
lines(x= startmo:endmo, y=MLR_WP_WY, col="yellowgreen", lwd = 2, lty=1)
legend(x=400,y=9000,lty=1, lwd =2, bty="n",legend = c("fn_WP_FW","fn_FW_WY", "MLR_FW_W", "MLR_WP_WY"),ncol=1,cex=0.8,col = c("orangered", "black", "dodgerblue3", "yellowgreen"))

dev.off()

##########

## Read in products from L2S runs
wd = setwd("/Users/skatzees/Dropbox (University of Michigan)/Hydro L2SWBM GSRA/WaterBalance/SFD_Indep_Q_Est/")
#load(paste(outputname,startAnalysisYear,endAnalysisYear,'_',iters/1000,'k_roll1.RData',sep=''))
load("Dec8_v2_SFD_StClair&Detroit_400000_PrecOnQis100_20002017_400k_roll1.RData")    ## <- Loading the RData workspace brings in all the data from the saved runs. More detail than the summary stats csv file.

L2S_run = paste(wd,"/", "Dec8_v2_SFD_StClair&Detroit_400000_PrecOnQis100_3k_stats.csv", sep="") ## <- Find and read in the summary csv file to easily pull 2.5-9.75 % CI
L2S_run = read.csv(L2S_run)
L2S_MHOutflow = L2S_run[3061:3276, ]      ## <- inelegant, but this reads in the St Clair River data
L2S_STCOutflow = L2S_run[1969:2184, ]      ## <- inelegant, but this reads in the Detroit River data


#### 4) Comparison Plots of Historical Flows for St Clair vs L2S output (flow versus time)
######
# PLOT 1 MLR_FG_DD
# PLOT 2 MLR_FG_SCSP
# PLOT 3 MLR_FG_AL
# PLOT 4 fn_FG_DD
# PLOT 5 fn_FG_SCSP
# PLOT 6 fn_FG_AL


#pdf(paste('Historical Q Comparisons Nov18v2.pdf'), width = 12, height = 4);
pdf(paste('Historical Q Comparisons StClair Dec08v2_400k_PrecOnQis100.pdf'), width = 10, height = 10);
par(mfrow = c(6, 1))

par(mar = c(0,2,0,2))
par(oma = c(4,4,4,4))

## PLOT 1 : MLR_FG_DD
plot(c(0), c(0), type = "n", col = 8, lwd = 3, axes = FALSE, xlim = c(1,252), ylim = c(3000,9000), xlab="", ylab="");
box()
axis(2, cex.axis = 0.9)
axis(4, cex.axis = 0.9, labels="FALSE");
axis(1, cex.axis = 0.9, labels="FALSE");
#axis(1, at=seq(1,252,12), labels=c(2000:2020))
mtext(paste("St Clair River Channel Flow (cms)"), side = 2, line = 2.5, cex=1.0);
rect(xleft=c(1:216)+0.2, xright=c(1:216)+0.8, ybottom = L2S_MHOutflow$X2.50., ytop =L2S_MHOutflow$X97.50., col="grey80", border =NA)
lines(x=1:216, y=L2S_MHOutflow$X50., col="red", lwd = 2, lty=1)
lines(x=1:252, y=MLR_FG_DD, col="black", lwd = 2, lty=1)
legend(x=0,y=9000,lty=c(1), lwd =2, bty="n",legend = c("L2SWBM Median","MLR_FG_DD"),ncol=1,cex=1.3,col = c("red", "black"))
legend(x=75, y=9000, fill="grey80",bty="n",legend = c("L2SWBM (95% CI)"),ncol=3,cex=1.3,border="grey80")
text(x=200, y=8500, "Plot 1: MLR_FG_DD", srt=0, cex=1.8)

## PLOT 2 : MLR_FG_SCSP
plot(c(0), c(0), type = "n", col = 8, lwd = 3, axes = FALSE, xlim = c(1,252), ylim = c(3000,9000), xlab="", ylab="");
box()
axis(2, cex.axis = 0.9, labels="FALSE")
axis(4, cex.axis = 0.9);
axis(1, cex.axis = 0.9, labels="FALSE");
#axis(1, at=seq(1,252,12), labels=c(2000:2020))
mtext(paste("St Clair River Channel Flow (cms)"), side = 4, line = 2.5, cex=1.0);
rect(xleft=c(1:216)+0.2, xright=c(1:216)+0.8, ybottom = L2S_MHOutflow$X2.50., ytop =L2S_MHOutflow$X97.50., col="grey80", border =NA)
lines(x=1:216, y=L2S_MHOutflow$X50., col="red", lwd = 2, lty=1)
lines(x=1:252, y=MLR_FG_SCSP, col="black", lwd = 2, lty=1)
legend(x=0,y=9000,lty=c(1), lwd =2, bty="n",legend = c("L2SWBM Median","MLR_FG_SCSP"),ncol=1,cex=1.3,col = c("red", "black"))
legend(x=75, y=9000, fill="grey80",bty="n",legend = c("L2SWBM (95% CI)"),ncol=3,cex=1.3,border="grey80")
text(x=200, y=8500, "Plot 2: MLR_FG_SCSP", srt=0, cex=1.8)

## PLOT 3 : MLR_FG_AL
plot(c(0), c(0), type = "n", col = 8, lwd = 3, axes = FALSE, xlim = c(1,252), ylim = c(3000,9000), xlab="", ylab="");
box()
axis(2, cex.axis = 0.9)
axis(4, cex.axis = 0.9, labels="FALSE");
axis(1, cex.axis = 0.9, labels="FALSE");
#axis(1, at=seq(1,252,12), labels=c(2000:2020))
mtext(paste("St Clair River Channel Flow (cms)"), side = 2, line = 2.5, cex=1.0);
rect(xleft=c(1:216)+0.2, xright=c(1:216)+0.8, ybottom = L2S_MHOutflow$X2.50., ytop =L2S_MHOutflow$X97.50., col="grey80", border =NA)
lines(x=1:216, y=L2S_MHOutflow$X50., col="red", lwd = 2, lty=1)
lines(x=1:252, y=MLR_FG_AL, col="black", lwd = 2, lty=1)
legend(x=0,y=9000,lty=c(1), lwd =2, bty="n",legend = c("L2SWBM Median","MLR_FG_AL"),ncol=1,cex=1.3,col = c("red", "black"))
legend(x=75, y=9000, fill="grey80",bty="n",legend = c("L2SWBM (95% CI)"),ncol=3,cex=1.3,border="grey80")
text(x=200, y=8500, "Plot 3: MLR_FG_AL", srt=0, cex=1.8)

## PLOT 4 : fn_FG_DD
plot(c(0), c(0), type = "n", col = 8, lwd = 3, axes = FALSE, xlim = c(1,252), ylim = c(3000,9000), xlab="", ylab="");
box()
axis(2, cex.axis = 0.9, labels="FALSE")
axis(4, cex.axis = 0.9);
axis(1, cex.axis = 0.9, labels="FALSE");
#axis(1, at=seq(1,252,12), labels=c(2000:2020))
mtext(paste("St Clair River Channel Flow (cms)"), side = 4, line = 2.5, cex=1.0);
rect(xleft=c(1:216)+0.2, xright=c(1:216)+0.8, ybottom = L2S_MHOutflow$X2.50., ytop =L2S_MHOutflow$X97.50., col="grey80", border =NA)
lines(x=1:216, y=L2S_MHOutflow$X50., col="red", lwd = 2, lty=1)
lines(x=1:252, y=fn_FG_DD, col="black", lwd = 2, lty=1)
legend(x=0,y=9000,lty=c(1), lwd =2, bty="n",legend = c("L2SWBM Median","fn_FG_DD"),ncol=1,cex=1.3,col = c("red", "black"))
legend(x=75, y=9000, fill="grey80",bty="n",legend = c("L2SWBM (95% CI)"),ncol=3,cex=1.3,border="grey80")
text(x=200, y=8500, "Plot 4: fn_FG_DD", srt=0, cex=1.8)

## PLOT 5 : fn_FG_SCSP
plot(c(0), c(0), type = "n", col = 8, lwd = 3, axes = FALSE, xlim = c(1,252), ylim = c(3000,9000), xlab="", ylab="");
box()
axis(2, cex.axis = 0.9)
axis(4, cex.axis = 0.9, labels="FALSE");
axis(1, cex.axis = 0.9, labels="FALSE");
#axis(1, at=seq(1,252,12), labels=c(2000:2020))
mtext(paste("St Clair River Channel Flow (cms)"), side = 2, line = 2.5, cex=1.0);
rect(xleft=c(1:216)+0.2, xright=c(1:216)+0.8, ybottom = L2S_MHOutflow$X2.50., ytop =L2S_MHOutflow$X97.50., col="grey80", border =NA)
lines(x=1:216, y=L2S_MHOutflow$X50., col="red", lwd = 2, lty=1)
lines(x=1:252, y=fn_FG_SCSP, col="black", lwd = 2, lty=1)
legend(x=0,y=9000,lty=c(1), lwd =2, bty="n",legend = c("L2SWBM Median","fn_FG_SCSP"),ncol=1,cex=1.3,col = c("red", "black"))
legend(x=75, y=9000, fill="grey80",bty="n",legend = c("L2SWBM (95% CI)"),ncol=3,cex=1.3,border="grey80")
text(x=200, y=8500, "Plot 5: fn_FG_SCSP", srt=0, cex=1.8)

## PLOT 6 : fn_FG_AL
plot(c(0), c(0), type = "n", col = 8, lwd = 3, axes = FALSE, xlim = c(1,252), ylim = c(3000,9000), xlab="", ylab="");
box()
axis(2, cex.axis = 0.9, labels="FALSE")
axis(4, cex.axis = 0.9);
axis(1, cex.axis = 0.9, labels="FALSE");
axis(1, at=seq(1,252,12), labels=c(2000:2020))
mtext(paste("St Clair River Channel Flow (cms)"), side = 4, line = 2.5, cex=1.0);
rect(xleft=c(1:216)+0.2, xright=c(1:216)+0.8, ybottom = L2S_MHOutflow$X2.50., ytop =L2S_MHOutflow$X97.50., col="grey80", border =NA)
lines(x=1:216, y=L2S_MHOutflow$X50., col="red", lwd = 2, lty=1)
lines(x=1:252, y=fn_FG_AL, col="black", lwd = 2, lty=1)
legend(x=0,y=9000,lty=c(1), lwd =2, bty="n",legend = c("L2SWBM Median","fn_FG_AL"),ncol=1,cex=1.3,col = c("red", "black"))
legend(x=75, y=9000, fill="grey80",bty="n",legend = c("L2SWBM (95% CI)"),ncol=3,cex=1.3,border="grey80")
text(x=200, y=8500, "Plot 6: fn_FG_AL", srt=0, cex=1.8)

dev.off()

#####
#### 5) Comparison Plots of Historical Flows for Detroit vs L2S output (flow versus time)
######
# PLOT 1 fn_WP_FW
# PLOT 2 fn_FW_WY
# PLOT 3 MLR_FW_WY
# PLOT 4 fn_WP_FW
# PLOT 5 MLR_WP_AM <- can not plot until AM monthly data is updated
# PLOT 6 fn_WP_AM  <- can not plot until AM monthly data is updated
# Plot 7 r


#pdf(paste('Historical Q Comparisons Nov18v2.pdf'), width = 12, height = 4);
pdf(paste('Historical Q Comparisons Detroit Dec08v2_400k_PrecOnQis100.pdf'), width = 10, height = 10);
par(mfrow = c(6, 1))

par(mar = c(0,2,0,2))
par(oma = c(4,4,4,4))

## PLOT 1 : fn_WP_FW
plot(c(0), c(0), type = "n", col = 8, lwd = 3, axes = FALSE, xlim = c(1,252), ylim = c(3000,9000), xlab="", ylab="");
box()
axis(2, cex.axis = 0.9)
axis(4, cex.axis = 0.9, labels="FALSE");
axis(1, cex.axis = 0.9, labels="FALSE");
#axis(1, at=seq(1,252,12), labels=c(2000:2020))
mtext(paste("Detroit River Channel Flow (cms)"), side = 2, line = 2.5, cex=1.0);
rect(xleft=c(1:216)+0.2, xright=c(1:216)+0.8, ybottom = L2S_STCOutflow$X2.50., ytop =L2S_STCOutflow$X97.50., col="grey80", border =NA)
lines(x=1:216, y=L2S_STCOutflow$X50., col="red", lwd = 2, lty=1)
lines(x=1:252, y=fn_WP_FW, col="black", lwd = 2, lty=1)
legend(x=0,y=9000,lty=c(1), lwd =2, bty="n",legend = c("L2SWBM Median","fn_WP_FW"),ncol=1,cex=1.3,col = c("red", "black"))
legend(x=75, y=9000, fill="grey80",bty="n",legend = c("L2SWBM (95% CI)"),ncol=3,cex=1.3,border="grey80")
text(x=200, y=8500, "fn_WP_FW", srt=0, cex=1.8)

## PLOT 2 : fn_FW_WY
plot(c(0), c(0), type = "n", col = 8, lwd = 3, axes = FALSE, xlim = c(1,252), ylim = c(3000,9000), xlab="", ylab="");
box()
axis(2, cex.axis = 0.9, labels="FALSE")
axis(4, cex.axis = 0.9);
axis(1, cex.axis = 0.9, labels="FALSE");
#axis(1, at=seq(1,252,12), labels=c(2000:2020))
mtext(paste("Detroit River Channel Flow (cms)"), side = 4, line = 2.5, cex=1.0);
rect(xleft=c(1:216)+0.2, xright=c(1:216)+0.8, ybottom = L2S_STCOutflow$X2.50., ytop =L2S_STCOutflow$X97.50., col="grey80", border =NA)
lines(x=1:216, y=L2S_STCOutflow$X50., col="red", lwd = 2, lty=1)
lines(x=1:252, y=fn_FW_WY, col="black", lwd = 2, lty=1)
legend(x=0,y=9000,lty=c(1), lwd =2, bty="n",legend = c("L2SWBM Median","fn_FW_WY"),ncol=1,cex=1.3,col = c("red", "black"))
legend(x=75, y=9000, fill="grey80",bty="n",legend = c("L2SWBM (95% CI)"),ncol=3,cex=1.3,border="grey80")
text(x=200, y=8500, "fn_FW_WY", srt=0, cex=1.8)

## PLOT 3 : MLR_FW_WY
plot(c(0), c(0), type = "n", col = 8, lwd = 3, axes = FALSE, xlim = c(1,252), ylim = c(3000,9000), xlab="", ylab="");
box()
axis(2, cex.axis = 0.9)
axis(4, cex.axis = 0.9, labels="FALSE");
axis(1, cex.axis = 0.9, labels="FALSE");
#axis(1, at=seq(1,252,12), labels=c(2000:2020))
mtext(paste("Detroit River Channel Flow (cms)"), side = 2, line = 2.5, cex=1.0);
rect(xleft=c(1:216)+0.2, xright=c(1:216)+0.8, ybottom = L2S_STCOutflow$X2.50., ytop =L2S_STCOutflow$X97.50., col="grey80", border =NA)
lines(x=1:216, y=L2S_STCOutflow$X50., col="red", lwd = 2, lty=1)
lines(x=1:252, y=MLR_FW_WY, col="black", lwd = 2, lty=1)
legend(x=0,y=9000,lty=c(1), lwd =2, bty="n",legend = c("L2SWBM Median","MLR_FW_WY"),ncol=1,cex=1.3,col = c("red", "black"))
legend(x=75, y=9000, fill="grey80",bty="n",legend = c("L2SWBM (95% CI)"),ncol=3,cex=1.3,border="grey80")
text(x=200, y=8500, "MLR_FW_WY", srt=0, cex=1.8)

## PLOT 4 : fn_WP_FW
plot(c(0), c(0), type = "n", col = 8, lwd = 3, axes = FALSE, xlim = c(1,252), ylim = c(3000,9000), xlab="", ylab="");
box()
axis(2, cex.axis = 0.9, labels="FALSE")
axis(4, cex.axis = 0.9);
axis(1, cex.axis = 0.9, labels="FALSE");
#axis(1, at=seq(1,252,12), labels=c(2000:2020))  ## <- comment out once plots 5 & 6 are ready
mtext(paste("Detroit River Channel Flow (cms)"), side = 4, line = 2.5, cex=1.0);
rect(xleft=c(1:216)+0.2, xright=c(1:216)+0.8, ybottom = L2S_STCOutflow$X2.50., ytop =L2S_STCOutflow$X97.50., col="grey80", border =NA)
lines(x=1:216, y=L2S_STCOutflow$X50., col="red", lwd = 2, lty=1)
lines(x=1:252, y=fn_WP_FW, col="black", lwd = 2, lty=1)
legend(x=0,y=9000,lty=c(1), lwd =2, bty="n",legend = c("L2SWBM Median","fn_WP_FW"),ncol=1,cex=1.3,col = c("red", "black"))
legend(x=75, y=9000, fill="grey80",bty="n",legend = c("L2SWBM (95% CI)"),ncol=3,cex=1.3,border="grey80")
text(x=200, y=8500, "fn_WP_FW", srt=0, cex=1.8)

## PLOT 5 : MLR_WP_AM <- can not plot until AM monthly data is updated
# plot(c(0), c(0), type = "n", col = 8, lwd = 3, axes = FALSE, xlim = c(1,252), ylim = c(3000,9000), xlab="", ylab="");
# box()
# axis(2, cex.axis = 0.9)
# axis(4, cex.axis = 0.9, labels="FALSE");
# axis(1, cex.axis = 0.9, labels="FALSE");
# #axis(1, at=seq(1,252,12), labels=c(2000:2020))
# mtext(paste("Detroit River Channel Flow (cms)"), side = 2, line = 2.5, cex=1.0);
# rect(xleft=c(1:216)+0.2, xright=c(1:216)+0.8, ybottom = L2S_STCOutflow$X2.50., ytop =L2S_STCOutflow$X97.50., col="grey80", border =NA)
# lines(x=1:216, y=L2S_STCOutflow$X50., col="red", lwd = 2, lty=1)
# lines(x=1:252, y=MLR_WP_AM, col="black", lwd = 2, lty=1)
# legend(x=0,y=9000,lty=c(1), lwd =2, bty="n",legend = c("L2SWBM Median","MLR_WP_AM"),ncol=1,cex=1.3,col = c("red", "black"))
# legend(x=75, y=9000, fill="grey80",bty="n",legend = c("L2SWBM (95% CI)"),ncol=3,cex=1.3,border="grey80")
# text(x=200, y=8500, "MLR_WP_AM", srt=0, cex=1.8)

## PLOT 6 : fn_WP_AM  <- can not plot until AM monthly data is updated
# plot(c(0), c(0), type = "n", col = 8, lwd = 3, axes = FALSE, xlim = c(1,252), ylim = c(3000,9000), xlab="", ylab="");
# box()
# axis(2, cex.axis = 0.9, labels="FALSE")
# axis(4, cex.axis = 0.9);
# axis(1, cex.axis = 0.9, labels="FALSE");
# axis(1, at=seq(1,252,12), labels=c(2000:2020))
# mtext(paste("Detroit River Channel Flow (cms)"), side = 4, line = 2.5, cex=1.0);
# rect(xleft=c(1:216)+0.2, xright=c(1:216)+0.8, ybottom = L2S_STCOutflow$X2.50., ytop =L2S_STCOutflow$X97.50., col="grey80", border =NA)
# lines(x=1:216, y=L2S_STCOutflow$X50., col="red", lwd = 2, lty=1)
# lines(x=1:252, y=fn_WP_AM, col="black", lwd = 2, lty=1)
# legend(x=0,y=9000,lty=c(1), lwd =2, bty="n",legend = c("L2SWBM Median","fn_WP_AM"),ncol=1,cex=1.3,col = c("red", "black"))
# legend(x=75, y=9000, fill="grey80",bty="n",legend = c("L2SWBM (95% CI)"),ncol=3,cex=1.3,border="grey80")
# text(x=200, y=8500, "fn_WP_AM", srt=0, cex=1.8)

## PLOT 7 : TF (transfer factor)
plot(c(0), c(0), type = "n", col = 8, lwd = 3, axes = FALSE, xlim = c(1,252), ylim = c(3000,9000), xlab="", ylab="");
box()
axis(2, cex.axis = 0.9)
axis(4, cex.axis = 0.9, labels="FALSE");
axis(1, cex.axis = 0.9, labels="FALSE");
axis(1, at=seq(1,252,12), labels=c(2000:2020))
mtext(paste("Detroit River Channel Flow (cms)"), side = 2, line = 2.5, cex=1.0);
rect(xleft=c(1:216)+0.2, xright=c(1:216)+0.8, ybottom = L2S_STCOutflow$X2.50., ytop =L2S_STCOutflow$X97.50., col="grey80", border =NA)
lines(x=1:216, y=L2S_STCOutflow$X50., col="red", lwd = 2, lty=1)
lines(x=1:252, y=TF, col="black", lwd = 2, lty=1)
legend(x=0,y=9000,lty=c(1), lwd =2, bty="n",legend = c("L2SWBM Median","TF"),ncol=1,cex=1.3,col = c("red", "black"))
legend(x=75, y=9000, fill="grey80",bty="n",legend = c("L2SWBM (95% CI)"),ncol=3,cex=1.3,border="grey80")
text(x=200, y=8500, "Transfer factor", srt=0, cex=1.8)

dev.off()

 
######### 
### 6) Comparison Plots of Historical Flows WITH DIFFERENCE PLOTS for St Clair vs L2S output
#######
dev.off()
pdf(paste('Historical Q Comparisons w DiffPlots StClair Dec08v2_400k_PrecOnQis100.pdf'), width = 20, height = 10);
par(mfrow = c(6, 2))

par(mar = c(0,2,0,6))
par(oma = c(4,4,4,4))

## PLOT 1 : MLR_FG_DD
plot(c(0), c(0), type = "n", col = 8, lwd = 3, axes = FALSE, xlim = c(1,252), ylim = c(3000,9000), xlab="", ylab="");
box()
axis(2, cex.axis = 0.9)
axis(4, cex.axis = 0.9, labels="FALSE");
axis(1, cex.axis = 0.9, labels="FALSE");
#axis(1, at=seq(1,252,12), labels=c(2000:2020))
mtext(paste("St Clair River Channel Flow (cms)"), side = 2, line = 2.5, cex=1.0);
rect(xleft=c(1:216)+0.2, xright=c(1:216)+0.8, ybottom = L2S_MHOutflow$X2.50., ytop =L2S_MHOutflow$X97.50., col="grey80", border =NA)
lines(x=1:216, y=L2S_MHOutflow$X50., col="red", lwd = 2, lty=1)
lines(x=1:252, y=MLR_FG_DD, col="black", lwd = 2, lty=1)
legend(x=0,y=9000,lty=c(1), lwd =2, bty="n",legend = c("L2SWBM Median","MLR_FG_DD"),ncol=1,cex=1.3,col = c("red", "black"))
legend(x=75, y=9000, fill="grey80",bty="n",legend = c("L2SWBM (95% CI)"),ncol=3,cex=1.3,border="grey80")
text(x=200, y=8500, "Plot 1: MLR_FG_DD", srt=0, cex=1.8)

## PLOT 1B: MLR_FG_DD difference
plot(c(0), c(0), type = "n", col = 8, lwd = 3, axes = FALSE, xlim = c(1,252), ylim = c(-1000,1000), xlab="", ylab="");
box()
axis(2, cex.axis = 0.9)
#axis(4, cex.axis = 0.9, labels="FALSE");
#axis(1, cex.axis = 0.9, labels="FALSE");
#axis(1, at=seq(1,252,12), labels=c(2000:2020))
rect(xleft=c(1:216)+0.1, xright=c(1:216)+0.9, ybottom = 0, ytop =L2S_MHOutflow$X50.-MLR_FG_DD[1:216], col="grey80", border ="grey70")
text(x=220, y=900, "MLR_FG_DD", srt=0, cex=1.8)
abline(h=0, col="red")

## PLOT 2 : MLR_FG_SCSP
plot(c(0), c(0), type = "n", col = 8, lwd = 3, axes = FALSE, xlim = c(1,252), ylim = c(3000,9000), xlab="", ylab="");
box()
axis(2, cex.axis = 0.9, labels="FALSE")
axis(4, cex.axis = 0.9);
axis(1, cex.axis = 0.9, labels="FALSE");
#axis(1, at=seq(1,252,12), labels=c(2000:2020))
mtext(paste("St Clair River Channel Flow (cms)"), side = 4, line = 2.5, cex=1.0);
rect(xleft=c(1:216)+0.2, xright=c(1:216)+0.8, ybottom = L2S_MHOutflow$X2.50., ytop =L2S_MHOutflow$X97.50., col="grey80", border =NA)
lines(x=1:216, y=L2S_MHOutflow$X50., col="red", lwd = 2, lty=1)
lines(x=1:252, y=MLR_FG_SCSP, col="black", lwd = 2, lty=1)
legend(x=0,y=9000,lty=c(1), lwd =2, bty="n",legend = c("L2SWBM Median","MLR_FG_SCSP"),ncol=1,cex=1.3,col = c("red", "black"))
legend(x=75, y=9000, fill="grey80",bty="n",legend = c("L2SWBM (95% CI)"),ncol=3,cex=1.3,border="grey80")
text(x=200, y=8500, "Plot 2: MLR_FG_SCSP", srt=0, cex=1.8)

## PLOT 2B: MLR_FG_SCSP difference
plot(c(0), c(0), type = "n", col = 8, lwd = 3, axes = FALSE, xlim = c(1,252), ylim = c(-1000,1000), xlab="", ylab="");
box()
axis(4, cex.axis = 0.9)
# axis(4, cex.axis = 0.9, labels="FALSE");
# axis(1, cex.axis = 0.9, labels="FALSE");
#axis(1, at=seq(1,252,12), labels=c(2000:2020))
rect(xleft=c(1:216)+0.1, xright=c(1:216)+0.9, ybottom = 0, ytop =L2S_MHOutflow$X50.-MLR_FG_SCSP[1:216], col="grey80", border ="grey70")
text(x=220, y=900, "MLR_FG_SCSP", srt=0, cex=1.8)
abline(h=0, col="red")

## PLOT 3 : MLR_FG_AL
plot(c(0), c(0), type = "n", col = 8, lwd = 3, axes = FALSE, xlim = c(1,252), ylim = c(3000,9000), xlab="", ylab="");
box()
axis(2, cex.axis = 0.9)
axis(4, cex.axis = 0.9, labels="FALSE");
axis(1, cex.axis = 0.9, labels="FALSE");
#axis(1, at=seq(1,252,12), labels=c(2000:2020))
mtext(paste("St Clair River Channel Flow (cms)"), side = 2, line = 2.5, cex=1.0);
rect(xleft=c(1:216)+0.2, xright=c(1:216)+0.8, ybottom = L2S_MHOutflow$X2.50., ytop =L2S_MHOutflow$X97.50., col="grey80", border =NA)
lines(x=1:216, y=L2S_MHOutflow$X50., col="red", lwd = 2, lty=1)
lines(x=1:252, y=MLR_FG_AL, col="black", lwd = 2, lty=1)
legend(x=0,y=9000,lty=c(1), lwd =2, bty="n",legend = c("L2SWBM Median","MLR_FG_AL"),ncol=1,cex=1.3,col = c("red", "black"))
legend(x=75, y=9000, fill="grey80",bty="n",legend = c("L2SWBM (95% CI)"),ncol=3,cex=1.3,border="grey80")
text(x=200, y=8500, "Plot 3: MLR_FG_AL", srt=0, cex=1.8)

## PLOT 3B: MLR_FG_AL difference
plot(c(0), c(0), type = "n", col = 8, lwd = 3, axes = FALSE, xlim = c(1,252), ylim = c(-1000,1000), xlab="", ylab="");
box()
axis(2, cex.axis = 0.9)
# axis(4, cex.axis = 0.9, labels="FALSE");
# axis(1, cex.axis = 0.9, labels="FALSE");
#axis(1, at=seq(1,252,12), labels=c(2000:2020))
rect(xleft=c(1:216)+0.1, xright=c(1:216)+0.9, ybottom = 0, ytop =L2S_MHOutflow$X50.-MLR_FG_AL[1:216], col="grey80", border ="grey70")
text(x=220, y=900, "MLR_FG_AL", srt=0, cex=1.8)
abline(h=0, col="red")

## PLOT 4 : fn_FG_DD
plot(c(0), c(0), type = "n", col = 8, lwd = 3, axes = FALSE, xlim = c(1,252), ylim = c(3000,9000), xlab="", ylab="");
box()
axis(2, cex.axis = 0.9, labels="FALSE")
axis(4, cex.axis = 0.9);
axis(1, cex.axis = 0.9, labels="FALSE");
#axis(1, at=seq(1,252,12), labels=c(2000:2020))
mtext(paste("St Clair River Channel Flow (cms)"), side = 4, line = 2.5, cex=1.0);
rect(xleft=c(1:216)+0.2, xright=c(1:216)+0.8, ybottom = L2S_MHOutflow$X2.50., ytop =L2S_MHOutflow$X97.50., col="grey80", border =NA)
lines(x=1:216, y=L2S_MHOutflow$X50., col="red", lwd = 2, lty=1)
lines(x=1:252, y=fn_FG_DD, col="black", lwd = 2, lty=1)
legend(x=0,y=9000,lty=c(1), lwd =2, bty="n",legend = c("L2SWBM Median","fn_FG_DD"),ncol=1,cex=1.3,col = c("red", "black"))
legend(x=75, y=9000, fill="grey80",bty="n",legend = c("L2SWBM (95% CI)"),ncol=3,cex=1.3,border="grey80")
text(x=200, y=8500, "Plot 4: fn_FG_DD", srt=0, cex=1.8)

## PLOT 4B: fn_FG_DD difference
plot(c(0), c(0), type = "n", col = 8, lwd = 3, axes = FALSE, xlim = c(1,252), ylim = c(-1000,1000), xlab="", ylab="");
box()
axis(4, cex.axis = 0.9)
# axis(4, cex.axis = 0.9, labels="FALSE");
# axis(1, cex.axis = 0.9, labels="FALSE");
#axis(1, at=seq(1,252,12), labels=c(2000:2020))
rect(xleft=c(1:216)+0.1, xright=c(1:216)+0.9, ybottom = 0, ytop =L2S_MHOutflow$X50.-fn_FG_DD[1:216], col="grey80", border ="grey70")
text(x=220, y=900, "fn_FG_DD", srt=0, cex=1.8)
abline(h=0, col="red")

## PLOT 5 : fn_FG_SCSP
plot(c(0), c(0), type = "n", col = 8, lwd = 3, axes = FALSE, xlim = c(1,252), ylim = c(3000,9000), xlab="", ylab="");
box()
axis(2, cex.axis = 0.9)
axis(4, cex.axis = 0.9, labels="FALSE");
axis(1, cex.axis = 0.9, labels="FALSE");
#axis(1, at=seq(1,252,12), labels=c(2000:2020))
mtext(paste("St Clair River Channel Flow (cms)"), side = 2, line = 2.5, cex=1.0);
rect(xleft=c(1:216)+0.2, xright=c(1:216)+0.8, ybottom = L2S_MHOutflow$X2.50., ytop =L2S_MHOutflow$X97.50., col="grey80", border =NA)
lines(x=1:216, y=L2S_MHOutflow$X50., col="red", lwd = 2, lty=1)
lines(x=1:252, y=fn_FG_SCSP, col="black", lwd = 2, lty=1)
legend(x=0,y=9000,lty=c(1), lwd =2, bty="n",legend = c("L2SWBM Median","fn_FG_SCSP"),ncol=1,cex=1.3,col = c("red", "black"))
legend(x=75, y=9000, fill="grey80",bty="n",legend = c("L2SWBM (95% CI)"),ncol=3,cex=1.3,border="grey80")
text(x=200, y=8500, "Plot 5: fn_FG_SCSP", srt=0, cex=1.8)

## PLOT 5B: fn_FG_SCSP difference
plot(c(0), c(0), type = "n", col = 8, lwd = 3, axes = FALSE, xlim = c(1,252), ylim = c(-1000,1000), xlab="", ylab="");
box()
axis(2, cex.axis = 0.9)
# axis(4, cex.axis = 0.9, labels="FALSE");
# axis(1, cex.axis = 0.9, labels="FALSE");
#axis(1, at=seq(1,252,12), labels=c(2000:2020))
rect(xleft=c(1:216)+0.1, xright=c(1:216)+0.9, ybottom = 0, ytop =L2S_MHOutflow$X50.-fn_FG_SCSP[1:216], col="grey80", border ="grey70")
text(x=220, y=900, "fn_FG_SCSP", srt=0, cex=1.8)
abline(h=0, col="red")

## PLOT 6 : fn_FG_AL
plot(c(0), c(0), type = "n", col = 8, lwd = 3, axes = FALSE, xlim = c(1,252), ylim = c(3000,9000), xlab="", ylab="");
box()
axis(2, cex.axis = 0.9, labels="FALSE")
axis(4, cex.axis = 0.9);
axis(1, cex.axis = 0.9, labels="FALSE");
axis(1, at=seq(1,252,12), labels=c(2000:2020))
mtext(paste("St Clair River Channel Flow (cms)"), side = 4, line = 2.5, cex=1.0);
rect(xleft=c(1:216)+0.2, xright=c(1:216)+0.8, ybottom = L2S_MHOutflow$X2.50., ytop =L2S_MHOutflow$X97.50., col="grey80", border =NA)
lines(x=1:216, y=L2S_MHOutflow$X50., col="red", lwd = 2, lty=1)
lines(x=1:252, y=fn_FG_AL, col="black", lwd = 2, lty=1)
legend(x=0,y=9000,lty=c(1), lwd =2, bty="n",legend = c("L2SWBM Median","fn_FG_AL"),ncol=1,cex=1.3,col = c("red", "black"))
legend(x=75, y=9000, fill="grey80",bty="n",legend = c("L2SWBM (95% CI)"),ncol=3,cex=1.3,border="grey80")
text(x=200, y=8500, "Plot 6: fn_FG_AL", srt=0, cex=1.8)

## PLOT 6B: fn_FG_AL difference
plot(c(0), c(0), type = "n", col = 8, lwd = 3, axes = FALSE, xlim = c(1,252), ylim = c(-1000,1000), xlab="", ylab="");
box()
axis(4, cex.axis = 0.9)
#axis(4, cex.axis = 0.9, labels=FALSE);
# axis(1, cex.axis = 0.9, labels="FALSE");
axis(1, at=seq(1,252,12), labels=c(2000:2020))
rect(xleft=c(1:216)+0.1, xright=c(1:216)+0.9, ybottom = 0, ytop =L2S_MHOutflow$X50.-fn_FG_AL[1:216], col="grey80", border ="grey70")
text(x=220, y=900, "fn_FG_AL", srt=0, cex=1.8)
abline(h=0, col="red")

dev.off()

######
#### 7) Comparison Plots of Historical Flows WITH DIFFERENCE PLOTS for Detroit vs L2S output
######
# PLOT 1 fn_WP_FW
# PLOT 2 fn_FW_WY
# PLOT 3 MLR_FW_WY
# PLOT 4 fn_WP_FW
# PLOT 5 MLR_WP_AM <- can not plot until AM monthly data is updated
# PLOT 6 fn_WP_AM  <- can not plot until AM monthly data is updated


#pdf(paste('Historical Q Comparisons Nov18v2.pdf'), width = 12, height = 4);
pdf(paste('Historical Q Comparisons w DiffPlots Detroit Dec08v2_400k_PrecOnQis100.pdf'), width = 20, height = 10);
par(mfrow = c(6, 2))

par(mar = c(0,2,0,6))
par(oma = c(4,4,4,4))

## PLOT 1 : fn_WP_FW
plot(c(0), c(0), type = "n", col = 8, lwd = 3, axes = FALSE, xlim = c(1,252), ylim = c(3000,9000), xlab="", ylab="");
box()
axis(2, cex.axis = 0.9)
axis(4, cex.axis = 0.9, labels="FALSE");
axis(1, cex.axis = 0.9, labels="FALSE");
#axis(1, at=seq(1,252,12), labels=c(2000:2020))
mtext(paste("Detroit River Channel Flow (cms)"), side = 2, line = 2.5, cex=1.0);
rect(xleft=c(1:216)+0.2, xright=c(1:216)+0.8, ybottom = L2S_STCOutflow$X2.50., ytop =L2S_STCOutflow$X97.50., col="grey80", border =NA)
lines(x=1:216, y=L2S_STCOutflow$X50., col="red", lwd = 2, lty=1)
lines(x=1:252, y=fn_WP_FW, col="black", lwd = 2, lty=1)
legend(x=0,y=9000,lty=c(1), lwd =2, bty="n",legend = c("L2SWBM Median","fn_WP_FW"),ncol=1,cex=1.3,col = c("red", "black"))
legend(x=75, y=9000, fill="grey80",bty="n",legend = c("L2SWBM (95% CI)"),ncol=3,cex=1.3,border="grey80")
text(x=200, y=8500, "fn_WP_FW", srt=0, cex=1.8)

## PLOT 1B: fn_WP_FW difference
plot(c(0), c(0), type = "n", col = 8, lwd = 3, axes = FALSE, xlim = c(1,252), ylim = c(-1000,1000), xlab="", ylab="");
box()
axis(2, cex.axis = 0.9)
#axis(4, cex.axis = 0.9, labels="FALSE");
#axis(1, cex.axis = 0.9, labels="FALSE");
#axis(1, at=seq(1,252,12), labels=c(2000:2020))
rect(xleft=c(1:216)+0.1, xright=c(1:216)+0.9, ybottom = 0, ytop =L2S_MHOutflow$X50.-fn_WP_FW[1:216], col="grey80", border ="grey70")
text(x=220, y=900, "fn_WP_FW", srt=0, cex=1.8)
abline(h=0, col="red")


## PLOT 2 : fn_FW_WY
plot(c(0), c(0), type = "n", col = 8, lwd = 3, axes = FALSE, xlim = c(1,252), ylim = c(3000,9000), xlab="", ylab="");
box()
axis(2, cex.axis = 0.9, labels="FALSE")
axis(4, cex.axis = 0.9);
axis(1, cex.axis = 0.9, labels="FALSE");
#axis(1, at=seq(1,252,12), labels=c(2000:2020))
mtext(paste("Detroit River Channel Flow (cms)"), side = 4, line = 2.5, cex=1.0);
rect(xleft=c(1:216)+0.2, xright=c(1:216)+0.8, ybottom = L2S_STCOutflow$X2.50., ytop =L2S_STCOutflow$X97.50., col="grey80", border =NA)
lines(x=1:216, y=L2S_STCOutflow$X50., col="red", lwd = 2, lty=1)
lines(x=1:252, y=fn_FW_WY, col="black", lwd = 2, lty=1)
legend(x=0,y=9000,lty=c(1), lwd =2, bty="n",legend = c("L2SWBM Median","fn_FW_WY"),ncol=1,cex=1.3,col = c("red", "black"))
legend(x=75, y=9000, fill="grey80",bty="n",legend = c("L2SWBM (95% CI)"),ncol=3,cex=1.3,border="grey80")
text(x=200, y=8500, "fn_FW_WY", srt=0, cex=1.8)

## PLOT 2B: fn_FW_WY difference
plot(c(0), c(0), type = "n", col = 8, lwd = 3, axes = FALSE, xlim = c(1,252), ylim = c(-1000,1000), xlab="", ylab="");
box()
axis(4, cex.axis = 0.9)
#axis(4, cex.axis = 0.9, labels="FALSE");
#axis(1, cex.axis = 0.9, labels="FALSE");
#axis(1, at=seq(1,252,12), labels=c(2000:2020))
rect(xleft=c(1:216)+0.1, xright=c(1:216)+0.9, ybottom = 0, ytop =L2S_MHOutflow$X50.-fn_FW_WY[1:216], col="grey80", border ="grey70")
text(x=220, y=900, "fn_FW_WY", srt=0, cex=1.8)
abline(h=0, col="red")

## PLOT 3 : MLR_FW_WY
plot(c(0), c(0), type = "n", col = 8, lwd = 3, axes = FALSE, xlim = c(1,252), ylim = c(3000,9000), xlab="", ylab="");
box()
axis(2, cex.axis = 0.9)
axis(4, cex.axis = 0.9, labels="FALSE");
axis(1, cex.axis = 0.9, labels="FALSE");
#axis(1, at=seq(1,252,12), labels=c(2000:2020))
mtext(paste("Detroit River Channel Flow (cms)"), side = 2, line = 2.5, cex=1.0);
rect(xleft=c(1:216)+0.2, xright=c(1:216)+0.8, ybottom = L2S_STCOutflow$X2.50., ytop =L2S_STCOutflow$X97.50., col="grey80", border =NA)
lines(x=1:216, y=L2S_STCOutflow$X50., col="red", lwd = 2, lty=1)
lines(x=1:252, y=MLR_FW_WY, col="black", lwd = 2, lty=1)
legend(x=0,y=9000,lty=c(1), lwd =2, bty="n",legend = c("L2SWBM Median","MLR_FW_WY"),ncol=1,cex=1.3,col = c("red", "black"))
legend(x=75, y=9000, fill="grey80",bty="n",legend = c("L2SWBM (95% CI)"),ncol=3,cex=1.3,border="grey80")
text(x=200, y=8500, "MLR_FW_WY", srt=0, cex=1.8)

## PLOT 3B: MLR_FW_WY difference
plot(c(0), c(0), type = "n", col = 8, lwd = 3, axes = FALSE, xlim = c(1,252), ylim = c(-1000,1000), xlab="", ylab="");
box()
axis(2, cex.axis = 0.9)
#axis(4, cex.axis = 0.9, labels="FALSE");
#axis(1, cex.axis = 0.9, labels="FALSE");
#axis(1, at=seq(1,252,12), labels=c(2000:2020))
rect(xleft=c(1:216)+0.1, xright=c(1:216)+0.9, ybottom = 0, ytop =L2S_MHOutflow$X50.-MLR_FW_WY[1:216], col="grey80", border ="grey70")
text(x=220, y=900, "MLR_FW_WY", srt=0, cex=1.8)
abline(h=0, col="red")


## PLOT 4 : fn_WP_FW
plot(c(0), c(0), type = "n", col = 8, lwd = 3, axes = FALSE, xlim = c(1,252), ylim = c(3000,9000), xlab="", ylab="");
box()
axis(2, cex.axis = 0.9, labels="FALSE")
axis(4, cex.axis = 0.9);
axis(1, cex.axis = 0.9, labels="FALSE");
#axis(1, at=seq(1,252,12), labels=c(2000:2020))  
mtext(paste("Detroit River Channel Flow (cms)"), side = 4, line = 2.5, cex=1.0);
rect(xleft=c(1:216)+0.2, xright=c(1:216)+0.8, ybottom = L2S_STCOutflow$X2.50., ytop =L2S_STCOutflow$X97.50., col="grey80", border =NA)
lines(x=1:216, y=L2S_STCOutflow$X50., col="red", lwd = 2, lty=1)
lines(x=1:252, y=fn_WP_FW, col="black", lwd = 2, lty=1)
legend(x=0,y=9000,lty=c(1), lwd =2, bty="n",legend = c("L2SWBM Median","fn_WP_FW"),ncol=1,cex=1.3,col = c("red", "black"))
legend(x=75, y=9000, fill="grey80",bty="n",legend = c("L2SWBM (95% CI)"),ncol=3,cex=1.3,border="grey80")
text(x=200, y=8500, "fn_WP_FW", srt=0, cex=1.8)

## PLOT 4B: fn_WP_FW difference
plot(c(0), c(0), type = "n", col = 8, lwd = 3, axes = FALSE, xlim = c(1,252), ylim = c(-1000,1000), xlab="", ylab="");
box()
axis(4, cex.axis = 0.9)
#axis(4, cex.axis = 0.9, labels="FALSE");
#axis(1, cex.axis = 0.9, labels="FALSE");
#axis(1, at=seq(1,252,12), labels=c(2000:2020))
rect(xleft=c(1:216)+0.1, xright=c(1:216)+0.9, ybottom = 0, ytop =L2S_MHOutflow$X50.-fn_WP_FW[1:216], col="grey80", border ="grey70")
text(x=220, y=900, "fn_WP_FW", srt=0, cex=1.8)
abline(h=0, col="red")

## PLOT 5 : MLR_WP_AM <- can not plot until AM monthly data is updated
# plot(c(0), c(0), type = "n", col = 8, lwd = 3, axes = FALSE, xlim = c(1,252), ylim = c(3000,9000), xlab="", ylab="");
# box()
# axis(2, cex.axis = 0.9)
# axis(4, cex.axis = 0.9, labels="FALSE");
# axis(1, cex.axis = 0.9, labels="FALSE");
# #axis(1, at=seq(1,252,12), labels=c(2000:2020))
# mtext(paste("Detroit River Channel Flow (cms)"), side = 2, line = 2.5, cex=1.0);
# rect(xleft=c(1:216)+0.2, xright=c(1:216)+0.8, ybottom = L2S_STCOutflow$X2.50., ytop =L2S_STCOutflow$X97.50., col="grey80", border =NA)
# lines(x=1:216, y=L2S_STCOutflow$X50., col="red", lwd = 2, lty=1)
# lines(x=1:252, y=MLR_WP_AM, col="black", lwd = 2, lty=1)
# legend(x=0,y=9000,lty=c(1), lwd =2, bty="n",legend = c("L2SWBM Median","MLR_WP_AM"),ncol=1,cex=1.3,col = c("red", "black"))
# legend(x=75, y=9000, fill="grey80",bty="n",legend = c("L2SWBM (95% CI)"),ncol=3,cex=1.3,border="grey80")
# text(x=200, y=8500, "MLR_WP_AM", srt=0, cex=1.8)

## PLOT 5B: MLR_WP_AM difference <- can not plot until AM monthly data is updated
# plot(c(0), c(0), type = "n", col = 8, lwd = 3, axes = FALSE, xlim = c(1,252), ylim = c(-1000,1000), xlab="", ylab="");
# box()
# axis(2, cex.axis = 0.9)
# #axis(4, cex.axis = 0.9, labels="FALSE");
# #axis(1, cex.axis = 0.9, labels="FALSE");
# #axis(1, at=seq(1,252,12), labels=c(2000:2020))
# rect(xleft=c(1:216)+0.1, xright=c(1:216)+0.9, ybottom = 0, ytop =L2S_MHOutflow$X50.-MLR_WP_AM[1:216], col="grey80", border ="grey70")
# text(x=220, y=900, "MLR_WP_AM", srt=0, cex=1.8)
# abline(h=0, col="red")

## PLOT 6 : fn_WP_AM  <- can not plot until AM monthly data is updated
# plot(c(0), c(0), type = "n", col = 8, lwd = 3, axes = FALSE, xlim = c(1,252), ylim = c(3000,9000), xlab="", ylab="");
# box()
# axis(2, cex.axis = 0.9, labels="FALSE")
# axis(4, cex.axis = 0.9);
# axis(1, cex.axis = 0.9, labels="FALSE");
# axis(1, at=seq(1,252,12), labels=c(2000:2020))
# mtext(paste("Detroit River Channel Flow (cms)"), side = 4, line = 2.5, cex=1.0);
# rect(xleft=c(1:216)+0.2, xright=c(1:216)+0.8, ybottom = L2S_STCOutflow$X2.50., ytop =L2S_STCOutflow$X97.50., col="grey80", border =NA)
# lines(x=1:216, y=L2S_STCOutflow$X50., col="red", lwd = 2, lty=1)
# lines(x=1:252, y=fn_WP_AM, col="black", lwd = 2, lty=1)
# legend(x=0,y=9000,lty=c(1), lwd =2, bty="n",legend = c("L2SWBM Median","fn_WP_AM"),ncol=1,cex=1.3,col = c("red", "black"))
# legend(x=75, y=9000, fill="grey80",bty="n",legend = c("L2SWBM (95% CI)"),ncol=3,cex=1.3,border="grey80")
# text(x=200, y=8500, "fn_WP_AM", srt=0, cex=1.8)

## PLOT 6B: fn_WP_AM difference <- can not plot until AM monthly data is updated
# plot(c(0), c(0), type = "n", col = 8, lwd = 3, axes = FALSE, xlim = c(1,252), ylim = c(-1000,1000), xlab="", ylab="");
# box()
# axis(2, cex.axis = 0.9)
# #axis(4, cex.axis = 0.9, labels="FALSE");
# #axis(1, cex.axis = 0.9, labels="FALSE");
# #axis(1, at=seq(1,252,12), labels=c(2000:2020))
# rect(xleft=c(1:216)+0.1, xright=c(1:216)+0.9, ybottom = 0, ytop =L2S_MHOutflow$X50.-fn_WP_AM[1:216], col="grey80", border ="grey70")
# text(x=220, y=900, "fn_WP_AM", srt=0, cex=1.8)
# abline(h=0, col="red")

## PLOT 7 : TF (transfer factor)
plot(c(0), c(0), type = "n", col = 8, lwd = 3, axes = FALSE, xlim = c(1,252), ylim = c(3000,9000), xlab="", ylab="");
box()
axis(2, cex.axis = 0.9)
axis(4, cex.axis = 0.9, labels="FALSE");
axis(1, cex.axis = 0.9, labels="FALSE");
axis(1, at=seq(1,252,12), labels=c(2000:2020))
mtext(paste("Detroit River Channel Flow (cms)"), side = 2, line = 2.5, cex=1.0);
rect(xleft=c(1:216)+0.2, xright=c(1:216)+0.8, ybottom = L2S_STCOutflow$X2.50., ytop =L2S_STCOutflow$X97.50., col="grey80", border =NA)
lines(x=1:216, y=L2S_STCOutflow$X50., col="red", lwd = 2, lty=1)
lines(x=1:252, y=TF, col="black", lwd = 2, lty=1)
legend(x=0,y=9000,lty=c(1), lwd =2, bty="n",legend = c("L2SWBM Median","TF"),ncol=1,cex=1.3,col = c("red", "black"))
legend(x=75, y=9000, fill="grey80",bty="n",legend = c("L2SWBM (95% CI)"),ncol=3,cex=1.3,border="grey80")
text(x=200, y=8500, "Transfer factor", srt=0, cex=1.8)

## PLOT 7B: TF (transfer factor) difference <- can not plot until AM monthly data is updated
plot(c(0), c(0), type = "n", col = 8, lwd = 3, axes = FALSE, xlim = c(1,252), ylim = c(-1000,1000), xlab="", ylab="");
box()
axis(2, cex.axis = 0.9)
#axis(4, cex.axis = 0.9, labels="FALSE");
#axis(1, cex.axis = 0.9, labels="FALSE");
axis(1, at=seq(1,252,12), labels=c(2000:2020))
rect(xleft=c(1:216)+0.1, xright=c(1:216)+0.9, ybottom = 0, ytop =L2S_MHOutflow$X50.-TF[1:216], col="grey80", border ="grey70")
text(x=220, y=900, "Transfer factor", srt=0, cex=1.8)
abline(h=0, col="red")

dev.off()
