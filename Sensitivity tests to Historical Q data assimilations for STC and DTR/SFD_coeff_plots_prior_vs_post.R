## What does this code do?
## Plots histograms for c, d, and f coefficients from SFD equations. Priors and posteriors from model run.
## ** IMPORTANT NOTE: If the priors (mean or sd) used for c, d, and f are ever in the L2S, you must also update them here!!

## author: Sarah Katz (skatzees@umich.edu)
###########################################################################################

## Read in products from L2S runs
wd = setwd("/Users/skatzees/Dropbox (University of Michigan)/Hydro L2SWBM GSRA/WaterBalance/SFD_Indep_Q_Est/")
load("Dec8_v2_SFD_StClair&Detroit_400000_PrecOnQis100_20002017_400k_roll1.RData")

## Note: to work on: Right now, the historical values of c, d, and f are manually entered, which is inelegant. Can change to pull from "Compiled_historicalSFD_coefficients.csv"  

######################
#### c, d, f coefficient plots ####
######################
pdf(paste('SFD_Coefficients_vsHistoricalCoeffs_Dec08v2_400k_PrecIs100.pdf'), width = 9, height = 8);
par(mfrow=c(2,3))
#### Makes PDFs of c, d, f values
##### SFD c StClair #####
hist(jSample[1][,paste("SFDc_STC")][[1]], freq = FALSE, main=NULL,xlab=NULL, xlim = c(1,600), ylim=c(0,0.009), breaks=20, border="grey70", col="grey90")
mtext(paste("c StClairRiver"), side = 1, line = 2.5, cex=1.0);
axis(1, at=seq(0,1200,200)) #, labels=c(0:1200))
input.c = seq(1,1200,.1)
lines(x = input.c,
      y = dnorm(input.c, mean = 250, sd = 50),
      col = "red", lwd=2)
lines(x = input.c,
      y = dnorm(input.c, mean = mean(jSample[1][,paste("SFDc_STC")][[1]]), sd = sd(jSample[1][,paste("SFDc_STC")][[1]])),
      col = "black", lwd=2)
#abline(v = 201.03, col = "blue", lwd = 2)
abline(v = 250, col = "blue", lwd = 2) #Prior
segments(x0= c(201.03, 262.84, 270.22, 458.5184, 511.0261, 478.3803), x1=c(201.03, 262.84, 270.22, 458.5184, 511.0261, 478.3803), y0=-1, y1=0, col=c("sandybrown", "springgreen", "dodgerblue3", "yellowgreen", "gold3", "purple"), lwd = 2) #Historical estimates from Thompson and Fay & Noorbakhsh report
legend(x=350,y=0.005,lty=1, lwd =2, bty="n",legend = c("Prior","Posterior"),ncol=1,cex=0.8,col = c("red", "black"))


# ##### SFD d StClair #####
hist(jSample[1][,paste("SFDd_STC")][[1]], freq = FALSE, main=NULL,xlab=NULL, xlim = c(-1,5), breaks=10, border="grey70", col="grey90")
mtext(paste("d StClairRiver"), side = 1, line = 2.5, cex=1.0);
input.d = seq(-3,6,.001)
lines(x = input.d,
      y = dnorm(input.d, mean = 1.6, sd = 2),
      col = "red", lwd=2)
lines(x = input.d,
      y = dnorm(input.d, mean = mean(jSample[1][,paste("SFDd_STC")][[1]]), sd = sd(jSample[1][,paste("SFDd_STC")][[1]])),
      col = "black", lwd=2)
#abline(v = 1.732, col = "blue", lwd = 2)
abline(v = 1.6, col = "blue", lwd = 2)
segments(x0 = c(1.732, 1.523, 1.417, 1.2932, 1.1792, 1.1342),x1 = c(1.732, 1.523, 1.417, 1.2932, 1.1792, 1.1342),y0=-1, y1=0, col=c("sandybrown", "springgreen", "dodgerblue3", "yellowgreen", "gold3", "purple"), lwd = 2) #Historical estimates from Thompson and Fay & Noorbakhsh report


# ##### SFD f StClair #####
hist(jSample[1][,paste("SFDf_STC")][[1]], freq = FALSE, main=NULL,xlab=NULL, xlim = c(-1,4), ylim=c(0,3.5), breaks=6, border="grey70", col="grey90")
mtext(paste("f StClairRiver"), side = 1, line = 2.5, cex=1.0);
input.f = seq(-1,3,.01)
input.fT = seq(0,3,.01)
lines(x = input.fT,
      y = dnorm(input.fT, mean = 0.5, sd = 1),
      col = "red", lwd=2)
lines(x = input.f,
      y = dnorm(input.f, mean = mean(jSample[1][,paste("SFDf_STC")][[1]]), sd = sd(jSample[1][,paste("SFDf_STC")][[1]])),
      col = "black", lwd=2)
#abline(v = 0.428, col = "blue", lwd = 2)
abline(v = 0.5, col = "blue", lwd = 2)
segments(x0= c(0.428, 0.554, 0.635, 1.2932, 1.1792, 0.5187), x1=c(0.428, 0.554, 0.635, 1.2932, 1.1792, 0.5187), y0=-1, y1=0, col=c("sandybrown", "springgreen", "dodgerblue3", "yellowgreen", "gold3", "purple"), lwd = 2) #Historical estimates from Thompson and Fay & Noorbakhsh report
legend(x=1,y=2,lty=1, lwd =2, bty="n",legend = c("MLR_FG_DD","MLR_FG_SCSP", "MLR_FG_AL", "fn_FG_DD", "fn_FG_SCSP", "fn_FG_AL"),ncol=1,cex=0.8,col = c("sandybrown", "springgreen", "dodgerblue3", "yellowgreen", "gold3", "purple"))


##### SFD c Detroit #####
hist(jSample[1][,paste("SFDc_DTR")][[1]], freq = FALSE, main=NULL,xlab=NULL, xlim = c(1,150), ylim=c(0,0.03), breaks=20, border="grey70", col="grey90")
mtext(paste("c DetroitRiver"), side = 1, line = 2.5, cex=1.0);
axis(1, at=seq(0,1200,200)) #, labels=c(0:1200))
input.c = seq(1,1200,.1)
lines(x = input.c,
      y = dnorm(input.c, mean = 55, sd = 16),
      col = "red", lwd=2)
lines(x = input.c,
      y = dnorm(input.c, mean = mean(jSample[1][,paste("SFDc_DTR")][[1]]), sd = sd(jSample[1][,paste("SFDc_DTR")][[1]])),
      col = "black", lwd=2)
#abline(v = 201.03, col = "blue", lwd = 2)
abline(v = 55, col = "blue", lwd = 2) #Prior
segments(x0= c(52.12,40.15,66.54,118.1081,51.3625,66.2808), x1=c(52.12,40.15,66.54,118.1081,51.3625,66.2808), y0=-1, y1=0, col=c("turquoise", "thistle", "slateblue1", "gold", "green4", "coral4"), lwd = 2) #Historical estimates from Thompson and Fay & Noorbakhsh report
legend(x=100,y=0.015,lty=1, lwd =2, bty="n",legend = c("Prior","Posterior"),ncol=1,cex=0.8,col = c("red", "black"))


# ##### SFD d Detroit #####
hist(jSample[1][,paste("SFDd_DTR")][[1]], freq = FALSE, main=NULL,xlab=NULL, xlim = c(-1,5), breaks=10, border="grey70", col="grey90")
mtext(paste("d DetroitRiver"), side = 1, line = 2.5, cex=1.0);
input.d = seq(-3,6,.001)
lines(x = input.d,
      y = dnorm(input.d, mean = 1.6, sd = 2),
      col = "red", lwd=2)
lines(x = input.d,
      y = dnorm(input.d, mean = mean(jSample[1][,paste("SFDd_DTR")][[1]]), sd = sd(jSample[1][,paste("SFDd_DTR")][[1]])),
      col = "black", lwd=2)
#abline(v = 1.732, col = "blue", lwd = 2)
abline(v = 1.6, col = "blue", lwd = 2)
segments(x0 = c(2.139,2.14,2.245,1.8364,2.0851,2.1220),x1 = c(2.139,2.14,2.245,1.8364,2.0851,2.1220),y0=-1, y1=0, col=c("turquoise", "thistle", "slateblue1", "gold", "green4", "coral4"), lwd = 2) #Historical estimates from Thompson and Fay & Noorbakhsh report


# ##### SFD f Detroit #####
hist(jSample[1][,paste("SFDf_DTR")][[1]], freq = FALSE, main=NULL,xlab=NULL, xlim = c(-1,3), ylim=c(0,4), breaks=6, border="grey70", col="grey90")
mtext(paste("f DetroitRiver"), side = 1, line = 2.5, cex=1.0);
input.f = seq(-1,3,.01)
input.fT = seq(0,3,.01)
lines(x = input.fT,
      y = dnorm(input.fT, mean = 0.5, sd = 1),
      col = "red", lwd=2)
lines(x = input.f,
      y = dnorm(input.f, mean = mean(jSample[1][,paste("SFDf_DTR")][[1]]), sd = sd(jSample[1][,paste("SFDf_DTR")][[1]])),
      col = "black", lwd=2)
#abline(v = 0.428, col = "blue", lwd = 2)
abline(v = 0.5, col = "blue", lwd = 2)
segments(x0= c(0.436,0.436,0.419,0.3624,0.3698,0.2943), x1=c(0.436,0.436,0.419,0.3624,0.3698,0.2943), y0=-1, y1=0, col=c("turquoise", "thistle", "slateblue1", "gold", "green4", "coral4"), lwd = 2) #Historical estimates from Thompson and Fay & Noorbakhsh report
legend(x=1,y=3,lty=1, lwd =2, bty="n",legend = c("MLR_WP_WY","MLR_WP_AM","MLR_FW_WY","fn_WP_FW","fn_WP_AM","fn_FW_WY"),ncol=1,cex=0.8,col = c("turquoise", "thistle", "slateblue1", "gold", "green4", "coral4"))

######
 dev.off()


