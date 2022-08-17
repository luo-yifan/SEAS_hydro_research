##########################################
###1. Set up working directory and data###
##########################################
#library packages & setwd
library(rjags)
setwd("D:\\Desktop\\WaterBalance\\Lake Victoria with Outflow")

#read data and clean up
#VictoriaData=read.csv("D:\\Downloads\\L2SWBM\\diif.version\\noaa\\L2SWBM_Code_v1\\public_export_v1\\Africa\\l2swbm2\\master_df2.csv",stringsAsFactors = F)
#VictoriaData=VictoriaData[,2:ncol(VictoriaData)]
LakeVictoria=read.csv("LakeVictoria.csv",stringsAsFactors = F)

#set up prior range
#PriorRange0=which(as.numeric(format(as.Date(LakeVictoria[,1]),"%y"))==2009 
#                  && as.numeric(format(as.Date(LakeVictoria[,1]),"%m"))==1)
#PriorRange=which(as.numeric(format(as.Date(LakeVictoria[,1]),"%y"))==2013 
 #                 && as.numeric(format(as.Date(LakeVictoria[,1]),"%m"))==12)

#LakeVictoria=VictoriaData[PriorRange0:PriorRange,]
#LakeVictoria[LakeVictoria==0]=NA # address logRunoff calculation issue
#date1=as.Date(LakeVictoria[,1],format='%m/%d/%Y')
#LakeVictoria=cbind(date1,LakeVictoria)
#set up posterior range
startAnalysisYear = 2009;
startAnalysisMonth = 1;
endAnalysisYear = 2018;
endAnalysisMonth = 12;
dateAll = (LakeVictoria[,1])
idStart = 1
idEnd = 120
PosteriorRange=idStart:idEnd

#########################################
###2. Parameter for Prior Distribution###
#########################################

#set prior vectors
PrecipShape=NULL
PrecipRate=NULL
EvapMean=NULL
EvapPrecision=NULL
OutflowMean=NULL
OutflowPrecision=NULL
LogRunoffMean=NULL
LogRunoffPrecision=NULL

#assign variable colunms
Precipitation.Column.Number=2
Runoff.Column.Number=3
#Outflow.Column.Number=7

#set up prior for evap using LoclimData 
# NOT USED!
# locclim=read.csv("LocClim_evap.csv",stringsAsFactors = F)
# locclim=locclim[16:29,]
# evap=as.numeric(as.character(locclim$Best.Estimate[2:nrow(locclim)-1]))
# evap=evap[2:13]
# evap.var=as.numeric(as.character(locclim$Standard.Error[2:nrow(locclim)-1]))
# evap.var=evap.var[2:13]
# evap.var=evap.var*evap.var



#dateNumbers <- as.numeric(as.character(as.Date(LakeVictoria$Date, format ="%Y-%m-%d" ), format="%Y%d%m"))

datacharcter=as.character(as.Date(LakeVictoria$Date, format ="%m/%d/%Y" ), format="%Y%d%m")
substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}

mont=substrRight(datacharcter,2)
mont
monn=as.numeric(mont)
ID=(1:length(monn))
monn
data1=cbind(ID,monn)
#Set up priors
for (i in 1:12){
  ###### Precipitation, Column 2
  if (monn[i]==i){
  x.bar_s=mean(LakeVictoria$m_p,na.rm = TRUE)
  theta_s=log(x.bar_s)-mean(log(LakeVictoria$m_p),na.rm = TRUE)
  PrecipShape[i]=1/(4*theta_s)*(1+sqrt(1+4*theta_s/3))
  PrecipRate[i]=PrecipShape[i]/x.bar_s
  
  
  ###### Evaporation
  # LocClim - NOT USED!
  # EvapMean[i]=evap[i]
  # EvapPrecision[i]=(1/evap.var[i])
  
  # ERA5 - column 10
  EvapMean[i]=mean(LakeVictoria$era_e,na.rm = TRUE)
  EvapPrecision[i]=(1/var(LakeVictoria$era_e,na.rm = TRUE))
  #Outflow
  OutflowMean[i]=mean(LakeVictoria$Outflow,na.rm = TRUE)
  OutflowPrecision[i]=(1/var(LakeVictoria$Outflow,na.rm = TRUE))
  
  ###### Runoff, Column 7
  LogRunoffMean[i]=mean(LakeVictoria$m_ro,na.rm = TRUE)
  LogRunoffPrecision[i]=(1/var(LakeVictoria$m_ro,na.rm = TRUE))
}
  }

############################################
###3. Observation for likelihood function###
############################################
m=monn[PosteriorRange]

RollPeriod=1

#Fill in observation (likelihood function y)
#waterlevel 1
# yVictoriaDStore=rowMeans(cbind(as.numeric(VictoriaData[,4])*1000,as.numeric(VictoriaData[,7])*1000),na.rm = T) #DStore=difference in storage
waterLevel = (LakeVictoria$m_wl)*1000 # use Cosmo data only
yVictoriaDStore=c(waterLevel[2:length(waterLevel)]-waterLevel[1:length(waterLevel)-1])

yVictoriaRStore=rep(NA,length(yVictoriaDStore)) #RStore=rollperiod cumulative storage
# yVictoriaRStore[RollPeriod:length(yVictoriaRStore)]=VictoriaData$c_wl[(RollPeriod+1):length(yVictoriaRStore)]-VictoriaData$c_wl[1:(length(yVictoriaRStore)-RollPeriod)]
# yVictoriaRStore = yVictoriaRStore * 1000
for (rp in RollPeriod:length(yVictoriaDStore)){
  yVictoriaRStore[rp]=waterLevel[rp+1] - waterLevel[(rp-RollPeriod+1)]
  # yVictoriaRStore[rp]=sum(yVictoriaDStore[(rp-RollPeriod+1):rp])
}

yVictoriaDStore=yVictoriaDStore[PosteriorRange]
yVictoriaDStore[yVictoriaDStore=='NaN']=NA
yVictoriaRStore=yVictoriaRStore[PosteriorRange]
yVictoriaRStore[yVictoriaRStore=='NaN']=NA

#other likelihood function
# dataset 1: mcCartney 
# dataset 2: Cosmo
# dataset 3: ERA5
f=as.numeric(LakeVictoria$m_p)
yVictoriaPrecip1=(f[PosteriorRange])

e=as.numeric(LakeVictoria$c_p)
yVictoriaPrecip2=(e[PosteriorRange])
yVictoriaPrecip2[109:120] <- NA
#yVictoriaPrecip3=as.numeric(LakeVictoria[PosteriorRange,8])


b=as.numeric(LakeVictoria$era_e)
yVictoriaEvap=(b[PosteriorRange])

oo=as.numeric(LakeVictoria$Outflow)
yVictoriaOutflow=(oo[PosteriorRange])


x=as.numeric(LakeVictoria$m_ro)
yVictoriaRunoff1=(x[PosteriorRange])
#yVictoriaRunoff2=as.numeric(LakeVictoria[PosteriorRange,6])
#yVictoriaRunoff3=as.numeric(LakeVictoria[PosteriorRange,9])

############################################
###############4.Call to JAGS###############
############################################

PosteriorStartMonth=1
PosteriorEndMonth=120

#R list pass to JAGS
inputDataCoreJAGS=list(
  "PosteriorStartMonth"=PosteriorStartMonth,
  "PosteriorEndMonth"=PosteriorEndMonth,
  "RollPeriod"=RollPeriod,
  "m"=m,
  "yVictoriaRStore"=yVictoriaRStore,
  "PrecipShape"=PrecipShape,
  "PrecipRate"=PrecipRate,
  "EvapMean"=EvapMean,
  "EvapPrecision"=EvapPrecision,
  "OutflowMean"=OutflowMean,
  "OutflowPrecision"=OutflowPrecision,
  "LogRunoffMean"=LogRunoffMean,
  "LogRunoffPrecision"=LogRunoffPrecision,
  "yVictoriaPrecip1"=yVictoriaPrecip1,
  "yVictoriaPrecip2"=yVictoriaPrecip2,
  #"yVictoriaPrecip3"=yVictoriaPrecip3,
  "yVictoriaEvap"=yVictoriaEvap,
  "yVictoriaOutflow"=yVictoriaOutflow,
  "yVictoriaRunoff1"=yVictoriaRunoff1
  #"yVictoriaRunoff2"=yVictoriaRunoff2,
 # "yVictoriaRunoff3"=yVictoriaRunoff3
)

#Variables that are of interest to us
ParamsToMonitor=c(
  "VictoriaPrecip",
  "VictoriaEvap",
  "VictoriaOutflow",
  "VictoriaRunoff",
  "VictoriaDStore",
  "VictoriaRStore",
  "VictoriaR6MStore",
  "VictoriaR1YStore",
  "VictoriaR5YStore",
  "yVictoriaDStorePP",
  "yVictoriaR6MStorePP",
  "yVictoriaR1YStorePP",
  "yVictoriaR5YStorePP",
  "VictoriaError",
  "yPrecip1Bias",
  "yPrecip2Bias",
  #"yPrecip3Bias",
  "yEvapBias",
  "yRunoff1Bias",
  "yOutflowBias",
  #"VictoriaOutflowMultBias",
  #"yRunoff2Bias",
  #"yRunoff3Bias",
  "yVictoriaRStorePreci",
  "yPrecip1Precision",
  "yPrecip2Precision",
  #"yPrecip3Precision",
  "yEvapPrecision",
  "yOutflowPrecision",
  "yRunoff1Precision",
  #"yRunoff2Precision",
  #"yRunoff3Precision",
  "yVictoriaPrecip1PP",
  "yVictoriaPrecip2PP",
  #"yVictoriaPrecip3PP",
  "yVictoriaEvapPP",
   "yVictoriaOutflowPP",
  "yVictoriaRunoff1PP"
  #"yVictoriaRunoff2PP",
  #"yVictoriaRunoff3PP"
)

#Call to JAGS

halfIters=4000

#Console output
cat(date(),paste(RollPeriod,' ROLL, ERA5 DATA, 3CHAINS, ',halfIters*2/1000,'K ITERATIONS\n\n', sep=''))

#Progress output
cat(date(),'ADAPTING SAMPLER TO MODEL...')

#Initializes and adapts the model
jMod = jags.model(
  file = "Victoria_Model.bug.r",
  data = inputDataCoreJAGS,
  n.chains = 3
)

#Progress output for burning
cat(date(),paste0('UPDATE STEP (',halfIters/1000,'K BURNIN)...\n'))
update(jMod, halfIters)

#Sampling
cat(date(),'SAMPLING... (',halfIters/1000,'K, THINNING DOWN TO 3K)\n')
jSample = coda.samples(jMod, ParamsToMonitor, halfIters, ceiling(halfIters/1000), na.rm=TRUE)

cat(date(),'COMPUTING STATS...\n')
jSumStats = summary(jSample)
jSumStats_MSD = jSumStats$statistics[,1:2]
jSumStats_Q = jSumStats$quantiles
jSumEff = effectiveSize(jSample)

# #Get R-Hats - NOT USED!
# cat('GETTING GELMAN-RUBIN STAT (COMPUTATIONALLY EXPENSIVE)...\n')
# jRHat = gelman.diag(jSample, multivariate=FALSE)
# jRHatEsts = jRHat$psrf

#Print the output
rn = rownames(jSumStats_MSD)
ro_MSD = match(rn, rownames(jSumStats_MSD))
ro_Q = match(rn, rownames(jSumStats_Q))
ro_eff = match(rn, names(jSumEff))
# ro_rhat = match(rn, rownames(jRHatEsts))

jSum = cbind(
  jSumStats_MSD,
  jSumStats_Q[ro_Q,],
  # jRHatEsts[ro_rhat,],
  jSumEff[ro_eff]
)
colnames(jSum)[8] = 'n.eff'

cat(date(),'SAVING WORK...\n')
workspacename = paste0('Victoria_Ver4_',startAnalysisYear,endAnalysisYear,'_',(halfIters*2)/1000,'k_roll',RollPeriod,'.RData')
save.image(workspacename)
cat(date(),'DONE!\n')

