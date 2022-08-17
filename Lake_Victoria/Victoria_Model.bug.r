model {
  
  #For each month
  for (j in PosteriorStartMonth:PosteriorEndMonth){
    
    #Priors
    VictoriaPrecip[j]~dgamma(PrecipShape[m[j]],PrecipRate[m[j]])
    #VictoriaEvap[j]~dunif(0,500)
    VictoriaEvap[j]~dnorm(EvapMean[m[j]],EvapPrecision[m[j]])
    VictoriaOutflow[j]~dnorm(OutflowMean[m[j]],OutflowPrecision[m[j]])#T(0,)
    #VictoriaOutflow[j]~dlnorm(log((OutflowMean[m[j]]^2)/sqrt((OutflowMean[m[j]]^2)+1/OutflowPrecision[m[j]])),1/log(1+(1/OutflowPrecision[m[j]]/(OutflowMean[m[j]]^2))))
    #VictoriaOutflow[j]~dunif(0,200)
    VictoriaRunoff[j]=exp(VictoriaLogRunoff[j])
    VictoriaLogRunoff[j]~dnorm(log((LogRunoffMean[m[j]]^2)/sqrt(LogRunoffMean[m[j]]^2+1/LogRunoffPrecision[m[j]])),1/log(1+(1/LogRunoffPrecision[m[j]]/LogRunoffMean[m[j]]^2)))
    #VictoriaRunoff[j]~dlnorm(log((LogRunoffMean[m[j]]^2)/sqrt((LogRunoffMean[m[j]]^2)+1/LogRunoffPrecision[m[j]])),1/log(1+(1/LogRunoffPrecision[m[j]]/(LogRunoffMean[m[j]]^2))))
    #VictoriaRunoff[j]~dunif(0,500)
    #VictoriaRunoff[j]~dnorm(LogRunoffMean[m[j]],LogRunoffPrecision[m[j]])T(0,)
    
    #Likelihood function
    yVictoriaPrecip1[j]~dnorm(yPrecip1Mean[j],yPrecip1Precision)
    yVictoriaPrecip2[j]~dnorm(yPrecip2Mean[j],yPrecip2Precision)
    #yVictoriaPrecip3[j]~dnorm(yPrecip3Mean[j],yPrecip3Precision)
    yVictoriaEvap[j]~dnorm(yEvapMean[j],yEvapPrecision)
    yVictoriaOutflow[j]~dnorm(yOutflowMean[j],yOutflowPrecision)
    yVictoriaRunoff1[j]~dnorm(yRunoff1Mean[j],yRunoff1Precision)
   # yVictoriaRunoff2[j]~dnorm(yRunoff2Mean[j],yRunoff2Precision)
   # yVictoriaRunoff3[j]~dnorm(yRunoff3Mean[j],yRunoff3Precision)
    
    #Mean of likelihood function = true mean + bias
    yPrecip1Mean[j]=VictoriaPrecip[j]+yPrecip1Bias[m[j]]
    yPrecip2Mean[j]=VictoriaPrecip[j]+yPrecip2Bias[m[j]]
    #yPrecip3Mean[j]=VictoriaPrecip[j]+yPrecip3Bias[m[j]]
    yEvapMean[j]=VictoriaEvap[j]+yEvapBias[m[j]]
    #yOutflowMean[j]=VictoriaOutflow[j]*VictoriaOutflowMultBias+yOutflowBias[m[j]]#--JLC
    yOutflowMean[j]=VictoriaOutflow[j]+yOutflowBias[m[j]]#--JLC
    yRunoff1Mean[j]=VictoriaRunoff[j]+yRunoff1Bias[m[j]]
    #yRunoff2Mean[j]=VictoriaRunoff[j]+yRunoff2Bias[m[j]]
    #yRunoff3Mean[j]=VictoriaRunoff[j]+yRunoff3Bias[m[j]]
    
  }
  
  #For rolling window and Water balance Equation
  for (k in RollPeriod:PosteriorEndMonth){
    yVictoriaRStore[k]~dnorm(VictoriaRStore[k],yVictoriaRStorePreci)
    
    VictoriaRStore[k]=sum(VictoriaPrecip[(k-RollPeriod+1):k])
    -sum(VictoriaEvap[(k-RollPeriod+1):k])
    -sum(VictoriaOutflow[(k-RollPeriod+1):k])
    +sum(VictoriaRunoff[(k-RollPeriod+1):k])
    +sum(VictoriaError[m[(k-RollPeriod+1):k]])
    
  }
  
  #Bias
  for (jp in 1:12){
    yPrecip1Bias[jp]~dnorm(0,0.01)
    yPrecip2Bias[jp]~dnorm(0,0.01)
    #yPrecip3Bias[jp]~dnorm(0,0.01)
    yEvapBias[jp]~dnorm(0,0.01)
    #yOutflowBias[jp]~dnorm(0,0.01)
    yOutflowBias[jp]~dnorm(0,0.01)#--JLC
    yRunoff1Bias[jp]~dnorm(0,0.01)
 #   yRunoff2Bias[jp]~dnorm(0,0.01)
  #  yRunoff3Bias[jp]~dnorm(0,0.01)
    
    VictoriaError[jp]~dnorm(0,0.1)#0.01
  }
  
  #VictoriaOutflowMultBias ~ dunif(0,2)#--JLC
  
  #Precision for Observation
  yVictoriaRStorePreci~dlnorm(-log(100)-log(RollPeriod),1)#dgamma(0.1,0.1)#--JLC
  yPrecip1Precision~dlnorm(-log(100),1)#dgamma(0.1,0.1)#--JLC
  yPrecip2Precision~dlnorm(-log(100),1)#dgamma(0.1,0.1)#--JLC
  #  yPrecip3Precision~dgamma(0.1,0.1)
  yEvapPrecision~dlnorm(-log(100),1)#dgamma(0.1,0.1)#--JLC
  yOutflowPrecision~dlnorm(-log(100),1)#dgamma(0.1,0.1)#--JLC
  yRunoff1Precision~dlnorm(-log(100),1)#dgamma(0.1,0.1)#--JLC
  #yRunoff2Precision~dgamma(0.1,0.1)
  #yRunoff3Precision~dgamma(0.1,0.1)
  
  #Posterior Predictive (PP) Distribution
  for (jp in PosteriorStartMonth:PosteriorEndMonth ){
    yVictoriaPrecip1PP[jp]~dnorm(yPrecip1Mean[jp],yPrecip1Precision)
    yVictoriaPrecip2PP[jp]~dnorm(yPrecip2Mean[jp],yPrecip2Precision)
 #   yVictoriaPrecip3PP[jp]~dnorm(yPrecip3Mean[jp],yPrecip3Precision)
    yVictoriaEvapPP[jp]~dnorm(yEvapMean[jp],yEvapPrecision)
    yVictoriaOutflowPP[jp]~dnorm(yOutflowMean[jp],yOutflowPrecision)
    yVictoriaRunoff1PP[jp]~dnorm(yRunoff1Mean[jp],yRunoff1Precision)
  #  yVictoriaRunoff2PP[jp]~dnorm(yRunoff2Mean[jp],yRunoff2Precision)
   # yVictoriaRunoff3PP[jp]~dnorm(yRunoff3Mean[jp],yRunoff3Precision)
    
    #Lake level in each month
    yVictoriaDStorePP[jp]~dnorm(VictoriaDStore[jp],yVictoriaRStorePreci)
    
    # VictoriaDStore[jp]=sum(VictoriaPrecip[jp])
    # -sum(VictoriaEvap[jp])
    # -sum(VictoriaOutflow[jp])
    # +sum(VictoriaRunoff[jp])
    # +sum(VictoriaError[m[jp]])
    
    VictoriaDStore[jp]=(VictoriaPrecip[jp]
    -VictoriaEvap[jp]
    -VictoriaOutflow[jp]
    +VictoriaRunoff[jp]
    +VictoriaError[m[jp]])
  }
  
  #6 months
  for(k in 6:PosteriorEndMonth){
    yVictoriaR6MStorePP[k]~dnorm(VictoriaR6MStore[k],yVictoriaRStorePreci)
    
    VictoriaR6MStore[k]=(
      sum(VictoriaPrecip[(k-6+1):k])
      -sum(VictoriaEvap[(k-6+1):k])
      -sum(VictoriaOutflow[(k-6+1):k])
      +sum(VictoriaRunoff[(k-6+1):k])
      +sum(VictoriaError[m[(k-6+1):k]])
    )
  }
  
  
  #1 year
  for(x in 12:PosteriorEndMonth){
    yVictoriaR1YStorePP[x]~dnorm(VictoriaR1YStore[x],yVictoriaRStorePreci)
    
    VictoriaR1YStore[x]=(
      sum(VictoriaPrecip[(x-12+1):x])
      -sum(VictoriaEvap[(x-12+1):x])
      -sum(VictoriaOutflow[(x-12+1):x])
      +sum(VictoriaRunoff[(x-12+1):x])
      +sum(VictoriaError[m[(x-12+1):x]])
    )
  }
  
  #5 years
  for (z in 60:PosteriorEndMonth){
    yVictoriaR5YStorePP[z]~dnorm(VictoriaR5YStore[z],yVictoriaRStorePreci)
    
    VictoriaR5YStore[z]=(
      sum(VictoriaPrecip[(z-60+1):z])
      -sum(VictoriaEvap[(z-60+1):z])
      -sum(VictoriaOutflow[(z-60+1):z])
      +sum(VictoriaRunoff[(z-60+1):z])
      +sum(VictoriaError[m[(z-60+1):z]])
    )
  }
}