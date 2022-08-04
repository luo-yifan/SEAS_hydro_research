####################################################################################################
### R Script to produce L2SWBM estimates Great Lakes water balance
### Author: Joeseph Smith - NOAA CIGLR
###         Hong Xuan DO  - SEAS U-M
### Date:   Jan 01, 2020
####################################################################################################

### RUN MODEL
#  Use two chains, the 1st half of all iterations was discarded, and the 2nd half was thinned to keep 1000 iterations per chain

cat(paste('ALL LAKES, ',rollPeriod,' ROLL, 2 CHAINS, ',iters,' ITERATIONS\n', sep=''));

startTime = proc.time()[3];

cat(date(),'ADAPTING SAMPLER TO MODEL...\n')

jMod = jags.model(
	file = paste(modelName,'.bug.r', sep=''),
	data = inputDataCoreJAGS,
	n.chains = 2
);
isAdapted = adapt(jMod);
while(!isAdapted){
	cat('More adapting...\n')
	adapt(jMod, 100)
}
gc();

# UPDATE FOR BURNIN
cat(date(),'UPDATE STEP (BURNIN)...\n')
update(jMod, halfIters)
gc();

# SAMPLE
cat(date(),'SAMPLING... (WITH THINNING)\n')
jSample = coda.samples(jMod, paramsToMonitor, halfIters, ceiling(halfIters/1000), na.rm=TRUE)

sampleEndTime = proc.time()[3] - startTime;
cat(date(),'SAMPLING TIME:',sampleEndTime,'\n')
gc();

### COMPUTE STATS
cat(date(),'COMPUTING STATS...\n')
jSumStats = summary(jSample)
jSumStats_MSD = jSumStats$statistics[,1:2]
jSumStats_Q = jSumStats$quantiles
jSumEff = effectiveSize(jSample)
gc();

# GET R-HATS 
# NOT RUN!
#cat(date(),'GETTING GELMAN-RUBIN STAT (COMPUTATIONALLY EXPENSIVE)...\n')
#jRHat = gelman.diag(jSample, multivariate=FALSE)
#jRHatEsts = jRHat$psrf

### Compose jSum
rn = rownames(jSumStats_MSD)
ro_MSD = match(rn, rownames(jSumStats_MSD))
ro_Q = match(rn, rownames(jSumStats_Q))
ro_eff = match(rn, names(jSumEff))
#ro_rhat = match(rn, rownames(jRHatEsts))

jSum = cbind(
	jSumStats_MSD,
	jSumStats_Q[ro_Q,],
#	jRHatEsts[ro_rhat,], # R-HATS NOT RUN!
	jSumEff[ro_eff]
);

# colnames(jSum)[10] = 'n.eff' 
colnames(jSum)[8] = 'n.eff'  # R-HATS NOT RUN!

endTime = proc.time()[3] - startTime;
sampleToSumTime = endTime - sampleEndTime;
gc();

cat(date(),'STAT-PROCESSING TIME:',sampleToSumTime,'\n')
