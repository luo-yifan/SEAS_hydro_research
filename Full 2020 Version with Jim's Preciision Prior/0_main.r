####################################################################################################
### R Script to produce L2SWBM estimates Great Lakes water balance
### Author: Joeseph Smith - NOAA CIGLR
###         Hong Xuan DO  - SEAS U-M
### Date:   Jan 01, 2020
####################################################################################################

library(rjags)

##### SET WORKING DIRECTORY
rootDir = "D:\\Desktop\\WaterBalance\\Full 2020 Version"
setwd(rootDir);

### GET FUNCTIONS, GLOBAL (GREAT LAKES WIDE) VARIABLES AND CONFIGURATION
source('1a_L2SWBM_config.r');
cat(date(),'Configuration - Done!\n')

### GET DATA AND STORE AS R OBJECTS
source('2_get_data.r');
cat(date(),'Data load - Done!\n')

### DERIVE PRIOR DISTRIBUTION INFORMATION FROM INPUT
source('3_proc_prior.r');
cat(date(),'Prior distribution estimate - Done!\n')

### CREATING OUTPUT FOLDERS
#CREATE PRIMARY OUTPUT DIRECTORY
if(!file.exists(file.path(rootDir,"output"))){
	dir.create(file.path(rootDir,"output"));
}

#CREAT SUB-DIRECTORY TO STORE L2SWBM OUTPUT FOR CURRENT SIMULATION
if(!file.exists(file.path(rootDir,"output", modelName))){
	dir.create(file.path(rootDir,"output", modelName));
}

#CREATE FOLDER TO STORE PLOTS OF DATA PREVIEW 
if(!file.exists(file.path(rootDir,"output",modelName,"plot_preview"))){
	dir.create(file.path(rootDir,"output",modelName,"plot_preview"));
}

#CREATE FOLDER TO STORE PRIOR DISTRIBUTIONS
if(!file.exists(file.path(rootDir,"output",modelName,"plot_prior"))){
	dir.create(file.path(rootDir,"output",modelName,"plot_prior"));
}

#CREATE FOLDER TO STORE PLOTS OF POSTERIOR INFERENCE 
if(!file.exists(file.path(rootDir,"output",modelName,"plot_posterior"))){
	dir.create(file.path(rootDir,"output",modelName,"plot_posterior"));
}

#CREATE FOLDER TO STORE TIME-SERIES OF POSTERIOR INFERENCE 
if(!file.exists(file.path(rootDir,"output",modelName,"ts_posterior"))){
	dir.create(file.path(rootDir,"output",modelName,"ts_posterior"));
}

### PREVIEW PLOTS FOR AVAILABLE DATA 
setwd(file.path(rootDir,"output",modelName,"plot_preview"))
source('../../../4_tsPlotter_Preview.r')
cat(date(),'Previewed Time series - Done!\n')

### WRITE CONFIG FILE FOR CHECKING
setwd(file.path(rootDir,"output", modelName))
write.table(config, paste(modelName,'_config.csv', sep=''), sep=',', row.names=TRUE, col.names=c("Config", "Value"))

### PLOT PRIOR DISTRIBUTION
setwd(file.path(rootDir,"output", modelName, "plot_prior"))
source('../../../5_priorPlotter.r')
cat(date(),'Prior Distribution Plot - Done!\n')

### WRITE THE RJAGS MODEL
setwd(file.path(rootDir,"output", modelName))
source('../../6_proc_model_write_JLC_PrecMod.r');

### RUN THE MODEL
# Note1: use five chains in parallels
# Note2: RAM increase exponentially with no. of chains
cat(date(),'Model running !\n')
source('../../7_proc_model_run.r');

### CHECK-POINT - SAVE WORKSPACE 
# Note: usually time consuming! 
cat(date(),'SAVING WORKSPACE... \n')
save.image(paste(modelName,'.RData', sep=''));
cat(date(),'Workspace saved!\n')

### PLOTTING POSTERIOR
cat(date(),'Posterior inference plotting...\n')
setwd(file.path(rootDir,"output",modelName,"plot_posterior"))
source('../../../8_post_tsPlotter.r');

### WRITE CALCULATED STATS
setwd(file.path(rootDir,"output",modelName))
cat(date(),'Writing posterior stats...\n')
if(checkModel){
	source('../../9_statsGen.r');
}

### WRITE POSTERIOR ESTIMATE TO FILES
setwd(file.path(rootDir,"output",modelName,"ts_posterior"))
source('../../../10_dataGen.r')

#### END! ####