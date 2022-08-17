This code is a modified version of "Lake Chilwa" code for "Lake Victoria"


- Victoria_Model.bug.r:          This R script is the core model of the L2SWBM for Lake Victoria. (Note that you do not need to run this file independently, you just need to run the two other files below) 
- 0_main_l2swbm_v5.R:          This R script loads the data, estimates the prior distributions, and executes the model defined by Victoria_Model.bug.r script. 
- 1_Closure_TimeSeriesPlots.R: This R script is used to post-process outputs from L2SWBM simulations. 
- LakeVictoria:              This excel file contains all datasets (Precip - p; Runoff - r; Evap - e; Water level - wl) synthesized by multiple sources.


To run the code please follow these steps:


   1-First open the "0_main_l2swbm_v5" file in R Studio and change the directory of lines 6 and 226 into the current directory of the code in your system.
   2-second in R studio import the excel file "LakeVictoria" before running.
   3-Then run the "0_main_l2swbm_v5" file.
   4-After that you can run the "1_Closure_TimeSeriesPlots" file and get the results and plots.
