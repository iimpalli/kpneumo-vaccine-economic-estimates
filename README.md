# kpneumo-vaccine-economic-estimates

This repository contains all of the input data and code for the manuscript "Economic Impact of a Maternal Klebsiella pneumoniae Vaccine: Estimates for 107 Low- and Middle-Income Countries" by Impalli et al.

Some notes:
meta-analysis_vF.R was used to calculate the parameter distribution for the length of hospital stay, and the result was hard coded into kpneumo_medexp_vF.R.

The files should be run in the following order:
  1. kpneumo_monetizedDALYs_vF.R
  2. kpneumo_medexp_vF.R
  3. kpneumo_productivityloss_vF.R
  4. kpneumo_societalcost_vF.R
  5. Results/Figures
