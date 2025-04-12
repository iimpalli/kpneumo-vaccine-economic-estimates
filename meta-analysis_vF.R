################################## SET UP ######################################

library(metafor)

setwd('/Users/XXX')

########################## HOSPITAL LENGTH OF STAY #############################

# to combine the two studies identified in Text S1, they need to be converted to a common format.

# study 1: Oumer et al.2021
  # given information
  iqr_study1 = 10 - 5 # interquartile range
  n_study1 = 631 # sample size of neonates
  mean_study1 = 7 # assume median = mean
  # calculate standard error
  stdev_study1 = iqr_study1 / 1.35
  sterr_study1 = stdev_study1 / sqrt(n_study1)

# study 2: Sisay et al. 2022
  # given information
  n_study2 = 206 # sample size of neonates
  mean_study2 = 8.92 # assume median = mean
  stdev_study2 = 6.18 # standard deviation 
  # calculate standard error
  sterr_study2 = stdev_study2 / sqrt(n_study2)  
  
# random effects meta-analysis
  # put studies in a df
  meta_df = data.frame(
    study = c("Oumer", "Sisay"),
    mean = c(mean_study1, mean_study2),
    sterr = c(sterr_study1, sterr_study2),
    n = c(n_study1, n_study2)
  )
  # run meta-analysis
  meta_result = rma(data = meta_df, 
                    yi = mean, 
                    sei = sterr, 
                    method = "REML")
  # results
  summary(meta_result)
  # weighting
  weights(meta_result)
  # forest plot
  forest(meta_result)
  
  
