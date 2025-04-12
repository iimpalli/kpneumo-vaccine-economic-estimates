################################## SET UP ######################################
################################## SET UP ######################################
################################## SET UP ######################################

library(dplyr)
library(readxl)
library(tidyr)
library(stringr)
library(rriskDistributions)
library(triangle)
library(xlsx)


setwd('/Users/XXX')

set.seed(123)

################################# IMPORT DATA ##################################
# avertable burden
  # baseline (VE = 70%)
  burden_x = read_excel('Code/InputData/burdenestimates_kumar2023_seventy.xlsx') %>% select(-`...1`)
  # VE = 50%
  burden_50_x = read_excel('Code/InputData/burdenestimates_kumar2023_fifty.xlsx') %>% select(-`...1`)

## GNI per capita
  GNIpc = read_excel('Code/InputData/GNIpc_2021usd.xlsx') %>%
    mutate(GNIpc_2021 = as.numeric(GNIpc_2021))

## life expectancy
  lifeexpectancy = read_excel('Code/InputData/life_expectancy.xlsx') %>%
    mutate(life_exp = as.numeric(life_exp))

################### CALCULATE PRODUCTIVITY LOSS DUE TO HOSP #####################
################### CALCULATE PRODUCTIVITY LOSS DUE TO HOSP #####################
################### CALCULATE PRODUCTIVITY LOSS DUE TO HOSP #####################

## create space to hold results
  prod_loss_hosp = list()
  prod_loss_hosp[[1]] = burden_x
  prod_loss_hosp[[2]] = burden_50_x
  
  names = c("baseline", "fifty")
  names(prod_loss_hosp) = names
  
## edit df of cases
  # combine avertable burden dfs with GNI information
  for (i in names) {
  prod_loss_hosp[[i]] = prod_loss_hosp[[i]] %>%
    select(country_name, region, iso3, avertable_cases, avertable_cases_lower, avertable_cases_upper,
           amp_cases_avertable, amp_cases_avertable_lower, amp_cases_avertable_upper,
           gen_cases_avertable, gen_cases_avertable_lower, gen_cases_avertable_upper,
           ceftazidime_cases_avertable, ceftazidime_cases_avertable_lower, ceftazidime_cases_avertable_upper,
           meropenem_cases_avertable, meropenem_cases_avertable_lower, meropenem_cases_avertable_upper) %>%
    left_join(GNIpc, by = "iso3") %>%
    mutate(GNIpc_2021 = as.numeric(GNIpc_2021),
           GNIpc_lower = GNIpc_2021 * 0.95,
           GNIpc_upper = GNIpc_2021 * 1.05)
  }

## run monte carlo simulations
    nsim = 100000 # number of simulations to run per country
   
    # loop through all quantities of interest
    for (i in names) {
      
      # assign current df
      df = prod_loss_hosp[[i]]

      # loop through all countries in each df
      for (j in 1:nrow(df)) {
        
        # get params for burden distribution, total
        pars = get.gamma.par(q = c(df$avertable_cases_lower[j], 
                                   df$avertable_cases[j], 
                                   df$avertable_cases_upper[j]), 
                             plot = FALSE, show.output = FALSE)
        if (is.na(pars[1])) {burden_sample = rtriangle(nsim, 
                                                       a = df$avertable_cases_lower[j], 
                                                       b = df$avertable_cases_upper[j], 
                                                       c = df$avertable_cases[j])}
        else {burden_sample = rgamma(nsim, shape = pars[1], rate = pars[2])}
        
        # get params for burden distribution, ampicillin resistance
        pars = get.gamma.par(q = c(df$amp_cases_avertable_lower[j], 
                                   df$amp_cases_avertable[j], 
                                   df$amp_cases_avertable_upper[j]), 
                             plot = FALSE, show.output = FALSE)
        if (is.na(pars[1])) {burden_sample_amp = rtriangle(nsim, 
                                                              a = df$amp_cases_avertable_lower[j], 
                                                              b = df$amp_cases_avertable_upper[j], 
                                                              c = df$amp_cases_avertable[j])}
        else {burden_sample_amp = rgamma(nsim, shape = pars[1], rate = pars[2])}
        
        # get params for burden distribution,gentamicin resistance
        pars = get.gamma.par(q = c(df$gen_cases_avertable_lower[j], 
                                   df$gen_cases_avertable[j], 
                                   df$gen_cases_avertable_upper[j]), 
                             plot = FALSE, show.output = FALSE)
        if (is.na(pars[1])) {burden_sample_gen = rtriangle(nsim, 
                                                              a = df$gen_cases_avertable_lower[j], 
                                                              b = df$gen_cases_avertable_upper[j], 
                                                              c = df$gen_cases_avertable[j])}
        else {burden_sample_gen = rgamma(nsim, shape = pars[1], rate = pars[2])}
        
        # get params for burden distribution, ceftazidime resistance
        pars = get.gamma.par(q = c(df$ceftazidime_cases_avertable_lower[j], 
                                   df$ceftazidime_cases_avertable[j], 
                                   df$ceftazidime_cases_avertable_upper[j]), 
                             plot = FALSE, show.output = FALSE)
        if (is.na(pars[1])) {burden_sample_ceft = rtriangle(nsim, 
                                                            a = df$ceftazidime_cases_avertable_lower[j], 
                                                            b = df$ceftazidime_cases_avertable_upper[j], 
                                                            c = df$ceftazidime_cases_avertable[j])}
        else {burden_sample_ceft = rgamma(nsim, shape = pars[1], rate = pars[2])}
        
        # get params for burden distribution, meropenem resistance
        pars = get.gamma.par(q = c(df$meropenem_cases_avertable_lower[j], 
                                   df$meropenem_cases_avertable[j], 
                                   df$meropenem_cases_avertable_upper[j]), 
                             plot = FALSE, show.output = FALSE)
        if (is.na(pars[1])) {burden_sample_mero = rtriangle(nsim, 
                                                            a = df$meropenem_cases_avertable_lower[j], 
                                                            b = df$meropenem_cases_avertable_upper[j], 
                                                            c = df$meropenem_cases_avertable[j])}
        else {burden_sample_mero = rgamma(nsim, shape = pars[1], rate = pars[2])}
      
        
        # sample hospital LOS
        hoslos_sample = rgamma(nsim, 
                               shape = ((7.9174)^2 / (1.9182)^2), 
                               scale = ((1.9182)^2 / 7.9174))
        
        
        hoslos_sample_res = rgamma(nsim, 
                                   shape = ((10.1)^2 / (2 * 6.82 / 9.219544)^2), 
                                   scale = ((2 * 6.82 / 9.219544)^2 / 10.1))
        
        # sample income loss / day
        prodlossday_sample = runif(nsim, min = df$GNIpc_lower[j], max = df$GNIpc_upper[j]) / 365
        
       
        # calculate the medical expenditures for the country
        prodloss_hosp = burden_sample * hoslos_sample * prodlossday_sample
        prodloss_hosp_amp = burden_sample_amp * hoslos_sample_res * prodlossday_sample
        prodloss_hosp_gen = burden_sample_gen * hoslos_sample_res * prodlossday_sample
        prodloss_hosp_ceft = burden_sample_ceft * hoslos_sample_res * prodlossday_sample
        prodloss_hosp_mero = burden_sample_mero * hoslos_sample_res * prodlossday_sample
        
        
        # save results for each country
          # total
          df$prodloss_hosp[j] = mean(prodloss_hosp)
          df$prodloss_hosp_lower[j] = quantile(prodloss_hosp, 0.025)
          df$prodloss_hosp_upper[j] = quantile(prodloss_hosp, 0.975)
          # amp only resistance
          df$prodloss_hosp_amp[j] = mean(prodloss_hosp_amp)
          df$prodloss_hosp_amp_lower[j] = quantile(prodloss_hosp_amp, 0.025)
          df$prodloss_hosp_amp_upper[j] = quantile(prodloss_hosp_amp, 0.975)
          # gen resistance
          df$prodloss_hosp_gen[j] = mean(prodloss_hosp_gen)
          df$prodloss_hosp_gen_lower[j] = quantile(prodloss_hosp_gen, 0.025)
          df$prodloss_hosp_gen_upper[j] = quantile(prodloss_hosp_gen, 0.975)
          # ceftazidime resistance
          df$prodloss_hosp_ceft[j] = mean(prodloss_hosp_ceft)
          df$prodloss_hosp_ceft_lower[j] = quantile(prodloss_hosp_ceft, 0.025)
          df$prodloss_hosp_ceft_upper[j] = quantile(prodloss_hosp_ceft, 0.975)
          # meropenem resistance
          df$prodloss_hosp_mero[j] = mean(prodloss_hosp_mero)
          df$prodloss_hosp_mero_lower[j] = quantile(prodloss_hosp_mero, 0.025)
          df$prodloss_hosp_mero_upper[j] = quantile(prodloss_hosp_mero, 0.975)
      }
      
      # filter out unneeded rows
      df = df %>%
        select(iso3, country_name, region, avertable_cases, starts_with("prod"))
      
      # save results for each quantity of interest
      prod_loss_hosp[[i]] = df
    }
    
## save results as excel files
  # loop through list
  for (i in 1:length(prod_loss_hosp)) {
    # save
    file_name = paste0(names(prod_loss_hosp)[i], "_prodloss_hosp.xlsx")
    write.xlsx(prod_loss_hosp[[i]], file = paste0('Code/OtherData/', file_name))
  }
