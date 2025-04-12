################################## SET UP ######################################
################################## SET UP ######################################
################################## SET UP ######################################

library(dplyr)
library(readxl)
library(writexl)
library(tidyr)
library(stringr)
library(rriskDistributions)
library(triangle)
library(xlsx)

setwd('/Users/XXX')

set.seed(123)

################################# IMPORT DATA ##################################

## avertable burden
  # baseline (VE = 70%)
  burden = read_excel('Code/InputData/burdenestimates_kumar2023_seventy.xlsx') %>% select(-`...1`)
  # VE = 50%
  burden_50 = read_excel('Code/InputData/burdenestimates_kumar2023_fifty.xlsx') %>% select(-`...1`)

## hospital cost per day
  hosp_cost = read_excel('Code/InputData/hosp_cost.xlsx')

## PPP adjustments
  PPP = read_excel('Code/InputData/PPP.xlsx') %>% 
    mutate(PPP_2010 = as.numeric(PPP_2010),
          PPP_2021 = as.numeric(PPP_2021))
  
  LCU_conversion = read_excel('Code/InputData/LCU_conversion.xlsx') %>% 
    mutate(LCU_USD = as.numeric(LCU_USD))
  # LCU_USD gives the amount of local currency (LCU) in USD

## GNI per capita
  GNIpc = read_excel('Code/InputData/GNIpc_2021usd.xlsx') %>%
    mutate(GNIpc_2021 = as.numeric(GNIpc_2021))
  
## OOP health expenditures
  OOP = read_excel('Code/InputData/oop.xlsx') 


################################ CLEAN DATA ####################################
  
## create space to hold results
  med_exp = list()
  med_exp[[1]] = burden
  med_exp[[2]] = burden_50
  
  names = c("baseline", "fifty")
  names(med_exp) = names
  
## remove unneeded columns, then add hospital costs and adjust for PPP
  for (i in names) {
    med_exp[[i]] = med_exp[[i]] %>%
      select(-avertable_deaths, -avertable_deaths_lower, -avertable_deaths_upper,
             -ampicillin_deaths_avertable, -ampicillin_deaths_avertable_lower, -ampicillin_deaths_avertable_upper,
             -gentamicin_deaths_avertable, -gentamicin_deaths_avertable_lower, -gentamicin_deaths_avertable_upper,
             -ceftazidime_deaths_avertable, -ceftazidime_deaths_avertable_lower, -ceftazidime_deaths_avertable_upper,
             -meropenem_deaths_avertable_2030, -meropenem_deaths_avertable_2030_upper, -meropenem_deaths_avertable_2030_lower,
             -meropenem_deaths_avertable, -meropenem_deaths_avertable_lower, -meropenem_deaths_avertable_upper,
             -percent_alldeaths_avertable, -percent_alldeaths_avertable_lower, -percent_alldeaths_avertable_upper,) %>%
      left_join(hosp_cost, by = c("code", "country_name")) %>%
      left_join(PPP, by = "country_name") %>%
      left_join(LCU_conversion, by = "iso3") %>%
      # replace Cuba and Venezuela with avg of the region
      group_by(region) %>%
      mutate(LCU_USD = ifelse(country_name == "Cuba" | country_name == "Venezuela", 
                              mean(LCU_USD, na.rm = TRUE), LCU_USD)) %>%
      ungroup() %>%
      # convert to 2021 US$ by first converting to 2021 I$ (mult by PPP_2021/PPP_2010) 
      # then convert to 2021 US$ (mult by PPP_2021 / LCU_USD)
      # have to manually adjust Sierra Leone, divide by 1000
      mutate(hosp_cost_mean = if_else(code == "sl", 
                                      hosp_cost_mean * (PPP_2021 / PPP_2010) * (PPP_2021 / LCU_USD) / 1000, 
                                      hosp_cost_mean * (PPP_2021 / PPP_2010) * (PPP_2021 / LCU_USD)),
             hosp_cost_sd = if_else(code == "sl", 
                                    hosp_cost_sd * (PPP_2021 / PPP_2010) * (PPP_2021 / LCU_USD) / 1000, 
                                    hosp_cost_sd * (PPP_2021 / PPP_2010) * (PPP_2021 / LCU_USD))) %>%
      select(-PPP_2010, -LCU_USD, -PPP_2021) 
  }
  
## add antibiotic tx costs and adjust for PPP 
  # make df with abx costs
  abx_costs = data.frame(region = c("AFR", "AMR", "EMR", "EUR", "SEAR", "WPR"),
                         ampicillin = c(3.7, 2.26, 2.98, 2.98, 2.98, 2.98),
                         gentamicin = c(0.54, 13.63, 1.29, 3.24, 0.4, 0.39),
                         ceftriaxone = c(24.08, 21.98, 45.95, 25.06, 6.98, 26.30),
                         meropenem = c(10.95, 6.92, 10.95, 10.95, 14.97, 10.95))
  
  # add abx costs ranges
  for (i in names) {
  med_exp[[i]] = med_exp[[i]] %>%
    left_join(abx_costs, by = "region") %>%
    mutate(ampicillin_lower = ampicillin * 0.75,
           ampicillin_upper = ampicillin * 1.25,
           gentamicin_lower = gentamicin * 0.75,
           gentamicin_upper = gentamicin * 1.25,
           ceftriaxone_lower = ceftriaxone * 0.75,
           ceftriaxone_upper = ceftriaxone * 1.25,
           meropenem_lower = meropenem * 0.75,
           meropenem_upper = meropenem * 1.25) 
  }
  
######################### CALCULATE MED EXPENDITURES ###########################
######################### CALCULATE MED EXPENDITURES ###########################
######################### CALCULATE MED EXPENDITURES ###########################
  
## run monte carlo simulations
    nsim = 100000 # number of simulations to run per country
    
    # loop through all quantities of interest
    for (i in names) {
      
      # assign current df
      df = med_exp[[i]]
      
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
        
        # get params for burden distribution gentamicin resistance
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
        
        # sample abx costs
        ampgen_sample = runif(nsim, min = df$ampicillin_lower[j], max = df$ampicillin_upper[j]) + 
                     runif(nsim, min = df$gentamicin_lower[j], max = df$gentamicin_upper[j])
        
        cef_sample = runif(nsim, min = df$ceftriaxone_lower[j], max = df$ceftriaxone_upper[j])
        
        mero_sample = runif(nsim, min = df$meropenem_lower[j], max = df$meropenem_upper[j])
        
        # sample hospital LOS
        hoslos_sample = rgamma(nsim, 
                               shape = ((7.9174)^2 / (1.9182)^2), 
                               scale = ((1.9182)^2 / 7.9174))
        
        hoslos_sample_res = rgamma(nsim, 
                                   shape = ((10.1)^2 / (2 * 6.82 / 9.219544)^2), 
                                   scale = ((2 * 6.82 / 9.219544)^2 / 10.1))
        
        # sample hospital cost
        hoscost_sample = rgamma(nsim, 
                                shape = ((df$hosp_cost_mean[j])^2 / (df$hosp_cost_sd[j])^2), 
                                scale = ((df$hosp_cost_sd[j])^2 / df$hosp_cost_mean[j]))
        
        # calculate the medical expenditures for the country
        medical_exp = burden_sample * (ampgen_sample + hoslos_sample * hoscost_sample)
        medical_exp_amp = burden_sample_amp * (ampgen_sample + cef_sample + hoslos_sample_res * hoscost_sample)
        medical_exp_gen = burden_sample_gen * (ampgen_sample + cef_sample + hoslos_sample_res * hoscost_sample)
        medical_exp_ceft = burden_sample_ceft * (ampgen_sample + cef_sample + mero_sample + hoslos_sample_res * hoscost_sample)
        medical_exp_mero = burden_sample_mero * (ampgen_sample + cef_sample + mero_sample + hoslos_sample_res * hoscost_sample)
       
        medical_exp_fl = (ampgen_sample + hoslos_sample * hoscost_sample) # first line tx, per case
        medical_exp_sl = (ampgen_sample + cef_sample + hoslos_sample_res * hoscost_sample) # second line tx, per case
        medical_exp_tl = (ampgen_sample + cef_sample + mero_sample + hoslos_sample_res * hoscost_sample) # third line tx, per case
        
        # calculate the ratio of abx costs of all costs
        ratio_fl = ampgen_sample
        ratio_sl = (ampgen_sample + cef_sample)
        ratio_tl = (ampgen_sample + cef_sample + mero_sample) 
        
        # save results for each country
          # per case
          df$ratio_fl[j] = mean(ratio_fl)
          df$ratio_sl[j] = mean(ratio_sl)
          df$ratio_tl[j] = mean(ratio_tl)
          df$medical_exp_fl[j] = mean(medical_exp_fl)
          df$medical_exp_sl[j] = mean(medical_exp_sl)
          df$medical_exp_tl[j] = mean(medical_exp_tl)
        
          # total
          df$medical_expenditures[j] = mean(medical_exp)
          df$medical_expenditures_lower[j] = quantile(medical_exp, 0.025)
          df$medical_expenditures_upper[j] = quantile(medical_exp, 0.975)
          # amp only resistance
          df$medical_expenditures_amp[j] = mean(medical_exp_amp)
          df$medical_expenditures_amp_lower[j] = quantile(medical_exp_amp, 0.025)
          df$medical_expenditures_amp_upper[j] = quantile(medical_exp_amp, 0.975)
          # gen resistance
          df$medical_expenditures_gen[j] = mean(medical_exp_gen)
          df$medical_expenditures_gen_lower[j] = quantile(medical_exp_gen, 0.025)
          df$medical_expenditures_gen_upper[j] = quantile(medical_exp_gen, 0.975)
          # ceftazidime resistance
          df$medical_expenditures_ceft[j] = mean(medical_exp_ceft)
          df$medical_expenditures_ceft_lower[j] = quantile(medical_exp_ceft, 0.025)
          df$medical_expenditures_ceft_upper[j] = quantile(medical_exp_ceft, 0.975)
          # meropenem resistance
          df$medical_expenditures_mero[j] = mean(medical_exp_mero)
          df$medical_expenditures_mero_lower[j] = quantile(medical_exp_mero, 0.025)
          df$medical_expenditures_mero_upper[j] = quantile(medical_exp_mero, 0.975)
      }
      
      # select relevant fields to save
      df = df %>%
        select(iso3, country_name, region, income_group, avertable_cases, starts_with("medical"), starts_with("ratio"))
      
      # save results for each quantity of interest
      med_exp[[i]] = df
    }
    
## save results as excel files
    # loop through list
    for (i in 1:length(med_exp)) {
      # save
      file_name = paste0(names(med_exp)[i], "_medexp.xlsx")
      write.xlsx(med_exp[[i]], file = paste0('Code/OtherData/', file_name))
    }
    
######################### CALCULATE CATASTROPHIC EXP ###########################
######################### CALCULATE CATASTROPHIC EXP ###########################
######################### CALCULATE CATASTROPHIC EXP ###########################
    
# question: would the average averted case of neonatal sepsis have been catastrophic 
# for the average person in country X?
  
# catastrophic expenditures at 10% threshold
  catastrophic_ex_10 = med_exp
  for (i in names) {
    catastrophic_ex_10[[i]] = med_exp[[i]] %>%
      select(-starts_with("medical_expenditures"), -starts_with("ratio"), -avertable_cases) %>%
      left_join(OOP, by = "country_name") %>%
      left_join(GNIpc, by = "iso3") %>%
      mutate(across(starts_with("medical"), ~ . * (oop / 100)),
             GNIpc_2021_10 = GNIpc_2021 * 0.1,
             across(starts_with("medical"), ~ GNIpc_2021_10 - .)) %>%
      filter(medical_exp_fl < 0 | medical_exp_sl < 0 | medical_exp_tl < 0)
  }
  
  for (i in 1:length(catastrophic_ex_10)) {
    # save
    file_name = paste0(names(catastrophic_ex_10)[i], "_cat10exp.xlsx")
    write.xlsx(catastrophic_ex_10[[i]], file = paste0('Code/OtherData/', file_name))
  }

  # catastrophic expenditures at 25% threshold
  catastrophic_ex_25 = med_exp
  for (i in names) {
    catastrophic_ex_25[[i]] = med_exp[[i]] %>%
      select(-starts_with("medical_expenditures"), -starts_with("ratio"), -avertable_cases) %>%
      left_join(OOP, by = "country_name") %>%
      left_join(GNIpc, by = "iso3") %>%
      mutate(across(starts_with("medical"), ~ . * (oop / 100)),
             GNIpc_2021 = GNIpc_2021 * 0.25,
             across(starts_with("medical"), ~ GNIpc_2021 - .)) %>%
      filter(medical_exp_fl < 0 | medical_exp_sl < 0 | medical_exp_tl < 0)
  }
  
  for (i in 1:length(catastrophic_ex_25)) {
    # save
    file_name = paste0(names(catastrophic_ex_25)[i], "_cat25exp.xlsx")
    write.xlsx(catastrophic_ex_25[[i]], file = paste0('Code/OtherData/', file_name))
  }
  
