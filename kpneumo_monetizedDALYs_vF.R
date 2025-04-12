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

################################ CALCULATE YLL #################################
################################ CALCULATE YLL #################################
################################ CALCULATE YLL #################################

## create space to hold results
  YLL = list()
  YLL[[1]] = burden_x
  YLL[[2]] = burden_50_x
  
  names = c("baseline", "fifty")
  names(YLL) = names
  
## edit dfs for YLL calculations
  # add the discount rate and life expectancy
  for (i in names) {
    YLL[[i]] = YLL[[i]] %>%
      select(iso3, country_name, region, income_group, avertable_deaths, avertable_deaths_lower, avertable_deaths_upper,
             meropenem_deaths_avertable, meropenem_deaths_avertable_lower, meropenem_deaths_avertable_upper,
             ceftazidime_deaths_avertable, ceftazidime_deaths_avertable_lower, ceftazidime_deaths_avertable_upper,
             ampicillin_deaths_avertable, ampicillin_deaths_avertable_lower, ampicillin_deaths_avertable_upper,
             gentamicin_deaths_avertable, gentamicin_deaths_avertable_lower, gentamicin_deaths_avertable_upper) %>% 
      left_join(lifeexpectancy, by ="iso3") %>%
      mutate(rate = .03,
             rate_alt = case_when(income_group == "Upper middle income" ~ .04,
                                  income_group == "Lower middle income" ~ .05,
                                  income_group == "Low income" ~ .05)) %>%
      mutate(life_exp_upper = life_exp * 1.03,
             life_exp_lower = life_exp * 0.97)
  }

## run monte carlo simulations
    nsim = 100000 # number of simulations to run per country
    
    # loop through all quantities of interest
    for (i in names) {
      
      # assign current df
      df = YLL[[i]]
      
      # loop through all countries in each df
      for (j in 1:nrow(df)) {
        
        # get params for burden distribution, total
        pars = get.gamma.par(q = c(df$avertable_deaths_lower[j], 
                                   df$avertable_deaths[j], 
                                   df$avertable_deaths_upper[j]), 
                             plot = FALSE, show.output = FALSE)
        if (is.na(pars[1])) {burden_sample = rtriangle(nsim, 
                                                       a = df$avertable_deaths_lower[j], 
                                                       b = df$avertable_deaths_upper[j], 
                                                       c = df$avertable_deaths[j])}
        else {burden_sample = rgamma(nsim, shape = pars[1], rate = pars[2])}
        
        # get params for burden distribution, ampicillin resistance
        pars = get.gamma.par(q = c(df$ampicillin_deaths_avertable_lower[j], 
                                   df$ampicillin_deaths_avertable[j], 
                                   df$ampicillin_deaths_avertable_upper[j]), 
                             plot = FALSE, show.output = FALSE)
        if (is.na(pars[1])) {burden_sample_amp = rtriangle(nsim, 
                                                            a = df$ampicillin_deaths_avertable_lower[j], 
                                                            b = df$ampicillin_deaths_avertable_upper[j], 
                                                            c = df$ampicillin_deaths_avertable[j])}
        else {burden_sample_amp = rgamma(nsim, shape = pars[1], rate = pars[2])}
        
        # get params for burden distribution, ampicillin & gentamicin resistance
        pars = get.gamma.par(q = c(df$gentamicin_deaths_avertable_lower[j], 
                                   df$gentamicin_deaths_avertable[j], 
                                   df$gentamicin_deaths_avertable_upper[j]), 
                             plot = FALSE, show.output = FALSE)
        if (is.na(pars[1])) {burden_sample_gen = rtriangle(nsim, 
                                                              a = df$gentamicin_deaths_avertable_lower[j], 
                                                              b = df$gentamicin_deaths_avertable_upper[j], 
                                                              c = df$gentamicin_deaths_avertable[j])}
        else {burden_sample_gen = rgamma(nsim, shape = pars[1], rate = pars[2])}
        
        # get params for burden distribution, ceftazidime resistance
        pars = get.gamma.par(q = c(df$ceftazidime_deaths_avertable_lower[j], 
                                   df$ceftazidime_deaths_avertable[j], 
                                   df$ceftazidime_deaths_avertable_upper[j]), 
                             plot = FALSE, show.output = FALSE)
        if (is.na(pars[1])) {burden_sample_ceft = rtriangle(nsim, 
                                                            a = df$ceftazidime_deaths_avertable_lower[j], 
                                                            b = df$ceftazidime_deaths_avertable_upper[j], 
                                                            c = df$ceftazidime_deaths_avertable[j])}
        else {burden_sample_ceft = rgamma(nsim, shape = pars[1], rate = pars[2])}
        
        # get params for burden distribution, meropenem resistance
        pars = get.gamma.par(q = c(df$meropenem_deaths_avertable_lower[j], 
                                   df$meropenem_deaths_avertable[j], 
                                   df$meropenem_deaths_avertable_upper[j]), 
                             plot = FALSE, show.output = FALSE)
        if (is.na(pars[1])) {burden_sample_mero = rtriangle(nsim, 
                                                            a = df$meropenem_deaths_avertable_lower[j], 
                                                            b = df$meropenem_deaths_avertable_upper[j], 
                                                            c = df$meropenem_deaths_avertable[j])}
        else {burden_sample_mero = rgamma(nsim, shape = pars[1], rate = pars[2])}
        
        # sample life expectancy
        lifeexp_sample = runif(nsim, min = df$life_exp_lower[j], max = df$life_exp_upper[j])
 
        # calculate the discounted YLL for each case
        YLL_sample = (1 / df$rate[j]) * (1 - exp(-1 * df$rate[j] * lifeexp_sample))
        
        # calculate the YLL averted in each country
        YLLs = YLL_sample * burden_sample
        YLL_amp = YLL_sample * burden_sample_amp
        YLL_gen = YLL_sample * burden_sample_gen
        YLL_ceft = YLL_sample * burden_sample_ceft
        YLL_mero = YLL_sample * burden_sample_mero
        
        # save results for each country
        # total
        df$YLLs[j] = mean(YLLs)
        df$YLLs_lower[j] = quantile(YLLs, 0.025)
        df$YLLs_upper[j] = quantile(YLLs, 0.975)
        # amp only resistance
        df$YLL_amp[j] = mean(YLL_amp)
        df$YLL_amp_lower[j] = quantile(YLL_amp, 0.025)
        df$YLL_amp_upper[j] = quantile(YLL_amp, 0.975)
        # gen resistance
        df$YLL_gen[j] = mean(YLL_gen)
        df$YLL_gen_lower[j] = quantile(YLL_gen, 0.025)
        df$YLL_gen_upper[j] = quantile(YLL_gen, 0.975)
        # ceftazidime resistance
        df$YLL_ceft[j] = mean(YLL_ceft)
        df$YLL_ceft_lower[j] = quantile(YLL_ceft, 0.025)
        df$YLL_ceft_upper[j] = quantile(YLL_ceft, 0.975)
        # meropenem resistance
        df$YLL_mero[j] = mean(YLL_mero)
        df$YLL_mero_lower[j] = quantile(YLL_mero, 0.025)
        df$YLL_mero_upper[j] = quantile(YLL_mero, 0.975)
      }
      # filter
      df = df %>%
        select(iso3, country_name, region, income_group, starts_with("YLL"))
      
      # save results for each quantity of interest
      YLL[[i]] = df
    }

################################ CALCULATE YLD #################################
################################ CALCULATE YLD #################################
################################ CALCULATE YLD #################################

## find the disability weight per case
    # create df with disability weightings
    # see supplementary text S1 for the below abbreviations of long-term sequelae
    disability_weights = data.frame(lts = c("cp", "scld", "d", "m", "b"),
                                    prevalence = c(.124, .264, .111, .048, .111),
                                    prevalence_25 = c(.111, .222, .041, 0, .056),
                                    prevalence_75 = c(.149, .30, .194, .214, .125),
                                    dw = c(0.63, 0.63, 0.2, 0.03, 0.19),
                                    dw_2.5 = c(0.45, 0.45, 0.13, 0.02, 0.12),
                                    dw_97.5 = c(0.78, 0.78, 0.29, 0.04, 0.26),
                                    avg = c(0, 0, 0, 0, 0))
    # find the average weights through Monte Carlo simulation and replace
    nsim = 100000
    for (i in 1:length(disability_weights)) {
      p_par = get.beta.par(p = c(0.25, 0.5, 0.75), q = c(disability_weights$prevalence_25[i], 
                                                         disability_weights$prevalence[i], 
                                                         disability_weights$prevalence_75[i]), plot = FALSE, show.output = FALSE)
      w_par = get.beta.par(q = c(disability_weights$dw_2.5[i], disability_weights$dw[i], disability_weights$dw_97.5[i]), plot = FALSE, show.output = FALSE)
      
      if (is.na(p_par[1])) {p = disability_weights$prevalence[i]}
      else {p = rbeta(nsim, shape1 = p_par[1], shape2 = p_par[2])}
      
      w = rbeta(nsim, shape1 = w_par[1], shape2 = w_par[2])
      
      disability_weights$avg[i] = mean(p * w)
    }
    # calculate the sum of the averages (weighted through the calculation above), which is the final disability weight
    disability_weight = sum(disability_weights$avg)

    
## create space to hold results
    YLD = list()
    YLD[[1]] = burden_x
    YLD[[2]] = burden_50_x
    
    names(YLD) = names
## edit dfs of YLD
    for (i in names) {
      YLD[[i]] = YLD[[i]] %>%
        # calculate the number of cases which did not result in death
        # equivalent to using the case fatality ratio as calculated previously
        mutate(cases = avertable_cases - avertable_deaths,
               cases_lower = avertable_cases_lower - avertable_deaths_lower,
               cases_upper = avertable_cases_upper - avertable_deaths_upper,
               cases_amp = amp_cases_avertable - ampicillin_deaths_avertable,
               cases_amp_lower = amp_cases_avertable_lower - ampicillin_deaths_avertable_lower,
               cases_amp_upper = amp_cases_avertable_upper - ampicillin_deaths_avertable_upper,
               cases_gen = gen_cases_avertable - gentamicin_deaths_avertable,
               cases_gen_lower = gen_cases_avertable_lower - gentamicin_deaths_avertable_lower,
               cases_gen_upper = gen_cases_avertable_upper - gentamicin_deaths_avertable_upper,
               cases_ceft = ceftazidime_cases_avertable - ceftazidime_deaths_avertable,
               cases_ceft_lower = ceftazidime_cases_avertable_lower - ceftazidime_deaths_avertable_lower,
               cases_ceft_upper = ceftazidime_cases_avertable_upper - ceftazidime_deaths_avertable_upper,
               cases_mero = meropenem_cases_avertable - meropenem_deaths_avertable,
               cases_mero_lower = meropenem_cases_avertable_lower - meropenem_deaths_avertable_lower,
               cases_mero_upper = meropenem_cases_avertable_upper - meropenem_deaths_avertable_upper) %>%
        # keep relevant fields
        select(country_name, iso3, region, income_group,
               cases, cases_lower, cases_upper,
               cases_amp, cases_amp_lower, cases_amp_upper,
               cases_gen, cases_gen_lower, cases_gen_upper,
               cases_ceft, cases_ceft_lower, cases_ceft_upper,
               cases_mero, cases_mero_lower, cases_mero_upper) %>% 
        left_join(lifeexpectancy, by ="iso3") %>%
        mutate(rate = .03,
               rate_alt = case_when(income_group == "Upper middle income" ~ .04,
                                    income_group == "Lower middle income" ~ .05,
                                    income_group == "Low income" ~ .05)) %>%
        mutate(life_exp_upper = life_exp * 1.03,
               life_exp_lower = life_exp * 0.97)
    }

## run monte carlo simulations
  nsim = 100000 # number of simulations to run per country
  
  # loop through all quantities of interest
  for (i in names) {
    
    # assign current df
    df = YLD[[i]]
    
    # loop through all countries in each df
    for (j in 1:nrow(df)) {
      
      # get params for burden distribution, total
      pars = get.gamma.par(q = c(df$cases_lower[j], 
                                 df$cases[j], 
                                 df$cases_upper[j]), 
                           plot = FALSE, show.output = FALSE)
      if (is.na(pars[1])) {burden_sample = rtriangle(nsim, 
                                                     a = df$cases_lower[j], 
                                                     b = df$cases_upper[j], 
                                                     c = df$cases[j])}
      else {burden_sample = rgamma(nsim, shape = pars[1], rate = pars[2])}
      
      # get params for burden distribution, ampicillin resistance
      pars = get.gamma.par(q = c(df$cases_amp_lower[j], 
                                 df$cases_amp[j], 
                                 df$cases_amp_upper[j]), 
                           plot = FALSE, show.output = FALSE)
      if (is.na(pars[1])) {burden_sample_amp = rtriangle(nsim, 
                                                            a = df$cases_amp_lower[j], 
                                                            b = df$cases_amp_upper[j], 
                                                            c = df$cases_amp[j])}
      else {burden_sample_amp = rgamma(nsim, shape = pars[1], rate = pars[2])}
      
      # get params for burden distribution, ampicillin & gentamicin resistance
      pars = get.gamma.par(q = c(df$cases_gen_lower[j], 
                                 df$cases_gen[j], 
                                 df$cases_gen_upper[j]), 
                           plot = FALSE, show.output = FALSE)
      if (is.na(pars[1])) {burden_sample_gen = rtriangle(nsim, 
                                                            a = df$cases_gen_lower[j], 
                                                            b = df$cases_gen_upper[j], 
                                                            c = df$cases_gen[j])}
      else {burden_sample_gen = rgamma(nsim, shape = pars[1], rate = pars[2])}
      
      # get params for burden distribution, ceftazidime resistance
      pars = get.gamma.par(q = c(df$cases_ceft_lower[j], 
                                 df$cases_ceft[j], 
                                 df$cases_ceft_upper[j]), 
                           plot = FALSE, show.output = FALSE)
      if (is.na(pars[1])) {burden_sample_ceft = rtriangle(nsim, 
                                                          a = df$cases_ceft_lower[j], 
                                                          b = df$cases_ceft_upper[j], 
                                                          c = df$cases_ceft[j])}
      else {burden_sample_ceft = rgamma(nsim, shape = pars[1], rate = pars[2])}
      
      # get params for burden distribution, meropenem resistance
      pars = get.gamma.par(q = c(df$cases_mero_lower[j], 
                                 df$cases_mero[j], 
                                 df$cases_mero_upper[j]), 
                           plot = FALSE, show.output = FALSE)
      if (is.na(pars[1])) {burden_sample_mero = rtriangle(nsim, 
                                                          a = df$cases_mero_lower[j], 
                                                          b = df$cases_mero_upper[j], 
                                                          c = df$cases_mero[j])}
      else {burden_sample_mero = rgamma(nsim, shape = pars[1], rate = pars[2])}
      
      # sample life expectancy
      lifeexp_sample = runif(nsim, min = df$life_exp_lower[j], max = df$life_exp_upper[j])
      
      # calculate the discounted YLD for each case
      YLD_sample = (1 / df$rate[j]) * (1 - exp(-1 * df$rate[j] * lifeexp_sample))
      
      # calculate the YLL averted in each country
      YLDs = YLD_sample * burden_sample * disability_weight
      YLD_amp = YLD_sample * burden_sample_amp * disability_weight
      YLD_gen = YLD_sample * burden_sample_gen * disability_weight
      YLD_ceft = YLD_sample * burden_sample_ceft * disability_weight
      YLD_mero = YLD_sample * burden_sample_mero * disability_weight
      
      # save results for each country
        # total
        df$YLDs[j] = mean(YLDs)
        df$YLDs_lower[j] = quantile(YLDs, 0.025)
        df$YLDs_upper[j] = quantile(YLDs, 0.975)
        # amp resistance
        df$YLD_amp[j] = mean(YLD_amp)
        df$YLD_amp_lower[j] = quantile(YLD_amp, 0.025)
        df$YLD_amp_upper[j] = quantile(YLD_amp, 0.975)
        # gen resistance
        df$YLD_gen[j] = mean(YLD_gen)
        df$YLD_gen_lower[j] = quantile(YLD_gen, 0.025)
        df$YLD_gen_upper[j] = quantile(YLD_gen, 0.975)
        # ceftazidime resistance
        df$YLD_ceft[j] = mean(YLD_ceft)
        df$YLD_ceft_lower[j] = quantile(YLD_ceft, 0.025)
        df$YLD_ceft_upper[j] = quantile(YLD_ceft, 0.975)
        # meropenem resistance
        df$YLD_mero[j] = mean(YLD_mero)
        df$YLD_mero_lower[j] = quantile(YLD_mero, 0.025)
        df$YLD_mero_upper[j] = quantile(YLD_mero, 0.975)
    }
    # filter
    df = df %>%
      select(iso3, country_name, region, income_group, starts_with("YLD"), starts_with("life_exp"))
    
    # save results for each quantity of interest
    YLD[[i]] = df
  }
  
############################### CALCULATE DALYs ################################
############################### CALCULATE DALYs ################################
############################### CALCULATE DALYs ################################

## calculate the DALYs
  DALY = YLL
  
  for (i in names) {
    # add YLD
    DALY[[i]] = DALY[[i]] %>%
      left_join(YLD[[i]], by = c("country_name", "iso3", "region", "income_group")) %>%
      # calculate DALYs by summing YLLs and YLDs
      mutate(DALY = YLLs + YLDs,
             DALY_lower = YLLs_lower + YLDs_lower,
             DALY_upper = YLLs_upper + YLDs_upper,
             DALY_amp = YLL_amp + YLD_amp,
             DALY_amp_lower = YLL_amp_lower + YLD_amp_lower,
             DALY_amp_upper = YLL_amp_upper + YLD_amp_upper,
             DALY_gen = YLL_gen + YLD_gen,
             DALY_gen_lower = YLL_gen_lower + YLD_gen_lower,
             DALY_gen_upper = YLL_gen_upper + YLD_gen_upper,
             DALY_ceft = YLL_ceft + YLD_ceft,
             DALY_ceft_lower = YLL_ceft_lower + YLD_ceft_lower,
             DALY_ceft_upper = YLL_ceft_upper + YLD_ceft_upper,
             DALY_mero = YLL_mero + YLD_mero,
             DALY_mero_lower = YLL_mero_lower + YLD_mero_lower,
             DALY_mero_upper = YLL_mero_upper + YLD_mero_upper) %>%
      left_join(GNIpc, by = "iso3") %>%
      mutate(gni_pc_lower = GNIpc_2021 * 0.95,
             gni_pc_upper = GNIpc_2021 * 1.05)
  }
  
  # make table of YLD and YLL breakdown for baseline and send to final results  
  YLL_YLD_baseline = DALY[["baseline"]] %>%
    select(country_name, YLLs, YLLs_lower, YLLs_upper, YLDs, YLDs_lower, YLDs_upper) %>%
    mutate(YLL = paste0(round(YLLs, 2), " (", round(YLLs_lower, 2), "-", round(YLLs_upper, 2), ")"),
           YLD = paste0(round(YLDs, 2), " (", round(YLDs_lower, 2), "-", round(YLDs_upper, 2), ")")) %>%
    select(country_name, YLL, YLD)
  # for supplement
  write.xlsx(YLL_YLD_baseline, file = paste0('Results/YLL_YLDtable.xlsx'))
    
  # keep only desired fields  
    for (i in names) {
    DALY[[i]] = DALY[[i]] %>%
      select(iso3, country_name, region, income_group, starts_with("DALY"), starts_with("life_exp"), starts_with("gni"), GNIpc_2021)
    }

## monetize 
  nsim = 100000 # number of simulations to run per country
  vsl_usa = 11.8e6 # value of a statistical life in the USA, 2021$
  gnipc_usa = 71773 # GNI per capita in USA
  
  for (i in names) {
    # assign current df
    df = DALY[[i]]
      
      # loop through all countries in each df
      for (j in 1:nrow(df)) {
        
        # get params for burden distribution, total
        pars = get.gamma.par(q = c(df$DALY_lower[j], 
                                   df$DALY[j], 
                                   df$DALY_upper[j]), 
                             plot = FALSE, show.output = FALSE)
        if (is.na(pars[1])) {burden_sample = rtriangle(nsim, 
                                                       a = df$DALY_lower[j], 
                                                       b = df$DALY_upper[j], 
                                                       c = df$DALY[j])}
        else {burden_sample = rgamma(nsim, shape = pars[1], rate = pars[2])}
        
        # get params for burden distribution, ampicillin resistance
        pars = get.gamma.par(q = c(df$DALY_amp_lower[j], 
                                   df$DALY_amp[j], 
                                   df$DALY_amp_upper[j]), 
                             plot = FALSE, show.output = FALSE)
        if (is.na(pars[1])) {burden_sample_amp = rtriangle(nsim, 
                                                              a = df$DALY_amp_lower[j], 
                                                              b = df$DALY_amp_upper[j], 
                                                              c = df$DALY_amp[j])}
        else {burden_sample_amp = rgamma(nsim, shape = pars[1], rate = pars[2])}
        
        # get params for burden distribution, ampicillin & gentamicin resistance
        pars = get.gamma.par(q = c(df$DALY_gen_lower[j], 
                                   df$DALY_gen[j], 
                                   df$DALY_gen_upper[j]), 
                             plot = FALSE, show.output = FALSE)
        if (is.na(pars[1])) {burden_sample_gen = rtriangle(nsim, 
                                                              a = df$DALY_gen_lower[j], 
                                                              b = df$DALY_gen_upper[j], 
                                                              c = df$DALY_gen[j])}
        else {burden_sample_gen = rgamma(nsim, shape = pars[1], rate = pars[2])}
        
        # get params for burden distribution, ceftazidime resistance
        pars = get.gamma.par(q = c(df$DALY_ceft_lower[j], 
                                   df$DALY_ceft[j], 
                                   df$DALY_ceft_upper[j]), 
                             plot = FALSE, show.output = FALSE)
        if (is.na(pars[1])) {burden_sample_ceft = rtriangle(nsim, 
                                                            a = df$DALY_ceft_lower[j], 
                                                            b = df$DALY_ceft_upper[j], 
                                                            c = df$DALY_ceft[j])}
        else {burden_sample_ceft = rgamma(nsim, shape = pars[1], rate = pars[2])}
        
        # get params for burden distribution, meropenem resistance
        pars = get.gamma.par(q = c(df$DALY_mero_lower[j], 
                                   df$DALY_mero[j], 
                                   df$DALY_mero_upper[j]), 
                             plot = FALSE, show.output = FALSE)
        if (is.na(pars[1])) {burden_sample_mero = rtriangle(nsim, 
                                                            a = df$DALY_mero_lower[j], 
                                                            b = df$DALY_mero_upper[j], 
                                                            c = df$DALY_mero[j])}
        else {burden_sample_mero = rgamma(nsim, shape = pars[1], rate = pars[2])}
        
        # sample life expectancy
        lifeexp_sample = runif(nsim, min = df$life_exp_lower[j], max = df$life_exp_upper[j])
        
        # sample GNI per capita
        gni_sample = runif(nsim, min = df$gni_pc_lower[j], max = df$gni_pc_upper[j])
        
        # calculate the distribution of VSLY in the country
        VSLY_sample = vsl_usa * ((gni_sample / gnipc_usa) ^ 1.5) / lifeexp_sample
        
        # adjust for values which are too low (lower than 20x GNI per capita)
        for (k in 1:length(VSLY_sample)) {
          if (VSLY_sample[k] * lifeexp_sample[k] < 20 * df$GNIpc_2021[j]) {
            VSLY_sample[k] = 20 * df$GNIpc_2021[j] / lifeexp_sample[k]
          }
        }
        
        # calculate the monetized DALYs averted in each country
        DALY_sample = VSLY_sample * burden_sample 
        DALY_amp_sample = VSLY_sample * burden_sample_amp
        DALY_gen_sample = VSLY_sample * burden_sample_gen
        DALY_ceft_sample = VSLY_sample * burden_sample_ceft
        DALY_mero_sample = VSLY_sample * burden_sample_mero
        
        # save results for each country
        df$VSLY[j] = mean(VSLY_sample)
          # total
          df$monet_DALY[j] = mean(DALY_sample)
          df$monet_DALY_lower[j] = quantile(DALY_sample, 0.025)
          df$monet_DALY_upper[j] = quantile(DALY_sample, 0.975)
          # amp only resistance
          df$monet_DALY_amp[j] = mean(DALY_amp_sample)
          df$monet_DALY_amp_lower[j] = quantile(DALY_amp_sample, 0.025)
          df$monet_DALY_amp_upper[j] = quantile(DALY_amp_sample, 0.975)
          # gen resistance
          df$monet_DALY_gen[j] = mean(DALY_gen_sample)
          df$monet_DALY_gen_lower[j] = quantile(DALY_gen_sample, 0.025)
          df$monet_DALY_gen_upper[j] = quantile(DALY_gen_sample, 0.975)
          # ceftazidime resistance
          df$monet_DALY_ceft[j] = mean(DALY_ceft_sample)
          df$monet_DALY_ceft_lower[j] = quantile(DALY_ceft_sample, 0.025)
          df$monet_DALY_ceft_upper[j] = quantile(DALY_ceft_sample, 0.975)
          # meropenem resistance
          df$monet_DALY_mero[j] = mean(DALY_mero_sample)
          df$monet_DALY_mero_lower[j] = quantile(DALY_mero_sample, 0.025)
          df$monet_DALY_mero_upper[j] = quantile(DALY_mero_sample, 0.975)
    }
    # filter
    df = df %>%
      select(iso3, country_name, region, income_group, starts_with("mone"), VSLY)
    
    # save results for each quantity of interest
    DALY[[i]] = df
  }
  
  ## save results as excel files
    # loop through list
    for (i in 1:length(DALY)) {
      # save
      file_name = paste0(names(DALY)[i], "_monetizedDALYs.xlsx")
      write.xlsx(DALY[[i]], file = paste0('Code/OtherData/', file_name))
    }
  
      
