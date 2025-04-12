################################## SET UP ######################################
################################## SET UP ######################################
################################## SET UP ######################################

library(dplyr)
library(readxl)
library(writexl)
library(tidyr)
library(stringr)
library(xlsx)
library(rriskDistributions)
library(triangle)

setwd('/Users/isabellaimpalli/Library/CloudStorage/OneDrive-CenterforDiseaseDynamics,Economics&Policy/AMR Vaccines CDC IPA')

set.seed(123)

################################# IMPORT DATA ##################################

## avertable burden
  # baseline (VE = 70%)
  burden = read_excel('Code/InputData/burdenestimates_kumar2023.xlsx', sheet = "seventy")
  # VE = 50%
  burden_50 = read_excel('Code/InputData/burdenestimates_kumar2023.xlsx', sheet = "fifty")
 
## iso3
  iso3_list = read_excel('Code/InputData/burdenestimates_kumar2023.xlsx', sheet = "iso3")

## income groups
  income = read_excel('Code/InputData/incomegroups.xlsx')
  
## proportion cases resistant
  resistant = read.csv('Code/InputData/AMR_231029.csv')


################################ CLEAN DATA ####################################
## calculate resistance proportions
  resistant = resistant %>%
    rename(region = WHO.Region) %>%
    filter(pathogen == "Klebsiella pneumoniae", # filter for K. pneumo only
           drug == "Penicillins: Ampicillin" |
           drug == "Cephalosporins: Ceftazidime" | 
           drug == "Carbapenems: Meropenem" |
           drug == "Aminoglycosides: Gentamicin") %>% # gentamicin will be used for amp+gen
    select(-pathogen) %>%
    mutate(drug = ifelse(drug == "Carbapenems: Meropenem", "mero", drug),
           drug = ifelse(drug == "Aminoglycosides: Gentamicin", "gen", drug),
           drug = ifelse(drug == "Cephalosporins: Ceftazidime", "ceft", drug),
           drug = ifelse(drug == "Penicillins: Ampicillin", "amp", drug)) %>%
    pivot_wider(names_from = "drug", values_from = starts_with("resistance")) %>%
    rename(amp_cases_avertable = resistance_cases_amp,
           amp_cases_avertable_lower = resistance_cases_lower_amp,
           amp_cases_avertable_upper = resistance_cases_upper_amp,
           gen_cases_avertable = resistance_cases_gen,
           gen_cases_avertable_lower = resistance_cases_lower_gen,
           gen_cases_avertable_upper = resistance_cases_upper_gen,
           ceftazidime_cases_avertable = resistance_cases_ceft,
           ceftazidime_cases_avertable_lower = resistance_cases_lower_ceft,
           ceftazidime_cases_avertable_upper = resistance_cases_upper_ceft,
           meropenem_cases_avertable = resistance_cases_mero,
           meropenem_cases_avertable_lower = resistance_cases_lower_mero,
           meropenem_cases_avertable_upper = resistance_cases_upper_mero)
  
## create dfs that contain only countries in the analysis
  # combine avertable burden dfs and income groups, and remove HICs
  burden = burden %>% 
    left_join(iso3_list, by = "code") %>%
    left_join(income, by = "iso3") %>%
    filter(income_group != "High income") %>%
    left_join(resistant, by = "region") 
  
  nsim = 1e5 # number of simulations
  
  for (j in 1:nrow(burden)) { # for each country
    # draw from resistance distributions and overall case distributions
    
      # overall
      pars = get.gamma.par(q = c(burden$avertable_cases_lower[j], 
                                 burden$avertable_cases[j], 
                                 burden$avertable_cases_upper[j]), 
                           plot = FALSE, show.output = FALSE)
      if (is.na(pars[1])) {overall_sample = rtriangle(nsim, 
                                                     a = burden$avertable_cases_lower[j], 
                                                     b = burden$avertable_cases_upper[j], 
                                                     c = burden$avertable_cases[j])}
      else {overall_sample = rgamma(nsim, shape = pars[1], rate = pars[2])}

      # amp
      pars = get.gamma.par(q = c(burden$amp_cases_avertable_lower[j], 
                                 burden$amp_cases_avertable[j], 
                                 burden$amp_cases_avertable_upper[j]), 
                           plot = FALSE, show.output = FALSE)
      if (is.na(pars[1])) {amp_sample = rtriangle(nsim, 
                                                     a = burden$amp_cases_avertable_lower[j], 
                                                     b = burden$amp_cases_avertable_upper[j], 
                                                     c = burden$amp_cases_avertable[j])}
      else {amp_sample = rgamma(nsim, shape = pars[1], rate = pars[2])}
      
      # gen
      pars = get.gamma.par(q = c(burden$gen_cases_avertable_lower[j], 
                                 burden$gen_cases_avertable[j], 
                                 burden$gen_cases_avertable_upper[j]), 
                           plot = FALSE, show.output = FALSE)
      if (is.na(pars[1])) {gen_sample = rtriangle(nsim, 
                                                      a = burden$gen_cases_avertable_lower[j], 
                                                      b = burden$gen_cases_avertable_upper[j], 
                                                      c = burden$gen_cases_avertable[j])}
      else {gen_sample = rgamma(nsim, shape = pars[1], rate = pars[2])}
      
      # ceft
      pars = get.gamma.par(q = c(burden$ceftazidime_cases_avertable_lower[j], 
                                 burden$ceftazidime_cases_avertable[j], 
                                 burden$ceftazidime_cases_avertable_upper[j]), 
                           plot = FALSE, show.output = FALSE)
      if (is.na(pars[1])) {ceftazidime_sample = rtriangle(nsim, 
                                                         a = burden$ceftazidime_cases_avertable_lower[j], 
                                                         b = burden$ceftazidime_cases_avertable_upper[j], 
                                                         c = burden$ceftazidime_cases_avertable[j])}
      else {ceftazidime_sample = rgamma(nsim, shape = pars[1], rate = pars[2])}
      
      # mero
      pars = get.gamma.par(q = c(burden$meropenem_cases_avertable_lower[j], 
                                 burden$meropenem_cases_avertable[j], 
                                 burden$meropenem_cases_avertable_upper[j]), 
                           plot = FALSE, show.output = FALSE)
      if (is.na(pars[1])) {meropenem_sample = rtriangle(nsim, 
                                                          a = burden$meropenem_cases_avertable_lower[j], 
                                                          b = burden$meropenem_cases_avertable_upper[j], 
                                                          c = burden$meropenem_cases_avertable[j])}
      else {meropenem_sample = rgamma(nsim, shape = pars[1], rate = pars[2])}
     
    # calc burden
      gen = overall_sample * gen_sample
      amp = overall_sample * amp_sample
      ceft = overall_sample * ceftazidime_sample
      mero = overall_sample * meropenem_sample
    
    # save  
      burden$amp_cases_avertable[j] = median(amp)
      burden$amp_cases_avertable_lower[j] = quantile(amp, 0.025)
      burden$amp_cases_avertable_upper[j] = quantile(amp, 0.975)
      burden$gen_cases_avertable[j] = median(gen)
      burden$gen_cases_avertable_lower[j] = quantile(gen, 0.025)
      burden$gen_cases_avertable_upper[j] = quantile(gen, 0.975)
      burden$ceftazidime_cases_avertable[j] = median(ceft)
      burden$ceftazidime_cases_avertable_lower[j] = quantile(ceft, 0.025)
      burden$ceftazidime_cases_avertable_upper[j] = quantile(ceft, 0.975)
      burden$meropenem_cases_avertable[j] = median(mero)
      burden$meropenem_cases_avertable_lower[j] = quantile(mero, 0.025)
      burden$meropenem_cases_avertable_upper[j] = quantile(mero, 0.975) 
  }

    # save
    write.xlsx(burden, file = 'Code/InputData/burdenestimates_kumar2023_seventy.xlsx')
  
# repeat for the 50% VE scenario
    burden_50 = burden_50 %>% 
      left_join(iso3_list, by = "code") %>%
      left_join(income, by = "iso3") %>%
      filter(income_group != "High income") %>%
      left_join(resistant, by = "region") 
    
    nsim = 1e5 # number of simulations

    for (j in 1:nrow(burden_50)) { # for each country
      # draw from resistance distributions and overall case distributions
      
      # overall
      pars = get.gamma.par(q = c(burden_50$avertable_cases_lower[j], 
                                 burden_50$avertable_cases[j], 
                                 burden_50$avertable_cases_upper[j]), 
                           plot = FALSE, show.output = FALSE)
      if (is.na(pars[1])) {overall_sample = rtriangle(nsim, 
                                                      a = burden_50$avertable_cases_lower[j], 
                                                      b = burden_50$avertable_cases_upper[j], 
                                                      c = burden_50$avertable_cases[j])}
      else {overall_sample = rgamma(nsim, shape = pars[1], rate = pars[2])}
      
      # amp
      pars = get.gamma.par(q = c(burden_50$amp_cases_avertable_lower[j], 
                                 burden_50$amp_cases_avertable[j], 
                                 burden_50$amp_cases_avertable_upper[j]), 
                           plot = FALSE, show.output = FALSE)
      if (is.na(pars[1])) {amp_sample = rtriangle(nsim, 
                                                  a = burden_50$amp_cases_avertable_lower[j], 
                                                  b = burden_50$amp_cases_avertable_upper[j], 
                                                  c = burden_50$amp_cases_avertable[j])}
      else {amp_sample = rgamma(nsim, shape = pars[1], rate = pars[2])}
      
      # gen
      pars = get.gamma.par(q = c(burden_50$gen_cases_avertable_lower[j], 
                                 burden_50$gen_cases_avertable[j], 
                                 burden_50$gen_cases_avertable_upper[j]), 
                           plot = FALSE, show.output = FALSE)
      if (is.na(pars[1])) {gen_sample = rtriangle(nsim, 
                                                     a = burden_50$gen_cases_avertable_lower[j], 
                                                     b = burden_50$gen_cases_avertable_upper[j], 
                                                     c = burden_50$gen_cases_avertable[j])}
      else {gen_sample = rgamma(nsim, shape = pars[1], rate = pars[2])}
      
      # ceft
      pars = get.gamma.par(q = c(burden_50$ceftazidime_cases_avertable_lower[j], 
                                 burden_50$ceftazidime_cases_avertable[j], 
                                 burden_50$ceftazidime_cases_avertable_upper[j]), 
                           plot = FALSE, show.output = FALSE)
      if (is.na(pars[1])) {ceftazidime_sample = rtriangle(nsim, 
                                                          a = burden_50$ceftazidime_cases_avertable_lower[j], 
                                                          b = burden_50$ceftazidime_cases_avertable_upper[j], 
                                                          c = burden_50$ceftazidime_cases_avertable[j])}
      else {ceftazidime_sample = rgamma(nsim, shape = pars[1], rate = pars[2])}
      
      # mero
      pars = get.gamma.par(q = c(burden_50$meropenem_cases_avertable_lower[j], 
                                 burden_50$meropenem_cases_avertable[j], 
                                 burden_50$meropenem_cases_avertable_upper[j]), 
                           plot = FALSE, show.output = FALSE)
      if (is.na(pars[1])) {meropenem_sample = rtriangle(nsim, 
                                                        a = burden_50$meropenem_cases_avertable_lower[j], 
                                                        b = burden_50$meropenem_cases_avertable_upper[j], 
                                                        c = burden_50$meropenem_cases_avertable[j])}
      else {meropenem_sample = rgamma(nsim, shape = pars[1], rate = pars[2])}
      
      # calc burden_50
      gen = overall_sample * gen_sample
      amp = overall_sample * amp_sample
      ceft = overall_sample * ceftazidime_sample
      mero = overall_sample * meropenem_sample
      
      # save  
      burden_50$amp_cases_avertable[j] = median(amp)
      burden_50$amp_cases_avertable_lower[j] = quantile(amp, 0.025)
      burden_50$amp_cases_avertable_upper[j] = quantile(amp, 0.975)
      burden_50$gen_cases_avertable[j] = median(gen)
      burden_50$gen_cases_avertable_lower[j] = quantile(gen, 0.025)
      burden_50$gen_cases_avertable_upper[j] = quantile(gen, 0.975)
      burden_50$ceftazidime_cases_avertable[j] = median(ceft)
      burden_50$ceftazidime_cases_avertable_lower[j] = quantile(ceft, 0.025)
      burden_50$ceftazidime_cases_avertable_upper[j] = quantile(ceft, 0.975)
      burden_50$meropenem_cases_avertable[j] = median(mero)
      burden_50$meropenem_cases_avertable_lower[j] = quantile(mero, 0.025)
      burden_50$meropenem_cases_avertable_upper[j] = quantile(mero, 0.975) 
    }
    
    # save
    write.xlsx(burden_50, file = 'Code/InputData/burdenestimates_kumar2023_fifty.xlsx')