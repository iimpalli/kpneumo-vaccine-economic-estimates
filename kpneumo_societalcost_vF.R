################################## SET UP ######################################
################################## SET UP ######################################
################################## SET UP ######################################

library(dplyr)
library(readxl)
library(tidyr)
library(stringr)
library(xlsx)

# set the wd to a folder which has the folder Code in it, which houses the InputData and OtherData folders
setwd('/Users/XXX')

# societal cost is a script which combines the productivity loss and medical expenditures
# into a single number called societal costs, which is used for plotting and any
# combined totals

################################# IMPORT DATA ##################################

# productivity loss due to time spent in hospital
baseline_prodloss_hosp = read_excel('Code/OtherData/baseline_prodloss_hosp.xlsx') %>% select(-`...1`)
fifty_prodloss_hosp = read_excel('Code/OtherData/fifty_prodloss_hosp.xlsx') %>% select(-`...1`)

# direct medical expenditures
baseline_medexp = read_excel('Code/OtherData/baseline_medexp.xlsx') %>% select(-`...1`)
fifty_medexp = read_excel('Code/OtherData/fifty_medexp.xlsx') %>% select(-`...1`) 

###################### CALCULATE SOCIETAL COST AVERTABLE #######################
###################### CALCULATE SOCIETAL COST AVERTABLE #######################
###################### CALCULATE SOCIETAL COST AVERTABLE #######################

## create dfs combining the three economic costs for each of the two scenarios
  # then save

baseline = baseline_prodloss_hosp %>%
  left_join(baseline_medexp, by = c("iso3", "country_name", "region", "avertable_cases")) %>%
  mutate(societalcost = prodloss_hosp + medical_expenditures,
         societalcost_lower = prodloss_hosp_lower + medical_expenditures_lower,
         societalcost_upper = prodloss_hosp_upper + medical_expenditures_upper,
         societalcost_gen = prodloss_hosp_gen + medical_expenditures_gen ,
         societalcost_gen_lower = prodloss_hosp_gen_lower + medical_expenditures_gen_lower,
         societalcost_gen_upper = prodloss_hosp_gen_upper + medical_expenditures_gen_upper,
         societalcost_ceft = prodloss_hosp_ceft + medical_expenditures_ceft,
         societalcost_ceft_lower = prodloss_hosp_ceft_lower + medical_expenditures_ceft_lower,
         societalcost_ceft_upper = prodloss_hosp_ceft_upper + medical_expenditures_ceft_upper,
         societalcost_mero = prodloss_hosp_mero + medical_expenditures_mero,
         societalcost_mero_lower = prodloss_hosp_mero_lower + medical_expenditures_mero_lower,
         societalcost_mero_upper = prodloss_hosp_mero_upper + medical_expenditures_mero_upper,
         societalcost_amp = prodloss_hosp_amp + medical_expenditures_amp,
         societalcost_amp_lower = prodloss_hosp_amp_lower + medical_expenditures_amp_lower,
         societalcost_amp_upper = prodloss_hosp_amp_upper + medical_expenditures_amp_upper,
         prop_prodloss_hosp = prodloss_hosp / societalcost,
         prop_medexp = medical_expenditures / societalcost,
         prop_medexp_gen = medical_expenditures_gen / societalcost,
         prop_medexp_ceft = medical_expenditures_ceft / societalcost,
         prop_medexp_mero = medical_expenditures_mero / societalcost)
  # save
  write.xlsx(baseline, file = paste0('Code/OtherData/baseline_societalcost.xlsx'))

fifty = fifty_prodloss_hosp %>%
  left_join(fifty_medexp, by = c("iso3", "country_name", "region", "avertable_cases")) %>%
  mutate(societalcost = prodloss_hosp + medical_expenditures,
         societalcost_lower = prodloss_hosp_lower + medical_expenditures_lower,
         societalcost_upper = prodloss_hosp_upper + medical_expenditures_upper,
         societalcost_gen = prodloss_hosp_gen + medical_expenditures_gen ,
         societalcost_gen_lower = prodloss_hosp_gen_lower + medical_expenditures_gen_lower,
         societalcost_gen_upper = prodloss_hosp_gen_upper + medical_expenditures_gen_upper,
         societalcost_ceft = prodloss_hosp_ceft + medical_expenditures_ceft,
         societalcost_ceft_lower = prodloss_hosp_ceft_lower + medical_expenditures_ceft_lower,
         societalcost_ceft_upper = prodloss_hosp_ceft_upper + medical_expenditures_ceft_upper,
         societalcost_mero = prodloss_hosp_mero + medical_expenditures_mero,
         societalcost_mero_lower = prodloss_hosp_mero_lower + medical_expenditures_mero_lower,
         societalcost_mero_upper = prodloss_hosp_mero_upper + medical_expenditures_mero_upper,
         societalcost_amp = prodloss_hosp_amp + medical_expenditures_amp,
         societalcost_amp_lower = prodloss_hosp_amp_lower + medical_expenditures_amp_lower,
         societalcost_amp_upper = prodloss_hosp_amp_upper + medical_expenditures_amp_upper,
         prop_prodloss_hosp = prodloss_hosp / societalcost,
         prop_medexp = medical_expenditures / societalcost,
         prop_medexp_gen = medical_expenditures_gen / societalcost,
         prop_medexp_ceft = medical_expenditures_ceft / societalcost,
         prop_medexp_mero = medical_expenditures_mero / societalcost)
  # save
  write.xlsx(fifty, file = paste0('Code/OtherData/fifty_societalcost.xlsx'))

