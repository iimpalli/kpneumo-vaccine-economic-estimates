################################## SET UP ######################################
################################## SET UP ######################################
################################## SET UP ######################################

library(dplyr)
library(readxl)
library(writexl)
library(ggplot2)
library(cowplot)
library(xlsx)
library(tidyr)
library(viridis)
library(forcats)
library(gridExtra)
library(patchwork)

setwd('/Users/isabellaimpalli/Library/CloudStorage/OneDrive-CenterforDiseaseDynamics,Economics&Policy/AMR Vaccines CDC IPA')


################################# IMPORT DATA ##################################
## population 2021
  population = read_excel('Code/InputData/population.xlsx') %>%
    mutate(pop = as.numeric(pop))

## societal cost dfs
  # baseline (VE = 70%)
  baseline_sc = read_excel('Code/OtherData/baseline_societalcost.xlsx') %>% select(-`...1`) %>%
    left_join(population, by = "iso3")
  # VE = 50%
  fifty_sc = read_excel('Code/OtherData/fifty_societalcost.xlsx') %>% select(-`...1`) %>%
    left_join(population, by = "iso3")

## monetized DALY dfs
  # baseline (VE = 70%)
  baseline_daly = read_excel('Code/OtherData/baseline_monetizedDALYs.xlsx') %>% select(-`...1`) %>%
    left_join(population, by = "iso3")
  # VE = 50%
  fifty_daly = read_excel('Code/OtherData/fifty_monetizedDALYs.xlsx') %>% select(-`...1`) %>%
    left_join(population, by = "iso3")
  
## other standard definitions
  names = c("baseline", "fifty")
  
############################## RESULTS, FIGURES, AND TABLES ##############################
#### Table 2: global totals of DALYs ####
  # compute totals
  total_daly = baseline_daly %>%
    select(starts_with("monet")) %>%
    summarize(across(starts_with("monet"), sum)) %>%
    rename(monet_DALY_all_sum = monet_DALY,
           monet_DALY_all_lower = monet_DALY_lower,
           monet_DALY_all_upper = monet_DALY_upper,
           monet_DALY_amp_sum = monet_DALY_amp,
           monet_DALY_gen_sum = monet_DALY_gen,
           monet_DALY_ceft_sum = monet_DALY_ceft,
           monet_DALY_mero_sum = monet_DALY_mero) %>%
    select(starts_with("monet")) %>%
    pivot_longer(cols = starts_with("monet"),
                 names_to = c("variable", "abx", "type"),
                 names_pattern = "(.*)_(.*)_(.*)",
                 values_to = "value") %>%
    pivot_wider(names_from = "type", values_from = "value") %>%
    select(-variable) %>%
    mutate(sum = sum / 1e9,
           lower = lower / 1e9,
           upper = upper / 1e9,
           monet_DALYs = paste0(round(sum, 1), " (", round(lower, 1), "-", round(upper, 1), ")"))
  
#### Table 2: global totals of societal costs ####
  # compute totals
  total_sc = baseline_sc %>%
    select(starts_with("societal") | starts_with("medical_expen") | starts_with("prod")) %>%
    summarize(across(starts_with("societal"), sum),
              across(starts_with("med"), sum),
              across(starts_with("prod"), sum)) %>%
    rename(medex_all_sum = medical_expenditures,
           medex_all_lower = medical_expenditures_lower,
           medex_all_upper = medical_expenditures_upper,
           medex_amp_sum = medical_expenditures_amp,
           medex_amp_lower = medical_expenditures_amp_lower,
           medex_amp_upper = medical_expenditures_amp_upper,
           medex_gen_sum = medical_expenditures_gen,
           medex_gen_lower = medical_expenditures_gen_lower,
           medex_gen_upper = medical_expenditures_gen_upper,
           medex_ceft_sum = medical_expenditures_ceft,
           medex_ceft_lower = medical_expenditures_ceft_lower,
           medex_ceft_upper = medical_expenditures_ceft_upper,
           medex_mero_sum = medical_expenditures_mero,
           medex_mero_lower = medical_expenditures_mero_lower,
           medex_mero_upper = medical_expenditures_mero_upper,
           prodloss_all_sum = prodloss_hosp,
           prodloss_all_lower = prodloss_hosp_lower,
           prodloss_all_upper = prodloss_hosp_upper,
           prodloss_amp_sum = prodloss_hosp_amp,
           prodloss_amp_lower = prodloss_hosp_amp_lower,
           prodloss_amp_upper = prodloss_hosp_amp_upper,
           prodloss_gen_sum = prodloss_hosp_gen,
           prodloss_gen_lower = prodloss_hosp_gen_lower,
           prodloss_gen_upper = prodloss_hosp_gen_upper,
           prodloss_ceft_sum = prodloss_hosp_ceft,
           prodloss_ceft_lower = prodloss_hosp_ceft_lower,
           prodloss_ceft_upper = prodloss_hosp_ceft_upper,
           prodloss_mero_sum = prodloss_hosp_mero,
           prodloss_mero_lower = prodloss_hosp_mero_lower,
           prodloss_mero_upper = prodloss_hosp_mero_upper)
  
  soccost = total_sc %>%
    select(-starts_with("soc")) %>%
    pivot_longer(cols = matches("(med|prod)"),
                 names_to = c("variable", "abx", "type"),
                 names_pattern = "(.*)_(.*)_(.*)",
                 values_to = "value") %>%
    pivot_wider(names_from = "type", values_from = "value") %>%
    mutate(sum = sum / 1e6,
           lower = lower / 1e6,
           upper = upper / 1e6,
           societalcost = paste0(round(sum, 1), " (", round(lower, 1), "-", round(upper, 1), ")"))
  
  
#### Figure 2, S2: maps of avertable DALYs per capita ####    
    # import map data
    worldplotlatlon = map_data("world")
    worldplotlatlon = worldplotlatlon[which(worldplotlatlon$region!="Antarctica"),]
    isomatch = read.csv(file = "Code/InputData/CountryIsoMatch.csv")
    worldplotlatlon = left_join(worldplotlatlon, isomatch, by = "region")
    
    plain = theme(
      axis.text = element_blank(),
      axis.line = element_blank(),
      axis.ticks = element_blank(),
      panel.border = element_blank(),
      panel.grid = element_blank(),
      axis.title = element_blank(),
      panel.background = element_rect(fill = "white"),
      plot.title = element_text(hjust = 0.5),
      legend.position = "none"
    )
    # create df
    monet_DALY_pc = baseline_daly %>%
      select(iso3, country_name, pop, starts_with("mone")) %>%
      mutate(DALYpc_mero = monet_DALY_mero / pop,
             DALYpc_amp = monet_DALY_amp / pop,
             DALYpc_gen = monet_DALY_gen / pop,
             DALYpc_ceft = monet_DALY_ceft / pop,
             DALYpc = monet_DALY / pop) %>%
      select(iso3, starts_with("DALY")) 
    
    DALYpc_mean = mean(monet_DALY_pc$DALYpc)
    DALYpc_median = median(monet_DALY_pc$DALYpc)
    
    # join DALY df to world map and plot
    monet_DALY_pc = left_join(worldplotlatlon, monet_DALY_pc, by = "iso3")
    
    dalymap_pc_mero = ggplot(data = monet_DALY_pc, mapping = aes(x = long, y = lat, group = group)) +
      coord_fixed(1.3) +
      geom_polygon(aes(fill = DALYpc_mero)) +
      scale_fill_viridis_c(limits = c(0,2), oob = scales::squish, option = "plasma", na.value = "lightgray") +
      plain 
    dalymap_pc_mero
    
    dalymap_pc = ggplot(data = monet_DALY_pc, mapping = aes(x = long, y = lat, group = group)) +
      coord_fixed(1.3) +
      geom_polygon(aes(fill = DALYpc)) +
      scale_fill_viridis_c(limits = c(0,4), oob = scales::squish, option = "plasma", na.value = "lightgray") +
      plain
    dalymap_pc
    
    dalymap_pc_amp = ggplot(data = monet_DALY_pc, mapping = aes(x = long, y = lat, group = group)) +
      coord_fixed(1.3) +
      geom_polygon(aes(fill = DALYpc_amp)) +
      scale_fill_viridis_c(limits = c(0,2), oob = scales::squish, option = "plasma", na.value = "lightgray") +
      plain
    dalymap_pc_amp
    
    dalymap_pc_gen = ggplot(data = monet_DALY_pc, mapping = aes(x = long, y = lat, group = group)) +
      coord_fixed(1.3) +
      geom_polygon(aes(fill = DALYpc_amp)) +
      scale_fill_viridis_c(limits = c(0,2), oob = scales::squish, option = "plasma", na.value = "lightgray") +
      plain
    dalymap_pc_gen
    
    dalymap_pc_ceft = ggplot(data = monet_DALY_pc, mapping = aes(x = long, y = lat, group = group)) +
      coord_fixed(1.3) +
      geom_polygon(aes(fill = DALYpc_ceft)) +
      scale_fill_viridis_c(limits = c(0,2), oob = scales::squish, option = "plasma", na.value = "lightgray") +
      plain
    dalymap_pc_ceft
    
    ggsave("Figures/dalys_mappc_mero.svg", dalymap_pc_mero, width = 6, height = 3, dpi = 300)
    ggsave("Figures/dalys_mappc_amp.svg", dalymap_pc_amp, width = 6, height = 3, dpi = 300)
    ggsave("Figures/dalys_mappc_gen.svg", dalymap_pc_gen, width = 6, height = 3, dpi = 300)
    ggsave("Figures/dalys_mappc_ceft.svg", dalymap_pc_ceft, width = 6, height = 3, dpi = 300)
    ggsave("Figures/dalys_mappc.svg", dalymap_pc, width = 6, height = 3, dpi = 300)
    
    
#### Figure 3: Regional medians/IQR of monetized DALYs ####
    # create dataframe for the per capita DALYs summarized by region
    DALYs_pc_region = baseline_daly %>%
      select(iso3, country_name, region, pop, starts_with("monet")) %>%
      # create per capita values for country means
      mutate(DALY_all = monet_DALY / pop,
             DALY_amp = monet_DALY_amp / pop,
             DALY_ceft = monet_DALY_ceft / pop,
             DALY_mero = monet_DALY_mero / pop,
             DALY_gen = monet_DALY_gen / pop) %>%
      select(iso3, region, starts_with("DALY")) %>%
      # then keep track of the median, 25%ile, and 75%ile by region
      group_by(region) %>%
      summarize(
        DALY_all_median = median(DALY_all),
        DALY_all_lower = quantile(DALY_all, 0.25),
        DALY_all_upper = quantile(DALY_all, 0.75),
        DALY_amp_median = median(DALY_amp),
        DALY_amp_lower = quantile(DALY_amp, 0.25),
        DALY_amp_upper = quantile(DALY_amp, 0.75),
        DALY_ceft_median = median(DALY_ceft),
        DALY_ceft_lower = quantile(DALY_ceft, 0.25),
        DALY_ceft_upper = quantile(DALY_ceft, 0.75),
        DALY_mero_median = median(DALY_mero),
        DALY_mero_lower = quantile(DALY_mero, 0.25),
        DALY_mero_upper = quantile(DALY_mero, 0.75),
        DALY_gen_median = median(DALY_gen),
        DALY_gen_lower = quantile(DALY_gen, 0.25),
        DALY_gen_upper = quantile(DALY_gen, 0.75)
      ) %>%
      ungroup() %>%
      # reshape data
      pivot_longer(cols = ends_with("_lower") | ends_with("_upper") | ends_with("_median"),
                   names_to = c("variable", "abx", "type"),
                   names_pattern = "(.*)_(.*)_(.*)",
                   values_to = "value") %>%
      pivot_wider(names_from = "type", values_from = "value") %>%
      select(-variable)
    
    # save as a table
    write_xlsx(DALYs_pc_region, 'Results/DALYsmediansIQR.xlsx')
    
    # create plot for all
    DALYs_pc_region_all = DALYs_pc_region %>%
      filter(abx == "all") %>%
      mutate(region = fct_rev(factor(region)))
    
    # create plot
    DALYs_pc_region_all_plot = ggplot(DALYs_pc_region_all, aes(y = region, x = median, xmin = lower, xmax = upper)) +
      #geom_bar(stat = "identity", width = .7, position = "dodge", color = "black", fill = "gray") +
      geom_point(shape = 5, size = 2) +
      geom_errorbar(width = 0.2, color = "black", position = position_dodge(width = 0.7)) + 
      xlim(0,2) +
      labs(x = "Avertable monetized DALYs\nper capita\n(2021 US$)") +
      theme_bw() +
      theme(
        plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(), 
        legend.position = 'none', 
        text = element_text(size = 18, family = "Arial"),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_blank(), 
        axis.text = element_text(color = "black"),
        axis.line.x = element_line(size = 0.5, linetype = "solid", colour = "black"),
        axis.line.y = element_line(size = 0.5, linetype = "solid", colour = "black"))
    DALYs_pc_region_all_plot
    # save ggplot figure
    ggsave("Figures/DALYs_pc_region_all_plot.svg", DALYs_pc_region_all_plot, width = 3, height = 4, dpi = 300)
    
    custom_order = c("amp", "gen", "ceft", "mero")
    
    # create and save plots for resistance
    DALYs_pc_region_res = DALYs_pc_region %>%
      filter(abx != "all") %>%
      mutate(abx = factor(abx, levels = custom_order)) %>% 
      arrange(abx, region) %>%
      mutate(region = fct_rev(factor(region)))
    
    DALYs_pc_region_res_plot = ggplot(DALYs_pc_region_res, aes(y = region, x = median, xmin = lower, xmax = upper, fill = region)) +
      geom_point(shape = 23, size = 4) +
      geom_errorbar(width = 0.2, color = "black", position = position_dodge(width = 0.7)) + 
      scale_fill_viridis_d(option = "plasma") +
      labs(x = "Avertable monetized DALYs\nper capita\n(2021 US$)") +
      xlim(0,1.5) +
      theme_bw() +
      theme(
        plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(), 
        legend.position = 'none', 
        text = element_text(size = 13),
        axis.text.y = element_text(size = 14),
        axis.title.y = element_blank(),
        axis.text = element_text(color = "black"),
        axis.line.x = element_line(size = 0.5, linetype = "solid", colour = "black"),
        axis.line.y = element_line(size = 0.5, linetype = "solid", colour = "black")) +
      facet_wrap(~ abx, ncol = 1, 
                 labeller = labeller(abx = c("amp" = "Ampicillin", 
                                             "gen" = "Gentamicin",
                                             "ceft" = "Ceftazidime", 
                                             "mero" = "Meropenem")))
    DALYs_pc_region_res_plot
    # save ggplot figure
    ggsave("Figures/DALYs_pc_region_res_plot.svg", DALYs_pc_region_res_plot, width = 5, height = 8, dpi = 300)
    
    
#### Results: country per capita totals of societal costs ####
    country_sc_pc = baseline_sc %>%
      select(country_name, pop, starts_with("soc"), income_group, region) %>%
      mutate(across(starts_with("soc"), ~ . / pop)) 
    
#### Results: regional medians/IQR of per capita societal costs ####  
    # create dataframe for the per capita societal cost summarized by region
    societalcost_pc_region = baseline_sc %>%
      select(iso3, country_name, region, pop, starts_with("soc")) %>%
      # create per capita values for country means
      mutate(socpc_all = societalcost / pop,
             socpc_gen = societalcost_gen / pop,
             socpc_ceft = societalcost_ceft / pop,
             socpc_mero = societalcost_mero / pop,
             socpc_amp = societalcost_amp / pop) %>%
      select(iso3, region, starts_with("socpc")) %>%
      # then keep track of the median, 25%ile, and 75%ile by region
      group_by(region) %>%
      summarize(
        socpc_all_median = median(socpc_all),
        socpc_all_lower = quantile(socpc_all, 0.25),
        socpc_all_upper = quantile(socpc_all, 0.75),
        socpc_gen_median = median(socpc_gen),
        socpc_gen_lower = quantile(socpc_gen, 0.25),
        socpc_gen_upper = quantile(socpc_gen, 0.75),
        socpc_ceft_median = median(socpc_ceft),
        socpc_ceft_lower = quantile(socpc_ceft, 0.25),
        socpc_ceft_upper = quantile(socpc_ceft, 0.75),
        socpc_mero_median = median(socpc_mero),
        socpc_mero_lower = quantile(socpc_mero, 0.25),
        socpc_mero_upper = quantile(socpc_mero, 0.75),
        socpc_amp_median = median(socpc_amp),
        socpc_amp_lower = quantile(socpc_amp, 0.25),
        socpc_amp_upper = quantile(socpc_amp, 0.75)
      ) %>%
      ungroup() %>%
      # reshape data for graphing
      pivot_longer(cols = ends_with("_lower") | ends_with("_upper") | ends_with("_median"),
                   names_to = c("variable", "abx", "type"),
                   names_pattern = "(.*)_(.*)_(.*)",
                   values_to = "value") %>%
      pivot_wider(names_from = "type", values_from = "value") %>%
      select(-variable)
    
    # save as a table
    write_xlsx(societalcost_pc_region, 'Results/societalcostsmediansIQR.xlsx')
    
    # create plots
    # create plot for all
    societalcost_pc_region_all = societalcost_pc_region %>%
      filter(abx == "all") %>%
      mutate(region = fct_rev(factor(region)))
    
    # create plot
    societalcost_pc_region_all_plot = ggplot(societalcost_pc_region_all, aes(y = region, x = median, xmin = lower, xmax = upper)) +
      geom_point(shape = 5, size = 2) +
      geom_errorbar(width = 0.2, color = "black", position = position_dodge(width = 0.7)) + 
      xlim(0,6) +
      labs(x = "Avertable societal cost\nper capita\n(intl 2021$)") +
      theme_bw() +
      theme(
        plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(), 
        legend.position = 'none', 
        text = element_text(size = 18),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_blank(), 
        axis.text = element_text(color = "black"),
        axis.line.x = element_line(size = 0.5, linetype = "solid", colour = "black"),
        axis.line.y = element_line(size = 0.5, linetype = "solid", colour = "black"))
    societalcost_pc_region_all_plot
    # save ggplot figure
    ggsave("Figures/societalcost_pc_region_all_plot.svg", societalcost_pc_region_all_plot, width = 3, height = 4, dpi = 300)
    
    # create and save plots for resistance
    custom_order = c("amp", "gen", "ceft", "mero")
    
    societalcost_pc_region_res = societalcost_pc_region %>%
      filter(abx != "all") %>%
      mutate(abx = factor(abx, levels = custom_order)) %>% 
      arrange(abx, region) %>%
      mutate(region = fct_rev(factor(region)))
    
    societalcost_pc_region_res_plot = ggplot(societalcost_pc_region_res, aes(y = region, x = median, xmin = lower, xmax = upper, fill = region)) +
      geom_point(shape = 23, size = 4) +
      geom_errorbar(width = 0.2, color = "black", position = position_dodge(width = 0.7)) + 
      scale_fill_viridis_d(option = "plasma") + 
      labs(x = "Avertable societal costs\nper capita\n(intl 2021$)") +
      xlim(0,0.03) +
      theme_bw() +
      theme(
        plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(), 
        legend.position = 'none', 
        text = element_text(size = 13),
        axis.text.y = element_text(size = 14),
        axis.title.y = element_blank(),
        axis.text = element_text(color = "black"),
        axis.line.x = element_line(size = 0.5, linetype = "solid", colour = "black"),
        axis.line.y = element_line(size = 0.5, linetype = "solid", colour = "black")) +
      facet_wrap(~ abx, ncol = 1, 
                 labeller = labeller(abx = c("amp" = "Ampicillin", 
                                             "gen" = "Gentamicin",
                                             "ceft" = "Ceftazidime", 
                                             "mero" = "Meropenem")))
    societalcost_pc_region_res_plot
    # save ggplot figure
    ggsave("Figures/societalcost_pc_region_res_plot.svg", societalcost_pc_region_res_plot, width = 3.5, height = 8, dpi = 300)
    
#### Figure S1: top countries societal costs per capita ####
    # in terms of avertable societal costs per capita
    baseline_soc_topcountry_pc = baseline_sc %>%
      select(iso3, country_name, region, income_group, pop, starts_with("soc")) %>%
      mutate(socpc_all_mean = societalcost / pop,
             socpc_all_lower = societalcost_lower / pop,
             socpc_all_upper = societalcost_upper / pop,
             socpc_amp_mean = societalcost_amp / pop,
             socpc_amp_lower = societalcost_amp_lower / pop,
             socpc_amp_upper = societalcost_amp_upper / pop,
             socpc_ceft_mean = societalcost_ceft / pop,
             socpc_ceft_lower = societalcost_ceft_lower / pop,
             socpc_ceft_upper = societalcost_ceft_upper / pop,
             socpc_mero_mean = societalcost_mero / pop,
             socpc_mero_lower = societalcost_mero_lower / pop,
             socpc_mero_upper = societalcost_mero_upper / pop,
             socpc_gen_mean = societalcost_gen / pop,
             socpc_gen_lower = societalcost_gen_lower / pop,
             socpc_gen_upper = societalcost_gen_upper / pop) %>%
      select(iso3, country_name, region, income_group, starts_with("socpc")) %>%
      pivot_longer(cols = starts_with("socpc"),
                   names_to = c("variable", "abx", "type"),
                   names_pattern = "(.*)_(.*)_(.*)",
                   values_to = "value") %>%
      pivot_wider(names_from = "type", values_from = "value") %>%
      select(-variable) %>%
      group_by(abx) %>%
      arrange(desc(mean)) %>%
      slice_head(n = 7) 
    baseline_soc_topcountry_pc$abx = as.factor(baseline_soc_topcountry_pc$abx)
    
    # make plot based on amp cases
    baseline_soc_topcountry_pc_amp = baseline_soc_topcountry_pc %>%
      filter(abx == "amp") %>%
      arrange(-mean)
    baseline_soc_topcountry_pc_amp_plot = ggplot(baseline_soc_topcountry_pc_amp, aes(y = reorder(country_name, mean), 
                                                                                     fill = region,
                                                                                     x = mean,
                                                                                     xmin = lower,
                                                                                     xmax = upper)) +
      geom_bar(stat = "identity", width = 0.7, position = "dodge", color = "black") +
      geom_errorbar(width = 0.2, color = "black", position = position_dodge(width = 0.7)) + 
      scale_fill_manual(values = c("AFR" = "#f0f921", "AMR" = "#fca636", "EMR" = "#e16462", "EUR" = "#b12a90", "SEAR" = "#6a00a8", "WPR" = "#0d0887")) +
      theme_minimal() +
      theme(
        plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(), 
        legend.position = 'none', 
        axis.title.y = element_blank(),
        text = element_text(size = 15),
        axis.title.x = element_text(size = 12),
        axis.text = element_text(color = "black"),
        axis.line.x = element_line(size = 0.5, linetype = "solid", colour = "black"),
        axis.line.y = element_line(size = 0.5, linetype = "solid", colour = "black")) +
      labs(x = "Avertable medical expenditures\nand productivity loss\n(2021 US$)")
    baseline_soc_topcountry_pc_amp_plot
    
    ggsave("Figures/baseline_soc_topcountry_pc_amp_plot.svg", baseline_soc_topcountry_pc_amp_plot, width = 5, height = 3)
    
    # make plot based on gen cases
    baseline_soc_topcountry_pc_gen = baseline_soc_topcountry_pc %>%
      filter(abx == "gen") %>%
      arrange(-mean)
    baseline_soc_topcountry_pc_gen_plot = ggplot(baseline_soc_topcountry_pc_gen, aes(y = reorder(country_name, mean), 
                                                                                     fill = region,
                                                                                     x = mean,
                                                                                     xmin = lower,
                                                                                     xmax = upper)) +
      geom_bar(stat = "identity", width = 0.7, position = "dodge", color = "black") +
      geom_errorbar(width = 0.2, color = "black", position = position_dodge(width = 0.7)) + 
      scale_fill_manual(values = c("AFR" = "#f0f921", "AMR" = "#fca636", "EMR" = "#e16462", "EUR" = "#b12a90", "SEAR" = "#6a00a8", "WPR" = "#0d0887")) +
      theme_minimal() +
      theme(
        plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(), 
        legend.position = 'none', 
        axis.title.y = element_blank(),
        text = element_text(size = 15),
        axis.title.x = element_text(size = 12),
        axis.text = element_text(color = "black"),
        axis.line.x = element_line(size = 0.5, linetype = "solid", colour = "black"),
        axis.line.y = element_line(size = 0.5, linetype = "solid", colour = "black")) +
      labs(x = "Avertable medical expenditures\nand productivity loss\n(2021 US$)")
    baseline_soc_topcountry_pc_gen_plot
    
    ggsave("Figures/baseline_soc_topcountry_pc_gen_plot.svg", baseline_soc_topcountry_pc_gen_plot, width = 5, height = 3)
    
    # make plot based on ceft cases
    baseline_soc_topcountry_pc_ceft = baseline_soc_topcountry_pc %>%
      filter(abx == "ceft") %>%
      arrange(-mean)
    baseline_soc_topcountry_pc_ceft_plot = ggplot(baseline_soc_topcountry_pc_ceft, aes(y = reorder(country_name, mean), 
                                                                                       fill = region,
                                                                                       x = mean,
                                                                                       xmin = lower,
                                                                                       xmax = upper)) +
      geom_bar(stat = "identity", width = 0.7, position = "dodge", color = "black") +
      geom_errorbar(width = 0.2, color = "black", position = position_dodge(width = 0.7)) + 
      scale_fill_manual(values = c("AFR" = "#f0f921", "AMR" = "#fca636", "EMR" = "#e16462", "EUR" = "#b12a90", "SEAR" = "#6a00a8", "WPR" = "#0d0887")) +
      theme_minimal() +
      theme(
        plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(), 
        legend.position = 'none', 
        axis.title.y = element_blank(),
        text = element_text(size = 15),
        axis.title.x = element_text(size = 12),
        axis.text = element_text(color = "black"),
        axis.line.x = element_line(size = 0.5, linetype = "solid", colour = "black"),
        axis.line.y = element_line(size = 0.5, linetype = "solid", colour = "black"))+
      labs(x = "Avertable medical expenditures\nand productivity loss\n(2021 US$)")
    baseline_soc_topcountry_pc_ceft_plot
    
    ggsave("Figures/baseline_soc_topcountry_pc_ceft_plot.svg", baseline_soc_topcountry_pc_ceft_plot, width = 5, height = 3)
    
    # make plot based on mero cases
    baseline_soc_topcountry_pc_mero = baseline_soc_topcountry_pc %>%
      filter(abx == "mero") %>%
      arrange(-mean)
    baseline_soc_topcountry_pc_mero_plot = ggplot(baseline_soc_topcountry_pc_mero, aes(y = reorder(country_name, mean), 
                                                                                       fill = region,
                                                                                       x = mean,
                                                                                       xmin = lower,
                                                                                       xmax = upper)) +
      geom_bar(stat = "identity", width = 0.7, position = "dodge", color = "black") +
      geom_errorbar(width = 0.2, color = "black", position = position_dodge(width = 0.7)) + 
      scale_fill_manual(values = c("AFR" = "#f0f921", "AMR" = "#fca636", "EMR" = "#e16462", "EUR" = "#b12a90", "SEAR" = "#6a00a8", "WPR" = "#0d0887")) +
      theme_minimal() +
      theme(
        plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(), 
        legend.position = 'none', 
        axis.title.y = element_blank(),
        text = element_text(size = 15),
        axis.title.x = element_text(size = 12),
        axis.text = element_text(color = "black"),
        axis.line.x = element_line(size = 0.5, linetype = "solid", colour = "black"),
        axis.line.y = element_line(size = 0.5, linetype = "solid", colour = "black"))+
      labs(x = "Avertable medical expenditures\nand productivity loss\n(2021 US$)")
    baseline_soc_topcountry_pc_mero_plot
    
    ggsave("Figures/baseline_soc_topcountry_pc_mero_plot.svg", baseline_soc_topcountry_pc_mero_plot, width = 5, height = 3)
    
    
#### Figure S3: maps per capita societal cost ####
    # create df
    soc_pc = baseline_sc %>%
      select(iso3, country_name, pop, starts_with("soc")) %>%
      mutate(socpc_mero = societalcost_mero / pop,
             socpc_amp = societalcost_amp / pop,
             socpc_gen = societalcost_gen / pop,
             socpc_ceft = societalcost_ceft / pop) %>%
      select(iso3, starts_with("socpc")) 
    
    # join soc df to world map and plot
    soc_pc = left_join(worldplotlatlon, soc_pc, by = "iso3")
    
    socmap_pc_mero = ggplot(data = soc_pc, mapping = aes(x = long, y = lat, group = group)) +
      coord_fixed(1.3) +
      geom_polygon(aes(fill = socpc_mero)) +
      scale_fill_viridis_c(limits = c(0,0.15), oob = scales::squish, option = "plasma", na.value = "lightgray") +
      plain
    socmap_pc_mero
    
    socmap_pc_amp = ggplot(data = soc_pc, mapping = aes(x = long, y = lat, group = group)) +
      coord_fixed(1.3) +
      geom_polygon(aes(fill = socpc_amp)) +
      scale_fill_viridis_c(limits = c(0,0.15), oob = scales::squish, option = "plasma", na.value = "lightgray") +
      plain
    socmap_pc_amp
    
    socmap_pc_gen = ggplot(data = soc_pc, mapping = aes(x = long, y = lat, group = group)) +
      coord_fixed(1.3) +
      geom_polygon(aes(fill = socpc_gen)) +
      scale_fill_viridis_c(limits = c(0,0.15), oob = scales::squish, option = "plasma", na.value = "lightgray") +
      plain
    socmap_pc_gen
    
    socmap_pc_ceft = ggplot(data = soc_pc, mapping = aes(x = long, y = lat, group = group)) +
      coord_fixed(1.3) +
      geom_polygon(aes(fill = socpc_ceft)) +
      scale_fill_viridis_c(limits = c(0,0.15), oob = scales::squish, option = "plasma", na.value = "lightgray") +
      plain
    socmap_pc_ceft
    
    ggsave("Figures/soc_mappc_mero.svg", socmap_pc_mero, width = 6, height = 3, dpi = 300)
    ggsave("Figures/soc_mappc_amp.svg", socmap_pc_amp, width = 6, height = 3, dpi = 300)
    ggsave("Figures/soc_mappc_gen.svg", socmap_pc_gen, width = 6, height = 3, dpi = 300)
    ggsave("Figures/soc_mappc_ceft.svg", socmap_pc_ceft, width = 6, height = 3, dpi = 300)
    ggsave("Figures/soc_mappc.svg", socmap_pc, width = 6, height = 3, dpi = 300)
    
#### Table S1: countries in analysis ####
    # creates the table of countries in the analysis for the supplement along with
    # their region and income grouping
    countries = baseline_daly %>%
      select(country_name, iso3, region, income_group) %>%
      arrange(country_name)
    write_xlsx(countries, 'Results/countries.xlsx')
    
#### Tables S2-S3, S5: avertable monetized DALYs by country ####
    dalys = list()
    dalys[[1]] = baseline_daly
    dalys[[2]] = fifty_daly
    names(dalys) = names
    
    for (i in names) {
      dalys[[i]] = dalys[[i]] %>%
        select(country_name, starts_with("monet")) %>%
        mutate(across(starts_with("monet"), ~ . / 1e6), 
               all = paste0(round(monet_DALY, 2), " (", round(monet_DALY_lower,2), "-", round(monet_DALY_upper, 2), ")"),
               amp = paste0(round(monet_DALY_amp, 2), " (", round(monet_DALY_amp_lower,2), "-", round(monet_DALY_amp_upper, 2), ")"),
               ceft = paste0(round(monet_DALY_ceft, 2), " (", round(monet_DALY_ceft_lower,2), "-", round(monet_DALY_ceft_upper, 2), ")"),
               mero = paste0(round(monet_DALY_mero, 2), " (", round(monet_DALY_mero_lower,2), "-", round(monet_DALY_mero_upper, 2), ")"),
               gen = paste0(round(monet_DALY_gen, 2), " (", round(monet_DALY_gen_lower,2), "-", round(monet_DALY_gen_upper, 2), ")")) %>%
        select(country_name, all, amp, gen, ceft, mero) %>%
        arrange(country_name)
      
      # save
      filename = paste0("Results/", i, "_dalys.xlsx")
      write_xlsx(dalys[[i]], filename)
      
    }  
    
    dalys_pc = baseline_daly %>%
      select(country_name, starts_with("monet"), pop) %>%
      mutate(across(starts_with("monet"), ~ . / pop), 
             all = paste0(round(monet_DALY, 2), " (", round(monet_DALY_lower,2), "-", round(monet_DALY_upper, 2), ")"),
             amp = paste0(round(monet_DALY_amp, 2), " (", round(monet_DALY_amp_lower,2), "-", round(monet_DALY_amp_upper, 2), ")"),
             ceft = paste0(round(monet_DALY_ceft, 2), " (", round(monet_DALY_ceft_lower,2), "-", round(monet_DALY_ceft_upper, 2), ")"),
             mero = paste0(round(monet_DALY_mero, 2), " (", round(monet_DALY_mero_lower,2), "-", round(monet_DALY_mero_upper, 2), ")"),
             gen = paste0(round(monet_DALY_gen, 2), " (", round(monet_DALY_gen_lower,2), "-", round(monet_DALY_gen_upper, 2), ")")) %>%
      select(country_name, all, amp, gen, ceft, mero) %>%
      arrange(country_name)
    
    # save
    write_xlsx(dalys_pc, "Results/dalys_pc.xlsx")
    
#### Figure S4: cost of antibiotic tx / composition of dir medical exp.####
    # import GNI data
    gni = read_excel('Code/InputData/GNIpc_2021usd.xlsx') %>%
      mutate(GNIpc_2021 = as.numeric(GNIpc_2021))
    
    # create relevant dataframe
  medexp_prop_income = baseline_sc %>%
    select(iso3, region, income_group, starts_with("medical_exp_"), starts_with("ratio")) %>%
    # note "ratio" is the abx cost, the variable name is just a misnomer
    mutate(hos_cost_fl = medical_exp_fl - ratio_fl,
           hos_cost_sl = medical_exp_sl - ratio_sl,
           hos_cost_tl = medical_exp_tl - ratio_tl) %>%
    select(-starts_with("medical_exp_")) %>%
    group_by(income_group) %>%
    summarize(across(starts_with("hos"), median, na.rm = TRUE),
              across(starts_with("ratio"), median, na.rm = TRUE)) %>%
    pivot_longer(cols = ends_with("fl") | ends_with("sl") | ends_with("tl"), 
                 names_to = c("variable", "abx"),
                 names_pattern = "(.*)_(.*)",
                 values_to = "value") %>%
    pivot_wider(names_from = "variable", values_from = "value") %>%
    mutate(abx_cost = ratio) %>%
    select(-ratio) %>%
    pivot_longer(cols = ends_with("cost")) 
  
  # make these fields factors, for graphing
  medexp_prop_income$abx = as.factor(medexp_prop_income$abx)
  medexp_prop_income$name = as.factor(medexp_prop_income$name)
  medexp_prop_income$income_group = as.factor(medexp_prop_income$income_group)
  
  # make the plot, and save!
  medexp_prop_income_plot = ggplot(medexp_prop_income, aes(x = abx, fill = name, y = value)) +
    geom_bar(stat = "identity", width = 0.75, position = "stack", color = "black") +
    facet_wrap(~ income_group) +
    scale_fill_manual(values = c("abx_cost" = "#0d0887", "hos_cost" = "#e16462")) +
    theme_minimal() +
    theme(legend.position = "none") 
  medexp_prop_income_plot
  
  ggsave("Figures/medexp_prop_income_plot.pdf", medexp_prop_income_plot, width = 6, height = 4)
  
  # make a version where it shows how many days worth of daily income are needed
  medexp_prop_income_gni = baseline_sc %>%
    select(iso3, region, income_group, starts_with("medical_exp_"), starts_with("ratio")) %>%
    # note "ratio" is the abx cost, the variable name is just a misnomer
    mutate(hos_cost_fl = medical_exp_fl - ratio_fl,
           hos_cost_sl = medical_exp_sl - ratio_sl,
           hos_cost_tl = medical_exp_tl - ratio_tl) %>%
    select(-starts_with("medical_exp_")) %>%
    left_join(gni, by = "iso3") %>%
    mutate(gni_day = GNIpc_2021 / 365) %>%
    select(-GNIpc_2021) %>%
    pivot_longer(cols = ends_with("fl") | ends_with("sl") | ends_with("tl"), 
                 names_to = c("variable", "abx"),
                 names_pattern = "(.*)_(.*)",
                 values_to = "value") %>%
    pivot_wider(names_from = "variable", values_from = "value") %>%
    mutate(abx_cost = ratio) %>%
    select(-ratio) %>%
    pivot_longer(cols = ends_with("cost")) %>%
    mutate(days_income = value / gni_day) %>%
    group_by(income_group, abx, name) %>%
    summarize(days_income_lower = quantile(days_income, 0.25),
              days_income_upper = quantile(days_income, 0.75),
              days_income = median(days_income)) %>%
    ungroup() %>%
    filter(!(abx == "tl" & name == "hos_cost")) %>%
    filter(name != "hos_cost") %>%
    mutate(cost = paste0(name, "_", abx),
           income_group = as.factor(income_group),
           cost = as.factor(cost)) %>%
    select(income_group, cost, starts_with("days"))
  
  
  # plot and save
  medexp_prop_income_gni_plot = ggplot(medexp_prop_income_gni, aes(x = cost, fill = income_group, y = days_income, ymin = days_income_lower, ymax = days_income_upper)) +
    geom_bar(stat = "identity", width = 0.7, position = "dodge", color = "black") +
    geom_errorbar(width = 0.2, color = "black", position = position_dodge(width = 0.7)) + 
    scale_fill_manual(values = c("Low income" = "#6a00a8", "Lower middle income" = "#b12a90", "Upper middle income" = "#fca636")) + 
    theme_minimal() +
    theme(legend.position = "none")
  medexp_prop_income_gni_plot
  
  ggsave("Figures/medexp_prop_income_gni_plot.pdf", medexp_prop_income_gni_plot, width = 6, height = 4)

  
#### Tables S6-S7: societal costs by country ####
  societalcosts = list()
  societalcosts[[1]] = baseline_sc
  societalcosts[[2]] = fifty_sc
  names(societalcosts) = names
  
  for (i in names) {
    societalcosts[[i]] = societalcosts[[i]] %>%
      select(country_name, starts_with("soc")) %>%
      mutate(across(starts_with("soc"), ~ . / 1e3), 
             all = paste0(round(societalcost, 2), " (", round(societalcost_lower,2), "-", round(societalcost_upper, 2), ")"),
             amp = paste0(round(societalcost_amp, 2), " (", round(societalcost_amp_lower,2), "-", round(societalcost_amp_upper, 2), ")"),
             ceft = paste0(round(societalcost_ceft, 2), " (", round(societalcost_ceft_lower,2), "-", round(societalcost_ceft_upper, 2), ")"),
             mero = paste0(round(societalcost_mero, 2), " (", round(societalcost_mero_lower,2), "-", round(societalcost_mero_upper, 2), ")"),
             gen = paste0(round(societalcost_gen, 2), " (", round(societalcost_gen_lower,2), "-", round(societalcost_gen_upper, 2), ")")) %>%
      select(country_name, all, amp, gen, ceft, mero) %>%
      arrange(country_name)
    
    # save
    filename = paste0("Results/", i, "_societalcosts.xlsx")
    write_xlsx(societalcosts[[i]], filename)
    
  }