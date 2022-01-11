# Compare_Water_Budgets.R

# Script for comparing water budget results produced by Water_Budgets_OneWater.R

# User Input --------------------------------------------------------------
headDir = 'SVHSM_runs/'
scenario_prefix = 'SVHSM_'
scenarios = c('0_pct_pumping', '25_pct_pumping', '50_pct_pumping', '75_pct_pumping')
ZoneBudget_File1 = 'Eastside_Westside_Budget.csv'
ZoneBudget_File2 = 'Eastside_Upper_Lower_Budget.csv'
WYstart = 2000
WYend = 2020

MF_plot_range = c(-50,100)
MF_plot_int = 25

# Initialize Script -------------------------------------------------------
library(dplyr)
library(lubridate)
library(plotly)
library(ggplot2)
library(ggthemes)
library(tidyr)
library(RColorBrewer)
library(cowplot)

SWBM_monthly_budgetFiles = paste0(headDir,scenario_prefix,scenarios,'/Results/Soil_Zone_Budget_Monthly_TAF.csv')
MODFLOW_monthly_budgetFiles = paste0(headDir,scenario_prefix,scenarios,'/Results/Groundwater_Budget_Monthly_TAF.csv')
SW_monthly_budgetFiles = paste0(headDir,scenario_prefix,scenarios,'/Results/Surface-Water_Budget_Monthly_TAF.csv')

SWBM_annual_budgetFiles = paste0(headDir,scenario_prefix,scenarios,'/Results/Soil_Zone_Budget_Annual_TAF.csv')
MODFLOW_annual_budgetFiles = paste0(headDir,scenario_prefix,scenarios,'/Results/Groundwater_Budget_Annual_TAF.csv')
SW_annual_budgetFiles = paste0(headDir,scenario_prefix,scenarios,'/Results/Surface-Water_Budget_Annual_TAF.csv')

Eastside_Westside_zoneBudgetFiles = paste0(headDir,scenario_prefix,scenarios,'/Results/Eastside_Westside_Budget.csv')
Eastside_Upper_Lower_zoneBudgetFiles = paste0(headDir,scenario_prefix,scenarios,'/Results/Eastside_Upper_Lower_Budget.csv')


# Functions ---------------------------------------------------------------
# Create Date Arrays ------------------------------------------------------
WYstartDate = paste0(WYstart-1,'-10-01') %>% as.Date()
WYendDate = paste0(WYend,'-09-30') %>% as.Date()
modelDays = seq.Date(from = WYstartDate, to = WYendDate, by = 'day')
modelMonths = seq.Date(from = WYstartDate, to = WYendDate, by = 'month')
modelYears = (WYend - WYstart) + 1
nmonths = length(modelMonths)

# Define Dry and Wet Water Years ------------------------------------------
WYtypes = read.table('Sacramento Valley Water Year Index for Plots.txt', header = T) %>%
  mutate(WY_Start_Dates = as.Date(paste0(WY_Start-1,'-10-01')),
         WY_End_Dates = as.Date(paste0(WY_End,'-09-30')),
         plot_color = if_else(Type=='Dry', 'light coral', 'steelblue2'),
         plot_annual_start = WY_Start -0.5,
         plot_annual_end = WY_End + 0.5,
         annual_label_date = WY_Start + (WY_End-WY_Start)/2,
         monthly_label_date = WY_Start_Dates + (WY_End_Dates - WY_Start_Dates)/2) %>%
  filter(WY_Start_Dates >= WYstartDate, WY_End_Dates <= WYendDate)
# General ggplot Options ----------------------------------------------------
theme_adj = theme(plot.title = element_text(hjust = 0.5, size = 14),
                  panel.grid.major.y = element_line(size = 0.1, color = 'gray80'),
                  panel.border = element_rect(color = 'black', fill = NA, size = 1),
                  plot.background = element_rect(fill = NA, color = NA),
                  axis.title = element_text(size = 13),
                  axis.title.x = element_blank(),
                  axis.text = element_text(size = 12),
                  axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))



# Read in Water Budgets ---------------------------------------------------
SWBM_monthly_budgets = lapply(X = SWBM_monthly_budgetFiles, FUN = function(x){read.csv(x, header = T)})
MODFLOW_monthly_budgets = lapply(X = MODFLOW_monthly_budgetFiles, FUN = function(x){read.csv(x, header = T)})
SW_monthly_budgets = lapply(X = SW_monthly_budgetFiles, FUN = function(x){read.csv(x, header = T)})

SWBM_annual_budgets = lapply(X = SWBM_annual_budgetFiles, FUN = function(x){read.csv(x, header = T)})
MODFLOW_annual_budgets = lapply(X = MODFLOW_annual_budgetFiles, FUN = function(x){read.csv(x, header = T)})
SW_annual_budgets = lapply(X = SW_annual_budgetFiles, FUN = function(x){read.csv(x, header = T)})

Eastside_Westside_annual = lapply(X = Eastside_Westside_zoneBudgetFiles, FUN = function(x){read.csv(x, header = T)})
Eastside_Upper_Lower_annual = lapply(X = Eastside_Upper_Lower_zoneBudgetFiles, FUN = function(x){read.csv(x, header = T)})


# SW Inflows --------------------------------------------------------------
SW_Inflows_Annual = SW_annual_budgets %>%
  sapply('[[', 'GW.Basin.Inflow') %>%
  as.data.frame() %>%
  mutate(WY = SW_annual_budgets[[1]][,1])
names(SW_Inflows_Annual) = c(scenarios, 'WY')

SW_Inflows_Annual_plot = SW_Inflows_Annual %>%
  pivot_longer(cols = -WY, names_to = 'Climate Scenario', values_to = 'SW Inflow Volume (TAF)') %>%
  ggplot(aes(x = WY, y = `SW Inflow Volume (TAF)`, color = `Climate Scenario`, fill = `Climate Scenario`)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  scale_x_continuous(limits = c(WYstart, WYend+1), breaks = seq(WYstart, WYend+1, by = 5), expand = c(0.01,0.01)) +
  scale_y_continuous(limits = c(0,300), breaks = seq(0,300, by = 50), expand = c(0,0)) +
  scale_color_manual(values = brewer.pal(5, 'Set1')[-1], labels = c('2030','2070','2070DEW','2070WMW')) + 
  ylab('SW Inflow Volume (TAF)') +
  ggtitle('Projected Surface Water Inflow Volume') +
  theme_few() +
  theme_adj

ggsave(SW_Inflows_Annual_plot, filename = 'Future_Streamflow.png', device = 'png', width = 8.5, height = 5, units = 'in')
# SW Runoff ---------------------------------------------------------------
SW_Runoff_Annual = SW_annual_budgets %>%
  sapply('[[', 'Valley.Floor.Runoff') %>%
  as.data.frame() %>%
  mutate(WY = SW_annual_budgets[[1]][,1])
names(SW_Runoff_Annual) = c(scenarios, 'WY')


# SW Outflow  -------------------------------------------------------------
SW_Outflows_Annual = SW_annual_budgets %>%
  sapply('[[', 'GW.Basin.Outflow') %>%
  as.data.frame() %>%
  mutate(WY = SW_annual_budgets[[1]][,1])
names(SW_Outflows_Annual) = c(scenarios, 'WY')
# SW Diversions -----------------------------------------------------------
SW_Diversions_Annual = SW_annual_budgets %>%
  sapply('[[', 'SW.Diversions') %>%
  as.data.frame() %>%
  mutate(WY = SW_annual_budgets[[1]][,1])
names(SW_Diversions_Annual) = c(scenarios, 'WY')



# SW GW Exchange ----------------------------------------------------------
SW_GW_Exchange_Annual = SW_annual_budgets %>%
  sapply('[[', 'GW.SW.Exchange') %>%
  as.data.frame() %>%
  mutate(WY = SW_annual_budgets[[1]][,1])
names(SW_GW_Exchange_Annual) = c(scenarios, 'WY')


# SWBM Precipitation ------------------------------------------------------
SWBM_Precip_Annual = SWBM_annual_budgets %>%
  sapply('[[', 'Precip') %>%
  as.data.frame() %>%
  mutate(WY = SWBM_annual_budgets[[1]][,1])
names(SWBM_Precip_Annual) = c(scenarios, 'WY')

# SWBM Surface Water Irrigation ------------------------------------------------------
SWBM_SW_Irr_Annual = SWBM_annual_budgets %>%
  sapply('[[', 'SW.Irrigation') %>%
  as.data.frame() %>%
  mutate(WY = SWBM_annual_budgets[[1]][,1])
names(SWBM_SW_Irr_Annual) = c(scenarios, 'WY')

# SWBM Groundwater Irrigation ------------------------------------------------------
SWBM_GW_Irr_Annual = SWBM_annual_budgets %>%
  sapply('[[', 'GW.Irrigation') %>%
  as.data.frame() %>%
  mutate(WY = SWBM_annual_budgets[[1]][,1])
names(SWBM_GW_Irr_Annual) = c(scenarios, 'WY')

# SWBM ET ------------------------------------------------------
SWBM_ET_Annual = SWBM_annual_budgets %>%
  sapply('[[', 'ET') %>%
  as.data.frame() %>%
  mutate(WY = SWBM_annual_budgets[[1]][,1])
names(SWBM_ET_Annual) = c(scenarios, 'WY')

# SWBM Recharge ------------------------------------------------------
SWBM_Recharge_Annual = SWBM_annual_budgets %>%
  sapply('[[', 'Recharge') %>%
  as.data.frame() %>%
  mutate(WY = SWBM_annual_budgets[[1]][,1])
names(SWBM_Recharge_Annual) = c(scenarios, 'WY')

# SWBM Runoff ------------------------------------------------------
SWBM_Runoff_Annual = SWBM_annual_budgets %>%
  sapply('[[', 'Runoff') %>%
  as.data.frame() %>%
  mutate(WY = SWBM_annual_budgets[[1]][,1])
names(SWBM_Runoff_Annual) = c(scenarios, 'WY')

# SWBM Storage ------------------------------------------------------
SWBM_Storage_Annual = SWBM_annual_budgets %>%
  sapply('[[', 'Storage') %>%
  as.data.frame() %>%
  mutate(WY = SWBM_annual_budgets[[1]][,1])
names(SWBM_Storage_Annual) = c(scenarios, 'WY')



# GW Recharge --------------------------------------------------------------
GW_Recharge_Annual = MODFLOW_annual_budgets %>%
  sapply('[[', 'Recharge') %>%
  as.data.frame() %>%
  mutate(WY = MODFLOW_annual_budgets[[1]][,1])
names(GW_Recharge_Annual) = c(scenarios, 'WY')

# GW MFR --------------------------------------------------------------
GW_MFR_Annual = MODFLOW_annual_budgets %>%
  sapply('[[', 'MFR') %>%
  as.data.frame() %>%
  mutate(WY = MODFLOW_annual_budgets[[1]][,1])
names(GW_MFR_Annual) = c(scenarios, 'WY')


# GW Pumping --------------------------------------------------------------
GW_Pumping_Annual = MODFLOW_annual_budgets %>%
  sapply('[[', 'GW.Pumping') %>%
  as.data.frame() %>%
  mutate(Date = MODFLOW_annual_budgets[[1]][,1])
names(GW_Pumping_Annual) = c(scenarios, 'WY')

GW_Pumping_Plot = GW_Pumping_Annual %>%
  pivot_longer(cols = -WY, names_to = 'Climate Scenario', values_to = 'GW Pumping (TAF)') %>%
  ggplot(aes(x = WY, y = -`GW Pumping (TAF)`, color = `Climate Scenario`, fill = `Climate Scenario`)) +
  geom_line(size = 1) +
  geom_point(size = 1.5) +
  scale_x_continuous(limits = c(WYstart, WYend+1), breaks = seq(WYstart, WYend+1, by = 5), expand = c(0.01,0.01)) +
  scale_y_continuous(limits = c(0,20), breaks = seq(0,20, by = 5), expand = c(0,0)) +
  scale_color_manual(values = brewer.pal(5, 'Set1')[-1], labels = c('2030','2070','2070DEW','2070WMW')) + 
  ylab('Groundwater Pumping Volume (TAF)') +
  ggtitle('Projected Groundwater Pumping Volume') +
  theme_few() +
  theme_adj

ggsave(GW_Pumping_Plot, filename = 'Future_Pumping.png', device = 'png', width = 8.5, height = 5, units = 'in')

# GW ET --------------------------------------------------------------
GW_ET_Annual = MODFLOW_annual_budgets %>%
  sapply('[[', 'ET') %>%
  as.data.frame() %>%
  mutate(Date = MODFLOW_annual_budgets[[1]][,1])
names(GW_ET_Annual) = c(scenarios, 'WY')


# GW SW Exchange ----------------------------------------------------------
GW_SW_Exchange_Annual = MODFLOW_annual_budgets %>%
  sapply('[[', 'GW.SW.Exchange') %>%
  as.data.frame() %>%
  mutate(Date = MODFLOW_annual_budgets[[1]][,1])
names(GW_SW_Exchange_Annual) = c(scenarios, 'WY')

# GW Storage --------------------------------------------------------------
GW_Storage_Annual = MODFLOW_annual_budgets %>%
  sapply('[[', 'Storage') %>%
  as.data.frame() %>%
  mutate(WY = MODFLOW_annual_budgets[[1]][,1])
names(GW_Storage_Annual) = c(scenarios, 'WY')

GW_Storage_Cumulative = GW_Storage_Annual %>%
  mutate(across(.cols = -WY, .fns = function(x){cumsum(x) - first(x)}))

GW_Storage_Cumulative_Annual_Plot = GW_Storage_Cumulative %>%
  pivot_longer(cols = -WY, names_to = 'Scenario', values_to = 'Storage (TAF)') %>%
  ggplot(aes(x = WY, y = -`Storage (TAF)`, color = Scenario, fill = Scenario)) +
  geom_hline(yintercept = 0, color = 'black', size = 0.5) +
  geom_line(size = 0.75) +
  geom_point(size = 1.25) +
  geom_rect(data = WYtypes, inherit.aes = F, aes(xmin = plot_annual_start, xmax = plot_annual_end, ymin = MF_plot_range[1], ymax = MF_plot_range[1] + MF_plot_range[2]*0.05),
            fill = WYtypes$plot_color , color = 'black', size = 0.2, show.legend = F) +
  geom_text(data = WYtypes, inherit.aes = F, aes(x = annual_label_date, y = MF_plot_range[1]*0.975, label = Type), size = 1.5) +
  scale_x_continuous(limits = c(WYstart, WYend+1), breaks = seq(WYstart, WYend+1, by = 5), expand = c(0.01,0.01)) +
  scale_y_continuous(limits = c(-50,50), breaks = seq(-50,50, by = 10), expand = c(0,0)) +
  scale_color_manual(values = brewer.pal(5, 'Set1')[-1],
                     labels = c('Muncipal Pumping Only', '75% Ag Reduction', '50% Ag Reduction', '25% Ag Reduction')) + 
  scale_fill_manual(values = c(brewer.pal(5, 'Set1')[-1]),
                    labels = c('Muncipal Pumping Only', '75% Ag Reduction', '50% Ag Reduction', '25% Ag Reduction')) + 
  ggtitle('Cumulative Groundwater Storage Change') +
  ylab('Change in Storage (TAF)') +
  theme_few() +
  theme_adj +
  theme(legend.position = 'right')

ggsave(plot = GW_Storage_Cumulative_Annual_Plot, filename = paste0(scenario_prefix,'Cumulative_Storage.png'),
       device = 'png', width = 8.125, height = 5, units = 'in', dpi = 300)

# Eastside Westside Storage Comparison ------------------------------------
Eastside_Westside_Storage = Eastside_Westside_annual %>%
  sapply('[[', 'Storage') %>%
  as.data.frame() %>%
  mutate(WY = Eastside_Westside_annual[[1]][,'WY'],
         Zone = Eastside_Westside_annual[[1]][,'Geographic.Area'])
names(Eastside_Westside_Storage) = c(scenarios, 'WY', 'Zone')

Eastside_Westside_Storage_Cumulative = Eastside_Westside_Storage %>%
  group_by(Zone) %>%
  mutate(across(.cols = -WY, .fns = function(x){cumsum(x) - first(x)}))

Eastside_Storage_Cumulative_Plot = Eastside_Westside_Storage_Cumulative %>%
  filter(Zone=='Eastside') %>%
  ungroup() %>%
  dplyr::select(-Zone) %>%
  pivot_longer(cols = -WY, names_to = 'Scenario', values_to = 'Storage (acre-ft/day)') %>%
  ggplot(aes(x = WY, y = -`Storage (acre-ft/day)`, color = Scenario, fill = Scenario)) +
  geom_hline(yintercept = 0, color = 'black', size = 0.75) +
  geom_line(size = 0.75) +
  geom_point(size = 1.25) +
  scale_x_continuous(limits = c(WYstart, WYend+1), breaks = seq(WYstart, WYend+1, by = 5), expand = c(0.01,0.01)) +
  scale_y_continuous(limits = c(-80,80), breaks = seq(-80,80, by = 20), expand = c(0,0)) +
  scale_color_manual(values = brewer.pal(5, 'Set1')[-1],
                     labels = c('Muncipal Pumping Only', '75% Ag Reduction', '50% Ag Reduction', '25% Ag Reduction')) + 
  scale_fill_manual(values = brewer.pal(5, 'Set1')[-1],
                    labels = c('Muncipal Pumping Only', '75% Ag Reduction', '50% Ag Reduction', '25% Ag Reduction')) + 
  annotate(geom = 'text', x = 2022, y=-190, label = 'Eastside', size = 6, hjust = 0, vjust = 0) +
  ggtitle(' Cumulative Change in Storage Rate') +
  ylab('Cumulative Average Daily Rate\n(acre-ft/day)') +
  theme_few() +
  theme_adj +
  theme(legend.justification = 'center',
        legend.position = 'right',
        legend.title = element_blank())

Westside_Storage_Cumulative_Plot = Eastside_Westside_Storage_Cumulative %>%
  filter(Zone=='Westside') %>%
  ungroup() %>%
  dplyr::select(-Zone) %>%
  pivot_longer(cols = -WY, names_to = 'Scenario', values_to = 'Storage (acre-ft/day)') %>%
  ggplot(aes(x = WY, y = -`Storage (acre-ft/day)`, color = Scenario, fill = Scenario)) +
  geom_hline(yintercept = 0, color = 'black', size = 0.75) +
  geom_line(size = 0.75) +
  geom_point(size = 1.25) +
  scale_x_continuous(limits = c(WYstart, WYend+1), breaks = seq(WYstart, WYend+1, by = 5), expand = c(0.01,0.01)) +
  scale_y_continuous(limits = c(-80,80), breaks = seq(-80,80, by = 20), expand = c(0,0)) +
  scale_color_manual(values = brewer.pal(5, 'Set1')[-1],
                     labels = c('Muncipal Pumping Only', '75% Ag Reduction', '50% Ag Reduction', '25% Ag Reduction')) + 
  scale_fill_manual(values = brewer.pal(5, 'Set1')[-1],
                    labels = c('Muncipal Pumping Only', '75% Ag Reduction', '50% Ag Reduction', '25% Ag Reduction')) + 
  annotate(geom = 'text', x = 2022, y=-190, label = 'Westside', size = 6, hjust = 0, vjust = 0) +
  ggtitle('Cumulative Change in Storage Rate') +
  ylab('Cumulative Average Daily Rate\n(acre-ft/day)') +
  theme_few() +
  theme_adj +
  theme(legend.justification = 'center',
        legend.position = 'right',
        legend.title = element_blank())

ggsave(plot = Eastside_Storage_Cumulative_Plot, filename = paste0(scenario_prefix,'Eastside_Cumulative_Storage.png'),
       width = 8.5, height = 4, units = 'in', device = 'png', dpi = 300)
ggsave(plot = Westside_Storage_Cumulative_Plot, filename = paste0(scenario_prefix,'Westside_Cumulative_Storage.png'),
       width = 8.5, height = 4, units = 'in', device = 'png', dpi = 300)

# Eastside Upper Lower Storage Comparison ------------------------------------
Eastside_Upper_Lower_Storage = Eastside_Upper_Lower_annual %>%
  sapply('[[', 'Storage') %>%
  as_tibble() %>%
  mutate(WY = Eastside_Upper_Lower_annual[[1]][,'WY'],
         Zone = Eastside_Upper_Lower_annual[[1]][,'Geographic.Area'])
names(Eastside_Upper_Lower_Storage) = c(scenarios, 'WY', 'Zone')

Eastside_Upper_Lower_Storage_Cumulative = Eastside_Upper_Lower_Storage %>%
  group_by(Zone) %>%
  mutate(across(.cols = -WY, .fns = function(x){cumsum(x) - first(x)}))

Eastside_Upper_Storage_Cumulative_Plot = Eastside_Upper_Lower_Storage_Cumulative %>%
  filter(Zone=='Eastside Upper Aquifer') %>%
  ungroup() %>%
  dplyr::select(-Zone) %>%
  pivot_longer(cols = -WY, names_to = 'Scenario', values_to = 'Storage (acre-ft/day)') %>%
  ggplot(aes(x = WY, y = -`Storage (acre-ft/day)`, color = Scenario, fill = Scenario)) +
  geom_hline(yintercept = 0, color = 'black', size = 0.75) +
  geom_line(size = 0.75) +
  geom_point(size = 1.25) +
  scale_x_continuous(limits = c(WYstart, WYend+1), breaks = seq(WYstart, WYend+1, by = 5), expand = c(0.01,0.01)) +
  scale_y_continuous(limits = c(-150,50), breaks = seq(-150,50, by = 50), expand = c(0,0)) +
  scale_color_manual(values = brewer.pal(5, 'Set1')[-1],
                     labels = c('Muncipal Pumping Only', '75% Ag Reduction', '50% Ag Reduction', '25% Ag Reduction')) + 
  scale_fill_manual(values = brewer.pal(5, 'Set1')[-1],
                    labels = c('Muncipal Pumping Only', '75% Ag Reduction', '50% Ag Reduction', '25% Ag Reduction')) + 
  annotate(geom = 'text', x = 2022, y=-140, label = 'Eastside Upper Aquifer', size = 6, hjust = 0, vjust = 0) +
  ggtitle('Cumulative Change in Storage Rate') +
  ylab('Cumulative Average Daily Rate\n(acre-ft/day)') +
  theme_few() +
  theme_adj +
  theme(legend.position = 'right',
        legend.title = element_blank())

Eastside_Lower_Storage_Cumulative_Plot = Eastside_Upper_Lower_Storage_Cumulative %>%
  filter(Zone=='Eastside Lower Aquifer') %>%
  ungroup() %>%
  dplyr::select(-Zone) %>%
  pivot_longer(cols = -WY, names_to = 'Scenario', values_to = 'Storage (acre-ft/day)') %>%
  ggplot(aes(x = WY, y = -`Storage (acre-ft/day)`, color = Scenario, fill = Scenario)) +
  geom_hline(yintercept = 0, color = 'black', size = 0.75) +
  geom_line(size = 0.75) +
  geom_point(size = 1.25) +
  geom_rect(data = WYtypes, inherit.aes = F, aes(xmin = plot_annual_start, xmax = plot_annual_end, ymin = MF_plot_range[1], ymax = MF_plot_range[1] + MF_plot_range[2]*0.05),
            fill = WYtypes$plot_color , color = 'black', size = 0.2, show.legend = F) +
  geom_text(data = WYtypes, inherit.aes = F, aes(x = annual_label_date, y = MF_plot_range[1]*0.975, label = Type), size = 1.5) +
  scale_x_continuous(limits = c(WYstart, WYend+1), breaks = seq(WYstart, WYend+1, by = 5), expand = c(0.01,0.01)) +
  scale_y_continuous(limits = c(-50,100), breaks = seq(-50,100, by = 25), expand = c(0,0)) +
  scale_color_manual(values = brewer.pal(5, 'Set1')[-1],
                     labels = c('Muncipal Pumping Only', '75% Ag Reduction', '50% Ag Reduction', '25% Ag Reduction')) + 
  scale_fill_manual(values = brewer.pal(5, 'Set1')[-1],
                    labels = c('Muncipal Pumping Only', '75% Ag Reduction', '50% Ag Reduction', '25% Ag Reduction')) + 
  annotate(geom = 'text', x = 2000.5, y=-35, label = 'Eastside Lower Aquifer', size = 6, hjust = 0, vjust = 0) +
  ggtitle('Cumulative Change in Storage Rate') +
  ylab('Cumulative Average Daily Rate\n(acre-ft/day)') +
  theme_few() +
  theme_adj +
  theme(legend.position = 'right',
        legend.title = element_blank())

ggsave(plot = Eastside_Upper_Storage_Cumulative_Plot, filename = paste0(scenario_prefix,'Eastside_Upper_Cumulative_Storage.png'),
       width = 8.5, height = 4, units = 'in', device = 'png', dpi = 300)
ggsave(plot = Eastside_Lower_Storage_Cumulative_Plot, filename = paste0(scenario_prefix,'Eastside_Lower_Cumulative_Storage.png'),
       width = 8.5, height = 4, units = 'in', device = 'png', dpi = 300)
