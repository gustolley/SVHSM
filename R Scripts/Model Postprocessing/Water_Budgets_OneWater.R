# General Comments --------------------------------------------------------

# Units of imported budget files are assumed to be m^3
# Fluxes for constant head boundary conditions are read in but not included in plots since there are no constant head boundary conditions in SVIHM

# User Input --------------------------------------------------------------
modelDir = ''
SWBMbudgetFile = paste0(modelDir, 'monthly_water_budget.dat')
MFbudgetFile = paste0(modelDir, 'MODFLOW_Budget.dat')
streamflowSim_file = paste0(modelDir, 'SVHSM_streamflow_UMFFR.dat')
resultsDir = paste0(modelDir, 'Results/')

WYstart = 2000                                   # Beginning water year of simulation
WYend = 2020                                     # Ending water year of simulation
budget_interval = 'month'
Dry_Avg_Wet_Yrs = c(2001,2010,2006)
fields = 'SWBM_Fields.shp'
fieldsMFgrid = 'SWBM_Fields_MF_grid.shp'
GWB = 'Sierra_Valley_Groundwater_Basin_Outline.shp'

fig_format = 'png'               #output format for figures (pdf, png, or jpg)
fig_width = 7           
fig_height = 5
fig_units = 'in'

DryAvgWet_Years = data.frame(WY = c(2020,2010,2017),
                             Type = c('Dry','Average','Wet'))

SWBM_plot_range = c(-600,600)
SWBM_plot_int = 200
MF_plot_range = c(-100,100)
MF_plot_int = 25
SW_plot_range = c(-800,800)
SW_plot_int = 200


ZB_EW_pumping_range = c(0,40)
ZB_EW_pumping_int = 10
ZB_EW_net_rch_range = c(-120,120)
ZB_EW_net_rch_int = 30
ZB_EW_exchange_range = c(-1.5,1.5)
ZB_EW_exchange_int = 0.5

ZB_E_UL_pumping_range = c(0,25)
ZB_E_UL_pumping_int = 5
ZB_E_UL_net_rch_range = c(-100,100)
ZB_E_UL_net_rch_int = 25
ZB_E_UL_exchange_range = c(-10,10)
ZB_E_UL_exchange_int = 2.5

# Script Initialization ---------------------------------------------------

options(warn=-1)   # suppress warnings (set to 0 to turn warnings on)
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggthemes)
library(plotly)
library(RColorBrewer)
library(cowplot)
library(lubridate)
dir.create(resultsDir)   #Create Results directory if it doesn't exist

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

# geom_rect(data = WYtypes, aes(xmin = WY_Start_Dates, xmax = WY_End_Dates, ymin = WLE_min, ymax = WLE_min + (WLE_max-WLE_min)/30), fill = WYtypes$plot_color, color = 'black', size = 0.2, show.legend = F) +

# Functions ---------------------------------------------------------------


# General ggplot Options --------------------------------------------------
barplot_theme = theme(legend.title=element_blank(),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
      panel.background = element_rect(color = 'black', fill = NA),
      panel.grid.major.y = element_line(color = 'gray80'),
      panel.border = element_rect(color = 'black', fill = NA, size = 1),
      plot.background = element_rect(color = NA, fill = NA),
      axis.text.x = element_text(angle = 45, hjust = 1, vjust = 0.7), 
      axis.text = element_text(size = 8),
      axis.title.x = element_blank(),
      plot.title = element_text(hjust = 0.5),
      legend.position = c(0, 1), 
      legend.justification = c('left', 'top'),
      legend.key = element_rect(fill = NA, color = NA),
      legend.background = element_rect(fill = NA, color = NA),
      legend.direction = 'horizontal',
      legend.box='horizontal',
      legend.text = element_text(size = 6, margin = margin(r=1,l=1, unit = 'pt')),
      legend.key.height = unit(10,'pt'),
      legend.spacing = unit(0, 'pt'))

# SFR SURFACE WATER INPUTS (NEEDS UPDATING) --------------------------------------------
#source('SVIHM_SFR_inputs.R')

# SWBM Budgets ----------------------------------------------------
SWBM_Monthly = read.table(SWBMbudgetFile, header = T) %>%
  rename(`SW Irrigation` = SW_Irr, `GW Irrigation` = GW_Irr,) %>%
  mutate(WY = rep(seq(WYstart, WYend),each = 12))

SWBM_Monthly_TAF = SWBM_Monthly[,-10]
SWBM_Monthly_TAF[,-1] = SWBM_Monthly_TAF[,-1]*0.000000810714

SWBM_Monthly_long = SWBM_Monthly %>%
  dplyr::select(-Error) %>%
  pivot_longer(cols = c(-Stress_Period,-WY), names_to = 'Budget_Term', values_to = 'value') %>%
  mutate(Budget_Term = factor(Budget_Term, levels = c('Precip', 'SW Irrigation', 'GW Irrigation', 'ET', 'Recharge', 'Runoff', 'Storage'), ordered = T))

SWBM_Annual_TAF = SWBM_Monthly %>%
  dplyr::select(-Stress_Period) %>%
  group_by(WY) %>%
  summarize_all(.funs = sum)
SWBM_Annual_TAF[,-1] = SWBM_Annual_TAF[,-1]*0.000000810714

SWBM_Annual_TAF_long = SWBM_Monthly_long %>%
  group_by(WY, Budget_Term) %>%
  summarize(value = sum(value)*0.000000810714) %>%
  ungroup()
  
SWBM_ann_running_storage = SWBM_Annual_TAF_long %>% 
  filter(Budget_Term=='Storage') %>%
  dplyr::select(value) %>% 
  cumsum() %>% 
  mutate(WY = unique(SWBM_Annual_TAF$WY),
         value = value - .$value[2])


SWBM_Annual_Barplot = ggplot(data = NULL) +
  geom_bar(data = SWBM_Annual_TAF_long, mapping = aes(x = WY, y = value, fill = Budget_Term), position = "stack", stat = 'identity', color = 'black', width = 0.95, size = 0.1) +
  scale_x_continuous(limits = c(WYstart-0.6,WYend+0.6), 
                     breaks = seq(format(WYstartDate,'%Y')%>% as.numeric()+1,format(WYendDate,'%Y')%>% as.numeric(), by = 2),
                     expand = c(0,0))  +
  scale_y_continuous(limits = SWBM_plot_range, breaks = seq(SWBM_plot_range[1],SWBM_plot_range[2],by = SWBM_plot_int), expand = c(0,0)) +
  ylab('Annual Volume (TAF)') +
  ggtitle('Annual Soil Water Budget') +
  geom_rect(data = WYtypes, aes(xmin = plot_annual_start, xmax = plot_annual_end, ymin = SWBM_plot_range[1], ymax = SWBM_plot_range[1] + SWBM_plot_range[2]*0.05),
            fill = WYtypes$plot_color , color = 'black', size = 0.2, show.legend = F) +
  geom_text(data = WYtypes, aes(x = annual_label_date, y = SWBM_plot_range[1]*0.975, label = Type), size = 1.5) +
  geom_line(data = SWBM_ann_running_storage, aes(x = WY, y = value, color = 'Cumulative Storage'), size = 0.75) +
  geom_point(data = SWBM_ann_running_storage, aes(x = WY, y = value, color = 'Cumulative Storage'), fill = 'black', size = 1.5, shape = 21) +
  scale_fill_manual(values = c('skyblue', 'orange', 'gold4','red','navyblue','aquamarine','gray75')) +
  scale_color_manual(values = 'black') +
  barplot_theme +
  guides(fill = guide_legend(order = 1), col = guide_legend(order = 2))
ggsave(SWBM_Annual_Barplot, filename = paste0(resultsDir,'SWBM_Annual_Barplot.',fig_format), device = fig_format, width = fig_width, height = fig_height, units = fig_units)

#Export Budget Tables
write.csv(SWBM_Monthly_TAF %>%
            mutate(Stress_Period = modelMonths) %>%
            rename(Month = Stress_Period), file = paste0(resultsDir,'Soil_Zone_Budget_Monthly_TAF.csv'), append = F, quote = F, row.names = F, col.names = T)
write.csv(SWBM_Annual_TAF, file = paste0(resultsDir,'Soil_Zone_Budget_Annual_TAF.csv'), append = F, quote = F, row.names = F, col.names = T)

# MODFLOW Budgets ----------------------------------------------------
MODFLOW_Monthly_TAF = read.table(MFbudgetFile, header = T) %>%
  mutate(Storage = (STORAGE_IN - STORAGE_OUT)*0.000000810714,
         MFR = (WEL_IN - WEL_OUT)*0.000000810714,
         Recharge = (RCH_IN - RCH_OUT)*0.000000810714,
         ET = (ETS_IN - ETS_OUT)*0.000000810714,
         `GW-SW Exchange` = (SFR_IN - SFR_OUT)*0.000000810714,
         `GW Pumping` = (MNW2_IN - MNW2_OUT)*0.000000810714) %>%
  dplyr::select(PER, Recharge, ET, MFR, `GW-SW Exchange`, `GW Pumping`, Storage) %>%
  group_by(PER) %>%
  summarize_all(sum)
  
MODFLOW_Annual_TAF = MODFLOW_Monthly_TAF %>%
  mutate(WY = rep(seq(WYstart, WYend), each = 12)) %>%
  group_by(WY) %>%
  summarise_all(.funs = sum) %>% 
  dplyr::select(-PER)

MODFLOW_Monthly_TAF %>%
  pivot_longer(cols = -PER, names_to = 'WB Component', values_to = 'Flux (TAF)') %>%
  plot_ly(x = ~PER, y = ~`Flux (TAF)`, color = ~`WB Component`, type = 'bar') %>%
  layout(barmode = 'relative')

MODFLOW_Annual_TAF %>%
  pivot_longer(cols = -WY, names_to = 'WB Component', values_to = 'Flux (TAF)') %>%
  plot_ly(x = ~WY, y = ~`Flux (TAF)`, color = ~`WB Component`, type = 'bar') %>%
  layout(barmode = 'relative')

MODFLOW_mon_running_storage = MODFLOW_Monthly_TAF %>% 
  pivot_longer(cols = -PER, names_to = 'WB Component', values_to = 'Flux (TAF)') %>%
  filter(`WB Component`=='Storage') %>%
  dplyr::select(`Flux (TAF)`) %>% 
  cumsum() %>% 
  mutate(Date = modelMonths,
         Month = format(Date, '%b'),
         WY = if_else(Month%in%month.abb[10:12], format(Date,'%Y') %>% as.numeric() + 1, format(Date,'%Y') %>% as.numeric()),
         `Flux (TAF)` = `Flux (TAF)` - .$`Flux (TAF)`[1])

# for (i in 1:nrow(DryAvgWet_Years)) {
#   MODFLOW_Monthly_Barplot = ggplot(data = MODFLOW_Monthly_TAF %>% filter(Budget_Term != 'CONSTANT HEAD', WY == DryAvgWet_Years$WY[i]), aes(x = Month, y = Flux_net_TAF )) +
#     geom_bar(aes(fill = Budget_Term), position = "stack", stat = 'identity', color = 'black', width = 0.95, size = 0.1) +
#     scale_y_continuous(limits = c(-150,150), breaks = seq(-150,150,by = 30), expand = c(0,0)) +
#     ylab('Volume (TAF)') +
#     ggtitle(paste0('Monthly Groundwater Budget for ',DryAvgWet_Years$Type[i],' Year (',DryAvgWet_Years$WY[i],')')) +
#     scale_fill_brewer(type = 'div', palette = 'Set1') +
#     monthly_MF_barplot_theme
#   ggsave(MODFLOW_Monthly_Barplot, filename = paste0(resultsDir,'MODFLOW_Monthly_Barplot_',DryAvgWet_Years$Type[i],'.',fig_format), device = fig_format, width = fig_width, height = fig_height, units = fig_units)
# }
  
#MODFLOW_Monthly_TAF %>% plot_ly(x = rep(modelDays, each = 7), y = ~Flux_net_TAF, color = ~Budget_Term, type = 'scatter', mode = 'lines+markers')


MODFLOW_ann_running_storage = MODFLOW_Annual_TAF %>% 
  pivot_longer(cols = -WY, names_to = 'WB Component', values_to = 'Flux (TAF)') %>%
  filter(`WB Component`=='Storage') %>%
  dplyr::select(`Flux (TAF)`) %>% 
  cumsum() %>% 
  mutate(WY = MODFLOW_Annual_TAF$WY,
         `Flux (TAF)` = (`Flux (TAF)` - .$`Flux (TAF)`[1])*-1)    # change sign for plotting purposes (line goes up when storage increases)

MODFLOW_Annual_Barplot =  ggplot(data = NULL) +
  geom_bar(data = MODFLOW_Annual_TAF %>%
             pivot_longer(cols = -WY, names_to = 'WB Component', values_to = 'Flux (TAF)') %>%
             mutate(`WB Component` = factor(`WB Component`, levels = c('Recharge', 'GW-SW Exchange', 'MFR', 'GW Pumping', 'ET', 'Storage'), ordered = T)),
           mapping = aes(x = WY, y = `Flux (TAF)`, fill = `WB Component`), position = "stack", stat = 'identity', color = 'black', width = 0.95, size = 0.1) +
  scale_x_continuous(limits = c(WYstart-0.6,WYend+0.6), 
                     breaks = seq(format(WYstartDate,'%Y')%>% as.numeric()+1,format(WYendDate,'%Y')%>% as.numeric(), by = 2),
                     expand = c(0,0))  +
  scale_y_continuous(limits = MF_plot_range, breaks = seq(MF_plot_range[1],MF_plot_range[2],by = MF_plot_int), expand = c(0,0)) +
  ylab('Annual Volume (TAF)') +
  ggtitle('Annual Aquifer Water Budget') +
  geom_rect(data = WYtypes, aes(xmin = plot_annual_start, xmax = plot_annual_end, ymin = MF_plot_range[1], ymax = MF_plot_range[1] + MF_plot_range[2]*0.05),
            fill = WYtypes$plot_color , color = 'black', size = 0.2, show.legend = F) +
  geom_text(data = WYtypes, aes(x = annual_label_date, y = MF_plot_range[1]*0.975, label = Type), size = 1.5) +
  geom_line(data = MODFLOW_ann_running_storage, aes(x = WY, y = `Flux (TAF)`, color = 'Cumulative Storage'), size = 0.75) +
  geom_point(data = MODFLOW_ann_running_storage, aes(x = WY, y = `Flux (TAF)`, color = 'Cumulative Storage'), fill = 'black', size = 1.5, shape = 21) +
  #scale_fill_brewer(type = 'div', palette = 'Set1') +
  scale_fill_manual(values = c('navyblue', 'deepskyblue', 'darkorchid','gold4','red','gray75')) +
  scale_color_manual(values = 'black') +
  barplot_theme +
  guides(fill = guide_legend(order = 1), col = guide_legend(order = 2))
ggsave(MODFLOW_Annual_Barplot, filename = paste0(resultsDir,'MODFLOW_Annual_Barplot.',fig_format), device = fig_format, width = fig_width, height = fig_height, units = fig_units)

#Export Budget Tables
write.csv(MODFLOW_Monthly_TAF %>%
            mutate(PER = modelMonths) %>%
            rename(Month = PER), file = paste0(resultsDir,'Groundwater_Budget_Monthly_TAF.csv'), append = F, quote = F, row.names = F, col.names = T)
write.csv(MODFLOW_Annual_TAF, file = paste0(resultsDir,'Groundwater_Budget_Annual_TAF.csv'), append = F, quote = F, row.names = F, col.names = T)

# Surface-Water Budgets ----------------------------------------------------
Irrigation_inflows_monthly = read.table(paste0(modelDir,'subwatershed_irrigation_inflows.txt'), skip = 1, header = F) %>%
  dplyr::select(-V1) %>%
  rowSums()*0.000000810714
  
Irrigation_inflows_annual = read.table(paste0(modelDir,'subwatershed_irrigation_inflows.txt'), skip = 1, header = F) %>%
  mutate(Date = as.Date(paste0('01-',V1), '%d-%b-%Y'),
         WY = if_else(format(Date, '%b')%in%month.abb[10:12], format(Date,'%Y') %>% as.numeric() + 1, format(Date,'%Y') %>% as.numeric())) %>%
  dplyr::select(-V1, -Date) %>%
  group_by(WY) %>%
  summarise_at(which(names(.) != 'WY'), sum) %>%
  dplyr::select(-WY) %>%
  rowSums()*0.000000810714

nonIrrigation_inflows_monthly = read.table(paste0(modelDir,'subwatershed_nonIrrigation_inflows.txt'), skip = 1, header = F) %>%
  dplyr::select(-V1) %>%
  rowSums()*0.000000810714

nonIrrigation_inflows_annual = read.table(paste0(modelDir,'subwatershed_nonIrrigation_inflows.txt'), skip = 1, header = F) %>%
  mutate(Date = as.Date(paste0('01-',V1), '%d-%b-%Y'),
         WY = if_else(format(Date, '%b')%in%month.abb[10:12], format(Date,'%Y') %>% as.numeric() + 1, format(Date,'%Y') %>% as.numeric())) %>%
  dplyr::select(-V1, -Date) %>%
  group_by(WY) %>%
  summarise_at(which(names(.) != 'WY'), sum) %>%
  dplyr::select(-WY) %>%
  rowSums()*0.000000810714

Streamflow_out_monthly = read.table(streamflowSim_file, skip = 2,  header = F) %>%
  mutate(Date = WYstartDate + V1 - 1,
         SP = format(Date, '%b-%Y'),
         WY = if_else(format(Date, '%b')%in%month.abb[10:12], format(Date,'%Y') %>% as.numeric() + 1, format(Date,'%Y') %>% as.numeric()),
         vol_TAF = V6*0.000000810714) %>%
  group_by(SP) %>%
  summarize(outflow_vol_TAF = sum(vol_TAF)) %>%
  mutate(Date = as.Date(paste0('01-',SP), '%d-%b-%Y')) %>%
  arrange(Date) %>%
  relocate(Date) %>%
  dplyr::select(-SP)

Streamflow_out_annual = Streamflow_out_monthly %>%
  mutate(WY = if_else(format(Date, '%b')%in%month.abb[10:12], 
                      format(Date,'%Y') %>% as.numeric() + 1, 
                      format(Date,'%Y') %>% as.numeric())) %>%
  group_by(WY) %>%
  summarize(outflow_vol_TAF = sum(outflow_vol_TAF))

SW_monthly_TAF = data.frame(Date = modelMonths,
                        `GW Basin Inflow` = Irrigation_inflows_monthly + nonIrrigation_inflows_monthly,
                        `SW Diversions` = -SWBM_Monthly$`SW Irrigation`*0.000000810714,
                        `GW-SW Exchange` = -MODFLOW_Monthly_TAF$`GW-SW Exchange`,
                        `Valley Floor Runoff` = -SWBM_Monthly$Runoff*0.000000810714,
                        `GW Basin Outflow` = -Streamflow_out_monthly$outflow_vol_TAF, 
                        check.names = F) %>%
  mutate(`Channel Storage/Error` = `GW Basin Inflow` + `SW Diversions` + `GW-SW Exchange` + `Valley Floor Runoff` + `GW Basin Outflow`)

SW_Annual_TAF = SW_monthly_TAF %>%
  mutate(WY = if_else(format(Date,'%b')%in%month.abb[1:9],      # populate water year
                      format(Date, '%Y') %>% as.numeric(),
                      format(Date, '%Y') %>% as.numeric()+1)) %>%
  dplyr::select(-Date) %>%
  group_by(WY) %>%
  summarise_all(.funs = sum)

SW_cumulative_storage_annual = SW_Annual_TAF %>%
  dplyr::select(WY, `Channel Storage/Error`) %>%
  mutate(`Channel Storage/Error` = cumsum(`Channel Storage/Error`))
SW_cumulative_storage_annual$`Channel Storage/Error` = SW_cumulative_storage_annual$`Channel Storage/Error` - SW_cumulative_storage_annual$`Channel Storage/Error`[1]


sfr = read.table(paste0(modelDir,'SVHSM.sfr'), skip = 2370, header = F, fill = T) %>% 
  na.omit() %>% 
  mutate(SP = rep(seq(1,252), each = 51),
         WY = rep(seq(2000,2020), each = 612),
         ndays = rep(days_in_month(modelMonths), each = 51),
         total_inflow = 0,
         inflow_rate = if_else(V5<0, 0, V5),
         runoff_rate = if_else(V5<0, V7, V6),
         inflow_TAF = inflow_rate*ndays*0.000000810714,
         runoff_TAF = runoff_rate*ndays*0.000000810714) %>%
  group_by(SP) %>%
  summarize(inflow_TAF = sum(inflow_TAF),
            runoff_TAF = sum(runoff_TAF))


SW_Annual_Barplot =  ggplot(data = NULL) +
  geom_bar(data = SW_Annual_TAF %>%
             pivot_longer(cols = -WY, names_to = 'WB Component', values_to = 'Flux (TAF)') %>%
             mutate(`WB Component` = factor(`WB Component`, levels = c('GW Basin Inflow', 'SW Diversions', 'GW-SW Exchange', 'Valley Floor Runoff', 'GW Basin Outflow', 'Channel Storage/Error'), ordered = T)),
           mapping = aes(x = WY, y = `Flux (TAF)`, fill = `WB Component`), position = "stack", stat = 'identity', color = 'black', width = 0.95, size = 0.1) +
  scale_x_continuous(limits = c(WYstart-0.6,WYend+0.6), 
                     breaks = seq(format(WYstartDate,'%Y')%>% as.numeric()+1,format(WYendDate,'%Y')%>% as.numeric(), by = 2),
                     expand = c(0,0))  +
  scale_y_continuous(limits = SW_plot_range, breaks = seq(SW_plot_range[1],SW_plot_range[2],by = SW_plot_int), expand = c(0,0)) +
  ylab('Annual Volume (TAF)') +
  ggtitle('Annual Surface Water Budget') +
  geom_rect(data = WYtypes, aes(xmin = plot_annual_start, xmax = plot_annual_end, ymin = SW_plot_range[1], ymax = SW_plot_range[1] + SW_plot_range[2]*0.05),
            fill = WYtypes$plot_color , color = 'black', size = 0.2, show.legend = F) +
  geom_text(data = WYtypes, aes(x = annual_label_date, y = SW_plot_range[1]*0.975, label = Type), size = 1.5) +
  #geom_line(data = SW_cumulative_storage_annual, aes(x = WY, y = `Channel Storage/Error`, color = 'Cumulative Channel Storage/Error'), size = 0.75) +
  #geom_point(data = SW_cumulative_storage_annual, aes(x = WY, y = `Channel Storage/Error`, color = 'Cumulative Channel Storage/Error'), fill = 'black', size = 1.5, shape = 21) +
  #scale_fill_brewer(type = 'div', palette = 'Set1') +
  scale_fill_manual(values = c('blue', 'green', 'darkorchid4','orange','deepskyblue','gray75')) +
  #scale_color_manual(values = 'black') +
  barplot_theme +
  guides(fill = guide_legend(order = 1), col = guide_legend(order = 2))
ggsave(SW_Annual_Barplot, filename = paste0(resultsDir,'SW_Annual_Barplot.',fig_format), device = fig_format, width = fig_width, height = fig_height, units = fig_units)


#Export Budget Tables
write.csv(SW_monthly_TAF %>%
            rename(Month = Date), file = paste0(resultsDir,'Surface-Water_Budget_Monthly_TAF.csv'), append = F, quote = F, row.names = F, col.names = T)
write.csv(SW_Annual_TAF, file = paste0(resultsDir,'Surface-Water_Budget_Annual_TAF.csv'), append = F, quote = F, row.names = F, col.names = T)



# Combined Water Budget Plots ----------------------------------------------------------
WB_stack = plot_grid(SW_Annual_Barplot + 
                       theme(axis.text.x = element_blank(),
                             plot.title = element_blank(),
                             legend.key.height = unit(1, 'mm'),
                             legend.key.width = unit(5, 'mm'),
                             legend.spacing.y = unit(-2,'mm')),
                     SWBM_Annual_Barplot + 
                       theme(axis.text.x = element_blank(),
                             plot.title = element_blank(),
                             legend.key.height = unit(1, 'mm'),
                             legend.key.width = unit(5, 'mm'),
                             legend.spacing.y = unit(-2,'mm')), 
                     MODFLOW_Annual_Barplot +
                       theme(plot.title = element_blank(),
                             legend.key.height = unit(1, 'mm'),
                             legend.key.width = unit(5, 'mm'),
                             legend.spacing.y = unit(-2,'mm')),ncol = 1)
ggsave(plot = WB_stack, filename = paste0(resultsDir,'WB_Stack.',fig_format), width = 6, height = 8.4, units = 'in', dpi = 300, device = fig_format)

# Pumping Comparisons -------------------------------------------------

# Simualted vs Observed
theme_adj = theme(plot.title = element_text(hjust = 0.5, size = 14),
                  panel.grid.major.y = element_line(size = 0.1, color = 'gray80'),
                  axis.title = element_text(size = 13),
                  axis.title.x = element_blank(),
                  axis.text = element_text(size = 12),
                  axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
                  legend.title = element_blank(),
                  legend.position = c(0,1.01),
                  legend.justification = c('left', 'top'),
                  legend.background = element_rect(color = NA, fill = NA))

SWBM_pumping = read.table(paste0(modelDir, 'Annual_Groundwater_Pumping_Totals.dat'), header = T) %>%
  mutate(WaterYear = gsub(x = WaterYear, pattern = 'WY', replacement = '') %>% as.numeric()) %>%
  pivot_longer(cols = -WaterYear, names_to = 'Pumping Type', values_to = 'Volume_m3') %>%
  mutate(Volume_TAF = if_else(Volume_m3>0,Volume_m3*0.000000810714,NaN))

SWBM_pumping %>%
  filter(`Pumping Type`%in%c('Est_Ag_Vol_m3', 'Specified_Ag_Vol_m3')) %>%
  plot_ly(x = ~WaterYear, y = ~Volume_TAF, color = ~`Pumping Type`, type = 'scatter', mode = 'lines+markers')

SWBM_pumping_plot = SWBM_pumping %>%
  filter(`Pumping Type`%in%c('Est_Ag_Vol_m3', 'Specified_Ag_Vol_m3')) %>%
  ggplot(aes(x = WaterYear, y = Volume_TAF, color = `Pumping Type`, fill = `Pumping Type`)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  scale_x_continuous(limits = c(WYstart-0.6, WYend+0.6), breaks = seq(WYstart, WYend, by = 2), expand = c(0,0)) +
  scale_y_continuous(limits = c(0,15), breaks = seq(0,15,by = 3), expand = c(0,0)) +
  scale_color_manual(values = c('blue', 'red'), labels = c('Simulated', 'Observed')) +
  scale_fill_manual(values = c('blue', 'red'), labels = c('Simulated', 'Observed')) +
  ylab('Volume (TAF)') +
  ggtitle('Simulated and Observed Historical Groundwater Pumping') +
  theme_few() +
  theme_adj

ggsave(SWBM_pumping_plot, filename = 'Simulated_Observed_Pumping_2000_2020.png', device = 'png', width = 8.5, height = 4, units = 'in')


# MNW2 Details (work in progress)
pumping_inputs = read.table(paste0(modelDir,'Monthly_Ag_GW_Pumping_Rate_By_Well.dat'),
                            skip = 1, header = T, stringsAsFactors = F, check.names = F) %>%
  mutate(Date = modelMonths) %>%
  pivot_longer(-Date, names_to = 'WELLID', values_to = 'Input')

pumping_sim = read.table(paste0(modelDir,'SVHSM_MNW_QSUM.dat'), header = T, sep = '', comment.char = 'H') %>%
  mutate(Date = Totim + WYstartDate - 1,
         Actual = -Qnet)

pumping_comp = left_join(pumping_inputs, pumping_sim, by = c('WELLID', 'Date')) %>%
  mutate(diff = Input-Actual,
         pct = Actual/Input)



# Zone Budgets (East-West) ------------------------------------------------
ZoneBud_Eastside_Westside_Monthly = read.csv(paste0(modelDir,'ZoneBudget_Eastside_Westside.2.csv'), header = T) %>%
  mutate(Date = WYstartDate + TOTIM -1,
         `Geographic Area` = if_else(ZONE==1, 'Westside', 'Eastside'),
         `Geographic Area` = factor(`Geographic Area`, levels = c('Westside', 'Eastside'), ordered = T),
         Storage = (STORAGE - STORAGE.1)*0.000810714,
         MFR = (WELLS - WELLS.1)*0.000810714,
         Recharge = (RECHARGE - RECHARGE.1)*0.000810714,
         ET = (ET.SEGMENTS-ET.SEGMENTS.1)*0.000810714,
         `GW-SW Exchange` = (STREAM.LEAKAGE-STREAM.LEAKAGE.1)*0.000810714,
         `GW Pumping` = (MNW2-MNW2.1)*0.000810714,
         `Net Zone Exchange` = (From.Other.Zones - To.Other.Zones)*0.000810714,
         `Net Recharge` = (RECHARGE - RECHARGE.1 + WELLS - WELLS.1 + STREAM.LEAKAGE-STREAM.LEAKAGE.1 + FROM.ZONE...1 - TO.ZONE...1 + FROM.ZONE...2 - TO.ZONE...2 + ET.SEGMENTS-ET.SEGMENTS.1 + MNW2-MNW2.1)*0.000810714) %>%
  dplyr::select(Date, `Geographic Area`, MFR, Recharge, ET,  `GW-SW Exchange`, `GW Pumping`,`Net Zone Exchange`, `Net Recharge`, Storage)


ZoneBud_Eastside_Westside_Annual = ZoneBud_Eastside_Westside_Monthly %>%
  mutate(WY = if_else(format(Date,'%b')%in%month.abb[1:9],      # populate water year
                      format(Date, '%Y') %>% as.numeric(),
                      format(Date, '%Y') %>% as.numeric()+1)) %>%
  dplyr::select(-Date) %>%
  group_by(WY, `Geographic Area`) %>%
  summarize_all(.funs = mean)

ZoneBud_Eastside_Westside_Annual_cumulative_storage = ZoneBud_Eastside_Westside_Annual %>%
  dplyr::select(WY, `Geographic Area`, Storage) %>%
  group_by(`Geographic Area`) %>%
  mutate(Storage = cumsum(Storage),
         Storage = if_else(`Geographic Area`=='Westside', Storage-.$Storage[1], Storage-.$Storage[2]),
         `Geographic Area` = if_else(`Geographic Area`=='Westside', 'Westside Cumulative\nStorage', 'Eastside Cumulative\nStorage'),
         `Geographic Area` = factor(`Geographic Area`, levels = c('Westside Cumulative\nStorage', 'Eastside Cumulative\nStorage'), ordered = T))

ZoneBud_Eastside_Westside_Annual_pumping =  ggplot(data = NULL) +
  geom_bar(data = ZoneBud_Eastside_Westside_Annual,
           mapping = aes(x = WY, y = -`GW Pumping`, fill = `Geographic Area`), position = "dodge", stat = 'identity', color = 'black', width = 0.8, size = 0.1) +
  scale_x_continuous(limits = c(WYstart-0.6,WYend + 0.6), 
                     breaks = seq(format(WYstartDate,'%Y')%>% as.numeric()+1,format(WYendDate,'%Y')%>% as.numeric(), by = 2),
                     expand = c(0,0))  +
  scale_y_continuous(limits = ZB_EW_pumping_range, breaks = seq(ZB_EW_pumping_range[1], ZB_EW_pumping_range[2],by = ZB_EW_pumping_int), expand = c(0,0)) +
  ylab('Average Daily Rate (acre-ft/day)') +
  ggtitle('Annual Groundwater Pumping By Geographic Area') +
  geom_rect(data = WYtypes, aes(xmin = plot_annual_start, xmax = plot_annual_end, ymin = ZB_EW_pumping_range[2] - ZB_EW_pumping_range[2]*0.05, ymax = ZB_EW_pumping_range[2] ),
            fill = WYtypes$plot_color , color = 'black', size = 0.2, show.legend = F) +
  geom_text(data = WYtypes, aes(x = annual_label_date, y = ZB_EW_pumping_range[2]*0.975, label = Type), size = 1.5) +
  scale_fill_brewer(type = 'div', palette = 'Set2') +
  barplot_theme +
  theme(legend.position = c(0,0.92))

ggsave(ZoneBud_Eastside_Westside_Annual_pumping, filename = paste0(resultsDir,'Eastside_Westside_Annual_pumping.',fig_format), device = fig_format, width = fig_width, height = fig_height, units = fig_units)

ZoneBud_Eastside_Westside_Annual_Storage =  ggplot(data = NULL) +
  geom_bar(data = ZoneBud_Eastside_Westside_Annual,
           mapping = aes(x = WY, y = `Net Recharge`, group_by = `Geographic Area`, fill = `Geographic Area`), position = "dodge", stat = 'identity', color = 'black', width = 0.9, size = 0.1) +
  scale_x_continuous(limits = c(WYstart-0.6,WYend+0.6), 
                     breaks = seq(format(WYstartDate,'%Y')%>% as.numeric()+1,format(WYendDate,'%Y')%>% as.numeric(), by = 2),
                     expand = c(0,0))  +
  scale_y_continuous(limits = ZB_EW_net_rch_range, breaks = seq(ZB_EW_net_rch_range[1],ZB_EW_net_rch_range[2],by = ZB_EW_net_rch_int), expand = c(0,0)) +
  ylab('Average Daily Rate (acre-ft/day)') +
  ggtitle('Annual Net Recharge By Geographic Area') +
  geom_rect(data = WYtypes, aes(xmin = plot_annual_start, xmax = plot_annual_end, ymin = ZB_EW_net_rch_range[1], ymax = ZB_EW_net_rch_range[1] + ZB_EW_net_rch_range[2]*0.05),
            fill = WYtypes$plot_color , color = 'black', size = 0.2, show.legend = F) +
  geom_text(data = WYtypes, aes(x = annual_label_date, y = ZB_EW_net_rch_range[1]*0.975, label = Type), size = 1.5) +
  geom_line(data = ZoneBud_Eastside_Westside_Annual_cumulative_storage, aes(x = WY, y = -Storage, group_by = `Geographic Area`, color = `Geographic Area`), size = 0.75) +
  #geom_point(data = ZoneBud_Eastside_Westside_Annual_cumulative_storage, aes(x = WY, y = Storage, group_by = `Geographic Area`, color = 'Cumulative', fill = `Geographic Area`), size = 1.5, shape = 21) +
  scale_fill_brewer(type = 'div', palette = 'Set2') +
  scale_color_brewer(type = 'div', palette = 'Set1', direction = -1) +
  barplot_theme 

ggsave(ZoneBud_Eastside_Westside_Annual_Storage, filename = paste0(resultsDir,'Eastside_Westside_Annual_Storage.',fig_format), device = fig_format, width = fig_width, height = fig_height, units = fig_units)

ZoneBud_Eastside_Westside_Annual_Exchange =  ggplot(data = NULL) +
  geom_bar(data = ZoneBud_Eastside_Westside_Annual %>% 
             filter(`Geographic Area`=='Westside') %>%
             mutate(`Geographic Area` = 'Westside Flux'),
           mapping = aes(x = WY, y = `Net Zone Exchange`, group_by = `Geographic Area`, fill = `Geographic Area`), position = "dodge", stat = 'identity', color = 'black', width = 0.9, size = 0.1) +
  scale_x_continuous(limits = c(WYstart-0.6,WYend+0.6), 
                     breaks = seq(format(WYstartDate,'%Y')%>% as.numeric()+1,format(WYendDate,'%Y')%>% as.numeric(), by = 2),
                     expand = c(0,0))  +
  scale_y_continuous(limits = ZB_EW_exchange_range, breaks = seq(ZB_EW_exchange_range[1],ZB_EW_exchange_range[2],by = ZB_EW_exchange_int), expand = c(0,0)) +
  ylab('Average Daily Rate (acre-ft/day)') +
  ggtitle('Groundwater Exchange Between Zones') +
  geom_rect(data = WYtypes, aes(xmin = plot_annual_start, xmax = plot_annual_end, ymin = ZB_EW_exchange_range[1], ymax = ZB_EW_exchange_range[1] + ZB_EW_exchange_range[2]*0.05),
            fill = WYtypes$plot_color , color = 'black', size = 0.2, show.legend = F) +
  geom_text(data = WYtypes, aes(x = annual_label_date, y = ZB_EW_exchange_range[1]*0.975, label = Type), size = 1.5) +
  scale_fill_brewer(type = 'div', palette = 'Set2') +
  barplot_theme

ggsave(ZoneBud_Eastside_Westside_Annual_Exchange, filename = paste0(resultsDir,'Eastside_Westside_Annual_Exchange.',fig_format), device = fig_format, width = fig_width, height = fig_height, units = fig_units)
write.csv(x = ZoneBud_Eastside_Westside_Annual, file = paste0(resultsDir,'Eastside_Westside_Budget.csv'), append = F, quote = F, row.names = F, col.names = T)

# Zone Budgets (Eastside Upper-Lower) ------------------------------------------------
ZoneBud_Eastside_Upper_Lower_Monthly = read.csv(paste0(modelDir,'ZoneBudget_EastSide_Upper_Lower.2.csv'), header = T) %>%
  mutate(Date = WYstartDate + TOTIM -1,
         `Geographic Area` = case_when(
           ZONE==1 ~ 'Westside',
           ZONE==2 ~ 'Eastside Upper Aquifer',
           ZONE==3 ~ 'Eastside Lower Aquifer'),
         `Geographic Area` = factor(`Geographic Area`, levels = c('Westside', 'Eastside Upper Aquifer', 'Eastside Lower Aquifer'), ordered = T),
         Storage = (STORAGE - STORAGE.1)*0.000810714,
         MFR = (WELLS - WELLS.1)*0.000810714,
         Recharge = (RECHARGE - RECHARGE.1)*0.000810714,
         ET = (ET.SEGMENTS-ET.SEGMENTS.1)*0.000810714,
         `GW-SW Exchange` = (STREAM.LEAKAGE-STREAM.LEAKAGE.1)*0.000810714,
         `GW Pumping` = (MNW2-MNW2.1)*0.000810714,
         `Upper to Lower Exchange` = (FROM.ZONE...2 - TO.ZONE...2 + FROM.ZONE...3 - TO.ZONE...3)*0.000810714,
         `Net Recharge` = (RECHARGE - RECHARGE.1 + WELLS - WELLS.1 + STREAM.LEAKAGE-STREAM.LEAKAGE.1 + FROM.ZONE...2 - TO.ZONE...2 + FROM.ZONE...3 - TO.ZONE...3 + ET.SEGMENTS-ET.SEGMENTS.1 + MNW2-MNW2.1)*0.000810714) %>%
  dplyr::select(Date, `Geographic Area`, MFR, Recharge, ET,  `GW-SW Exchange`, `GW Pumping`,`Upper to Lower Exchange`, `Net Recharge`, Storage)


ZoneBud_Eastside_Upper_Lower_Annual = ZoneBud_Eastside_Upper_Lower_Monthly %>%
  mutate(WY = if_else(format(Date,'%b')%in%month.abb[1:9],      # populate water year
                      format(Date, '%Y') %>% as.numeric(),
                      format(Date, '%Y') %>% as.numeric()+1)) %>%
  dplyr::select(-Date) %>%
  group_by(WY, `Geographic Area`) %>%
  summarize_all(.funs = mean)

ZoneBud_Eastside_Upper_Lower_Annual_cumulative_storage = ZoneBud_Eastside_Upper_Lower_Annual %>%
  dplyr::select(WY, `Geographic Area`, Storage) %>%
  group_by(`Geographic Area`) %>%
  mutate(Storage = cumsum(Storage),
         Storage = case_when(
           `Geographic Area`=='Westside' ~ Storage - .$Storage[1],
           `Geographic Area`=='Eastside Upper Aquifer' ~ Storage - .$Storage[2],
           `Geographic Area`=='Eastside Lower Aquifer' ~ Storage - .$Storage[3]),
         `Geographic Area` = case_when(
           `Geographic Area`=='Westside' ~ 'Westside Cumulative\nStorage',
           `Geographic Area`=='Eastside Upper Aquifer' ~ 'Eastside Upper Aquifer\nCumulative Storage',
           `Geographic Area`=='Eastside Lower Aquifer' ~ 'Eastside Lower Aquifer\nCumulative Storage'),
         `Geographic Area` = factor(`Geographic Area`, levels = c('Westside Cumulative\nStorage',
                                                                  'Eastside Upper Aquifer\nCumulative Storage',
                                                                  'Eastside Lower Aquifer\nCumulative Storage'), ordered = T))


ZoneBud_Eastside_Upper_Lower_Annual_pumping =  ggplot(data = NULL) +
  geom_bar(data = ZoneBud_Eastside_Upper_Lower_Annual %>% filter(`Geographic Area` != 'Westside'),
           mapping = aes(x = WY, y = -`GW Pumping`, fill = `Geographic Area`), position = "dodge", stat = 'identity', color = 'black', width = 0.8, size = 0.1) +
  scale_x_continuous(limits = c(WYstart-0.6,WYend+0.6), 
                     breaks = seq(format(WYstartDate,'%Y')%>% as.numeric()+1,format(WYendDate,'%Y')%>% as.numeric(), by = 2),
                     expand = c(0,0))  +
  scale_y_continuous(limits = ZB_E_UL_pumping_range, breaks = seq(ZB_E_UL_pumping_range[1], ZB_E_UL_pumping_range[2],by = ZB_E_UL_pumping_int), expand = c(0,0)) +
  ylab('Average Daily Rate (acre-ft/day)') +
  ggtitle('Annual Groundwater Pumping By Aquifer Zone') +
  geom_rect(data = WYtypes, aes(xmin = plot_annual_start, xmax = plot_annual_end, ymin = ZB_E_UL_pumping_range[2] - ZB_E_UL_pumping_range[2]*0.05, ymax = ZB_E_UL_pumping_range[2] ),
            fill = WYtypes$plot_color , color = 'black', size = 0.2, show.legend = F) +
  geom_text(data = WYtypes, aes(x = annual_label_date, y = ZB_E_UL_pumping_range[2]*0.975, label = Type), size = 1.5) +
  scale_fill_brewer(type = 'div', palette = 'Set2') +
  barplot_theme +
  theme(legend.position = c(0,0.92))

ggsave(ZoneBud_Eastside_Upper_Lower_Annual_pumping, filename = paste0(resultsDir,'Eastside_Upper_Lower_Aquifer_Annual_Pumping.',fig_format), device = fig_format, width = fig_width, height = fig_height, units = fig_units)


ZoneBud_Eastside_Upper_Lower_Annual_Storage =  ggplot(data = NULL) +
  geom_bar(data = ZoneBud_Eastside_Upper_Lower_Annual %>% filter(`Geographic Area` != 'Westside'),
           mapping = aes(x = WY, y = `Net Recharge`, group_by = `Geographic Area`, fill = `Geographic Area`), position = "dodge", stat = 'identity', color = 'black', width = 0.9, size = 0.1) +
  scale_x_continuous(limits = c(WYstart-0.6,WYend+0.6), 
                     breaks = seq(format(WYstartDate,'%Y')%>% as.numeric()+1,format(WYendDate,'%Y')%>% as.numeric(), by = 2),
                     expand = c(0,0))  +
  scale_y_continuous(limits = ZB_E_UL_net_rch_range, breaks = seq(ZB_E_UL_net_rch_range[1],ZB_E_UL_net_rch_range[2],by = ZB_E_UL_net_rch_int), expand = c(0,0)) +
  ylab('Average Daily Rate (acre-ft/day)') +
  ggtitle('Annual Net Recharge By Aquifer Zone') +
  geom_rect(data = WYtypes, aes(xmin = plot_annual_start, xmax = plot_annual_end, ymin = ZB_E_UL_net_rch_range[1], ymax = ZB_E_UL_net_rch_range[1] + ZB_E_UL_net_rch_range[2]*0.05),
            fill = WYtypes$plot_color , color = 'black', size = 0.2, show.legend = F) +
  geom_text(data = WYtypes, aes(x = annual_label_date, y = ZB_E_UL_net_rch_range[1]*0.975, label = Type), size = 1.5) +
  geom_line(data = ZoneBud_Eastside_Upper_Lower_Annual_cumulative_storage %>%
              filter(`Geographic Area` != 'Westside Cumulative\nStorage'), 
            aes(x = WY, y = -Storage, group_by = `Geographic Area`, color = `Geographic Area`), size = 0.75) +
  scale_fill_manual(values = c('cadetblue3', 'indianred3')) +
  scale_color_manual(values = c('mediumseagreen', 'khaki4')) +
  barplot_theme
  # theme(legend.title=element_blank(),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
  #       panel.background = element_rect(color = 'black', fill = NA),
  #       plot.background = element_rect(color = NA, fill = NA),
  #       axis.text.x = element_text(angle = 45, hjust = 1, vjust = 0.7), 
  #       axis.text = element_text(size = 8),
  #       axis.title.x = element_blank(),
  #       plot.title = element_text(hjust = 0.5),
  #       legend.position = c(0, 1), 
  #       legend.justification = c('left', 'top'),
  #       legend.key = element_rect(fill = NA, color = NA),
  #       legend.background = element_rect(fill = NA, color = NA),
  #       legend.direction = 'horizontal',
  #       legend.text = element_text(size = 6, margin = margin(r=1,l=1, unit = 'pt')),
  #       legend.key.height = unit(10,'pt'),
  #       legend.spacing.y = unit(0, 'pt'))

ggsave(ZoneBud_Eastside_Upper_Lower_Annual_Storage, filename = paste0(resultsDir,'Eastside_Upper_Lower_Aquifer_Annual_Storage.',fig_format), device = fig_format, width = fig_width, height = fig_height, units = fig_units)

ZoneBud_Eastside_Upper_Lower_Annual_Exchange =  ggplot(data = NULL) +
  geom_bar(data = ZoneBud_Eastside_Upper_Lower_Annual %>% 
             filter(`Geographic Area`=='Eastside Lower Aquifer'),
           mapping = aes(x = WY, y = `Upper to Lower Exchange`, group_by = `Geographic Area`, fill = `Geographic Area`), position = "dodge", stat = 'identity', color = 'black', width = 0.9, size = 0.1) +
  scale_x_continuous(limits = c(WYstart-0.6,WYend+0.6), 
                     breaks = seq(format(WYstartDate,'%Y')%>% as.numeric()+1,format(WYendDate,'%Y')%>% as.numeric(), by = 2),
                     expand = c(0,0))  +
  scale_y_continuous(limits = ZB_E_UL_exchange_range, breaks = seq(ZB_E_UL_exchange_range[1],ZB_E_UL_exchange_range[2],by = ZB_E_UL_exchange_int), expand = c(0,0)) +
  ylab('Average Daily Rate (acre-ft/day)') +
  ggtitle('Eastside Upper and Lower Aquifer Groundwater') +
  geom_rect(data = WYtypes, aes(xmin = plot_annual_start, xmax = plot_annual_end, ymin = ZB_E_UL_exchange_range[1], ymax = ZB_E_UL_exchange_range[1] + ZB_E_UL_exchange_range[2]*0.05),
            fill = WYtypes$plot_color , color = 'black', size = 0.2, show.legend = F) +
  geom_text(data = WYtypes, aes(x = annual_label_date, y = ZB_E_UL_exchange_range[1]*0.975, label = Type), size = 1.5) +
  scale_fill_brewer(type = 'div', palette = 'Set2') +
  barplot_theme 
  # theme(legend.title=element_blank(),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
  #       panel.background = element_rect(color = 'black', fill = NA),
  #       plot.background = element_rect(color = NA, fill = NA),
  #       axis.text.x = element_text(angle = 45, hjust = 1, vjust = 0.7), 
  #       axis.text = element_text(size = 8),
  #       axis.title.x = element_blank(),
  #       plot.title = element_text(hjust = 0.5),
  #       legend.position = c(0, 1), 
  #       legend.justification = c('left', 'top'),
  #       legend.key = element_rect(fill = NA, color = NA),
  #       legend.background = element_rect(fill = NA, color = NA),
  #       legend.direction = 'horizontal',
  #       legend.text = element_text(size = 6, margin = margin(r=1,l=1, unit = 'pt')),
  #       legend.key.height = unit(10,'pt'),
  #       legend.spacing.y = unit(0, 'pt'))

ggsave(ZoneBud_Eastside_Upper_Lower_Annual_Exchange, filename = paste0(resultsDir,'Eastside_Upper_Lower_Annual_Exchange.',fig_format), device = fig_format, width = fig_width, height = fig_height, units = fig_units)
write.csv(x = ZoneBud_Eastside_Upper_Lower_Annual, file = paste0(resultsDir,'Eastside_Upper_Lower_Budget.csv'), append = F, quote = F, row.names = F, col.names = T)
