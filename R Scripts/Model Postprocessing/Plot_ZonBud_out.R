# MODFLOW Pre-Processing.R
# User Inputs -------------------------------------------------------------

#General Information
modelName = 'SVHSM'
ZonBud_csv = 'SVHSM_EW.2.csv'
outDir = 'Results/'

# Temporal Information
WYstart = 2008                                   # Beginning water year of simulation
WYend = 2013                                     # Ending water year of simulatuion

# Water Budget Legend Labels and Colors -------------------------------------------------------------------
SWBM_flux_labels = c('Precipitation','ET','SW Irrigation', 'Recharge','GW Irrigation','Storage')
MODFLOW_flux_labels = c('Recharge','ET','Storage','Drains','Stream Leakage','Wells', 'Canal Seepage/MFR')
Streamflow_flux_labels = c('Inflow', 'Overland Flow', 'Farmers Ditch', 'SVID Ditch', 'Stream leakage', 'Outflow', 'Storage')

SWBM_colors = c('lightblue1', 'red', 'darkcyan', 'mediumblue','darkgreen', 'goldenrod' )
MODFLOW_colors = c('mediumblue', 'red', 'goldenrod', 'mediumorchid2','dodgerblue1', 'darkgreen', 'salmon' )
Streamflow_colors = c('turquoise2', 'sienna3', 'green2', 'green4', 'dodgerblue1', 'lightgoldenrod1', 'goldenrod')  

# Script Initialization ---------------------------------------------------
outDirTables = paste0(outDir,'Tables/')
outdirPlots = paste0(outDir,'Figures/')
dir.create(outDir)
dir.create(outDirTables)
dir.create(outdirPlots)

library(dplyr)
library(tidyr)
library(lubridate)
library(sf)
library(plotly)

options(dplyr.summarise.inform = FALSE)

# Functions ---------------------------------------------------------------
'%!in%' = function(x,y)!('%in%'(x,y))

roundUp <- function(x,base){                                                                # function for rounding numbers down to specified base
  base*ceiling(x/base)
}

#__________________________________________________________________________
Plot_Annual_Budget_Bar = function(df, outUnits, startDate, endDate, titletxt, scale_round){
  if (outUnits=='TAF'){
    df = df %>%
      mutate(value = value*0.000810714/1000)
    axis_title = 'Volume (TAF)'
  } else if (outUnits=='Mm3'){
    df = df %>%
      mutate(value = value/1E6)
    axis_title = bquote('Volume ('*Mm^3*')')
  } else {
    errorCondition('Units must be TAF or Mm3')
  }
  x_limits = c(format(startDate,'%Y')%>% as.numeric()+0.4,
               format(endDate,'%Y')%>% as.numeric()+0.6)
  y_limit = df %>%
    group_by(WY) %>%
    summarize(total = sum(abs(value))/2) %>% 
    dplyr::select(total) %>%
    max() %>% 
    roundUp(scale_round)
  
  Annual_Barplot = ggplot(data = NULL) +
    geom_bar(data = df, mapping = aes(x = WY, y = value, fill = Flux), position = "stack", stat = 'identity', color = 'black', width = 0.95, size = 0.1) +
    scale_x_continuous(limits = x_limits, 
                       breaks = seq(format(startDate,'%Y')%>% as.numeric()+1,format(endDate,'%Y')%>% as.numeric(), by = 2),
                       expand = c(0,0))  +
    scale_y_continuous(limits = c(-y_limit,y_limit), breaks = seq(-y_limit,y_limit,by = scale_round), expand = c(0,0)) +
    ylab(axis_title) +
    ggtitle(titletxt) +
    scale_fill_manual(breaks = SWBM_WB_colors$label[c(-7,-8)], values = c('light coral', '#ff0000', 'darkgreen', '#33ccff', '#0000ff', '#ffcc00', '#cc33ff', 'steelblue2')) +
    geom_rect(data = WYtypes, aes(xmin = plot_annual_start, xmax = plot_annual_end, ymin = -y_limit, ymax = -y_limit + y_limit*0.05, 
                                  fill = (Type)), color = 'black', size = 0.2, show.legend = F) +
    geom_text(data = WYtypes, aes(x = annual_label_date, y = -y_limit*0.975, label = Type), size = 1.5) +
    theme(legend.title=element_blank(),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_rect(color = 'black', fill = NA),
          plot.background = element_rect(color = NA, fill = NA),
          axis.text.x = element_text(angle = 45, hjust = 1, vjust = 0.7), 
          axis.text = element_text(size = 8),
          axis.title.x = element_blank(),
          plot.title = element_text(hjust = 0.5),
          legend.position = c(0.25, 0.95), 
          legend.key = element_rect(fill = NA, color = NA),
          legend.background = element_rect(fill = NA, color = NA),
          legend.direction = 'horizontal',
          legend.text = element_text(size = 6, margin = margin(r=1,l=1, unit = 'pt')),
          legend.key.height = unit(10,'pt'))
  return(Annual_Barplot)
  
}

Plot_Monthly_Budget_Bar = function(df, outUnits, titletxt, y_limit, break_value){
  if (outUnits=='TAF'){
    df = df %>%
      mutate(value = value*0.000810714/1000)
    axis_title = 'Volume (TAF)'
  } else if (outUnits=='Mm3'){
    df = df %>%
      mutate(value = value/1E6)
    axis_title = bquote('Volume ('*Mm^3*')')
  } else {
    errorCondition('Units must be TAF or Mm3')
  }
  
  Monthly_Barplot = ggplot(df, aes(x = calendarMonth, y = value)) +
    geom_bar(aes(fill = Flux), position = "stack", stat = 'identity', color = 'black', width = 0.95, size = 0.1) +
    scale_y_continuous(limits = c(-y_limit,y_limit), breaks = seq(-y_limit,y_limit,by = break_value), expand = c(0,0)) +
    ylab(axis_title) +
    ggtitle(titletxt) +
    scale_fill_manual(breaks = SWBM_flux_colors$label, values = SWBM_flux_colors$color) +
    theme(legend.title=element_blank(),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_rect(color = 'black', fill = NA),
          plot.background = element_rect(color = NA, fill = NA),
          axis.text.x = element_text(angle = 45, hjust = 1, vjust = 0.7), 
          axis.text = element_text(size = 8),
          axis.title.x = element_blank(),
          plot.title = element_text(hjust = 0.5),
          legend.position = c(0.25, 0.95), 
          legend.key = element_rect(fill = NA, color = NA),
          legend.background = element_rect(fill = NA, color = NA),
          legend.direction = 'horizontal',
          legend.text = element_text(size = 6, margin = margin(r=1,l=1, unit = 'pt')),
          legend.key.height = unit(10,'pt'))
  return(Monthly_Barplot)
  
}

# Create Date Arrays ------------------------------------------------------
WYstartDate = paste0(WYstart-1,'-10-01') %>% as.Date()
WYendDate = paste0(WYend,'-09-30') %>% as.Date()
modelDays = seq.Date(from = WYstartDate, to = WYendDate, by = 'day')
modelMonths = seq.Date(from = WYstartDate, to = WYendDate, by = 'month')
NPER = length(modelMonths)
modelYears = (WYend - WYstart) + 1
modelTimes = data.frame(water_level_date = modelDays,
                        IREFSP = rep(seq(1,NPER), days_in_month(modelMonths)),
                        TOFFSET = format(modelDays, '%d') %>% as.numeric())

# Read Data ---------------------------------------------------------
Annual_Fluxes = read.csv(ZonBud_csv, header = T) %>%
  mutate(Date = rep(modelMonths, each = 2),
         STORAGE_vol_net = STORAGE*days_in_month(Date) - STORAGE.1*days_in_month(Date),
         MNW2_vol_net = MNW2*days_in_month(Date) - MNW2.1*days_in_month(Date),
         Recharge_vol_net = RECHARGE*days_in_month(Date) - RECHARGE.1*days_in_month(Date)) %>%
  mutate(WY = rep(seq(WYstart,WYend), each = 2*12),
         Zone_Name = if_else(ZONE==1, 'West Side', 'East Side'),
         Zone_Name = factor(Zone_Name, levels = c('West Side', 'East Side'))) %>%
  dplyr::select(ZONE, Zone_Name, WY, STORAGE_vol_net, MNW2_vol_net, Recharge_vol_net) %>%
  group_by(ZONE, Zone_Name, WY) %>%
  summarize_all(.funs = sum) %>%
  ungroup()

ggplot(Annual_Fluxes, aes(x = WY, y = STORAGE_vol_net, group = Zone_Name,  fill = Zone_Name, color = Zone_Name)) +
  geom_line() +
  geom_point()

ggplot(data = NULL) +
  geom_bar(data = Annual_Fluxes, mapping = aes(x = WY, y = -1*MNW2_vol_net*0.000000810714, fill = Zone_Name), position = "dodge", stat = 'identity', color = 'black', width = 0.95, size = 0.1) +
  # scale_x_continuous(limits = x_limits, 
  #                    breaks = seq(format(startDate,'%Y')%>% as.numeric()+1,format(endDate,'%Y')%>% as.numeric(), by = 2),
  #                    expand = c(0,0))  +
  scale_y_continuous(limits = c(0,20), breaks = seq(0,20,by = 5), expand = c(0,0)) +
  ylab('Annual Volume (TAF)') +
  ggtitle('East Side vs West Side - Storage') +
  scale_fill_manual(values = c('#0156ff', '#ffa201')) +
  # geom_rect(data = WYtypes, aes(xmin = plot_annual_start, xmax = plot_annual_end, ymin = -y_limit, ymax = -y_limit + y_limit*0.05, 
  #                               fill = (Type)), color = 'black', size = 0.2, show.legend = F) +
  # geom_text(data = WYtypes, aes(x = annual_label_date, y = -y_limit*0.975, label = Type), size = 1.5) +
  theme(legend.title=element_blank(),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_rect(color = 'black', fill = NA),
        plot.background = element_rect(color = NA, fill = NA),
        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 0.7), 
        axis.text = element_text(size = 8),
        axis.title.x = element_blank(),
        plot.title = element_text(hjust = 0.5),
        legend.position = c(0.25, 0.95), 
        legend.key = element_rect(fill = NA, color = NA),
        legend.background = element_rect(fill = NA, color = NA),
        legend.direction = 'horizontal',
        legend.text = element_text(size = 6, margin = margin(r=1,l=1, unit = 'pt')),
        legend.key.height = unit(10,'pt'))
  


