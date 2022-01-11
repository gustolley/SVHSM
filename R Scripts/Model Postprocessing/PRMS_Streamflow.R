#Plot PRMS Simualted Inflow


# User Input --------------------------------------------------------------
prms_subbasins = 'model_points_new5_35c.shp'     # Point shapefile with locations of real or synthetic gage data used in gsflow python scripts
statvar_file = 'statvar.dat'                               # Daily output from PRMS
param_out = 'sub_cfs'   
PRMS_daily = 'PRMS_streamflows.txt'
Flow_obs = 'Sierra_Valley_Inflows_2007-2020_data_only.txt'

WYstartDate = as.Date('1989-10-01')
WYendDate = as.Date('2020-09-30')

# Initialize Script -------------------------------------------------------
library(dplyr)
library(tidyr)
library(sf)
library(ggplot2)
library(ggthemes)
library(plotly)

theme_adj = theme(plot.title = element_text(hjust = 0.5, size = 14),
                  panel.grid.major.y = element_line(size = 0.1, color = 'gray80'),
                  panel.border = element_rect(color = 'black', fill = NA, size = 1),
                  plot.background = element_rect(fill = NA, color = NA),
                  axis.title = element_text(size = 13),
                  axis.title.x = element_blank(),
                  axis.text = element_text(size = 12),
                  axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))

# Functions ----------------------------------------------------------
PRMS_monthly_inflows_m3 = function(statvar_file, param_type, subbasin_id, subbasin_name, startDate, endDate){
  n_params = readLines(statvar_file, n = 1) %>% as.integer()
  prms_params = read.table(statvar_file, skip = 1, nrows = n_params, col.names = c('param_type', 'id'))
  flow_idx = which(prms_params$param_type == param_type & prms_params$id%in%subbasin_id) + 7   # location of desired output adjusted for time info
  prms_flows = read.table(statvar_file, skip = n_params+1) %>%
    mutate(modelDate = as.Date(paste(V2, V3, V4, sep = '-')),
           modelDate = format(modelDate, '%b-%Y')) %>%
    dplyr::select(modelDate, paste0('V',flow_idx)) %>%
    group_by(modelDate) %>%
    summarize_all(.funs = sum) %>%                                          # aggregate daily values to monthly total
    mutate(modelDate = as.Date(paste0('01-',modelDate), '%d-%b-%Y')) %>%
    arrange(modelDate) %>%
    filter(modelDate>=startDate & modelDate<=endDate)
  if(param_type == 'sub_cfs'){
    prms_flows[,-1] = prms_flows[,-1]*2446.58     # convert cfs to m^3
  }
  names(prms_flows) = c('ModelDate', subbasin_name)
  return(prms_flows)
}


# Daily Flows ------------------------------------------------------------
PRMS_daily_flow = read.table(PRMS_daily, sep = '\t', header = T, stringsAsFactors = F, check.names=FALSE) %>%
  mutate(Date = as.Date(paste(Year, Month, Day, sep = '-')),
         Type = 'Simulated',
         Smithneck_combo = `Smithneck Creek` + `Bear Valley Creek` + `Badenough Creek` ) %>%
  dplyr::select(-`Elapsed Time (days)`, -Year, -Month, -Day) %>%
  pivot_longer(cols = c(-Date, -Type), names_to = 'Stream', values_to = 'Flow_cfs')

Obs_streamflow = read.table(Flow_obs, sep = '\t', header = T, check.names=FALSE) %>%
  mutate(Date = as.Date(Date, format('%m/%d/%Y')),
         Type = 'Observed',
         Smithneck_combo = `Smithneck Creek`) %>%
  pivot_longer(cols = c(-Date, -Type), names_to = 'Stream', values_to = 'Flow_cfs')
  
Flows_Combo = rbind(PRMS_daily_flow,Obs_streamflow) %>%
  filter(Stream %in% intersect(unique(Obs_streamflow$Stream), unique(PRMS_daily_flow$Stream)))



# Berry Creek -------------------------------------------------------------
ggplot(data = NULL, aes(x = Date, y = Flow_cfs)) +
  geom_line(data = Flows_Combo %>% filter(Stream == 'Berry Creek', Type == 'Simulated'), mapping = aes(color = 'Simulated')) +
  geom_point(data = Flows_Combo %>% filter(Stream == 'Berry Creek', Type == 'Observed'), mapping = aes(fill = 'Observed'), shape = 21, size = 1.5) +
  ggtitle('Berry Creek') +
  ylab('Flow (cfs)') +
  scale_y_log10(limits = c(0.1,10000), expand= c(0,0), breaks = c(0.1,1,10,100,1000, 10000), labels = as.character(1%o%10^(-1:4))) +
  scale_x_date(limits = c(as.Date('2007-01-01'), '2021-01-01'), date_labels = '%b-%Y', date_breaks = '2 years', expand = c(0,0)) +
  scale_color_manual(values = c('red')) +
  scale_fill_manual(values = c('blue')) +
  theme_few() +
  theme_adj +
  annotation_logticks() +
  theme(legend.title = element_blank(),
        legend.position = 'none',
        axis.title.x = element_blank())

ggsave('Berry Creek Obs vs Sim.jpg',device = 'jpg', width = 8, height = 4, units = 'in', dpi = 300)

# Bonta Creek -------------------------------------------------------------
ggplot(data = NULL, aes(x = Date, y = Flow_cfs)) +
  geom_line(data = Flows_Combo %>% filter(Stream == 'Bonta Creek', Type == 'Simulated'), mapping = aes(color = 'Simulated')) +
  geom_point(data = Flows_Combo %>% filter(Stream == 'Bonta Creek', Type == 'Observed'), mapping = aes(fill = 'Observed'), shape = 21, size = 1.5) +
  ggtitle('Bonta Creek') +
  ylab('Flow (cfs)') +
  scale_y_log10(limits = c(0.01,1000), expand= c(0,0), breaks = c(0.01, 0.1,1,10,100,1000), labels = as.character(1%o%10^(-2:3))) +
  scale_x_date(limits = c(as.Date('2007-01-01'), '2021-01-01'), date_labels = '%b-%Y', date_breaks = '2 years', expand = c(0,0)) +
  scale_color_manual(values = c('red')) +
  scale_fill_manual(values = c('blue')) +
  theme_few() +
  theme_adj +
  annotation_logticks() +
  theme(legend.title = element_blank(),
        legend.position = 'none',
        axis.title.x = element_blank())

ggsave('Bonta Creek Obs vs Sim.jpg',device = 'jpg', width = 8, height = 4, units = 'in', dpi = 300)

# Cold Stream -------------------------------------------------------------
ggplot(data = NULL, aes(x = Date, y = Flow_cfs)) +
  geom_line(data = Flows_Combo %>% filter(Stream == 'Cold Stream', Type == 'Simulated'), mapping = aes(color = 'Simulated')) +
  geom_point(data = Flows_Combo %>% filter(Stream == 'Cold Stream', Type == 'Observed'), mapping = aes(fill = 'Observed'), shape = 21, size = 1.5) +
  ggtitle('Cold Stream') +
  ylab('Flow (cfs)') +
  scale_y_log10(limits = c(0.01,1000), expand= c(0,0), breaks = c(0.01,0.1,1,10,100,1000), labels = as.character(1%o%10^(-2:3))) +
  scale_x_date(limits = c(as.Date('2007-01-01'), '2021-01-01'), date_labels = '%b-%Y', date_breaks = '2 years', expand = c(0,0)) +
  scale_color_manual(values = c('red')) +
  scale_fill_manual(values = c('blue')) +
  theme_few() +
  theme_adj +
  annotation_logticks() +
  theme(legend.title = element_blank(),
        legend.position = 'none',
        axis.title.x = element_blank())

ggsave('Cold Stream Obs vs Sim.jpg',device = 'jpg', width = 8, height = 4, units = 'in', dpi = 300)


# Fletcher Creek -------------------------------------------------------------
ggplot(data = NULL, aes(x = Date, y = Flow_cfs)) +
  geom_line(data = Flows_Combo %>% filter(Stream == 'Fletcher Creek', Type == 'Simulated'), mapping = aes(color = 'Simulated')) +
  geom_point(data = Flows_Combo %>% filter(Stream == 'Fletcher Creek', Type == 'Observed'), mapping = aes(fill = 'Observed'), shape = 21, size = 1.5) +
  ggtitle('Fletcher Creek') +
  ylab('Flow (cfs)') +
  scale_y_log10(limits = c(0.01,1000), expand= c(0,0), breaks = c(0.01,0.1,1,10,100,1000), labels = as.character(1%o%10^(-2:3))) +
  scale_x_date(limits = c(as.Date('2007-01-01'), '2021-01-01'), date_labels = '%b-%Y', date_breaks = '2 years', expand = c(0,0)) +
  scale_color_manual(values = c('red')) +
  scale_fill_manual(values = c('blue')) +
  theme_few() +
  theme_adj +
  annotation_logticks() +
  theme(legend.title = element_blank(),
        legend.position = 'none',
        axis.title.x = element_blank())

ggsave('Fletcher Creek Obs vs Sim.jpg',device = 'jpg', width = 8, height = 4, units = 'in', dpi = 300)


# Hamlin Creek -------------------------------------------------------------
ggplot(data = NULL, aes(x = Date, y = Flow_cfs)) +
  geom_line(data = Flows_Combo %>% filter(Stream == 'Hamlin Creek', Type == 'Simulated'), mapping = aes(color = 'Simulated')) +
  geom_point(data = Flows_Combo %>% filter(Stream == 'Hamlin Creek', Type == 'Observed'), mapping = aes(fill = 'Observed'), shape = 21, size = 1.5) +
  ggtitle('Hamlin Creek') +
  ylab('Flow (cfs)') +
  scale_y_log10(limits = c(0.1,10000), expand= c(0,0), breaks = c(0.1,1,10,100,1000, 10000), labels = as.character(1%o%10^(-1:4))) +
  scale_x_date(limits = c(as.Date('2007-01-01'), '2021-01-01'), date_labels = '%b-%Y', date_breaks = '2 years', expand = c(0,0)) +
  scale_color_manual(values = c('red')) +
  scale_fill_manual(values = c('blue')) +
  theme_few() +
  theme_adj +
  annotation_logticks() +
  theme(legend.title = element_blank(),
        legend.position = 'none',
        axis.title.x = element_blank())

ggsave('Hamlin Creek Obs vs Sim.jpg',device = 'jpg', width = 8, height = 4, units = 'in', dpi = 300)


# Lemon Creek -------------------------------------------------------------
ggplot(data = NULL, aes(x = Date, y = Flow_cfs)) +
  geom_line(data = Flows_Combo %>% filter(Stream == 'Lemon Creek', Type == 'Simulated'), mapping = aes(color = 'Simulated')) +
  geom_point(data = Flows_Combo %>% filter(Stream == 'Lemon Creek', Type == 'Observed'), mapping = aes(fill = 'Observed'), shape = 21, size = 1.5) +
  ggtitle('Lemon Creek') +
  ylab('Flow (cfs)') +
  scale_y_log10(limits = c(0.1,1000), expand= c(0,0), breaks = c(0.1,1,10,100,1000), labels = as.character(1%o%10^(-1:3))) +
  scale_x_date(limits = c(as.Date('2007-01-01'), '2021-01-01'), date_labels = '%b-%Y', date_breaks = '2 years', expand = c(0,0)) +
  scale_color_manual(values = c('red')) +
  scale_fill_manual(values = c('blue')) +
  theme_few() +
  theme_adj +
  annotation_logticks() +
  theme(legend.title = element_blank(),
        legend.position = 'none',
        axis.title.x = element_blank())

ggsave('Lemon Creek Obs vs Sim.jpg',device = 'jpg', width = 8, height = 4, units = 'in', dpi = 300)


# Smithneck Creek -------------------------------------------------------------
ggplot(data = NULL, aes(x = Date, y = Flow_cfs)) +
  geom_line(data = Flows_Combo %>% filter(Stream == 'Smithneck_combo', Type == 'Simulated'), mapping = aes(color = 'Simulated')) +
  geom_point(data = Flows_Combo %>% filter(Stream == 'Smithneck_combo', Type == 'Observed'), mapping = aes(fill = 'Observed'), shape = 21, size = 1.5) +
  ggtitle('Smithneck Creek') +
  ylab('Flow (cfs)') +
  scale_y_log10(limits = c(0.1,1000), expand= c(0,0), breaks = c(0.1,1,10,100,1000), labels = as.character(1%o%10^(-1:3))) +
  scale_x_date(limits = c(as.Date('2007-01-01'), '2021-01-01'), date_labels = '%b-%Y', date_breaks = '2 years', expand = c(0,0)) +
  scale_color_manual(values = c('red')) +
  scale_fill_manual(values = c('blue')) +
  theme_few() +
  theme_adj +
  annotation_logticks() +
  theme(legend.title = element_blank(),
        legend.position = 'none',
        axis.title.x = element_blank())

ggsave('Smithneck Creek (combo) Obs vs Sim.jpg',device = 'jpg', width = 8, height = 4, units = 'in', dpi = 300)


# Staverville Creek -------------------------------------------------------------
ggplot(data = NULL, aes(x = Date, y = Flow_cfs)) +
  geom_line(data = Flows_Combo %>% filter(Stream == 'Staverville Creek', Type == 'Simulated'), mapping = aes(color = 'Simulated')) +
  geom_point(data = Flows_Combo %>% filter(Stream == 'Staverville Creek', Type == 'Observed'), mapping = aes(fill = 'Observed'), shape = 21, size = 1.5) +
  ggtitle('Staverville Creek') +
  ylab('Flow (cfs)') +
  scale_y_log10(limits = c(0.1,1000), expand= c(0,0), breaks = c(0.1,1,10,100,1000), labels = as.character(1%o%10^(-1:3))) +
  scale_x_date(limits = c(as.Date('2007-01-01'), '2021-01-01'), date_labels = '%b-%Y', date_breaks = '2 years', expand = c(0,0)) +
  scale_color_manual(values = c('red')) +
  scale_fill_manual(values = c('blue')) +
  theme_few() +
  theme_adj +
  annotation_logticks() +
  theme(legend.title = element_blank(),
        legend.position = 'none',
        axis.title.x = element_blank())

ggsave('Staverville Creek Obs vs Sim.jpg', device = 'jpg', width = 8, height = 4, units = 'in', dpi = 300)


# Turner Creek -------------------------------------------------------------
ggplot(data = NULL, aes(x = Date, y = Flow_cfs)) +
  geom_line(data = Flows_Combo %>% filter(Stream == 'Turner Creek', Type == 'Simulated'), mapping = aes(color = 'Simulated')) +
  geom_point(data = Flows_Combo %>% filter(Stream == 'Turner Creek', Type == 'Observed'), mapping = aes(fill = 'Observed'), shape = 21, size = 1.5) +
  ggtitle('Turner Creek') +
  ylab('Flow (cfs)') +
  scale_y_log10(limits = c(0.001,100), expand= c(0,0), breaks = c(0.001, 0.01, 0.1,1,10,100), labels = as.character(1%o%10^(-3:2))) +
  scale_x_date(limits = c(as.Date('2007-01-01'), '2021-01-01'), date_labels = '%b-%Y', date_breaks = '2 years', expand = c(0,0)) +
  scale_color_manual(values = c('red')) +
  scale_fill_manual(values = c('blue')) +
  theme_few() +
  theme_adj +
  annotation_logticks() +
  theme(legend.title = element_blank(),
        legend.position = 'none',
        axis.title.x = element_blank())

ggsave('Turner Creek Obs vs Sim.jpg',device = 'jpg', width = 8, height = 4, units = 'in', dpi = 300)

# Legend -------------------------------------------------------------
ggplot(data = NULL, aes(x = Date, y = Flow_cfs)) +
  geom_line(data = Flows_Combo %>% filter(Stream == 'Turner Creek', Type == 'Simulated'), mapping = aes(color = 'Simulated')) +
  geom_point(data = Flows_Combo %>% filter(Stream == 'Turner Creek', Type == 'Observed'), mapping = aes(fill = 'Observed'), shape = 21, size = 1.5) +
  ggtitle('Turner Creek') +
  ylab('Flow (cfs)') +
  scale_y_log10(limits = c(0.001,100), expand= c(0,0), breaks = c(0.001, 0.01, 0.1,1,10,100), labels = as.character(1%o%10^(-3:2))) +
  scale_x_date(limits = c(as.Date('2007-01-01'), '2021-01-01'), date_labels = '%b-%Y', date_breaks = '2 years', expand = c(0,0)) +
  scale_color_manual(values = c('red')) +
  scale_fill_manual(values = c('blue')) +
  theme_few() +
  theme_adj +
  annotation_logticks() +
  theme(legend.title = element_blank(),
        legend.direction = 'horizontal',
        axis.title.x = element_blank())

 ggsave('Legend.jpg', device = 'jpg', width = 16, height = 8, units = 'in', dpi = 300)

