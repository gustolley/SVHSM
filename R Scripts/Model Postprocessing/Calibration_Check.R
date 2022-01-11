# Calibration_Check.R
# User Inputs -------------------------------------------------------------

#General Information
modelDir = ''
outDir =  paste0(modelDir,'Results/')
streamflowObs_file = ''
streamflowSim_file = paste0(modelDir, 'SVHSM_streamflow_UMFFR.dat')
HOB_file = paste0(modelDir, 'SVHSM_HOB_out.dat')
DB_name ='SierraValley'
SharedDB_name = 'OFPShared'
my_server=''
my_username=''
my_pwd=''

# Shapefiles
basinBoundary = 'Sierra_Valley_Groundwater_Basin.shp'

# Temporal Information
WYstart = 2000                                   # Beginning water year of simulation
WYend = 2020                                     # Ending water year of simulatuion

# Script Initialization ---------------------------------------------------
library(dplyr)
library(sf)
library(maptools)
library(units)
library(RODBC)
library(raster)
library(xlsx)
library(lubridate)
library(tidyr)
library(ggplot2)
library(ggthemes)
library(RColorBrewer)
library(colorspace)
library(plotly)
library(mapview)

dir.create(outDir)

theme_adj = theme(plot.title = element_text(hjust = 0.5, size = 14),
                  panel.grid.major.y = element_line(size = 0.1, color = 'gray80'),
                  panel.border = element_rect(color = 'black', fill = NA, size = 1),
                  plot.background = element_rect(fill = NA, color = NA),
                  axis.title = element_text(size = 13),
                  axis.text = element_text(size = 12),
                  axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))

# Functions ---------------------------------------------------------------
mrounddown = function(x,base){                                                                # function for rounding numbers down to specified base
  base*floor(x/base)
}

'%!in%' = function(x,y)!('%in%'(x,y))

# Create RODBC Connection -------------------------------------------------
DB <- odbcDriverConnect(connection = paste0('DRIVER={SQL Server}; 
                                 server=',my_server,';
                                 database=',DB_name,';
                                 uid=',my_username,';
                                 pwd=',my_pwd))

sharedDB <- odbcDriverConnect(connection = paste0('DRIVER={SQL Server}; 
                                 server=',my_server,';
                                 database=',SharedDB_name,';
                                 uid=',my_username,';
                                 pwd=',my_pwd))



# Create Date Arrays ------------------------------------------------------
WYstartDate = paste0(WYstart-1,'-10-01') %>% as.Date()
WYendDate = paste0(WYend,'-09-30') %>% as.Date()
modelDays = seq.Date(from = WYstartDate, to = WYendDate, by = 'day')
modelMonths = seq.Date(from = WYstartDate, to = WYendDate, by = 'month')
modelYears = (WYend - WYstart) + 1
nmonths = length(modelMonths)
# Read Data ---------------------------------------------------------
HOB_data = read.table(HOB_file, header = T) %>%
  rename(Simulated_m = SIMULATED.EQUIVALENT, Observed_m = OBSERVED.VALUE, ObsName = OBSERVATION.NAME, Date = DATE) %>%
  mutate(well_id = strsplit(ObsName, split = '_') %>% sapply("[[",1) %>% as.integer(),
         Simulated_m = if_else(Simulated_m == -5555, NaN, Simulated_m),
         Date = as.Date(Date) - 1,
         Residual_m = Observed_m - Simulated_m) %>%
  left_join(sqlFetch(DB, 'wells') %>% dplyr::select(well_id, well_name, well_total_depth_ft,
                                                    top_of_screen_depth_ft, bottom_of_screen_depth_ft)) %>%
  group_by(well_name)

streamflowObs = read.csv(streamflowObs_file, comment.char = "#", header = T) %>%
  rename(Date = Date.Time, flow_cfs = Flow.Daily.Mean..CFS.) %>%
  mutate(Date = as.Date(Date, '%m/%d/%Y'),
         type = 'Observed') %>%
  dplyr::select(-Quality.Code)

streamflowSim = read.table(streamflowSim_file, skip = 2,  header = F) %>%
  mutate(Date = WYstartDate + V1 - 1,
         flow_cfs = V6*0.000408734569,
         type = 'Simulated') %>%
  dplyr::select(Date, flow_cfs, type)

streamflowSim_comp = read.table(streamflowSim_file_comp, skip = 2,  header = F) %>%
  mutate(Date = WYstartDate + V1 - 1,
         flow_cfs = V6*0.000408734569,
         type = 'Simulated') %>%
  dplyr::select(Date, flow_cfs, type)

# Plot Heads --------------------------------------------------------------
minmax_vals = range(c(HOB_data$Simulated_m, HOB_data$Observed_m), na.rm = T) %>% mrounddown(10)

one2one = data.frame(x = seq(minmax_vals[1],minmax_vals[2])) %>%
  mutate(y=x)
plot_ly(data = HOB_data, x = ~Observed_m, y = ~Simulated_m, color = ~well_name, fill = ~well_name, type = 'scatter', mode = 'markers') %>%
  add_trace(data = one2one, x = ~x, y = ~y, color = '1-1 Line', fill = '1-1 Line', type = 'scatter', mode = 'lines') %>%
  layout(title = 'Observed vs Simulated Water Levels',
         xaxis = list(range = c(1420,1560), tickvals = seq(1420,1560,by = 20), title = 'Observed (m)'),
         yaxis = list(range = c(1420,1560), tickvals = seq(1420,1560,by = 20), title = 'Simulated (m)'))

heads.lm = lm(Simulated_m ~ Observed_m, data = HOB_data)


one2one = ggplot(HOB_data, aes(x = Observed_m*3.28, y= Simulated_m*3.28)) +
  geom_point(fill = 'skyblue', shape = 21, stroke = 0.25) +
  geom_abline(slope = heads.lm$coefficients[2], intercept = heads.lm$coefficients[1]*3.28, linetype = 'dashed', size = 0.5) +
  geom_abline(slope = 1, intercept = 0, size = 0.75, alpha = 1) +
  ggtitle('Simulated vs Observed\nGroundwater Elevations') +
  ylab('Simulated (ft amsl)') +
  xlab('Observed (ft amsl)') +
  scale_y_continuous(limits = c(4650,5100), breaks = seq(4650, 5100, 75), expand = c(0,0)) +
  scale_x_continuous(limits = c(4650,5100), breaks = seq(4650, 5100, 75), expand = c(0,0)) +
  coord_fixed(ratio = 1) +
  theme_few() + 
  theme_adj +
  theme(panel.grid.major = element_line(size = 0.1, color = 'gray80'))
ggsave(plot = one2one, filename = paste0(outDir,'Heads_one2one.png'), device = 'png', width = 4, height = 4, units = 'in', dpi = 600)

Head_Residuals = ggplot(HOB_data, aes(x = well_name, y=Residual_m*3.28, fill = well_name)) +
  geom_hline(yintercept = 0, size = 1) +
  geom_point(shape = 21, size = 1, stroke = 0.25) +
  ylab('Residual (ft)') +
  xlab('Well Name') +
  scale_y_continuous(limits = c(-80,80), breaks = seq(-80, 80, 20), expand = c(0,0)) +
  # scale_x_discrete(labels = unique(HOB_data$well_id)) +
  # coord_fixed(ratio = 1) +
  scale_fill_manual(values = qualitative_hcl(n = 63, palette = 'Set 2')) +
  theme_few() + 
  theme_adj +
  theme(panel.grid.major = element_line(size = 0.1, color = 'gray80'),
        axis.text.x = element_text(angle = 45, size = 6),
        legend.position = 'none')
  ggsave(plot = Head_Residuals, filename = paste0(outDir,'Heads_residuals.png'), device = 'png', width = 7.5, height = 4, units = 'in', dpi = 600)

# Plot Residuals ----------------------------------------------------------
basinBoundary.sf = st_read(basinBoundary)

Head_Residuals.sf = HOB_data %>%
  summarize(Min_abs = min(abs(Residual_m)),
            Max_abs = max(abs(Residual_m)),
            Min_pos = min(Residual_m[Residual_m>0]),
            Max_neg = max(Residual_m[Residual_m<0]),
            Min_global = min(Residual_m),
            Max_global = max(Residual_m),
            Mean_m = mean(Residual_m),
            StDev_m = sd(Residual_m),
            Median_m = median(Residual_m),
            count = n()) %>%
  mutate(Min_m = if_else(Min_abs==abs(Max_neg), Max_neg, Min_pos),
         Max_m = if_else(Max_abs==abs(Max_global), Max_global, Min_global),
         Mean_ft = Mean_m*3.28,
         Min_ft = Min_m*3.28,
         Max_ft = Max_m*3.28,
         Median_ft = Median_m*3.28,
         StDev_ft = StDev_m*3.28) %>%
  dplyr::select(well_name, Mean_m, Mean_ft, Min_m, Min_ft, Max_m, Max_ft, Median_m, Median_ft, StDev_m, StDev_ft, count) %>%
left_join(sqlFetch(DB, 'wells') %>% dplyr::select(well_id, well_name, well_long, well_lat)) %>%
  st_as_sf(coords = c('well_long', 'well_lat'), crs = 4269) %>%
  st_transform(crs = 26910)

mapview(Head_Residuals.sf, cex = 'Mean') + mapview(basinBoundary.sf)

st_write(Head_Residuals.sf, dsn = paste0(outDir,'Head_Residuals.shp'), delete_dsn = T)

# HOB_data %>% 
#   #filter(well_name == 'DMW 2s') %>% 
#   ggplot(aes(x = Observed_m, y = Simulated_m, color = well_name)) +
#   geom_point() +
#   geom_abline(slope = 1)
# 
# HOB_data %>% 
#   #filter(well_name == 'DMW 2s') %>% 
#   plot_ly(x = ~Observed_m, y = ~Simulated_m, color = ~well_name, type = 'scatter') 
# 
# HOB_data %>%
#   dplyr::select(Simulated_m, Observed_m, well_name, water_level_date) %>%
#   pivot_longer(!c(well_name, water_level_date), names_to = 'WL_type', values_to = 'Value') %>%
#   filter(well_name == 'DMW 2s', WL_type == 'Observed_m') %>%
#   arrange(water_level_date) %>%
#   ggplot(aes(x = water_level_date, y = Value, color = WL_type)) +
#   geom_line() + 
#   geom_point()
  

# Plot Streamflow ---------------------------------------------------------

flows_combined = rbind(streamflowObs, streamflowSim)
#flows_combined = rbind(streamflowObs, streamflowSim, streamflowSim_comp)

NSE = flows_combined %>%
  pivot_wider(id_cols = Date, names_from = 'type', values_from = 'flow_cfs') %>%
  filter(!is.na(Observed)) %>%
  mutate(log_obs = log10(Observed),
         log_sim = log10(Simulated),
         residual_sq = (log_obs-log_sim)**2,
         var_sq = (log_obs - mean(log_obs))**2) %>%
  summarise(residual_sum = sum(residual_sq),
            var_sum = sum(var_sq)) %>%
  mutate(NSE = 1-(residual_sum/var_sum)) %>%
  dplyr::select(NSE) %>%
  unlist() %>%
  round(2)

plot_ly(flows_combined, x = ~Date, y = ~flow_cfs, color = ~type, colors = c('blue', 'red'), type = 'scatter', mode = 'lines') %>%
  layout(xaxis = list(title = F),
         yaxis = list(type = "log", title = 'Streamflow (cfs)', tickformat =  '.1r'),
         title = 'Observed and Simulated Streamflow') %>%
  add_annotations(x = as.Date('2016-04-01'),
                  y = 0.001,
                  ax = 0,
                  ay=0,
                  text = paste('NSE =',NSE),
                  showarrow = F,
                  font = list(size = 20))

MFP_plot = ggplot(flows_combined, aes(x = Date, y = flow_cfs, color = type)) +
  geom_line(size = 0.75) +
  ggtitle('Middle Fork Feather River (MFP)\nObserved and Simulated Streamflow') +
  ylab('Streamflow (cfs)') +
  scale_y_log10(limits = c(0.1,10000), breaks = c(1%o%10^(-1:4)),labels = c('0.1', '1', '10', '100', '1,000', '10,000'), expand = c(0,0)) +
  scale_x_date(limits = c(as.Date('1999-10-01'), as.Date('2020-10-01')), breaks = seq.Date(from = as.Date('1999-10-01'),to = as.Date('2020-10-01'), by = '24 months'), expand = c(0,0), date_labels = '%b-%Y') +
  scale_color_brewer(type = 'div', palette = 'Set1', direction = -1) +
  theme_few() +
  theme_adj +
  theme(axis.title.x = element_blank(),
        legend.title = element_blank(),
        legend.position = c(0.1, 0.9),
        legend.background = element_rect(fill = NA, color = NA),
        legend.key = element_rect(fill = NA)) +
  annotate(geom = 'text', x = as.Date('2002-01-01'), y = 2, label = paste('NSE =',NSE))
ggsave(plot = MFP_plot, filename = paste0(outDir,'Streamflow_UMFFR_Obs_v_Sim.png'), device = 'png', width = 7.5, height = 4, units = 'in', dpi =600)

# Check Volumes
Observed_TAF = streamflowObs %>% 
  filter(Date >= as.Date('2006-10-01'), Date < as.Date('2018-10-01')) %>% 
  mutate(WY = if_else(format(Date,'%b')%in%month.abb[1:9],      # populate water year
                      format(Date, '%Y') %>% as.numeric(),
                      format(Date, '%Y') %>% as.numeric()+1)) %>%
  summarise(total = sum(flow_cfs)*1.983/1000) %>%
  as.numeric()

Simulated_TAF = streamflowSim %>% 
  filter(Date >= as.Date('2006-10-01'), Date < as.Date('2018-10-01')) %>% 
  mutate(WY = if_else(format(Date,'%b')%in%month.abb[1:9],      # populate water year
                      format(Date, '%Y') %>% as.numeric(),
                      format(Date, '%Y') %>% as.numeric()+1)) %>%
  summarise(total = sum(flow_cfs)*1.983/1000) %>%
  as.numeric()

Volume_pct_diff = abs(Observed_TAF - Simulated_TAF)/mean(Observed_TAF, Simulated_TAF)*100
