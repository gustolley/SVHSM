# UCODE Input Files.R
# User Inputs -------------------------------------------------------------

#General Information
modelName = 'SVHSM'
outDirUCODE = 'UCODE_v3/'
DB_name ='SierraValley'
SharedDB_name = 'OFPShared'
my_server=''
my_username=''
my_pwd=''

# Shapefiles
basinBoundary = 'Sierra_Valley_Groundwater_Basin.shp' 
modelGrid = 'modelGrid_MF.shp'

# Temporal Information
WYstart = 2000                                   # Beginning water year of simulation
WYend = 2020                                     # Ending water year of simulation

# Observation Info Files
HOB_info = 'MODFLOW_v3/SVHSM_Water_Level_Observations.dat'
pumping_inputs = 'SWBM/input_150m_grid/ag_well_specified_volume.txt'
streamflow_UMFFR = 'A55420_Flow_Daily_Mean.csv'

# Script Initialization ---------------------------------------------------
library(dplyr)
library(sf)
library(maptools)
library(units)
library(tictoc)
library(RODBC)
library(raster)
library(xlsx)
library(lubridate)
library(tidyr)

dir.create(paste0(outDirUCODE, 'UCODE_Input_Files/'), showWarnings = F)
dir.create(paste0(outDirUCODE, 'UCODE_Instruction_Files/'), showWarnings = F)
dir.create(paste0(outDirUCODE, 'UCODE_Template_Files/'), showWarnings = F)


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

# Read Shapefiles ---------------------------------------------------------
basinBoundary.sf = st_read(basinBoundary)
modelGrid.sf = st_read(modelGrid)

# HOB Input and Instruction Files -------------------------------------------------------
HOB_data = read.table(HOB_info, header = T, sep = '\t') %>%
  mutate(water_level_date = as.Date(water_level_date)) %>%
  group_by(well_id) %>%
  mutate(obs_num = seq(1,n()),
         ObsName = paste0('W',well_id,'_',obs_num,'_',format(water_level_date,'%Y%m%d')),
         GroupName = 'Heads',
         buffer = '',
         water_level_elev_m = round(water_level_elev_m,2)) %>%
  ungroup()

# Input File
cat('BEGIN Observation_Data Table', file = paste0(outDirUCODE, 'UCODE_Input_Files/',modelName,'.headobs'), append = F, sep = '\n')
cat(paste0('  NROW=',nrow(HOB_data),' NCOL=3 COLUMNLABELS'), file = paste0(outDirUCODE, 'UCODE_Input_Files/',modelName,'.headobs'), append = T, sep = '\n')
cat('  ObsName  GroupName  ObsValue', file = paste0(outDirUCODE, 'UCODE_Input_Files/',modelName,'.headobs'), append = T, sep = '\n')
write.table(HOB_data %>% dplyr::select(buffer,ObsName, GroupName, water_level_elev_m),
            file = paste0(outDirUCODE, 'UCODE_Input_Files/',modelName,'.headobs'), 
            append = T, row.names = F, col.names = F, quote = F, sep = '  ', eol = '\n')
cat('END Observation_Data', file = paste0(outDirUCODE, 'UCODE_Input_Files/',modelName,'.headobs'), append = T, sep = '\n')

# Instruction File
cat('jif @', file = paste0(outDirUCODE, 'UCODE_Instruction_Files/',modelName,'_HOB_out_dat.jif'), append = F, sep = '\n')
cat(paste0('StandardFile  1  1  ',nrow(HOB_data)), file = paste0(outDirUCODE, 'UCODE_Instruction_Files/',modelName,'_HOB_out_dat.jif'), append = T, sep = '\n')
write.table(HOB_data$ObsName, file = paste0(outDirUCODE, 'UCODE_Instruction_Files/',modelName,'_HOB_out_dat.jif'), 
            append = T, row.names = F, col.names = F, quote = F, eol = '\n')

# Pumping Observations Input and Instruction Files ------------------------
pumping_vols = read.table(pumping_inputs, head = T) %>%
  rename(Date = well_id) %>%
  mutate(Date = as.Date(paste0('01',Date),'%d%b%Y')) %>%
  filter(specify_pumping) %>%
  mutate(WY = if_else(format(Date,'%b')%in%month.abb[10:12], format(Date, '%Y') %>% as.numeric() + 1,format(Date, '%Y') %>% as.numeric())) %>%
  dplyr::select(-Date,-specify_pumping) %>%
  group_by(WY) %>%
  summarize_all(.funs = sum) %>%
  filter(WY >= WYstart, WY<= WYend)

pumping_data = data.frame(buffer = '',
                          ObsName = paste0('Ag_Pumping_WY_',pumping_vols$WY),
                          GroupName = 'Est_Ag_GW',
                          ObsValue = rowSums(pumping_vols[,-1]) %>% round(0))

# Input File
cat('BEGIN Observation_Data Table', file = paste0(outDirUCODE, 'UCODE_Input_Files/',modelName,'.pumpingobs'), append = F, sep = '\n')
cat(paste0('  NROW=',nrow(pumping_data),' NCOL=3 COLUMNLABELS'), file = paste0(outDirUCODE, 'UCODE_Input_Files/',modelName,'.pumpingobs'), append = T, sep = '\n')
cat('  ObsName  GroupName  ObsValue', file = paste0(outDirUCODE, 'UCODE_Input_Files/',modelName,'.pumpingobs'), append = T, sep = '\n')
write.table(pumping_data, file = paste0(outDirUCODE, 'UCODE_Input_Files/',modelName,'.pumpingobs'), 
            append = T, row.names = F, col.names = F, quote = F, sep = '  ', eol = '\n')
cat('END Observation_Data', file = paste0(outDirUCODE, 'UCODE_Input_Files/',modelName,'.pumpingobs'), append = T, sep = '\n')

# Instruction File
cat('jif @', file = paste0(outDirUCODE, 'UCODE_Instruction_Files/Annual_Groundwater_Pumping_Totals_dat.jif'), append = F, sep = '\n')
cat(paste0('StandardFile  1  2  ',nrow(pumping_data)), file = paste0(outDirUCODE, 'UCODE_Instruction_Files/Annual_Groundwater_Pumping_Totals_dat.jif'), append = T, sep = '\n')
write.table(pumping_data$ObsName, file = paste0(outDirUCODE, 'UCODE_Instruction_Files/Annual_Groundwater_Pumping_Totals_dat.jif'), 
            append = T, row.names = F, col.names = F, quote = F, eol = '\n')

# Streamflow Observations Input and Instruction Files ------------------------
UMFFR = read.csv(streamflow_UMFFR, skip = 8, head = T) %>%
  rename(flow_cfs = Flow.Daily.Mean..CFS.) %>%
  filter(Quality.Code == '1: Good data') %>%
  mutate(Date = as.Date(Date.Time, '%m/%d/%Y'),
         flow_cat = case_when(
           flow_cfs >= 1 &  flow_cfs < 10~ 'low flow',
           flow_cfs >= 10 & flow_cfs < 100 ~ 'med flow',
           flow_cfs >= 100 ~ 'high flow')) 

UMFFR_low_flows = UMFFR %>%
  filter(flow_cat == 'low flow') %>%
  slice_sample(n = 100) %>%
  arrange(Date) %>%
  mutate(ObsName = paste0('UMMFR_',format(Date,'%Y%m%d')),
         GroupName = 'UMFFR_low',
         ObsValue = flow_cfs*2446.58 )
UMFFR_med_flows = UMFFR %>%
  filter(flow_cat == 'med flow') %>%
  slice_sample(n = 300) %>%
  arrange(Date) %>%
  mutate(ObsName = paste0('UMMFR_',format(Date,'%Y%m%d')),
         GroupName = 'UMFFR_med',
         ObsValue = flow_cfs*2446.58 )
UMFFR_high_flows = UMFFR %>%
  filter(flow_cat == 'high flow') %>%
  slice_sample(n = 100) %>%
  arrange(Date) %>%
  mutate(ObsName = paste0('UMMFR_',format(Date,'%Y%m%d')),
         GroupName = 'UMFFR_high',
         ObsValue = flow_cfs*2446.58 )

UMFFR_Obs = rbind(UMFFR_low_flows, UMFFR_med_flows, UMFFR_high_flows) %>%
  arrange(Date) %>%
  filter(Date > WYstartDate + 365, Date < WYendDate) %>%
  mutate(buffer = '',
         gage_lines = c(which(modelDays%in%.$Date, arr.ind = T)[1], which(modelDays%in%.$Date, arr.ind = T) %>% diff()),
         jif_text = paste0('l',gage_lines,' (',ObsName,')86:99'))

# Input File
cat('BEGIN Observation_Data Table', file = paste0(outDirUCODE, 'UCODE_Input_Files/',modelName,'.flowobs'), append = F, sep = '\n')
cat(paste0('  NROW=',nrow(UMFFR_Obs),' NCOL=3 COLUMNLABELS'), file = paste0(outDirUCODE, 'UCODE_Input_Files/',modelName,'.flowobs'), append = T, sep = '\n')
cat('  ObsName  GroupName  ObsValue', file = paste0(outDirUCODE, 'UCODE_Input_Files/',modelName,'.flowobs'), append = T, sep = '\n')
write.table(UMFFR_Obs %>% dplyr::select(buffer, ObsName, GroupName, ObsValue), file = paste0(outDirUCODE, 'UCODE_Input_Files/',modelName,'.flowobs'), 
            append = T, row.names = F, col.names = F, quote = F, sep = '  ', eol = '\n')
cat('END Observation_Data', file = paste0(outDirUCODE, 'UCODE_Input_Files/',modelName,'.flowobs'), append = T, sep = '\n')

# Instruction File
cat('jif @', file = paste0(outDirUCODE, 'UCODE_Instruction_Files/SVHSM_streamflow_UMFFR_dat.jif'), append = F, sep = '\n')
cat('@"DATA: Time@', file = paste0(outDirUCODE, 'UCODE_Instruction_Files/SVHSM_streamflow_UMFFR_dat.jif'), append = T, sep = '\n')
write.table(UMFFR_Obs$jif_text, file = paste0(outDirUCODE, 'UCODE_Instruction_Files/SVHSM_streamflow_UMFFR_dat.jif'), 
            append = T, row.names = F, col.names = F, quote = F, eol = '\n')


