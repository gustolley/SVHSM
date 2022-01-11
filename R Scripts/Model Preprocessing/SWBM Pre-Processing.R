# SWBM Pre-Processing.R
# User Inputs -------------------------------------------------------------

#General Information
modelName = 'SVHSM'
outDirSWBM = 'SWBM/input_150m_grid/'
outdirGIS = 'GIS/Modeling_150m_grid/'
calSoft = 'UCODE'                                # Calibration software (UCODE or PEST) 
Scenario = 'Basecase'                            # 
DB_name ='SierraValley'
SharedDB_name = 'OFPShared'
my_server=''
my_username=''
my_pwd=''

# Temporal Information
WYstart = 2000                                   # Beginning water year of simulation
WYend = 2020                                     # Ending water year of simulatuion

# Model Discretization
modelGrid = 'grid_150m_35deg_cc.shp'    
MF_L1_active_cells = 'L1active_cells.txt' # matrix of active cells in layer 1 (same used in BAS file)
basinBoundary = 'Sierra_Valley_Groundwater_Basin.shp' 

# Climate
PRISM_data_dir = 'PRISM_ppt_30yr_normal_800mM2_annual_bil/'

# Stream Inflows (from PRMS)
prms_subbasins = 'model_points_new5_35c.shp'     # Point shapefile with locations of real or synthetic gage data used in gsflow python scripts
statvar_file = 'statvar.dat'                               # Daily output from PRMS
param_out = 'sub_cfs'                                                                                                                   # Flag for PRMS output units (specified in statvar file)                                                   

# Stream Network (for MODFLOW)
stream_segments = 'SVHSM_Streams_20210713.shp'                 # Line shapefile with ISEG, 
points_along_segments = 'SVHSM_Streams_pts_15m_20210713.shp'

# DEM (for MODFLOW)
DEM = 'DEM_Leapfrog_extent.tif'

# Agricultural Information (NOTE: see landuse_table.txt  and kc_values.txt section for more detailed inputs)
fields = 'DWR_Landuse_2013_soils_intrsct_modified_20210728.shp'
nlandcover = 9
Alfalfa_Kc_dates =  c('04-01', '10-15')   # Array of dates in mm-dd format where Kc values change 
Alfalfa_Kc_values = c(0.9, 0.9)           # Array of Kc values that correspond with dates. Values are linearly interpolated between dates.
Grain_Kc_dates =  c('04-01', '04-15', '05-01', '06-01', '07-20')     # Array of dates in mm-dd format where Kc values change 
Grain_Kc_values = c(0, 0.3, 1.15, 1.15, 0)   # Array of Kc values that correspond with dates. Values are linearly interpolated between dates.
Pasture_Kc_dates =  c('03-15', '11-01')      # Array of dates in mm-dd format where Kc values change 
Pasture_Kc_values = c(0.9, 0.9)              # Array of Kc values that correspond with dates. Values are linearly interpolated between dates.
Native_Veg_Kc_dates = c('01-01', '02-01', '03-01', '04-01', '05-01', '06-01', '07-01', '08-01', '09-01', '10-01', '11-01', '12-01', '12-31')        # Array of dates in mm-dd format where Kc values change 
Native_Veg_Kc_values = c(0,0,0,0.2,0.4,0.6,0.8,0.8,0.8,0.6,0.4,0.2,0)              # Array of Kc values that correspond with dates. Values are linearly interpolated between dates.


# MODFLOW Parameters
CONST = 86400              # SFR package: A real value (or conversion factor) used in calculating stream depth for stream reach (see documentation for proper value)
DLEAK = 0.0001             # SFR package: A real value equal to the closure tolerance for stream depth used to calculate leakage between each stream reach and active model cell.
ISTCB1 = 50                # SFR package: unit number for cell-by-cell budget file
ISTCB2 = 600               # SFR package: unit number for global streamflow output file
IWL2CB = 50                # MNW2 package: unit number for cell-by-cell budget file
LOSSTYPE = 'SPECIFYcwc'    # character flag to determine the user-specified model for well loss (currently handles THIEM or SPECIFYcwc)

# Script Initialization ---------------------------------------------------
dir.create(outDirSWBM)
dir.create(paste0(outDirSWBM, 'recharge/'))
dir.create(outdirGIS)
dir.create(paste0(outdirGIS, 'Grid/'))
dir.create(paste0(outdirGIS, 'Landuse/'))
dir.create(paste0(outdirGIS, 'Surface Water/'))
dir.create(paste0(outdirGIS, 'Wells/'))

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

# Functions ---------------------------------------------------------------
mrounddown = function(x,base){                                                                # function for rounding numbers down to specified base
  base*floor(x/base)
}

'%!in%' = function(x,y)!('%in%'(x,y))

Kc_timeseries = function(startDate, endDate, break_days, Kc_values){
  if(length(break_days)!= length(Kc_values)){stop('vector of breaks and Kc values must be the same')}
  break_days = paste0('2000-',break_days) %>% as.Date()
  Kc.df = data.frame(Date = seq.Date(from = as.Date(startDate), to = as.Date(endDate), by = 'day'),
                     Kc = 0)
  for (i in 2:length(break_days)){
    temp.days = seq.Date(from = break_days[i-1], to = break_days[i], by = 'day') %>% format('%m-%d')
    temp.values = seq(Kc_values[i-1], Kc_values[i], length.out = length(temp.days)) %>% round(digits = 4)
    Kc.df$Kc[format(Kc.df$Date,'%m-%d')%in%temp.days] = temp.values
  }
  return(Kc.df)
}

###
###


Two_Crop_Rotation = function(df, Landcover_id_1, Landcover_id_2, rotation_interval, nmonths){   # Two_Crop_Rotation(initial_landcover, 1, 1, 2, 8, nmonths)
  if(rotation_interval>(nmonths/12)){ # if rotation interval is longer than simulation period then no rotations are necessary and initial field info is populated for nmonths
    if(is.matrix(df)){                     # If input data is Landcover matrix, use first row as initial landcover                     
      df = data.frame(SWBM_LU = df[1,])  
      Landcover_matrix = matrix(data = rep(df %>% unlist(),nmonths), nrow = nmonths, byrow = T)
    } else {                               # If input data is vector, repeat it to create Landcover matrix       
    Landcover_matrix = matrix(data = rep(df %>% unlist(),nmonths), nrow = nmonths, byrow = T)
    }
    return(Landcover_matrix)
  }  else{                       # rotation interval is longer than simulation period and rotations are necessary
    if(is.matrix(df)){
      old_Landcover_matrix = df
      matrix_in = TRUE
      df = data.frame(SWBM_LU = df[1,])
    } else {
      matrix_in = FALSE
    }
    Landcover_matrix = matrix(nrow = nmonths, ncol = nrow(df), data = NA)
    if(length(Landcover_id_1)==1) {
      if (length(Landcover_id_1) != 1 | length(Landcover_id_2) != 1 | length(rotation_interval) != 1){
        stop('Error: Landcover and rotation interval inputs are of different lengths')
      }
      rotation_field_idx = which(df$SWBM_LU==Landcover_id_1 | df$SWBM_LU==Landcover_id_2)
      n_rotation_fields = length(rotation_field_idx)
      n_annual_rotation = mrounddown(n_rotation_fields, rotation_interval)/rotation_interval
      rotation_periods = c(1, seq(16, nmonths, by = 12))                                                             # Rotations occur in first October (initialization) and in January (excluding first Jan)
      rotation_completion_periods = rotation_periods[seq(rotation_interval,nmonths/12, by = rotation_interval)]      # End of rotation cycle (for handling condition where number of fields is not evenly divisible by rotation interval)  
      rotation = 0
      for(i in 1:nmonths){
        if(matrix_in){df = data.frame(SWBM_LU = old_Landcover_matrix[i,])}
        if (i%in%rotation_periods & i%!in%rotation_completion_periods) {
          rotation_id_start = 1+(rotation*n_annual_rotation)
          rotation_id_end = (rotation+1)*n_annual_rotation
          Landcover_2_fields = rotation_field_idx[rotation_id_start:rotation_id_end]
          Landcover_1_fields = rotation_field_idx[rotation_field_idx%!in%Landcover_2_fields]
          df$SWBM_LU[Landcover_2_fields] = Landcover_id_2
          df$SWBM_LU[Landcover_1_fields] = Landcover_id_1
          landcover = df$SWBM_LU %>% as.matrix(nrows = 1)
          rotation = rotation + 1
        } else if (i%in%rotation_completion_periods){
          rotation_id_start = 1+(rotation*n_annual_rotation)
          rotation_id_end = n_rotation_fields
          Landcover_2_fields = rotation_field_idx[rotation_id_start:rotation_id_end]
          Landcover_1_fields = rotation_field_idx[rotation_field_idx%!in%Landcover_2_fields]
          df$SWBM_LU[Landcover_2_fields] = Landcover_id_2
          df$SWBM_LU[Landcover_1_fields] = Landcover_id_1
          landcover = df$SWBM_LU %>% as.matrix(nrows = 1)
          rotation = 0
        }
        Landcover_matrix[i,] = landcover
        
      }
    } else if(length(Landuse)>1){
      # CODE FOR MULTIPLE LANDCOVER ROTATIONS HERE
      
    }
    return(Landcover_matrix)
  }
}

PRMS_monthly_vol = function(statvar_file, param_type, subbasin_id, subbasin_name, startDate, endDate){
  if(param_type == 'sub_cfs'){                  # assign output column name based on PRMS output units
    value_header = 'Monthly_Volume_ft3'
  } else if(param_type == 'sub_cms'){
    value_header = 'Monthly_Volume_m3'
  }
  n_params = readLines(statvar_file, n = 1) %>% as.integer()                                              # read number of parameters in statvar file
  prms_params = read.table(statvar_file, skip = 1, nrows = n_params, col.names = c('param_type', 'id'))   # read parameter names
  flow_idx = which(prms_params$param_type == param_type & prms_params$id%in%subbasin_id) + 7              # locate which columns are param_type (first 7 columns are always output by PRMS)
  prms_flows = read.table(statvar_file, skip = n_params+1) %>%                                            # read data
    mutate(modelMonth = as.Date(paste(V2, V3, V4, sep = '-')),                                            # convert timing info to date
           modelMonth = format(modelMonth, '%b-%Y')) %>%                                                  # convert date to month for grouping 
    dplyr::select(modelMonth, paste0('V',flow_idx))                                                       # select only param_type columns 
  names(prms_flows) = c('modelMonth', subbasin_name)                                                      # rename columns 
  
  prms_flows = prms_flows %>% 
    pivot_longer(cols = -modelMonth, names_to = 'Stream', values_to = 'flow_rate') %>%                    # convert to long table for data managemnet
    group_by(modelMonth, Stream) %>%                                                                      # group for aggregating
    summarize(!!sym(value_header) := sum(flow_rate)* 86400) %>%                                           # aggregate daily flow rates to monthly volumes
    mutate(modelMonth = as.Date(paste0('01-',modelMonth), '%d-%b-%Y')) %>%                                # convert to date for sorting
    arrange(modelMonth) %>%
    filter(modelMonth>=startDate & modelMonth<=endDate) %>%                                               # filter data for MODFLOW dates
    ungroup()
  return(prms_flows)
}

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

tic('SWBM Pre-Processing Complete')
# Create Date Arrays ------------------------------------------------------
WYstartDate = paste0(WYstart-1,'-10-01') %>% as.Date()
WYendDate = paste0(WYend,'-09-30') %>% as.Date()
modelDays = seq.Date(from = WYstartDate, to = WYendDate, by = 'day')
modelMonths = seq.Date(from = WYstartDate, to = WYendDate, by = 'month')
modelYears = (WYend - WYstart) + 1
nmonths = length(modelMonths)
# Read Shapefiles and Rasters ---------------------------------------------------------
modelGrid.sf = st_read(modelGrid)
fields.sf = st_read(fields) %>% 
  dplyr::select(MUKEY, MUNAME, subws_Name, subws_ID, SWBM_LUtxt, SWBM_LU, SWBM_IRRtx, SWBM_IRR, WtrSrcTxt, WATERSOURC, WL_2_CP_Yr, AWCcmprcm, well_id, BdgtZoneID, BdgtZone) %>%
  mutate(MUKEY = as.integer(MUKEY),
         SWBM_LU = as.integer(SWBM_LU),
         SWBM_IRR = as.integer(SWBM_IRR),
         WATERSOURC = as.integer(WATERSOURC),
         well_id = as.integer(well_id),
         SWBM_id = sample(1:nrow(.), nrow(.), replace = F),
         Area_m2 = st_area(.) %>% round(2),
         Area_acres = set_units(Area_m2, acre))

basinBoundary.sf = st_read(basinBoundary)
prms_subbasins.sf = st_read(prms_subbasins)
stream_segments.sf = st_read(stream_segments)
points_along_segments.sf = st_read(points_along_segments)
DEM.raster = raster(DEM)

# Subset MODFLOW Grid -----------------------------------------------------
rowRange = modelGrid.sf %>%                       # Find range of rows with active cells
  filter(cell_type == 'MODFLOW') %>%
  dplyr::select(row) %>%
  st_drop_geometry() %>%
  range()
numRow_MF = rowRange[2] - rowRange[1] + 1            # Calculate number of active rows

colRange = modelGrid.sf %>%                       # Find range of columns with active cells
  filter(cell_type == 'MODFLOW') %>%
  dplyr::select(col) %>%
  st_drop_geometry() %>%
  range()
numCol_MF = colRange[2] - colRange[1] + 1            # Calculate number of active columns

modelGrid_MF.sf = modelGrid.sf %>%
  filter(row >= rowRange[1],
         row <= rowRange[2],
         col >= colRange[1],
         col <= colRange[2]) %>%
  rename(row_global = row,
         col_global = col) %>%
  mutate(row_MF = rep(seq(1,numRow_MF),each = numCol_MF),
         col_MF = rep(seq(1,numCol_MF), numRow_MF),
         MF_idx = seq(1,numRow_MF*numCol_MF)) %>%
  relocate(row_MF, col_MF, MF_idx, cell_type, row_global, col_global, global_id)
st_write(modelGrid_MF.sf, dsn = paste0(outdirGIS, 'Grid/modelGrid_MF.shp'), delete_dsn = T)
# polygons_table.txt ------------------------------------------------------
fieldsMF.sf = modelGrid_MF.sf %>%                                                                               # Join field information to MODFLOW cells (keep field data with largest area within cell)
  mutate(L1_active = read.table(MF_L1_active_cells, header = F) %>% as.matrix() %>% t() %>% as.vector()) %>%    # add field identifying MODFLOW active cells in Layer 1 (read matrix, transpose, then unlist)
  filter(L1_active == 1) %>%                                                                                       # filter active cells
  st_join(fields.sf, largest = T) %>%                                                                           # join active cells to fields shapefile
  arrange(SWBM_id)                                                                                              # arrange by SWBM id

fields.sf = fields.sf %>%                  
  filter(SWBM_id%in%unique(fieldsMF.sf$SWBM_id)) %>%                                                            # Remove fields that are smaller than the MODFLOW resolution
  mutate(SWBM_id = sample(1:nrow(.), nrow(.), replace = F)) %>%                                                 # Create new SWBM id values
  arrange(SWBM_id) %>%                                                                                          # arrange by SWBM id
  st_make_valid()                                                                                               # make sure geometry is valid

fieldsMF.sf = modelGrid_MF.sf %>%                                                                               # Repeat join on subsetted fields to preserve new field IDs              
  mutate(L1_active = read.table(MF_L1_active_cells, header = F) %>% as.matrix() %>% t() %>% as.vector())  %>%   # add field identifying MODFLOW active cells in Layer 1 (read matrix, transpose, then unlist)
  filter(L1_active == 1) %>%                                                                                       # filter active cells
  st_join(fields.sf, largest = T) %>%   
  mutate(MF_Area_m2 = st_area(.)) %>%
  arrange(SWBM_id)

fieldsMF_dissolved.sf = fieldsMF.sf %>%                           # Dissolve MF field cells
  group_by(SWBM_id, subws_Name, subws_ID, SWBM_LU, SWBM_LUtxt, SWBM_IRR, SWBM_IRRtx, WL_2_CP_Yr, WATERSOURC, WtrSrcTxt, AWCcmprcm, well_id, BdgtZoneID, BdgtZone) %>%
  summarise(MF_Area_m2 = sum(MF_Area_m2) %>% as.numeric() %>% round(2) %>% format(scientific = F)) %>%
  left_join(fields.sf %>% st_drop_geometry() %>% dplyr::select(SWBM_id, Area_m2, Area_acres), by = 'SWBM_id')

st_write(fields.sf, dsn = paste0(outdirGIS, 'Land Use/SWBM_Fields.shp'), delete_dsn = T)
st_write(fieldsMF_dissolved.sf, dsn = paste0(outdirGIS,'Land Use/SWBM_Fields_MF_grid.shp'), delete_dsn = T) 

polygons_table = fieldsMF_dissolved.sf %>% 
  ungroup() %>%
  st_drop_geometry() %>% 
  mutate(Initial_fill_fraction = 0.5, 
         ILR_Flag = F, 
         Notes = '',
         subws_Name = gsub(x = subws_Name, pattern = ' ', replacement = '_'),
         WL_2_CP_Yr = if_else(is.na(WL_2_CP_Yr), as.integer(0), WL_2_CP_Yr)) %>%
  dplyr::select(SWBM_id, subws_ID, SWBM_LU, SWBM_IRR, MF_Area_m2, WATERSOURC, AWCcmprcm, Initial_fill_fraction, WL_2_CP_Yr, ILR_Flag, Notes)
write.table(polygons_table, file = paste0(outDirSWBM, 'polygons_table.txt'), sep = '  ', quote = F, row.names = F)                                   # write polygons input file
npoly = nrow(polygons_table)

# landcover_table.txt ----------------------------------------------------------
# modify table below to add different landcovers represented in the model
landcover_table = data.frame(Landcover_id = seq(1,nlandcover),                            # Unique identifier for each land cover type                                                        
                             Landcover_Name = c('Alfalfa_Irrigated', 'Grain_Irrigated', 'Pasture_Irrigated',               # Character field for land cover
                                                'Native_Vegetation', 'Urban_Barren', 'Water',
                                                'Alfalfa_Non-Irrigated', 'Grain_Non-Irrigated', 'Pasture_Non-Irrigated'),
                             IrrFlag = c(T, T, T, F, F, F, F, F, F),                                  # Logical for designating if field is irrigated or not
                             ET_Flag = c(T, T, T, T, F, T, T, T, T),                                  # Logical for applying ET to a field
                             IrrSWC = c(0.55,0.55,0.45,0,0,0,0,0,0),                             # Maximum allowable depletion % (irrigation trigger)
                             IrrMonStart = c(3,3,4,0,0,0,0,0,0),                                   # Calendar month when irrigation starts (Jan =1, Feb =2, ..., Dec = 12)   
                             IrrDayStart = c(25,16,15,0,0,0,0,0,0),                                # Day of month that irrigation starts
                             IrrMonEnd = c(8,7,10,0,0,0,0,0,0),                                    # Calendar month when irrigation ends (Jan =1, Feb =2, ..., Dec = 12)
                             IrrDayEnd = c(31,10,15,0,0,0,0,0,0),                                  # Day of month that irrigation starts
                             RootDepth = c(2.44,1,1,2.44,0,2,2.44,1,1),                            # Root depth (multiplied by AWCcmpercm to get total WHC)
                             IrrEff_Flood = c(0.7,0.7,0.7,0,0,0,0,0,0),                      # Irrigation efficiency for flood irrigation
                             IrrEff_WL = c(1,1,0.85,0,0,0,0,0,0),                               # Irrigation efficiency for wheel line irrigation
                             IrrEff_CP = c(1.15,1.15,0.9,0,0,0,0,0,0),                     # Irrigation efficiency for center pivot irrigation
                             Kc_Mult = 1)                                                          # Crop coefficient multiplier (scaling parameter for calibration)

write.table(landcover_table, file = paste0(outDirSWBM, 'landcover_table.txt'),
            sep = '  ', quote = F, row.names = F)      

# polygon_landcover_ids.txt ----------------------------------------------------
# Landcover_id is specified for each field every month by the user
initial_landcover = fieldsMF_dissolved.sf %>%
  ungroup()  %>%
  st_drop_geometry() %>%
  dplyr::select(SWBM_id, SWBM_LU) %>%
  arrange(SWBM_id) %>%
  dplyr::select(SWBM_LU)

Landcover = Two_Crop_Rotation(initial_landcover, Landcover_id_1 = 1, Landcover_id_2 = 2, rotation_interval = 8, nmonths) %>%
  Two_Crop_Rotation(Landcover_id_1 = 7, Landcover_id_2 = 8, rotation_interval = 8, nmonths)

write(x = c('Stress_Period', paste0('SWBM_ID_',seq(1,npoly))), file = paste0(outDirSWBM, 'polygon_landcover_ids.txt'), ncolumns = ncol(Landcover)+1, append = F, sep = '  ')
for (i in 1:nmonths){
  write(x = c(format(modelMonths[i],'%b-%Y'), Landcover[i,]), file = paste0(outDirSWBM, 'polygon_landcover_ids.txt'), ncolumns = ncol(Landcover)+1, append = T, sep = '  ')  
}

# ref_et.txt --------------------------------------------------------------
# Header: Date ETo_m

Macdoel = read.csv('CIMIS_Macdoel_II_236_May2015-2020.csv', header = T) %>%
  mutate(PM.ETo..mm. = ifelse(is.na(PM.ETo..mm.), ETo..mm.,PM.ETo..mm.)) %>%       # Fill in missing ET values
  dplyr::select(Date, Stn.Name,PM.ETo..mm.) %>%
  rename(Station = Stn.Name, ETo_m = PM.ETo..mm.) %>%
  mutate(Date = as.Date(Date,'%m/%d/%Y'),
         Month = format(Date, '%b'),
         ETo_m = ETo_m/1000)

Buntingville = read.csv('CIMIS_Buntingville_057_2000-2020.csv', header = T) %>%
  mutate(PM.ETo..mm. = ifelse(is.na(PM.ETo..mm.), ETo..mm.,PM.ETo..mm.)) %>%       # Fill in missing ET values
  dplyr::select(Date, Stn.Name,PM.ETo..mm.) %>%
  rename(Station = Stn.Name, ETo_m = PM.ETo..mm.) %>%
  mutate(Date = as.Date(Date,'%m/%d/%Y'),
         Month = format(Date, '%b'),
         ETo_m = ETo_m/1000)


monthly_ET_comp = data.frame(Date = Macdoel$Date, 
                             Madcdoel_ETo = Macdoel$ETo_m,
                             Buntingville_ETo = Buntingville %>% filter(Date>=as.Date('2015-05-01')) %>% dplyr::select(ETo_m) %>% rename(Buntingville_ETo = ETo_m)) %>%
  mutate(Month = format(Date, '%b'),
         Month = factor(Month, levels = month.abb)) %>%
  group_by(Month) %>%
  dplyr::select(-Date) %>%
  summarize_all(.funs = mean) %>%
  arrange(Month) %>%
  mutate(ratio = Madcdoel_ETo/Buntingville_ETo)

Buntingville_adj = Buntingville %>%
  left_join(monthly_ET_comp %>%dplyr::select(Month, ratio), by = 'Month') %>%
  mutate(ETo_m = ETo_m*ratio) %>%
  dplyr::select(-Month,-ratio)


ref_ET = data.frame(Date = Buntingville$Date,
                    ETo_m = c(Buntingville_adj$ETo_m[Buntingville_adj$Date<as.Date('2015-05-01')], Macdoel$ETo_m)) %>%
  mutate(ETo_m = round(ETo_m, digits = 6),
         ETo_m = format(ETo_m, scientific = F))

write.table(ref_ET, file = paste0(outDirSWBM,'ref_et.txt'), quote = F, sep = '  ', row.names = F, col.names = T)

# kc_values.txt ----------------------------------------------------------------
Alfalfa_Kc_timeseries = Kc_timeseries(WYstartDate, WYendDate, Alfalfa_Kc_dates, Alfalfa_Kc_values)
Grain_Kc_timeseries = Kc_timeseries(WYstartDate, WYendDate, Grain_Kc_dates, Grain_Kc_values)
Pasture_Kc_timeseries = Kc_timeseries(WYstartDate, WYendDate, Pasture_Kc_dates, Pasture_Kc_values)
Native_Veg_Kc_timeseries = Kc_timeseries(WYstartDate, WYendDate, Native_Veg_Kc_dates, Native_Veg_Kc_values)
Barren_Urban_Kc_timeseries = data.frame(Date = modelDays, Kc = 0)
Water_Kc_timeseries = data.frame(Date = modelDays, Kc = 1.2)

Kc_all_landuse = Alfalfa_Kc_timeseries %>%
  left_join(Grain_Kc_timeseries, by = 'Date') %>%
  left_join(Pasture_Kc_timeseries, by = 'Date') %>%
  left_join(Native_Veg_Kc_timeseries, by = 'Date') %>%
  left_join(Barren_Urban_Kc_timeseries, by = 'Date') %>%
  left_join(Water_Kc_timeseries, by = 'Date',) %>%
  left_join(Alfalfa_Kc_timeseries, by = 'Date') %>%
  left_join(Grain_Kc_timeseries, by = 'Date') %>%
  left_join(Pasture_Kc_timeseries, by = 'Date')

names(Kc_all_landuse) = c('Date', 'Alfalfa_Kc', 'Grain_Kc', 'Pasture_Kc', 'Native_Veg_Kc', 'Barren_Urban_Kc', 'Water_Kc', 'Alfalfa_Kc', 'Grain_Kc', 'Pasture_Kc')
write.table(x = Kc_all_landuse, file = paste0(outDirSWBM, 'kc_values.txt'), append = F, quote = F, sep = '  ', row.names = F)

# MF_recharge_zones.txt ---------------------------------------------------
fieldsMF = st_drop_geometry(fieldsMF.sf) %>%
  dplyr::select(MF_idx, SWBM_id) 
missingCells = setdiff(seq(1,numRow_MF*numCol_MF), fieldsMF$MF_id)          # find global cell index values outside of model domain
fillCells = data.frame(MF_idx = missingCells,                               # assign cells outside of domain a SWBM_id of 0
                       SWBM_id = rep(0, length(missingCells)))
RechargeZonesMF = rbind(fieldsMF, fillCells) %>%                            
  arrange(MF_idx)
RechargeZonesMF.mat = matrix(data = RechargeZonesMF$SWBM_id, ncol = numCol_MF, byrow = T)    # convert to matrix
write.table(RechargeZonesMF.mat, file = paste0(outDirSWBM,'recharge_zones.txt'), sep = '  ', row.names = F, col.names = F, quote = F)  #write recharge matrix to file

# precip_factors.txt --------------------------------------------------------------
norm_val = 637.05                                                                             # value at weather station cell location used to normalize raster (637.05 for Sierra Valley Station, 361.10 for Vinton Station)
PRISM = raster(paste0(PRISM_data_dir, 'PRISM_ppt_30yr_normal_800mM2_annual_bil.bil'))         # load PRISM raster

mask.sf = basinBoundary.sf %>% 
  st_buffer(dist = 500) %>%
  st_union() %>%
  st_transform(crs = crs(PRISM)) %>%                                  # create mask file for extracting data
  as('Spatial')
raster.masked = PRISM %>%                                                                       # extract data based on mask
  raster::crop(mask.sf) %>%
  raster::mask(mask.sf)                                                                                                                   # set raster mask values to NA
PRISM.sf = as(raster.masked, 'SpatialPolygonsDataFrame') %>%                                    # convert raster to polygons
  st_as_sf() %>%
  st_transform(crs(modelGrid_MF.sf))

Field_precip_factor.sf = fieldsMF_dissolved.sf %>%
  dplyr::select(SWBM_id) %>%
  cbind(ppt_fact=raster::extract(raster.masked, fieldsMF_dissolved.sf) %>%
          lapply(FUN=na.omit) %>%
          lapply(FUN=mean) %>%
          unlist()/norm_val)

Field_precip_factor.sf %>%
  st_drop_geometry() %>%
  arrange(SWBM_id) %>%
  mutate(ppt_fact = round(ppt_fact, 2)) %>%
  write.table(file = paste0(outDirSWBM, 'precip_factors.txt'), append = F, quote = F, sep = '  ', row.names = F, col.names = T)

# SFR_network.txt -------------------------------------------------------------
stream_reaches.sf = st_intersection(stream_segments.sf, modelGrid_MF.sf) %>%                      # Generate reaches for segments
  st_cast(to = 'MULTILINESTRING') %>%                                                             # Convert all reaches to multi-line geometry
  st_cast('LINESTRING') %>%                                                                       # Convert all reaches back to single-line geometry
  mutate(Length_m = st_length(.) %>% drop_units()) %>%                                            # Calculate reach lengths
  filter(Length_m >= 15) %>%                                                                      # Remove reach lengths shorter than threshold value (runtime optimization)
  st_join(points_along_segments.sf %>% dplyr::select(distance), join = st_nearest_feature) %>%    # Find distance along segment for each reach 
  arrange(ISEG, distance) %>%                                                                     # Arrange by segment and distance along segment
  mutate(IREACH = rle(.$ISEG)$length %>% sequence())                                              # Number reaches from upstream (small distance) to downstream (large distance)

reachMidPts.sf = st_as_sf(SpatialLinesMidPoints(sldf= as_Spatial(st_zm(stream_reaches.sf))))  %>% # Find midpoint of stream reaches
  mutate(Bed_Z_m = raster::extract(DEM.raster, .),
         Bed_Z_adj = Bed_Z_m,
         bed_slope = NA)

for(i in 1:nrow(reachMidPts.sf)){
  if (reachMidPts.sf$IREACH[i]==1){
    reachMidPts.sf$Bed_Z_adj[i] = reachMidPts.sf$Bed_Z_adj[i]                  # Set elevations for first reach to DEM value
  }
  else if (reachMidPts.sf$Bed_Z_adj[i] - reachMidPts.sf$Bed_Z_adj[i-1] > 0){   # If downstream reach has higher elevation that upstream, adjust elevation to equal upstream reach
    reachMidPts.sf$Bed_Z_adj[i] = reachMidPts.sf$Bed_Z_adj[i-1]
  }
  else {
    reachMidPts.sf$Bed_Z_adj[i] = reachMidPts.sf$Bed_Z_adj[i]                  # Keep original reach elevation
  }
}

reachInfo_adj = reachMidPts.sf %>%
  st_drop_geometry() %>%
  mutate(up_z = lag(Bed_Z_adj),
         down_z = lead(Bed_Z_adj),
         bed_slope = (lag(Bed_Z_adj) - lead(Bed_Z_adj))/Length_m,    # calculate reach slopes based on upstream and downstream elevations
         bed_slope = case_when(
           bed_slope < 0 & IREACH == 1 ~ lead(bed_slope),            # Correct negative slopes that occur in first reach of segment
           bed_slope < 0 & IREACH != 1 ~ lag(bed_slope),             # Correct negative slopes that occur in last reach of segment
           is.na(bed_slope) ~ 0.1,                                   # correct slope for segment 1 that only has one reach
           TRUE ~ bed_slope),
         bed_slope = if_else(bed_slope < 1E-4, 1E-5, bed_slope)) %>%
  dplyr::select(ISEG, IREACH, Bed_Z_adj, bed_slope)

SFR_reaches.shp  = stream_reaches.sf %>%
  left_join(reachInfo_adj) %>%                       # join adjusted reach info to shapefile 
  relocate(row_MF, col_MF ,ISEG, IREACH, Length_m, Bed_Z_adj, bed_slope, Name, OUTSEG, IUPSEG)
st_write(SFR_reaches.shp, dsn = paste0(outdirGIS,'Surface Water/SFR_reaches.shp'), delete_dsn = T)


NSTRM = nrow(SFR_reaches.shp)
NSS = max(SFR_reaches.shp$ISEG)

SFR_network = SFR_reaches.shp %>%
  st_drop_geometry() %>%
  mutate(lyr_MF = '1',
         STRTHICK = 1,
         STRHC1 = case_when(
           Bed_K_cat == 'Low' ~ 0.01,
           Bed_K_cat == 'Med' ~ 0.5,
           Bed_K_cat == 'High' ~ 10),
         Length_m = round(Length_m, digits = 2),
         Bed_Z_adj = round(Bed_Z_adj, digits = 2),
         bed_slope = formatC(bed_slope, format = 'E', digits = 2)) %>%
  dplyr::select(lyr_MF, row_MF, col_MF ,ISEG, IREACH, Length_m, Bed_Z_adj, bed_slope, STRTHICK, STRHC1)

# write output to text file  
cat('# MODFLOW Streamflow Routing (SFR) Package', file = paste0(outDirSWBM, 'SFR_network.txt'), append = F, sep = '\n')
cat(paste('# Data set created', Sys.Date(),'using MODFLOW Pre-Processing.R'), file = paste0(outDirSWBM, 'SFR_network.txt'), append = T, sep = '\n')
cat('#', file = paste0(outDirSWBM, 'SFR_network.txt'), append = T, sep = '\n')
cat('BEGIN OPTION', file = paste0(outDirSWBM, 'SFR_network.txt'), append = T, sep = '\n')
cat('  DBFILE SFR_out.dat', file = paste0(outDirSWBM, 'SFR_network.txt'), append = T, sep = '\n')
cat('END', file = paste0(outDirSWBM, 'SFR_network.txt'), append = T, sep = '\n')
cat(paste(-1*NSTRM, NSS, '0  0  ', CONST, DLEAK, ISTCB1, ISTCB2,'1        ! NSTRM NSS NSFRPAR NPARSEG CONST DLEAK ISTCB1  ISTCB2 ISFROPT', sep = '  '), 
    file = paste0(outDirSWBM, 'SFR_network.txt'), append = T, sep = '\n')
write.table(SFR_network, file = paste0(outDirSWBM, 'SFR_network.txt'), sep = '  ', append = T, row.names = F, col.names = F, quote = F, eol = '\n')

# SFR_network.jtf ---------------------------------------------------------
SFR_network_jtf = SFR_reaches.shp %>%
  st_drop_geometry() %>%
  mutate(lyr_MF = '1',
         STRTHICK = 1,
         STRHC1 = case_when(
           Bed_K_cat == 'Low' ~ '@BedK_3         @',
           Bed_K_cat == 'Med' ~ '@BedK_2         @',
           Bed_K_cat == 'High' ~ '@BedK_1         @'),
         Length_m = round(Length_m, digits = 2),
         Bed_Z_adj = round(Bed_Z_adj, digits = 2),
         bed_slope = formatC(bed_slope, format = 'E', digits = 2)) %>%
  dplyr::select(lyr_MF, row_MF, col_MF ,ISEG, IREACH, Length_m, Bed_Z_adj, bed_slope, STRTHICK, STRHC1)


# write output to text file  
cat('# MODFLOW Streamflow Routing (SFR) Package', file = paste0(outDirSWBM, 'SFR_network_jtf.txt'), append = F, sep = '\n')
cat(paste('# Data set created', Sys.Date(),'using MODFLOW Pre-Processing.R'), file = paste0(outDirSWBM, 'SFR_network_jtf.txt'), append = T, sep = '\n')
cat('#', file = paste0(outDirSWBM, 'SFR_network_jtf.txt'), append = T, sep = '\n')
cat('BEGIN OPTION', file = paste0(outDirSWBM, 'SFR_network_jtf.txt'), append = T, sep = '\n')
cat('  DBFILE SFR_out.dat', file = paste0(outDirSWBM, 'SFR_network_jtf.txt'), append = T, sep = '\n')
cat('END', file = paste0(outDirSWBM, 'SFR_network_jtf.txt'), append = T, sep = '\n')
cat(paste(-1*NSTRM, NSS, '0  0  ', CONST, DLEAK, ISTCB1, ISTCB2,'1        ! NSTRM NSS NSFRPAR NPARSEG CONST DLEAK ISTCB1  ISTCB2 ISFROPT', sep = '  '), 
    file = paste0(outDirSWBM, 'SFR_network_jtf.txt'), append = T, sep = '\n')
write.table(SFR_network_jtf, file = paste0(outDirSWBM, 'SFR_network_jtf.txt'), sep = '  ', append = T, row.names = F, col.names = F, quote = F, eol = '\n')

# SFR_routing.txt ---------------------------------------------------------
SFR_routing = SFR_reaches.shp %>%
  st_drop_geometry() %>%
  rename(NSEG = ISEG) %>%
  group_by(NSEG) %>%
  slice_head() %>%
  mutate(ICALC = 1,
         IPRIOR = if_else(IUPSEG == 0, 0, -2),
         WIDTH1 = as.numeric(WIDTH) %>% format(nsmall = 1),
         WIDTH2 = as.numeric(WIDTH) %>% format(nsmall = 1),
         MANNING_N = 0.035,
         Bed_K_Param = case_when(
           Bed_K_cat == 'Low' ~ 'BedK_3',
           Bed_K_cat == 'Med' ~ 'BedK_2',
           Bed_K_cat == 'High' ~ 'BedK_1'),
         Manning_n_Param = case_when(
             Bed_K_cat == 'Low' ~ 'Manning_n_3',
             Bed_K_cat == 'Med' ~ 'Manning_n_2',
             Bed_K_cat == 'High' ~ 'Manning_n_1')) %>%
  dplyr::select(NSEG, ICALC, OUTSEG, IUPSEG, IPRIOR, WIDTH1, WIDTH2, MANNING_N, Bed_K_Param, Manning_n_Param)
write.table(SFR_routing, file = paste0(outDirSWBM, 'SFR_routing.txt'), quote = F, sep = '  ', row.names = F)

# SFR_diversions.txt ---------------------------------------------------
SFR_diversions = stream_segments.sf %>%
  filter(IUPSEG>0)

cat(paste(nrow(SFR_diversions), '    ! Number of diversions'), file = paste0(outDirSWBM, 'SFR_diversions.txt'), sep = '\n', append = F)
cat(paste(paste(SFR_diversions$ISEG, collapse = '  '), '    ! ISEG for diversions', collapse = '  '), file = paste0(outDirSWBM, 'SFR_diversions.txt'), sep = '\n', append = T)
cat(paste(c(rep(-2, nrow(SFR_diversions)), '    ! IPRIOR for diversions'), collapse = '  '), file = paste0(outDirSWBM, 'SFR_diversions.txt'), sep = '\n', append = T)
cat(paste(c(rep(0.5, nrow(SFR_diversions)), '    ! FLOW for diversions'), collapse = '  '), file = paste0(outDirSWBM, 'SFR_diversions.txt'), sep = '\n', append = T)

#code below is for specfying transient FLOW values (not currently implemented in SWBM)
# write.table(x = data.frame(d1 = rep(0.5, 12) %>% rep(nmonths),
#                            d2 = rep(0.5, 12) %>% rep(nmonths),
#                            d3 = rep(0.5, 12) %>% rep(nmonths),
#                            d4 = rep(0.5, 12) %>% rep(nmonths),
#                            d5 = rep(0.5, 12) %>% rep(nmonths),
#                            d6 = '# FLOW'), 
#             file = paste0(outDirSWBM, 'SFR_diversions.txt'), append = T, sep = '  ', row.names = F, col.names = F, quote = F)


# subwatershed_irrigation_inflows.txt and subwatershed_non-irrigation_inflows.txt (PRMS and Reservoir Data)  ----------------------------------------------------
# Little Truckee River Imported Water
LTR_Imports = read.xlsx('Little_Truckee_Diversion_1959_2020.xlsx', header = T, sheetIndex = 1) %>%   # Imported water from Little Truckee River
  dplyr::select(-Date) %>%
  rename(Date = Date_2) %>%
  mutate(Date = as.Date(Date, '%Y-%m-%d')) %>%
  filter(Date >= WYstartDate, Date < WYendDate) %>%
  mutate(Month = format(Date, '%b-%Y')) %>%
  group_by(Month) %>% 
  summarize(Monthly_Volume_m3 = sum(AF)/0.000810714) %>%
  mutate(Date = as.Date(paste0('01-',Month),'%d-%b-%Y')) %>%
  arrange(Date)

# Reservoir Release Data
Frenchman_daily = read.xlsx('Frenchman_2000-2020.xlsx', 'Releases')
Davis_daily = read.xlsx('Davis_2000-2020.xlsx', 'Releases')

Frenchman_monthly = Frenchman_daily %>% 
  mutate(Month = format(Date,'%b-%Y')) %>%
  group_by(Month) %>%
  summarize(Irrigation_Release_TAF = sum(Contract_Release_cfs + Water_Right_Release_cfs, na.rm = T)*1.983/1000,
            NonIrrigation_Release_TAF = sum(Environmental_Release_cfs+Spill_cfs, na.rm = T)*1.983/1000) %>%
  mutate(Date = as.Date(paste0('01-',Month),'%d-%b-%Y')) %>%
  arrange(Date) %>%
  right_join(data.frame(Date = seq.Date(from = WYstartDate, to = WYendDate, by = 'month')), by = 'Date') %>%
  arrange(Date) %>%
  mutate(Month = format(Date, '%b-%Y'),
         WY = if_else(format(Date,'%b')%in%month.abb[1:9],      # populate water year
                      format(Date, '%Y') %>% as.numeric(),
                      format(Date, '%Y') %>% as.numeric()+1),
         WYtype = case_when(
           WY%in%c(2001,2002,2007,2008,2009,2013,2014,2015,2020) ~ 'Dry',
           WY%in%c(2006,2011,2017,2019) ~ 'Wet',
           TRUE ~ 'Average')) %>%
  dplyr::select(-Date)

Average_Frenchman_releases_TAF = Frenchman_monthly %>%
  filter(WY>=2001) %>%
  mutate(Month = strtrim(Month,3)) %>%
  group_by(Month, WYtype) %>%
  summarize(mean_irr_release_TAF = mean(Irrigation_Release_TAF, na.rm = T),
            mean_nonirr_release_TAF = mean(NonIrrigation_Release_TAF, na.rm=T)) %>%
  mutate(Month = factor(Month, levels = c(month.abb[10:12], month.abb[1:9]))) %>%
  arrange(WYtype, Month)

Davis_monthly = Davis_daily %>% 
  mutate(Month = format(Date,'%b-%Y')) %>%
  group_by(Month) %>%
  summarize(Irrigation_Release_TAF = sum(Contract_Release_cfs + Water_Right_Release_cfs, na.rm = T)*1.983/1000,
            NonIrrigation_Release_TAF = sum(Environmental_Release_cfs+Spill_cfs, na.rm = T)*1.983/1000) %>%
  mutate(Date = as.Date(paste0('01-',Month),'%d-%b-%Y')) %>%
  arrange(Date) %>%
  right_join(data.frame(Date = seq.Date(from = WYstartDate, to = WYendDate, by = 'month')), by = 'Date') %>%
  arrange(Date) %>%
  mutate(Month = format(Date, '%b-%Y'),
         WY = if_else(format(Date,'%b')%in%month.abb[1:9],      # populate water year
                      format(Date, '%Y') %>% as.numeric(),
                      format(Date, '%Y') %>% as.numeric()+1),
         WYtype = case_when(
           WY%in%c(2001,2002,2007,2008,2009,2013,2014,2015,2020) ~ 'Dry',
           WY%in%c(2006,2011,2017,2019) ~ 'Wet',
           TRUE ~ 'Average')) %>%
  dplyr::select(-Date)

Average_Davis_releases_TAF = Davis_monthly %>%
  filter(WY>=2007) %>%
  mutate(Month = strtrim(Month,3)) %>%
  group_by(Month, WYtype) %>%
  summarize(mean_irr_release_TAF = mean(Irrigation_Release_TAF, na.rm = T),
            mean_nonirr_release_TAF = mean(NonIrrigation_Release_TAF, na.rm=T)) %>%
  mutate(Month = factor(Month, levels = c(month.abb[10:12], month.abb[1:9]))) %>%
  arrange(WYtype, Month)

# Fill in months with missing data.
Frenchman_monthly$Irrigation_Release_TAF[1:3] = Average_Frenchman_releases_TAF$mean_irr_release_TAF[1:3]
Frenchman_monthly$NonIrrigation_Release_TAF[1:3] = Average_Frenchman_releases_TAF$mean_nonirr_release_TAF[1:3]
Davis_monthly$Irrigation_Release_TAF[1:3] = Average_Davis_releases_TAF$mean_irr_release_TAF[1:3]
Davis_monthly$NonIrrigation_Release_TAF[1:3] = Average_Davis_releases_TAF$mean_nonirr_release_TAF[1:3]

###
###
# Code below is for estimating missing reservoir release data based on water year type  

# Davis_monthly$Irrigation_Release_TAF[Davis_monthly$WY%in%c(2000,2003,2004,2005)] = rep(Average_Davis_releases_TAF$mean_irr_release_TAF[Average_Davis_releases_TAF$WYtype=='Average'],4)
# Davis_monthly$Irrigation_Release_TAF[Davis_monthly$WY%in%c(2001,2002)] = rep(Average_Davis_releases_TAF$mean_irr_release_TAF[Average_Davis_releases_TAF$WYtype=='Dry'],2)
# Davis_monthly$Irrigation_Release_TAF[Davis_monthly$WY==2006] = Average_Davis_releases_TAF$mean_irr_release_TAF[Average_Davis_releases_TAF$WYtype=='Wet']
# Davis_monthly$Irrigation_Release_TAF[Davis_monthly$Month%in%c('Oct-2006', 'Nov-2006','Dec-2006')] =   Average_Davis_releases_TAF$mean_irr_release_TAF[Average_Davis_releases_TAF$WYtype=='Dry'][1:3] 
# 
# Davis_monthly$NonIrrigation_Release_TAF[Davis_monthly$WY%in%c(2000,2003,2004,2005)] = rep(Average_Davis_releases_TAF$mean_nonirr_release_TAF[Average_Davis_releases_TAF$WYtype=='Average'],4)
# Davis_monthly$NonIrrigation_Release_TAF[Davis_monthly$WY%in%c(2001,2002)] = rep(Average_Davis_releases_TAF$mean_nonirr_release_TAF[Average_Davis_releases_TAF$WYtype=='Dry'],2)
# Davis_monthly$NonIrrigation_Release_TAF[Davis_monthly$WY==2006] = Average_Davis_releases_TAF$mean_nonirr_release_TAF[Average_Davis_releases_TAF$WYtype=='Wet']
# Davis_monthly$NonIrrigation_Release_TAF[Davis_monthly$Month%in%c('Oct-2006', 'Nov-2006','Dec-2006')] =   Average_Davis_releases_TAF$mean_nonirr_release_TAF[Average_Davis_releases_TAF$WYtype=='Dry'][1:3] 

###
###

prms_gages = prms_subbasins.sf %>%
  st_drop_geometry() %>%
  filter(TYPE == 'SUBBASIN') %>%
  arrange(ZONE_VALUE)

PRMS_inflows_m3 = PRMS_monthly_vol(statvar_file = statvar_file,          # extract daily flow data from PRMS output and convert to monthly colume
                                   param_type = 'sub_cfs', 
                                   subbasin_id = prms_gages$ZONE_VALUE, 
                                   subbasin_name = prms_gages$desc, 
                                   startDate = WYstartDate,
                                   endDate = WYendDate) %>%
  mutate(Monthly_Volume_m3 = Monthly_Volume_ft3/35.3147) %>%          # convert monthly volume from ft^3 to m^3
  dplyr::select(-Monthly_Volume_ft3) %>%
  pivot_wider(names_from = Stream, values_from = Monthly_Volume_m3)
  

inflow_is_vol = TRUE

SWBM_subwatersheds = fields.sf %>%
  st_drop_geometry() %>%
  dplyr::select(subws_ID, subws_Name) %>%
  unique() %>%
  arrange(subws_ID)

# Subwatershed contributing streams below:
# Cold Stream:  Cold Stream + Bonta Creek
# Turner Creek: Turner Creek + Berry Creek
# Carman Creek: East Fork Carman Creek + West Fork Carman Creek
# Bear Valley Creek-Smithneck Creek: Bear Valley Creek + Smithneck Creek

subws_irrigation_vol = PRMS_inflows_m3 %>%
  mutate(modelMonth = format(modelMonth, '%b-%Y'),
         `Cold Stream` = `Cold Stream` + `Bonta Creek` + `Antelope Creek` + `Berry Creek` +  LTR_Imports$Monthly_Volume_m3,
         `Carman Creek` = `West Fork Carman Creek` + `East Fork Carman Creek`,
         `Bear Valley Creek-Smithneck Creek` = `Bear Valley Creek` + `Smithneck Creek` + `Badenough Creek` + `Staverville Creek`,
         `Big Grizzly Creek` = Davis_monthly$Irrigation_Release_TAF/0.000000810714,
         `Little Last Chance Creek` = Frenchman_monthly$Irrigation_Release_TAF/0.000000810714,
         `Correco Canyon` = 0) %>%
  dplyr::select(modelMonth, `Cold Stream`, `Hamlin Creek`, `Turner Creek`, `Fletcher Creek`, `Carman Creek`, `Big Grizzly Creek`, `Mapes Creek`,
                `Little Last Chance Creek`, `Correco Canyon`, `Bear Valley Creek-Smithneck Creek`, `Lemon Creek`)

subws_nonirrigation_vol = subws_irrigation_vol %>%    # use irrigation volume data frame so subwatershed ordering stays consistent 
  mutate(`Cold Stream` = 0, 
         `Hamlin Creek` = 0,
         `Turner Creek` = 0,                                                                          
         `Fletcher Creek` = 0,
         `Carman Creek` = 0,
         `Big Grizzly Creek` = Davis_monthly$NonIrrigation_Release_TAF/0.000000810714,
         `Mapes Creek` = 0,                                                                      
         `Little Last Chance Creek` = Frenchman_monthly$NonIrrigation_Release_TAF/0.000000810714,
         `Correco Canyon` = 0,                                                                                                   # No gages and no SW irrigation
         `Bear Valley Creek-Smithneck Creek` = 0,
         `Lemon Creek` = 0)
  
nSubws = ncol(subws_irrigation_vol[,-1])
write.table(x = subws_irrigation_vol, file = paste0(outDirSWBM, 'subwatershed_irrigation_inflows.txt'), na = '0', append = F, quote = F, sep = '  ', row.names = F, col.names = T, eol = '\n')
write.table(x = subws_nonirrigation_vol, file = paste0(outDirSWBM, 'subwatershed_nonirrigation_inflows.txt'), na = '0', append = F, quote = F, sep = '  ', row.names = F, col.names = T, eol = '\n')


# SFR_inflow_segments.txt and SFR_flow_partitioning.txt -------------------------------------------------------------
SFR_Inflow_segs = stream_segments.sf %>%
  st_join(fields.sf %>% dplyr::select(subws_Name,subws_ID), join = st_join, largest = T) %>%
  st_drop_geometry() %>%
  filter(Inflow_BC==T) %>%
  mutate(subws_ID = if_else(Name=='Berry Creek', as.integer(1),subws_ID),                    # assign Berry Creek to Cold Stream subwatershed
         subws_Name = if_else(Name=='Berry Creek', 'Cold Stream',subws_Name),
         subws_ID = if_else(Name=='Turner Creek', as.integer(3),subws_ID),                    # assign Turner Creek to Turner creek subwatershed
         subws_Name = if_else(Name=='Turner Creek', 'Turner Creek',subws_Name)) %>%
  arrange(subws_ID, ISEG) %>%
  group_by(subws_Name) %>%
  dplyr::select(subws_ID, ISEG, subws_Name , Name) %>%
  rename(stream_name = Name, subws_name = subws_Name)

write.table(SFR_Inflow_segs,file = paste0(outDirSWBM, 'SFR_inflow_segments.txt'), append = F, sep = '  ', row.names = F, quote = F)

nSFR_inflow_segs = nrow(SFR_Inflow_segs)

for(i in 1:nrow(SFR_Inflow_segs)){
temp = (PRMS_inflows_m3 %>% dplyr::select(SFR_Inflow_segs$stream_name[i])) / (subws_irrigation_vol %>% dplyr::select(SFR_Inflow_segs$subws_name[i]))
temp[temp==Inf]=0
if(i==1){
  flow_frac = cbind(subws_irrigation_vol$modelMonth, temp)
} else {
  flow_frac = cbind(flow_frac, temp)
}
}
flow_frac[,'Big Grizzly Creek'] = 1              # Regulated inflows 
flow_frac[,'Little Last Chance Creek'] = 1       # Regulated inflows
flow_frac[,-1] = round(flow_frac[,-1], 5)

write.table(flow_frac,file = paste0(outDirSWBM, 'SFR_subws_flow_partitioning.txt'), append = F, sep = '  ', row.names = F, quote = F)

# muni_well_pumping_rates.txt and muni_wells.txt ---------------------------------------------------
muniPumpingObs = read.xlsx2('Pumping_data_municipal_ready_for_DB_upload.xlsx', header = T, sheetIndex = 1) %>%
  mutate(startDate = as.Date(startDate), 
         endDate = as.Date(endDate),
         miUsageAF = as.numeric(miUsageAF),
         well_id = as.integer(well_id),
         comment= 'Measured') %>% 
  left_join(sqlFetch(DB, 'wells') %>% dplyr::select(well_id, well_name, well_name_2), by = 'well_id') %>%
  arrange(startDate, well_id) %>%
  dplyr::select(well_id, well_name, well_name_2, startDate, miUsageAF, comment)

AverageMuniPumping = muniPumpingObs %>%
  mutate(WY = if_else(format(startDate,'%b')%in%month.abb[10:12], format(startDate,'%Y') %>% as.numeric() + 1, format(startDate,'%Y') %>% as.numeric())) %>%
  group_by(WY) %>%
  summarize(sum = sum(miUsageAF, na.rm = T)) %>%
  summarize(average = mean(sum)) %>% 
  unlist()/length(unique(muniPumpingObs$well_id))


muniPumping = expand.grid(unique(muniPumpingObs$well_id), modelMonths) %>%
  rename(well_id = Var1, startDate = Var2) %>%
  mutate(startDate = as.Date(startDate),
         month = format(startDate, '%b')) %>%
  left_join(muniPumpingObs %>% dplyr::select(well_id, well_name, startDate, miUsageAF, comment), by =c('well_id', 'startDate')) %>%
  arrange(well_id, startDate) %>%
  mutate(pumping_m3 = miUsageAF/0.000810714,
         startDate = format(startDate , '%b%Y')) %>%
  dplyr::select(well_id, startDate, pumping_m3)  %>%
  pivot_wider(names_from = well_id, values_from = pumping_m3) %>%
  rename(well_id = startDate)

# code below is for hard-wiring missing municipal pumping data specific to Sierra Valley (Use 2005 data for missing years)
muniPumping[1:63,-1] = muniPumping[73:135,-1]

write.table(muniPumping, paste0(outDirSWBM, 'muni_well_specified_volume.txt'), append = F, quote = F, row.names = F, na = '0', sep = '  ')

muniWells.sf = sqlFetch(DB, 'wells') %>%
  filter(well_id%in%unique(muniPumpingObs$well_id)) %>%   # Filter municipal wells, exclude calpine wells as they are located outside of groundwater basin and screened in bedrock
  dplyr::select(well_id, well_name, well_name_2, entity, well_type, well_status, well_notes, well_lat, well_long, 
                well_total_depth_ft, top_of_screen_depth_ft, bottom_of_screen_depth_ft, other_screen_intervals,
                mp_elevation, elev_ground_surface, casing_diameter_inches, monitoring_point_data_source) %>%
  rename(status = well_status, notes = well_notes, ttlDpth_ft = well_total_depth_ft, TOS_ftbgs = top_of_screen_depth_ft, 
         BOS_ftbgs = bottom_of_screen_depth_ft, perf_ints = other_screen_intervals, RPE_ft = mp_elevation, 
         GSE_ft = elev_ground_surface, diam_in = casing_diameter_inches, source = monitoring_point_data_source) %>%
  st_as_sf(coords = c('well_long', 'well_lat'), crs = 4326) %>%
  st_transform(crs=26910) 
st_write(obj = muniWells.sf, dsn = paste0(outdirGIS, 'Wells/muniWells.shp'), delete_dsn = T)

muniWellAttTbl = st_intersection(modelGrid_MF.sf, muniWells.sf) %>%
  st_drop_geometry() %>%
  mutate(well_name = gsub(x = well_name, pattern = '\\(|\\)', replacement = ''),
         well_name = gsub(x = well_name, pattern = '&', replacement = 'and'),
         top_scrn_z = (GSE_ft - TOS_ftbgs)/3.28084,   # Convert ft to m
         bot_scrn_z = (GSE_ft - BOS_ftbgs)/3.28084,   # Convert ft to m
         UTM_E = st_coordinates(muniWells.sf)[,1],
         UTM_N = st_coordinates(muniWells.sf)[,2],
         well_name = gsub(x = well_name, pattern = ' ', replacement = '_'),
         top_scrn_z = if_else(is.na(top_scrn_z), (GSE_ft - 100)/3.28084, top_scrn_z),     # If top of screen info is unavailable, assume screen starts 100 ft bgs
         bot_scrn_z = case_when(
           is.na(bot_scrn_z) & !is.na(ttlDpth_ft) ~ (GSE_ft - ttlDpth_ft)/3.28084,        # If bottom of screen info is unavailable, assume well is screened to total depth
           is.na(bot_scrn_z) & is.na(ttlDpth_ft) ~ (GSE_ft - 800)/3.28084,                # If bottom of screen info and total depth are unavailable, assume well is 400 ft
           TRUE ~ bot_scrn_z),
         top_scrn_z = round(top_scrn_z, digits = 2),
         bot_scrn_z = round(bot_scrn_z, digits = 2),
         UTM_E = round(UTM_E, digits = 2),
         UTM_N = round(UTM_N, digits = 2)) %>%
  dplyr::select(well_id, well_name, top_scrn_z,bot_scrn_z,row_MF,col_MF,UTM_E, UTM_N )

write.table(muniWellAttTbl, file = paste0(outDirSWBM, 'muni_well_summary.txt'), sep = '  ', quote = F, row.names = F)   

nMuniWells = nrow(muniWellAttTbl)

# ag_well_pumping_rates.txt,  ag_well_rates.txt, ag_well_list_by_polygon.txt, and  ag_well_summary.txt---------------------------------------------------

# Ag wells only have annual values provided, so they are distributed to monthly values
agPumpingObs = read.xlsx2('Pumping_data_ag_ready_for_DB_upload.xlsx', header = T, sheetIndex = 1) %>%
  mutate(startDate = as.Date(startDate), 
         endDate = as.Date(endDate),
         AgUsageAF = as.numeric(AgUsageAF),
         well_id = as.integer(well_id),
         year = format(startDate, '%Y') %>% as.integer()) %>%
  dplyr::select(well_id, year,AgUsageAF,notes)

agWells.sf = sqlFetch(DB, 'wells') %>%                                                                                   # create shapefile of Ag wells
  filter(well_type == 'Agricultural Well' | well_id%in%fields.sf$well_id | well_id%in%agPumpingObs$well_id) %>%
  dplyr::select(well_id, well_name, entity, well_type, well_status, well_notes, well_lat, well_long, 
                well_total_depth_ft, top_of_screen_depth_ft, bottom_of_screen_depth_ft, other_screen_intervals,
                mp_elevation, elev_ground_surface, casing_diameter_inches, monitoring_point_data_source) %>%
  rename(status = well_status, notes = well_notes, ttlDpth_ft = well_total_depth_ft, TOS_ftbgs = top_of_screen_depth_ft, 
         BOS_ftbgs = bottom_of_screen_depth_ft, perf_ints = other_screen_intervals, RPE_ft = mp_elevation, 
         GSE_ft = elev_ground_surface, diam_in = casing_diameter_inches, source = monitoring_point_data_source) %>%
  st_as_sf(coords = c('well_long', 'well_lat'), crs = 4326) %>%
  st_transform(crs=26910) 

# Determine wells assigned to polygons
wellsByPolygon  = data.frame(SWBM_id = fieldsMF_dissolved.sf$SWBM_id) %>%                                          
  left_join(fields.sf %>%
              st_drop_geometry() %>% 
              dplyr::select(SWBM_id,well_id), by = 'SWBM_id') %>%                      
  arrange(SWBM_id) %>%
  mutate(well_id = as.integer(well_id),                                                # Assign wells to fields (wells are known to irrigate the field)
         well_id = if_else(is.na(well_id),                                             # If no well is known to be associated with a field, assign nearest Ag well                                             
                           agWells.sf$well_id[st_nearest_feature(fieldsMF_dissolved.sf, agWells.sf)],             
                           well_id))

# Determine if GW is ever used on a polygon
GW_used = data.frame(SWBM_id = polygons_table$SWBM_id,
                     Landcover_id = apply(X = Landcover, MARGIN = 2, FUN = min)) %>%   # Set Landcover_id to smallest value in table (Irrigated landcover types appear first)        
  left_join(landcover_table %>% dplyr::select(Landcover_id, IrrFlag), by = 'Landcover_id')

# Remove wells that are never used
activeWells = wellsByPolygon %>%
  left_join(GW_used, by = 'SWBM_id') %>%
  left_join(polygons_table, by = 'SWBM_id') %>%
  mutate(well_id = if_else(IrrFlag & WATERSOURC != 1, well_id, NULL))                       # Remove wells associated with non-irrigated and surface-water only irrigated fields

agWellIDs = data.frame(well_id = c(agPumpingObs$well_id, na.omit(activeWells$well_id))) %>% # Combine wells with known pumping rates and wells assigned to fields
  distinct() %>%
  filter(well_id %!in%c(243, 1534)) %>%                                                     # remove DMS 76 and DMS 77 (CP west of Loyalton uses WWTP effluent for irrigation)
  arrange(well_id)

# Create attribute table for wells
agWellAttTbl = st_intersection(modelGrid_MF.sf, agWells.sf) %>%
  st_drop_geometry() %>%
  filter(well_id%in%agWellIDs$well_id) %>%
  mutate(well_name = gsub(x = well_name, pattern = '\\(|\\)', replacement = ''),
         well_name = gsub(x = well_name, pattern = '&', replacement = 'and'),
         top_scrn_z = (GSE_ft - TOS_ftbgs)/3.28084,   # Convert ft to m
         bot_scrn_z = (GSE_ft - BOS_ftbgs)/3.28084,   # Convert ft to m
         UTM_E = st_coordinates(agWells.sf %>% filter(well_id%in%agWellIDs$well_id))[,1],
         UTM_N = st_coordinates(agWells.sf %>% filter(well_id%in%agWellIDs$well_id))[,2],
         well_name = gsub(x = well_name, pattern = ' ', replacement = '_'),
         well_name = gsub(x = well_name, pattern = '(|)', replacement = ''),
         top_scrn_z = if_else(is.na(top_scrn_z), (GSE_ft - 10)/3.28084, top_scrn_z),     # If top of screen info is unavailable, assume screen starts 10 ft bgs
         bot_scrn_z = case_when(
           is.na(bot_scrn_z) & !is.na(ttlDpth_ft) ~ (GSE_ft - ttlDpth_ft)/3.28084,        # If bottom of screen info is unavailable, assume well is screened to total depth
           is.na(bot_scrn_z) & is.na(ttlDpth_ft) ~ (GSE_ft - 800)/3.28084,                # If bottom of screen info and total depth are unavailable, assume well is 800 ft
           TRUE ~ bot_scrn_z),
         top_scrn_z = round(top_scrn_z, digits = 2),
         bot_scrn_z = round(bot_scrn_z, digits = 2),
         UTM_E = round(UTM_E, digits = 2),
         UTM_N = round(UTM_N, digits = 2)) %>%
  dplyr::select(well_id, well_name, top_scrn_z,bot_scrn_z,row_MF,col_MF,UTM_E, UTM_N )

nAgWells = nrow(agWellAttTbl)

write.table(wellsByPolygon, file = paste0(outDirSWBM, 'ag_well_list_by_polygon.txt'), sep = '  ', quote = F, row.names = F,)   
write.table(agWellAttTbl, file = paste0(outDirSWBM, 'ag_well_summary.txt'), sep = '  ', quote = F, row.names = F)        

# Distribute annual Ag pumping 
ref_ET_monthly = ref_ET %>%
  mutate(monthYear = format(Date, '%b-%Y')) %>%
  group_by(monthYear) %>%
  summarize(ETo_m := sum(as.numeric(ETo_m))) %>%
  mutate(month = as.Date(paste0('01-',monthYear),'%d-%b-%Y') %>% format('%b') %>% factor(levels = month.abb),
         year = as.Date(paste0('01-',monthYear),'%d-%b-%Y') %>% format('%Y') %>% as.integer()) %>%
  arrange(month) %>%
  pivot_wider(id_cols = year, names_from = month, values_from = ETo_m)

Alfalfa_ET_fraction = ref_ET_monthly %>%
  mutate(Jan = 0,
         Feb = 0,
         Sep = 0,
         Oct = 0,
         Nov = 0,
         Dec = 0) %>%
  rowwise() %>%
  mutate(irrigation_season_total = sum(Mar,Apr,May,Jun,Jul,Aug)) %>%
  ungroup() %>%
  mutate(Mar = Mar/irrigation_season_total,
         Apr = Apr/irrigation_season_total,
         May = May/irrigation_season_total,
         Jun = Jun/irrigation_season_total,
         Jul = Jul/irrigation_season_total,
         Aug = Aug/irrigation_season_total) %>%
  dplyr::select(-irrigation_season_total) %>%
  pivot_longer(!year, names_to = 'month', values_to = 'ET_frac_alfalfa') %>%
  mutate(month = factor(month, levels = month.abb)) %>%
  arrange(year, month)

Pasture_ET_fraction = ref_ET_monthly %>%
  mutate(Jan = 0,
         Feb = 0,
         Nov = 0,
         Dec = 0) %>%
  rowwise() %>%
  mutate(irrigation_season_total = sum(Mar,Apr,May,Jun,Jul,Aug,Sep,Oct)) %>%
  ungroup() %>%
  mutate(Mar = Mar/irrigation_season_total,
         Apr = Apr/irrigation_season_total,
         May = May/irrigation_season_total,
         Jun = Jun/irrigation_season_total,
         Jul = Jul/irrigation_season_total,
         Aug = Aug/irrigation_season_total,
         Sep = Sep/irrigation_season_total,
         Oct = Oct/irrigation_season_total) %>%
  dplyr::select(-irrigation_season_total) %>%
  pivot_longer(!year, names_to = 'month', values_to = 'ET_frac_pasture') %>%
  mutate(month = factor(month, levels = month.abb)) %>%
  arrange(year, month)

ET_fractions = Alfalfa_ET_fraction %>%
  left_join(Pasture_ET_fraction, by = c('year', 'month')) %>%
  arrange(year,month)

# Determine dominant landcover type for fields associated with each well for each year
well_landcover = wellsByPolygon %>% 
  filter(well_id%in%agWellAttTbl$well_id) %>%
  mutate(Landcover_id = Landcover[1,.$SWBM_id],
         Landcover_id = case_when(
           Landcover_id%in%c(1,2,7,8) ~ as.integer(1),
           Landcover_id%in%c(3,9) ~ as.integer(3),
           TRUE ~ as.integer(Landcover_id))) %>%
  left_join(polygons_table %>% dplyr::select(SWBM_id, MF_Area_m2), by = 'SWBM_id') %>%
  group_by(well_id, Landcover_id) %>%
  summarize(total_area = sum(as.numeric(MF_Area_m2))) %>%
  group_by(well_id) %>%
  arrange(well_id, desc(total_area)) %>%
  slice_head() %>%
  dplyr::select(well_id, Landcover_id)

# Pumping timeseries
agSpecifiedPumping = expand.grid(agWellIDs$well_id, modelMonths) %>%                          # create table of permutations for agWells and stress periods
  rename(well_id = Var1, month = Var2) %>%                                       
  mutate(year = format(month, '%Y') %>% as.integer(),
         month = format(month, '%b')) %>%
  left_join(sqlFetch(DB, 'wells') %>% dplyr::select(well_id, well_name), by = 'well_id') %>%
  relocate(well_id, well_name) %>%
  left_join(agPumpingObs, by = c('well_id', 'year')) %>%
  arrange(well_id) %>%
  left_join(ET_fractions, by = c('month', 'year')) %>%
  left_join(well_landcover, by = 'well_id') %>%
  mutate(AgUsageAF = if_else(year >= 2003 & is.na(AgUsageAF), 0, AgUsageAF),     # only pumping data for WY2000-2002 is unavailable
         pumping_rate_m3day = case_when(
           Landcover_id == 3 ~ AgUsageAF*ET_frac_pasture/0.000810714,                # calculate monthly volume using ET fraction and convert to m^3
           TRUE ~ AgUsageAF*ET_frac_alfalfa/0.000810714), 
         ag_pumping_specified = if_else(is.na(pumping_rate_m3day), FALSE, TRUE))

read_pumping_rate = agSpecifiedPumping %>%
  mutate(monthYear = paste0(month,year)) %>%
  group_by(monthYear) %>%
  summarize(specify_pumping = any(ag_pumping_specified)) %>%
  mutate(monthYear = as.Date(paste0('01',monthYear), '%d%b%Y')) %>% 
  arrange(monthYear) %>%
  mutate(monthYear = format(monthYear,'%b%Y'))

ag_pumping_volume = agSpecifiedPumping %>%
  mutate(monthYear = paste0(month,year),
         well_name = gsub(pattern = ' ', replacement = '_', x = well_name),
         well_name = gsub(x = well_name, pattern = '\\(|\\)', replacement = ''),
         well_name = gsub(x = well_name, pattern = '&', replacement = 'and'),
         pumping_rate_m3day = if_else(is.na(pumping_rate_m3day), 0, pumping_rate_m3day)) %>%
  dplyr::select(well_id, monthYear, pumping_rate_m3day,  ) %>%
  pivot_wider(names_from = well_id, values_from = pumping_rate_m3day) %>%
  rename(well_id = monthYear) %>%
  left_join(read_pumping_rate %>% rename(well_id = monthYear)) %>%
  relocate(well_id, specify_pumping)

write.table(ag_pumping_volume, file = paste0(outDirSWBM, 'ag_well_specified_volume.txt'), append = F, quote = F, row.names = F, sep = '  ')

fields.sf = fields.sf %>%
  dplyr::select(-well_id) %>%                                         # remove partially populated well_id field
  left_join(wellsByPolygon, by = 'SWBM_id')                           # add fully populated well_id field to polygons

fieldsMF_dissolved.sf = fieldsMF_dissolved.sf %>%
  dplyr::select(-well_id) %>%                                         # remove partially populated well_id field
  left_join(wellsByPolygon, by = 'SWBM_id')                           # add fully populated well_id field to polygons

st_write(fields.sf, dsn = paste0(outdirGIS,'Land Use/SWBM_Fields.shp'), delete_dsn = T)              # write fields shapefile
st_write(fieldsMF_dissolved.sf, dsn = paste0(outdirGIS,'/Land Use/SWBM_Fields_MF_grid.shp'), delete_dsn = T)    # write fields mapped to MF grid shapefile

fields_GW.sf = fields.sf %>%
  filter((WtrSrcTxt == 'Groundwater' | WtrSrcTxt == 'Mixed') & SWBM_IRRtx != 'Non-Irrigated')
agWells_pumping.sf = agWells.sf %>%                                                                 # Identify active pumping wells
  filter(well_id%in%fields_GW.sf$well_id)

st_write(agWells_pumping.sf, paste0(outdirGIS,'Wells/SWBM_Ag_Pumping_Wells.shp'), delete_dsn = T)   # write active pumping wells to shapefile
st_write(fields_GW.sf, paste0(outdirGIS,'Land Use/SWBM_GW_Irr_Fields.shp'), delete_dsn = T)   # write GW irrigated fields to shapefile

# MNW2_wells.txt -------------------------------------------------------
nWells = nAgWells + nMuniWells
wellSummary = rbind(agWellAttTbl, muniWellAttTbl)
nWells = nrow(wellSummary)
well_diameters = sqlFetch(DB, 'wells') %>%
  filter(well_id %in% wellSummary$well_id) %>%
  dplyr::select(casing_diameter_inches)

cat('# Multi-Node Well (MNW2) Package', file = paste0(outDirSWBM,'MNW2_wells.txt'), append = F, sep = '\n')
cat(paste('# Data set created', Sys.Date(),'using MODFLOW Pre-Processing.R'), file = paste0(outDirSWBM,'MNW2_wells.txt'), append = T, sep = '\n')
cat('# Contains pumping values for agricultural irrigation wells and municipal supply wells', file = paste0(outDirSWBM,'MNW2_wells.txt'), append = T, sep = '\n')
cat(paste(nWells,  IWL2CB, '2  !  DataSet 1: MNWMAX, IWL2CB, MNWPRNT', sep = '  '), file = paste0(outDirSWBM,'MNW2_wells.txt'), append = T, sep = '\n')

if(LOSSTYPE == 'THIEM'){
  for(i in 1:nWells){
    temp_diam_m = well_diameters$casing_diameter_inches[i]/2*0.0254
    cat(paste0(wellSummary$well_name[i],'  -1      ! Data Set 2A: WELLID, NNODES'), file = paste0(outDirSWBM,'MNW2_wells.txt'), append = T, sep = '\n')
    cat(paste0('THIEM  0  0  0  0      ! Data Set 2B: LOSSTYPE, PUMPLOC, Qlimit, PPFLAG, PUMPCAP'), file = paste0(outDirSWBM,'MNW2_wells.txt'), append = T, sep = '\n')
    if (is.na(temp_diam_m)){                                                                                   # If no information available for well diameter
      cat('0.2032     ! Data Set 2C; Rw', file = paste0(outDirSWBM,'MNW2_wells.txt'), append = T, sep = '\n') # Assume 8 inch radius well (model units are in meters)
    } else {
      cat(paste0(temp_diam_m ,'     ! Data Set 2C; Rw'), file = paste0(outDirSWBM,'MNW2_wells.txt'), append = T, sep = '\n') # Use reported well dimensions (model units are in meters)
    }
    cat(paste(wellSummary$top_scrn_z[i], wellSummary$bot_scrn_z[i], wellSummary$row[i], wellSummary$col[i],
              '   ! Data Set 2D; Ztop, Zbotm, ROW, COL', sep = '  '), file = paste0(outDirSWBM,'MNW2_wells.txt'), append = T, sep = '\n')
  }
} else if(LOSSTYPE=='SPECIFYcwc'){
  for(i in 1:nWells){
    temp_diam_m = well_diameters$casing_diameter_inches[i]/2*0.0254
    cat(paste0(wellSummary$well_name[i],'  -1                   ! Data Set 2A: WELLID, NNODES'), file = paste0(outDirSWBM,'MNW2_wells.txt'), append = T, sep = '\n')
    cat(paste0('SPECIFYcwc  0  0  0  0        ! Data Set 2B: LOSSTYPE, PUMPLOC, Qlimit, PPFLAG, PUMPCAP'), file = paste0(outDirSWBM,'MNW2_wells.txt'), append = T, sep = '\n')
    cat('1000                          ! Data Set 2C; CWC', file = paste0(outDirSWBM,'MNW2_wells.txt'), append = T, sep = '\n') # Assume 8 inch radius well (model units are in meters)
cat(paste(wellSummary$top_scrn_z[i], wellSummary$bot_scrn_z[i], wellSummary$row[i], wellSummary$col[i],
              '   ! Data Set 2D; Ztop, Zbotm, ROW, COL', sep = '  '), file = paste0(outDirSWBM,'MNW2_wells.txt'), append = T, sep = '\n')
  }
} else {
  stop('Invalid LOSSTYPE')
}

# general_inputs.txt ------------------------------------------------------
cat('modelName  WYstart  npoly  nlandcover  nAgWells  nMuniWells  nSubws inflow_is_vol nSFR_inflow_segs  nmonths  nrows  ncols  UCODE/PEST  Basecase/MAR/ILR/MAR_ILR', 
    file = paste0(outDirSWBM, 'general_inputs.txt'), sep = '\n', append = F)
cat(paste(modelName, WYstart, npoly, nlandcover, nAgWells, nMuniWells, nSubws, inflow_is_vol, nSFR_inflow_segs, nmonths, numRow_MF, numCol_MF, calSoft, Scenario, sep = '  '), file = paste0(outDirSWBM, 'general_inputs.txt'), sep = '\n', append = T)


# print_daily.txt ------------------------------------------------------
print_daily = fieldsMF_dissolved.sf %>% 
  st_drop_geometry() %>%
  ungroup() %>%
  mutate(Field_Cat = paste(SWBM_LUtxt, WtrSrcTxt, SWBM_IRRtx, sep = '_'),
         Field_Cat = gsub(x = Field_Cat, pattern = 'Alfalfa/Grain', replacement = 'AG'),
         Field_Cat = gsub(x = Field_Cat, pattern = 'Pasture', replacement = 'P'), 
         Field_Cat = gsub(x = Field_Cat, pattern = 'Native Vegetation', replacement = 'NV'),
         Field_Cat = gsub(x = Field_Cat, pattern = 'Surface-Water', replacement = 'SW'),
         Field_Cat = gsub(x = Field_Cat, pattern = 'Groundwater', replacement = 'GW'),
         Field_Cat = gsub(x = Field_Cat, pattern = 'Groundwater', replacement = 'GW'),
         Field_Cat = gsub(x = Field_Cat, pattern = 'Center Pivot', replacement = 'CP'),
         Field_Cat = gsub(x = Field_Cat, pattern = 'Wheel Line', replacement = 'WL'))
print_daily = print_daily[!duplicated(print_daily$Field_Cat),] %>%
  dplyr::select(SWBM_id, Field_Cat) %>%
  arrange(Field_Cat)

cat(paste0(nrow(print_daily), '  TRUE'), file = paste0(outDirSWBM, 'print_daily.txt'), sep = '\n', append = F)
write.table(print_daily, file = paste0(outDirSWBM, 'print_daily.txt'), append = T, quote = F, sep = '  ', row.names = F, col.names = F)

# ndays.txt ----------------------------------------------------------------
seq.Date(from = paste0(WYstart-1,'-10-01') %>% as.Date(), 
         to = paste0(WYend,'-10-01') %>% as.Date(), 
         by = 'month') %>%
  diff() %>%
  as.data.frame() %>%
  mutate(Stress_Period = seq(1:nmonths)) %>%
  rename('nDays' = '.') %>%
  relocate(Stress_Period, nDays) %>%
  write.table(file = paste0(outDirSWBM, 'ndays.txt'), append = F, quote = F, row.names = F, sep = '  ')

toc() # total script runtime

# system_commands.txt -----------------------------------------------------
cat('4', file = paste0(outDirSWBM, 'system_commands.txt'), append = F, sep = '\n')
cat('mkdir recharge', file = paste0(outDirSWBM, 'system_commands.txt'), append = T, sep = '\n')
cat('copy SFR_network.txt SVHSM.sfr', file = paste0(outDirSWBM, 'system_commands.txt'), append = T, sep = '\n')
cat('copy MNW2_wells.txt SVHSM.mnw2', file = paste0(outDirSWBM, 'system_commands.txt'), append = T, sep = '\n')
cat('copy SFR_network_jtf.txt SVHSM_sfr.jtf', file = paste0(outDirSWBM, 'system_commands.txt'), append = T, sep = '\n')

