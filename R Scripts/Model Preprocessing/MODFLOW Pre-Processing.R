# MODFLOW Pre-Processing.R
# User Inputs -------------------------------------------------------------

#General Information
modelName = 'SVHSM'
outDirMODFLOW = 'MODFLOW_v3/'
outdirGIS = 'GIS/Modeling_150m_grid_v3/'
DB_name ='SierraValley'
SharedDB_name = 'OFPShared'
my_server=''
my_username=''
my_pwd='(nLD'

# Temporal Information
WYstart = 2000                                   # Beginning water year of simulation
WYend = 2020                                     # Ending water year of simulatuion
obsBuffer = 365                                 # number of days from start of model that observations are not used
 
# Model Discretization
modelBoundary = 'Sierra_Valley_Groundwater_Basin.shp'
nZones = 8                    # number of hydrologic property zones
NLAY = 12                     # number of model layers
NROW = 216                    # number of model rows
NCOL = 243                    # number of model columns
# NPER automatically calculated based on WYstart and WYend values
ITMUNI = 4                    # MODFLOW time units
LENUNI = 2                    # MODFLOW length units
dx = 150                      # grid spacing in x-direction
dy = 150                      # grid spacing in y-direction

# Solver Options
HEADTOL = 1E-2      # maximum head change between outer iterations for solution of the nonlinear problem (real)
FLUXTOL = 25        # maximum root-mean-squared flux difference between outer iterations for solution of the nonlinear problem (real)
MAXITEROUT = 400    # maximum number of iterations to be allowed for solution of the outer (nonlinear) problem (integer)
THICKFACT = 1E-5    # portion of the cell thickness (length) used for smoothly adjusting storage and conductance coefficients to zero
LINMETH = 2         # 1 = GMRES matrix solver, 2 = xMD matrix solver
IPRNWT = 0
IBOTAV = 0

# Upstream weighting package (UPW) options
IUPWCB = 50     # cell-by-cell file unit number      
HDRY =  -5555   # head that is assigned to cells that are converted to dry during a simulation
NPUPW = 40      # number of UPW parameters
IPHDRY = 1       # flag that indicates whether groundwater head will be set to HDRY when the groundwater head is less than 1×10-4 above the cell bottom
unconfined_layers = c(1,2,3)   # specify which model layers will be unconfined (LAYTYP = 1)


# Head Observation Package (HOB) Options
rotated = T                   # Flag specifying if grid is rotated (calculation method for ROFF and COFF assumes no grid rotation) 
GRIDROTATION = -35            # Degrees, negative values indicate counter-clockwise rotation
MF_grid_unrotated = 'modelGrid_MF_unrotated.shp'     # MF grid "derotated"
Obs_wells_unrotated = 'MF_Observation_Wells_unrotated.shp' # Observation wells "derotated"
min_nObs = 3    # minimum number of observations for a well to be included in the HOB file
ignore_well_id = c(202)   # observation well_id values to exclude (comment out if no wells are excluded)
TOMULTH = 1.0   # time-offset multiplier for head observations

# Gage Package Data
SFR_gages = data.frame(buffer = '',
                       ISEG = 51,
                       IRCH = 2,
                       UNIT = 601,
                       OUTTYPE = 4,
                       GAGE_NAME = '# UMMFR',
                       out_file = paste0(modelName,'_streamflow_UMFFR.dat'))

# SFR Options
ISTCB2 = 600

# ETS Package Options
NETSOP = 1
IETSCB = 50
NPETS = 0
NETSEG = 2

# Wel Package Options
IUNITRAMP = 100

#MNWI Package Options
WEL1flag = 101
QSUMflag = 102
BYNDflag = 103
MNWI_output_wells = c('DMS_047', 'DMS_013', 'DMS_026', 'DMS_039')

# General Inputs
MF_grid = 'modelGrid_MF.shp'
dis_file = 'Leapfrog Geologic Model/Leapfrog_export_2_MODFLOW_20210921/SVHSM_MODFLOW_150m_Grid_(12_Layers).dis'
zon_file = 'Leapfrog Geologic Model/Leapfrog_export_2_MODFLOW_20210921/SVHSM_MODFLOW_150m_Grid_(12_Layers).zon'
gridInfo =  paste0(outdirGIS, 'Grid/MF_grid_dis_bas_zon_wel_adj.shp')   # Shapefile of grid information (manually adjusted if necessary)
ZoneBud = 'MF_grid_ZonBud.shp'

ActiveCellAdjustment = F
AdjustGrid4Faults = T
MFR_BoundaryAdjustment = F
faults = 'All_Modeled_faults_v2.shp'
alter_faults = c('Loyalton Fault', 'Grizzly Valley Fault')   # array of fault names where hydraulic properties should be altered
faultLyrs = list(c(4:12),c(4:12))                            # list of layer numbers to adjust zone values for (must be same order as alter_faults)
fault_prop_zones = c(7,8)                                    # array of new zone numbers to assign to fault cells

MFR_Segments = 'MFR_Segments.shp'
MFR_Layer_Boundaries_adj = 'SVHSM_Layer_Boundaries_adj.shp'     
HighFluxZones = c(1,2,6,7,8)                                 # Zone numbers where MFR fluxes are higher
HighFluxFraction = 1                                       # Fraction of MFR that comes into HighFluxZones (0-1)          

PRMS_grid = 'grid_100m_35deg_cc.shp'
PRMS_recharge_monthly = 'PRMS/SV_prms_v2/recharge.monthly'
MFR_Seg_Contributing_Area = 'MFR_Seg_Contributing_Area.shp'
ETo_file = 'SWBM/input/ref_et.txt'

# Script Initialization ---------------------------------------------------
library(dplyr)
library(tidyr)
library(lubridate)
library(sf)
library(lwgeom)
library(tictoc)
library(RODBC)

options(dplyr.summarise.inform = FALSE)
dir.create(paste0(outdirGIS,'Grid'), showWarnings = F)
dir.create(paste0(outdirGIS,'Land Use'), showWarnings = F)
dir.create(paste0(outdirGIS,'MFR'), showWarnings = F)
dir.create(paste0(outdirGIS,'Surface Water'), showWarnings = F)
dir.create(outDirMODFLOW, showWarnings = F)
dir.create(paste0(outDirMODFLOW,'zones'), showWarnings = F)
dir.create(paste0(outDirMODFLOW,'active_cells'), showWarnings = F)
dir.create(paste0(outDirMODFLOW,'layer_z'), showWarnings = F)
dir.create(paste0(outDirMODFLOW,'ET'), showWarnings = F)

# Functions ---------------------------------------------------------------
mrounddown = function(x,base){                                                                # function for rounding numbers down to specified base
  base*floor(x/base)
}

#_______________________________________________

'%!in%' = function(x,y)!('%in%'(x,y))

#_______________________________________________

MF_Mat2Tbl = function(dis_filename, dis_skipLines, n_mat_rows, nrow, ncol, headers){
  for(i in 1:length(dis_skipLines)){
    matrix_values = read.table(file = dis_filename, header = F, skip = dis_skipLines [i], nrows = n_mat_rows, fill = T, na.strings = '') %>%          # Read in text file and subset
      as.matrix() %>%                                                              # convert table to matrix
      t() %>%                                                                      # transpose
      as.vector() %>%                                                              # convert to vector
      na.omit()                                                                    # remove NA values
    temp.out = data.frame(row_MF = rep(seq(1,nrow), each = ncol),               # create dataframe
                          col_MF = rep(seq(1,ncol), nrow),
                          header = headers[i],
                          value = matrix_values)
    if (i==1){
      MF_Mat2Tbl_out = temp.out
    }  else {
      MF_Mat2Tbl_out = rbind(MF_Mat2Tbl_out,temp.out)
    }
  }
  return(MF_Mat2Tbl_out)
}

#_______________________________________________

joinValues2Grid = function(grid, MF_Mat2Tbl_out){
  MF_Mat2Tbl_out = MF_Mat2Tbl_out %>%
    pivot_wider(names_from = header, values_from = value)
  grid = grid %>%
    left_join(MF_Mat2Tbl_out, by = c('row_MF', 'col_MF'))
    return(grid)
}

#_______________________________________________
extract_MFR_Recharge = function(PRMS_rech_file, extractDates, contrib_area.sf, modelGrid.sf){
  modelGrid_joined.sf = st_join(modelGrid.sf, contrib_area.sf) %>%     # perform spatial join
    filter(!duplicated(.$geometry)) %>%              # remove duplicated values from spatial join
    mutate(area_m2 = st_area(.) %>% as.vector())     # add cell area
  rechargeText = readLines(PRMS_rech_file)                             # read PRMS recharge output
  data_block_idx = grep(x = rechargeText, pattern = 'Basin')
  model_recharge_idx = rechargeText[data_block_idx] %>%
    grep(pattern = paste(extractDates %>% format('%Y/%m/%d'), collapse = '|'))
  startLines =   data_block_idx[model_recharge_idx]
  endLines =   startLines + max(modelGrid.sf$row) - 1

  for(i in 1:length(extractDates)){                                    # loop over months
    recharge = rechargeText[startLines[i]:endLines[i]] %>%                                             # subset text
      strsplit(split = " ") %>%                                                                # split strings
      lapply(function(x){x[!x ==""]}) %>%                                                      # remove blanks
      unlist() %>%                                                                             # remove list format
      as.numeric()                                                                             # convert text to numbers

    temp_recharge = modelGrid_joined.sf %>%
      st_drop_geometry() %>%
      mutate(monthly_recharge_m3day = recharge*area_m2,
             month = extractDates[i] %>% as.Date('%Y/%m/%d')) %>%
      group_by(month, MFR_Seg) %>%
      summarize(monthly_recharge_m3day = sum(monthly_recharge_m3day), quiet = T) %>%
      filter(!is.na(MFR_Seg))

    if(i==1){
      monthly_recharge = temp_recharge
    } else {
      monthly_recharge = rbind(monthly_recharge, temp_recharge)
    }
    average_recharge = monthly_recharge %>%
      group_by(MFR_Seg) %>%
      summarize(average_recharge_m3day = mean(monthly_recharge_m3day))

  }
  return(average_recharge)
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

# Create Date Arrays ------------------------------------------------------
WYstartDate = paste0(WYstart-1,'-10-01') %>% as.Date()
WYendDate = paste0(WYend,'-09-30') %>% as.Date()
modelDays = seq.Date(from = WYstartDate, to = WYendDate, by = 'day')
modelMonths = seq.Date(from = WYstartDate, to = WYendDate, by = 'month')
NPER = length(modelMonths)
modelYears = (WYend - WYstart) + 1
modelTimes = data.frame(water_level_date = modelDays,
                        IREFSP = rep(seq(1,NPER), days_in_month(modelMonths)),
                        TOFFSET = format(modelDays, '%d') %>% as.numeric()-1)

# Read Data ---------------------------------------------------------
MF_grid.sf = st_read(MF_grid)
modelBoundary.sf = st_read(modelBoundary)
faults.sf = st_read(faults)
ZoneBud.sf = st_read(ZoneBud)
dis_text = readLines(dis_file)
zon_text = readLines(zon_file)
if(rotated){
  MF_grid_unrotated.sf = st_read(MF_grid_unrotated)
  Obs_wells_unrotated.sf = st_read(Obs_wells_unrotated)
}
MFR_Segments.sf = st_read(MFR_Segments)
MFR_Contrib_Area.sf = st_read(MFR_Seg_Contributing_Area)
PRMS_grid.sf = st_read(PRMS_grid)

# Process Discretization and Property Zone Inputs ------------------------------------
dis_mat_skipLines = grep(pattern = 'INTERNAL', x = dis_text)[c(-1,-2)]            # find rows where matrices begin
dis_mat_nrows = dis_mat_skipLines[2] - dis_mat_skipLines[1] - 1                   # find number of rows for each matrix
dis_headers = c('L1_top_z', 'L1_bot_z', paste0('L',seq(2,NLAY),'_bot_z'))
dis_table = MF_Mat2Tbl(dis_filename = dis_file, dis_skipLines = dis_mat_skipLines, n_mat_rows = dis_mat_nrows, nrow = NROW, ncol = NCOL, headers = dis_headers)  

zon_mat_skipLines = grep(pattern = 'INTERNAL', x = zon_text)                      # find rows where matrices begin
zon_mat_nrows = zon_mat_skipLines[2] - zon_mat_skipLines[1] - 2                   # find number of rows for each matrix
zon_names = paste0('LITHO',seq(1,12))
zon_headers = paste0('L',seq(1,12),'_Litho')
zon_table = MF_Mat2Tbl(zon_file, zon_mat_skipLines, zon_mat_nrows, nrow = NROW, ncol = NCOL, zon_headers)  

MF_grid_z.sf = joinValues2Grid(MF_grid.sf, dis_table) 
MF_grid_zones.sf = joinValues2Grid(MF_grid.sf, zon_table) 

MF_grid_info.sf = left_join(MF_grid_zones.sf, MF_grid_z.sf %>%
                              st_drop_geometry() %>%
                              dplyr::select(-MF_idx, -cell_type, -row_global, -col_global, -global_id),
                            by =c('row_MF', 'col_MF'))

# for (i in 1:NLAY){
#   MF_grid_info.sf = MF_grid_info.sf %>%
#     mutate(!!sym(paste0('L',i,'_MFRseg')) := NA)  # populate MFR columns with NA in shapefile (need to manually select which cells have MFR assigned)
# }

st_write(MF_grid_info.sf, dsn = paste0(outdirGIS, 'Grid/MF_grid_dis_bas_zon_wel.shp'), delete_dsn = T)



###
### Modify property grid shapefile accordingly before continuing if adjustments are needed.
###
if(ActiveCellAdjustment){
  stop('Begin manual modification of MF_grid_dis_bas_zon_wel.shp')
}
gridInfo.sf = st_read(gridInfo)

# Export layer boundary polylines for defining MFR cells 
for (i in 1:NLAY){
  temp = gridInfo.sf %>%
    mutate(active = if_else(!!sym(paste0('L',i,'_Litho')) == 0,0,1)) %>%
    filter(active != 0 ) %>%
    summarize() %>%
    st_cast(to = 'MULTILINESTRING') %>%
    st_cast(to = 'LINESTRING') %>%
    mutate(Layer = i)
  
  if (i==1){
    layer_boundaries.sf = temp
  } else{
    layer_boundaries.sf = rbind(layer_boundaries.sf, temp)
  }
  
}
st_write(layer_boundaries.sf, dsn = paste0(outdirGIS, 'MFR/',modelName,'_Layer_Boundaries.shp'), delete_dsn = T)

###
### Modify MFR boundary shapefile accordingly before continuing if adjustments are needed.
###
if(MFR_BoundaryAdjustment){
  stop(paste0('Begin manual modification of ',modelName,'_Layer_Boundaries.shp'))
}

# Create new zones for faults
if(AdjustGrid4Faults){
  for (i in 1:length(alter_faults)){
    lyrs = faultLyrs[[i]]
    faultCells.sf = faults.sf %>%      
      filter(Name == alter_faults[i]) %>%        
      st_buffer(dist = 100) %>%                            # set buffer distance (m) from fault
      st_intersects(gridInfo.sf) %>%        # find model cells that intersect with buffer
      unlist() %>%                                         # return row numbers of cells that intersect with the buffered fault
      gridInfo.sf[.,] 
    
    for (j in 1:length(lyrs)){
      temp.sf = faultCells.sf %>%
        filter(!!sym(paste0('L',lyrs[j],'_Litho')) != 0)
      gridInfo.sf = gridInfo.sf %>%
        mutate(!!sym(paste0('L',lyrs[j],'_Litho')) := if_else(MF_idx%in%temp.sf$MF_idx,true = fault_prop_zones[i] %>% as.integer(), !!sym(paste0('L',lyrs[j],'_Litho')) %>%as.integer()))
    }
  }
}
st_write(gridInfo.sf, dsn = paste0(outdirGIS, 'Grid/MF_grid_dis_bas_zon_wel_adj_w_faults.shp'), delete_dsn = T)


# Calculate MFR fluxes
MFR_months = seq.Date(from = as.Date('1999-11-01'), to = as.Date('2020-10-01'), by = 'month') - 1
MFR_avg_flux = extract_MFR_Recharge(PRMS_recharge_monthly, MFR_months, MFR_Contrib_Area.sf, PRMS_grid.sf)

MFR_Layer_Boundaries_adj.sf = st_read(MFR_Layer_Boundaries_adj) %>%
  arrange(Layer, MFR_Seg)
MFR_lyrs = unique(MFR_Layer_Boundaries_adj.sf$Layer)
MFR_seg_nums = unique(MFR_Layer_Boundaries_adj.sf$MFR_Seg)

for(k in 1:length(MFR_lyrs)){                     # loop over layers
  temp = MFR_Layer_Boundaries_adj.sf %>%          # create temporary dataframe
    filter(Layer == MFR_lyrs[k]) %>%              # subset dataframe by layer
    st_join(gridInfo.sf %>%                       # join model grid information
              dplyr::select(row_MF, col_MF, MF_idx, row_global, col_global, global_id, paste0('L',k,'_Litho')), ., by = st_intersection) %>%
    st_drop_geometry() %>%
    filter(!is.na(Layer), !!sym(paste0('L',k,'_Litho')) != 0) %>%  # filter by layer
    rename(Zone = paste0('L',k,'_Litho')) %>%                      # rename column for joining later
    arrange(Layer, MFR_Seg)                                        # sort dataframe
  if(k==1){                                                        # combine dataframes for each layer
    MFR_cells = temp                                               
  } else {
    MFR_cells = rbind(MFR_cells, temp)
  }
}
MFR_cells_summary = MFR_cells %>%                                  # summarize number of cells associated with each MFR parameter
  group_by(MFR_Seg) %>% 
  summarize(count = n())

MFR_flux_summary = MFR_cells %>%                                   # distribute proportion of total flux to each cell based on zone using Qfact (Qfact should sum to 1 for each MFR parameter)
  mutate(Flux_Grp = case_when(
    Zone%in%HighFluxZones ~ 'High',                                   # Sands and Gravels, Silty Clayey Sands and Gravels, and the two fault zones
    TRUE ~ 'Low')) %>%
  group_by(MFR_Seg, Flux_Grp) %>% 
  summarize(nCells = n()) %>%                               # count cells
  left_join(MFR_avg_flux, by = 'MFR_Seg') %>%
  mutate(MFR_flux = if_else(Flux_Grp =='High', 0.5*average_recharge_m3day*HighFluxFraction, 0.1*average_recharge_m3day*(1-HighFluxFraction)),  # Specified HighFluxFraction of recharge (assume 50% of PRMS recharge) comes in via the coarse sediments 
         Qfact = if_else(Flux_Grp =='High', HighFluxFraction*(1/nCells ), (1-HighFluxFraction)*(1/nCells )))   # divide flux by number of cells it is distributed across

MFR_cell_info = expand.grid(MFR_seg_nums, seq(1,nZones)) %>%
  rename(MFR_Seg = Var1, Zone = Var2) %>%
  mutate(Flux_Grp = case_when(
    Zone%in%HighFluxZones ~ 'High',
    TRUE ~ 'Low')) %>%
  left_join(MFR_flux_summary) %>%
  dplyr::select(MFR_Seg, Zone, Qfact) %>%
  right_join(MFR_cells) %>%
  dplyr::select(MFR_Seg, Layer, row_MF, col_MF, Qfact) %>%
  filter(Qfact>0)

# Write zones and active cells to external matrix files 
for(i in 1:NLAY){
  temp_zones = gridInfo.sf %>%
    st_drop_geometry() %>%
    arrange(row_MF, col_MF) %>%
    dplyr::select(zon_headers[i]) %>%
    unlist() %>%
    matrix(nrow = NROW, ncol = NCOL, byrow = T)
  
  temp_active = temp_zones
  temp_active[temp_active>0] = 1
  
  write.table(temp_zones, file = paste0(outDirMODFLOW, 'zones/L',i,'zones.txt'), quote = F, row.names = F, col.names = F, sep = '\t')
  write.table(temp_active, file = paste0(outDirMODFLOW, 'active_cells/L',i,'active_cells.txt'), quote = F, row.names = F, col.names = F, sep = '\t')
}

for (i in 1:(NLAY+1)){
  z_matrix = gridInfo.sf %>%
    st_drop_geometry() %>%
    arrange(row_MF, col_MF) %>%
    dplyr::select(dis_headers[i]) %>%
    unlist() %>%
    matrix(nrow = NROW,ncol = NCOL, byrow = T) %>%
    write.table(file = paste0(outDirMODFLOW,'layer_z/',dis_headers[i],'.txt'), quote = F, row.names = F, col.names = F, sep = '\t')
}

# Process Water Level Observation Input Data ------------------------------
MF_grid_centroids.sf = st_centroid(MF_grid.sf) %>%
  mutate(grid_centroid_easting = st_coordinates(.)[,1],
         grid_centroid_northing = st_coordinates(.)[,2])

st_write(obj = MF_grid_centroids.sf %>% 
           rename(easting = grid_centroid_easting,
                  northing = grid_centroid_northing), dsn = paste0(outdirGIS, 'Grid/MF_grid_centroids.shp'), delete_dsn = T)
WL_obs.sf = sqlFetch(DB,'water_levels') %>% 
  as_tibble() %>%
  mutate(water_level_date = as.Date(water_level_date)) %>%
  filter(water_level_date > WYstartDate + obsBuffer, 
         water_level_date < WYendDate,
         !is.na(water_level_elev_ft),
         qaQCLevel == 'High') %>%
  mutate(water_level_elev_m = water_level_elev_ft/3.28084) %>%
  left_join(sqlFetch(DB,'wells') %>% dplyr::select(well_id, well_name,well_long,well_lat), by = 'well_id') %>%
  st_as_sf(coords = c('well_long', 'well_lat'), crs = 4269) %>%
  st_transform(crs = 26910) %>%
  arrange(well_name, water_level_date) %>%
  st_intersection(modelBoundary.sf) %>%
  dplyr::select(!names(modelBoundary.sf)) %>%
  relocate(well_id, well_name) %>% 
  group_by(well_id) %>%
  filter(n() > min_nObs) %>%
  arrange(well_id)

if(exists('ignore_well_id')){                  # Remove wells specified in ignore_well_id
  WL_obs.sf = WL_obs.sf %>%
    filter(well_id%!in%ignore_well_id)
}

write.table(WL_obs.sf %>% st_drop_geometry(), paste0(outDirMODFLOW, 'SVHSM_Water_Level_Observations.dat'), append = F, na = '', quote = F, row.names = F, col.names = T, sep = '\t')

WL_wells.sf = WL_obs.sf %>%
  group_by(well_id, well_name) %>%
  summarize(Date_min = min(water_level_date), Date_max = max(water_level_date),
            WLE_m_min = min(water_level_elev_m), WLE_m_max = max(water_level_elev_m),
            num_WL_obs = n()) %>%
  arrange(well_id)

st_write(WL_wells.sf, dsn = paste0(outdirGIS, 'Wells/MF_Observation_Wells.shp'), delete_dsn = T)

if(rotated==F){
  HOB_info = WL_wells.sf %>%
  st_intersection(MF_grid.sf) %>%
  left_join(MF_grid_centroids.sf %>% 
              st_drop_geometry() %>% 
              dplyr::select(row_MF, col_MF, grid_centroid_easting, grid_centroid_northing), by = c('row_MF', 'col_MF')) %>%
  mutate(well_easting = st_coordinates(.)[,1],
         well_northing = st_coordinates(.)[,2]) %>%
  group_by(well_id, well_name, row_MF, col_MF, MF_idx, row_global, col_global, well_easting, well_northing, grid_centroid_easting, grid_centroid_northing) %>%
  summarize(num_WL_obs = n()) %>%
  st_drop_geometry() %>%
  ungroup() %>%
  mutate(ROFF = (well_easting-grid_centroid_easting)/(dx/cos(35*pi/180)),          # row offset used to locate the observation within a finite-difference cell
         COFF = (well_northing-grid_centroid_northing)/(dy/cos(35*pi/180))) %>%        # column offset used to locate the observation within a finite-difference cell
  rename(easting = well_easting,
         northing = well_northing) %>%I
    dplyr::select(-grid_centroid_easting, -grid_centroid_northing)
    } else {
      HOB_info = Obs_wells_unrotated.sf %>%
        mutate(well_easting = st_coordinates(.)[,1],
               well_northing = st_coordinates(.)[,2]) %>%
        st_join(MF_grid_unrotated.sf %>% 
                  st_centroid() %>%
                  mutate(grid_centroid_easting = st_coordinates(.)[,1],   # add x coodinates of grid centroids
                         grid_centroid_northing = st_coordinates(.)[,2]), # add y coodinates of grid centroids
                join = st_nearest_feature) %>%
        mutate(ROFF = (well_easting-grid_centroid_easting)/(dx),          # row offset used to locate the observation within a finite-difference cell
               COFF = (well_northing-grid_centroid_northing)/dy) %>%      # column offset used to locate the observation within a finite-difference cell
        rename(easting = well_easting,
               northing = well_northing) %>%
        dplyr::select(well_id, well_name, row_MF, col_MF, MF_idx, row_global, col_global, easting, northing, num_WL_obs, ROFF, COFF) %>%
          st_drop_geometry()
    }
gridInfo_ObsWells = HOB_info %>%
  filter(well_id%in%WL_obs.sf$well_id) %>%
  left_join(gridInfo.sf %>% 
              st_drop_geometry() %>%
              dplyr::select(-paste0('L',seq(1,NLAY),'_Litho'))) %>%       # remove columns with zone information
  left_join(sqlFetch(DB,'wells') %>% dplyr::select(well_id, elev_ground_surface, well_total_depth_ft, top_of_screen_depth_ft, bottom_of_screen_depth_ft, other_screen_intervals)) %>%
  mutate(elev_ground_surface = elev_ground_surface / 3.28084,             # convert from ft to m
         well_total_depth_m = well_total_depth_ft / 3.28084,              # convert from ft to m
         top_of_screen_depth_m = top_of_screen_depth_ft /3.28084,         # convert from ft to m
         bottom_of_screen_depth_m = bottom_of_screen_depth_ft /3.28084,   # convert from ft to m
         top_of_screen_z = case_when(
           !is.na(top_of_screen_depth_ft) ~ (elev_ground_surface - top_of_screen_depth_m),                                  # use top of screen depth if available
           TRUE ~ L1_top_z - 5),                                                                                            # otherwise assume screened from 5 m below surface
         bottom_of_screen_z = case_when(
           !is.na(bottom_of_screen_depth_ft) ~ (elev_ground_surface - bottom_of_screen_depth_m),                            # use bottom of screen depth if available
           is.na(bottom_of_screen_depth_ft) & !is.na(well_total_depth_m) ~ (elev_ground_surface - well_total_depth_m),      # if not, assume screened to bottom
           TRUE ~ elev_ground_surface - 150),                                                                               # assume well is 150 m deep if no depth info
         ROFF = round(ROFF,digits = 2),
         COFF = round(COFF,digits = 2),
         TOS_dpth_sim_m = elev_ground_surface - top_of_screen_z,
         BOS_dpth_sim_m = elev_ground_surface - bottom_of_screen_z) %>%
  mutate_if(is.double, round, 2) %>%
  arrange(well_id)

if(exists('ignore_well_id')){                  # Remove wells specified in ignore_well_id
  gridInfo_ObsWells = gridInfo_ObsWells %>%
    filter(well_id%!in%ignore_well_id)
}

write.table(gridInfo_ObsWells %>% dplyr::select(well_id, well_name, row_MF, col_MF, MF_idx, elev_ground_surface,
                                             well_total_depth_m, top_of_screen_depth_m, bottom_of_screen_depth_m,
                                             TOS_dpth_sim_m, BOS_dpth_sim_m, num_WL_obs,
                                             easting, northing, row_global, col_global, global_id), 
            paste0(outDirMODFLOW, 'SVHSM_Observation_Well_Info.dat'), 
            append = F, na = '', quote = F, row.names = F, col.names = T, sep = '\t')
         
layer_tops = gridInfo_ObsWells %>%
  dplyr::select(L1_top_z, paste0('L',seq(1,NLAY-1), '_bot_z'))
layer_bots = gridInfo_ObsWells %>%
  dplyr::select(paste0('L',seq(1,NLAY), '_bot_z'))

well_layers = list(NA)
for(i in 1:nrow(gridInfo_ObsWells)){
  c = NA
  for(j in 1:(NLAY)){
    if(gridInfo_ObsWells$top_of_screen_z[i] > layer_bots[i,j] & gridInfo_ObsWells$bottom_of_screen_z[i] < layer_tops[i,j])
      c = c(c,j)
  }
  well_layers[[i]] = c[complete.cases(c)]
}
multiLayerObsWells = gridInfo_ObsWells %>%
  mutate(multiLayerObs = lapply(X = well_layers, FUN = length) %>% unlist()>1) %>%   # add field that identifies if well is screened across multiple model layers
  filter(multiLayerObs) %>%
  dplyr::select(well_id) %>%
  unlist()

WL_obs.sf = WL_obs.sf %>%
  left_join(modelTimes)

# Discretization (DIS) Package ------------------------------------------------------------
XFIRSTCOORD = MF_grid_centroids.sf %>%
  st_drop_geometry() %>%
  filter(row_MF ==1, col_MF==1) %>%
  dplyr::select(grid_centroid_easting) %>% 
  unlist()
YFIRSTCOORD = MF_grid_centroids.sf %>%
  st_drop_geometry() %>%
  filter(row_MF ==1, col_MF==1) %>%
  dplyr::select(grid_centroid_northing) %>%
  unlist()

cat('# MODFLOW Discretization (DIS) Package', file = paste0(outDirMODFLOW, modelName, '.dis'), append = F, sep = '\n')
cat(paste('# Data set created', Sys.Date(),'using MODFLOW Pre-Processing.R'), file = paste0(outDirMODFLOW, modelName, '.dis'), append = T, sep = '\n')
cat('#', file = paste0(outDirMODFLOW, modelName, '.dis'), append = T, sep = '\n')
cat(paste(NLAY, NROW, NCOL, NPER ,ITMUNI, LENUNI, XFIRSTCOORD, YFIRSTCOORD, GRIDROTATION), file = paste0(outDirMODFLOW, modelName, '.dis'), append = T, sep = '\n')
cat('0  0  0  0  0  0  0  0  0  0  0  0        !(No false layers)', file = paste0(outDirMODFLOW, modelName, '.dis'), append = T, sep = '\n')
cat('INTERNAL  1  (FREE)  1       ! delta-x, column sizes', file = paste0(outDirMODFLOW, modelName, '.dis'), append = T, sep = '\n')
cat(paste(rep(format(dx, nsmall = 1), NCOL), collapse = '  '), file = paste0(outDirMODFLOW, modelName, '.dis'), append = T, sep = '\n')
cat('INTERNAL  1  (FREE)  1       ! delta-y, row sizes', file = paste0(outDirMODFLOW, modelName, '.dis'), append = T, sep = '\n')
cat(paste(rep(format(dy, nsmall = 1), NCOL), collapse = '  '), file = paste0(outDirMODFLOW, modelName, '.dis'), append = T, sep = '\n')
for (i in 1:(NLAY+1)){
  cat(paste0('OPEN/CLOSE .\\layer_z\\',dis_headers[i],'.txt  1  (FREE)  -1     !',dis_headers[i]), file = paste0(outDirMODFLOW, modelName, '.dis'), append = T, sep = '\n')
}
for (i in 1:NPER){
  cat(paste(days_in_month(modelMonths[i]), days_in_month(modelMonths[i]), 1, 'TR', sep = '  '), file = paste0(outDirMODFLOW, modelName, '.dis'), append = T, sep = '\n')
}

# Basic (BAS6) Package ------------------------------------------------------------
cat('# MODFLOW Basic (BAS6) Package', file = paste0(outDirMODFLOW, modelName, '.bas'), append = F, sep = '\n')
cat(paste('# Data set created', Sys.Date(),'using MODFLOW Pre-Processing.R'), file = paste0(outDirMODFLOW, modelName, '.bas'), append = T, sep = '\n')
cat('#', file = paste0(outDirMODFLOW, modelName, '.bas'), append = T, sep = '\n')

# cat('FREE SHOWPROGRESS', file = paste0(outDirMODFLOW, modelName, '.bas'), append = T, sep = '\n')    # Keep for older MODFLOW executables

cat('BEGIN OPTION', file = paste0(outDirMODFLOW, modelName, '.bas'), append = T, sep = '\n')           # MFOWHM_v2 options block
cat(paste('  START_DATE', WYstartDate), file = paste0(outDirMODFLOW, modelName, '.bas'), append = T, sep = '\n')
cat('  NO_FAILED_CONVERGENCE_STOP', file = paste0(outDirMODFLOW, modelName, '.bas'), append = T, sep = '\n')
cat('  BUDGETDB MODFLOW_Budget.dat', file = paste0(outDirMODFLOW, modelName, '.bas'), append = T, sep = '\n')
cat('END', file = paste0(outDirMODFLOW, modelName, '.bas'), append = T, sep = '\n')

for (i in 1:NLAY){
  cat(paste0('OPEN/CLOSE  .\\active_cells\\L',i,'active_cells.txt  1  (FREE)  -1     ! Active cell matrix for layer ',i), file = paste0(outDirMODFLOW, modelName, '.bas'), append = T, sep = '\n')
}
cat('-9999     ! Default head value for inactive cells (HNOFLO)', file = paste0(outDirMODFLOW, modelName, '.bas'), append = T, sep = '\n')
for (i in 1:NLAY){
  cat(paste0('OPEN/CLOSE  .\\layer_z\\L1_top_z.txt  1  (FREE)  -1     # Initial heads for layer ',i), file = paste0(outDirMODFLOW, modelName, '.bas'), append = T, sep = '\n')
}

# Zone (ZONE) package ------------------------------------------------------------
cat('# MODFLOW Zone (ZON) Package', file = paste0(outDirMODFLOW, modelName, '.zone'), append = F, sep = '\n')
cat(paste('# Data set created', Sys.Date(),'using MODFLOW Pre-Processing.R'), file = paste0(outDirMODFLOW, modelName, '.zone'), append = T, sep = '\n')
cat('#', file = paste0(outDirMODFLOW, modelName, '.zone'), append = T, sep = '\n')
cat('#     0 = Inactive Cell', file = paste0(outDirMODFLOW, modelName, '.zone'), append = T, sep = '\n')
cat('#     1 = Aquifer Sediments, Sand and Gravel', file = paste0(outDirMODFLOW, modelName, '.zone'), append = T, sep = '\n')
cat('#     2 = Aquifer Sediments, Silty Clayey Sand and Gravel', file = paste0(outDirMODFLOW, modelName, '.zone'), append = T, sep = '\n')
cat('#     3 = Aquifer Sediments, Sandy Gravelly Silt and Clay', file = paste0(outDirMODFLOW, modelName, '.zone'), append = T, sep = '\n')
cat('#     4 = Aquifer Sediments, Silt and Clay', file = paste0(outDirMODFLOW, modelName, '.zone'), append = T, sep = '\n')
cat('#     5 = Aquifer Sediments, Tuff', file = paste0(outDirMODFLOW, modelName, '.zone'), append = T, sep = '\n')
cat('#     6 = Aquifer Sediments, No Data', file = paste0(outDirMODFLOW, modelName, '.zone'), append = T, sep = '\n')
cat('#     7 = Loyalton Fault', file = paste0(outDirMODFLOW, modelName, '.zone'), append = T, sep = '\n')
cat('#     8 = Grizzly Valley Fault', file = paste0(outDirMODFLOW, modelName, '.zone'), append = T, sep = '\n')
cat('#', file = paste0(outDirMODFLOW, modelName, '.zone'), append = T, sep = '\n')
cat(paste0(NLAY,'     ! Number of zone matrices to read in '), file = paste0(outDirMODFLOW, modelName, '.zone'), append = T, sep = '\n')
for(i in 1:NLAY){
  cat(paste0(zon_names[i]), file = paste0(outDirMODFLOW, modelName, '.zone'), append = T, sep = '\n')
  cat(paste0('OPEN/CLOSE  .\\zones\\L',i,'zones.txt  1  (FREE)  -1     ! Lithology Zones for layer ',i), file = paste0(outDirMODFLOW, modelName, '.zone'), append = T, sep = '\n')
}

# Output Control (OC) Package ---------------------------------------------
cat('# MODFLOW Output Control (OC) Package', file = paste0(outDirMODFLOW, modelName, '.oc'), append = F, sep = '\n')
cat(paste('# Data set created', Sys.Date(),'using MODFLOW Pre-Processing.R'), file = paste0(outDirMODFLOW, modelName, '.oc'), append = T, sep = '\n')
cat('#', file = paste0(outDirMODFLOW, modelName, '.oc'), append = T, sep = '\n')
cat('HEAD SAVE UNIT 30', file = paste0(outDirMODFLOW, modelName, '.oc'), append = T, sep = '\n')
cat('HEAD PRINT FORMAT 0', file = paste0(outDirMODFLOW, modelName, '.oc'), append = T, sep = '\n')
# cat('DRAWDOWN SAVE UNIT 31', file = paste0(outDirMODFLOW, modelName, '.oc'), append = T, sep = '\n')
# cat('DRAWDOWN PRINT FORMAT 0', file = paste0(outDirMODFLOW, modelName, '.oc'), append = T, sep = '\n')
cat('COMPACT BUDGET', file = paste0(outDirMODFLOW, modelName, '.oc'), append = T, sep = '\n')
for (i in 1:NPER){
  cat(paste0('PERIOD ',i,' STEP ',days_in_month(modelMonths[i])), file = paste0(outDirMODFLOW, modelName, '.oc'), append = T, sep = '\n')
  cat('SAVE HEAD', file = paste0(outDirMODFLOW, modelName, '.oc'), append = T, sep = '\n')
#  cat('SAVE DRAWDOWN', file = paste0(outDirMODFLOW, modelName, '.oc'), append = T, sep = '\n')
  cat('SAVE BUDGET', file = paste0(outDirMODFLOW, modelName, '.oc'), append = T, sep = '\n')
  cat('PRINT BUDGET', file = paste0(outDirMODFLOW, modelName, '.oc'), append = T, sep = '\n')
}

# Newton Solver (NWT) Package ----------------------------------------------------
cat('# MODFLOW-NWT Newton Solver (NWT) Package', file = paste0(outDirMODFLOW, modelName, '.nwt'), append = F, sep = '\n')
cat(paste('# Data set created', Sys.Date(),'using MODFLOW Pre-Processing.R'), file = paste0(outDirMODFLOW, modelName, '.nwt'), append = T, sep = '\n')
cat('#', file = paste0(outDirMODFLOW, modelName, '.nwt'), append = T, sep = '\n')
cat(paste(HEADTOL, FLUXTOL, MAXITEROUT, THICKFACT, LINMETH, IPRNWT,IBOTAV, 'COMPLEX  CONTINUE     ! HEADTOL FLUXTOL MAXITEROUT THICKFACT LINMETH IPRNWT IBOTAV OPTIONS', sep = '  '), file = paste0(outDirMODFLOW, modelName, '.nwt'), append = T, sep = '\n')


# Upstream Weighting (UPW) Package ----------------------------------------
LAYTYP = rep(0,NLAY)
LAYTYP[unconfined_layers] = 1

cat('# MODFLOW-NWT Upstream Weighting (UPW) Package', file = paste0(outDirMODFLOW, modelName, '.upw'), append = F, sep = '\n')
cat(paste('# Data set created', Sys.Date(),'using MODFLOW Pre-Processing.R'), file = paste0(outDirMODFLOW, modelName, '.upw'), append = T, sep = '\n')
cat('#', file = paste0(outDirMODFLOW, modelName, '.upw'), append = T, sep = '\n')
cat(paste(IUPWCB, HDRY, NPUPW, IPHDRY, '       # IUPWCB HDRY NPUPW IPHDRY'), file = paste0(outDirMODFLOW, modelName, '.upw'), append = T, sep = '\n')               
cat(paste(c(rep(1,3), rep(0,NLAY-3), '    ! LAYTYP'), collapse = '  '), file = paste0(outDirMODFLOW, modelName, '.upw'), append = T, sep = '\n')         # Specify if layers are confined or convertible
cat(paste(c(rep(0,NLAY),'    ! LAYAVG'), collapse = '  '), file = paste0(outDirMODFLOW, modelName, '.upw'), append = T, sep = '\n')         # Specify method of calculating interblock transmissivity
cat(paste(c(rep(0,NLAY),'    ! CHANI'), collapse = '  '), file = paste0(outDirMODFLOW, modelName, '.upw'), append = T, sep = '\n')          # Specify if horizontal anisotropy is defined by variable or for the entire layer
cat(paste(c(rep(-1,NLAY),'    ! LAYVKA'), collapse = '  '), file = paste0(outDirMODFLOW, modelName, '.upw'), append = T, sep = '\n')         # Specify if vertical anisotropy is defined by variable or for the entire layer
cat(paste(c(rep(0,NLAY),'    ! LAYWET'), collapse = '  '), file = paste0(outDirMODFLOW, modelName, '.upw'), append = T, sep = '\n')          # Specify if wetting is active (always 0 for MODFLOW-NWT)
HK_params = paste0('Kx_',seq(1:nZones))
HANI_params = paste0('HANI_',seq(1:nZones))
KVAR_params = paste0('KVAR_',seq(1:nZones))
SY_params = paste0('Sy_',seq(1:nZones))
SS_params = paste0('Ss_',seq(1:nZones))

# Write Kx parameters
for(i in 1:nZones){ 
  cat(paste(HK_params[i], 'HK', 1, NLAY), file = paste0(outDirMODFLOW, modelName, '.upw'), append = T, sep = '\n')          
  for (j in 1:NLAY){
    cat(paste0(j, '  NONE', '  LITHO',j,'  ',i), file = paste0(outDirMODFLOW, modelName, '.upw'), append = T, sep = '\n')      # layer, multiplier matrix, zone matrix, zone number    
  }
}

# Write horizontal anisotropy parameters
for(i in 1:nZones){
  cat(paste(HANI_params[i], 'HANI', 1, NLAY), file = paste0(outDirMODFLOW, modelName, '.upw'), append = T, sep = '\n')          
  for (j in 1:NLAY){
    cat(paste0(j, '  NONE', '  LITHO',j,'  ',i), file = paste0(outDirMODFLOW, modelName, '.upw'), append = T, sep = '\n')         
  }
}

# Write vertical anisotropy parameters
for(i in 1:nZones){
  cat(paste(KVAR_params[i], 'VANI', 1, NLAY), file = paste0(outDirMODFLOW, modelName, '.upw'), append = T, sep = '\n')          
  for (j in 1:NLAY){
    cat(paste0(j, '  NONE', '  LITHO',j,'  ',i), file = paste0(outDirMODFLOW, modelName, '.upw'), append = T, sep = '\n')          
  }
}

# Write Sy parameters
for(i in 1:nZones){
  cat(paste(SY_params[i], 'SY', 1, NLAY), file = paste0(outDirMODFLOW, modelName, '.upw'), append = T, sep = '\n')          
  for (j in 1:NLAY){
    cat(paste0(j, '  NONE', '  LITHO',j,' ',i), file = paste0(outDirMODFLOW, modelName, '.upw'), append = T, sep = '\n')          
  }
}

# Write Ss parameters
for(i in 1:nZones){
  cat(paste(SS_params[i], 'SS', 1, NLAY), file = paste0(outDirMODFLOW, modelName, '.upw'), append = T, sep = '\n')          
  for (j in 1:NLAY){
    cat(paste0(j, '  NONE', '  LITHO',j,'  ',i), file = paste0(outDirMODFLOW, modelName, '.upw'), append = T, sep = '\n')          
  }
}
# print codes for clusters
for (i in 1:(NPUPW*NLAY)) {
  cat('-1', file = paste0(outDirMODFLOW, modelName, '.upw'), append = T, sep = '\n')          
}

# Head Observation (HOB) Package () ------------------------------------------
if(all(unique(WL_obs.sf$well_name)== gridInfo_ObsWells$well_name) != T){
  stop('Ordering of well names in WL_obs.sf differs from gridInfo_ObsWells')
}

NH = nrow(WL_obs.sf)
MOBS = WL_obs.sf %>%
  filter(well_id%in%multiLayerObsWells) %>%
  nrow()
MAXM = well_layers %>% unlist() %>% max()
IUHOBSV = 31
ITT = 1

cat('# Head Observation (HOB) Package', file = paste0(outDirMODFLOW, modelName, '.hob'), append = F, sep = '\n')                                        # Data set 0
cat(paste('# Data set created', Sys.Date(),'using MODFLOW Pre-Processing.R'), file = paste0(outDirMODFLOW, modelName, '.hob'), append = T, sep = '\n')  # Data set 0
if(exists('ignore_well_id')){
  cat(paste(c('#  excluded well_ids with water level observations: ',ignore_well_id),collapse = ' '))                                                   # Data set 0
}
cat('#', file = paste0(outDirMODFLOW, modelName, '.hob'), append = T, sep = '\n')                                                                       # Data set 0
cat(paste(NH, MOBS, MAXM, IUHOBSV, -5555), file = paste0(outDirMODFLOW, modelName, '.hob'), append = T, sep = '\n')                                     # Data set 1
cat(TOMULTH , file = paste0(outDirMODFLOW, modelName, '.hob'), append = T, sep = '\n')                                                                  # Data set 2          
for(i in 1:nrow(gridInfo_ObsWells)){
  temp_data = WL_obs.sf %>%                           #filter data for well
    st_drop_geometry() %>%
    filter(well_id%in%gridInfo_ObsWells$well_id[i]) %>%
    mutate(water_level_elev_m = round(water_level_elev_m, digits = 2))
  nLyrs = well_layers[[i]] %>% length()             # determine how many layers the wells is screened in
  if (nLyrs==1 & nrow(temp_data)==1){               # Single layer well with one observation
    cat(paste(gridInfo_ObsWells$well_id[i], nLyrs, gridInfo_ObsWells$row_MF[i], gridInfo_ObsWells$col_MF[i],
              temp_data$IREFSP, temp_data$TOFFSET, gridInfo_ObsWells$ROFF[i], gridInfo_ObsWells$COFF[i],
              temp_data$water_level_elev_m, sep = '  ', collapse = '  '), file = paste0(outDirMODFLOW, modelName, '.hob'), append = T, sep = '\n')
    cat(paste(ITT), file = paste0(outDirMODFLOW, modelName, '.hob'), append = T, sep = '\n')
  } else if (nLyrs>1 & nrow(temp_data)==1) {       # Multiple layer well with one observation
    cat(paste(gridInfo_ObsWells$well_id[i], -nLyrs, gridInfo_ObsWells$row_MF[i], gridInfo_ObsWells$col_MF[i],
              temp_data$IREFSP, temp_data$TOFFSET, gridInfo_ObsWells$ROFF[i], gridInfo_ObsWells$COFF[i],
              temp_data$water_level_elev_m, sep = '  ', collapse = '  '), file = paste0(outDirMODFLOW, modelName, '.hob'), append = T, sep = '\n')
    cat(paste(well_layers[[i]], 1/nLyrs, sep = '  ', collapse = '  '), file = paste0(outDirMODFLOW, modelName, '.hob'), append = T, sep = '\n')
    cat(paste(ITT), file = paste0(outDirMODFLOW, modelName, '.hob'), append = T, sep = '\n')
  } else if (nLyrs==1 & nrow(temp_data)>1) {       # Single layer well with multiple observations
    cat(paste(gridInfo_ObsWells$well_id[i], nLyrs, gridInfo_ObsWells$row_MF[i], gridInfo_ObsWells$col_MF[i],
              -nrow(temp_data), temp_data$TOFFSET[1], gridInfo_ObsWells$ROFF[i], gridInfo_ObsWells$COFF[i],
              temp_data$water_level_elev_m[1], sep = '  ', collapse = '  '), file = paste0(outDirMODFLOW, modelName, '.hob'), append = T, sep = '\n')
    cat(paste(ITT), file = paste0(outDirMODFLOW, modelName, '.hob'), append = T, sep = '\n')
    for (j in 1:nrow(temp_data)){
      cat(paste(paste0(gridInfo_ObsWells$well_id[i],'_',j),temp_data$IREFSP[j], temp_data$TOFFSET[j], temp_data$water_level_elev_m[j], sep = '  ', collapse = '  '), file = paste0(outDirMODFLOW, modelName, '.hob'), append = T, sep = '\n')
    }
  } else if (nLyrs>1 & nrow(temp_data)>1) {       # Multiple layer well with multiple observations
    cat(paste(gridInfo_ObsWells$well_id[i], -nLyrs, gridInfo_ObsWells$row_MF[i], gridInfo_ObsWells$col_MF[i],
              -nrow(temp_data), temp_data$TOFFSET[1], gridInfo_ObsWells$ROFF[i], gridInfo_ObsWells$COFF[i],
              temp_data$water_level_elev_m[1], sep = '  '), file = paste0(outDirMODFLOW, modelName, '.hob'), append = T, sep = '\n')
    cat(paste(well_layers[[i]], 1/nLyrs, sep ='  ', collapse = '  '), file = paste0(outDirMODFLOW, modelName, '.hob'), append = T, sep = '\n')
    cat(paste(ITT), file = paste0(outDirMODFLOW, modelName, '.hob'), append = T, sep = '\n')
    for (j in 1:nrow(temp_data)){
      cat(paste(paste0(gridInfo_ObsWells$well_id[i],'_',j),temp_data$IREFSP[j], temp_data$TOFFSET[j], temp_data$water_level_elev_m[j], sep = '  ', collapse = '  '), file = paste0(outDirMODFLOW, modelName, '.hob'), append = T, sep = '\n')
    }
  }
}


# Gage (GAGE) Package -----------------------------------------------------
cat(paste0('  ', nrow(SFR_gages)), file = paste0(outDirMODFLOW, modelName, '.gage'), append = F, sep = '\n')
write.table(SFR_gages %>% dplyr::select(-out_file), file = paste0(outDirMODFLOW, modelName, '.gage'), append = T, row.names = F, col.names = F, quote = F, sep = '  ', eol = '\n')

# Well (WEL) Package ------------------------------------------------------
NPWEL = length(MFR_seg_nums) 
MXL = nrow(MFR_cell_info)
PHIRAMP = 0.05
FLOWRATETEMP = 9999
MFR_params = data.frame(param_name = paste0('MFR_',MFR_seg_nums),
                        Seg_Num = MFR_seg_nums)

cat('# Well (WEL) Package', file = paste0(outDirMODFLOW, modelName, '.wel'), append = F, sep = '\n')
cat(paste('# Data set created', Sys.Date(),'using MODFLOW Pre-Processing.R'), file = paste0(outDirMODFLOW, modelName, '.wel'), append = T, sep = '\n')
cat('#', file = paste0(outDirMODFLOW, modelName, '.wel'), append = T, sep = '\n')
cat(paste('PARAMETER',NPWEL, MXL, sep = '  '), file = paste0(outDirMODFLOW, modelName, '.wel'), append = T, sep = '\n')
cat(paste(MXL,IUPWCB, sep = '  '), file = paste0(outDirMODFLOW, modelName, '.wel'), append = T, sep = '\n')
cat(paste('SPECIFY',PHIRAMP, IUNITRAMP, sep = '  '), file = paste0(outDirMODFLOW, modelName, '.wel'), append = T, sep = '\n')

for(i in 1:nrow(MFR_params)){
  MFR_temp = MFR_cells %>% 
    filter(MFR_Seg==MFR_seg_nums[i]) %>%
    relocate(Layer, row_MF, col_MF, )
  param_name = paste0('MFR_',i)
  NLST = MFR_cell_info %>% filter(MFR_Seg == i) %>% nrow()
  #NLST = sum(MFR_cells_summary$count[MFR_cells_summary$MFR_Seg==MFR_seg_nums[i]])
  MFR_seg_info = MFR_cell_info %>%
    filter(MFR_Seg == MFR_params$Seg_Num[i]) %>%
    mutate(Qfact = round(Qfact, 6)) %>%
    dplyr::select(-MFR_Seg) %>%
    arrange(Layer, row_MF, col_MF)
  
  cat(paste(MFR_params$param_name[i],'Q',FLOWRATETEMP, NLST, sep = '  '), file = paste0(outDirMODFLOW, modelName, '.wel'), append = T, sep = '\n')
  write.table(MFR_seg_info, file = paste0(outDirMODFLOW, modelName, '.wel'), sep = '  ', append = T, row.names = F, col.names = F, quote = F, eol = '\n')
}
for (i in 1:NPER){
  cat('0  6', file = paste0(outDirMODFLOW, modelName, '.wel'), append = T, sep = '\n')
  cat(paste(MFR_params$param_name, sep = '  '), file = paste0(outDirMODFLOW, modelName, '.wel'), append = T, sep = '\n')
}


# Parameter Value (PVAL) File ------------------------------------------------------
pvals = data.frame(Parameter =  c(HK_params, HANI_params, KVAR_params, SY_params, SS_params,MFR_params$param_name),
                   Value = c(100, 20, 0.02, 0.0001, 1.00E-09, 5, 150, 150,                # Kx for Zones 1-8
                             1, 1, 1, 1, 1, 1, 20, 20,                                    # HANI for Zones 1-8
                             1, 10, 100, 100, 1, 25, 1, 1,                                 # KVAR for Zones 1-8
                             0.3, 0.15, 0.1, 0.05, 0.001, 0.1, 0.2, 0.2,                   # Sy for Zones 1-8
                             1E-05, 1E-4, 2E-4, 2E-3, 1E-07, 1E-4, 1E-4, 1E-4,               # Ss for Zones 1-8
                             0.5*MFR_avg_flux$average_recharge_m3day %>% round(0)))                # MFR Segment Flux

cat(nrow(pvals), file = paste0(outDirMODFLOW, modelName, '.pval'), append = F, sep = '\n')
write.table(pvals, file = paste0(outDirMODFLOW, modelName, '.pval'), sep = '\t', append = T, row.names = F, col.names = F, quote = F, eol = '\n')

# Recharge (RCH) Package --------------------------------------------------
cat('# Recharge (RCH) File', file = paste0(outDirMODFLOW, modelName, '.rch'), append = F, sep = '\n')
cat(paste('# Data set created', Sys.Date(),'using MODFLOW Pre-Processing.R'), file = paste0(outDirMODFLOW, modelName, '.rch'), append = T, sep = '\n')
cat('#', file = paste0(outDirMODFLOW, modelName, '.rch'), append = T, sep = '\n')
cat('1  50', file = paste0(outDirMODFLOW, modelName, '.rch'), append = T, sep = '\n')
for (i in 1:NPER){
  cat('1', file = paste0(outDirMODFLOW, modelName, '.rch'), append = T, sep = '\n')
  cat(paste0('OPEN/CLOSE .\\recharge\\recharge_SP',i,'.txt 1 (FREE) -1'), file = paste0(outDirMODFLOW, modelName, '.rch'), append = T, sep = '\n')
}

# Evapotranspiration Segments (ETS) Package -------------------------------
ETo = read.table(ETo_file, header = T) %>%
  mutate(Date = as.Date(Date),
         monthYear = format(Date, '%b-%Y')) %>%
  group_by(monthYear) %>%
  summarize(ETo_avg_m = mean(ETo_m) %>% round(4)) %>%
  mutate(Date = as.Date(paste0('01-',monthYear),'%d-%b-%Y')) %>%
  arrange(Date) %>%
  filter(Date%in%modelMonths)

layer_tops$L1_top_z - 1 %>%                 # create matrix for ET extinction depth
  matrix(nrow = NROW, ncol = NCOL) %>%
  write.table(file = paste0(outDirMODFLOW, 'ET/ETo_ETSX.txt'), row.names = F, col.names = F, quote = F, sep = '\t', eol = '\n')

matrix(0.5, nrow = NROW, ncol = NCOL) %>%  # create matrix for defining ET extinction curve (or line if NETSEG = 2)
  write.table(file = paste0(outDirMODFLOW, 'ET/ETo_PXDP.txt'), row.names = F, col.names = F, quote = F, sep = '\t', eol = '\n')

matrix(0.5, nrow = NROW, ncol = NCOL) %>%  # create matrix for defining ET extinction curve (or line if NETSEG = 2)
  write.table(file = paste0(outDirMODFLOW, 'ET/ETo_PETM.txt'), row.names = F, col.names = F, quote = F, sep = '\t', eol = '\n')
  

cat('# Evapotranspiration Segments (ETS) File', file = paste0(outDirMODFLOW, modelName, '.ets'), append = F, sep = '\n')
cat(paste('# Data set created', Sys.Date(),'using MODFLOW Pre-Processing.R'), file = paste0(outDirMODFLOW, modelName, '.ets'), append = T, sep = '\n')
cat('#', file = paste0(outDirMODFLOW, modelName, '.ets'), append = T, sep = '\n')
cat(paste(NETSOP, IETSCB, NPETS, NETSEG, '   ! NETSOP IETSCB NPETS NETSEG', sep = '  '), file = paste0(outDirMODFLOW, modelName, '.ets'), append = T, sep = '\n')
for (i in 1:NPER){
  ETo %>% 
    filter(Date == modelMonths[i]) %>%
    dplyr::select(ETo_avg_m) %>%
    matrix(nrow = NROW, ncol = NCOL) %>%
    write.table(file = paste0(outDirMODFLOW, 'ET/ETo_avg_SP',i,'.txt'), row.names = F, col.names = F, quote = F, sep = '\t', eol = '\n')
  if(i==1){
    cat('1  1  1  1  1     ! INETSS  INETSR  INETSX  INIETS  INSGDF', file = paste0(outDirMODFLOW, modelName, '.ets'), append = T, sep = '\n')
    cat('OPEN/CLOSE  .\\layer_z\\L1_top_z.txt  1  (FREE)  -1', file = paste0(outDirMODFLOW, modelName, '.ets'), append = T, sep = '\n')
    cat(paste0('OPEN/CLOSE  .\\ET\\ETo_avg_SP',i,'.txt  1  (FREE)  -1'), file = paste0(outDirMODFLOW, modelName, '.ets'), append = T, sep = '\n')
    cat('OPEN/CLOSE  .\\ET\\ETo_ETSX.txt  1  (FREE)  -1', file = paste0(outDirMODFLOW, modelName, '.ets'), append = T, sep = '\n')
    cat('OPEN/CLOSE  .\\ET\\ETo_PXDP.txt  1  (FREE)  -1', file = paste0(outDirMODFLOW, modelName, '.ets'), append = T, sep = '\n')
    cat('OPEN/CLOSE  .\\ET\\ETo_PETM.txt  1  (FREE)  -1', file = paste0(outDirMODFLOW, modelName, '.ets'), append = T, sep = '\n')
  } else {
    cat('-1  1  -1  -1  -1     ! INETSS  INETSR  INETSX  INIETS  INSGDF', file = paste0(outDirMODFLOW, modelName, '.ets'), append = T, sep = '\n')
    cat(paste0('OPEN/CLOSE .\\ET\\ETo_avg_SP',i,'.txt 1 (FREE) -1'), file = paste0(outDirMODFLOW, modelName, '.ets'), append = T, sep = '\n')
  }
}


# Multi-Node Well Information (MNWI) Package ------------------------------
cat(paste(WEL1flag, QSUMflag, BYNDflag, sep = '  '), file = paste0(outDirMODFLOW, modelName, '.mnwi'), append = F, sep = '\n')
cat(length(MNWI_output_wells), file = paste0(outDirMODFLOW, modelName, '.mnwi'), append = T, sep = '\n')
for(i in 1:length(MNWI_output_wells)){
  cat(paste(MNWI_output_wells[i],  103+i,  '1  0'), file = paste0(outDirMODFLOW, modelName, '.mnwi'), append = T, sep = '\n')
}
# Name (NAM) File ------------------------------------------------------
cat(paste0('LIST 10 ',modelName,'.lst'), file = paste0(outDirMODFLOW, modelName, '.nam'), append = F, sep = '\n')
cat(paste0('BAS6 11 ',modelName,'.bas'), file = paste0(outDirMODFLOW, modelName, '.nam'), append = T, sep = '\n')
cat(paste0('DIS 12 ',modelName,'.dis'), file = paste0(outDirMODFLOW, modelName, '.nam'), append = T, sep = '\n')
cat(paste0('UPW 13 ',modelName,'.upw'), file = paste0(outDirMODFLOW, modelName, '.nam'), append = T, sep = '\n')
cat(paste0('ZONE 14 ',modelName,'.zone'), file = paste0(outDirMODFLOW, modelName, '.nam'), append = T, sep = '\n')
cat(paste0('SFR 15 ',modelName,'.sfr'), file = paste0(outDirMODFLOW, modelName, '.nam'), append = T, sep = '\n')
cat(paste0('GAGE 16 ',modelName,'.gage'), file = paste0(outDirMODFLOW, modelName, '.nam'), append = T, sep = '\n')
cat(paste0('WEL 17 ',modelName,'.wel'), file = paste0(outDirMODFLOW, modelName, '.nam'), append = T, sep = '\n')
cat(paste0('MNW2 18 ',modelName,'.mnw2'), file = paste0(outDirMODFLOW, modelName, '.nam'), append = T, sep = '\n')
cat(paste0('MNWI 19 ',modelName,'.mnwi'), file = paste0(outDirMODFLOW, modelName, '.nam'), append = T, sep = '\n')
cat(paste0('RCH 20 ',modelName,'.rch'), file = paste0(outDirMODFLOW, modelName, '.nam'), append = T, sep = '\n')
cat(paste0('PVAL 21 ',modelName,'.pval'), file = paste0(outDirMODFLOW, modelName, '.nam'), append = T, sep = '\n')
cat(paste0('OC 22 ',modelName,'.oc'), file = paste0(outDirMODFLOW, modelName, '.nam'), append = T, sep = '\n')
cat(paste0('NWT 23 ',modelName,'.nwt'), file = paste0(outDirMODFLOW, modelName, '.nam'), append = T, sep = '\n')
cat(paste0('ETS 24 ',modelName,'.ets'), file = paste0(outDirMODFLOW, modelName, '.nam'), append = T, sep = '\n')
cat(paste0('HOB 25 ',modelName,'.hob'), file = paste0(outDirMODFLOW, modelName, '.nam'), append = T, sep = '\n')
cat(paste0('DATA(BINARY) 30  ',modelName,'.hds'), file = paste0(outDirMODFLOW, modelName, '.nam'), append = T, sep = '\n')
cat(paste0('DATA ',IUHOBSV,' ',modelName,'_HOB_out.dat'), file = paste0(outDirMODFLOW, modelName, '.nam'), append = T, sep = '\n')
# cat(paste0('DATA(BINARY) 32  ',modelName,'.ddn'), file = paste0(outDirMODFLOW, modelName, '.nam'), append = T, sep = '\n')
cat(paste0('DATA(BINARY) 50  ',modelName,'.cbb'), file = paste0(outDirMODFLOW, modelName, '.nam'), append = T, sep = '\n')
cat(paste0('DATA ',IUNITRAMP,' ',modelName,'_WEL_Reduced.dat'), file = paste0(outDirMODFLOW, modelName, '.nam'), append = T, sep = '\n')
if(WEL1flag>0){cat(paste0('DATA ',WEL1flag,' ',modelName,'_MNW_Wel1.dat'), file = paste0(outDirMODFLOW, modelName, '.nam'), append = T, sep = '\n')}
if(QSUMflag>0){cat(paste0('DATA ',QSUMflag,' ',modelName,'_MNW_QSUM.dat'), file = paste0(outDirMODFLOW, modelName, '.nam'), append = T, sep = '\n')}
if(BYNDflag>0){cat(paste0('DATA ',BYNDflag,' ',modelName,'_MNW_BYND.dat'), file = paste0(outDirMODFLOW, modelName, '.nam'), append = T, sep = '\n')}
for(i in 1:length(MNWI_output_wells)){
  cat(paste0('DATA ', 103+i, ' SVHSM_MNW_',MNWI_output_wells[i],'.dat'), file = paste0(outDirMODFLOW, modelName, '.nam'), append = T, sep = '\n')
}
cat(paste0('DATA ',ISTCB2,' ',modelName,'_Streamflow_Global.dat'), file = paste0(outDirMODFLOW, modelName, '.nam'), append = T, sep = '\n')
for(i in 1:nrow(SFR_gages)){
  cat(paste0('DATA ',SFR_gages$UNIT,' ',SFR_gages$out_file), file = paste0(outDirMODFLOW, modelName, '.nam'), append = T, sep = '\n')
}
# Zone Budget File --------------------------------------------------------

for (i in 1:NLAY){
  temp = ZoneBud.sf %>% 
    st_drop_geometry() %>%
    dplyr::select(paste0('L',i,'_ZB_ID')) %>%
    unlist() %>%
    matrix(nrow = NROW, ncol = NCOL, byrow = T)
  
  if(i==1){
    cat(paste(NLAY, NROW, NCOL, sep = '  '),file = paste0(outDirMODFLOW, modelName, '.zonbud'), append = F, sep = '\n')
  }
    cat(paste0('INTERNAL  ()  -1'),file = paste0(outDirMODFLOW, modelName, '.zonbud'), append = T, sep = '\n')
    write.table(temp, file = paste0(outDirMODFLOW, modelName, '.zonbud'), append = T, quote = F, sep = '  ', row.names = F, col.names = F, eol = '\n', )
}

