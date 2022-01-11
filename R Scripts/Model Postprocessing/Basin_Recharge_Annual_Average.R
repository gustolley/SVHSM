# General Comments --------------------------------------------------------

# User Input --------------------------------------------------------------
PRMS_monthly_rch_file = 'recharge.monthly'
SWBM_rch_dir = 'recharge/'
GISdir = 'GIS/'
outDirGIS = paste0(GISdir, 'Model_Results/WY2000_2021/')

WYstart = 2000                                   # Beginning water year of simulation
WYend = 2020                                     # Ending water year of simulation
PRMS_nrows = 599
PRMS_ncols = 484
SWBM_nrows = 216
SWBM_ncols = 243

PRMSGrid = 'grid_100m_35deg_cc.shp'
MFGrid = 'SVHSM_grid_data.shp'

# Script Initialization ---------------------------------------------------

options(warn=-1)   # suppress warnings (set to 0 to turn warnings on)
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggthemes)
library(plotly)
library(RColorBrewer)
library(cowplot)
library(sf)
dir.create(resultsDir)   #Create Results directory if it doesn't exist

# Create Date Arrays ------------------------------------------------------
WYstartDate = paste0(WYstart-1,'-10-01') %>% as.Date()
WYendDate = paste0(WYend,'-09-30') %>% as.Date()
modelDays = seq.Date(from = WYstartDate, to = WYendDate, by = 'day')
modelMonths = seq.Date(from = WYstartDate, to = WYendDate, by = 'month')
modelYears = (WYend - WYstart) + 1
nmonths = length(modelMonths)

# Read in Model Grid Shapefiles
PRMSGrid.sf = st_read(PRMSGrid)

MFGrid.sf = st_read(MFGrid) %>%
  dplyr::select(row_MF, col_MF, MF_idx, cell_type, row_global, col_global, global_id)


# Read in PRMS Recharge
rch_idx = readLines(PRMS_monthly_rch_file) %>% grep(x = ., pattern = format(modelMonths[13], '%Y/%m'))

for (i in 1:(length(modelMonths)-12)){
  if (i==1){
    PRMS_total = array(data = 0, dim = c(PRMS_nrows, PRMS_ncols))
  }
  
  PRMS_temp = read.table(PRMS_monthly_rch_file, header = F, skip = rch_idx, nrows = PRMS_nrows) %>% as.matrix()
  PRMS_total = PRMS_total + PRMS_temp
  
  rch_idx = rch_idx + PRMS_nrows + 3
  
  if (i == (length(modelMonths)-12)){
    PRMS_avg = PRMS_total / (modelYears - 1)
  }
}
PRMSGrid.sf = PRMSGrid.sf %>%
  mutate(avg_rch = PRMS_avg %>% t() %>% unlist() %>% as.vector())
PRMSGrid.sf$avg_rch[PRMSGrid.sf$cell_type=='Inactive'] = NA

st_write(PRMSGrid.sf, paste0(outDirGIS, 'PRMS_ann_avg_rch.shp'), delete_dsn = T, driver = 'ESRI Shapefile')

# Read in SWBM Recharge
for (i in 13:length(modelMonths)){
  if(i==13){
    SWBM_total = array(data = 0, dim = c(SWBM_nrows, SWBM_ncols))
  }
  SWBM_temp = read.table(paste0(SWBM_rch_dir, 'recharge_SP',i,'.txt'), header = F) %>% as.matrix()
  
  SWBM_total = SWBM_total + SWBM_temp
  if (i == length(modelMonths)){
    SWBM_avg = SWBM_total / (modelYears - 1)
  }
  
}
MFGrid.sf = MFGrid.sf %>%
  mutate(avg_rch = SWBM_avg %>% t() %>% unlist() %>% as.vector())
MFGrid.sf$avg_rch[MFGrid.sf$cell_type!='MODFLOW'] = NA
st_write(MFGrid.sf, paste0(outDirGIS, 'SWBM_ann_avg_rch.shp'), delete_dsn = T)

