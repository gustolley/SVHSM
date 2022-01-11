# Leapfrog_Inputs.R
# Writes collar and lithology input data files for Leapfrog geologic modeling software
# Created by Gus Tolley April 29, 2020

# User Input --------------------------------------------------------------
dbName ='SierraValley'                                              # name of GLA database
myServer=''                                                         # GLA server location
userName=''                                                         # GLA SQL server username
pwd=''                                                              # GLA SQL server password
outDir = 'Leapfrog Geologic Model/'                                 # location where output will be saved
convert_2_m = T                                                     # Convert all linear units from ft to m
CRS_EPSG = 26910                                                    # Numeric EPSG value for desired coordinate system

Include_Modeled_Wells = T                                           # Includes simulated pumping wells with no lithology data
modeled_ag_wells = 'SWBM/input_150m_grid/ag_well_summary.txt'                                                 
modeled_muni_wells ='SWBM/input_150m_grid/muni_well_summary.txt'
Include_Obs_Wells = T                                               #Includes simulated observation wells with no lithology data
modeled_obs_wells = 'MODFLOW/SVHSM_Observation_Well_Info.dat'

# Initialize Script -------------------------------------------------------
library(RODBC)
library(dplyr)
library(sf)
date.current = format(Sys.Date(), '%Y%m%d')

# Functions----------------------------------------------------------------
'%!in%' <- function(x,y)!('%in%'(x,y))

# Create RODBC Connection -------------------------------------------------
DB <- odbcDriverConnect(connection = paste0('DRIVER={SQL Server}; 
                                 server=',myServer,';
                                 database=',dbName,';
                                 uid=',userName,';
                                 pwd=',pwd))

MP_tbl = sqlFetch(DB, 'wells', stringsAsFactors=FALSE) # extract monitoring points table
Lith_tbl = sqlFetch(DB, 'lithology', stringsAsFactors=FALSE) # extract monitoring points table
well_names = MP_tbl %>%
  dplyr::select(well_id, well_name)

# Extract Lithology Data --------------------------------------------------
lithology = Lith_tbl %>%
  left_join(well_names, by = 'well_id') %>%
  dplyr::select(well_id, well_name, interval_start, interval_end, interval_units, log_description, lithology_type, lithology_texture, lithology_color, lithology_model) 

if (convert_2_m){                                                  #convert well surface elevation and total depth from ft to m
  lithology = lithology %>%
    mutate(interval_start = if_else(interval_units=='ft',true =  interval_start*0.3048, interval_start),
           interval_end = if_else(interval_units=='ft', interval_end*0.3048, interval_end),
           interval_units = 'm')
}

write.table(x = lithology, file = paste0(outDir, 'Sierra_Valley_Lithology',date.current,'.dat'), append = F, quote =  F, sep = '\t', row.names = F, col.names = T, na = '')

# Extract Collar Data -----------------------------------------------------
collar = MP_tbl  %>%                # extract monitoring points table
  filter(well_id%in%lithology$well_id) %>%
  dplyr::select(well_id, well_name, well_lat, well_long, well_installation_date, well_total_depth_ft,
                elev_ground_surface, well_log, well_notes) %>%
  st_as_sf(coords = c('well_long', 'well_lat'), crs = 4269) %>%
  st_transform(CRS_EPSG) %>%
  mutate(X = st_coordinates(.)[,1],                        
         Y = st_coordinates(.)[,2]) %>%
  st_drop_geometry() %>%
  relocate(c(well_id, well_name, X, Y, well_installation_date, elev_ground_surface,
             well_total_depth_ft, well_log, well_notes))

if(Include_Modeled_Wells){
  if(exists('modeled_ag_wells') & exists('modeled_muni_wells')){         # if ag and muni wells are simulated
    modeled_wells = read.table(modeled_ag_wells, header = T) %>%
      rbind(read.table(modeled_muni_wells, header = T))
  } else if (exists('modeled_ag_wells') & !exists('modeled_muni_wells')){  # if only ag wells are simulated
    modeled_wells = read.table(modeled_ag_wells, header = T)
  } else if(!exists('modeled_ag_wells') & exists('modeled_muni_wells')){   # if only muni wells are simulated
    modeled_wells = read.table(modeled_muni_wells, header = T)
  }
  modeled_wells = modeled_wells %>%
    mutate(well_name = gsub(pattern = '_', replacement = ' ', x = well_name),          # change well names back (modified for MODFLOW input requirements)
           well_name = gsub(pattern = 'and', replacement = '&', x = well_name),
           well_name = gsub(pattern = 'old', replacement = '(old)', x = well_name))
  
  collar_append = MP_tbl  %>%                # extract monitoring points table
    filter(well_id%in%modeled_wells$well_id, !well_id%in%collar$well_id) %>%
    dplyr::select(well_id, well_name, well_lat, well_long, well_installation_date, well_total_depth_ft,
                  elev_ground_surface, well_log, well_notes) %>%
    st_as_sf(coords = c('well_long', 'well_lat'), crs = 4269) %>%
    st_transform(CRS_EPSG) %>%
    mutate(X = st_coordinates(.)[,1],                        
           Y = st_coordinates(.)[,2]) %>%
    st_drop_geometry() %>%
    relocate(c(well_id, well_name, X, Y, well_installation_date, elev_ground_surface,
               well_total_depth_ft, well_log, well_notes))
  
  collar = rbind(collar, collar_append)
}

if (convert_2_m){                                                  #convert well surface elevation and total depth from ft to m
  collar = collar %>%
    mutate(elev_ground_surface = elev_ground_surface*0.3048,
           well_total_depth_m = well_total_depth_ft*0.3048)
}

if(Include_Obs_Wells){                                             # include observation wells (assumes unit conversions have already been made)
  obs_wells = read.table(modeled_obs_wells, header = T, sep = '\t') %>%
    filter(well_id%!in%collar$well_id) %>%
    dplyr::select(well_id, elev_ground_surface, well_total_depth_m, TOS_dpth_sim_m, BOS_dpth_sim_m) %>%
    left_join(MP_tbl %>% dplyr::select(well_id, well_name,  well_log, well_notes, well_installation_date, well_lat, well_long)) %>%
    st_as_sf(coords = c('well_long', 'well_lat'), crs = 4269) %>%
    st_transform(crs = 26910) %>%
    mutate(X = st_coordinates(.)[,1],                        
           Y = st_coordinates(.)[,2],
           well_total_depth_ft = well_total_depth_m*3.28 %>% round(2)) %>%
    relocate(well_id, well_name, X, Y, well_installation_date, elev_ground_surface, 
             well_total_depth_ft, well_log, well_notes, well_total_depth_m) %>%
    st_drop_geometry()
  
  collar = rbind(collar, obs_wells %>% dplyr::select(-TOS_dpth_sim_m, -BOS_dpth_sim_m))
}

write.table(x = collar, file = paste0(outDir, 'Sierra_Valley_Collar_',date.current,'.dat'), append = F, quote =  F, sep = '\t', row.names = F, col.names = T, na = '')

# Write screen interval data ----------------------------------------------
screen_tbl = MP_tbl %>%
  filter(well_id%in%collar$well_id) %>%
  dplyr::select(well_id, well_name, top_of_screen_depth_ft, bottom_of_screen_depth_ft) %>%
  left_join(collar)

if (convert_2_m){                                                  #convert well surface elevation and total depth from ft to m
  screen_tbl = screen_tbl %>%
    mutate(top_of_screen_depth_m = top_of_screen_depth_ft*0.3048,
           bottom_of_screen_depth_m = bottom_of_screen_depth_ft*0.3048)
}

if(Include_Modeled_Wells){
  screen_tbl = screen_tbl %>%
    left_join(modeled_wells) %>%
    mutate(top_of_screen_depth_m = if_else(is.na(top_of_screen_depth_m), elev_ground_surface-top_scrn_z, top_of_screen_depth_m),
           bottom_of_screen_depth_m = if_else(is.na(bottom_of_screen_depth_m), elev_ground_surface-bot_scrn_z, bottom_of_screen_depth_m))
}
if(Include_Obs_Wells){
  test = screen_tbl %>%
    left_join(obs_wells) %>%
    mutate(top_of_screen_depth_m = if_else(is.na(top_of_screen_depth_m), TOS_dpth_sim_m, top_of_screen_depth_m),
           bottom_of_screen_depth_m = if_else(is.na(bottom_of_screen_depth_m), BOS_dpth_sim_m, bottom_of_screen_depth_m))
}

screen_tbl = screen_tbl %>%
  dplyr::select(well_id, well_name, top_of_screen_depth_m, bottom_of_screen_depth_m) %>%
  filter(!is.na(top_of_screen_depth_m), !is.na(bottom_of_screen_depth_m))

write.table(screen_tbl, file = paste0(outDir, 'Sierra_Valley_Screens_',date.current,'.dat'), append = F, quote =  F, sep = '\t', row.names = F, col.names = T, na = '')

# Wells Needing Processing ------------------------------------------------
processed_logs = MP_tbl %>% 
  filter(well_id%in%lithology$well_id) %>%
  dplyr::select(well_id, well_name, well_log)

Wells_2_Process = MP_tbl  %>%                
  filter(well_lat != -9999, !is.na(well_log), xy_survey_method != 'Derived from TRS', 
         well_id%!in%lithology$well_id, well_log%!in%processed_logs$well_log) %>%
  dplyr::select(well_id, well_wcr, well_name, well_log) %>%
  mutate(Lithology_in_DB = 'No') %>%
  write.table(file = paste0(outDir,'Well_Logs_2_Process.dat'), quote = F, sep = '\t', row.names = F)




