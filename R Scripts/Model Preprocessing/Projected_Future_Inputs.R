# Projected_Future_Inputs.R

# Script for generating future climate and streamflow inputs
# DWR change factors for precip, ET, and streamflow are used


# User Input --------------------------------------------------------------
Future_WYstart = 2021
Future_WYend = 2070


# Initialize Script -------------------------------------------------------
library(dplyr)
library(lubridate)
library(plotly)
library(ggplot2)
library(ggthemes)
library(tidyr)
library(RColorBrewer)
library(cowplot)


# Functions ---------------------------------------------------------------
Group_WY = function(df, grp_col){
  for(i in 1:nrow(df)){
    if (i==1){
      c = 1
      df = df %>%
        mutate(group = NA)
      df$group[1] = c
    } else {
      if (df[[grp_col]][i] != df[[grp_col]][i-1]){
        c = c+1
      }
      df$group[i] = c
    }
  }
  return(df)
}

# Create Date Arrays ------------------------------------------------------
Future_WYstartDate = paste0(Future_WYstart-1,'-10-01') %>% as.Date()
Future_WYendDate = paste0(Future_WYend,'-09-30') %>% as.Date()
FutureModelDays = seq.Date(from = Future_WYstartDate, to = Future_WYendDate, by = 'day')
FutureModelDays = FutureModelDays[!(format(FutureModelDays,"%m") == "02" & format(FutureModelDays, "%d") == "29"), drop = FALSE]
FutureModelMonths = seq.Date(from = Future_WYstartDate, to = Future_WYendDate, by = 'month')
NPER = length(FutureModelMonths)
nModelYears = (Future_WYend - Future_WYstart) + 1


# Years for Future Simulation
set.seed(741)        # Used to recreate random selection of water years
Future_WYs = runif(50,1990,2011) %>% floor()

Hist_Future_WYs = data.frame(WY = Future_WYs,
                             Future_WY = seq(2021,2070))

# Create WY Type Files For Plotting
WY_Types_no_change = read.csv('DWR_Sac_Valley_WYIdx.csv', header = T) %>%
  mutate(Type = case_when(
    Type =='W' ~ 'Wet',
    Type == 'AN' ~ 'Above Normal',
    Type == 'BN' ~ 'Below Normal',
    Type == 'D' ~ 'Dry',
    Type == 'C' ~ 'Dry')) %>%
  left_join(Hist_Future_WYs) %>%
  filter(Future_WY >= Future_WYstart, Future_WY <= Future_WYend) %>%
  arrange(Future_WY) %>%
  Group_WY(df = ., grp_col = 'Type') %>%
  group_by(group) %>%
  summarize(WY_Start = min(Future_WY), WY_End = max(Future_WY), Type = Type) %>%
  slice_head() %>%
  ungroup() %>%
  filter(Type%in%c('Critical', 'Dry', 'Wet')) %>%
  dplyr::select(WY_Start, WY_End, Type)

WY_Types_2030 = read.csv('CentralValley_2030_WYT.csv', header = T) %>%
  filter(Valley=='Sacramento') %>%
  mutate(DWR_2030_Projection_WYT = gsub(x = DWR_2030_Projection_WYT, pattern = 'Critical', replacement = 'Dry')) %>%
  left_join(Hist_Future_WYs) %>%
  filter(Future_WY >= Future_WYstart, Future_WY <= Future_WYend) %>%
  arrange(Future_WY) %>%
  Group_WY(df = ., grp_col = 'DWR_2030_Projection_WYT') %>%
  group_by(group) %>%
  summarize(WY_Start = min(Future_WY), WY_End = max(Future_WY), Type = DWR_2030_Projection_WYT) %>%
  slice_head() %>%
  ungroup() %>%
  filter(Type%in%c('Critical', 'Dry', 'Wet')) %>%
  dplyr::select(WY_Start, WY_End, Type)

WY_Types_2070 = read.csv('CentralValley_2070_WYT.csv', header = T) %>%
  filter(Valley=='Sacramento') %>%
  mutate(DWR_2070_Projection_WYT = gsub(x = DWR_2070_Projection_WYT, pattern = 'Critical', replacement = 'Dry')) %>%
  left_join(Hist_Future_WYs) %>%
  filter(Future_WY >= Future_WYstart, Future_WY <= Future_WYend) %>%
  arrange(Future_WY) %>%
  Group_WY(df = ., grp_col = 'DWR_2070_Projection_WYT') %>%
  group_by(group) %>%
  summarize(WY_Start = min(Future_WY), WY_End = max(Future_WY), Type = DWR_2070_Projection_WYT) %>%
  slice_head() %>%
  ungroup() %>%
  filter(Type%in%c('Critical', 'Dry', 'Wet')) %>%
  dplyr::select(WY_Start, WY_End, Type)

WY_Types_2070DEW = read.csv('CentralValley_2070DEW_WYT.csv', header = T) %>%
  filter(Valley=='Sacramento') %>%
  mutate(DWR_2070DEW_Projection_WYT = gsub(x = DWR_2070DEW_Projection_WYT, pattern = 'Critical', replacement = 'Dry')) %>%
  left_join(Hist_Future_WYs) %>%
  filter(Future_WY >= Future_WYstart, Future_WY <= Future_WYend) %>%
  arrange(Future_WY) %>%
  Group_WY(df = ., grp_col = 'DWR_2070DEW_Projection_WYT') %>%
  group_by(group) %>%
  summarize(WY_Start = min(Future_WY), WY_End = max(Future_WY), Type = DWR_2070DEW_Projection_WYT) %>%
  slice_head() %>%
  ungroup() %>%
  filter(Type%in%c('Critical', 'Dry', 'Wet')) %>%
  dplyr::select(WY_Start, WY_End, Type)

WY_Types_2070WMW = read.csv('CentralValley_2070WMW_WYT.csv', header = T) %>%
  filter(Valley=='Sacramento') %>%
  mutate(DWR_2070MWM_Projection_WYT = gsub(x = DWR_2070MWM_Projection_WYT, pattern = 'Critical', replacement = 'Dry')) %>%
  left_join(Hist_Future_WYs) %>%
  filter(Future_WY >= Future_WYstart, Future_WY <= Future_WYend) %>%
  arrange(Future_WY) %>%
  Group_WY(df = ., grp_col = 'DWR_2070MWM_Projection_WYT') %>%
  group_by(group) %>%
  summarize(WY_Start = min(Future_WY), WY_End = max(Future_WY), Type = DWR_2070MWM_Projection_WYT) %>%
  slice_head() %>%
  ungroup() %>%
  filter(Type%in%c('Critical', 'Dry', 'Wet')) %>%
  dplyr::select(WY_Start, WY_End, Type)

write.table(WY_Types_no_change, file = 'SVHSM_Simulated_WY_Types_Historical.txt', append = F, quote = F, sep = '\t', row.names = F, col.names = T)
write.table(WY_Types_2030, file = 'SVHSM_Simulated_WY_Types_2030.txt', append = F, quote = F, sep = '\t', row.names = F, col.names = T)
write.table(WY_Types_2070, file = 'SVHSM_Simulated_WY_Types_2070.txt', append = F, quote = F, sep = '\t', row.names = F, col.names = T)
write.table(WY_Types_2070DEW, file = 'SVHSM_Simulated_WY_Types_2070DEW.txt', append = F, quote = F, sep = '\t', row.names = F, col.names = T)
write.table(WY_Types_2070WMW, file = 'SVHSM_Simulated_WY_Types_2070WMW.txt', append = F, quote = F, sep = '\t', row.names = F, col.names = T)

# Read in DWR Change Factors ----------------------------------------------
climateCF = read.csv('SGMA_Precip-ET_2030-2070_ChangeFactors.csv') %>%
  filter(VICGrid_ID==3208) %>%
  mutate(ModelDate = as.Date(ModelDate, '%m/%d/%Y'))

streamflowCF = read.csv('HUC8_18020123_MonthlyChangeFactors.csv') %>%
  mutate(ModelDate = as.Date(ModelDate, '%m/%d/%Y'),
         Month = format(ModelDate,'%b'),
         WY = if_else(format(ModelDate,'%b')%in%month.abb[1:9],      # populate water year
                      format(ModelDate, '%Y') %>% as.numeric(),
                      format(ModelDate, '%Y') %>% as.numeric()+1)) 


# Precipitation  -----------------------------------------------------------
Precip_Adjusted = read.table('PRMS/SV_prms_v2/input/sv.data', header = F, skip = 15) %>% 
  rename(Year = V1, Month = V2, Day = V3, tmax = V7, tmin = V8, precip_mm = V9) %>%
  mutate(MonthDay = paste(Month,Day,sep = '-'),
         ModelDate = as.Date(paste(Year,Month, '01',sep = '-')), 
         WY = if_else(Month%in%c(10,11,12), Year+1 %>% as.numeric(), Year %>% as.numeric()),
         Precip_m = precip_mm/1000) %>%
  filter(WY <= 2011, MonthDay != '2-29') %>%
  left_join(climateCF %>% dplyr::select(-grep(x = names(climateCF), pattern = 'ET'))) %>%    # join and exclude ET columns
  mutate(Precip_m_2030 = Precip_m * Precip2030Factor,
         Precip_m_2070 = Precip_m * Precip2070Factor,
         Precip_m_2070DEW = Precip_m * Precip2070DEWFactor,
         Precip_m_2070WMW = Precip_m * Precip2070WMWFactor)

for(i in 1:50){
  if (i==1) {
    Precip_Projected = Precip_Adjusted %>% 
      filter(WY==Future_WYs[i]) %>%
      mutate(FWY = 2020+i)
  } else {
    Precip_Projected = rbind(Precip_Projected, Precip_Adjusted %>%
                               filter(WY==Future_WYs[i]) %>%
                               mutate(FWY = 2020+i))
    
  }
}
Precip_Projected$FutureDate = FutureModelDays

# No Change
write.table(Precip_Projected %>% dplyr::select(Precip_m , FutureDate), 
            file = 'SWBM/input_150m_grid/alternate_input_files/precip_projected_Historical.txt',
            append = F, quote = F, row.names = F, col.names = T, sep = '\t')

# 2030 Change Factors
write.table(Precip_Projected %>% dplyr::select(Precip_m_2030, FutureDate), 
            file = 'SWBM/input_150m_grid/alternate_input_files/precip_projected_2030.txt',
            append = F, quote = F, row.names = F, col.names = T, sep = '\t')

# 2070 Change Factors
write.table(Precip_Projected %>% dplyr::select(Precip_m_2070, FutureDate), 
            file = 'SWBM/input_150m_grid/alternate_input_files/precip_projected_2070.txt',
            append = F, quote = F, row.names = F, col.names = T, sep = '\t')

# 2070DEW Change Factors
write.table(Precip_Projected %>% dplyr::select(Precip_m_2070DEW, FutureDate), 
            file = 'SWBM/input_150m_grid/alternate_input_files/precip_projected_2070DEW.txt',
            append = F, quote = F, row.names = F, col.names = T, sep = '\t')

# 2070WMW Change Factors
write.table(Precip_Projected %>% dplyr::select(Precip_m_2070WMW, FutureDate), 
            file = 'SWBM/input_150m_grid/alternate_input_files/precip_projected_2070WMW.txt',
            append = F, quote = F, row.names = F, col.names = T, sep = '\t')

# Create Monthly and Annual Tables
Precip_Projected_Monthly = Precip_Projected %>%
  mutate(FutureMonth = paste0(format(FutureDate,'%Y-%m'),'-01') %>% as.Date()) %>%
  dplyr::select(ModelDate, FutureMonth, Precip_m, Precip_m_2030, Precip_m_2070, Precip_m_2070DEW, Precip_m_2070WMW) %>%
  group_by(ModelDate, FutureMonth) %>%
  summarize_all(.funs = sum) %>%
  rename(HistoricalMonth = ModelDate) %>%
  arrange(FutureMonth)

Precip_Projected_Annual = Precip_Projected %>%
  mutate(FutureMonth = paste0(format(FutureDate,'%Y-%m'),'-01') %>% as.Date()) %>%
  dplyr::select(WY, FWY, Precip_m, Precip_m_2030, Precip_m_2070, Precip_m_2070DEW, Precip_m_2070WMW) %>%
  group_by(WY, FWY) %>%
  summarize_all(.funs = sum) %>%
  arrange(FWY)

write.csv(Precip_Projected, file = 'Precip_Daily_50yrs.csv', quote = F, row.names = F)
write.csv(Precip_Projected_Monthly, file = 'Precip_Monthly_50yrs.csv', quote = F, row.names = F)
write.csv(Precip_Projected_Annual, file = 'Precip_Annual_50yrs.csv', quote = F, row.names = F)

# Reference ET ------------------------------------------------------------
SWBM_ETo = read.table('SWBM/input_150m_grid/ref_et.txt', header = T) %>%
  mutate(Date = as.Date(Date)) %>%
  filter(Date < as.Date('2011-10-01'))
Buntingville_CIMIS = read.csv('CIMIS_Buntingville_057_entire_record.csv', header = T) %>%
  mutate(Date = as.Date(Date, '%m/%d/%Y'),
         ETo_m = ETo..mm./1000) %>%
  filter(Date >= as.Date('1989-10-01'), Date < as.Date('1999-10-01')) %>%
  dplyr::select(ETo_m, Date)

Ref_ET_Adjusted = rbind(SWBM_ETo, Buntingville_CIMIS) %>%
  mutate(WY = if_else(format(Date,'%b') %in% month.abb[10:12], format(Date,'%Y') %>% as.numeric() + 1, format(Date,'%Y') %>% as.numeric()),
         ModelDate = as.Date(paste0(format(Date, '%Y-%m'), '-01'))) %>%
  filter(format(Date,'%b-%d') != 'Feb-29') %>%
  left_join(climateCF %>% dplyr::select(-grep(x = names(climateCF), pattern = 'Precip'))) %>%
  mutate(ETo_m_2030 = ETo_m * ET2030Factor,
         ETo_m_2070 = ETo_m * ET2070Factor,
         ETo_m_2070DEW = ETo_m * ET2070DEWFactor,
         ETo_m_2070WMW = ETo_m * ET2070WMWFactor)

for(i in 1:50){
  if (i==1) {
    Ref_ET_Projected = Ref_ET_Adjusted %>% 
      filter(WY==Future_WYs[i]) %>%
      mutate(FWY = 2020+i)
  } else {
    Ref_ET_Projected = rbind(Ref_ET_Projected, Ref_ET_Adjusted %>%
                               filter(WY==Future_WYs[i]) %>%
                               mutate(FWY = 2020+i))
    
  }
}
Ref_ET_Projected$FutureDate = FutureModelDays


# No Change
write.table(Ref_ET_Projected %>% 
              dplyr::select(FutureDate, ETo_m) %>%
              mutate(ETo_m = format(ETo_m, digits = 6)), 
            file = 'SWBM/input_150m_grid/alternate_input_files/Ref_ET_projected_Historical.txt',
            append = F, quote = F, row.names = F, col.names = T, sep = '\t')

# 2030 Change Factors
write.table(Ref_ET_Projected %>% 
              dplyr::select(FutureDate, ETo_m_2030) %>%
              mutate(ETo_m_2030 = format(ETo_m_2030, digits = 6)), 
            file = 'SWBM/input_150m_grid/alternate_input_files/Ref_ET_projected_2030.txt',
            append = F, quote = F, row.names = F, col.names = T, sep = '\t')

# 2070 Change Factors
write.table(Ref_ET_Projected %>% 
              dplyr::select(FutureDate, ETo_m_2070) %>%
              mutate(ETo_m_2070 = format(ETo_m_2070, digits = 6)), 
            file = 'SWBM/input_150m_grid/alternate_input_files/Ref_ET_projected_2070.txt',
            append = F, quote = F, row.names = F, col.names = T, sep = '\t')

# 2070DEW Change Factors
write.table(Ref_ET_Projected %>% 
              dplyr::select(FutureDate, ETo_m_2070DEW) %>%
              mutate(ETo_m_2070DEW = format(ETo_m_2070DEW, digits = 6)), 
            file = 'SWBM/input_150m_grid/alternate_input_files/Ref_ET_projected_2070DEW.txt',
            append = F, quote = F, row.names = F, col.names = T, sep = '\t')

# 2070WMW Change Factors
write.table(Ref_ET_Projected %>% 
              dplyr::select(FutureDate, ETo_m_2070WMW) %>%
              mutate(ETo_m_2070WMW = format(ETo_m_2070WMW, digits = 6)), 
            file = 'SWBM/input_150m_grid/alternate_input_files/Ref_ET_projected_2070WMW.txt',
            append = F, quote = F, row.names = F, col.names = T, sep = '\t')


# Create Monthly and Annual Tables
Ref_ET_Projected_Monthly = Ref_ET_Projected %>%
  mutate(FutureMonth = paste0(format(FutureDate,'%Y-%m'),'-01') %>% as.Date()) %>%
  dplyr::select(ModelDate, FutureMonth, ETo_m, ETo_m_2030, ETo_m_2070, ETo_m_2070DEW, ETo_m_2070WMW) %>%
  group_by(ModelDate, FutureMonth) %>%
  summarize_all(.funs = sum) %>%
  rename(HistoricalMonth = ModelDate) %>%
  arrange(FutureMonth)

Ref_ET_Projected_Annual = Ref_ET_Projected %>%
  mutate(FutureMonth = paste0(format(FutureDate,'%Y-%m'),'-01') %>% as.Date()) %>%
  dplyr::select(WY, FWY, ETo_m, ETo_m_2030, ETo_m_2070, ETo_m_2070DEW, ETo_m_2070WMW) %>%
  group_by(WY, FWY) %>%
  summarize_all(.funs = sum) %>%
  arrange(FWY)

write.csv(Ref_ET_Projected, file = 'Ref_ET_Daily_50yrs.csv', quote = F, row.names = F)
write.csv(Ref_ET_Projected_Monthly, file = 'RefET_Monthly_50yrs.csv', quote = F, row.names = F)
write.csv(Ref_ET_Projected_Annual, file = 'Ref_ET_Annual_50yrs.csv', quote = F, row.names = F)


# Streamflow --------------------------------------------------------------
average_streamflow = function(SWBM_inflows, WY_array){
  sf_avg = SWBM_inflows %>%
    filter(WY%in%WY_array) %>%
    dplyr::select(-modelMonth, -WY, -Year) %>%
    group_by(Month) %>%
    summarize_all(.funs = mean)
  return(sf_avg)
}

DWR_WYIdx = read.csv('DWR_Sac_Valley_WYIdx.csv')

Irrigation_inflows = read.table('SWBM/input_150m_grid/subwatershed_irrigation_inflows.txt', sep = '\t', header = T) %>%
  mutate(Month = strsplit(modelMonth, split = '-') %>% sapply('[[', 1),
         Month = factor(Month, levels = c(month.abb[10:12], month.abb[1:9]), ordered = T),
         Year = strsplit(modelMonth, split = '-') %>% sapply('[[', 2) %>% as.numeric(),
         WY = if_else(Month%in%month.abb[1:9],      # populate water year
                      Year,
                      Year + 1))
nonIrrigation_inflows = read.table('SWBM/input_150m_grid/subwatershed_nonIrrigation_inflows.txt', sep = '\t', header = T) %>%
  mutate(Month = strsplit(modelMonth, split = '-') %>% sapply('[[', 1),
         Month = factor(Month, levels = c(month.abb[10:12], month.abb[1:9]), ordered = T),
         Year = strsplit(modelMonth, split = '-') %>% sapply('[[', 2) %>% as.numeric(),
         WY = if_else(Month%in%month.abb[1:9],      # populate water year
                      Year,
                      Year + 1))

W_fill_irr = average_streamflow(Irrigation_inflows, DWR_WYIdx$WY[DWR_WYIdx$Type=='W'])
AN_fill_irr  = average_streamflow(Irrigation_inflows, DWR_WYIdx$WY[DWR_WYIdx$Type=='AN'])
BN_fill_irr  = average_streamflow(Irrigation_inflows, DWR_WYIdx$WY[DWR_WYIdx$Type=='BN'])
D_fill_irr  = average_streamflow(Irrigation_inflows, DWR_WYIdx$WY[DWR_WYIdx$Type=='D'])
C_fill_irr  = average_streamflow(Irrigation_inflows, DWR_WYIdx$WY[DWR_WYIdx$Type=='C'])

W_fill_nonIrr = average_streamflow(nonIrrigation_inflows, DWR_WYIdx$WY[DWR_WYIdx$Type=='W'])
AN_fill_nonIrr = average_streamflow(nonIrrigation_inflows, DWR_WYIdx$WY[DWR_WYIdx$Type=='AN'])
BN_fill_nonIrr = average_streamflow(nonIrrigation_inflows, DWR_WYIdx$WY[DWR_WYIdx$Type=='BN'])
D_fill_nonIrr = average_streamflow(nonIrrigation_inflows, DWR_WYIdx$WY[DWR_WYIdx$Type=='D'])
C_fill_nonIrr = average_streamflow(nonIrrigation_inflows, DWR_WYIdx$WY[DWR_WYIdx$Type=='C'])

W_change_factors = streamflowCF %>%
  filter(WY == 2006) %>%
  dplyr::select(-ModelDate, -WY) %>%
  relocate(Month)
AN_change_factors = streamflowCF %>%
  filter(WY %in% c(2000,2003,2005)) %>%
  dplyr::select(-ModelDate, -WY) %>%
  group_by(Month) %>%
  summarize_all(.funs = mean) %>%
  mutate(Month = factor(Month, levels = c(month.abb[10:12], month.abb[1:9]), ordered = T)) %>%
  arrange(Month)
BN_change_factors = streamflowCF %>%
  filter(WY %in% c(2004,2010)) %>%
  dplyr::select(-ModelDate, -WY) %>%
  group_by(Month) %>%
  summarize_all(.funs = mean) %>%
  mutate(Month = factor(Month, levels = c(month.abb[10:12], month.abb[1:9]), ordered = T)) %>%
  arrange(Month)
D_change_factors = streamflowCF %>%
  filter(WY %in% c(2001,2002,2007,2009)) %>%
  dplyr::select(-ModelDate, -WY) %>%
  group_by(Month) %>%
  summarize_all(.funs = mean) %>%
  mutate(Month = factor(Month, levels = c(month.abb[10:12], month.abb[1:9]), ordered = T)) %>%
  arrange(Month)
C_change_factors = streamflowCF %>%
  filter(WY == 2008) %>%
  dplyr::select(-ModelDate, -WY) %>%
  relocate(Month)


for (i in 1:50){
  if(Future_WYs[i]%in%Irrigation_inflows$WY){
    irr_temp = Irrigation_inflows %>% 
      filter(WY==Future_WYs[i]) %>%
      dplyr::select(-modelMonth, -Year, -WY)
    nonIrr_temp = nonIrrigation_inflows %>% 
      filter(WY==Future_WYs[i]) %>%
      dplyr::select(-modelMonth, -Year, -WY)
    if (Future_WYs[i]<2011){
      changeFactors_temp = streamflowCF %>%
        filter(WY == Future_WYs[i]) %>%
        dplyr::select(-ModelDate,-WY) %>%
        relocate(Month)
    }
  } else {
    if (DWR_WYIdx$Type[DWR_WYIdx$WY==Future_WYs[i]]=='W') {
      irr_temp = W_fill_irr
      nonIrr_temp = W_fill_nonIrr
      changeFactors_temp = W_change_factors
    } else if(DWR_WYIdx$Type[DWR_WYIdx$WY==Future_WYs[i]]=='AN') {
      irr_temp = AN_fill_irr
      nonIrr_temp = AN_fill_nonIrr
      changeFactors_temp = AN_change_factors
    }  else if(DWR_WYIdx$Type[DWR_WYIdx$WY==Future_WYs[i]]=='BN') {
      irr_temp = AN_fill_irr
      nonIrr_temp = AN_fill_nonIrr
      changeFactors_temp = BN_change_factors
    } else if(DWR_WYIdx$Type[DWR_WYIdx$WY==Future_WYs[i]]=='D') {
      irr_temp = D_fill_irr
      nonIrr_temp = D_fill_nonIrr
      changeFactors_temp = D_change_factors
    } else if(DWR_WYIdx$Type[DWR_WYIdx$WY==Future_WYs[i]]=='C') {
      irr_temp = C_fill_irr
      nonIrr_temp = C_fill_nonIrr
      changeFactors_temp = C_change_factors
    }
  }
  if (i==1){
    future_irr_inflows = irr_temp
    future_nonIrr_inflows = nonIrr_temp
    future_change_factors = changeFactors_temp
  } else{
    future_irr_inflows = rbind(future_irr_inflows, irr_temp)
    future_nonIrr_inflows = rbind(future_nonIrr_inflows, nonIrr_temp)
    future_change_factors = rbind(future_change_factors, changeFactors_temp)
  }
}
future_irr_inflows = future_irr_inflows %>%
  mutate(Month = paste0(Month,'-',rep(seq(2021,2070), each = 12))) %>%
  relocate(Month)
future_nonIrr_inflows = future_nonIrr_inflows %>%
  mutate(Month = paste0(Month,'-',rep(seq(2021,2070), each = 12))) %>%
  relocate(Month)
future_change_factors = future_change_factors %>%
  mutate(Month = paste0(Month,'-',rep(seq(2021,2070), each = 12))) %>%
  relocate(Month)

#No Change Streamflow
write.table(future_irr_inflows,'SWBM/input_150m_grid/alternate_input_files/subwatershed_irrigation_inflows_Historical.txt',
            append = F, quote = F, sep = '\t', row.names = F, col.names = T)
write.table(future_nonIrr_inflows,'SWBM/input_150m_grid/alternate_input_files/subwatershed_nonirrigation_inflows_Historical.txt',
            append = F, quote = F, sep = '\t', row.names = F, col.names = T)

#2030 Streamflow
irr_inflows_2030 = cbind(future_irr_inflows[,1],
                         future_irr_inflows[,-1]*future_change_factors$MonthlyFactor2030)
nonIrr_inflows_2030 = cbind(future_nonIrr_inflows[,1],
                            future_nonIrr_inflows[,-1]*future_change_factors$MonthlyFactor2030)
write.table(irr_inflows_2030,'SWBM/input_150m_grid/alternate_input_files/subwatershed_irrigation_inflows_2030.txt',
            append = F, quote = F, sep = '\t', row.names = F, col.names = T)
write.table(nonIrr_inflows_2030,'SWBM/input_150m_grid/alternate_input_files/subwatershed_nonirrigation_inflows_2030.txt',
            append = F, quote = F, sep = '\t', row.names = F, col.names = T)

#2070 Streamflow
irr_inflows_2070 = cbind(future_irr_inflows[,1],
                         future_irr_inflows[,-1]*future_change_factors$MonthlyFactor2070)
nonIrr_inflows_2070 = cbind(future_nonIrr_inflows[,1],
                            future_nonIrr_inflows[,-1]*future_change_factors$MonthlyFactor2070)
write.table(irr_inflows_2070,'SWBM/input_150m_grid/alternate_input_files/subwatershed_irrigation_inflows_2070.txt',
            append = F, quote = F, sep = '\t', row.names = F, col.names = T)
write.table(nonIrr_inflows_2070,'SWBM/input_150m_grid/alternate_input_files/subwatershed_nonirrigation_inflows_2070.txt',
            append = F, quote = F, sep = '\t', row.names = F, col.names = T)

#2070DEW Streamflow
irr_inflows_2070DEW = cbind(future_irr_inflows[,1],
                         future_irr_inflows[,-1]*future_change_factors$MonthlyFactor2070DEW)
nonIrr_inflows_2070DEW = cbind(future_nonIrr_inflows[,1],
                            future_nonIrr_inflows[,-1]*future_change_factors$MonthlyFactor2070DEW)
write.table(irr_inflows_2070DEW,'SWBM/input_150m_grid/alternate_input_files/subwatershed_irrigation_inflows_2070DEW.txt',
            append = F, quote = F, sep = '\t', row.names = F, col.names = T)
write.table(nonIrr_inflows_2070DEW,'SWBM/input_150m_grid/alternate_input_files/subwatershed_nonirrigation_inflows_2070DEW.txt',
            append = F, quote = F, sep = '\t', row.names = F, col.names = T)

#2070WMW Streamflow
irr_inflows_2070WMW = cbind(future_irr_inflows[,1],
                            future_irr_inflows[,-1]*future_change_factors$MonthlyFactor2070WMW)
nonIrr_inflows_2070WMW = cbind(future_nonIrr_inflows[,1],
                               future_nonIrr_inflows[,-1]*future_change_factors$MonthlyFactor2070WMW)
write.table(irr_inflows_2070WMW,'SWBM/input_150m_grid/alternate_input_files/subwatershed_irrigation_inflows_2070WMW.txt',
            append = F, quote = F, sep = '\t', row.names = F, col.names = T)
write.table(nonIrr_inflows_2070WMW,'SWBM/input_150m_grid/alternate_input_files/subwatershed_nonirrigation_inflows_2070WMW.txt',
            append = F, quote = F, sep = '\t', row.names = F, col.names = T)

write.table(FutureModelDays, file = 'dates.txt', quote = F)


# Summary Tables
Stream_Inflow_Summary = data.frame(FutureMonth = future_irr_inflows$Month,
                                       Historical = rowSums(future_irr_inflows[,-1]) + rowSums(future_nonIrr_inflows[,-1]),
                                       `Inflows 2030` = rowSums(irr_inflows_2030[,-1]) + rowSums(nonIrr_inflows_2030[,-1]),
                                       `Inflows 2070` = rowSums(irr_inflows_2070[,-1]) + rowSums(nonIrr_inflows_2070[,-1]),
                                       `Inflows 2070DEW` = rowSums(irr_inflows_2070DEW[,-1]) + rowSums(nonIrr_inflows_2070DEW[,-1]),
                                       `Inflows 2070WMW` = rowSums(irr_inflows_2070WMW[,-1]) + rowSums(nonIrr_inflows_2070WMW[,-1]))


# Plots -------------------------------------------------------------------
theme_adj = theme(plot.title = element_text(hjust = 0.5, size = 14),
                      panel.grid.major.y = element_line(size = 0.1, color = 'gray80'),
                      axis.title = element_text(size = 13),
                      axis.title.x = element_blank(),
                      axis.text = element_text(size = 12),
                      axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))


# Cumulative Plots  
Cumulative_precip_plot = Precip_Projected_Monthly %>%
  ungroup() %>%
  dplyr::select(-HistoricalMonth) %>%
  pivot_longer(cols = -FutureMonth, names_to = 'Scenario', values_to = 'Precip_m') %>%
  group_by(Scenario) %>%
  summarize(`Cumulative Precipitation (ft)` = cumsum(Precip_m)*3.28084) %>%
  ungroup() %>%
  mutate(FutureMonth = rep(Precip_Projected_Monthly$FutureMonth, 5)) %>%
  ggplot(aes(x = FutureMonth, y = `Cumulative Precipitation (ft)`, color = Scenario)) +
  geom_line(size = 1) +
  scale_x_date(limits = c(Future_WYstartDate, Future_WYendDate), date_breaks = '5 years', date_labels = '%Y', expand = c(0.01,0.01)) +
  scale_y_continuous(limits = c(0,150), breaks = seq(0,150, by = 30), expand = c(0,0)) +
  scale_color_brewer(type = 'div', palette = 'Set1', labels = c('Historical','2030','2070','2070DEW','2070WMW')) + 
  ggtitle('Cumulative Precipitation') +
  theme_few() +
  theme_adj

Cumulative_ETo_plot = Ref_ET_Projected_Monthly %>%
  ungroup() %>%
  dplyr::select(-HistoricalMonth) %>%
  pivot_longer(cols = -FutureMonth, names_to = 'Scenario', values_to = 'ETo_m') %>%
  group_by(Scenario) %>%
  summarize(`Cumulative ETo (ft)` = cumsum(ETo_m)*3.28084) %>%
  ungroup() %>%
  mutate(FutureMonth = rep(Ref_ET_Projected_Monthly$FutureMonth, 5)) %>%
  ggplot(aes(x = FutureMonth, y = `Cumulative ETo (ft)`, color = Scenario)) +
  geom_line(size = 1) +
  scale_x_date(limits = c(Future_WYstartDate, Future_WYendDate), date_breaks = '5 years', date_labels = '%Y', expand = c(0.01,0.01)) +
  scale_y_continuous(limits = c(0,250), breaks = seq(0,250, by = 50), expand = c(0,0)) +
  scale_color_brewer(type = 'div', palette = 'Set1', labels = c('Historical','2030','2070','2070DEW','2070WMW')) + 
  ggtitle('Cumulative Reference ET') +
  theme_few() +
  theme_adj

Cumulative_Streamflow_plot = Stream_Inflow_Summary %>%
  pivot_longer(cols = -FutureMonth, names_to = 'Scenario', values_to = 'Stream_Inflow_m3') %>%
  group_by(Scenario) %>%
  summarize(`Cumulative Stream Inflow (MAF)` = cumsum(Stream_Inflow_m3)*0.000000000810714) %>%
  ungroup() %>%
  mutate(FutureMonth = rep(Ref_ET_Projected_Monthly$FutureMonth, 5)) %>%
  ggplot(aes(x = FutureMonth, y = `Cumulative Stream Inflow (MAF)`, color = Scenario)) +
  geom_line(size = 1) +
  scale_x_date(limits = c(Future_WYstartDate, Future_WYendDate), date_breaks = '5 years', date_labels = '%Y', expand = c(0.01,0.01)) +
  scale_y_continuous(limits = c(0,8), breaks = seq(0,8, by = 2), expand = c(0,0)) +
  scale_color_brewer(type = 'div', palette = 'Set1', labels = c('Historical','2030','2070','2070DEW','2070WMW')) + 
  ggtitle('Cumulative Stream Inflow') +
  theme_few() +
  theme_adj

cumulative_stack = plot_grid(Cumulative_precip_plot, Cumulative_ETo_plot, Cumulative_Streamflow_plot, ncol = 1)
ggsave(plot = cumulative_stack, 
       filename = 'Cumulative_Changes.png', 
       device = 'png',
       width = 8.5,
       height = 11,
       units = 'in')

# Average Difference by Month
Precip_Diff_by_Month_Plot = Precip_Projected_Monthly %>%
  ungroup() %>%
  mutate(diff_in_2030 = (Precip_m_2030 - Precip_m)*39.3701,
         diff_in_2070 = (Precip_m_2070 - Precip_m)*39.3701,
         diff_in_2070DEW = (Precip_m_2070DEW - Precip_m)*39.3701,
         diff_in_2070WMW = (Precip_m_2070WMW - Precip_m)*39.3701,
         Month = format(FutureMonth,'%b') %>% factor(levels = c(month.abb[10:12], month.abb[1:9]), ordered = T)) %>%
  dplyr::select(Month, diff_in_2030, diff_in_2070, diff_in_2070DEW, diff_in_2070WMW) %>%
  group_by(Month) %>%
  summarise_all(.funs = mean) %>%
  pivot_longer(cols = -Month, names_to = 'Scenario', values_to = 'Average Change from Historical (in)') %>%
  ggplot(aes(x = Month, y = `Average Change from Historical (in)`, fill = Scenario)) +
  geom_bar(stat = 'identity', position = 'dodge') +
  scale_y_continuous(limits = c(-3,3), breaks = seq(-3,3, by = 1), expand = c(0,0)) +
  scale_fill_manual(values = brewer.pal(5, 'Set1')[-1], labels = c('2030','2070','2070DEW','2070WMW')) + 
  ggtitle('Average Monthly Precipitation Change') +
  theme_few() +
  theme_adj

Ref_ET_Diff_by_Month_Plot = Ref_ET_Projected_Monthly %>%
  ungroup() %>%
  mutate(diff_in_2030 = (ETo_m_2030 - ETo_m)*39.3701,
         diff_in_2070 = (ETo_m_2070 - ETo_m)*39.3701,
         diff_in_2070DEW = (ETo_m_2070DEW - ETo_m)*39.3701,
         diff_in_2070WMW = (ETo_m_2070WMW - ETo_m)*39.3701,
         Month = format(FutureMonth,'%b') %>% factor(levels = c(month.abb[10:12], month.abb[1:9]), ordered = T)) %>%
  dplyr::select(Month, diff_in_2030, diff_in_2070, diff_in_2070DEW, diff_in_2070WMW) %>%
  group_by(Month) %>%
  summarise_all(.funs = mean) %>%
  pivot_longer(cols = -Month, names_to = 'Scenario', values_to = 'Average Change from Historical (in)') %>%
  ggplot(aes(x = Month, y = `Average Change from Historical (in)`, fill = Scenario)) +
  geom_bar(stat = 'identity', position = 'dodge') +
  scale_y_continuous(limits = c(-0.5,2), breaks = seq(-0.5,2, by = 0.5), expand = c(0,0)) +
  scale_fill_manual(values = brewer.pal(5, 'Set1')[-1], labels = c('2030','2070','2070DEW','2070WMW')) + 
  ggtitle('Average Monthly Reference ET Change') +
  theme_few() +
  theme_adj

Stream_Inflow_Diff_by_Month_Plot = Stream_Inflow_Summary %>%
  ungroup() %>%
  mutate(diff_in_2030 = (Inflows.2030 - Historical )*0.000000810714,
         diff_in_2070 = (Inflows.2070 - Historical )*0.000000810714,
         diff_in_2070DEW = (Inflows.2070DEW - Historical )*0.000000810714,
         diff_in_2070WMW = (Inflows.2070WMW - Historical )*0.000000810714,
         Month = strtrim(FutureMonth,3) %>% factor(levels = c(month.abb[10:12], month.abb[1:9]), ordered = T)) %>%
  dplyr::select(Month, diff_in_2030, diff_in_2070, diff_in_2070DEW, diff_in_2070WMW) %>%
  group_by(Month) %>%
  summarise_all(.funs = mean) %>%
  pivot_longer(cols = -Month, names_to = 'Scenario', values_to = 'Average Change from Historical (TAF)') %>%
  ggplot(aes(x = Month, y = `Average Change from Historical (TAF)`, fill = Scenario)) +
  geom_bar(stat = 'identity', position = 'dodge') +
  scale_y_continuous(limits = c(-12,12), breaks = seq(-12,12, by = 3), expand = c(0,0)) +
  scale_fill_manual(values = brewer.pal(5, 'Set1')[-1], labels = c('2030','2070','2070DEW','2070WMW')) + 
  ggtitle('Average Monthly Stream Inflow Change') +
  theme_few() +
  theme_adj

diff_stack = plot_grid(Precip_Diff_by_Month_Plot, Ref_ET_Diff_by_Month_Plot, Stream_Inflow_Diff_by_Month_Plot, ncol = 1)
ggsave(plot = diff_stack, 
       filename = 'Average_Monthly_Change.png', 
       device = 'png',
       width = 8.5,
       height = 11,
       units = 'in')
