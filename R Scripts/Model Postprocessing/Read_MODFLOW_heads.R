# Read MODFLOW Binary Head File

# User Inputs -------------------------------------------------------------
options(warn = -1)

NLAY = 12                 # Number of layers in model
NSP = 252                 # Number of stress periods for which heads are printed
filename = 'SVHSM.hds'   #Name of binary head file
Observations_file = 'SVHSM_Water_Level_Observations.dat/'
No_FLow_Val = -9999       #Value of no flow cells
output_dir = 'Results/'  #Output directory
layer_z_dir = 'layer_z/'
active_cells_dir = 'active_cells/'

WYstart = 2000
WYend = 2020

Monitoring_Well_Hydrographs = TRUE

Export_Starting_Heads = FALSE
Starting_Heads_SP = 1
Starting_Heads_dir = 'initial_heads/'
dryCellValue = -5555


theme_adj = theme(plot.title = element_text(hjust = 0.5, size = 14),
                  panel.grid.major.y = element_line(size = 0.1, color = 'gray80'),
                  panel.border = element_rect(color = 'black', fill = NA, size = 1),
                  plot.background = element_rect(fill = NA, color = NA),
                  axis.title = element_blank(),
                  axis.text = element_text(size = 12),
                  axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))
# Initialize Script -------------------------------------------------------
library(dplyr)
library(plotly)
library(ggplot2)
library(RColorBrewer)
library(ggthemes)

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


# Read Layer Info ---------------------------------------------------------
surface_z = read.table(file = paste0(layer_z_dir,'L1_top_z.txt'), header = F) %>% as.matrix()
for(i in 1:NLAY){
  temp_bot_z = read.table(file = paste0(layer_z_dir,'L',i,'_bot_z.txt'), header = F) %>% as.matrix()
  temp_active_cells = read.table(file = paste0(active_cells_dir,'L',i,'active_cells.txt'), header = F) %>% as.matrix()
  if(i==1){
    layer_bot_z = temp_bot_z
    active_cells = temp_active_cells
  }else{
    layer_bot_z = abind::abind(layer_bot_z, temp_bot_z, along = 3)
    active_cells = abind::abind(active_cells, temp_active_cells, along = 3)
  }
}

# functions --------------------------------------------------------------
roundUp <- function(x,base){                                                                # function for rounding numbers down to specified base
  base*ceiling(x/base)
}

roundDown <- function(x,base){                                                                # function for rounding numbers down to specified base
  base*floor(x/base)
}

SP_heads = function(HDS_binary, NLAY, SP_extract){
fid = file(HDS_binary, "rb")                      
bytes = 0                                     #Bytes counter
# Read first SP data to determine how many bytes are in each printing
  KSPT = readBin(fid, integer(), n = 1, size = 4); bytes = bytes + 4                          #Time step number in stress period 
  KPER = readBin(fid, integer(), n = 1, size = 4); bytes = bytes + 4                          #Stress period number
  PERTIM = readBin(fid, numeric(), n = 1, size = 4); bytes = bytes + 4                        #Time in the current stress period
  TOTIM = readBin(fid, numeric(), n = 1, size = 4); bytes = bytes + 4                         #Total elapsed time
  DESC =  readBin(readBin(fid, "raw", n=16L, size=1L, endian="little"),
                           "character", n=1L, endian="little"); bytes = bytes + 16            #Description of the array
  NCOL = readBin(fid, integer(), n = 1, size = 4); bytes = bytes + 4                          #Number of columns in the model
  NROW = readBin(fid, integer(), endian = "little", size = 4); bytes = bytes + 4              #Number of rows in the model
  ILAY = readBin(fid, integer(), endian = "little", size = 4); bytes = bytes + 4              #Current layer number
  H1 = matrix(readBin(fid, numeric(), n=NCOL*NROW, size = 4), nrow = NROW, ncol = NCOL, byrow = T) ;bytes = bytes + 4*NCOL*NROW  #Read in head matrix
  for (i in 2:NLAY){ # Read in data for remaining layers
    KSPT = readBin(fid, integer(), n = 1, size = 4); bytes = bytes + 4                          #Time step number in stress period 
    KPER = readBin(fid, integer(), n = 1, size = 4); bytes = bytes + 4                          #Stress period number
    PERTIM = readBin(fid, numeric(), n = 1, size = 4); bytes = bytes + 4                        #Time in the current stress period
    TOTIM = readBin(fid, numeric(), n = 1, size = 4); bytes = bytes + 4                         #Total elapsed time
    DESC =  readBin(readBin(fid, "raw", n=16L, size=1L, endian="little"),
                    "character", n=1L, endian="little"); bytes = bytes + 16            #Description of the array
    NCOL = readBin(fid, integer(), n = 1, size = 4); bytes = bytes + 4                          #Number of columns in the model
    NROW = readBin(fid, integer(), endian = "little", size = 4); bytes = bytes + 4              #Number of rows in the model
    ILAY = readBin(fid, integer(), endian = "little", size = 4); bytes = bytes + 4              #Current layer number  
    H_temp = matrix(readBin(fid, numeric(), n=NCOL*NROW, size = 4), nrow = NROW, ncol = NCOL, byrow = T); bytes = bytes + 4*NCOL*NROW  #Read in head matrix
  }
  fid = file(filename, "rb")                                                                  # re-read binary file to reset location
  seek(fid, 2519952*(SP_extract-1))                                                           # skip to desired stress period
  print(paste0('Reading Heads for Stress Period ', SP_extract))
  KSPT = readBin(fid, integer(), n = 1, size = 4); bytes = bytes + 4                          #Time step number in stress period 
  KPER = readBin(fid, integer(), n = 1, size = 4); bytes = bytes + 4                          #Stress period number
  PERTIM = readBin(fid, numeric(), n = 1, size = 4); bytes = bytes + 4                        #Time in the current stress period
  TOTIM = readBin(fid, numeric(), n = 1, size = 4); bytes = bytes + 4                         #Total elapsed time
  DESC =  readBin(readBin(fid, "raw", n=16L, size=1L, endian="little"),
                  "character", n=1L, endian="little"); bytes = bytes + 16            #Description of the array
  NCOL = readBin(fid, integer(), n = 1, size = 4); bytes = bytes + 4                          #Number of columns in the model
  NROW = readBin(fid, integer(), endian = "little", size = 4); bytes = bytes + 4              #Number of rows in the model
  ILAY = readBin(fid, integer(), endian = "little", size = 4); bytes = bytes + 4              #Current layer number
  H = array(data = NA, dim = c(NROW, NCOL, NLAY))
  H1 = matrix(readBin(fid, numeric(), n=NCOL*NROW, size = 4), nrow = NROW, ncol = NCOL, byrow = T) ;bytes = bytes + 4*NCOL*NROW  #Read in head matrix
  H[,,1] = H1
  for (k in 2:NLAY){ # Read in data for remaining layers
    KSPT = readBin(fid, integer(), n = 1, size = 4); bytes = bytes + 4                          #Time step number in stress period 
    KPER = readBin(fid, integer(), n = 1, size = 4); bytes = bytes + 4                          #Stress period number
    PERTIM = readBin(fid, numeric(), n = 1, size = 4); bytes = bytes + 4                        #Time in the current stress period
    TOTIM = readBin(fid, numeric(), n = 1, size = 4); bytes = bytes + 4                         #Total elapsed time
    DESC =  readBin(readBin(fid, "raw", n=16L, size=1L, endian="little"),
                    "character", n=1L, endian="little"); bytes = bytes + 16            #Description of the array
    NCOL = readBin(fid, integer(), n = 1, size = 4); bytes = bytes + 4                          #Number of columns in the model
    NROW = readBin(fid, integer(), endian = "little", size = 4); bytes = bytes + 4              #Number of rows in the model
    ILAY = readBin(fid, integer(), endian = "little", size = 4); bytes = bytes + 4              #Current layer number  
    H_temp = matrix(readBin(fid, numeric(), n=NCOL*NROW, size = 4), nrow = NROW, ncol = NCOL, byrow = T); bytes = bytes + 4*NCOL*NROW  #Read in head matrix
    H[,,k] = H_temp
  }
  return(H)
}

well_hydrograph = function(HDS_binary, NLAY, NSP, row, col){
  fid = file(HDS_binary, "rb")                      
  bytes = 0                                     #Bytes counter
  for(t in 1:NSP){
    KSPT = readBin(fid, integer(), n = 1, size = 4); bytes = bytes + 4                          #Time step number in stress period 
    KPER = readBin(fid, integer(), n = 1, size = 4); bytes = bytes + 4                          #Stress period number
    PERTIM = readBin(fid, numeric(), n = 1, size = 4); bytes = bytes + 4                        #Time in the current stress period
    TOTIM = readBin(fid, numeric(), n = 1, size = 4); bytes = bytes + 4                         #Total elapsed time
    DESC =  readBin(readBin(fid, "raw", n=16L, size=1L, endian="little"),
                    "character", n=1L, endian="little"); bytes = bytes + 16            #Description of the array
    NCOL = readBin(fid, integer(), n = 1, size = 4); bytes = bytes + 4                          #Number of columns in the model
    NROW = readBin(fid, integer(), endian = "little", size = 4); bytes = bytes + 4              #Number of rows in the model
    ILAY = readBin(fid, integer(), endian = "little", size = 4); bytes = bytes + 4              #Current layer number
    H_temp = matrix(readBin(fid, numeric(), n=NCOL*NROW, size = 4), nrow = NROW, ncol = NCOL, byrow = T) ;bytes = bytes + 4*NCOL*NROW  #Read in head matrix
    if(t==1){
      head = data.frame(`Stress Period` = KPER,
                        `Time Step` = KSPT,
                        `Time in SP` = PERTIM,
                        `Total Model Time` = TOTIM,
                        Head = H_temp[row,col],
                        Layer = 'Layer 1')
    } else {
      head_temp = data.frame(`Stress Period` = KPER,
                             `Time Step` = KSPT,
                             `Time in SP` = PERTIM,
                             `Total Model Time` = TOTIM,
                             Head = H_temp[row,col],
                             Layer = 'Layer 1') 
      head = rbind(head, head_temp)
    }
    for (k in 2:NLAY){ # Read in data for remaining layers
      KSPT = readBin(fid, integer(), n = 1, size = 4); bytes = bytes + 4                          #Time step number in stress period 
      KPER = readBin(fid, integer(), n = 1, size = 4); bytes = bytes + 4                          #Stress period number
      PERTIM = readBin(fid, numeric(), n = 1, size = 4); bytes = bytes + 4                        #Time in the current stress period
      TOTIM = readBin(fid, numeric(), n = 1, size = 4); bytes = bytes + 4                         #Total elapsed time
      DESC =  readBin(readBin(fid, "raw", n=16L, size=1L, endian="little"),
                      "character", n=1L, endian="little"); bytes = bytes + 16            #Description of the array
      NCOL = readBin(fid, integer(), n = 1, size = 4); bytes = bytes + 4                          #Number of columns in the model
      NROW = readBin(fid, integer(), endian = "little", size = 4); bytes = bytes + 4              #Number of rows in the model
      ILAY = readBin(fid, integer(), endian = "little", size = 4); bytes = bytes + 4              #Current layer number  
      H_temp = matrix(readBin(fid, numeric(), n=NCOL*NROW, size = 4), nrow = NROW, ncol = NCOL, byrow = T); bytes = bytes + 4*NCOL*NROW  #Read in head matrix
      head_temp = data.frame(`Stress Period` = KPER,
                             `Time Step` = KSPT,
                             `Time in SP` = PERTIM,
                             `Total Model Time` = TOTIM,
                             Head = H_temp[row,col],
                             Layer = paste('Layer',k))
      head = rbind(head, head_temp)
    }
  }
  return(head)
}

# Plot Data ---------------------------------------------------------------
plot_SP = 252
plot_Layer = 3


Heads = SP_heads(HDS_binary = filename, NLAY = NLAY, SP_extract = plot_SP)
Heads[Heads==-9999] = NA

range(Heads[!Heads%in%c(-5555)], na.rm = T)

Head_nodry = Heads
Head_nodry[Head_nodry%in%c(-9999,-5555)] = NA

# Head
plot_ly(x = seq(1,dim(Heads)[2]), y = seq(1,dim(Heads)[1]), z = Heads[,,plot_Layer], type = "contour",
        colorscale = 'Jet', reversescale = T,  autocontour = F,
        contours = list(start = 1450, end = 1550, size = 1, showlabels = TRUE)) %>%
  layout(yaxis = list(autorange = "reversed"))

plot_ly(x = seq(1,dim(Head_nodry)[2]), y = seq(1,dim(Head_nodry)[1]), z = Head_nodry[,,plot_Layer], type = "contour",
        colorscale = 'Jet', reversescale = T,  autocontour = T,
        contours = list(size = 3, showlabels = TRUE)) %>%
  layout(yaxis = list(autorange = "reversed"))

#Recharge
plot_ly(x = seq(1,dim(recharge)[2]), y = seq(1,dim(recharge)[1]), z = recharge[,,plot_SP], type = "contour", 
        colorscale = 'Jet', reversescale = T,  autocontour = F,
        contours = list(start = 1400, end = 1500, size = 3, showlabels = TRUE)) %>% 
  layout(yaxis = list(autorange = "reversed"))

#DTW L1
plot_ly(x = seq(1,dim(Heads)[2]), y = seq(1,dim(Heads)[1]), z = Heads[,,1]- surface_z, type = "contour", 
        colorscale = 'Jet', reversescale = T,  autocontour = F,
        contours = list(start = -50, end = 50, size = 2, showlabels = TRUE)) %>% 
  layout(yaxis = list(autorange = "reversed"))



#Hydrographs
Observations = read.table(Observations_file, header = T, sep = '\t') %>%
  mutate(water_level_date = as.Date(water_level_date))

row_col_plot = c(108,150)   # MODEL CELL
well_id_plot = c(100)       #well_id

timeseries = well_hydrograph(HDS_binary = filename, NLAY = NLAY, NSP = NSP, row = row_col_plot[1], col = row_col_plot[2]) 
head_range = c(Observations %>% filter(well_id%in%well_id_plot) %>% dplyr::select(water_level_elev_m), 
               timeseries %>% filter(Head!=-9999) %>% dplyr::select(Head)) %>%
  range()

plot_ly() %>% add_trace(data = timeseries %>% filter(Head != -9999), 
                        x = ~Total.Model.Time+WYstartDate, y = ~Head, color = ~Layer, 
                        colors = "Reds", type = 'scatter', mode = 'lines+markers') %>%
  add_trace(data = Observations %>% filter(well_id%in%well_id_plot), x = ~water_level_date, y = ~water_level_elev_m, type = 'scatter', mode = 'lines+markers', color = ~well_name, 
            marker = list(color = 'blue'), line = list(color = 'blue')) %>%
  layout(title = list(text = 'Water Level Elevations', x = 0.5),
         xaxis = list(title = 'Date'), 
         yaxis = list(title = 'Groundwater Elevation (m)'),
         legend = list(font = list(size = 20)),
         barmode = 'relative',
         shapes = list(
           list(type = "rect",
                fillcolor = '#F08080', line = list(color = '#F08080'), opacity = 1, hoverinfo = 'Dry',
                x0 = WYtypes$WY_Start_Dates[1], x1 = WYtypes$WY_End_Dates[1], xref = "x", y0 = head_range[1]-2, y1 = head_range[1]-1, yref = "y"),
           list(type = "rect",
                fillcolor = '#80dfff', line = list(color = '#80dfff'), opacity = 1,
                x0 = WYtypes$WY_Start_Dates[2], x1 = WYtypes$WY_End_Dates[2], xref = "x", y0 = head_range[1]-2, y1 = head_range[1]-1, yref = "y"),
           list(type = "rect",
                fillcolor = '#F08080', line = list(color = '#F08080'), opacity = 1,
                x0 = WYtypes$WY_Start_Dates[3], x1 = WYtypes$WY_End_Dates[3], xref = "x", y0 = head_range[1]-2, y1 = head_range[1]-1, yref = "y"),
           list(type = "rect",
                fillcolor = '#80dfff', line = list(color = '#80dfff'), opacity = 1,
                x0 = WYtypes$WY_Start_Dates[4], x1 = WYtypes$WY_End_Dates[4], xref = "x", y0 = head_range[1]-2, y1 = head_range[1]-1, yref = "y"),
           list(type = "rect",
                fillcolor = '#F08080', line = list(color = '#F08080'), opacity = 1,
                x0 = WYtypes$WY_Start_Dates[5], x1 = WYtypes$WY_End_Dates[5], xref = "x", y0 = head_range[1]-2, y1 = head_range[1]-1, yref = "y"),
           list(type = "rect",
                fillcolor = '#80dfff', line = list(color = '#80dfff'), opacity = 1,
                x0 = WYtypes$WY_Start_Dates[6], x1 = WYtypes$WY_End_Dates[6], xref = "x", y0 = head_range[1]-2, y1 = head_range[1]-1, yref = "y"),
           list(type = "rect",
                fillcolor = '#80dfff', line = list(color = '#80dfff'), opacity = 1,
                x0 = WYtypes$WY_Start_Dates[7], x1 = WYtypes$WY_End_Dates[7], xref = "x", y0 = head_range[1]-2, y1 = head_range[1]-1, yref = "y"),
           list(type = "rect",
                fillcolor = '#F08080', line = list(color = '#F08080'), opacity = 1,
                x0 = WYtypes$WY_Start_Dates[8], x1 = WYtypes$WY_End_Dates[8], xref = "x", y0 = head_range[1]-2, y1 = head_range[1]-1, yref = "y"))
         # ,
         # annotations = list(x=WYtypes$annual_label_date, 
         #                    y = -585, 
         #                    text = paste0('<b>',WYtypes$Type,'<b>'), 
         #                    xref = 'x', 
         #                    yref = 'y', 
         #                    showarrow = F, 
         #                    font = list(size = 6, color = 'black'))
         )



# # Export Starting Heads ---------------------------------------------------
# if(Export_Starting_Heads){
# initialHeads = SP_heads(HDS_binary = filename, NLAY = NLAY, SP_extract = Starting_Heads_SP)         # Read heads
# 
# 
# 
# for(i in 1:NLAY){                                                                     # write inital heads for each layer to folder
#   temp_head = initialHeads[,,i]
#   temp_bot_z = read.table(paste0(layer_z_dir, 'L',i,'_bot_z.txt'), header = F) %>% as.matrix()
#   
#   
#   temp_head[which(temp_head==-5555)] = temp_bot_z[which(temp_head==-5555)] + 2
#   
#   
#   write.table(x = initialHeads[,,i], file = paste0(Starting_Heads_dir, 'L',i,'_initial_head.txt'), append = F, quote = F, sep = '\t', row.names = F, col.names = F)
# }
# }

