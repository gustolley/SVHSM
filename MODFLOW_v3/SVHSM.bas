# MODFLOW Basic (BAS6) Package
# Data set created 2021-10-03 using MODFLOW Pre-Processing.R
#
BEGIN OPTION
  START_DATE 1999-10-01
  NO_FAILED_CONVERGENCE_STOP
  BUDGETDB MODFLOW_Budget.dat
END
OPEN/CLOSE  .\active_cells\L1_active_cells.txt  1  (FREE)  -1     ! Active cell matrix for layer 1
OPEN/CLOSE  .\active_cells\L2_active_cells.txt  1  (FREE)  -1     ! Active cell matrix for layer 2
OPEN/CLOSE  .\active_cells\L3_active_cells.txt  1  (FREE)  -1     ! Active cell matrix for layer 3
OPEN/CLOSE  .\active_cells\L4_active_cells.txt  1  (FREE)  -1     ! Active cell matrix for layer 4
OPEN/CLOSE  .\active_cells\L5_active_cells.txt  1  (FREE)  -1     ! Active cell matrix for layer 5
OPEN/CLOSE  .\active_cells\L6_active_cells.txt  1  (FREE)  -1     ! Active cell matrix for layer 6
OPEN/CLOSE  .\active_cells\L7_active_cells.txt  1  (FREE)  -1     ! Active cell matrix for layer 7
OPEN/CLOSE  .\active_cells\L8_active_cells.txt  1  (FREE)  -1     ! Active cell matrix for layer 8
OPEN/CLOSE  .\active_cells\L9_active_cells.txt  1  (FREE)  -1     ! Active cell matrix for layer 9
OPEN/CLOSE  .\active_cells\L10_active_cells.txt  1  (FREE)  -1     ! Active cell matrix for layer 10
OPEN/CLOSE  .\active_cells\L11_active_cells.txt  1  (FREE)  -1     ! Active cell matrix for layer 11
OPEN/CLOSE  .\active_cells\L12_active_cells.txt  1  (FREE)  -1     ! Active cell matrix for layer 12
-9999     ! Default head value for inactive cells (HNOFLO)
OPEN/CLOSE  .\starting_heads\L1_initial_head.txt  1  (FREE)  -1     # Initial heads for layer 1
OPEN/CLOSE  .\starting_heads\L2_initial_head.txt  1  (FREE)  -1     # Initial heads for layer 2
OPEN/CLOSE  .\starting_heads\L3_initial_head.txt  1  (FREE)  -1     # Initial heads for layer 3
OPEN/CLOSE  .\starting_heads\L4_initial_head.txt  1  (FREE)  -1     # Initial heads for layer 4
OPEN/CLOSE  .\starting_heads\L5_initial_head.txt  1  (FREE)  -1     # Initial heads for layer 5
OPEN/CLOSE  .\starting_heads\L6_initial_head.txt  1  (FREE)  -1     # Initial heads for layer 6
OPEN/CLOSE  .\starting_heads\L7_initial_head.txt  1  (FREE)  -1     # Initial heads for layer 7
OPEN/CLOSE  .\starting_heads\L8_initial_head.txt  1  (FREE)  -1     # Initial heads for layer 8
OPEN/CLOSE  .\starting_heads\L9_initial_head.txt  1  (FREE)  -1     # Initial heads for layer 9
OPEN/CLOSE  .\starting_heads\L10_initial_head.txt  1  (FREE)  -1     # Initial heads for layer 10
OPEN/CLOSE  .\starting_heads\L11_initial_head.txt  1  (FREE)  -1     # Initial heads for layer 11
OPEN/CLOSE  .\starting_heads\L12_initial_head.txt  1  (FREE)  -1     # Initial heads for layer 12
