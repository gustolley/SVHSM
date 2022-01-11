PROGRAM SWBM
  
! Scott valley soil water budget model (SWBM)
! 
! *****************************************************************************
! Input and Output is in Metric units:
! 
!                        [L]   =  meters
!                        [L^2] =  squared meters
!                        [L^3] =  cubic meters
!                        [T]   =  days
  
! subwn      : watershed name
! SWBM_LU    : 
! rotation   : 
! irr_type   : 
! tot_irr efficiency coefficients: 
! area       : 
! watersource: 
! whc        : 

  USE define_fields
  USE irrigation
  USE SWBM_output
  
  IMPLICIT NONE

  INTEGER :: nmonths, numdays, WY, month, jday, i, im, nrows, ncols, ncmds, dummy, WYstart
  INTEGER :: n_wel_param, num_daily_out, unit_num, num_MAR_fields, nSFR_inflow_segs
  INTEGER, ALLOCATABLE, DIMENSION(:,:) :: rch_zones, Discharge_Zone_Cells
  INTEGER, ALLOCATABLE, DIMENSION(:)   :: MAR_fields, ip_daily_out, ndays, SFR_inflow_segs
  REAL :: stn_precip, Total_Ref_ET, MAR_vol
  REAL, ALLOCATABLE, DIMENSION(:)  :: drain_flow, max_MAR_field_rate, moisture_save
  REAL :: start, finish
  CHARACTER(10) :: SFR_Template, scenario, suffix, date_text
  CHARACTER(20) :: model_name
  CHARACTER(30) :: filename
  CHARACTER(400) :: cmd
  CHARACTER(50), ALLOCATABLE, DIMENSION(:) :: daily_out_name
  INTEGER, DIMENSION(31) :: ET_Active
  LOGICAL :: MAR_active, ILR_active, daily_out_flag, inflow_is_vol, ag_wells_specified
  REAL :: eff_precip
 
  CALL cpu_time(start)
  open(unit=10, file = 'system_commands.txt', status = 'old')
  read(10,*) ncmds
  do i=1, ncmds
  	read(10, '(A400)') cmd
  	CALL execute_command_line(trim(cmd))
  enddo
  close(10)
  
  open(unit=800, file='SWBM.log')                         ! Open record file for screen output
  eff_precip = 0.
  Total_Ref_ET = 0.
  open(unit=10, file='general_inputs.txt', status='old')
  read(10, *) ! read header into nothing 
  read(10, *) model_name, WYstart, npoly, nlandcover, nAgWells, nMuniWells, nSubws, &
  inflow_is_vol, nSFR_inflow_segs, nmonths, nrows, ncols, SFR_Template, scenario
  close (10)
  print*, SFR_Template
  if (trim(SFR_Template)/='UCODE' .and. trim(SFR_Template)/='PEST') then 
    	write(*,*)'Invalid Template File Format Variable in general_inputs.txt'
    	write(800,*)'Invalid Template File Format Variable in general_inputs.txt'
    	CALL EXIT
  endif

  if (trim(scenario)=='basecase' .or. trim(scenario)=='Basecase' .or. trim(scenario)=='BASECASE') then            ! Set logicals for Scenario type
    MAR_active=  .FALSE.  
    ILR_active = .FALSE.
  else if(trim(scenario)=='MAR' .or. trim(scenario)=='mar') then
    MAR_active=  .TRUE. 
    ILR_active = .FALSE.
  else if (trim(scenario)=='ILR' .or. trim(scenario)=='ilr') then
    MAR_active=  .FALSE.
  	ILR_active = .TRUE.
  else if (trim(scenario)=='MAR_ILR' .or. trim(scenario)=='mar_ilr') then
  	 MAR_active=  .TRUE.       
     ILR_active = .TRUE.    
  else if(trim(scenario).ne.'basecase' .or. trim(scenario).ne.'Basecase' .or. trim(scenario).ne.'BASECASE' &      ! Exit program if incorrect scenario type
         .or. trim(scenario).ne.'MAR' .or. trim(scenario).ne.'mar' &
         .or. trim(scenario).ne.'ILR' .or. trim(scenario).ne.'ilr' &
         .or. trim(scenario).ne.'MAR_ILR' .or. trim(scenario).ne.'mar_ilr' ) then
    write(*,*)'Unknown scenario input in general_inputs.txt'
    write(800,*)'Unknown scenario input in general_inputs.txt'
    CALL EXIT
  endif
  write(*,'(2a10)')'Scenario: ',trim(scenario)
  write(800,'(2a10)')'Scenario: ',trim(scenario)
  SFR_Template = TRIM(SFR_Template)
  write(*,'(A27, A6)') 'SFR Template File Format = ',SFR_Template
  write(800,'(A27, A6)') 'SFR Template File Format = ',SFR_Template
  write(*,'(A7,I6,A14,I4,A12,I5,A14,I5,A12,I3,A10,I5)') "npoly =", npoly, "nlandcover =", nlandcover, "nAgWells =", nAgWells, &
  "nMuniWells =", nMuniWells, "nSubws =", nSubws, "nmonths =", nmonths
  write(800,'(A7,I6,A14,I4,A12,I5,A14,I5,A12,I3,A10,I5)') "npoly =", npoly, "nlandcover =", nlandcover, "nAgWells =", nAgWells, &
  "nMuniWells =", nMuniWells, "nSubws =", nSubws, "nmonths =", nmonths
  ! [NEED TO UPDATE CODE FOR MNW2 PACKAGE] open (unit=536, file="MNW2_template.txt", status="old")     
  ! [NEED TO UPDATE CODE FOR MNW2 PACKAGE] read(536,*) ! Read heading line into nothing
  ! [NEED TO UPDATE CODE FOR MNW2 PACKAGE] read(536,*)param_dummy,n_wel_param  ! read in number of well parameters (for printing later)
  ! [NEED TO UPDATE CODE FOR MNW2 PACKAGE] close(536)
  open(unit=10, file='ndays.txt', status = 'old')
  read(10,*)   ! read headers into nothing
  ALLOCATE(ndays(nmonths))
  do i=1, nmonths
  	read(10,*) dummy, ndays(i)
  enddo
  close(10)
  
  ALLOCATE(rch_zones(nrows,ncols))
  ALLOCATE(Discharge_Zone_Cells(nrows,ncols))
  ALLOCATE(drain_flow(nmonths))
  
  open(unit=10,file='recharge_zones.txt',status='old')      ! Read in MODFLOW recharge zone matrix
  read(10,*) rch_zones
  close(10)
  ! [NEED TO RECODE FOR SIERRA VALLEY MODEL] open(unit=218,file='ET_Cells_DZ.txt',status='old')      ! Read in MODFLOW recharge zone matrix
  ! [NEED TO RECODE FOR SIERRA VALLEY MODEL] read(218,*) Discharge_Zone_Cells
  ! [NEED TO RECODE FOR SIERRA VALLEY MODEL] close(218)
  open(unit=85, file = 'SFR_subws_flow_partitioning.txt', status = 'old')
  read(85,*)  ! read header into nothing
  CALL initialize_streams(nSubws, nSFR_inflow_segs)
  CALL read_landcover_table(nlandcover)
  CALL readpoly(npoly, nrows, ncols, rch_zones)    ! Read in field info
  CALL initialize_wells(npoly, nAgWells, nMuniWells)                       ! Read in Ag well info
  open(unit=82, file = 'polygon_landcover_ids.txt', status = 'old')
  read(82,*)  ! read header into nothing
  
  if (MAR_active) then
    open(unit=10,file='MAR_Fields.txt',status='old')      ! Read in MAR recharge matrix
    read(10,*) num_MAR_fields, MAR_vol
    ALLOCATE(MAR_fields(num_MAR_fields))                  ! Array of MAR field polygon IDs
    ALLOCATE(max_MAR_field_rate(num_MAR_fields))          ! Array of maximum infiltration rate for MAR fields (1/10th lowest SSURGO value)
    ALLOCATE(moisture_save(npoly))                        ! Array of soil-swc needed to recalculate recharge for MAR fields
    moisture_save = 0.                                    ! Initialize array
    do i=1, num_MAR_fields
      read(10,*)MAR_fields(i), max_MAR_field_rate(i)
    enddo
    close(10)
  endif

  open(unit=887,file='precip.txt', status = 'old')
  read(887,*)  ! read header into nothing                   
  open(unit=88,file='ref_et.txt', status = 'old')
  read(88,*)  ! read header into nothing
  open(unit=79, file='kc_values.txt', status = 'old')   
  read(79,*)  ! read header into nothing

  open(unit=599, file = 'print_daily.txt', status = 'old')
  read(599,*) num_daily_out, daily_out_flag
  ALLOCATE(ip_daily_out(num_daily_out))
  ALLOCATE(daily_out_name(num_daily_out))
  if (daily_out_flag) then
  	 do i=1, num_daily_out
  	 	 unit_num =  599 + i 
  	   read(599,*)ip_daily_out(i), daily_out_name(i)
  	   daily_out_name(i) = trim(daily_out_name(i)) // '_daily_out.dat'
  	   open(unit=unit_num, file=daily_out_name(i))
  	   write(unit_num,*)'field_id  effective_precip  streamflow  SW_irrig  GW_irr  total_irr  rch  swc  pET',&
  	                    '  aET  deficiency  residual  field_capacity  subws_ID  SWBM_LU  landcover_id'    
    enddo
  endif

  CALL output_files(model_name, daily_out_flag)
  open (unit=220, file='Drains_m3day.txt')
  read(220,*)                  ! Read header into nothing
  fields%irr_flag = 0          ! Initialize tot_irr flag array
  month = 10                   ! Initialize month variable to start in October
  
  open(unit=801, file= trim(model_name)//'.mnw2', Access = 'append', status='old')       
  WY = WYstart   ! water year
  do im=1, nmonths                ! Loop over each month
    read(82,*) date_text, fields(:)%landcover_id        ! read in landuse type
    read(85,*) date_text, SFR_allocation(:)%frac_subws_flow        ! read in multiplier for converting remaining subwatershed flows to SFR inflows
    read(537,*)  date_text, ag_wells_specified, ag_wells(:)%specified_volume 
    read(539,*) date_text, muni_wells(:)%specified_volume
    if (im==1) CALL initial_conditions                  ! initialize soil-water content for fields 
    if (month==10) then
      CALL zero_year           ! If October zero out yearly accumulated volume
    elseif (month==13) then
    	month = 1                ! Reset month to January
    endif
    numdays = ndays(im)        ! Number of days in the current month
    Total_Ref_ET = 0.          ! Reset monthly Average ET
    CALL zero_month                               ! Zero out monthly accumulated volume
    if (month==10) CALL zero_year                 ! If October zero out yearly accumulated volume                   
    CALL read_monthly_stream_inflow(inflow_is_vol, numdays)
    write(*,'(a15, i3,a13,i2,a18,i2)')'Stress Period: ',im,'   Month ID: ',month,'   Length (days): ', numdays
    write(800,'(a15, i3,a13,i2,a18,i2)')'Stress Period: ',im,'   Month ID: ',month,'   Length (days): ', numdays
    read(220,*)drain_flow(im)                       ! Read drain flow into array         
    do jday=1, numdays                              ! Loop over days in each month
      if (jday==1) monthly%ET_active = 0            ! Set ET counter to 0 at the beginning of the month. Used for turning ET on and off in MODFLOW so it is not double counted.    
      daily%ET_active  = 0                          ! Reset ET active counter
      daily%tot_irr = 0.                            ! Reset daily tot_irr value to zero
      daily%gw_irr = 0.                             ! Reset daily gw_irr value to zero
      daily%effprecip = 0.                          ! Reset daily effective precip value to zero
      daily%pET = 0.                                ! Reset daily ET value to zero
      daily%recharge = 0.                           ! Reset daily recharge value to zero 
      daily%runoff = 0.                             ! Reset daily runoff value to zero
      read(88,*) date_text, ETo     
      Total_Ref_ET = Total_Ref_ET + ETo        ! Increment monthly ET rate         
      read(887,*) stn_precip
      read(79,*) date_text, crops(:)%daily_kc 
      daily%effprecip = stn_precip * fields%precip_fact
      daily%pET=ETo*crops(fields%landcover_id)%daily_kc*crops(fields%landcover_id)%kc_mult                        ! Set ET to current value for the day
      do ip=1, npoly
	    	if (daily(ip)%effprecip < (0.2*ETo)) daily(ip)%effprecip = 0        
        if (ILR_active) then
          ! CALL IRRIGATION_ILR(ip, month, jday, eff_precip)
	      else
	        CALL IRRIGATION_RULESET(ip, month, jday)
	      endif 
	      CALL water_budget(ip,jday,month,moisture_save,MAR_active)   
        if (month==12 .and. jday==31 .and. ip==npoly) then               ! If last day of the year, set tot_irr flags and logical to zero
		      fields%irr_flag = 0         
		      irrigating = .false.           
 	        CALL IRR2CP(WY)                          ! Convert fields to center pivot irrigation
	      endif
       enddo              ! End of polygon loop! if (MAR_active) then 
      ! CALL MAR(month, num_MAR_fields, MAR_fields, max_MAR_field_rate, MAR_vol, eff_precip, jday, moisture_save)
      if (daily_out_flag) CALL daily_out(num_daily_out,ip_daily_out)              ! Print Daily Output for Selected Fields
      CALL groundwater_pumping(jday, nAgWells, npoly, numdays, daily_out_flag, ag_wells_specified)      ! Assign gw_irr to wells
	    CALL monthly_SUM                                                            ! add daily value to monthly total (e.g., monthly%tot_irr = monthly%tot_irr + daily%tot_irr)
      CALL annual_SUM                                                             ! add daily value to annual total (e.g., yearly%tot_irr = yearly%tot_irr + daily%tot_irr)
      if (jday==numdays) then                                         
      	CALL SFR_streamflow(npoly, numdays, nSubws, nSegs, nSFR_inflow_segs)      ! Convert remaining surface water and runoff to SFR inflows at end of the month	
        ann_spec_ag_vol = ann_spec_ag_vol + SUM(ag_wells%specified_volume)	      ! add monthly specified ag pumping volume to annual total
        ann_spec_muni_vol = ann_spec_muni_vol + SUM(muni_wells%specified_volume)  ! add monthly specified volume to annual total
      endif
    enddo             ! End of day loop  
    CALL convert_length_to_volume
    CALL monthly_out_by_field(im)
    CALL write_MODFLOW_RCH(im,numdays,nrows,ncols,rch_zones)
    CALL print_monthly_output(im, nlandcover, nSubws)
    CALL write_MODFLOW_SFR(im, nmonths, nSegs, model_name)
    CALL write_UCODE_SFR_template(im, nmonths, nSegs, model_name)   ! Write JTF file for UCODE 
    ! CALL write_MODFLOW_WEL(im, month, nAgWells, n_wel_param, model_name)       
    CALL write_MODFLOW_MNW2(im, nAgWells, nMuniWells, ag_wells_specified)          
    if (month==9) then
    CALL print_annual(WY, ag_wells_specified)        ! print annual values at the end of September
      WY = WY +1
    endif
    month = month + 1
    call flush
  enddo                  ! End of month loop
  CALL cpu_time(finish)
  write(*,'(A23,F6.2,A8)')'Model run completed in ',((finish-start)/60),' minutes'
  write(800,'(A23,F6.2,A8)')'Model run completed in ',((finish-start)/60),' minutes'
  
END PROGRAM SWBM