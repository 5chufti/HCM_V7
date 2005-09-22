!
!	P_to_P_calculation.f90								P. Benner		03.02.2004
!														G.H.			22.09.2005
!
!
!	Subroutine to calculate the field strength (pont to point calculation).
!
!
!	This module calculates the field strength between point A (Tx)
!	and point B (Rx) according to Annex 5 of the Agreement 2003.
!
!	Input values
!	
!	C_mode				Mode of calculation
!	LongTx				Longitude of Tx
!	LatTx				Latitude  of Tx
!	LongRx				Longitude of Rx
!	LatRx				Latitude  of Rx
!	Rad_of_Rx_serv_area	Radius of Rx service area
!	Calculated_FS		Calculated field strength
!	H_Rx_input			Input value of height above sea level of Rx site
!	H_AntTx				Antenna height of Tx
!	H_AntRx				Antenna height of Rx
!	Ant_typ_H_Tx		Type of Tx antenna horizontal			(9XH) (Tx)		
!	Ant_typ_V_Tx		Type of Rx antenna horizontal			(9XV) (Tx)
!	Azi_Tx_input		Azimuth of maximum radiation			 (9A) (Tx)
!	Ele_Tx_input		Elevation angle of main radiation		 (9B) (Tx)
!	Type_of_Tx_ant		Type of Tx antenna (E or I)
!	Max_power			Maximum radiated power					(8B1) (Tx)
!	D_sea_input			Input value for distance over sea
!
!                                                          
!
!	Output values
!
!	HCM_Error			Error value (see list)
!	Distance			Distance between point A and B in km
!	Dir_Rx_Tx			Direction from Rx to Tx
!	Dir_Tx_Rx			Direction from Tx to Rx
!	Rx_serv_area		Service area of Rx
!	New_LongTx			New Tx longitude (calculated)
!	New_LatTx			New Tx latitude (calculated)
!	New_LongRx			New Rx longitude (calculated)
!	New_LatRx			New Rx latitude (calculated)
!	Coo_Tx_new			New Tx coordinates (ASCII)
!	Coo_Rx_new			New Rx coordinates (ASCII)
!	H_Datab_Rx			Height of Rx site from terrain database
!	H_Rx				Height of Rx site used during all calculations
!						(from input value or from terrain database)
!	V_angle_Tx_Rx		Vertical angle from Tx to Rx
!	Tx_Elevation		Elevation of Tx antenna
!	V_diff_angle_Tx_Rx	Difference angle Tx elevation - vertical angle Tx to Rx
!	Tx_Azimuth			Azimuth of Tx antenna
!	H_diff_angle_Tx_Rx	Difference angle Tx azimuth - horizontal angle Tx to Rx
!	Tx_ant_corr			Correction according Tx antenna
!	Tx_ant_type_corr	Correction according Tx antenna type (E or I)
!	Power_to_Rx			Power in direction to Rx
!	Free_space_FS		Free space field strength
!	Calculated_FS		Calculated field strength
!	T_Prof(x)			Terrain profile (heights)
!	PN					Number of profile elements
!	Tx_TCA				Transmitter terrain clearance angle
!	Rx_TCA				Receiver terrain clearance angle
!	Tx_TCA_corr			Transmitter terrain clearance angle correction factor
!	Rx_TCA_corr			Receiver terrain clearance angle correction factor
!	Heff_Tx				Effective transmitter antenna height
!	Heff_Rx				Effective receiver antenna height
!	Heff				Effective antenna height used for calculation of field strength
!	D_sea_calculated	Calculated (or from input) distance over sea
!	Dh					Terrain irregularity
!	Dh_corr				Correction factor according to delta h
!	Land_FS_1kW			Land field strength for 1 kW
!	Sea_FS_1kW			Sea field strength for 1 kW
!	Land_FS				Land field strength
!	Sea_FS				Sea field strength
!    
!
!	HCM_error values:
!
!	0		no error
!	36		error opening terrain- or morphological data file (data not available)
!	200		error in longitude (in 'Point_height' or 'Point_type' subroutine) 
!	210		error in latitude (in 'Point_height' or 'Point_type' subroutine)
!	220		error reading record (in 'Point_height' or 'Point_type' subroutine)
!	300		latitude is not in range of 0.0 - 90.0
!			(in 'Point_height' or 'Point_type' subroutine)
!	400		height is missing (-9999) (in 'Point_height' subroutine)
!	1000	Distance between Tx and Rx = 0. Calculations not possible
!	1001	Error in geographical coordinates (Tx longitude, degrees)
!	1002	Error in geographical coordinates (Tx longitude, minutes)
!	1003	Error in geographical coordinates (Tx longitude, seconds)
!	1004	Error in geographical coordinates (Tx longitude, E/W)
!	1005	Error in geographical coordinates (Tx latitude, degrees)
!	1006	Error in geographical coordinates (Tx latitude, minutes)
!	1007	Error in geographical coordinates (Tx latitude, seconds)
!	1008	Error in geographical coordinates (Tx latitude, N/S)
!	1009	Error in Tx antenna height
!	1010	Error in transmitting frequency value
!	1011	Error in transmitting frequency unit
!	1012	Error in radius of service area of Tx
!	1013	Error in input value height of Tx site above sea level
!	1014	Error in geographical coordinates (Rx longitude, degrees)
!	1015	Error in geographical coordinates (Rx longitude, minutes)
!	1016	Error in geographical coordinates (Rx longitude, seconds)
!	1017	Error in geographical coordinates (Rx longitude, E/W)
!	1018	Error in geographical coordinates (Rx latitude, degrees)
!	1019	Error in geographical coordinates (Rx latitude, minutes)
!	1020	Error in geographical coordinates (Rx latitude, seconds)
!	1021	Error in geographical coordinates (Rx latitude, N/S)
!	1022	Error in Rx antenna height
!	1023	Error in reception frequency value
!	1024	Error in reception frequency unit
!	1025	C_mode is out of range
!	1026	Error in input value of permissible field strength
!	1027	Error in input value of maximum cross border range
!	1028	The distance is greater than 1000 km. Calculations not possible
!	1029	Error in radius of Rx service area
!	1030	Error in input value Rx site height above sea level
!	1031	Error in Tx elevation
!	1032	Error in Tx azimuth
!	1033	Error in typ of Tx antenna (E/I)
!	1034	Error in power
!	1035	Error in input value of distance over sea
!	1036	The 'xxx.ALL' borderline file for Tx is missing
!	1037	The 'xxx.ALL' borderline file for Rx is missing
!	1038	Error ib type of antenna (TR20-08)
!	1039	Error in input data of correction factor according frequency difference
!	1040	Channel spacing outside definition range (Rx)
!	1041	Channel spacing outside definition range (Tx)
!	1042	Error in Rx elevation
!	1043	Error in Rx azimuth
!	1044	Error in Rx type of antenna ("E" or "I")
!	1045	Error in gain of Rx antenna
!	1046	Error in input data of depolarization loss
!	1047	Distance to borderline is too long
!	1048	Selected line data not available
!	1049	Error in line data
!
!	2000	wrong Figure_frequency (from Get_figure_FS_value)
!	2001	wrong Time_percentage  (from Get_figure_FS_value)
!	2002	wrong Sea_temperature  (from Get_figure_FS_value)
!	2003	wrong Figure_Heff      (from Get_figure_FS_value)
!	2004	wrong Figure_distance  (from Get_figure_FS_value)
!
!	Info values:
!
!	Info(1)		No height of Tx site is given,
!				height is taken from the terrain database
!	Info(2)		Height of Tx site differs from height of terrain database
!	Info(3)		Height of Tx site differs more than 10%,
!				calculated values may be (extremely) wrong!
!	Info(4)		Frequency out of range of table in Annex 1
!	Info(5)		Input value of permissible field strength is used
!	Info(6)		Input value of maximum cross border range is used
!	Info(7)		Distance between Tx and Rx is less than both service area radius;
!				field strength is set to 999.9
!	Info(8)		No height of Rx site is given, height is from the terrain database
!	Info(9)		Height of Rx site differs from height of terrain data
!	Info(10)	Rx site height differs more than 10%,
!				calculated values may be (extremely) wrong!
!	Info(11)	Free space field strength used because distance < 1km
!	Info(12)	Free space field strength is used, because 1st Fresnel zone is free
!	Info(13)	Distance over sea is greater than total distance.
!				Distance between Tx and Rx is used!
!	Info(14)	Input value of correction factor according frequency difference is used
!	Info(15)	Frequency difference outside definition range; 82 dB is used
!	Info(16)	Calculated distance over sea is set to 0 because of missing
!				morphological data.
!	Info(17)	Tx channel spacing outside definition range, 25 kHz is used!
!
!
!
!	Called subroutines:
!	Calc_distance, Calc_direction, Position_of_mobile, CooConv,
!	Point_height, Antenna_correction, PROFILE, TCA_correction_calculation,
!	Dh_calculation, Dh_correction, Get_FS_from_figures
!
!
!	**********************************************************************
!
	SUBROUTINE P_to_P_calculation ( LongTx, LatTx, LongRx, LatRx)
!
	IMPLICIT		   NONE
!
!	Include the interface definitions:
	INCLUDE				'HCM_MS_V7_definitions.F90'
!
	INTEGER				IOS, I, J, J1, D1, D2, HSUM
	INTEGER*4			I1, I2
	REAL				X, x_TCA, DS1, A1, A2, Ax
	REAL				HDZ(10002), HDC(10002), HDF(10002)
	REAL				A(11), Factor_of_path_over_sea
	REAL				Land_FS_1kW, Sea_FS_1kW
	DOUBLE PRECISION	LongTx, LatTx, LongRx, LatRx ,CI
	DOUBLE PRECISION	New_LongTx, New_LatTx, New_LongRx, New_LatRx
	LOGICAL				Free, null
	CHARACTER*1			Point_Type
!
!	*****************************************************************
!
!	Interpolation factor for mixed path (<10%)
	DATA A   /0.0,0.07,0.13,0.2,0.29,0.37,0.46,0.56,0.67,0.8,1.0/
!
!	Clear all Info(i)'s for P_to_P_calculation subroutine
!
	Info(1)  = .FALSE.
	Info(2)  = .FALSE.
	Info(3)  = .FALSE.
	Info(7)  = .FALSE.
	Info(8)  = .FALSE.
	Info(9)  = .FALSE.
	Info(10) = .FALSE.
	Info(11) = .FALSE.
	Info(12) = .FALSE.
	Info(13) = .FALSE.
	Info(16) = .FALSE.
!
	Calculated_FS = -999.9
!	Calculate new positions, if Tx or Rx or both are mobiles and not CBR or '99':
!	for CBR new positions are calculated in CBR_Coordinates !
	IF ((C_mode .NE. 99) .AND. ((C_mode .GE. 0) .OR. (D_to_border .GE. 0)) &
			.AND. ((Tx_serv_area .GT. 0.0) .OR. (Rx_serv_area .GT. 0.0))) THEN
!		Calculate new co-ordinates:
		CALL Position_of_mobile ( LongTx, LatTx, LongRx, LatRx, &
					New_LongTx, New_LatTx, New_LongRx, New_LatRx )
		IF (HCM_Error .NE. 0) RETURN
	ELSE
		New_LongTx = LongTx
		New_LatTx  = LatTx
		New_LongRx = LongRx
		New_LatRx  = LatRx
	END IF
!
	CALL CooConv (New_LongTx, New_LatTx, Coo_Tx_new)
	CALL CooConv (New_LongRx, New_LatRx, Coo_Rx_new)
!
!	GOTO 10
!-------------------------------------------------------------------
!	Get the longitude of transmitter 'LongTX':
	READ (Coo_Tx_new(1:3), '(F3.0)', IOSTAT = IOS) New_LongTx
	READ (Coo_Tx_new(5:6), '(F2.0)', IOSTAT = IOS) CI
	New_LongTx = New_LongTx + CI / 6.0D1
	READ (Coo_Tx_new(7:8), '(F2.0)', IOSTAT = IOS) CI
	New_LongTx = New_LongTx + CI / 3.6D3
	IF (Coo_Tx_new(4:4) .EQ. 'W') New_LongTx = 3.6D2 - New_LongTx
!
!	Get the latitude of transmitter 'LatTx':
	READ (Coo_Tx_new(9:10),  '(F2.0)', IOSTAT = IOS) New_LatTx
	READ (Coo_Tx_new(12:13), '(F2.0)', IOSTAT = IOS) CI
	New_LatTx = New_LatTx + CI / 6.0D1
	READ (Coo_Tx_new(14:15), '(F2.0)', IOSTAT = IOS) CI
	New_LatTx = New_LatTx + CI / 3.6D3
!
!   Get the longitude of receiver 'LongRx':
	READ (Coo_Rx_new(1:3), '(F3.0)', IOSTAT=IOS) New_LongRx
	READ (Coo_Rx_new(5:6), '(F2.0)', IOSTAT=IOS) CI
	New_LongRx = New_LongRx + CI / 6.0D1
	READ (Coo_Rx_new(7:8), '(F2.0)', IOSTAT=IOS) CI
	New_LongRx = New_LongRx + CI / 3.6D3
	IF (Coo_Rx_new(4:4) .EQ. 'W') New_LongRx = 3.6D2 - New_LongRx
!
!	Get the latitude of receiver 'LATB':
	READ (Coo_Rx_new(9:10),  '(F2.0)', IOSTAT=IOS) New_LatRx
	READ (Coo_Rx_new(12:13), '(F2.0)', IOSTAT=IOS) CI
	New_LatRx = New_LatRx + CI / 6.0D1
	READ (Coo_Rx_new(14:15), '(F2.0)', IOSTAT=IOS) CI
	New_LatRx = New_LatRx + CI / 3.6D3
!
!------------------------------------------------------------------------
!	Checking of Tx site height:
!		Height of Tx site above sea level:
10	CALL Point_height (New_LongTx, New_LatTx, H_Datab_Tx, HCM_error)

	IF (HCM_error .NE. 0) RETURN
!
	IF (H_Tx_input .EQ. '    ') THEN
		H_Tx = H_Datab_Tx
		Info(1) = .TRUE.
!		No height of Tx site is given, height is taken from the terrain database.
	ELSE
		READ (H_Tx_input, '(I4)', IOSTAT=IOS) H_Tx
		IF (IOS .NE. 0) THEN
			HCM_error = 1013
!			Error in input value height of Tx site above sea level.
			RETURN
		END IF
		IF (H_Tx .NE. H_Datab_Tx) THEN
			IF (ABS(H_Tx-H_Datab_Tx) .LE. H_Tx/10) THEN
				Info(2) = .TRUE.
!				Height of Tx site differs from height of terrain database.
			ELSE
				Info(3) = .TRUE.
!				Height of Tx site differs more than 10%,
!				calculated values may be (extremely) wrong!
			END IF
		END IF
	END IF
!	Checking Rx site height
!		Height of Rx above sealevel
	CALL Point_height (New_LongRx, New_LatRx, H_Datab_Rx, HCM_Error)
	IF (HCM_Error .NE. 0) RETURN
	IF (H_Rx_input .EQ. '    ') THEN
		H_Rx = H_Datab_Rx
		Info(8) = .TRUE.
!		No height of Rx site is given, height is from the terrain database.
	ELSE
		READ (H_Rx_input, '(I4)', IOSTAT=IOS) H_Rx
		IF (IOS .NE. 0) THEN
			HCM_Error = 1030
!			Error in input value Rx site height above sea level.
			RETURN	
		END IF
		IF (H_Rx .NE. H_Datab_Rx) THEN
			IF (ABS(H_Rx-H_Datab_Rx) .LE. H_Rx/10) THEN
				Info(9) = .TRUE.
!				Height of Rx site differs from height of terrain data.
			ELSE
				Info(10) = .TRUE.
!				Rx site height differs more than 10%,
!				calculated values may be (extremely) wrong!
			END IF
		END IF
	END IF
!
!
!	Calculate distance and directions again (with new co-ordinates):
	CALL Calc_distance (New_LongTx, New_LatTx, New_LongRx, New_LatRx, Distance)
!
	IF (Distance .LE. 0.0D0) THEN
	  Calculated_FS = 999.9
!	  Distance between Tx and Rx = 0. Calculations not possible.
	  HCM_error = 1000   
	  RETURN
	END IF
!
	IF (Distance .GT. 1D3) THEN
	  Calculated_FS = -999.9
!	  The distance is greater than 1000 km. Calculations not possible.
	  HCM_error = 1028
	  RETURN
	END IF
!
!	Calculate the direction from Rx to Tx:
	CALL Calc_direction (New_LongRx, New_LatRx, New_LongTx, New_LatTx, Dir_Rx_Tx)
!
!	Calculate the direction from Tx to Rx:
	CALL Calc_direction (New_LongTx, New_LatTx, New_LongRx, New_LatRx, Dir_Tx_Rx)
!
!	***************************************************************
!	*							      
!	*             Free space field strength calculation           
!	*							      
!	***************************************************************
!
!	Calculation of power in direction of the receiver 'Power_to_Rx':
!
!
	IF ((Ant_typ_H_Tx .EQ. '000ND00') .AND. (Ant_typ_V_Tx .EQ. '000ND00')) THEN
		Tx_ant_corr   = 0.0
	ELSE
!	Setting of the vertical angle:
!		IF ((C_mode .GE. 0) .AND. (Tx_serv_area .EQ. 0.0) .AND. &
!			(Rx_serv_area .EQ. 0.0) .AND. (C_mode .NE. 99)) THEN
!			Normal point to point calculation
			V_angle_Tx_Rx = ATAND ((H_Rx + H_AntRx - H_Tx + H_AntTx) / (1E3 * Distance))
!		ELSE
!			Calculation to co-ordination lines or mobiles
!			V_angle_Tx_Rx = 0D0
!		END IF
		READ (Ele_Tx_input, '(F5.1)', IOSTAT=IOS) Tx_Elevation
		IF (IOS .NE. 0) THEN
			HCM_Error = 1031
!			Error in Tx elevation.
			RETURN
		END IF
		READ (Azi_Tx_input, '(F5.1)', IOSTAT=IOS) Tx_Azimuth
		IF (IOS .NE. 0) THEN
			HCM_Error = 1032
!			Error in Tx azimuth.
			RETURN
		END IF
		CALL Ctransf (Dir_Tx_Rx, Tx_Azimuth, V_angle_Tx_Rx, Tx_Elevation, H_diff_angle_Tx_Rx, V_diff_angle_Tx_Rx)
		CALL Antenna_correction (H_diff_angle_Tx_Rx, V_diff_angle_Tx_Rx, Ant_typ_H_Tx, Ant_typ_V_Tx, Tx_ant_corr, HCM_Error)
		IF (HCM_Error .NE. 0) RETURN
	END IF
!
!	'Tx_ant_corr' is calculated.
!
	Power_to_Rx = MaxPow - Tx_ant_corr
!
!	Calculation of free space field strength 'Free_space_FS':
!
	Free_space_FS = 77.0 - 20.0 * LOG10 (Distance) + Power_to_Rx
!
!	****************************************************************
!
!		Free space field strength "Free_space_FS" is calculated
!
!	****************************************************************
!
!	If distance is less than 1 km, use free space field strength:
!	
	IF (Distance .LT. 1.0D0) THEN
	  Calculated_FS = Free_space_FS
	  Info(11) = .TRUE.
!	  Free space field strength used because distance < 1km.
	  RETURN
	END IF
!
!	*****************************************************************
!	*								
!	*				Elements of the terrain profile	
!	*								
!	*****************************************************************
!
	Point_Type = 'e'	! for elevation data
	CALL PROFILE (New_LongTx, New_LatTx, New_LongRx, New_LatRx,  &
			T_Prof, HCM_Error, Point_Type)
	IF (HCM_Error .NE. 0) RETURN
!
!
!	*****************************************************************
!	*								
!	*Calculation the first Fresnel zone				
!	*								
!	*****************************************************************
!
!	First Fresnel zone is only calculated, if Tx and Rx are no mobiles
!	and if it is a point to point calculation:
	IF ((Tx_serv_area .EQ. 0.0) .AND. (Rx_serv_area .EQ. 0.0) .AND. &
		(C_Mode .GE. 0) .AND. (C_mode .NE. 99)) THEN
!	  First value = height of Tx, last value (PN) = height of Rx !
!	  J is number of points between Rx and Tx
	  J = PN - 2	
!	  Calculate first Fresnel zone
!	  Calculation of ho(z); add all ho to hz (='HDZ'):
!
	  DO I = 1, J
		HDZ(I) = REAL (T_Prof(I+1)) + PD*I*(Distance - PD*I) / 17.0
	  END DO
!
!
!	  Add 10 m to all hz except the 1st and last km of the distance:
!	  (If distance is less than 2 km, no addition is done !)
!
	  IF (Distance .GT. 2.0) THEN
		I1 = DNINT(1D0 / PD)
		I2 = DNINT((Distance - 1D0) / PD)
		DO I = I1, I2
		  HDZ(I) = HDZ(I) + 10.0
		END DO
	  END IF
!
!	  Calculate all hc ('HDC'):
!	  Calculate the 1st Fresnel zone 'HDF':
!	  Calculate all differences 'HDD':
!
	  DO I = 1, J
		HDC(I) = H_AntRx * PD * I / Distance - HDZ(I)
		X = PD*I*(Distance-PD*I)
		IF (X .LE. 0.0) THEN
			HDF(I) = 0.0
		  ELSE
			HDF(I) = 547.7 * SQRT ( X / (Distance * Tx_frequency) )
		END IF
	  END DO
!	check if fresnel zone hdf is touched by hdc
	  Free = .TRUE.
	  DO I = 1, J
		IF ((HDC(I) - HDF(I)) .LT. 0.0) THEN
		  Free = .FALSE.
		  EXIT
		END IF
	  END DO
!
	  IF (Free) THEN
!		1st Fresnel zone is free:
!		Free space field strength is used
		Calculated_FS = Free_space_FS
		Info(12) = .TRUE.
		RETURN
	  END IF
	END IF
!
!	**********************************************************************
!	*									
!	*  Calculation of the field strength according to the ITU-R method	
!	*									
!	**********************************************************************
!
	Tx_TCA	= 0.0
	Rx_TCA	= 0.0
	Tx_TCA_corr	= 0.0
	Rx_TCA_corr	= 0.0
!
!	***********************************************************
!	Calculate Terrain Clearance Angles and TCA correction factors
!	limitation of corr.factors is done in TCA_correction_calculation
!	***********************************************************
!
!	calculate transmitter clearance angle 'Tx_TCA' according to proceeding table
	IF (Tx_serv_area .EQ. 0.0) THEN
!	  Calculate Tx_TCA:
		Tx_TCA = -90.0
		IF (Distance .GE. 1.6D1) THEN
			J = DNINT(1.6D1 / PD)
		ELSE
			J = PN - 2
		END IF
		DO I = 1, J
			x_TCA = (REAL(T_Prof(1+I))-H_AntTx)/(SNGL(PD)*REAL(I)*1E3)
			x_TCA = ATAND (x_TCA)	! in degrees
			IF (x_TCA .GT. Tx_TCA) Tx_TCA = x_TCA
		END DO
!	Calculate correction factor:
	CALL TCA_correction_calculation (Tx_TCA, Tx_frequency, Tx_TCA_corr)
	END IF
!
!	calculate receiver clearance angle 'Rx_TCA' according to proceeding table
!
	IF ((Rx_serv_area .EQ. 0.0) .AND. (C_Mode .GE. 0) .AND. (C_mode .NE. 99)) THEN
!	  Calculate Rx_TCA:
		Rx_TCA = -90.0
		IF (Distance .GE. 1.6D1) THEN
			J = DNINT(1.6D1 / PD)
		ELSE
			J = PN - 2
		END IF
		DO I = 1, J
			x_TCA = (REAL(T_Prof(PN-I))-H_AntRx)/(SNGL(PD)*REAL(I)*1E3)
			x_TCA = ATAND (x_TCA)	! in degrees
			IF (x_TCA .GT. Rx_TCA) Rx_TCA = x_TCA
		END DO
!	Calculate correction factor:
	CALL TCA_correction_calculation (Rx_TCA, Rx_frequency, Rx_TCA_corr)
	ENDIF
!
!	************************************************
!	Effective antenna heights:
!	calculate effective antenna heights 
!	and correct according to limits in table
!	************************************************
!
	Heff_Tx = 0.0
	Heff_Rx = 0.0
	IF (Distance .LT. 1.5D1) THEN
	    D1 = NINT(REAL(PN) / 15.0)
		D2 = PN - 1
	ELSE
		D1 = DNINT(1D0 / PD)  
		D2 = DNINT(1.5D1 / PD)
	END IF
!
!	Transmitter:
	IF (Tx_serv_area .GT. 0.0) THEN
!		Height of a mobile 'hmTx'
		Heff_Tx = REAL(H_AntTx)
	ELSE
		  HSUM = 0
		  DO I = D1, D2
		    HSUM = HSUM + T_Prof(1+I)
		  END DO
		  Heff_Tx = H_AntTx - REAL(HSUM)/REAL(D2-D1+1)
	END IF
	IF (Heff_Tx .LT. 3.0) Heff_Tx = 3.0
!
!	Receiver: 
	IF ((C_Mode .GE. 0) .AND. (C_Mode .NE. 99)) THEN
!	  for point to point calculations
	  IF (Rx_serv_area .GT. 0) THEN
!		Height of a mobile 'hmRx'
		Heff_Rx = REAL(H_AntRx)
	  ELSE
		  HSUM = 0
		  DO I = D1, D2
		    HSUM = HSUM + T_Prof(PN-I)
		  END DO
		  Heff_Rx = REAL(H_AntRx) - REAL(HSUM)/REAL(D2-D1+1)
	  END IF
	ELSE
!	for line calculations
	  Heff_Rx = 10.0
	END IF
	IF (Heff_Rx .LT. 3.0) Heff_Rx = 3.0
!
!	heff for curves according to proceding table:
!	value of 0.3m results from 3m/10, 3m is done as min in Heff_Xx calc.
!
	Heff = (Heff_Tx * Heff_Rx) / 10.0
!
!
!
!	*****************************************************************
!	*								
!	*		Calculate distance over sea			
!	*								
!	*****************************************************************
!
!
	IF (D_sea_input .EQ. '     ') THEN
		D_sea_calculated = 0.0
!		Get the morphological profile:
		Point_Type = 'm'
		CALL PROFILE (New_LongTx, New_LatTx, New_LongRx, New_LatRx, &
			M_Prof, HCM_Error, Point_Type)
		IF (HCM_Error .EQ. 0) THEN
			null = .FALSE.
			DS1 = 0.0               
			J1 = 0
			IF (Distance .GT. 1.0D0) THEN
			  DO I = 1, PN      
!				Test, if morphological information = 'sea'
				IF (BITEST(M_Prof(I),0)) THEN
					IF (null) THEN
						J1 = J1 + 1 
					  ELSE
						J1 = 0
						null = .TRUE.
					END IF
				  ELSE                
					IF (null) THEN
					  DS1 = REAL(J1) * PD
					  D_sea_calculated = D_sea_calculated + DS1
					  DS1 = 0.0
					  J1 = 0 
					  null = .FALSE.
					END IF
				END IF
			  END DO                 
			  IF (null) THEN
				DS1 = REAL(J1) * PD 
				D_sea_calculated = D_sea_calculated + DS1
				null = .FALSE.
			  END IF
			END IF
		  ELSE
		    IF (HCM_Error .EQ. 36) THEN
			  D_sea_calculated = 0.0
			  Info(16) = .TRUE.
!			  Calculated distance over sea is set to 0 because of missing morphological data
			  HCM_Error = 0
			END IF
		END IF
	  ELSE              
		READ (D_sea_input, *, IOSTAT=IOS) D_sea_calculated
		IF (IOS .NE. 0) THEN
		  HCM_Error = 1035
!		  Error in input value of distance over sea
		  RETURN
		END IF
	END IF      
!
	IF (D_sea_calculated .GT. Distance) THEN
	  Info(13) = .TRUE.
!	  Distance over sea is greater than total
!	  distance. Distance between Tx and Rx is used!
	  D_sea_calculated = Distance
	END IF
!
!	*****************************************************************
!	*								
!	*	calc Delta h and Dh correction factor	
!	*								
!	*****************************************************************
!
	IF ((Distance .GT. 10.0) .AND. (D_sea_calculated .LT. Distance)) THEN
!		Calculate delta-h
		CALL Dh_calculation ()
		CALL Dh_orrection (Dh, Distance, Tx_frequency, Dh_corr)
	  ELSE
		Dh = 50.0
		Dh_corr = 0.0
	END IF
!
!	*****************************************************************
!	*								
!	*	Calculate the field strength (from the figures):	
!	*								
!	*****************************************************************
!
!	Get the land- and sea field strength for 1 kW:
	CALL Get_FS_from_figures ( Land_FS_1kW, Sea_FS_1kW )
	IF (HCM_Error .NE. 0) RETURN
!
!	Clearance angle corrections and delta h correction:
	Land_FS = Land_FS_1kW + Tx_TCA_corr + Rx_TCA_corr - Dh_corr
!
!	Corrections according to the power:
	Land_FS = Land_FS - 30.0 + Power_to_Rx
	Sea_FS = Sea_FS_1kW - 30.0 + Power_to_Rx
!
	IF (D_sea_calculated .EQ. 0.0) THEN
		Calculated_FS = Land_FS
		RETURN
	  ELSE
!		Mixed path calculation:
		IF (Time_percentage .LT. 10) THEN
!			Apply Annex 5, section 3.6 a
			Factor_of_path_over_sea = D_sea_calculated / Distance
			X  = Factor_of_path_over_sea * 10.0
			A1 = A(INT(X)+1)
			IF (X .LT. 10.0) THEN
				A2 = A(INT(X)+2)
			  ELSE
				A2 = A1
			END IF
			X = X - INT(X)
			Ax = A1 + X * (A2 - A1)
			Calculated_FS  = Land_FS + Ax * (Sea_FS - Land_FS)
			RETURN
		  ELSE
!			Apply Annex 5, section 3.6 b
			Calculated_FS = Land_FS * (Distance - D_sea_calculated) / &
					Distance + Sea_FS * D_sea_calculated / Distance
			RETURN
		END IF
	END IF
!
	END SUBROUTINE P_to_P_calculation
!
