!
!	P_to_P_calculation.f90								P. Benner
!														03.02.2004
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
	SUBROUTINE P_to_P_calculation ( LongTx, LatTx, LongRx, LatRx, T_L, M_L, &
									B_L, H_Tx_Ant_top, H_Rx_Ant_top )
!
	IMPLICIT		   NONE
!
!	Include the interface definitions:
	INCLUDE				'HCM_MS_V7_definitions.F90'
!
	INTEGER				IOS, I, J, J1, D1, D2, HSUM, NOH
	INTEGER*4			T_L, M_L, B_L, I1, I2
	REAL				X, x_TCA, hmTx, hmRx, DS1, A1, A2, Ax
	REAL				HDZ(10002), HDC(10002), HDF(10002)
	REAL				A(11), Factor_of_path_over_sea
	REAL				H_Tx_Ant_top, H_Rx_Ant_top
	REAL				Land_FS_1kW, Sea_FS_1kW, DI1, DI2
	DOUBLE PRECISION	XX, LongTx, LatTx, LongRx, LatRx
	DOUBLE PRECISION	New_LongTx, New_LatTx, New_LongRx, New_LatRx
	LOGICAL				Free, kdh_use, null
	CHARACTER*1			Point_Type
!
!	*****************************************************************
!
!	Interpolation factor for mixed path (<10%)
	DATA A   /0.0,0.07,0.13,0.2,0.29,0.37,0.46,0.56,0.67,0.8,1.0/
!
!
!	*****************************************************************
!	*																*
!	*	Calculate the distance 'Distance' between point A and B		*
!	*																*
!	*****************************************************************
!
	Coo_Tx_new = Coo_Tx
	Coo_Rx_new = Coo_Rx
	New_LongTx = LongTx
	New_LatTx  = LatTx
	New_LongRx = LongRx
	New_LatRx  = LatRx
!
!
!	Clear all Info(i)'s for P_to_P_calculation subroutine
!
	Info(7)  = .FALSE.
	Info(8)  = .FALSE.
	Info(9)  = .FALSE.
	Info(10) = .FALSE.
	Info(11) = .FALSE.
	Info(12) = .FALSE.
	Info(13) = .FALSE.
	Info(16) = .FALSE.
!
	CALL Calc_distance (LongTx, LatTx, LongRx, LatRx, Distance)
!
	IF (Distance .EQ. 0.0D0) THEN
!	  Distance between Tx and Rx = 0. Calculations not possible.
	  HCM_error = 1000   
	  RETURN
	END IF
!
	IF (Distance .GT. 1D3) THEN
	  HCM_error = 1028
!	  The distance is greater than 1000 km. Calculations not possible.
	  RETURN
	END IF
!
!	Calculate the direction from Rx to Tx:
	CALL Calc_direction (LongRx, LatRx, LongTx, LatTx, Dir_Rx_Tx)
!
!	Calculate the direction from Tx to Rx:
	CALL Calc_direction (LongTx, LatTx, LongRx, LatRx, Dir_Tx_Rx)
!
!	Calculate new positions, if Tx or Rx or both are mobiles:
!	Tx already calculated in HCM_MS_V7 subroutine !
!	Get radius of the Rx service area:
	IF (C_mode .GE. 0) THEN
		READ (Rad_of_Rx_serv_area, '(F5.0)', IOSTAT=IOS) Rx_serv_area
		IF (IOS .NE. 0) THEN
		  HCM_error = 1029
!		  Error in radius of Rx service area
		  RETURN
		END IF
	  ELSE
		Rx_serv_area = 0.0
	END IF
!
	IF ((C_mode .LT. 0) .AND. (D_to_border .LT. 0)) THEN
!		In case of CBR calculation, the position of the (mobile)
!		Tx is set in subroutine CBR_Coordinates !
	  ELSE
		IF ((Tx_serv_area .GT. 0.0) .OR. (Rx_serv_area .GT. 0.0)) THEN
!		  Calculate new co-ordinates:
		  CALL Position_of_mobile ( LongTx, LatTx, LongRx, LatRx, &
					New_LongTx, New_LatTx, New_LongRx, New_LatRx, B_L )
		  IF (HCM_Error .NE. 0) RETURN
		END IF
	END IF

!
	CALL CooConv (New_LongTx, New_LatTx, Coo_Tx_new)
	CALL CooConv (New_LongRx, New_LatRx, Coo_Rx_new)
!
	IF (Info(7)) THEN
!	  Calculated_FS = 999.9
!	  Distance between Tx and Rx is less than both service area radius.
	  RETURN
	END IF
!
!	Calculate distance and directions again (with new co-ordinates):
	CALL Calc_distance (New_LongTx, New_LatTx, New_LongRx, New_LatRx, Distance)
!
	IF (Distance .EQ. 0.0D0) THEN
	  Calculated_FS = 999.9
!	  Distance between Tx and Rx = 0. Calculations not possible.
	  HCM_error = 1000   
	  RETURN
	END IF
!
!	Calculate the direction from Rx to Tx:
	CALL Calc_direction (New_LongRx, New_LatRx, New_LongTx, New_LatTx, Dir_Rx_Tx)
!
!	Calculate the direction from Tx to Rx:
	CALL Calc_direction (New_LongTx, New_LatTx, New_LongRx, New_LatRx, Dir_Tx_Rx)
!
!
!	Height of Rx
	H_Rx = 0		! default value
	H_Datab_Rx = 0	! default value
!	If borderline calculations, no receiver height
	IF ((C_mode .GE. 0) .AND. (Rx_serv_area .EQ. 0.0) .AND. (C_mode .NE. 99)) THEN
	  CALL Point_height (LongRx, LatRx, H_Datab_Rx, HCM_Error, Topo_path, T_L)
	  IF (HCM_Error .NE. 0) RETURN
	  IF (H_Rx_input .EQ. '    ') THEN
		  H_Rx = H_Datab_Rx
		  Info(8) = .TRUE.
!		  No height of Rx site is given, height is from the terrain database.
		ELSE
		  READ (H_Rx_input, '(I4)', IOSTAT=IOS) H_Rx
		  IF (IOS .NE. 0) THEN
			HCM_Error = 1030
!			Error in input value Rx site height above sea level.
			RETURN	
		  END IF
		  IF (H_Rx .NE. H_Datab_Rx) THEN
			IF (ABS(H_Rx-H_Datab_Rx) .EQ. 1) THEN
				Info(9) = .TRUE.
!				Height of Rx site differs from height of terrain data.
			  ELSE
				IF (ABS(H_Rx-H_Datab_Rx) .LE. H_Rx/10) THEN
					Info(9) = .TRUE.
!					Height of Rx site differs from height of terrain data.
				  ELSE
					Info(10) = .TRUE.
!					Rx site height differs more than 10%,
!					calculated values may be (extremely) wrong!
				END IF
			END IF
		  END IF
	  END IF
	END IF
!
!
!	H_Rx = height of receiver site above sea level for all calculations.
!
!
!	***************************************************************
!	*															  *
!	*             Free space field strength calculation           *
!	*															  *
!	***************************************************************
!
!	Calculation of power in direction of the receiver 'Power_to_Rx':
!
!	Setting of the vertical angle:
	IF ((C_mode .GE. 0) .AND. (Tx_serv_area .EQ. 0.0) .AND. &
		(Rx_serv_area .EQ. 0.0)) THEN
!		Normal point to point calculation
		V_angle_Tx_Rx = ATAN(((H_Rx+H_AntRx)-(H_Tx+H_AntTx))/(1.0E3*Distance))
		V_angle_Tx_Rx = V_angle_Tx_Rx * 180.0 / 3.14159265
	  ELSE
!		Calculation on co-ordination lines
		V_angle_Tx_Rx = 0.0
	END IF
!
	IF ((Ant_typ_H_Tx .EQ. '000ND00') .AND. (Ant_typ_V_Tx .EQ. '000ND00')) THEN
		Tx_ant_corr   = 0.0
	  ELSE
		IF (Ant_typ_V_Tx .EQ. '000ND00') THEN
			V_diff_angle_Tx_Rx = 0.0
		  ELSE
			READ (Ele_Tx_input, '(F5.1)', IOSTAT=IOS) Tx_Elevation
			IF (IOS .NE. 0) THEN
			  HCM_Error = 1031
!			  Error in Tx elevation.
			  RETURN
			END IF
			V_diff_angle_Tx_Rx = Tx_Elevation - V_angle_Tx_Rx
			IF (V_diff_angle_Tx_Rx .LT. 0.0) V_diff_angle_Tx_Rx = 360.0 + V_diff_angle_Tx_Rx
		END IF
		IF (Ant_typ_H_Tx .EQ. '000ND00') THEN
			H_diff_angle_Tx_Rx = 0.0
		  ELSE
			READ (Azi_Tx_input, '(F5.1)', IOSTAT=IOS) Tx_Azimuth
			IF (IOS .NE. 0) THEN
			  HCM_Error = 1032
!			  Error in Tx azimuth.
			  RETURN
			END IF
			H_diff_angle_Tx_Rx = Dir_Tx_Rx - Tx_Azimuth
			IF (H_diff_angle_Tx_Rx .LT.   0.0) H_diff_angle_Tx_Rx = 360.0 + &
							H_diff_angle_Tx_Rx
			IF (H_diff_angle_Tx_Rx .GT. 360.0) H_diff_angle_Tx_Rx = &
							H_diff_angle_Tx_Rx - 360.0
		END IF
		DI1 = H_diff_angle_Tx_Rx
		DI2 = V_diff_angle_Tx_Rx
		CALL Antenna_correction (DI1, DI2, Ant_typ_H_Tx, Ant_typ_V_Tx, Tx_ant_corr, HCM_Error)
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
!	*																*
!	*				Elements of the terrain profile					*
!	*																*
!	*****************************************************************
!
	Point_Type = 'e'	! for elevation data
	CALL PROFILE (New_LongTx, New_LatTx, New_LongRx, New_LatRx, PD, &
			T_Prof, PN, HCM_Error, Point_Type, Topo_path, &
			Morpho_path, T_L, M_L)
	IF (HCM_Error .NE. 0) RETURN
!
!
!	*****************************************************************
!	*																*
!	*				Calculation the first Fresnel zone				*
!	*																*
!	*****************************************************************
!
!	Add Tx antenna height to height of Tx, add Rx antenna height
!	to height of Rx:
!
	H_Tx_Ant_top = FLOAT (H_Tx + H_AntTx)
	H_Rx_Ant_top = FLOAT (H_Rx + H_AntRx)
!
!	First Fresnel zone is only calculated, if Tx and Rx are no mobiles
!	and if it is a point to point calculation:
	IF ((Tx_serv_area .EQ. 0.0) .AND. (Rx_serv_area .EQ. 0.0) .AND. &
		(C_Mode .GE. 0)) THEN
!	  First value = height of Tx, last value (PN) = height of Rx !
!	  "J" = number of points between Tx and Rx, excluding Tx-point
!	  and Rx-point !
!
	  IF (PN * PD .EQ. Distance) THEN
		  J = PN - 1
		ELSE
		  J = PN - 2
	  END IF
!
!	  Calculate first Fresnel zone
!	  Calculation of ho(z); add all ho to hz (='HDZ'):
!
	  DO I = 1, J
		HDZ(I) = FLOAT (T_Prof(I+1)) + PD*I * (Distance - PD*I) / 17.0
	  END DO
!
!
!	  Add 10 m to all hz except the 1st and last km of the distance:
!	  (If distance is less than 2 km, no addition is done !)
!
	  XX = 1.0D0 / PD
	  I1 = NINT(XX)
	  XX = (Distance - 1.0D0) / PD
	  I2 = INT(xx)
	  IF (Distance .GT. 2.0) THEN
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
		HDC(I) = H_Tx_Ant_top - (H_Tx_Ant_top - H_Rx_Ant_top) * PD * I / &
					Distance - HDZ(I)
		X = PD*I*(Distance-PD*I)
		IF (X .LE. 0.0) THEN
			HDF(I) = 0.0
		  ELSE
			HDF(I) = 547.7 * SQRT ( X / (Distance * Tx_frequency) )
		END IF
	  END DO
!
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
		Calculated_FS = Free_space_FS
		Info(12) = .TRUE.
!		Free space field strength is used, because 1st Fresnel zone
!		is free.
		RETURN
	  END IF
	END IF
!
!	**********************************************************************
!	*																	 *
!	*	Calculation of the field strength according to the ITU-R method	 *
!	*																	 *
!	**********************************************************************
!
	Tx_TCA	= 0.0
	Rx_TCA	= 0.0
	Tx_TCA_corr	= 0.0
	Rx_TCA_corr	= 0.0
!
!	If transmitter is not a mobile, calculate the transmitter
!	clearance angle 'Tx_TCA'
!
	IF (Tx_serv_area .EQ. 0.0) THEN
!	  Calculate Tx_TCA:
	  Tx_TCA = -90.0
	  IF (Distance .GE. 1.6D1) THEN
		  XX = 1.6D1 / PD
		ELSE
		  XX = Distance / PD
	  END IF
	  J = INT(XX) + 1
	  IF (J .GT. 1) THEN
		  DO I = 2, J
			x_TCA = (FLOAT(T_Prof(I))-H_Tx_Ant_top)/((I-1)*PD*1000.0)
			x_TCA = ATAND (x_TCA)	! in degrees
			IF (x_TCA .GT. Tx_TCA) Tx_TCA = x_TCA
		  END DO
		ELSE
		  Tx_TCA = 0.0
	  END IF
	END IF
!
	IF (Tx_TCA .LT. 0.0) THEN
		Tx_TCA_corr = 0.0
!		Not used
	  ELSE
!		Calculate correction factor:
		CALL TCA_correction_calculation (Tx_TCA, Tx_frequency, Tx_TCA_corr)
		IF (Distance .LT. 1.6D1) Tx_TCA_corr = Tx_TCA_corr * Distance / 16.0
	END IF
!
!
!	If receiver is not a mobile and no line calculations, then
!	calculate the receiver clearance angle 'Rx_TCA'
!
	IF ((Rx_serv_area .EQ. 0.0) .AND. (C_Mode .GE. 0) .AND. (C_mode .NE. 99)) THEN
!	  Calculate Rx_TCA and it's correction factor:
	  Rx_TCA = -90.0
	  IF (Distance .GE. 1.6D1) THEN
		  XX = (Distance-1.6D1) / PD
		  J = INT(XX) + 1
		ELSE
		  J = 1
	  END IF
	  IF (PN .GT. 2) THEN
		  DO I = J, PN-1
			x_TCA = (FLOAT(T_Prof(I))-H_Rx_Ant_top)/((Distance-(I-1)*PD)*1000.0)
			x_TCA = ATAND (x_TCA)	! in degrees
			IF (x_TCA .GT. Rx_TCA) Rx_TCA = x_TCA
		  END DO
		ELSE
		  Rx_TCA = 0.0
	  END IF
!
	  IF (Rx_TCA .LT. 0.0) THEN
		  Rx_TCA_corr = 0.0
!		  Not used
		ELSE
!		  Calculate correction factor:
		  CALL TCA_correction_calculation (Rx_TCA, Rx_frequency, Rx_TCA_corr)
		  IF (Distance .LT. 1.6D1) Rx_TCA_corr = Rx_TCA_corr * Distance / 16.0
	  END IF
	END IF
!
!	Effective antenna heights:
	Heff_Tx = 0.0
	Heff_Rx = 0.0
!	Transmitter:
	IF (Tx_serv_area .GT. 0.0) THEN
!		Height of a mobile 'hmTx'
		hmTx = FLOAT(H_AntTx)
		IF (hmTx .LT. 3.0) hmTx = 3.0
		Heff_Tx = hmTx
	  ELSE
		IF (Tx_TCA .LT. 0.0) THEN
!		  Eff. antenna height of transmitter is only calculated,
!		  if transmitter clearance angle is negativ.
		  IF (Distance .LT. 1.5D1) THEN
		      XX = (Distance+1.0D-7)/PD 
		      D2 = DINT(XX)
		    ELSE
		      XX = (1.5D1+1.0D-7)/PD
		      D2 = DINT(XX)
		  END IF
		  HSUM = 0
		  NOH  = 0
		  XX = (1.0D0+1.0D-7)/PD
		  DO I = DINT(XX), D2
		    HSUM = HSUM + T_Prof(I+1)
		    NOH  = NOH  + 1
		  END DO
		  Heff_Tx = FLOAT(HSUM)/FLOAT(NOH)
		  Heff_Tx = H_Tx_Ant_top - Heff_Tx
		  IF (Heff_Tx .LT. 0.0) Heff_Tx = 0.0
		END IF
	END IF
!
!	Receiver: (only for point to point calculations)
	IF ((C_Mode .GE. 0) .AND. (C_Mode .NE. 99)) THEN
	  IF (Rx_serv_area .GT. 0) THEN
!		  Height of a mobile 'hmRx'
		  hmRx = FLOAT(H_AntRx)
		  IF (hmRx .LT. 3.0) hmRx = 3.0
		  Heff_Rx = hmRx
		ELSE
		  IF (Rx_TCA .LT. 0.0) THEN
!			Eff. antenna height of receiver is only calculated,
!			if receiver clearance angle is negativ.
			IF (Distance .LT. 15.0) THEN
				D1 = 1
			  ELSE
			    XX = (Distance-1.5D1) / PD
				D1 = INT(XX)
			END IF
			HSUM = 0
			NOH  = 0
			XX = (Distance-1.0D0)/PD
			DO I = D1, INT(XX)
			  HSUM = HSUM + T_Prof(I+1)
			  NOH  = NOH  + 1
			END DO
			Heff_Rx = FLOAT(HSUM)/FLOAT(NOH)
			Heff_Rx = H_Rx_Ant_top - Heff_Rx
		  END IF
	  END IF
	END IF
!
!
!	Proceding table:
	IF ((Tx_TCA .LT. 0.0) .AND. (Rx_TCA .LT. 0.0)) THEN
	  Heff = Heff_Tx * Heff_Rx / 10.0
	  kdh_use = .TRUE.
	  Tx_TCA_corr = 0.0
	  Rx_TCA_corr = 0.0
	END IF
	IF ((Tx_TCA .LT. 0.0) .AND. (Rx_TCA .GE. 0.0)) THEN
	  Heff = Heff_Tx
	  kdh_use = .FALSE.
	  Tx_TCA_corr = 0.0
	END IF
	IF ((Tx_TCA .GE. 0.0) .AND. (Rx_TCA .LT. 0.0)) THEN
	  Heff = Heff_Rx
	  kdh_use = .FALSE.
	  Rx_TCA_corr = 0.0
	END IF
	IF ((Tx_TCA .GE. 0.0) .AND. (Rx_TCA .GE. 0.0)) THEN
	  Heff = 0.0
	  kdh_use = .FALSE.
	END IF
	IF ((Tx_serv_area .GT. 0.0) .AND. (Rx_TCA .LT. 0.0)) THEN
	  Heff = hmTx * Heff_Rx / 10.0
	  kdh_use = .TRUE.
	  Tx_TCA_corr = 0.0
	  Rx_TCA_corr = 0.0
	END IF
	IF ((Tx_serv_area .GT. 0.0) .AND. (Rx_TCA .GE. 0.0)) THEN
	  Heff = hmTx
	  kdh_use = .FALSE.
	  Rx_TCA_corr = 0.0
	END IF
	IF ((Tx_TCA .LT. 0.0) .AND. (Rx_serv_area .GT. 0)) THEN
	  Heff = hmRx * Heff_Tx / 10.0
	  kdh_use = .TRUE.
	  Tx_TCA_corr = 0.0
	  Rx_TCA_corr = 0.0
	END IF
	IF ((Tx_TCA .GE. 0.0) .AND. (Rx_serv_area .GT. 0)) THEN
	  Heff = hmRx
	  kdh_use = .FALSE.
	  Rx_TCA_corr = 0.0
	END IF
	IF ((Tx_serv_area .GT. 0.0) .AND. (Rx_serv_area .GT. 0)) THEN
	  Heff = hmTx * hmRx / 10.0
	  kdh_use = .TRUE.
	  Tx_TCA_corr = 0.0
	  Rx_TCA_corr = 0.0
	END IF
	IF ((Tx_TCA .LT. 0.0) .AND. ((C_Mode .LT. 0) .OR. (C_Mode .EQ. 99))) THEN
	  Heff = Heff_Tx * FLOAT(H_AntRx) / 10.0
	  Heff_Rx = FLOAT(H_AntRx)
	  kdh_use = .TRUE.
	  Tx_TCA_corr = 0.0
	  Rx_TCA_corr = 0.0
	END IF
	IF ((Tx_TCA .GE. 0.0) .AND. ((C_Mode .LT. 0) .OR. (C_Mode .EQ. 99))) THEN
	  Heff = FLOAT(H_AntRx)
	  kdh_use = .FALSE.
	  Rx_TCA_corr = 0.0
	END IF
	IF ((Tx_serv_area .GT. 0.0) .AND. ((C_Mode .LT. 0) .OR. (C_Mode .EQ. 99))) THEN
	  Heff = hmTx * FLOAT(H_AntRx) / 10.0
	  Heff_Rx = FLOAT(H_AntRx)
	  kdh_use = .TRUE.
	  Tx_TCA_corr = 0.0
	  Rx_TCA_corr = 0.0
	END IF
!
!
!
!	*****************************************************************
!	*																*
!	*					Calculate distance over sea					*
!	*																*
!	*****************************************************************
!
!
	IF (D_sea_input .EQ. '     ') THEN
		D_sea_calculated = 0.0
!		Get the morphological profile:
		Point_Type = 'm'
		CALL PROFILE (New_LongTx, New_LatTx, New_LongRx, New_LatRx, PD, &
			M_Prof, PN, HCM_Error, Point_Type, Topo_path, Morpho_path, T_L, M_L)
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
					  DS1 = FLOAT(J1) * PD
					  D_sea_calculated = D_sea_calculated + DS1
					  DS1 = 0.0
					  J1 = 0 
					  null = .FALSE.
					END IF
				END IF
			  END DO                 
			  IF (null) THEN
				DS1 = FLOAT(J1) * PD 
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
!	*																*
!	*					Delta h calculation							*
!	*																*
!	*****************************************************************
!
	IF ((Distance .GT. 10.0) .AND. (D_sea_calculated .LT. Distance)) THEN
!		Calculate delta-h
		CALL Dh_calculation ()
	  ELSE
		Dh = 50.0
	END IF
!
!
!
!	*****************************************************************
!	*																*
!	*				Correction according to delta h					*
!	*																*
!	*****************************************************************
!
	CALL Dh_orrection (Dh, Distance, Tx_frequency, Dh_corr)
	IF (.NOT. kdh_use) Dh_corr = 0.0
!
!
!
!	*****************************************************************
!	*																*
!	*			Calculate the field strength (from the figures):	*
!	*																*
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
