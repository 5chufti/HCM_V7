!
!	Pofile.f90											P. Benner		20.11.2003
!														G.H.			26.04.2017
!
!	This subroutine constructs a terrain- or morphological profile from point A to
!	point B in steps of 100 m. The heights or morphological information are stored
!	in 'Prof(i)'. The total number of points is in 'PN'. The first profile point
!	'Prof(1)' is the height or morphological type of point A, the last profile point
!	'Prof(PN)' is the height or morphological type of point B.
!
!
!	Input values:
!			LongA		DOUBLE PRECISION	longitude of point A (starting point)
!			LatA		DOUBLE PRECISION	latitude of point A
!			LongB		DOUBLE PRECISION	longitude of point B (ending point)
!			LatB		DOUBLE PRECISION	latitude of point B
!			PD			DOUBLE PRECISION	point distance (grid size) of the profile
!			Topo_path	CHARACTER*63		path of topo database, e.g. 'D:\TOPO'
!			Morpho_path	CHARACTER*63		path of morpho database, e.g. 'D:\MORPHO'
!
!
!	Output values:
!			Prof(i)		INTEGER*2(10002)	field with heights or morpho information
!			PN			INTEGER*2			number of profile points 
!			Error		INTEGER*4			error value
!
!	Possible error values
!				 0 = No error
!		   1 - 999 = reserved for Point_height or Point_Type -subroutine
!	Negativ values = reserved for Point_height or Point_Type -subroutine
!			  1000 = Distance 0 km
!			  1028 = Distance greater than 1000 km
!
!	Called subroutines : Point_Info
!
!	**********************************************************************************
!
	SUBROUTINE PROFILE (LongA, LatA, LongB, LatB)
!
	IMPLICIT			NONE
!
	INCLUDE				'HCM_MS_V7_definitions.F90'
!
	DOUBLE PRECISION	LongA, LatA, LongB, LatB
!
!
	INTEGER(2)			PC
	DOUBLE PRECISION	LAY, LOY, DD, DIS, DIR, K, SLA, CLA, SDIR, CDIR, SIDD, CODD, o_Tx, o_Rx
!
!	**********************************************************************************
!
!	Max. distance = 1000km; distance between two points = 100m
!	-> number of points are 10.000 + 2 for Tx site and Rx site
!
!	Calculate the total distance 'DIS' in km:
	CALL Calc_distance (LongA, LatA, LongB, LatB, DIS) 
!
!	If the distance is greater than 1000 km, 'ERROR' value is 1028
	IF (DIS .GT. 1.0D3) THEN
	  HCM_Error = 1028
	  RETURN
	END IF
!
!	number of points in profile
	PN = NINT(DIS / PD) + 1
!	set END Marker
	T_Prof(PN+1)=-9999
!	slant = .FALSE.
	IF (slant) THEN
!	prepare for sloped profile
		o_Tx = DBLE(H_Tx)
		o_Rx = DBLE(H_Rx)
		K = DBLE(H_Rx - H_Tx) / (PN-1)
		T_Prof(PN)=0
		T_Prof(1)=0
	ELSE
		T_Prof(PN)=H_Rx
		T_Prof(1)=H_Tx
	END IF
!
!	Direction Tx to Rx 
	CALL Calc_Direction (LongA,LatA,LongB,LatB,DIR)
!	Prepare often used values
	SLA = DSIND(LatA)
	CLA = DCOSD(LatA)
	SDIR = DSIND(DIR)
	CDIR = DCOSD(DIR)
!	Loop for waypoints Tx to center
	DO PC = 2, NINT(PN/2.0), 1
	  DD=((PC-1)*PD)/6.37129D3
	  SIDD = DSIN(DD)
	  CODD = DCOS(DD)
	  DD = SLA * CODD + CLA * SIDD * CDIR
	  LAY = DASIND(DD)
	  LOY = LongA + DATAN2D(SDIR * SIDD * CLA, CODD - SLA * DD)
!	  get Information of new point:
		CALL Point_info (LOY, LAY, T_Prof(PC), M_Prof(PC))
		IF (slant) T_Prof(PC) = T_Prof(PC) - NINT(o_Tx + K * (PC-1))
	  IF (HCM_Error .NE. 0) RETURN
	END DO
!
!	Direction Rx to Tx 
	CALL Calc_Direction (LongB,LatB,LongA,LatA,DIR)
!	Prepare often used values
	SLA = DSIND(LatB)
	CLA = DCOSD(LatB)
	SDIR = DSIND(DIR)
	CDIR = DCOSD(DIR)
!	Loop for waypoints Rx to center
	DO PC = PC+1, PN-1, 1
!	http://www.movable-type.co.uk/scripts/latlong.html
	  DD=((PN-PC)*PD)/6.37129D3
	  SIDD = DSIN(DD)
	  CODD = DCOS(DD)
	  DD = SLA * CODD + CLA * SIDD * CDIR
	  LAY = DASIND(DD)
	  LOY = LongB + DATAN2D(SDIR * SIDD * CLA, CODD - SLA * DD)
!	  get Information of new point:
		CALL Point_info (LOY, LAY, T_Prof(PC), M_Prof(PC))
		IF (slant) T_Prof(PC) = T_Prof(PC) - NINT(o_Rx - K * (PN-PC))
	  IF (HCM_Error .NE. 0) RETURN
	END DO
!
	RETURN
!
	END SUBROUTINE PROFILE
!
!	**********************************************************************************
!