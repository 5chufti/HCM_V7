!
!	Pofile.f90											P. Benner		20.11.2003
!														G.H.			20.06.2006
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
!			P_Type		CHARACTER*1			'e' = elevation, 'm' = morpho information
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
!	Called subroutines : Point_height, Point_type
!
!	**********************************************************************************
!
	SUBROUTINE PROFILE (LongA, LatA, LongB, LatB, Prof, P_Type)
!
	IMPLICIT			NONE
!
	INCLUDE				'HCM_MS_V7_definitions.F90'
!
	DOUBLE PRECISION	LongA, LatA, LongB, LatB
	INTEGER(2)			Prof(10002)
	CHARACTER*1			P_Type
!
	INTEGER(2)			PC
	DOUBLE PRECISION	SIDA, SILAB, COLAB, SILAA, COLAA, COLOA, SILOA, COLOB, SILOB
	DOUBLE PRECISION	LAY, LOY, DD, DA, DIS, DP, A, B, K, x, y, z
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
!	calculate avg. earthradius at given mean latitude  360/2*Pi = 57,295779513082321
!	R = (6.378137D3 - 2.1385D1 * DSIND((LatA + LatB) / 2D0)) / 5.7295779513082321D1
!
!	'DA' distance in degrees,
	DA = DIS / 1.112D2
	SIDA  = DSIND(DA)
!
!	adjust PD to Distance
	PN = DNINT(DIS / PD)
!	PD = DIS / DBLE(PN)
!
!	Distance 'DP' between two points in degrees:
	DP   = DA / DBLE(PN)
!
!	number of points in profile
	PN = PN + 1
!
!	first part of profile (TX to center)
		SILAA = DSIND(LatA)
		COLAA = DCOSD(LatA)
		SILAB = DSIND(LatB)
		COLAB = DCOSD(LatB)
		SILOA = DSIND(LongA)
		COLOA = DCOSD(LongA)
		SILOB = DSIND(LongB)
		COLOB = DCOSD(LongB)
		K = DBLE(H_Rx - H_Tx) / DIS
!
!	Loop for waypoints
	DO PC = 1, NINT(REAL(PN)/2.0), 1
!	Distance 'DD' between starting point and new point in degrees:
		DD = DBLE(PC-1) * DP
!	vector to new point
		A = DSIND(DA - DD) / SIDA
		B = DSIND(DD) / SIDA
!	convert startpoint to x,y,z system and add vector
		x = A * COLAA * COLOA +  B * COLAB * COLOB
		y = A * COLAA * SILOA +  B * COLAB * SILOB
		z = A * SILAA         +  B * SILAB
!	convert new x,y,z to coordinates
		LAY = DATAN2D(z,DSQRT(x**2D0 + y**2D0))
		LOY = DATAN2D(y,x)
!
!	  get Information of new point:
!
		IF (P_Type .EQ. 'e') THEN 
			CALL Point_height (LOY, LAY, Prof(PC))
		ELSE
			CALL Point_type (LOY, LAY, Prof(PC))
		END IF	
		IF (HCM_Error .NE. 0) RETURN
	END DO
!
!	second part of profile RX to center
		SILAA = DSIND(LatB)
		COLAA = DCOSD(LatB)
		SILAB = DSIND(LatA)
		COLAB = DCOSD(LatA)
		SILOA = DSIND(LongB)
		COLOA = DCOSD(LongB)
		SILOB = DSIND(LongA)
		COLOB = DCOSD(LongA)
!
!	Loop for waypoints
	DO PC = PN, PC, -1
!	Distance 'DD' between starting point and new point in degrees:
		DD = DBLE(PN-PC) * DP
!	vector to new point
		A = DSIND(DA - DD) / SIDA
		B = DSIND(DD) / SIDA
!	convert startpoint to x,y,z system and add vector
		x = A * COLAA * COLOA +  B * COLAB * COLOB
		y = A * COLAA * SILOA +  B * COLAB * SILOB
		z = A * SILAA         +  B * SILAB
!	convert new x,y,z to coordinates
		LAY = DATAN2D(z,DSQRT(x**2D0 + y**2D0))
		LOY = DATAN2D(y,x)
!
!	  get Information of new point:
!
		IF (P_Type .EQ. 'e') THEN 
			CALL Point_height (LOY, LAY, Prof(PC)) 
		ELSE
			CALL Point_type (LOY, LAY, Prof(PC))
		END IF	
		IF (HCM_Error .NE. 0) RETURN
	END DO
!
!	Correction for 'new profile'
	IF ((C_mode .GE. 0) .AND. (C_mode .LT. 99) .AND. (P_Type .EQ. 'e')) THEN
		DO PC = 1, PN, 1
			Prof(PC) =Prof(PC) - DNINT(DBLE(H_Tx) + K * DBLE(PC-1) * PD)
		END DO
	END IF
!
	RETURN
!
	END SUBROUTINE PROFILE
!
!	**********************************************************************************
!