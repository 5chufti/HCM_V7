!
!	Pofile.f90											P. Benner		20.11.2003
!														G.H.			04.05.2005
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
	SUBROUTINE PROFILE (LongA, LatA, LongB, LatB, Prof, Error, P_Type)
!
	IMPLICIT			NONE
!
	INCLUDE				'HCM_MS_V7_definitions.F90'
!
	INTEGER(2)			Prof(10002)
	INTEGER(4)			Error
	INTEGER(2)			PC
	DOUBLE PRECISION	LongA, LatA, LongB, LatB
	DOUBLE PRECISION	SIDA, SILAB, COLAB, SILAA, COLAA, COLOA, SILOA, COLOB, SILOB
	DOUBLE PRECISION	LAY, LOY, DD, DA, DIS, DP, A, B, K, R, x, y, z
	CHARACTER*1			P_Type
!
!	**********************************************************************************
!
	Error = 0
!
!
!	Max. distance = 1000km; distance between two points = 100m
!	-> number of points are 10.000 + 2 for Tx site and Rx site
!	
!	Calculate the total distance 'DIS' in km:
	CALL Calc_distance (LongA, LatA, LongB, LatB, DIS) 
!
!	If the distance is greater than 1000 km, 'ERROR' value is 1028
	IF (DIS .GT. 1.0D3) THEN
	  ERROR = 1028
	  RETURN
	END IF
!
!	calculate avg. earthradius at given mean latitude  360/2*Pi = 57,295779513082321
	R = (6.378137D3 - 2.1385D1 * DSIND((LatA + LatB) / 2D0)) / 5.7295779513082321D1
!
!	'DA' distance in degrees,
	DA = DIS / R
	SIDA  = DSIND(DA)
!
!	adjust PD to Distance
	PN = DNINT(DIS / PD)
	PD = DIS / DBLE(PN)
!
!	Distance 'DP' between two points in degrees:
	DP   = DA / DBLE(PN)
!
!	number of points in profile
	PN = PN + 1
!
!	Calculate point #1 (TX):
!
	IF (P_Type .EQ. 'e') THEN
!			CALL Point_height (LongA, LatA, Prof(1), Error)
		Prof(1) = 0
	  ELSE
		CALL Point_type (LongA, LatA, Prof(1), Error)
	END IF
	IF (Error .NE. 0) RETURN
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
!	Loop starts with #2, because point #1 is S.
	DO PC = 2, NINT(REAL(PN)/2.0), 1
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
			CALL Point_height (LOY, LAY, Prof(PC), Error)
			Prof(PC) = Prof(PC) - DNINT(DBLE(H_Tx) + K * DBLE(PC-1) * PD)
		ELSE
			CALL Point_type (LOY, LAY, Prof(PC), Error)
		END IF	
		IF (Error .NE. 0) RETURN
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
		K = DBLE(H_Tx - H_Rx) / DIS
!
!	Loop for waypoints
!	Loop starts with #PN-1, because point #PN is RX.
	DO PC = (PN - 1), PC, -1
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
			CALL Point_height (LOY, LAY, Prof(PC), Error) 
			Prof(PC) = Prof(PC) - DNINT(DBLE(H_Rx) + K * DBLE(PC-1) * PD)
		ELSE
			CALL Point_type (LOY, LAY, Prof(PC), Error)
		END IF	
		IF (Error .NE. 0) RETURN
	END DO
!
!	calculate last point #PC+1 (RX):
	IF (P_Type .EQ. 'e') THEN
!			CALL Point_height (LongB, LatB, Prof(PN), Error)
		Prof(PN) = 0
	ELSE
		CALL Point_type (LongB, LatB, Prof(PN), Error)
	END IF
	IF (Error .NE. 0) RETURN	
!
	RETURN
!
	END SUBROUTINE PROFILE
!
!	**********************************************************************************
!