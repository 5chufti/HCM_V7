!
!	Pofile.f90										P. Benner
!													20.11.2003
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
!			  1000 = Distance greater than 1000 km
!
!	Called subroutines : Point_height, Point_type
!
!	**********************************************************************************
!
	SUBROUTINE PROFILE (LongA, LatA, LongB, LatB, PD, Prof, PN, Error, P_Type, &
						Topo_path, Morpho_path, T_L, M_L)
!
	IMPLICIT			NONE
!
	INTEGER(2)			PN, Prof(10002)
	INTEGER(4)			Error, T_L, M_L
	DOUBLE PRECISION	LongA, LatA, LongB, LatB, PD, SILA, COLA
	DOUBLE PRECISION	SILAB, COLAB, T, DA, DIS, DC, CODC, DP
	DOUBLE PRECISION	CODD, DD, T1, T2, LAY, SILAY, COLAY, LOY
	CHARACTER*1			P_Type
	CHARACTER*63		Topo_path, Morpho_path
!
!	**********************************************************************************
!
	Error = 0
!
!
!	Calculate point A:
	IF (P_Type .EQ. 'e') THEN
		CALL Point_height (LongA, LatA, Prof(1), Error, Topo_path, T_L)
		IF (Error .NE. 0) THEN
		  Prof(1) = -9999
		  RETURN
		END IF
	  ELSE
		CALL Point_type (LongA, LatA, Prof(1), Error, Morpho_path, M_L)
		IF (Error .NE. 0) THEN
		  Prof(1) = 0
		  RETURN
		END IF
	END IF
!
	SILA  = DSIND(LatA)
	COLA  = DCOSD(LatA)
	SILAB = DSIND(LatB)
	COLAB = DCOSD(LatB)
!
!	Max. distance = 1000km; distance between two points = 100m
!	-> number of points are 10.000 + 2 for Tx site and Rx site
!
!	Calculate the total distance 'DA' in degrees, 'DIS' in km:
!
	T = SILA * SILAB + COLA * COLAB * DCOSD(LongB-LongA)
	IF (T .GT.  1.0D0) T =  1.0D0
	IF (T .LT. -1.0D0) T = -1.0D0
	DA   = DACOSD(T)
	DIS = DA * 1.112D2
!
!	If the distance is greater than 1000 km, 'ERROR' value is 1000
!
	IF (DIS .GT. 1.0D3) THEN
	  ERROR = 1028
	  RETURN
	END IF
!
!	Calculate the profile:
!
!	Direction 'DC' to ending point in degrees:
	IF (LongA .NE. LongB) THEN
		T = (SILAB - SILA * DCOSD(DA)) / (COLA * DSIND(DA))
		IF (T .GT.  1.0D0) T =  1.0D0
		IF (T .LT. -1.0D0) T = -1.0D0
		DC = DACOSD(T)
	  ELSE
		IF (LatA .GE. LatB) THEN
			DC = 1.8D2
		  ELSE
			DC = 0.0D0
		END IF
	END IF                   
!
	IF (DABS(LongB-LongA) .LT. 1.8D2) THEN
		IF (LongB .LT. LongA) DC = 3.6D2 - DC
	  ELSE
		IF (LongB .GT. LongA) DC = 3.6D2 - DC
	END IF
!
	CODC = DCOSD(DC)
!
!	Distance 'DP' between two points in degrees:
	DP   = PD / 1.112D2
!
!	Loop:
!	Loop starts with no. 2, because point 1 is Tx site height.
!
	DO PN = 2, 10002
!	  Distance 'DD' between starting point and new point in degrees:
	  DD = (PN-1) * DP
!
!	  Receiving point reached ?
!	  If remaining distance is less or equal 10 cm -> 150
!	  (10 cm is appr. 0.000000899 degree)
!
	  IF (DA-DD .LE. 8.99D-7) EXIT
!
	  CODD = DCOSD(DD)
!
!	  Next point coordinates 'LOY' and 'LAY':
!
	  T1 = COLA * DSIND(DD)
	  T2 = (SILA * CODD) / T1
	  T = T1 * (CODC + T2)
	  IF (T .GT.  1.0D0) T =  1.0D0
	  IF (T .LT. -1.0D0) T = -1.0D0
	  LAY = DASIND(T)   
!                                            
	  SILAY = DSIND(LAY)                                         
	  COLAY = DCOSD(LAY)
!
	  IF ((DC .EQ. 0.0D0) .OR. (DC .EQ. 1.8D2)) THEN
		  LOY = LongA
		ELSE
		  T = (CODD - SILA * SILAY) / (COLA * COLAY)
		  IF (T .GT.  1.0D0) T =  1.0D0
		  IF (T .LT. -1.0D0) T = -1.0D0
		  LOY = DACOSD(T)
		  IF (DC .LT. 1.8D2) LOY = LongA + LOY
		  IF (DC .GT. 1.8D2) LOY = LongA - LOY
	  END IF
!
!	  Information of the point:
50	  IF (LOY .GT. 1.8D2) LOY = LOY - 3.6D2
!
	  IF (P_Type .EQ. 'e') THEN 
		  CALL Point_height (LOY, LAY, Prof(PN), Error, Topo_path, T_L) 
		  IF (Error .NE. 0) THEN
			Prof(PN) = -9999
			RETURN
		  END IF
		ELSE
		  CALL Point_type (LOY, LAY, Prof(PN), Error, Morpho_path, M_L)
		  IF (Error .NE. 0) THEN
			Prof(PN) = 0
			RETURN
		  END IF
	  END IF	
!	  End of loop:
	END DO
!
!	Information of ending point:
!
	IF (P_Type .EQ. 'e') THEN
		CALL Point_height (LongB, LatB, Prof(PN), Error, Topo_path, T_L)
		IF (Error .NE. 0) THEN
		  Prof(PN) = -9999
		  RETURN
		END IF
	  ELSE
		CALL Point_type (LongB, LatB, Prof(PN), Error, Morpho_path, M_L)
		IF (Error .NE. 0) THEN
		  Prof(PN) = 0
		  RETURN
		END IF
	END IF
!
	RETURN
!
	END SUBROUTINE PROFILE
!
!	**********************************************************************************
!