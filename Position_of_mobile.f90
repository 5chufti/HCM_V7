!
!	Position_of_mobile.f90								P. Benner		29.11.2004
!														G.H.			25.03.2013
!
!	Subroutine to calculate the new position of Tx (New_LongTx, New_LatTx)
!	and/or Rx (New_LongRx, New_latRx) if at least one is a mobile and
!	to test the overlapping situation.
!
!	Possible HCM_Errors:
!	1036	The 'xxx.ALL' borderline file for Tx is missing
!	1037	The 'xxx.ALL' borderline file for Rx is missing
!
!	Info(7)	Calculated field strength is set to 999.9 because
!			the distance is 0 or less than the radius of the
!			service areas (overlapping).
!
!	**********************************************************************
!
	SUBROUTINE Position_of_mobile ( LongTx, LatTx, LongRx, LatRx, &
					New_LongTx, New_LatTx, New_LongRx, New_LatRx )
!
	IMPLICIT			NONE
!
!	Include the interface definitions:
	INCLUDE				'HCM_MS_V7_definitions.F90'
!
!
	DOUBLE PRECISION	LongTx, LatTx, LongRx, LatRx
	DOUBLE PRECISION	New_LongTx, New_LatTx, New_LongRx, New_LatRx
!
	REAL				DP1
	INTEGER				N_Cut_Rx,N_Cut_Tx
!
!	************************************************************************
!
	New_LongTx = LongTx
	New_LatTx  = LatTx  
	New_LongRx = LongRx
	New_LatRx  = LatRx
	N_Cut_Rx = 0
	N_Cut_Tx = 0
!
!	Calculate the distance 'Distance' between point A and B	
	CALL Calc_distance (LongTx, LatTx, LongRx, LatRx, Distance)
!
!	Calculate the direction from Rx to Tx:
	CALL Calc_direction (LongRx, LatRx, LongTx, LatTx, Dir_Rx_Tx)
!
!	Calculate the direction from Tx to Rx:
	CALL Calc_direction (LongTx, LatTx, LongRx, LatRx, Dir_Tx_Rx)
!
!	special case calc mobile Tx on (border)line
!	IF ((Tx_serv_area .GT. 0.0) .AND. (C_mode .LT. 0)) THEN
!		CALL Calc_Tx_pos ( LongTx, LatTx, LongRx, LatRx, New_LongTx, New_LatTx )
!		RETURN
!	END IF
!
!Check if borderline is cut
	IF (Tx_serv_area .GT. 0.0) THEN
!	  Determine, if in direction of Rx, the Tx circle is cut:
	  DP1 = Distance
	  IF (DP1 .GT. Tx_serv_area) DP1 = Tx_serv_area
	  IF ((D_to_border .EQ. 0) .AND. (C_mode .LT. 0)) THEN
		N_Cut_Tx = 1
	  ELSE
	    CALL TestCut (Dir_Tx_Rx, LongTx, LatTx, DP1, N_Cut_Tx, HCM_error, Land_from)
	    IF (HCM_error .NE. 0) THEN
		  HCM_error = 1036
!		The 'xxx.ALL' borderline file for Tx is missing
		  RETURN
	    END IF
	  END IF
	END IF
!
	IF (Rx_serv_area .GT. 0.0) THEN
!	  Determine, if in direction of Tx, the Rx circle is cut:
	  DP1 = Distance
	  IF (DP1 .GT. Rx_serv_area) DP1 = Rx_serv_area
	  CALL TestCut (Dir_Rx_Tx, LongRx, LatRx, DP1, N_Cut_Rx, HCM_error, Land_to)
	  IF (HCM_error .NE. 0) THEN
		HCM_error = 1037
!		The 'xxx.ALL' borderline file for Rx is missing
		RETURN
	  END IF
	END IF
!
!	First select those cases, where no borderline is cut:
!
	IF ((N_Cut_Tx .EQ. 0) .AND. (N_Cut_Rx .EQ. 0)) THEN
!	no borderline cut
	  IF (Distance .LE. DBLE(Tx_serv_area + Rx_serv_area)) THEN
!		  Overlapping -> Tx and Rx in same country:
		  Distance = 0.0D0
	      Info(7) = .TRUE.
!
		ELSE
!		  Calculate new positions:
		  IF (Tx_serv_area .GT. 0.0) THEN
!			Calculate the new Tx co-ordinates:
			CALL New_coordinates (LongTx, LatTx, Dir_Tx_Rx, Tx_serv_area, New_LongTx, New_LatTx)
		  END IF
		  IF (Rx_serv_area .GT. 0.0) THEN
!			Calculate the new Rx co-ordinates:
			CALL New_coordinates (LongRx, LatRx, Dir_Rx_Tx, Rx_serv_area, New_LongRx, New_LatRx)
		  END IF
	  END IF
	  RETURN
	END IF
!
!
!	At least one service area is cut in the direction of the other station:
!
!	Second, select those cases, where both stations are mobiles:
!
!	Are both (Tx and Rx) mobiles?
	IF ((Tx_serv_area .GT. 0.0) .AND. (Rx_serv_area .GT. 0.0)) THEN
!	  Calculate the bigger service area first!
	  IF (Tx_serv_area .GT. Rx_serv_area) THEN
!		  Tx service area is bigger, calculate new Tx co-ordinates:
		  CALL Calc_Tx_pos ( LongTx, LatTx, LongRx, LatRx, New_LongTx, New_LatTx)
!		  Calculate the distance from new Tx point to Rx:
		  CALL Calc_distance (New_LongTx, New_LatTx, LongRx, LatRx, Distance)
!		  Calculate the direction from new Tx point to Rx:
		  CALL Calc_direction (New_LongTx, New_LatTx, LongRx, LatRx, Dir_Tx_Rx) 
!		  Calculate the direction from Rx to new Tx point:
		  CALL Calc_direction (LongRx, LatRx, New_LongTx, New_LatTx, Dir_Rx_Tx) 
!		  Now calculate new Rx Position (Rx is a mobile too):	
		  CALL Calc_Rx_pos ( New_LongTx, New_LatTx, LongRx, LatRx, New_LongRx, New_LatRx)
	    ELSE
!		  Rx service area is bigger, calculate new Rx co-ordinates:
		  CALL Calc_Rx_pos ( LongTx, LatTx, LongRx, LatRx, New_LongRx, New_LatRx )
!		  Calculate the distance from new Rx point to Tx:
		  CALL Calc_distance (New_LongRx, New_LatRx, LongTx, LatTx, Distance)
!		  Calculate the direction from Tx to new Rx point:
		  CALL Calc_direction (LongTx, LatTx, New_LongRx, New_LatRx, Dir_Tx_Rx) 
!		  Calculate the direction from the new Rx point to Tx:
		  CALL Calc_direction (New_LongRx, New_LatRx, LongTx, LatTx, Dir_Rx_Tx) 
!		  Now calculate new Tx Position (Tx is a mobile too):
		  CALL Calc_Tx_pos (LongTx, LatTx, New_LongRx, New_LatRx, New_LongTx, New_LatTx )
	  END IF
!
!	Only one station is a mobile: 
!	If Tx is a mobile:
	ELSEIF (Tx_serv_area .GT. 0.0) THEN
	  CALL Calc_Tx_pos ( LongTx, LatTx, LongRx, LatRx, New_LongTx, New_LatTx )
!
!	If Rx is a mobile:
	ELSEIF (Rx_serv_area .GT. 0.0) THEN
	  CALL Calc_Rx_pos ( LongTx, LatTx, LongRx, LatRx, New_LongRx, New_LatRx )
	END IF
!
	RETURN
!
	END SUBROUTINE Position_of_mobile
!
!	*****************************************************************************************
!
	SUBROUTINE TestCut (Dir, Long, Lat, ServiceArea, N_Cut, Error, Country)
!
	IMPLICIT			NONE
!	Include the interface definitions:
	INCLUDE				'HCM_MS_V7_definitions.F90'
!
	DOUBLE PRECISION	Dir, Long, Lat
	REAL				ServiceArea
	INTEGER				N_Cut, Error
	CHARACTER*3			Country
!	
	CHARACTER*176		BREC
	INTEGER				I, T, IOS
	DOUBLE PRECISION	BCOO(22), N_Long, N_Lat, PI, B
	DOUBLE PRECISION	CX, CY, DX, DY, AX, AY, BX, BY, RN, R, S
!
	EQUIVALENCE (BREC, BCOO)
!
	N_Cut = 0
	PI = 3.141592653589793238D0
	B = 1.8D2 / PI
	Error = 0
!
!	Calculate the point on the circle (N_Long, N_Lat):
	CALL New_coordinates (Long, Lat, Dir, ServiceArea, N_Long, N_Lat)
	AX = Long
	AY = Lat
	BX = N_Long
	BY = N_Lat
!
	OPEN (UNIT=8, FILE=TRIM(Border_path) // '\' // Country // '.ALL', &
          STATUS='OLD', ACCESS='DIRECT',RECL=176, MODE='READ', IOSTAT=IOS)
	IF (IOS .NE. 0) THEN
	  Error = 1
	  RETURN
	END IF         
	T = 1
	I = 3
	READ (8, REC=T, IOSTAT=IOS) BREC
	IF (IOS .NE. 0) Goto 80
!
!	Take first two points:
	DX = BCOO(1) * B
	DY = BCOO(2) * B

70	CX = DX
	CY = DY
	DX = BCOO(I) * B
	I = I + 1
	DY = BCOO(I) * B
!
!	Determine intersection:
	RN = (BX - AX) * (DY - CY) - (BY - AY) * (DX - CX)

	IF (RN .NE. 0.0D0) THEN
	  R = ((AY - CY) * (DX - CX) - (AX - CX) * (DY - CY)) / RN
	  S = ((AY - CY) * (BX - AX) - (AX - CX) * (BY - AY)) / RN
	  IF ((R .GE. 0.0D0) .AND. (R .LE. 1.0D0) .AND. &
          (S .GE. 0.0D0) .AND. (S .LE. 1.0D0)) THEN
	    N_Cut = N_Cut + 1
	  END IF
	END IF
!
!	Take next line point:
	I = I + 1
	IF (I .EQ. 21) THEN
		T = T + 1
		READ (8, REC=T, IOSTAT=IOS) BREC
		IF (IOS .NE. 0) Goto 80
		I = 1
	END IF
	GOTO 70
!	
80	CLOSE (UNIT=8)
!
	RETURN
!
	END SUBROUTINE
!
!	********************************************************************************
!
	SUBROUTINE NearestLinePoint (Long, Lat, N_Long, & 
				N_Lat, LongX, LatX, ServiceArea, Country)
!
	IMPLICIT			NONE
!	Include the interface definitions:
	INCLUDE				'HCM_MS_V7_definitions.F90'
!
	DOUBLE PRECISION	Long, Lat, N_Long, N_Lat, LongX, LatX
	REAL				ServiceArea
	CHARACTER*3			Country
!
	CHARACTER*176		BREC
	INTEGER				IOS, T, J
	DOUBLE PRECISION	PI, B, DI, DIS, MAXDI, BCOO(22), X, Y
!
	EQUIVALENCE (BREC, BCOO)
!
	PI = 3.141592653589793238D0
	B = 1.8D2 / PI
	MAXDI = 1.0D7
!
	OPEN (UNIT=8, FILE=TRIM(Border_path) // '\' // Country // '.ALL', &
		STATUS='OLD', ACCESS='DIRECT',RECL=176, MODE='READ', IOSTAT=IOS)
	IF (IOS .NE. 0) RETURN
!
!	Calculate whole borderline
	T = 1
70	READ (8, REC=T, IOSTAT=IOS) BREC
	IF (IOS .NE. 0) THEN
	  CLOSE (UNIT=8)
	  RETURN
	END IF
!	Do all entries of this record:
	DO J = 1, 19, 2
	  X = BCOO(J)   * B
	  Y = BCOO(J+1) * B
!	  Calculate the distance to the centre of circle:
	  CALL Calc_distance (LongX, LatX, X, Y, DI)
	  IF (DI .LE. ServiceArea) THEN
!		Calculate the distance to other station:
		CALL Calc_distance (Long, Lat, X, Y, DIS)
		IF (DIS .LT. MAXDI) THEN
		  MAXDI = DIS
		  N_Long = X
		  N_Lat = Y
		END IF
	  END IF
	END DO
	T = T + 1
	GOTO 70
!
	CLOSE (UNIT=8)
!
	RETURN
!
	END SUBROUTINE NearestLinePoint
!
!	*************************************************************
!
!	Subroutine New_coordinates						08.01.2004
!
	SUBROUTINE New_coordinates (LONG, LAT, DIR, D, N_LONG, N_LAT)
!
	IMPLICIT			NONE
!	
	DOUBLE PRECISION	LONG, LAT, N_LONG, N_LAT, DIR
	REAL				D
!
	DOUBLE PRECISION	DP, T, T1, T2   
!
	IF (D .EQ. 0.0D0) THEN
	  N_LONG = LONG
	  N_LAT = LAT
	  RETURN
	END IF
!
!	Distance 'DP' in degrees:
	DP = DBLE(D) / 1.112D2
!
!	New co-ordinates:
	T1 = DCOSD(LAT) * DSIND(DP)
	T2 = (DSIND(LAT) * DCOSD(DP)) / T1
	T = T1 * (DCOSD(DIR) + T2)
	IF (T .GT.  1.0D0) T =  1.0D0
	IF (T .LT. -1.0D0) T = -1.0D0
	N_LAT = DASIND(T)   
!                                            
	IF ((DIR .EQ. 0.0D0) .OR. (DIR .EQ. 3.6D2) .OR. (DIR .EQ. 1.8D2)) THEN
	  N_LONG = LONG
	  RETURN
	END IF
!
	T = (DCOSD(DP) - DSIND(LAT) * DSIND(N_LAT)) / (DCOSD(LAT) * DCOSD(N_LAT))
	IF (T .GT.  1.0D0) T =  1.0D0
	IF (T .LT. -1.0D0) T = -1.0D0
	N_LONG = DACOSD(T)
	IF (DIR .LT. 1.8D2) N_LONG = LONG + N_LONG
	IF (DIR .GT. 1.8D2) N_LONG = LONG - N_LONG
!	IF (N_LONG .LT. 0.0D0) N_LONG = 3.6D2 + N_LONG
!
	RETURN
!
	END SUBROUTINE New_coordinates
!
!	*********************************************************************************
!
!
	SUBROUTINE Calc_Tx_pos ( LongTx, LatTx, LongRx, LatRx, New_LongTx, &
							 New_LatTx)
!
	IMPLICIT	NONE
!
!	Include the interface definitions:
	INCLUDE				'HCM_MS_V7_definitions.F90'
!
	DOUBLE PRECISION	LongTx, LatTx, LongRx, LatRx
	DOUBLE PRECISION	New_LongTx, New_LatTx
!
	REAL				DP1
	INTEGER				N_Cut
!
!
!	First: Determine, if in direction of Rx, the Tx circle is cut:
	DP1 = Distance
	IF (DP1 .GT. Tx_serv_area) DP1 = Tx_serv_area
	CALL TestCut (Dir_Tx_Rx, LongTx, LatTx, DP1, N_Cut, HCM_error, Land_from)
!	If the number of (border-) line cuts (N_Cut) is odd, than the line is cutted
!	(point is outside the closed line); if the number of cuts is even,
!	then the point is inside the closed line:	
!	Second: If not cutted, use circle point, if cutted, test all distances
!	from Rx to line point and select the shortest distance:
!	IF (REAL(N_Cut/2) .EQ. REAL(N_Cut)/2.0) THEN
	IF (MOD(N_Cut,2) .EQ. 0) THEN 
!		The border line is not cut by the Tx service area:
!		Calculate the new Tx co-ordinates:
		CALL New_coordinates (LongTx, LatTx, Dir_Tx_Rx, DP1, New_LongTx, New_LatTx)
	  ELSE
!		The border line is cut by the Tx service area:
		CALL NearestLinePoint (LongRx, LatRx, New_LongTx, New_LatTx, &
    				LongTx, LatTx, Tx_serv_area, Land_from)
	END IF
!
	END SUBROUTINE Calc_Tx_pos
!
!	*********************************************************************************
!
	SUBROUTINE Calc_Rx_pos ( LongTx, LatTx, LongRx, LatRx, New_LongRx, New_LatRx)
!
	IMPLICIT	NONE
!
!	Include the interface definitions:
	INCLUDE				'HCM_MS_V7_definitions.F90'
!
	DOUBLE PRECISION	LongTx, LatTx, LongRx, LatRx
	DOUBLE PRECISION	New_LongRx, New_LatRx
!
	INTEGER				N_Cut
	REAL				DP1
!
!
!	First: Determine, if in direction of Tx, the Rx circle is cut:
	DP1 = Distance
	IF (DP1 .GT. Rx_serv_area) DP1 = Rx_serv_area
	CALL TestCut (Dir_Rx_Tx, LongRx, LatRx, DP1, N_Cut, HCM_error, Land_to)
!	If the number of (border-) line cuts (N_Cut) is odd, than the line is cutted
!	(point is ouitside the closed line); if the number of cuts is even,
!	than the point is inside the closed line:	
!	Second: If not cutted, use circle point, if cutted, test all distances
!	from Tx to line point and select the shortest distance:
!	IF (FLOAT(N_Cut/2) .EQ. FLOAT(N_Cut)/2.0) THEN
	IF (MOD(N_Cut,2) .EQ. 0) THEN 
!		The border line is not cut by the Rx service area:
!		Calculate the new Rx co-ordinates:
		CALL New_coordinates (LongRx, LatRx, Dir_Rx_Tx, DP1, New_LongRx, New_LatRx)
	  ELSE
!		The border line is cut by the Rx service area:
		CALL NearestLinePoint (LongTx, LatTx, New_LongRx, New_LatRx, &
     					LongRx, LatRx, Rx_serv_area, Land_to)
	END IF
!
	RETURN
!
	END SUBROUTINE Calc_Rx_pos
!
