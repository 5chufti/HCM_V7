!
!	Position_of_mobile.f90
!																	24.08.2004
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
					New_LongTx, New_LatTx, New_LongRx, New_LatRx, B_L )
!
	IMPLICIT			NONE
!
!	Include the interface definitions:
	INCLUDE				'HCM_MS_V7_definitions.F90'
!
	INTEGER				N_Cut
	INTEGER*4			B_L
!
	REAL				DP1
!
	DOUBLE PRECISION	XDi, YDi, LongTx, LatTx, LongRx, LatRx
	DOUBLE PRECISION	New_LongTx, New_LatTx, New_LongRx, New_LatRx
!
	LOGICAL				CutTx, CutRx
!
!	************************************************************************
!
	New_LongTx = LongTx
	New_LatTx  = LatTx  
	New_LongRx = LongRx
	New_LatRx  = LatRx
!
	CutTx = .FALSE.
	CutRx = .FALSE.
!
!	First select those cases, where no borderline is cut:
	IF (Tx_serv_area .GT. 0.0) THEN
!	  Determine, if in direction of Rx, the Tx circle is cut:
	  DP1 = Distance
	  IF (DP1 .GT. Tx_serv_area) DP1 = Tx_serv_area
	  CALL TestCut (Border_path, B_L, Dir_Tx_Rx, LongTx, LatTx, DP1, N_Cut, &
     				HCM_error, Land_from)
	  IF (HCM_error .NE. 0) THEN
		HCM_error = 1036
!		The 'xxx.ALL' borderline file for Tx is missing
		RETURN
	  END IF
	  IF (N_Cut .GT. 0) CutTx = .TRUE.
	END IF
!
	IF (Rx_serv_area .GT. 0.0) THEN
!	  Determine, if in direction of Tx, the Rx circle is cut:
	  DP1 = Distance
	  IF (DP1 .GT. Rx_serv_area) DP1 = Rx_serv_area
	  CALL TestCut (Border_path, B_L, Dir_Rx_Tx, LongRx, LatRx, DP1, N_Cut, &
     				HCM_error, Land_to)
	  IF (HCM_error .NE. 0) THEN
		HCM_error = 1037
!		The 'xxx.ALL' borderline file for Rx is missing
		RETURN
	  END IF
	  IF (N_Cut .GT. 0) CutRx = .TRUE.
	END IF
!
	IF ((.NOT. CutTx) .AND. (.NOT. CutRx)) THEN
	  IF (Distance .LE. DBLE(Tx_serv_area + Rx_serv_area)) THEN
!		  Overlapping:
		  Distance = 0.0D0
		  Calculated_FS = 999.9
	      Info(7) = .TRUE.
!
!		  Calculate the positions:
		  IF (Tx_serv_area .GT. Rx_serv_area) THEN
			  DP1 = Rx_serv_area
			  IF (Distance .LT. Rx_serv_area) THEN
				DP1 = Distance
			  END IF
!			  Calculate the new Rx co-ordinates:
			  CALL New_coordinates (LongRx, LatRx, Dir_Rx_Tx, DP1, &
								    New_LongRx, New_LatRx)
			  New_LongTx = New_LongRx
			  New_LatTx  = New_LatRx
		    ELSE
			  DP1 = Tx_serv_area
			  IF (Distance .LT. Tx_serv_area) THEN
				DP1 = Distance
			  END IF
!			  Calculate the new Tx co-ordinates:
			  CALL New_coordinates (LongTx, LatTx, Dir_Tx_Rx, DP1, &
								    New_LongTx, New_LatTx)
			  New_LongRx = New_LongTx
			  New_LatRx  = New_LatTx
		  END IF
		ELSE
!		  Calculate new positions:
		  IF (Tx_serv_area .GT. 0.0) THEN
!			Calculate the new Tx co-ordinates:
			CALL New_coordinates (LongTx, LatTx, Dir_Tx_Rx, Tx_serv_area, &
								  New_LongTx, New_LatTx)
		  END IF
		  IF (Rx_serv_area .GT. 0.0) THEN
!			Calculate the new Rx co-ordinates:
			CALL New_coordinates (LongRx, LatRx, Dir_Rx_Tx, Rx_serv_area, &
								  New_LongRx, New_LatRx)
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
		  CALL Calc_Tx_pos ( LongTx, LatTx, LongRx, LatRx, New_LongTx, &
							 New_LatTx, B_L )
!		  Now calculate new Rx Position (Rx is a mobile too):
!		  Calculate the distance from new Tx point to Rx:
		  CALL Calc_distance (New_LongTx, New_LatTx, LongRx, LatRx, XDi)
		  Distance = XDi
!		  Calculate the direction from new Tx point to Rx:
		  CALL Calc_direction (New_LongTx, New_LatTx, LongRx, LatRx, YDi) 
		  Dir_Tx_Rx = YDI
!		  Calculate the direction from Rx to new Tx point:
		  CALL Calc_direction (LongRx, LatRx, New_LongTx, New_LatTx, YDi) 
		  Dir_Rx_Tx = YDI
		  CALL Calc_Rx_pos ( New_LongTx, New_LatTx, LongRx, LatRx, &
							 New_LongRx, New_LatRx, B_L )


	    ELSE
!		  Rx service area is bigger, calculate new Rx co-ordinates:
		  CALL Calc_Rx_pos ( LongTx, LatTx, LongRx, LatRx, New_LongRx, &
							 New_LatRx, B_L )
!		  Now calculate new Tx Position (Tx is a mobile too):
!		  Calculate the distance from new Rx point to Tx:
		  CALL Calc_distance (New_LongRx, New_LatRx, LongTx, LatTx, XDi)
		  Distance = XDi
!		  Calculate the direction from Tx to new Rx point:
		  CALL Calc_direction (LongTx, LatTx, New_LongRx, New_LatRx, YDi) 
		  Dir_Tx_Rx = YDI
!		  Calculate the direction from the new Rx point to Tx:
		  CALL Calc_direction (New_LongRx, New_LatRx, LongTx, LatTx, YDi) 
		  Dir_Rx_Tx = YDI
		  CALL Calc_Tx_pos ( LongTx, LatTx, New_LongRx, New_LatRx, &
							 New_LongTx, New_LatTx, B_L )
	  END IF
!	  Calculate the distance from new Rx point to the new Tx point:
	  CALL Calc_distance (New_LongRx, New_LatRx, New_LongTx, New_LatTx, XDi)
	  Distance = XDi
!	  Calculate the direction from the new Tx point to new Rx point:
	  CALL Calc_direction (New_LongTx, New_LatTx, New_LongRx, New_LatRx, YDi) 
	  Dir_Tx_Rx = YDI
!	  Calculate the direction from the new Rx point to the new Tx point:
	  CALL Calc_direction (New_LongRx, New_LatRx, New_LongTx, New_LatTx, YDi) 
	  Dir_Rx_Tx = YDI
	  IF (Distance .EQ. 0.0D0) THEN
!		Overlapping:
		Calculated_FS = 999.9
	    Info(7) = .TRUE.
	  END IF
	  RETURN
	END IF
!
!
!	Third, select those cases, where only one is a mobile:
!
!	Only one station is a mobile: 
!	If Tx is a mobile:
	IF (Tx_serv_area .GT. 0.0) THEN
	  CALL Calc_Tx_pos ( LongTx, LatTx, LongRx, LatRx, New_LongTx, &
						 New_LatTx, B_L )
!	  Calculate the distance from new Tx point to Rx:
	  CALL Calc_distance (New_LongTx, New_LatTx, LongRx, LatRx, XDi)
	  Distance = XDi
!	  Calculate the direction from new Tx point to Rx:
	  CALL Calc_direction (New_LongTx, New_LatTx, LongRx, LatRx, YDi) 
	  Dir_Tx_Rx = YDI
!	  Calculate the direction from Rx to new Tx point:
	  CALL Calc_direction (LongRx, LatRx, New_LongTx, New_LatTx, YDi) 
	  Dir_Rx_Tx = YDI
!
!	  Test overlapping:
	  IF (Distance .EQ. 0.0D0) THEN
!		Overlapping:
		Calculated_FS = 999.9
		Info(7) = .TRUE.
		RETURN
	  END IF
	END IF
!
!
!	If Rx is a mobile:
	IF (Rx_serv_area .GT. 0.0) THEN
	  CALL Calc_Rx_pos ( LongTx, LatTx, LongRx, LatRx, New_LongRx, &
						 New_LatRx, B_L )
!	  Calculate the distance from new Rx point to Tx:
	  CALL Calc_distance (New_LongRx, New_LatRx, LongTx, LatTx, XDi)
	  Distance = XDi
!	  Calculate the direction from Tx to new Rx point:
	  CALL Calc_direction (LongTx, LatTx, New_LongRx, New_LatRx, YDi) 
	  Dir_Tx_Rx = YDI
!	  Calculate the direction from the new Rx point to Tx:
	  CALL Calc_direction (New_LongRx, New_LatRx, LongTx, LatTx, YDi) 
	  Dir_Rx_Tx = YDI
!
!	  Test overlapping:
	  IF (Distance .EQ. 0.0D0) THEN
!		Overlapping:
		Calculated_FS = 999.9
		Info(7) = .TRUE.
		RETURN
	  END IF
	END IF
!
!
	RETURN
!
	END SUBROUTINE Position_of_mobile
!
!	*****************************************************************************************
!
	SUBROUTINE TestCut (Path, B_L, Dir, Long, Lat, ServiceArea, N_Cut, &
     					Error, Country)
!
	IMPLICIT			NONE
!
	CHARACTER*3			Country
	CHARACTER*7			BORFIL
	CHARACTER*63		Path
	CHARACTER*176		BREC
!
	INTEGER				N_Cut, I, T, IOS
	INTEGER*4			B_L
	INTEGER				Error
!
	REAL				ServiceArea
!
	DOUBLE PRECISION	BCOO(22), Long, Lat, N_Long, N_Lat, PI, B, Dir
	DOUBLE PRECISION	CX, CY, DX, DY, AX, AY, BX, BY, RT, RN, R, S
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
	BORFIL = 'xxx.ALL'
	BORFIL(1:3) = Country
	IF (BORFIL(2:2) .EQ. ' ') BORFIL(2:2) = '_'
	IF (BORFIL(3:3) .EQ. ' ') BORFIL(3:3) = '_'
	OPEN (UNIT=14, FILE=Path(1:B_L)//BORFIL, STATUS='OLD', &
          ACCESS='DIRECT',RECL=176, MODE='READ', IOSTAT=IOS)
	IF (IOS .NE. 0) THEN
	  Error = 1
	  RETURN
	END IF         
	T = 1
	I = 4
	READ (14, REC=T, IOSTAT=IOS) BREC
	IF (IOS .NE. 0) THEN
	  CLOSE (UNIT=14)
	  RETURN
	END IF
!
!	Take first two points:
	CX = BCOO(1) * B
	CY = BCOO(2) * B
	DX = BCOO(3) * B
	DY = BCOO(4) * B
!
50	READ (14, REC=T, IOSTAT=IOS) BREC
	IF (IOS .NE. 0) THEN
	  CLOSE (UNIT=14)
	  RETURN
	END IF
!
!	Determine intersection:
70	RT = (AY - CY) * (DX - CX) - (AX - CX) * (DY - CY)
	RN = (BX - AX) * (DY - CY) - (BY - AY) * (DX - CX)
	IF ((RN .NE. 0.0D0) .AND. (RT .NE. 0.0D0)) THEN
	  R = RT / RN
	  S = ((AY - CY) * (BX - AX) - (AX - CX) * (BY - AY)) / RN
	  IF ((R .GE. 0.0D0) .AND. (R .LE. 1.0D0) .AND. &
          (S .GE. 0.0D0) .AND. (S .LE. 1.0D0)) THEN
	    N_Cut = N_Cut + 1
	  END IF
	END IF
	CX = DX
	CY = DY
!
!	Take next line point:
	I = I + 1
	IF (I .GE. 20) THEN
		T = T + 1
		I = 0
		GOTO 50
	  ELSE
		DX = BCOO(I) * B
		I = I + 1
		DY = BCOO(I) * B
		GOTO 70
	END IF
!	
	CLOSE (UNIT=14)
!
	RETURN
!
	END SUBROUTINE
!
!	********************************************************************************
!
	SUBROUTINE NearestLinePoint (Long, Lat, N_Long, & 
				N_Lat, LongX, LatX, ServiceArea, PATH, B_L, Country)
!
	IMPLICIT			NONE
!
	CHARACTER*3			Country
	CHARACTER*7			BORFIL
	CHARACTER*63		PATH
	CHARACTER*176		BREC
!
	INTEGER				IOS, T, J
	INTEGER*4			B_L
	REAL				ServiceArea
!
	DOUBLE PRECISION	Long, Lat, N_Long, N_Lat, BCOO(22), X, Y
	DOUBLE PRECISION	LongX, LatX, PI, B, DI, DIS, MAXDI
!
	EQUIVALENCE (BREC, BCOO)
!
	PI = 3.141592653589793238D0
	B = 1.8D2 / PI
	MAXDI = 1.0D7
!
	BORFIL = 'xxx.ALL'
	BORFIL(1:3) = Country
	IF (BORFIL(2:2) .EQ. ' ') BORFIL(2:2) = '_'
	IF (BORFIL(3:3) .EQ. ' ') BORFIL(3:3) = '_'
	OPEN (UNIT=14, FILE=PATH(1:B_L)//BORFIL, STATUS='OLD', &
          ACCESS='DIRECT',RECL=176, MODE='READ', IOSTAT=IOS)
	IF (IOS .NE. 0) RETURN
!
!	Calculate whole borderline
	T = 1
70	READ (14, REC=T, IOSTAT=IOS) BREC
	IF (IOS .NE. 0) THEN
	  CLOSE (UNIT=14)
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
	CLOSE (UNIT=14)
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
	REAL				D
!
	DOUBLE PRECISION	LONG, LAT, N_LONG, N_LAT
	DOUBLE PRECISION	DP, T, T1, T2, DIR    
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
	IF (N_LONG .LT. 0.0D0) N_LONG = 3.6D2 + N_LONG
!
	RETURN
!
	END SUBROUTINE New_coordinates
!
!	*********************************************************************************
!
!
	SUBROUTINE Calc_Tx_pos ( LongTx, LatTx, LongRx, LatRx, New_LongTx, &
							 New_LatTx, B_L )
!
	IMPLICIT	NONE
!
!	Include the interface definitions:
	INCLUDE				'HCM_MS_V7_definitions.F90'
!
	INTEGER				N_Cut
	INTEGER*4			B_L
!
	REAL				DP1
!
	DOUBLE PRECISION	LongTx, LatTx, LongRx, LatRx
	DOUBLE PRECISION	New_LongTx, New_LatTx
!
	LOGICAL				CutTx
!
!
!	First: Determine, if in direction of Rx, the Tx circle is cut:
	DP1 = Distance
	IF (DP1 .GT. Tx_serv_area) DP1 = Tx_serv_area
	CALL TestCut (Border_path, B_L, Dir_Tx_Rx, LongTx, LatTx, DP1, N_Cut, &
    			  HCM_error, Land_from)
!	If the number of (border-) line cuts (N_Cut) is odd, than the line is cutted
!	(point is ouitside the closed line); if the number of cuts is even,
!	than the point is inside the closed line:	
	IF (FLOAT(N_Cut/2) .EQ. FLOAT(N_Cut)/2.0) THEN
		CutTx = .FALSE.
	  ELSE
		CutTx = .TRUE.
	END IF
!
!	Second: If not cutted, use circle point, if cutted, test all distances
!	from Rx to line point and select the shortest distance:
	IF (CutTx) THEN
!		The border line is cut by the Tx service area:
		CALL NearestLinePoint (LongRx, LatRx, New_LongTx, New_LatTx, &
    				LongTx, LatTx, Tx_serv_area, Border_path, B_L, Land_from)
	  ELSE
!		The border line is not cut by the Tx service area:
!		Calculate the new Tx co-ordinates:
		CALL New_coordinates (LongTx, LatTx, Dir_Tx_Rx, DP1, &
								New_LongTx, New_LatTx)
	END IF
!
	RETURN
!
	END SUBROUTINE Calc_Tx_pos
!
!	*********************************************************************************
!
	SUBROUTINE Calc_Rx_pos ( LongTx, LatTx, LongRx, LatRx, New_LongRx, &
							 New_LatRx, B_L )
!
	IMPLICIT	NONE
!
!	Include the interface definitions:
	INCLUDE				'HCM_MS_V7_definitions.F90'
!
	INTEGER				N_Cut
	INTEGER*4			B_L
!
	REAL				DP1
!
	DOUBLE PRECISION	LongTx, LatTx, LongRx, LatRx
	DOUBLE PRECISION	New_LongRx, New_LatRx
!
	LOGICAL				CutRx
!
!
!	First: Determine, if in direction of Tx, the Rx circle is cut:
	DP1 = Distance
	IF (DP1 .GT. Rx_serv_area) DP1 = Rx_serv_area
	CALL TestCut (Border_path, B_L, Dir_Rx_Tx, LongRx, LatRx, DP1, N_Cut, &
    				HCM_error, Land_to)
!	If the number of (border-) line cuts (N_Cut) is odd, than the line is cutted
!	(point is ouitside the closed line); if the number of cuts is even,
!	than the point is inside the closed line:	
	IF (FLOAT(N_Cut/2) .EQ. FLOAT(N_Cut)/2.0) THEN
		CutRx = .FALSE.
	  ELSE
		CutRx = .TRUE.
	END IF
!
!	Second: If not cutted, use circle point, if cutted, test all distances
!	from Tx to line point and select the shortest distance:
	IF (CutRx) THEN
!		The border line is cut by the Rx service area:
		CALL NearestLinePoint (LongTx, LatTx, New_LongRx, New_LatRx, &
     					LongRx, LatRx, Rx_serv_area, Border_path, B_L, Land_to)
	  ELSE
!		The border line is not cut by the Rx service area:
!		Calculate the new Rx co-ordinates:
		CALL New_coordinates (LongRx, LatRx, Dir_Rx_Tx, DP1, &
								New_LongRx, New_LatRx)
	END IF
!
	RETURN
!
	END SUBROUTINE Calc_Rx_pos
!
