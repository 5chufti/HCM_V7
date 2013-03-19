!
!	Line_calculation.f90								P.Benner		23.11.2004
!														G.H.			18.07.2011
!
!	23.11.2004	Steps from 100 / 10 / 1 modified to 25 / 5 / 1
!	18.07.2011  Steps modified to 5 / 1
!
!	Subroutine to calculate the field strengs on a line
!	or to calculate the cross border field strength.
!
!	Input values
!
!	Land_to			Country to calculate to
!	Land_from		Country of Tx
!	D_to_border		Distance to borderline (input)
!					(negativ = Cross-border range calculation)      
!	Border_path		Path where border data are stored
!	B_L				Length of Border_path
!	LongTx			Longitude of Tx
!	LatTx			Latitude of Tx
!
!
!	Output values
!
!	HCM_error		Error value
!	Calculated_FS	Calculated field strength
!
!
!
!	HCM_error values:
!
!	1047	Distance to borderline is too long
!	1048	Selected line data not available
!	1049	Error in (border-) line data
!            
!	************************* Line_calculation ******************
!	*	used by	:	HCM_MS_V7															
!	*	uses	:	Manage_List, CBR_Coordinates, 
!	*				P_to_P_Calculation											
!	*************************************************************
	SUBROUTINE Line_calculation ( LongTx, LatTx, LongRx, LatRx)                
!
	IMPLICIT NONE
!
	INCLUDE				'HCM_MS_V7_definitions.F90'
!
	DOUBLE PRECISION	LongTx, LatTx, LongRx, LatRx
!
	INTEGER				IOS, N_rec, N_List, Rec_N_list(3), N_cp
	INTEGER				N_List1, Rec_N_list1(3), teststep
	INTEGER				I, J, K, Rec_N_x, Rec_x, N_cut
	DOUBLE PRECISION	N_Record(22), RB, PI, Lo, La, Co_cp(2000,2)
	REAL				FS_list(3), FS_list1(3), FS_x
	CHARACTER*8			C_Record(22)
	CHARACTER*10		BorderFile
	LOGICAL				CBR, Take_it
!
	EQUIVALENCE			(N_Record,C_Record)
!
	COMMON /Co_ord_cp/	Co_cp, N_cp
!    
!	***************************************************************
!
	PI = 3.141592653589793238D0                                 
	RB = 1.8D2 / PI
!
	Rx_serv_area = 0.0
!
!	Select line data
!
	BorderFile(1:3) = Land_from
	BorderFile(4:6) = Land_to
	BorderFile(7:7) = '.'
! 
	CBR = .FALSE.
!
!	1. Cross border range:
	IF (D_to_border .LT. 0) THEN
	  CBR = .TRUE.
	  BorderFile(8:10) = 'CBR'
	END IF
!	2. Border line:
!	IF (D_to_border .EQ. 0) THEN
!	  BorderFile(8:10) = '000'
!	END IF
!	3. x-km line:
	IF (D_to_border .GE. 0) THEN
	  IF (D_to_border .GT. 999) THEN
			HCM_Error = 1047
!			Distance to borderline is too long
			RETURN
	    ELSE
		  WRITE (BorderFile(8:10), '(I3.3)') D_to_border
	  END IF
	END IF
!
!	get additionally all borderline centerpoints of the affected coutry:
	  OPEN (UNIT=3, FILE=TRIM(Border_path) // '\' // BorderFile(1:7) // '000', &
			STATUS='OLD', ACCESS='DIRECT',RECL=176, MODE='READ', IOSTAT=IOS)
!
	  IF (IOS .NE. 0) THEN
		HCM_Error= 1048
!		Selected line data not available
		RETURN
	  END IF                            
!	  Store all center points table 'N_Record(i,j)':
	  DO N_rec = 1, 2000      
		READ (3, REC=N_rec, IOSTAT=IOS) C_Record
!		End of file reached (or non existing record) ?
		IF ((IOS .LT. 0) .OR. (IOS .EQ. 36)) EXIT
		IF (IOS .NE. 0) THEN   
!		  Error in (border-) line data
		  HCM_Error = 1049
		  RETURN
		END IF
		Co_cp(N_rec,1) = N_Record(21) * RB
		Co_cp(N_rec,2) = N_Record(22) * RB
	  END DO
!
!	  Number of entries in the list of center co-ordinates
	  N_cp = N_rec -1
!
	  CLOSE (UNIT=3)
!
!	Open line file:
	OPEN (UNIT=3, FILE=TRIM(Border_path) // '\' // BorderFile, &
			STATUS='OLD', ACCESS='DIRECT',RECL=176, MODE='READ', IOSTAT=IOS)
!
	IF (IOS .NE. 0) THEN
	  HCM_Error= 1048
!	  Selected line data not available
	  RETURN
	END IF                            
!	all linepoints?
	INQUIRE (FILE='HCM_LP',EXIST=Take_it)
	IF (.NOT. Take_it) GOTO 80
!-----------------------------------------------------------------------
!	Testroutine to calculate to each point:
	FS_x = -999.9
	DO J = 1, 5000
	  READ (3, REC=J, IOSTAT=IOS) C_Record
!	  End of file reached (or non existing record) ?
	  IF ((IOS .LT. 0) .OR. (IOS .EQ. 36)) EXIT ! end of file reached
	  IF (IOS .NE. 0) THEN
		HCM_Error = 1049
!		Error in line data
		CLOSE (UNIT = 3)
		RETURN
	  END IF
!
!	  Calculate to all 10 points inside this record
	  DO K = 1, 19, 2
		LongRx = N_Record(K)   * RB
		LatRx  = N_Record(K+1) * RB
		Lo = LongTx
		La = LatTx
		IF (CBR) CALL CBR_Coordinates (LongRx, LatRx, Lo, La, CBR_D, Tx_serv_area)
!		check ctry affected if x-km or CBR
		IF (D_to_border .NE. 0) THEN
			CALL Test_cut1 (Lo, La, LongRx, LatRx, N_cut)
			IF (N_cut .EQ. 0) GOTO 70
		END IF
!		  
		CALL P_to_P_Calculation (Lo, La, LongRx, LatRx)
		IF ((HCM_Error .NE. 0) .OR. INFO(7)) RETURN
		IF (HCM_Error .EQ. 1028) GOTO 70	! Distance > 1000 km
!		  Find maximun of field strength:
		IF (Calculated_FS .GT. FS_x) THEN
			FS_x = Calculated_FS
			Rec_x = K
			Rec_N_x = J
		END IF
70		CONTINUE
	  END DO	! K
	END DO	! J
!
75	GOTO 140
!	End of testroutine
!-------------------------------------------------------------------------
!	1st: calculate to every 5th centerpoint:
!	Use 1st list:
80	N_List = 0	! number of stored record numbers and field strength
	teststep = 5
	N_rec = 3	! record number in file
!	for tests
!	teststep = 1
!	N_rec = 1	
90	IOS = 0
!
	DO WHILE (IOS .EQ. 0)
	  READ (3, REC=N_rec, IOSTAT=IOS) C_Record
!	  End of file reached (or non existing record) ?
	  IF ((IOS .LT. 0) .OR. (IOS .EQ. 36)) EXIT ! end of file reached
	  IF (IOS .NE. 0) THEN   
		HCM_Error = 1049
!		Error in line data
		CLOSE (UNIT = 3)
		RETURN
	  END IF
	  LongRx = N_Record(21) * RB
	  LatRx  = N_Record(22) * RB
	  Lo = LongTx
	  La = LatTx
	  IF (CBR) CALL CBR_Coordinates (LongRx, LatRx, Lo, La, CBR_D, Tx_serv_area)
!		check ctry affected if x-km or CBR
	  IF (D_to_border .NE. 0) THEN
			CALL Test_cut1 (Lo, La, LongRx, LatRx, N_cut)
			IF (N_cut .EQ. 0) GOTO 100
	  END IF
!		  
	  CALL P_to_P_Calculation (Lo, La, LongRx, LatRx)
	  IF (HCM_Error .EQ. 1028) GOTO 100	! Distance > 1000 km
	  CALL Manage_List (N_rec, N_List, Rec_N_list, FS_list, Calculated_FS)
	  IF ((HCM_Error .NE. 0) .OR. INFO(7)) Goto 125
100	  N_rec = N_rec + teststep
	END DO
!
	IF (teststep .EQ. 5) THEN
		IF (N_List .EQ. 0) THEN
			teststep = 1
			N_rec = 1
			GOTO 90
		ELSE
		  N_List1 = N_List
		  DO I = 1, 3
		    Rec_N_list1(I) = Rec_N_list(I)
		    FS_list1(I) = FS_list(I)
		  END DO
		END IF
!
!	2nd:  calculate to every +2/-2 neighbouring centerpoint of stored centerpoints
!	Use 1st list again:
		N_List = 0	! number of stored records and field strength
		DO I = 1,3
		  FS_List(I) = 0.0
		  Rec_N_list(I) = 0
		END DO
		Calculated_FS = -999.9	! default setting
		IF (N_List1 .GT. 0) THEN
		  DO I = 1, N_List1
			J = Rec_N_list1(I)
			DO J = (J-2),(J+2)
			  IF (J .EQ. Rec_N_list1(I)) THEN
!				  This calculation is already done in the previous step!
				  Calculated_FS = FS_list1(I)
				ELSE
				  READ (3, REC=J, IOSTAT=IOS) C_Record
				  IF ((IOS .LT. 0) .OR. (IOS .EQ. 36)) EXIT
				  IF (IOS .NE. 0) THEN   
					HCM_Error = 1049
!					Error in line data
					CLOSE (UNIT = 3)
					RETURN
				  END IF
				  LongRx = N_Record(21) * RB
				  LatRx  = N_Record(22) * RB
				  Lo = LongTx
				  La = LatTx
!		check ctry affected if x-km or CBR
				  IF (D_to_border .NE. 0) THEN
					CALL Test_cut1 (Lo, La, LongRx, LatRx, N_cut)
					IF (N_cut .EQ. 0) GOTO 120
				  END IF
!		  
				  CALL P_to_P_Calculation (Lo, La, LongRx, LatRx)
				  IF (HCM_Error .EQ. 1028) GOTO 120	! Distance > 1000 km
			  END IF
			  CALL Manage_List (J, N_List, Rec_N_list, FS_list, Calculated_FS)
			  IF ((HCM_Error .NE. 0) .OR. INFO(7)) GOTO 125
120			  CONTINUE
			END DO
		  END DO
		END IF
	END IF
!
!	3rd: calculate to all points inside the stored records:
125	FS_x = -999.9
	IF (N_List .GT. 0) THEN
	  DO I = 1, N_List
		J = Rec_N_list(I)
		READ (3, REC=J, IOSTAT=IOS) C_Record
		IF (IOS .NE. 0) THEN   
		  HCM_Error = 1049
!		  Error in line data
		  CLOSE (UNIT = 3)
		  RETURN
		END IF
!		Calculate to all 10 points inside this record
		DO K = 1, 19, 2
		  LongRx = N_Record(K)   * RB
		  LatRx  = N_Record(K+1) * RB
		  Lo = LongTx
		  La = LatTx
!		check ctry affected if x-km or CBR
		  IF (D_to_border .NE. 0) THEN
			CALL Test_cut1 (Lo, La, LongRx, LatRx, N_cut)
			IF (N_cut .EQ. 0) GOTO 130
		  END IF
!		  
		  CALL P_to_P_Calculation ( Lo, La, LongRx, LatRx )
		  IF (HCM_Error .EQ. 1028) GOTO 130	! Distance > 1000 km
!		  Find maximun of field strength:
		  IF (Calculated_FS .GE. FS_x) THEN
			FS_x = Calculated_FS
			Rec_x = K
			Rec_N_x = J
		  END IF
		  IF ((HCM_Error .NE. 0) .OR. INFO(7)) GOTO 140
130		  CONTINUE
		END DO
	  END DO
	END IF
!
!	4th: calculate to point of maximum field strength again to get all
!		 output values:
140	IF (Rec_N_x .GT. 0) THEN
	  READ (3, REC=Rec_N_x, IOSTAT=IOS) C_Record
	  IF (IOS .NE. 0) THEN   
		HCM_Error = 1049
!		Error in line data
		RETURN
	  END IF
	  CLOSE (UNIT = 3)
	  LongRx = N_Record(Rec_x)   * RB
	  LatRx  = N_Record(Rec_x+1) * RB
	  Lo = LongTx
	  La = LatTx
	  IF (CBR) CALL CBR_Coordinates (LongRx, LatRx, Lo, La, CBR_D, Tx_serv_area)
	  CALL P_to_P_Calculation ( Lo, La, LongRx, LatRx )
    ELSE
	  HCM_Error = 1047
	END IF
!
	END SUBROUTINE Line_calculation
!
!
!	*********************** Test_cut1 ***************************
!	*	used by	:	CBR_Coordinates
!	*	uses	:
!	*************************************************************
!
	SUBROUTINE Test_cut1 (LONG, LAT, N_LONG, N_LAT, N_cut)
!
	IMPLICIT			NONE
!
	DOUBLE PRECISION	LONG, LAT, N_LONG, N_LAT
	INTEGER				N_cut
!
	INTEGER				I, N_cp
	DOUBLE PRECISION	CX, CY, DX, DY, AX, AY, BX, BY, RN, R, S
	DOUBLE PRECISION	Co_cp(2000,2)
!
	COMMON /Co_ord_cp/	Co_cp, N_cp
!
	N_cut = 0
!
	AX = LONG
	AY = LAT
	BX = N_LONG
	BY = N_LAT
!
	I = 2
!	Take first two points:
	CX = Co_cp(1,1)
	CY = Co_cp(1,2)
	DX = Co_cp(2,1)
	DY = Co_cp(2,2)
!
!	Determine intersection:
70	RN = (BX - AX) * (DY - CY) - (BY - AY) * (DX - CX)
	IF (RN .EQ. 0.0D0) GOTO 100
	R = ((AY - CY) * (DX - CX) - (AX - CX) * (DY - CY)) / RN
	S = ((AY - CY) * (BX - AX) - (AX - CX) * (BY - AY)) / RN
	IF ((R .GE. 0.0D0) .AND. (R .LE. 1.0D0) .AND. &
		(S .GE. 0.0D0) .AND. (S .LE. 1.0D0)) THEN
	  N_cut = 1
	  RETURN
	END IF
!
!	Take next line point:
100	CX = DX
	CY = DY
	I = I + 1
	IF (I .GT. N_cp) RETURN
	DX = Co_cp(I,1)
	DY = Co_cp(I,2)
	GOTO 70
!
	END SUBROUTINE Test_cut1
!
!	************************* CBR_Coordinates *******************
!	*	used by	:	Line_calculation															
!	*	uses	:	Calc_Direction, Calc_Distance,
!	*				New_coordinates, Test_cut1															
!	*************************************************************
!
!
	SUBROUTINE CBR_Coordinates (LongLi, LatLi, LongTx, LatTx, CBR_D, ServTx)
!
	IMPLICIT			NONE
!
	DOUBLE PRECISION	LongLi, LatLi, LongTx, LatTx
	REAL				CBR_D, ServTx
!
	DOUBLE PRECISION	Dir, Lo, La, D
!
	CALL Calc_direction (LongTx, LatTx, LongLi, LatLi, Dir)
	Lo = LongLi
	La = LatLi
	CALL New_coordinates (Lo, La, Dir, CBR_D, LongLi, LatLi)
!
!	In case of Tx is a mobile, calculate new Tx co-ordinates:
	IF (ServTx .GT. 0.0) THEN
	  CALL Calc_distance (LongTx, LatTx, Lo, La, D)
	  IF (D .LE. ServTx) THEN
!		  Tx position = borderline point
		  LongTx = Lo	! = borderline point
		  LatTx  = La	! = borderline point
	    ELSE
!		  Calculate new Tx position
		  Lo = LongTx	! starting point
		  La = LatTx	! starting point
!		  Ending point = LongTX, LatTx
		  CALL New_coordinates (Lo, La, Dir, ServTx, LongTx, LatTx)
	  END IF
	END IF
!
	END SUBROUTINE CBR_Coordinates
!
!	************************* Manage_List ***********************
!	*	used by	:	Line_calculation															
!	*	uses	:												
!	*************************************************************
!
	SUBROUTINE Manage_List (N_rec, N_List, Rec_N_list, FS_list, Calculated_FS)
!
	IMPLICIT		NONE
!
	INTEGER			N_rec, N_List, Rec_N_list(3)
	REAL			Calculated_FS, FS_list(3)
!
	INTEGER			I, J
	REAL			FS_x
!
	IF (N_List .LT. 3) THEN
	  N_List = N_list + 1
	  Rec_N_list(N_List) = N_rec
	  FS_list(N_List) = Calculated_FS
	ELSE
!
!	More than MAX enties:
!	Find list-number of lowest fiel strength:
	  J = 1
	  FS_x = 999.9
	  DO I = 1, 3
	    IF (FS_list(I) .LT. FS_x) THEN
	      J = I
		  FS_x = FS_list(I)
	    END IF
	  END DO
!	J = number of list entry with the lowest field strength
	  IF (Calculated_FS .GT. FS_list(J)) THEN
	    FS_list(J) = Calculated_FS
	    Rec_N_list(J) = N_rec
	  END IF
	END IF
!
	END SUBROUTINE Manage_List
