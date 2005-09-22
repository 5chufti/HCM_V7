!
!	Point_type.f90										P. Benner		09.10.2003
!														G.H.			22.09.2005
!
!
!	Subroutine to read the morphological type of a given point from the morpho-database.
!
!
!	Input values:
!			Long		DOUBLE PRECISION	longitude of point
!			Lat			DOUBLE PRECISION	latitude of point
!			Morpho_path	CHARACTER*63		path of morpho database, e.g. 'D:\MORPHO'
!
!
!	Output values:
!			M_Type		INTEGER*2			type of morphologie
!			Error		INTEGER*4			error value
!
!	Possible error values
!		  0 no error
!		 36 error opening file (no data)
!		200 error in longitude 
!		210 error in latitude 
!		220 error reading record
!		300 latitude is not in range of 0.0 - 90.0
!
!	Morpho-data location: Morpho_path\Subdir\
!			where subdir is first 4 char of filename
!
!	Morpho_Path e.g. = D:\MORPHO
!
!	Filenames are (example) E009N50.63M or E012N45.33M where
!		E009 or E012  is  9 or 12 degree east,
!		N50  or  N45  is 50 or 45 degree north,
!		33   or   63  is the resolution in east-west (3 or 6)
!					  and in north-south direction (always 3) 
!					  in seconds and
!				   M  for morphological-data.
!
!	Filenames correspond to the south-west corner of a 1 * 1
!	degree block.
!
!	**********************************************************************************
!
	SUBROUTINE Point_type (Long, Lat, M_Type, Error)
!
	IMPLICIT			NONE
!
	INCLUDE				'HCM_MS_V7_definitions.F90'
!
	INTEGER(2)			M_Type, M1, M2, M3, M4
	INTEGER(2)			M_F_3(101,101), M_F_6(51,101)
	INTEGER(4)			Error, RESH, SLO, SLA, IOS, LOD, LAD, BH, BV, T, OLD_T
	INTEGER(4)			P1X, P1Y
	DOUBLE PRECISION	Long, Lat, LOR, LAR, LO_P1, LA_P1, RELLO, RELLA
	CHARACTER(1)		M_3(20402), M_6(10302)
	CHARACTER(11)		FN, O_FN
!
	EQUIVALENCE (M_F_3, M_3)
	EQUIVALENCE (M_F_6, M_6) 
!
!	**********************************************************************************
!
	Error  = 0
	M_Type = 0	! normal land
!
!
!                 Field M_F_3(I,J)
!
!         I=1,J=101   ********************* I=101,J=101
!                     *********************
!              North  *********************
!                     *********************
!                     *********************
!                     *********************
!                     *********************
!             South   *********************
!                     *********************
!         I=1,J=1     ********************* I=101,J=1
!     
!                   West                 East
!                                          
!
!                 Field M_F_6(I,J)
!
!         I=1,J=101   ********************* I=51,J=101
!                     *********************
!              North  *********************
!                     *********************
!                     *********************
!                     *********************
!                     *********************
!             South   *********************
!                     *********************
!         I=1,J=1     ********************* I=51,J=1
!
!                   West                 East
!
!
	OLD_T = 0
	O_FN = ""
!
!	Longitude in range of -180.0  to 180.0 
	IF (Long .GT. 1.8D2) Long = Long - 3.6D2
	IF (DABS(Long) .GT. 1.8D2) THEN
	  Error = 200
	  RETURN
	END IF
!
!	Latitude in range of -90.0  to 90.0 
	IF (Lat .GT. 1.8D2) Lat = Lat - 3.6D2
	IF (DABS(Lat) .GT. 9.0D1) THEN
	  Error = 210
	  RETURN
	END IF
!
	LOD = DINT(Long)
	LAD = DINT(Lat)
	LOD = ABS(LOD)
	LAD = ABS(LAD)
!
	WRITE (FN(2:4), '(I3.3)', IOSTAT=IOS) LOD
	IF (Long .GE. 0.0D0) THEN
		FN(1:1) = 'E'                     
	  ELSE
		FN(1:1) = 'W'                     
	END IF
	IF (IOS.NE. 0) THEN
	  ERROR = 200
	  RETURN
	END IF
!
	WRITE (FN(6:7), '(I2.2)', IOSTAT=IOS) LAD
	IF (Lat .GE. 0.0D0) THEN
		FN(5:5) = 'N'
	  ELSE
		FN(5:5) = 'S'
	END IF
	IF (IOS.NE. 0) THEN
	  ERROR = 210
	  RETURN
	END IF
!
	IF (LAD .LT. 50) THEN 
		RESH = 3  
		FN(8:11) = '.33M'
	  ELSE
		RESH = 6
		FN(8:11) = '.63M'
	END IF
!
!	Remaining value behind the decimalpoint in degrees:
	LOR = DABS(Long - DBLE(DINT(Long)))
	LAR = DABS(Lat  - DBLE(DINT(Lat)))
!
!
!	Remaining value behind the decimalpoint in seconds:
	SLO = DNINT(3.6D3 * LOR)
	IF (SLO .GE. 3600) THEN
	  SLO = 0
	  IF (LOD .GE. 0) THEN
		  LOD = LOD + 1
		ELSE
		  LOD = LOD - 1
	  END IF
	  LOR = 0.0D0
	END IF
	SLA = NINT(3.6D3 * LAR)
	IF (SLA .GE. 3600) THEN
	  SLA = 0
	  IF (LAD .GE. 0) THEN
		  LAD = LAD + 1
		ELSE
		  LAD = LAD - 1
	  END IF
	  LAR = 0.0D0
	END IF
!                        
!	Data-block-No. horizontal:
	BH = 1 + SLO / 300   
!      
!	Data-block-No. vertical:
	BV = 1 + SLA / 300   
!
!	Block number T:
	T = (BV-1) * 12 + BH
!
	IF ((FN .EQ. O_FN) .AND. (T .EQ. OLD_T)) GOTO 100
	IF (FN .EQ. O_FN) GOTO 50
!   
!	Data not present and file not open:
	IF (RESH .EQ. 3) THEN
		OPEN (UNIT=2, FILE=TRIM(Morpho_path) // '\' // FN(1:4) // '\' // FN,  &
				ACCESS='DIRECT',RECL=20402, STATUS='OLD', IOSTAT=IOS, &
				MODE='READ')
	  ELSE
		OPEN (UNIT=2, FILE=TRIM(Morpho_path) // '\' // FN(1:4) // '\' // FN, &
				ACCESS='DIRECT',RECL=10302, STATUS='OLD', IOSTAT=IOS, &
				MODE='READ')
	END IF
	IF (IOS .NE. 0) THEN
	  ERROR = 36
	  RETURN
	END IF
!      
	O_FN = FN
!
!	Data not present, but file is open:  
!	Read RECORD :
50	IF (RESH .EQ. 3) READ (2, IOSTAT=IOS, REC=T) M_3
	IF (RESH .EQ. 6) READ (2, IOSTAT=IOS, REC=T) M_6
	IF (IOS .NE. 0) THEN
	  ERROR = 220
	  RETURN
	END IF
!  
	OLD_T = T
!                                           
!	Record-No. in the files:
!
!       133 134 135 136 137 138 139 140 141 142 143 144                    
!       121                                         132                    
!       109                                         120                    
!       097                                         108                    
!       085                                         096                    
!       073                                         084                    
!       061                                         072                    
!       049                                         060                    
!       037                                         048                    
!       025                                         036                    
!       013                                         024                    
!       001 002 003 004 005 006 007 008 009 010 011 012
!
!	Calculate height :
!
!	Calculate P1 :
!
!         Morphlogical information surrounding point "P" :
!
!                   P3            P4
!
!                            P
!
!
!                   P1            P2           
!
!
100	P1X = INT((SLO - (BH - 1) * 300) / RESH + 1)
	P1Y = INT((SLA - (BV - 1) * 300) / 3 + 1)
	IF (RESH .EQ. 3) THEN
	  M1 = M_F_3(P1X, P1Y)
	  M2 = M_F_3(P1X+1, P1Y)
	  M3 = M_F_3(P1X, P1Y+1)
	  M4 = M_F_3(P1X+1, P1Y+1)
	END IF
	IF (RESH .EQ. 6) THEN
	  M1 = M_F_6(P1X, P1Y)
	  M2 = M_F_6(P1X+1, P1Y)
	  M3 = M_F_6(P1X, P1Y+1)
	  M4 = M_F_6(P1X+1, P1Y+1)
	END IF
!
!
!	Point-1 position in degrees relativ to the beginning of block:
	LO_P1 = DBLE ((BH-1) * 300 + (P1X-1) * RESH) / 3.6D3
	LA_P1 = DBLE ((BV-1) * 300 + (P1Y-1) *    3) / 3.6D3
!
!	Position of P relativ to P1 in degrees :
	RELLO = LOR - LO_P1
	RELLA = LAR - LA_P1                   
!   
!	In seconds :
	LOR = RELLO * 3.6D3
	LAR = RELLA * 3.6D3
!      
	IF (LOR .LT. REAL(RESH)/2.0) THEN
		! take point 1 or 3
		IF (LAR .LT. 1.5) THEN
			! take point 1
			M_Type = M1
			RETURN
		  ELSE
			! take point 3
			M_Type = M3
			RETURN
		END IF
	  ELSE
		! take point 2 or 4 
		IF (LAR .LT. 1.5) THEN
			! take point 2
			M_Type = M2
			RETURN
		  ELSE
			! take point 4
			M_Type = M4
			RETURN
		END IF
	END IF
!
	RETURN
!
	END SUBROUTINE Point_type
!
!	**********************************************************************************
!
