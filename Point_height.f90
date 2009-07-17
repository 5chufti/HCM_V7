!
!	Point_height.F90									P.Benner		20.11.2003
!														G.H.			17.07.2009
!
!	Subroutine to read the height of a given point from the terrain-database.
!
!
!	Input values:
!			Long		DOUBLE PRECISION	longitude of point	(-180.0....+180.0)
!			Lat			DOUBLE PRECISION	latitude of point	(-90.0...+90.0)
!			Topo_path	CHARACTER*63		path of terrain database, e.g. 'D:\TOPO'
!
!
!	Output values:
!			Height		INTEGER*2			height of point in meter
!			Error		INTEGER*4			error value
!
!	Possible error values
!		  0 no error
!		 36 error opening file (no data)
!		200 error in longitude 
!		210 error in latitude 
!		220 error reading record
!		300 latitude is not in range of 0.0 - 90.0
!		400 height is missing (-9999)
!
!	Terrain-data location: Topo_path\Subdir\
!		where Subdir is first 4 char of filename
!
!	Topo_Path e.g. = D:\TOPO
!
!	Filenames are (example) E009N50.63E or E012N45.33E where
!		E009 or E012  is  9 or 12 degree east,
!		N50  or  N45  is 50 or 45 degree north,
!		33   or   63  is the resolution in east-west (3 or 6)
!					  and in north-south direction (always 3) 
!					  in seconds and
!				   E  for elevation-data.
!
!	!!!!   Longitudes are [0° - 360°[    !!!!!
!
!	Filenames correspond to the south-west corner of a 1 * 1
!	degree block in !both! hemispheres. 
!   allways the lowest long(0..360)/lat(-90..+90) = 1st record in file)
!
!	Record-No. (R) in the files:
!
!       133 134 135 136 137 138 139 140 141 142 143 144                    
!       121                                         132                    
!       109                                         120                    
!       097                                         108                    
!       085                                         096                    
! (BV)  073                                         084                    
!       061                                         072                    
!       049                                         060                    
!       037                                         048                    
!       025                                         036                    
!       013                                         024                    
!       001 002 003 004 005 006 007 008 009 010 011 012
!                            (BH)
!
!	one record: 
!                Field H_F_3(I,J)
!
!         I=1,J=101   ********************* I=101,J=101
!                     *********************
!              North  *********************
!                     *********************
!                (EV) *********************
!                     *********************
!                     *********************
!             South   *********************
!                     *********************
!         I=1,J=1     ********************* I=101,J=1
!                             (EH)  
!                   West                 East
!                                          
!
!                 Field H_F_6(I,J)
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
!             Heights surrounding point "P" :
!
!                   P3            P4
!
!                            P
!
!
!                   P1(E)         P2           
!
!	**********************************************************************************
!
	SUBROUTINE Point_height (Long, Lat, Height)
!
	IMPLICIT	NONE
!
	INCLUDE				'HCM_MS_V7_definitions.F90'
!
	DOUBLE PRECISION	Long, Lat, Lo, La
	INTEGER(2)			Height
!			
	INTEGER(2)			H1, H2, H3, H4
	INTEGER(4)			RESH, LOD, LAD, BH, BV, R, E
	DOUBLE PRECISION	LOR, LAR, EH, EV, LORR, LARR, H12, H34
	CHARACTER(1)		H_C(20402)
	CHARACTER(11)		FN
!
	EQUIVALENCE			(H_X,H_C)
!	**********************************************************************************
!
	Height = -9999.9
!
!	convert Longitude, split in Integer and Remainder
	Lo  = DMOD(Long + 3.6D2,3.6D2)
	LOR = DNINT(DMOD(Lo,1D0)*3.6D3)
	LOD = INT(Lo)
	La  = DMOD(Lat + 9D1,9D1)
	LAR = DNINT(DMOD(La,1D0)*3.6D3)
	LAD = INT(La)
!
	IF (LOR .GT. 3599) LOR = 3599
	IF (LAR .GT. 3599) LAR = 3599
!
	IF (LOD .LT. 1.8D2) THEN
		FN(1:1) = 'E'                     
	  ELSE
		FN(1:1) = 'W'
		LOD = 360-LOD
	END IF
	WRITE (FN(2:4), '(I3.3)') LOD
!
	IF (Lat .GE. 0.0D0) THEN
		FN(5:5) = 'N'
	  ELSE
		FN(5:5) = 'S'
		LAD = 90-LAD
	END IF
	WRITE (FN(6:7), '(I2.2)') LAD
!
	IF (LAD .LT. 5D1) THEN 
		RESH = 100  
		FN(8:11) = '.33E'
	  ELSE
		RESH = 50
		FN(8:11) = '.63E'
	END IF
!
!	coordinates of block containing P
	BH = INT(LOR/3D2)
	BV = INT(LAR/3D2)
!	coordinates of point P in block
	EH = DBLE(LOR-BH*3D2)/DBLE(300/RESH)
	EV = DBLE(LAR-BV*3D2)/3D0
!
	R = 12*BV + BH + 1
!
	IF ((FN .EQ. O_FN) .AND. (R .EQ. OLD_T)) GOTO 200
	IF (FN .EQ. O_FN) GOTO 100
!	point in new file
	OPEN (UNIT=5, FILE=TRIM(Topo_path) // '\' // FN(1:4) // '\' // FN,  &
		ACCESS='DIRECT',RECL=202*(RESH+1), STATUS='OLD', &
		ERR=400, MODE='READ')
!
	O_FN=FN
!
100	READ (UNIT=5, ERR=450, REC=R) H_C(1:(202*(RESH+1)))
	OLD_T = R
!
200	E = INT(EV)*(RESH+1) + INT(EH) + 1
!
!	Read Element :
	H1=H_X(E)
	H2=H_X(E+1)
	H3=H_X(E+RESH+1)
	H4=H_X(E+RESH+2)
!
	IF (MIN(H1,H2,H3,H4) .EQ. -9999) THEN
	  HCM_Error = 400
	  RETURN
	END IF
!
!	coordinates of P relativ to P1
	LORR = DMOD(EH,1D0)
	LARR = DMOD(EV,1D0)
!	calculate height P
	H12 = DBLE(H1) + DBLE(H2 - H1) * LORR
	H34 = DBLE(H3) + DBLE(H4 - H3) * LORR
	Height = NINT(H12 + (H34 - H12) * LARR)
!
	RETURN
!
400	HCM_Error = 36
	RETURN
!
450	HCM_Error = 220
	RETURN
!
	END SUBROUTINE Point_Height
!
!	**********************************************************************************
!