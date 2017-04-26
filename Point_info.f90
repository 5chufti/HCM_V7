!
!	Point_info.F90									P.Benner		20.11.2003
!														G.H.			27.04.2017
!
!	Subroutine to read the info of a given point from the database.
!
!
!	Input values:
!			Long		DOUBLE PRECISION	longitude of point	(-180.0....+180.0)
!			Lat			DOUBLE PRECISION	latitude of point	(-90.0...+90.0)
!			Topo_path	CHARACTER*63		path of terrain database, e.g. 'D:\TOPO'
!			Morpho_path	CHARACTER*63		path of morpho database, e.g. 'D:\MORPHO'
!
!
!	Output values:
!			Height		INTEGER*2			height of point in meter
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
!		400 height is missing (-9999)
!
!	Terrain-data location: Topo_path\Subdir\
!	Morpho-data location: Morpho_path\Subdir\
!		where Subdir is first 4 char of filename
!
!	Topo_Path e.g. = D:\TOPO
!	Morpho_Path e.g. = D:\MORPHO
!
!	Filenames are (example) E009N50.63E or E012N45.33M where
!		E009 or E012  is  9 or 12 degree east,
!		N50  or  N45  is 50 or 45 degree north,
!		33   or   63  is the resolution in east-west (3 or 6)
!					  and in north-south direction (always 3) 
!					  in seconds and
!				   E  for elevation-data,
!				   M  for morphological-data.
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
	SUBROUTINE Point_info (Long, Lat, Height, M_Type)
!
	IMPLICIT	NONE
!
	INCLUDE				'HCM_MS_V7_definitions.F90'
!
	DOUBLE PRECISION	Long, Lat
	INTEGER(2)			Height, M_Type
!			
	INTEGER(2)			H1, H2, H3, H4
	INTEGER(4)			LOD, LAD, LOR, LAR, BH, BV, R, E
	DOUBLE PRECISION	EH, EV, LORR, LARR, H12, H34
	CHARACTER(1)		H_C(20402), M_C(20402)
	CHARACTER(10)		FN
!
	EQUIVALENCE			(H_I,H_C),(M_I,M_C)
!	**********************************************************************************
!
	Height = -9999
	M_Type = 0
!
!	convert Longitude, split in Integer and Remainder
	LOD = FLOOR(Long)
	LOR = IDNINT((Long-LOD)*3.599D3)
	LAD = FLOOR(Lat)
	LAR = IDNINT((Lat-LAD)*3.599D3)
!
	IF ((O_LOD .NE. LOD) .OR. (O_LAD .NE. LAD)) THEN
		O_LOD=LOD
		O_LAD=LAD
		CLOSE(UNIT=5)

		IF (LOD .GE. 0) THEN
			FN(1:1) = 'E'                     
		ELSE
			FN(1:1) = 'W'
		END IF
		WRITE (FN(2:4), '(I3.3)') ABS(LOD)
!
		IF (LAD .GE. 0) THEN
			FN(5:5) = 'N'
		ELSE
			FN(5:5) = 'S'
		END IF
		WRITE (FN(6:7), '(I2.2)') ABS(LAD)
!
		IF (ABS(LAD) .LT. 50) THEN 
			RESH = 101  
			FN(8:10) = '.33'
		ELSE
			RESH = 51
			FN(8:10) = '.63'
		END IF
!	open topo
		OPEN (UNIT=5, FILE=TRIM(Topo_path) // '/' // FN(1:4) // '/' // FN // 'E',  &
		ACCESS='DIRECT',RECL=202*(RESH), STATUS='OLD', &
		ERR=400, MODE='READ')
!	open morpho
		IF (with_morpho) THEN
			CLOSE (UNIT=6)
			OPEN (UNIT=6, FILE=TRIM(Morpho_path) // '/' // FN(1:4) // '/' // FN // 'M',  &
				ACCESS='DIRECT',RECL=202*(RESH), STATUS='OLD', IOSTAT=OLD_R, MODE='READ')
			IF (OLD_R .NE. 0) THEN
				with_morpho = .FALSE.
				D_sea_input = '  0.0'
			END IF
		END IF
		OLD_R=-1
	END IF
!
!	coordinates of block containing P
	BH = (LOR/300)
	BV = (LAR/300)
!	coordinates of point P in block
	EH = DBLE(LOR-BH*3D2)/DBLE(300/(RESH-1))
	EV = DBLE(LAR-BV*3D2)/3D0
!
	R = 12*BV + BH + 1
!
	IF (R .NE. OLD_R) THEN
!	point in new file
!
		READ (UNIT=5, ERR=450, REC=R) H_C(1:(202*(RESH)))
		IF (with_morpho) READ (UNIT=6, ERR=450, REC=R) M_C(1:(202*(RESH)))
		OLD_R = R
	END IF
!
!	get morpho
	IF (with_morpho) M_Type=M_I(NINT(EV)*(RESH) + NINT(EH) + 1)
!	
	E = INT(EV)*(RESH) + INT(EH) + 1
!
!	get topo
	H1=H_I(E)
	H2=H_I(E+1)
	H3=H_I(E+RESH)
	H4=H_I(E+RESH+1)
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
	END SUBROUTINE Point_info
!
!	**********************************************************************************
!