!
!	Point_type.f90										P. Benner		09.10.2003
!														G.H.			25.04.2017
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
!	!!!!   Longitudes are [0° - 360°[    !!!!!
!
!	Filenames correspond to the south-west corner of a 1 * 1
!	degree block in !both! hemispheres. 
!   allways the lowest long(0..360)/lat(-90..+90) = 1st record in file)
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
!                 
!	one record:
!				Field M_F_3(I,J)
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
!	**********************************************************************************
!
	SUBROUTINE Point_type (Long, Lat, M_Type)
!
	IMPLICIT			NONE
!
	INCLUDE				'HCM_MS_V7_definitions.F90'
!
	DOUBLE PRECISION	Long, Lat
	INTEGER(2)			M_Type
!
	INTEGER(4)			LOD, LAD, LOR, LAR, BH, BV, R, E
	DOUBLE PRECISION	EH, EV
	CHARACTER(1)		H_C(20402)
	CHARACTER(11)		FN
!
	EQUIVALENCE			(H_M,H_C)
!	**********************************************************************************
!
	M_Type = 0	! normal land
!
!	convert Longitude, split in Integer and Remainder
	LOD = FLOOR(Long)
	LOR = IDNINT((Long-LOD)*3.599D3)
	LAD = FLOOR(Lat)
	LAR = IDNINT((Lat-LAD)*3.599D3)
!
	IF ((O_LOM .NE. LOD) .OR. (O_LAM .NE. LAD)) THEN
		O_LOM=LOD
		O_LAM=LAD
		CLOSE(UNIT=6)

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
			FN(8:11) = '.33M'
		ELSE
			RESH = 51
			FN(8:11) = '.63M'
		END IF
		OLD_M=-1
		OPEN (UNIT=6, FILE=TRIM(Morpho_path) // '/' // FN(1:4) // '/' // FN,  &
		ACCESS='DIRECT',RECL=202*(RESH), STATUS='OLD', &
		ERR=400, MODE='READ')
	END IF
!
!	coordinates of block containing P
	BH = INT(LOR/300)
	BV = INT(LAR/300)
!	coordinates of point P in block
	EH = DBLE(LOR-BH*3D2)/DBLE(300/(RESH-1))
	EV = DBLE(LAR-BV*3D2)/3D0
!
	R = 12*BV + BH + 1
!
	IF (R .NE. OLD_M) THEN
!	point in new file
		READ (UNIT=6, ERR=450, REC=R) H_C(1:(202*(RESH)))
		OLD_M = R
	END IF
!
	E = NINT(EV)*(RESH) + NINT(EH) + 1
!
!	Read Element :
	M_Type=H_M(E)
!
	RETURN
!
400	HCM_Error = 36
	RETURN
!
450	HCM_Error = 220
	RETURN
!
	END SUBROUTINE Point_type
!
!	**********************************************************************************
!
