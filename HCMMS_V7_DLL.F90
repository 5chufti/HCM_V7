!
!	HCMMS_V7_DLL.F90									P. Benner		08.01.2004
!														G.H.			13.04.2005
!	DLL to the HCMMS_V7 subroutuine (Berlin 2003)
!
	SUBROUTINE HCMMS_V7_DLL ( I_C_mode, I_bor_dis, I_PD, I_Distance, I_H_Datab_Tx, &
							  I_H_Datab_Rx, I_HCM_error, I_Heff, I_Dh, I_Dh_corr, &
							  I_Power_to_Rx, I_Free_space_FS, I_Land_FS, I_Sea_FS, &
							  I_Tx_ant_corr, I_Tx_ant_type_corr, I_Dir_Tx_Rx, &
							  I_V_angle_Tx_Rx, I_Tx_TCA, I_Rx_TCA, I_Tx_TCA_corr, &
							  I_Rx_TCA_corr, I_D_sea_calculated, I_Rx_ant_corr, &
							  I_Rx_ant_type_corr, I_Delta_frequency, I_Corr_delta_f, &
							  I_Calculated_FS, I_Perm_FS, I_CBR_D, I_ERP_ref_Tx, &
							  I_Prot_margin, I_str)
!
	!DEC$  ATTRIBUTES DLLEXPORT::HCMMS_V7_DLL
	!DEC$  ATTRIBUTES ALIAS:'HCMMS_V7_DLL'::HCMMS_V7_DLL
!
!
	IMPLICIT	NONE
!
!	Include the interface definitions:
	INCLUDE			   'HCM_MS_V7_definitions.F90'
!
	INTEGER*2			I_H_Datab_Tx, I_H_Datab_Rx
	INTEGER*4			I_HCM_error
	DOUBLE PRECISION	I_PD, I_Distance, I_Dir_Tx_Rx, I_V_angle_Tx_Rx
	DOUBLE PRECISION	I_D_sea_calculated, I_Delta_frequency
	REAL				I_Heff, I_Dh, I_Dh_corr, I_Power_to_Rx, I_Tx_TCA
	REAL				I_Free_space_FS, I_Land_FS, I_Sea_FS, I_Rx_TCA
	REAL				I_Tx_ant_corr, I_Tx_ant_type_corr, I_Tx_TCA_corr
	REAL				I_Rx_TCA_corr, I_Rx_ant_corr, I_Rx_ant_type_corr
	REAL				I_Corr_delta_f, I_Calculated_FS, I_Perm_FS
	REAL				I_CBR_D, I_ERP_ref_Tx, I_Prot_margin
	INTEGER				I, J, IOS
	INTEGER				I_C_mode, I_bor_dis, DB_LEN
	CHARACTER*128		DEBUG_DIR
	CHARACTER*(*)		I_str
	LOGICAL				DEBUG
!
!
!	***********************************************************************
!
!	Prepare input values:
!
!	Test INTSTR
	J = LEN(I_str)
	IF (J .LT. 432) THEN
	  I_HCM_error = 3000
	  RETURN
	END IF
!
	DEBUG = .FALSE.
	IF (J .GT. 432) THEN
	  IF ((I_str(433:433) .EQ. CHAR(0)) .OR. (I_str(433:433) .EQ. " ")) THEN
!		  Do nothing, no valid directory
	    ELSE
		  IF (J .LE. 560) THEN
			  DB_LEN = J - 432
			  DEBUG_DIR(1:DB_LEN) = I_str(433:J)
			ELSE
			  DEBUG_DIR = I_str(433:560)
			  DB_LEN = 128
		  END IF 
		  OPEN (UNIT = 12, FILE = DEBUG_DIR(1:DB_LEN) // "\\INPUT.TXT", IOSTAT=IOS)
		  IF (IOS .EQ. 0) DEBUG = .TRUE.
	  END IF
	END IF
!
!	Geogr. cooordinates of TX
	Coo_Tx = I_str(1:15)
!
!	Geogr. cooordinates of RX
	Coo_Rx = I_str(16:30)
!
!	Height of TX above sea level
	H_Tx_input = I_str(31:34)
!	
!	Height of RX above sea level
	H_Rx_input = I_str(35:38)
!
!	TX type of horizontal antenna
	Ant_typ_H_Tx = I_str(39:45)
!
!	TX type of vertictal antenna
	Ant_typ_V_Tx = I_str(46:52)
!
!	TX azimuth
	Azi_Tx_input = I_str(53:57)
!
!	TX elevation
	Ele_Tx_input = I_str(58:62)
!
!	TX antenna height
	H_Tx_ant = I_str(63:66)
!
!	RX antenna height
	H_Rx_ant = I_str(67:70)
!
!	Type of TX antenna (E/I)
	Type_of_Tx_ant = I_str(71:71)
!
!	Maximum radiated power
	Max_power = I_str(72:77)
!
!	TX frequency
	Tx_frequ = I_str(78:89)
!
!	Channel occupation
	Chan_occup = I_str(90:90)
!
!	Sea temperature
	Sea_temperature = I_str(91:91)
!
!	TX service area
	Rad_of_Tx_serv_area = I_str(92:96)
!
!	RX service area
	Rad_of_Rx_serv_area = I_str(97:101)
!
!	Input value: distance over sea
	D_sea_input = I_str(102:106)
!
!	RX frequency
	Rx_frequ = I_str(107:118)
!
!	RX designation of emmission
	Desig_of_Rx_emis = I_str(119:127)
!
!	TX designation of emmission
	Desig_of_Tx_emis = I_str(128:136)
!
!	RX type of horizontal antenna
	Ant_typ_H_Rx = I_str(137:143)
!
!	RX type of vertictal antenna
	Ant_typ_V_Rx = I_str(144:150)
!
!	RX azimuth
	Azi_Rx_input = I_str(151:155)
!
!	RX elevation
	Ele_Rx_input = I_str(156:160)
!
!	Type of RX antenna (E/I)
	Type_of_Rx_ant = I_str(161:161)
!
!	Gain of RX antenna
	Rx_ant_gain = I_str(162:165)
!
!	Depolarization loss
	Depol_loss = I_str(166:169)
!
!	Input value: permissible field strength
	Perm_FS_input = I_str(170:174)
!
!	Input value: correction factor according frequency difference
	Cor_fact_frequ_diff = I_str(175:178)
!
!	Calculation mode
	C_mode  = I_C_mode
!
!	Country of Rx or country to calculate to
	Land_to = I_str(179:181)
!
!	Country of Tx
	Land_from = I_str(182:184)
!
!	Distance to the border line
	D_to_border  = I_bor_dis
!
!	Input value: maximum crossborder range
	Max_CBR_D_input = I_str(185:187)
!
!	Topo - path
	Topo_path = I_str(188:250)
!
!	Border - path
	Border_path = I_str(251:313)
!
!	Morpho - path
	Morpho_path = I_str(314:376)
!
!
	I_str(377:432) = ' '
!
!	Distance between two profile points
	PD  = I_PD
!
	IF (DEBUG) THEN
		WRITE (12,*) '  Coo_Tx              = ',Coo_Tx
		WRITE (12,*) '  Coo_Rx              = ',Coo_Rx
		WRITE (12,*) '  H_Tx_input          = ',H_Tx_input
		WRITE (12,*) '  H_Rx_input          = ',H_Rx_input
		WRITE (12,*) '  Ant_typ_H_Tx        = ',Ant_typ_H_Tx
		WRITE (12,*) '  Ant_typ_V_Tx        = ',Ant_typ_V_Tx
		WRITE (12,*) '  Azi_Tx_input        = ',Azi_Tx_input
		WRITE (12,*) '  Ele_Tx_input        = ',Ele_Tx_input
		WRITE (12,*) '  H_Tx_ant            = ',H_Tx_ant
		WRITE (12,*) '  H_Rx_ant            = ',H_Rx_ant
		WRITE (12,*) '  Type_of_Tx_ant      = ',Type_of_Tx_ant
		WRITE (12,*) '  Max_power           = ',Max_power
		WRITE (12,*) '  Tx_frequ            = ',Tx_frequ
		WRITE (12,*) '  Chan_occup          = ',Chan_occup
		WRITE (12,*) '  Sea_temperature     = ',Sea_temperature
		WRITE (12,*) '  Rad_of_Tx_serv_area = ',Rad_of_Tx_serv_area
		WRITE (12,*) '  Rad_of_Rx_serv_area = ',Rad_of_Rx_serv_area
		WRITE (12,*) '  D_sea_input         = ',D_sea_input
		WRITE (12,*) '  Rx_frequ            = ',Rx_frequ
		WRITE (12,*) '  Desig_of_Rx_emis    = ',Desig_of_Rx_emis
		WRITE (12,*) '  Desig_of_Tx_emis    = ',Desig_of_Tx_emis
		WRITE (12,*) '  Ant_typ_H_Rx        = ',Ant_typ_H_Rx
		WRITE (12,*) '  Ant_typ_V_Rx        = ',Ant_typ_V_Rx
		WRITE (12,*) '  Azi_Rx_input        = ',Azi_Rx_input
		WRITE (12,*) '  Ele_Rx_input        = ',Ele_Rx_input
		WRITE (12,*) '  Type_of_Rx_ant      = ',Type_of_Rx_ant
		WRITE (12,*) '  Rx_ant_gain         = ',Rx_ant_gain
		WRITE (12,*) '  Depol_loss          = ',Depol_loss
		WRITE (12,*) '  Perm_FS_input       = ',Perm_FS_input
		WRITE (12,*) '  Cor_fact_frequ_diff = ',Cor_fact_frequ_diff
		WRITE (12,*) '  C_mode              = ',C_mode
		WRITE (12,*) '  Land_to             = ',Land_to
		WRITE (12,*) '  Land_from           = ',Land_from
		WRITE (12,*) '  D_to_border         = ',D_to_border
		WRITE (12,*) '  Max_CBR_D_input     = ',Max_CBR_D_input
		WRITE (12,*) '  Topo_path           = ',Topo_path
		WRITE (12,*) '  Border_path         = ',Border_path
		WRITE (12,*) '  Morpho_path         = ',Morpho_path
		WRITE (12,*) '  PD                  = ',PD
		CLOSE (UNIT = 12)	
	END IF
!
!	***********************************************************************
!
!	Call HCM_MS_V7 subroutuine:
!
	CALL HCM_MS_V7
!
!	***********************************************************************
!
!	Set output values:
!
	I_str(377:382) = Version		! Version number
	I_Distance     = Distance		! Distance
	I_H_Datab_Tx   = H_Datab_Tx		! Heigth of TX above sea level (terrain database)
	I_H_Datab_Rx   = H_Datab_Rx		! Heigth of RX above sea level (terrain database)
	I_HCM_error    = HCM_error		! Error value
!
!	Info
	J = 383
	DO I = 1, 20
	  IF (Info(I)) THEN
		  I_str(J:J) = "T"
	    ELSE
		  I_str(J:J) = "F"
	  END IF 
	  J = J + 1
	END DO
!
	I_Heff             = Heff			 ! Effective antenna height
	I_Dh               = Dh				 ! Terrain irregularity
	I_Dh_corr          = Dh_corr		 ! Correction factor according terrain irregularity
	I_Power_to_Rx      = Power_to_Rx	 ! Power in the direction of the receiver
	I_Free_space_FS    = Free_space_FS	 ! Free space field strength
	I_Land_FS          = Land_FS		 ! Land field strength
	I_Sea_FS           = Sea_FS			 ! Sea field strength
	I_Tx_ant_corr      = Tx_ant_corr	 ! Correction according Tx antenna (H + V)
	I_Tx_ant_type_corr = Tx_ant_type_corr ! Correction according antenna type (E/I) TX
	I_Dir_Tx_Rx        = Dir_Tx_Rx		 ! Horizontal direction TX to RX
	I_V_angle_Tx_Rx    = V_angle_Tx_Rx	 ! Vertical direction TX to RX
	I_Tx_TCA           = Tx_TCA			 ! TX clearance angle
	I_Rx_TCA           = Rx_TCA			 ! RX clearance angle
	I_Tx_TCA_corr      = Tx_TCA_corr	 ! TX clearance angle correction factor
	I_Rx_TCA_corr      = Rx_TCA_corr	 ! RX clearance angle correction factor
	I_D_sea_calculated = D_sea_calculated ! Distance over sea calculated
	I_Rx_ant_corr      = Rx_ant_corr	 ! Correction according Rx antenna (H + V)
	I_Tx_ant_type_corr = Tx_ant_type_corr ! Correction according antenna type (E/I) Rx
	I_Delta_frequency  = Delta_frequency ! Frequency difference (kHz)
	I_Corr_delta_f     = Corr_delta_f	 ! Correction factor according frequency difference
	I_Calculated_FS    = Calculated_FS	 ! Calculated field strength
	I_Perm_FS          = Perm_FS		 ! Permissible field strength
	I_CBR_D            = CBR_D			 ! Maximum range of harmful interference
	I_ERP_ref_Tx       = ERP_ref_Tx		 ! Power of the reference transmitter
	I_Prot_margin      = Prot_margin	 ! Protection margin (EP - EC)
	I_str(403:417) = Coo_Tx_new			 ! Calculated co-ordinates of the transmitter
	I_str(418:432) = Coo_Rx_new			 ! Calculated co-ordinates of the receiver
!
!
	IF (DEBUG) THEN
		OPEN (UNIT = 12, FILE = DEBUG_DIR(1:DB_LEN) // "\\OUTPUT.TXT")
		WRITE (12,*) '  Version = ',Version
		WRITE (12,*) '  Distance   = ',Distance
		WRITE (12,*) '  H_Datab_Tx    = ',H_Datab_Tx
		WRITE (12,*) '  H_Datab_Rx    = ',H_Datab_Rx
		WRITE (12,*) '  HCM_error  = ',HCM_error
		WRITE (12,*) '  Info  = ',Info
		WRITE (12,*) '  Heff     = ',Heff
		WRITE (12,*) '  Dh     = ',Dh
		WRITE (12,*) '  Dh_corr    = ',Dh_corr
		WRITE (12,*) '  Power_to_Rx     = ',Power_to_Rx
		WRITE (12,*) '  Free_space_FS     = ',Free_space_FS
		WRITE (12,*) '  Land_FS     = ',Land_FS
		WRITE (12,*) '  Sea_FS     = ',Sea_FS
		WRITE (12,*) '  Tx_ant_corr     = ',Tx_ant_corr
		WRITE (12,*) '  Tx_ant_type_corr     = ',Tx_ant_type_corr
		WRITE (12,*) '  Dir_Tx_Rx  = ',Dir_Tx_Rx
		WRITE (12,*) '  V_angle_Tx_Rx = ',V_angle_Tx_Rx
		WRITE (12,*) '  V_diff_angle_Tx_Rx    = ',V_diff_angle_Tx_Rx
		WRITE (12,*) '  H_diff_angle_Tx_Rx    = ',H_diff_angle_Tx_Rx
		WRITE (12,*) '  Tx_TCA = ',Tx_TCA
		WRITE (12,*) '  Rx_TCA = ',Rx_TCA
		WRITE (12,*) '  Tx_TCA_corr = ',Tx_TCA_corr
		WRITE (12,*) '  Rx_TCA_corr = ',Rx_TCA_corr
		WRITE (12,*) '  D_sea_calculated = ',D_sea_calculated
		WRITE (12,*) '  Rx_ant_corr    = ',Rx_ant_corr
		WRITE (12,*) '  Tx_ant_type_corr    = ',Tx_ant_type_corr
		WRITE (12,*) '  Delta_frequency     = ',Delta_frequency
		WRITE (12,*) '  Corr_delta_f    = ',Corr_delta_f
		WRITE (12,*) '  Calculated_FS     = ',Calculated_FS
		WRITE (12,*) '  Perm_FS     = ',Perm_FS
		WRITE (12,*) '  CBR_D  = ',CBR_D
		WRITE (12,*) '  ERP_ref_Tx  = ',ERP_ref_Tx
		WRITE (12,*) '  Prot_margin  = ',Prot_margin
		WRITE (12,*) '  Coo_Tx_new = ',Coo_Tx_new
		WRITE (12,*) '  Coo_Rx_new = ',Coo_Rx_new
		CLOSE (UNIT = 12)
	END IF
!
!	***********************************************************************
!
	RETURN
!
	END SUBROUTINE HCMMS_V7_DLL
!