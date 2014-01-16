!
!	HCMMS_V7_DLL.F90									P. Benner		08.01.2004
!														G.H.			30.10.2013
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
	IMPLICIT	NONE
!	Include the interface definitions:
	INCLUDE			   'HCM_MS_V7_definitions.F90'
!
	INTEGER*2			I_H_Datab_Tx, I_H_Datab_Rx
	INTEGER*4			I_HCM_error, I_C_mode, I_bor_dis, Ta, Te
	DOUBLE PRECISION	I_PD, I_Distance, I_Dir_Tx_Rx, I_V_angle_Tx_Rx
	DOUBLE PRECISION	I_D_sea_calculated, I_Delta_frequency
	REAL				I_Heff, I_Dh, I_Dh_corr, I_Power_to_Rx, I_Tx_TCA
	REAL				I_Free_space_FS, I_Land_FS, I_Sea_FS, I_Rx_TCA
	REAL				I_Tx_ant_corr, I_Tx_ant_type_corr, I_Tx_TCA_corr
	REAL				I_Rx_TCA_corr, I_Rx_ant_corr, I_Rx_ant_type_corr
	REAL				I_Corr_delta_f, I_Calculated_FS, I_Perm_FS
	REAL				I_CBR_D, I_ERP_ref_Tx, I_Prot_margin
	INTEGER				I, J, IOS
	CHARACTER*(*)		I_str 
	LOGICAL				DEBUG, doit
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
!	Country of Rx or country to calculate to
	Land_to = I_str(179:181)
!
!	Country of Tx
	Land_from = I_str(182:184)
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
!	Calculation mode
	C_mode  = I_C_mode
!
!	Distance to the border line
	D_to_border  = I_bor_dis
!
!	Distance between two profile points
	PD  = I_PD
!
	DEBUG = .FALSE.
	OPEN (UNIT = 2, FILE = TRIM(I_str(433:)) // "\\DEBUG.TXT", IOSTAT=IOS)
	IF (IOS .EQ. 0) THEN
		DEBUG = .TRUE.
!
		WRITE (2,*) ""
		WRITE (2,*) "                       R e s u l t s"
		WRITE (2,*) ""
		WRITE (2,'(A35,I4,A2,$)') &
				"  Mode of calculation            : ", C_mode, " ("
	SELECT CASE (C_mode)
		CASE (99) 
			WRITE (2,*) "Point to point like line calc.)"
		CASE  (12) 
			WRITE (2,*) "p2p non strict HCM t%=1.)"
		CASE  (11) 
			WRITE (2,*) "p2p non strict HCM t%=50.)"
		CASE  (10) 
			WRITE (2,*) "p2p non strict HCM t%=10.)"
		CASE  (9) 
			WRITE (2,*) "UMTS/IMT2000 point to p.)"
		CASE  (8) 
			WRITE (2,*) "380-400 MHz emerg./secur.)"
		CASE  (7) 
			WRITE (2,*) "Normal Agreement cov. 50% t.)"
		CASE  (6) 
			WRITE (2,*) "GSM1800 <-> GSM1800 ML 42 dBµV/m)"
		CASE  (5) 
			WRITE (2,*) "GSM1800 <-> GSM1800 FB 38 dBµV/m)"
		CASE  (4) 
			WRITE (2,*) "ERMES<->ERMES 32 dBµV/m)"
		CASE  (3)
			WRITE (2,*) "GSM900 <-> NMT)"
		CASE  (2) 
			WRITE (2,*) "GSM900 <-> TACS)"
		CASE  (1) 
			WRITE (2,*) "GSM900 <-> GSM900)"
		CASE  (0) 
			WRITE (2,*) "Normal Agreement)"
		CASE (-1) 
			WRITE (2,*) "Agreement line calc. h2=10m)"
		CASE (-2) 
			WRITE (2,*) "GSM900 line calc. h2=3m)"
		CASE (-3) 
			WRITE (2,*) "ERMES line calc.12 dbµV/m)"
		CASE (-4) 
			WRITE (2,*) "ERMES line calc.32 dbµV/m)"
		CASE (-5) 
			WRITE (2,*) "ERMES line calc.52 dbµV/m)"
		CASE (-6) 
			WRITE (2,*) "GSM1800 line calc. h2=3m)"
		CASE (-7) 
			WRITE (2,*) "380-400 MHz line calc.)"
		CASE (-8) 
			WRITE (2,*) "UMTS/IMT2000 line calc.)"
		CASE (-9) 
			WRITE (2,*) "p2l non strict HCM t%=10 h2=3.)"
		CASE (-10) 
			WRITE (2,*) "p2l non strict HCM t%=10 h2=10.)"
		CASE (-11) 
			WRITE (2,*) "p2l non strict HCM t%=50 h2=3.)"
	END SELECT
		WRITE (2,*) ""
		WRITE (2,*) "                     Input data :"
		WRITE (2,*)
		WRITE (2,*) " Geographical co-ordinates of Tx: ", Coo_Tx(1:8)," ",Coo_Tx(9:15)
		IF (C_mode .GE. 0) THEN
			WRITE (2,*) " Geographical co-ordinates of Rx: ", Coo_Rx(1:8)," ",Coo_Rx(9:15)
		END IF
		WRITE (2,*) " Height above sea level of Tx   : ", H_Tx_input, " m"
		IF (C_mode .GE. 0) THEN
			WRITE (2,*) " Height above sea level of Rx   : ", H_Rx_input, " m"
		END IF
		WRITE (2,*) " Type of antenna horiz. of Tx   : ", Ant_typ_H_Tx
		WRITE (2,*) " Type of antenna verti. of Tx   : ", Ant_typ_V_Tx
		WRITE (2,*) " Azimuth of max. radiation of Tx: ", Azi_Tx_input, " degr."
		WRITE (2,*) " Elevation of max. radia. of Tx : ", Ele_Tx_input, " degr."
		WRITE (2,*) " Antenna height of Tx           : ", H_Tx_ant, " m"
		IF (C_mode .GE. 0) THEN
			WRITE (2,*) " Antenna height of Rx           : ", H_Rx_ant, " m"
		END IF
		WRITE (2,*) " Type of antenna Tx             : ", Type_of_Tx_ant
		WRITE (2,*) " Maximum radiated power         : ", Max_power, " dBW"
		WRITE (2,*) " Transmitting frequency         : ", Tx_frequ, "Hz"
		WRITE (2,*) " Designation of emission Tx     : ", Desig_of_Tx_emis
		WRITE (2,*) " Channel occupation             : ", Chan_occup
		WRITE (2,*) " Cold or warm sea               : ", Sea_temperature
		WRITE (2,*) " Distance over sea              : ", D_sea_input, " km"
		WRITE (2,*) " Radius of the service area Tx  : ", Rad_of_Tx_serv_area, " km"
		WRITE (2,*) " Country code of Tx station     : ", Land_from
		IF (C_mode .GE. 0) THEN
			WRITE (2,*) " Reception frequency            : ", Rx_frequ, "Hz"
			WRITE (2,*) " Designation of emission Rx     : ", Desig_of_Rx_emis
			WRITE (2,*) " Type of antenna horiz. of Rx   : ", Ant_typ_H_Rx
			WRITE (2,*) " Type of antenna verti. of Rx   : ", Ant_typ_V_Rx
			WRITE (2,*) " Azimuth of max. radiation of Rx: ", Azi_Rx_input, " degr."
			WRITE (2,*) " Elevation of max. radia. of Rx : ", Ele_Rx_input, " degr."
			WRITE (2,*) " Type of antenna Rx             : ", Type_of_Rx_ant
			WRITE (2,*) " Gain of Rx-antenna             : ", Rx_ant_gain, " dB"
			WRITE (2,*) " Depolarization loss            : ", Depol_loss, " dB"
			WRITE (2,*) " Country code of Rx station     : ", Land_to
			WRITE (2,*) " Input value of corr. f.delta f.: ", Cor_fact_frequ_diff, " dB"
			WRITE (2,*) " Radius of the service area Rx  : ", Rad_of_Rx_serv_area, " km"
		ELSE
			WRITE (2,'(A35,I4,A3)') " Input value of dist. to border : ", D_to_border, " km"
			WRITE (2,*) " Input value of max. crossb. r. :  ", Max_CBR_D_input, " km"
		END IF
		WRITE (2,*) " Input value of permissible fs. : ", Perm_FS_input, " dBµV/m"
		WRITE (2,*) ""
		WRITE (2,*) "" 
	END IF
!
!	***********************************************************************
!
!	Call HCM_MS_V7 subroutuine:
!
	Call System_Clock(Ta)
	CALL HCM_MS_V7
	Call System_Clock(Te)
!
!	***********************************************************************
!
!	Set output values:
!
	I_str(377:382) = Version		! Version number
!
	J = 383							! INFO Values
	DO I = 1, 20
	  IF (Info(I)) THEN
		  I_str(J:J) = "T"
	    ELSE
		  I_str(J:J) = "F"
	  END IF 
	  J = J + 1
	END DO
!
	I_str(403:417)     = Coo_Tx_new		 ! Calculated co-ordinates of the transmitter
	I_str(418:432)     = Coo_Rx_new		 ! Calculated co-ordinates of the receiver
	I_str(166:169)     = Depol_loss		 ! Depolarization loss
!
	I_Distance         = Distance		! Distance
	I_H_Datab_Tx       = H_Datab_Tx		! Heigth of TX above sea level (terrain database)
	I_H_Datab_Rx       = H_Datab_Rx		! Heigth of RX above sea level (terrain database)
	I_HCM_error        = HCM_error		! Error value
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
	I_Rx_ant_type_corr = Rx_ant_type_corr ! Correction according antenna type (E/I) Rx
	I_Delta_frequency  = Delta_frequency ! Frequency difference (kHz)
	I_Corr_delta_f     = Corr_delta_f	 ! Correction factor according frequency difference
	I_Calculated_FS    = Calculated_FS	 ! Calculated field strength
	I_Perm_FS          = Perm_FS		 ! Permissible field strength
	I_CBR_D            = CBR_D			 ! Maximum range of harmful interference
	I_ERP_ref_Tx       = ERP_ref_Tx		 ! Power of the reference transmitter
	I_Prot_margin      = Prot_margin	 ! Protection margin (EP - EC)
!
!
	IF (DEBUG) THEN
	  WRITE (2,*) ""
	  WRITE (2,*) " Version of the HCM module      : V", Version
	  WRITE (2,*) ""
	  WRITE (2,*) "               Important output :"
	  WRITE (2,*) ""
	  WRITE (2,'(A35,I5)') "  Error value                    : ",HCM_error
	  IF (HCM_error .NE. 0) THEN
		SELECT CASE (HCM_Error)
		  CASE (:999)
			WRITE (2,*) " Error in Point_heigt or Point_type subroutine (database)."
		  CASE (1000)
			WRITE (2,*) " Distance is 0.0 km. Calculations not possible !"
		  CASE (1001:1008)
			WRITE (2,*) " Error in geographical Tx co-ordinates !"
		  CASE (1009)
			WRITE (2,*) " Error in input data of Tx antenna height !"
		  CASE (1010)
			WRITE (2,*) " Error in transmitting frequency value !"
		  CASE (1011)
			WRITE (2,*) " Error in transmitting frequency unit !"
		  CASE (1012)
			WRITE (2,*) " Error in input data of service area Tx!"
		  CASE (1013)
			WRITE (2,*) " Error in input data of height of transmitter !"
		  CASE (1014:1021)
			WRITE (2,*) " Error in geographical Rx co-ordinates !"
		  CASE (1022)
			WRITE (2,*) " Error in input data of Rx antenna height !"
		  CASE (1023)
			WRITE (2,*) " Error in reception frequency value !"
		  CASE (1024)
			WRITE (2,*) " Error in reception frequency unit !"
		  CASE (1025)
			WRITE (2,*) " Mode of calculation is not available !"
		  CASE (1026)
			WRITE (2,*) " Error in input data of permissible field strength !"
		  CASE (1027)
			WRITE (2,*) " Error in input value of maximum crossborder range!"
		  CASE (1028)
			WRITE (2,*) " Distance greater than 1000 km. Calculations not possible !"
		  CASE (1029)
			WRITE (2,*) " Error in input data of service area Rx!"
		  CASE (1030)
			WRITE (2,*) " Error in input data of height of receiver !"
		  CASE (1031)
			WRITE (2,*) " Error in input data of Tx elevation !"
		  CASE (1032)
			WRITE (2,*) " Error in input-data of Tx azimuth !"
		  CASE (1033)
			WRITE (2,*) " Error in input data of Tx antenna type (E or I) !"
		  CASE (1034)
			WRITE (2,*) " Error in input data of maximal radiated power !"
		  CASE (1035)
			WRITE (2,*) " Error in input value of distance over sea !"
		  CASE (1036)
			WRITE (2,*) " xxx.ALL borderline file for Tx is missing!"
		  CASE (1037)
			WRITE (2,*) " xxx.ALL borderline file for Rx is missing!"
		  CASE (1038)
			WRITE (2,*) " Error in input data of antenna type (TR25-08), (Tx or Rx) !"
		  CASE (1039)
			WRITE (2,*) " Error in input data of correction factor according frequency difference !"
		  CASE (1040)
			WRITE (2,*) " Channel spacing outside definition range (Rx) !"   
		  CASE (1041)
			WRITE (2,*) " Channel spacing outside definition range (Tx) !"
		  CASE (1042)
			WRITE (2,*) " Error in input data of elevation (Rx) !"
		  CASE (1043)
			WRITE (2,*) " Error in input data of azimuth (Rx) !"
		  CASE (1044)
			WRITE (2,*) " Error in type of antenna Rx (E/I)!"
		  CASE (1045)
			WRITE (2,*) " Error in input data of gain of Rx antenna !"
		  CASE (1046)
			WRITE (2,*) " Error in input data of depolarization loss !"
		  CASE (1047)
			WRITE (2,*) " Error in input value of borderline distance  (max. 999 km)!"
		  CASE (1048)
			WRITE (2,*) " Selected (border-) line data not available !"
		  CASE (1049)
			WRITE (2,*) " Error in line data !"
		  CASE (1050)
			WRITE (2,*) " No Agreement frequency or C_Mode and important input is missing !"
		  CASE (2000:)
			WRITE (2,*) " Error in looking up FS-figures !"
		END SELECT
	  ELSE	! HCM_Error=0
		WRITE (2,*) " Tx co-ordinates calculated     : ", Coo_Tx_new(1:8)," ",Coo_Tx_new(9:15)
		IF (C_mode .LT. 0) THEN
		  IF (D_to_border .EQ. 0) THEN
			WRITE (2,*) " The maximum field strength at"
			WRITE (2,'(A35, F7.1, A7)') "  the border line                : ", Calculated_FS, " dBuV/m"
		  END IF
		  IF (D_to_border .GT. 0) THEN
            WRITE (2,*) " The maximum field strength at"
            WRITE (2,'(A6, I3, A26, F7.1, A7)') "  the ", D_to_border,  " km line                : ", Calculated_FS, " dBuV/m"
          END IF
          IF (D_to_border .LT. 0) THEN
            WRITE (2,*) " The maximum field strength at"
		    WRITE (2,'(A35, F7.1, A7)') "  the cross border range         : ", Calculated_FS, " dBuV/m"
          END IF
          WRITE (2,*) " The co-ordinates of the"
          WRITE (2,'(A35, A8, A1, A7)') "  (border-) line point are       : ", Coo_Rx_new(1:8), " ", Coo_Rx_new(9:15)
          WRITE (2,*) " The distance to the (border-)"
          WRITE (2, '(A35, F7.2, A4)') "  line point is                  : ", Distance, " km."
          WRITE (2,*) " Direction to the (border-)"   
          WRITE (2, '(A35, F7.2, A6)') "  line point                     : ", Dir_Tx_Rx, " degr."
          WRITE (2, '(A35, F7.1, A8)') "  Permissible field strength     : ", Perm_FS, " dBµV/m."
          WRITE (2, '(A35, F7.1, A4)') "  The protection margin is       : ", Prot_margin, " dB."
		  WRITE (2, '(A35, A3)') "  Land of transmitter            : ", Land_from
		  WRITE (2, '(A35, A3)') "  Land to calculate to           : ", Land_to
		  IF (D_to_border .EQ. 0) THEN
			WRITE (2,*) " Calculation is performed on the borderline."
		  END IF
		  IF (D_to_border .LT. 0) THEN
			WRITE (2,*) " Calculation is performed on the cross border range."
		  END IF
		  IF (D_to_border .GT. 0) THEN
			WRITE (2,'(A33, I4, A9)') "  Calculation is performed on the", D_to_border, " km-line."
		  END IF
		  WRITE (2,'(A35, F7.1, A3)') "  Max. range of harmful interfer.: ", CBR_D, " km"
		  WRITE (2,'(A35, F7.1, A4)') "  E.R.P of reference transmitter : ", ERP_ref_Tx, " dBW"
		ELSE !	CMODE >= 0
		  WRITE (2,*) " Rx co-ordinates calculated     : ", Coo_Rx_new(1:8)," ",Coo_Rx_new(9:15)
		  WRITE (2,'(A35, F7.1, A7)') "  Calculated field strength      : ", Calculated_FS, " dBuV/m"
		  WRITE (2,'(A35, F7.1, A7)') "  Permissible field strength     : ", Perm_FS, " dBuV/m"
		  WRITE (2,'(A19)') "  Protection margin"
		  WRITE (2,'(A35, F7.1, A3)') "  (Perm_FS - Calculated_FS)      : ", Prot_margin, " dB"
		  WRITE (2,'(A35, F7.3, A3)') "  Distance Tx -> Rx              : ", Distance, " km"
		END IF ! CMODE
		WRITE (2,'(A35, F7.1, A3)') "  Distance over sea              : ", D_sea_calculated, " km"
		WRITE (2,*) " "
		WRITE (2,*) " "
		WRITE (2,*) " "
		WRITE (2,*) "                    Information :"
		WRITE (2,*) " "
!
		IF (Info(1)) THEN
			WRITE (2,*) " No height of Tx is given or Tx is mobile !"
			WRITE (2,*) " The height is taken from terrain database !"
			WRITE (2,*) " "
		END IF
!
		IF (Info(2)) THEN
			WRITE (2,*) " Height of transmitter differs from terrain database !"
			WRITE (2,*) " "
		END IF
!
		IF (Info(3)) THEN
			WRITE (2,*) " The transmitter height differs more than 10% "
			WRITE (2,*) " from the terrain database!"
			WRITE (2,*) " Calculations may be (extremely) wrong !"
			WRITE (2,*) " "
		END IF
!
		IF (Info(4)) THEN
			WRITE (2,*) " No HCM TX frequency or non strict C_Mode!"
			WRITE (2,*) " Permissible field strength, max CBR and"
			WRITE (2,*) " ERP of the ref. TX are taken from input!"
			WRITE (2,*) " "   
		END IF
!
		IF (Info(5)) THEN
			WRITE (2,*) " Input value of permissible field strength is used !"
			WRITE (2,*) " "
		END IF
!
		IF (Info(6)) THEN
			WRITE (2,*) " Input value for the maximum crossborder range is used !"
			WRITE (2,*) " "
		END IF
!
		IF (Info(7)) THEN
			WRITE (2,*) " The field stength is set to 999.9, because of service area overlapping."
			WRITE (2,*) " "   
		END IF
!
		IF (Info(8)) THEN
			WRITE (2,*) " No height of Rx is given or Rx is mobile / line!"
			WRITE (2,*) " The height is taken from terrain database !"
			WRITE (2,*) " "
		END IF
!
		IF (Info(9)) THEN
			WRITE (2,*) " Height of receiver differs from terrain database !"
			WRITE (2,*) " "
		END IF
!
		IF (Info(10)) THEN
			WRITE (2,*) " The receiver height differs more than 10% from the terrain database!"
			WRITE (2,*) " Calculations may be (extremely) wrong !"
			WRITE (2,*) " "  
		END IF
!
		IF (Info(11)) THEN
			WRITE (2,*) " Free space field strength is used because distance is less than 1 km !"
			WRITE (2,*) " "
		END IF
!
		IF (Info(12)) THEN
			WRITE (2,*) " Free space field strength is used because first fresnel zone is free !"
			WRITE (2,*) " "
		END IF
!
		IF (Info(13)) THEN
			WRITE (2,*) " Distance over sea is greater than total distance; distance between", &
				  " Tx and Rx is used !"
			WRITE (2,*) " "
		END IF
!
 		IF (Info(14)) THEN
			WRITE (2,*) " Input value of corr. factor accord. frequency difference is used !"
			WRITE (2,*) " "
		END IF
!
		IF (Info(15)) THEN
			WRITE (2,*) " Frequency difference outside definition range !"
			WRITE (2,*) " 82 dB value is used for correction factor !"
			WRITE (2,*) " "
		END IF
!
		IF (Info(16)) THEN
			WRITE (2,*) " Calculated distance over sea is set to 0"
			WRITE (2,*) " because of missing morphological data !"
			WRITE (2,*) " "
		END IF
!
		IF (Info(17)) THEN
			WRITE (2,*) " Channel spacing outside curve range, broadband formula is used!"
		END IF
!
		IF (Info(18)) THEN
			WRITE (2,*) " Correction factors for the band 380 - 400 MHz are used."   
		END IF
!
		WRITE (2,*) " "
		WRITE (2,*) " "
		WRITE (2,*) "              Additional output :"
		WRITE (2,*) " "
		WRITE (2,'(A35,F7.3,A2)') "  Calc. Time                     : ", (Real(Te-Ta)/10000.0), " s"
		WRITE (2,'(A35, I7, A2)') "  Height of Tx (terrain database): ", H_Datab_Tx, " m"
		WRITE (2,'(A35, F7.3, A7)') "  Transmitter clearance angle    : ", Tx_TCA, " degree"
		WRITE (2,'(A35, F7.2, A3)') "  Tx clearance angle corr. factor: ", Tx_TCA_corr, " dB"
		WRITE (2,'(A35, F7.1, A2)') "  Effective antenna height of Tx : ", Heff_Tx, " m"
		WRITE (2,'(A35, F7.1, A2)') "  Effective antenna height of Rx : ", Heff_Rx, " m"
		WRITE (2,'(A35, F7.1, A2)') "  Effective antenna height (CCIR): ", Heff, " m"
		WRITE (2,'(A35, F7.1, A2)') "  Delta height  (CCIR)           : ", Dh, " m"
		WRITE (2,'(A35, F7.1, A3)') "  Correct. factor delta h (CCIR) : ", Dh_corr, " dB"
		WRITE (2,'(A18, I3, A8)')   "   Values from the", Time_percentage, "% curves"
		WRITE (2,'(A35, F7.1, A7)') "  Land field strength            : ", Land_FS, " dBuV/m"
		WRITE (2,'(A35, F7.1, A7)') "  Sea field strength             : ", Sea_FS, " dBuV/m"
		WRITE (2,'(A35, F7.1, A3)') "  Correction factor Tx antenna   : ", Tx_ant_corr, " dB"
		WRITE (2,'(A35, F7.1, A3)') "  Corr. factor antenna type Tx   : ", Tx_ant_type_corr, " dB"
		WRITE (2,'(A35, F7.1, A7)') "  Direction Tx -> Rx             : ", Dir_Tx_Rx, " degree"
		WRITE (2,'(A35, F7.1, A7)') "  Angle vertical Tx to Rx        : ", V_angle_Tx_Rx, " degree"
		WRITE (2,'(A35, F7.1, A7)') "  Diff.angle hori. (Tx->Rx - Azi): ", H_diff_angle_Tx_Rx, " degree"
		WRITE (2,'(A35, F7.1, A7)') "  Diff.angle vert. (Tx->Rx - Ele): ", V_diff_angle_Tx_Rx, " degree"
		IF (C_mode .GE. 0) THEN
		  WRITE (2,'(A35, I7, A2)')   "  Height of Rx (terrain database): ", H_Datab_Rx, " m" 
		  WRITE (2,'(A35, F7.3, A7)') "  Receiver clearance angle       : ", Rx_TCA, " degree"
		  WRITE (2,'(A35, F7.2, A3)') "  Rx clearance angle corr. factor: ", Rx_TCA_corr, " dB"
		  WRITE (2,'(A35, F7.1, A7)') "  Direction Rx -> Tx             : ", Dir_Rx_Tx, " degree"
		  WRITE (2,'(A35, F7.1, A7)') "  Angle vertical Rx to Tx        : ", V_angle_Rx_Tx, " degree"
		  WRITE (2,'(A35, F7.1, A7)') "  Diff.angle hori.(Rx->Tx - AziR): ", H_diff_angle_Rx_Tx, " degree"
		  WRITE (2,'(A35, F7.1, A7)') "  Diff.angle vert.(Rx->Tx - EleR): ", V_diff_angle_Rx_Tx, " degree"
		  WRITE (2,'(A35, F7.1, A3)') "  Correction factor Rx antenna   : ", Rx_ant_corr, " dB"
		  WRITE (2,'(A35, F7.1, A3)') "  Corr. factor antenna type Rx   : ", Rx_ant_type_corr, " dB"
		  WRITE (2,'(A35, F7.1, A4)') "  Delta frequency                : ", Delta_frequency/1000.0, " kHz"
		  WRITE (2,'(A35, F7.1, A4)') "  Cannel spacing of Rx           : ", Channel_sp_Rx/1000.0, " kHz"
		  WRITE (2,'(A35, F7.1, A4)') "  Cannel spacing of Tx           : ", Channel_sp_Tx/1000.0, " kHz"
		  WRITE (2,'(A35, F7.1, A3)') "  Corr. factor according delta f : ", Corr_delta_f, " dB"
		  WRITE (2,'(A35, A7, A3)')   "  Depolarization loss            : ", Depol_loss, " dB"

		END IF
		WRITE (2,*) "  Permissible field str. of table"
		WRITE (2,'(A35, F7.1, A7)') "  (0, if there is an input)      : ", Perm_FS_from_table, " dbuV/m"
		WRITE (2,'(A35, F7.1, A4)') "  Power in direction of Rx       : ", Power_to_Rx, " dBW"
		WRITE (2,'(A35, F7.1, A7)') "  Free space field strength      : ", Free_space_FS, " dbuV/m"
		WRITE (2,*) ""
!	-------------------------------------------------------
!	profilepoints?
		INQUIRE (FILE='HCM_PR',EXIST=doit)
		IF (.NOT. doit) GOTO 10
	  	WRITE (2,*) " "
	    WRITE (2,'(A28, I6)')       "  Number of profile points  ", PN
		WRITE (2,'(A27, F7.3, A3)') "  profile sampling distance", PD, " km"
	    WRITE (2,*) " "
	    WRITE (2,*) " (p2p: normalized) profile heights Tx -> Rx [m]" 
	    WRITE (2,*) " "    
	    DO I = 1, PN, 10
         WRITE (2,'(A1, I6, A2, 10(I5))') " ", I, "  ", &
						T_Prof(I), T_Prof(I+1), T_Prof(I+2), &
						T_Prof(I+3), T_Prof(I+4), T_Prof(I+5), &
						T_Prof(I+6), T_Prof(I+7), T_Prof(I+8), T_Prof(I+9)
	    END DO
!	--------------------------------------------------------
10	  END IF ! HCM_Error
	  CLOSE (UNIT = 2)
	END IF ! DEBUG
!
!	***********************************************************************
!
	RETURN
!
	END SUBROUTINE HCMMS_V7_DLL
!