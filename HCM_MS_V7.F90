!	
!	HCMMS_V7.F90										P.Benner		23.02.2004
!														G.H.			10.11.2008
!	Version 7					
!
!	Harmonized Calculation Method for mobile services
!
!	All input- and output values are in HCM_MS_V7_definitions.F90
!	values are passed via COMMON blocks
!
!	Called subroutines:
!	Point_height
!	P_to_P_calculation
!	Permissble_FS_calculation
!	Line_calculation
!
!	Subroutines Point_height, Point_type and Line_calculation are hardware
!	and compiler depending!
!				-----  please customize  -----
!
	SUBROUTINE HCM_MS_V7
!
	IMPLICIT NONE
!
	INCLUDE				'HCM_MS_V7_definitions.F90'
!
	DOUBLE PRECISION	CI, LongTx, LatTx, LongRx, LatRx
	INTEGER*4			I, IOS, IMR
	CHARACTER*376		Strings
	LOGICAL				time50
!
	EQUIVALENCE			(Strings,Sea_temperature)
!
!
!	*************************************************************************
!	*																		*
!	*							Main input:									*
!	*																		*
!	*************************************************************************
!
!	Input values 
!
!	Coo_Tx				Geographical coordinates of Tx				(4C) (Tx)
!	Coo_Rx				Geographical coordinates of Rx				(4C) (Rx)
!	H_Tx_input			Height of transmitter above sea level		(4Z) (Tx)
!	H_Rx_input			Height of receiver above sea level			(4Z) (Rx)
!	Ant_typ_H_Tx		Type of antenna horizontal				   (9XH) (Tx)
!	Ant_typ_V_Tx		Type of antenna vertical				   (9XV) (Tx)
!	Azi_Tx_input		Azimuth of maximum radiation				(9A) (Tx)
!	Ele_Tx_input		Elevation angle of main radiation			(9B) (Tx)
!	H_Tx_ant			Antenna height transmitter					(9Y) (Tx)
!	H_Rx_ant			Antenna height receiver						(9Y) (Rx)
!	Type_of_Tx_ant		Type of antenna [E or I]				   (8B2) (Tx)
!	Max_power			Maximum radiated power					   (8B1) (Tx)
!	Tx_frequ			Transmitting frequency						(1A) (Tx)
!	Chan_occup			Channel occupation [0 or 1]				   (10Z) (Tx)
!	Sea_temperature		Cold or warm sea [C or W, default = C]
!	Rad_of_Tx_serv_area	Radius of the service area Tx				(4D) (Tx)
!	Rad_of_Rx_serv_area	Radius of the service area Rx				(4D) (Rx)
!	D_sea_input			Distance over sea [km] (input value)
!						[format 999.9 or blank]
!	Rx_frequ			Reception frequency							(1Y) (Rx)
!	Desig_of_Rx_emmis	Designation of emission						(7A) (Rx)
!	Desig_of_Tx_emmis	Designation of emission						(7A) (Tx)
!	Ant_typ_H_Rx		Type of antenna horizontal				   (9XH) (Rx)
!	Ant_typ_V_Rx		Type of antenna vertical				   (9XV) (Rx)
!	Azi_Rx_input		Azimuth of maximum radiation				(9A) (Rx)
!	Ele_Rx_input		Elevation angle of main radiation			(9B) (Rx)
!	Type_of_Rx_ant		Type of antenna [E or I]				   (8B2) (Rx)
!	Rx_ant_gain			Gain of Rx-antenna							(9G) (Rx)
!	Depol_loss			Depolarization loss [format 99.9 (positiv)]
!	Perm_FS_input		Input value of permissible field strength
!						[format 999.9]
!	Cor_fact_frequ_diff	Input value of correction factor according
!						frequency difference [dB][format 99.9]
!	PD					Point distace for the profile (km)
!
!	C_mode				Mode of calculation                 
!						 9 = UMTS / IMT2000 point to point calculations.
!						 8 = 380 - 400 MHz emergency / security services calcl.
!						 7 = normal Vienna Agreement coverage calcl. (50% time)
!						 6 = DCS - DCS ML (10 %, EP = 42 dBuV/m)
!						 5 = DCS - DCS FB (10 %, EP = 38 dBuV/m)
!						 4 = ERMES - ERMES (10 %, EP = 32 dBuV/m)
!						 3 = GSM - NMT (10 %, EP = 32 dBuV/m)
!						 2 = GSM - TACS (10 %, EP = 32 dBuV/m)
!						 1 = GSM - GSM (10 %, EP = 32 dBuV/m)
!						 0 = normal Vienna Agreement
!						-1 = Coordination line calculation (h2 = 10m)
!						-2 = Coordination line calcul. GSM (h2 = 3m)
!						-3 = Coordination line calcul. ERMES
!							 (h2 = 3m), EP = 12 dBuV/m, 10 %
!						-4 = Coordination line calcul. ERMES
!							 (h2 = 3m), EP = 32 dBuV/m, 10 %
!						-5 = Coordination line calcul. ERMES
!							 (h2 = 3m), EP = 52 dBuV/m, 50 %
!						-6 = Coordination line calcul. DCS
!							 (h2 = 3m), EP = 25 dBuV/m, 10 %
!						-7 = 380 - 400 MHz emergency / security services line calcl.
!						-8 = UMTS / IMT2000 line calcl.
!
!	Land_to				Land to calculate to (Preface to the IFL)
!	Land_from			Land to calculate from (Preface to the IFL)
!
!	D_to_border			Distance to borderline
!						negativ   -> Cross border range
!						0         -> Borderline calculation
!						positiv x -> Calculation on x km -line
!
!	Max_CBR_D_input		Input value of "Maximum cross border range of harmful
!						interference" [km] [format 100]
!
!	Topo_path			Path to the terrain elevation data, e.g. "D:\TOPO"  
!
!	***************************************************************************
!
	Version = '7.16'
!
	HCM_error = 0
!
!	Convert strings to uppercase and remove chr(0)
!
	DO I = 1, 376
	  IMR = ICHAR(STRINGS(I:I))		
      IF ((IMR .GE. 97) .AND. (IMR .LE. 122)) STRINGS(I:I) = CHAR(IMR - 32)
	  IF (IMR .EQ. 0) STRINGS(I:I) = CHAR(32)
	END DO
!
!	Clear all Info's
!
	DO I = 1, 20
	  Info(I) = .FALSE.
	END DO
!
	O_FN=""
	OLD_T=-1
!	Read all input data:    
!	correct countrycodes for filenames
	IF (Land_to(3:3) .EQ. ' ') Land_to(3:3) = '_'
	IF (Land_to(2:2) .EQ. ' ') Land_to(2:2) = '_'
	IF (Land_from(3:3) .EQ. ' ') Land_from(3:3) = '_'
	IF (Land_from(2:2) .EQ. ' ') Land_from(2:2) = '_'
!
!	Get the longitude of transmitter 'LongTX':
	READ (Coo_Tx(1:3), '(F3.0)', IOSTAT = IOS) LongTx
	IF (IOS .NE. 0) THEN
	  HCM_error = 1001
!	  Error in geographical coordinates (Tx longitude, degrees)
	  RETURN
	END IF
	READ (Coo_Tx(5:6), '(F2.0)', IOSTAT = IOS) CI
	IF (IOS .NE. 0) THEN
	  HCM_error = 1002
!	  Error in geographical coordinates (Tx longitude, minutes)
	  RETURN
	END IF
	LongTx = LongTx + CI / 6.0D1
	READ (Coo_Tx(7:8), '(F2.0)', IOSTAT = IOS) CI
	IF (IOS .NE. 0) THEN
	  HCM_error = 1003
!	  Error in geographical coordinates (Tx longitude, seconds)
	  RETURN
	END IF
	LongTx = LongTx + CI / 3.6D3
	IF (Coo_Tx(4:4) .EQ. 'W') LongTx = 0D0 - LongTx
	IF ((Coo_Tx(4:4).NE.'E') .AND. (Coo_Tx(4:4).NE.'W')) THEN
	  HCM_error = 1004
!	  Error in geographical coordinates (Tx longitude, E/W)
	  RETURN
	END IF
!
!	Get the latitude of transmitter 'LatTx':
	READ (Coo_Tx(9:10),  '(F2.0)', IOSTAT = IOS) LatTx
	IF (IOS .NE. 0) THEN
	  HCM_error = 1005
!	  Error in geographical coordinates (Tx latitude, degrees)
	  RETURN
	END IF
	READ (Coo_Tx(12:13), '(F2.0)', IOSTAT = IOS) CI
	IF (IOS .NE. 0) THEN
	  HCM_error = 1006
!	  Error in geographical coordinates (Tx latitude, minutes)
	  RETURN
	END IF
	LatTx = LatTx + CI / 6.0D1
	READ (Coo_Tx(14:15), '(F2.0)', IOSTAT = IOS) CI
	IF (IOS .NE. 0) THEN
	  HCM_error = 1007
!	  Error in geographical coordinates (Tx latitude, seconds)
	  RETURN
	END IF
	LatTx = LatTx + CI / 3.6D3
	IF (Coo_Tx(11:11) .EQ. 'S') LatTx = 0D0 - LatTx
	IF ((Coo_Tx(11:11) .NE. 'N') .AND. (Coo_Tx(11:11) .NE. 'S')) THEN
	  HCM_error = 1008
!	  Error in geographical coordinates (Tx latitude, N/S)
	  RETURN
	END IF
!
!	Get the antenna height of transmitter 'H_AntTx':
	READ (H_Tx_ant ,'(I4)', IOSTAT=IOS) H_AntTx
	IF (IOS .NE. 0) THEN
	  HCM_error = 1009
!	  Error in Tx antenna height
	  RETURN
	END IF
!
!	Get the transmitting frequency 'Tx_frequency':
	READ (Tx_frequ(1:11), '(F11.5)', IOSTAT=IOS) Tx_frequency
	IF (IOS .NE. 0) THEN
	  HCM_error = 1010
!	  Error in transmitting frequency value
	  RETURN
	END IF
	IF (Tx_frequ(12:12) .EQ. 'M') THEN
!		nothing
	ELSEIF (Tx_frequ(12:12) .EQ. 'K') THEN
		Tx_frequency = Tx_frequency / 1.0D3
	ELSEIF (Tx_frequ(12:12) .EQ. 'G') THEN 
		Tx_frequency = Tx_frequency * 1.0D3
	ELSE
	  HCM_error = 1011
!	  Error in transmitting frequency unit
	  RETURN
	END IF
!
!
!	Get the radius of the Tx service area:
	READ (Rad_of_Tx_serv_area, '(F5.0)', IOSTAT=IOS) Tx_serv_area
	IF (IOS .NE. 0) THEN
	  HCM_error = 1012
!	  Error in radius of service area of Tx
	  RETURN
	END IF
!
!	Default receiver antenna height:
	H_AntRx = 10
!
!	Read data for point to point calculations:
	IF (C_mode .GE. 0) THEN
!	  Test co-ordinates:
	  IF (Coo_Tx .EQ. Coo_Rx) THEN
!	    Distance between Tx and Rx = 0. P-P Calculations not possible.
	    HCM_error = 1000   
	    RETURN
	  END IF
!	  Get the longitude of receiver 'LongRx':
	  READ (Coo_Rx(1:3), '(F3.0)', IOSTAT=IOS) LongRx
	  IF (IOS .NE. 0) THEN
		HCM_error = 1014
!		Error in geographical coordinates (Rx longitude, degrees)
		RETURN
	  END IF
	  READ (Coo_Rx(5:6), '(F2.0)', IOSTAT=IOS) CI
	  IF (IOS .NE. 0) THEN
		HCM_error = 1015
!		Error in geographical coordinates (Rx longitude, minutes)
		RETURN
	  END IF
	  LongRx = LongRx + CI / 6.0D1
	  READ (Coo_Rx(7:8), '(F2.0)', IOSTAT=IOS) CI
	  IF (IOS .NE. 0) THEN
		HCM_error = 1016
!		Error in geographical coordinates (Rx longitude, seconds)
		RETURN
	  END IF
	  LongRx = LongRx + CI / 3.6D3
	  IF (Coo_Rx(4:4) .EQ. 'W') LongRx = 0D0 - LongRx
	  IF ((Coo_Rx(4:4).NE.'E') .AND. (Coo_Rx(4:4).NE.'W')) THEN
		HCM_error = 1017
!		Error in geographical coordinates (Rx longitude, E/W)
		RETURN
	  END IF
!
!	  Get the latitude of receiver 'LATB':
	  READ (Coo_Rx(9:10),  '(F2.0)', IOSTAT=IOS) LatRx
	  IF (IOS .NE. 0) THEN
		HCM_error = 1018
!		Error in geographical coordinates (Rx latitude, degrees)
		RETURN
	  END IF
	  READ (Coo_Rx(12:13), '(F2.0)', IOSTAT=IOS) CI
	  IF (IOS .NE. 0) THEN
		HCM_error = 1019
!		Error in geographical coordinates (Rx latitude, minutes)
		RETURN
	  END IF
	  LatRx = LatRx + CI / 6.0D1
	  READ (Coo_Rx(14:15), '(F2.0)', IOSTAT=IOS) CI
	  IF (IOS .NE. 0) THEN
		HCM_error = 1020
!		Error in geographical coordinates (Rx latitude, seconds)
		RETURN
	  END IF
	  LatRx = LatRx + CI / 3.6D3
	  IF (Coo_Rx(11:11) .EQ. 'S') LatRx = 0D0 - LatRx
	  IF ((Coo_Rx(11:11) .NE. 'N') .AND. (Coo_Rx(11:11) .NE. 'S')) THEN
		HCM_error = 1021
!		Error in geographical coordinates (Rx latitude, N/S)
		RETURN
	  END IF
!
!	  Get the antenna height of receiver 'H_AntRx':
	  READ (H_Rx_ant ,'(I4)', IOSTAT=IOS) H_AntRx
	  IF (IOS .NE. 0) THEN
		HCM_error = 1022
!		Error in Rx antenna height
		RETURN
	  END IF
!
!	  Get reception frequency 'Rx_frequency':
	  READ (Rx_frequ(1:11), '(F11.5)', IOSTAT=IOS) Rx_frequency
	  IF (IOS .NE. 0) THEN
		HCM_error = 1023
!		Error in reception frequency value
		RETURN
	  END IF
	  IF (Rx_frequ(12:12) .EQ. 'M') THEN
!		nothing
	  ELSEIF (Rx_frequ(12:12) .EQ. 'K') THEN
		Rx_frequency = Rx_frequency / 1.0D3
	  ELSEIF (Rx_frequ(12:12) .EQ. 'G') THEN 
		Rx_frequency = Rx_frequency * 1.0D3
	  ELSE
		HCM_error = 1024
!		Error in receiving frequency unit
		RETURN
	  END IF
!
!	  Get radius of the Rx service area:
	  IF (C_mode .NE. 99) THEN
		READ (Rad_of_Rx_serv_area, '(F5.0)', IOSTAT=IOS) Rx_serv_area
		IF (IOS .NE. 0) THEN
		  HCM_error = 1029
!		  Error in radius of Rx service area
		  RETURN
		END IF
	  ELSE
		Rx_serv_area = 0.0
	  END IF
	END IF
!	end of point to point data
!
!	get power
	READ (Max_power, '(F6.1)', IOSTAT=IOS) MaxPow
	IF (IOS .NE. 0) THEN
	  HCM_Error = 1034
!	  Error in power.
	  RETURN
	END IF
!
!	Calculate 'Tx_ant_type_corr':
	IF (Type_of_Tx_ant .EQ. 'E') THEN
		Tx_ant_type_corr = 0.0
	ELSEIF (Type_of_Tx_ant .EQ. 'I') THEN
		Tx_ant_type_corr = 2.1
	ELSE
		HCM_Error = 1033
!		Error in typ of Tx antenna (E/I)
		RETURN
	END IF
!
	MaxPow = MaxPow - Tx_ant_type_corr
!
!	Channel occupation:
	IF (Chan_occup .EQ. '1') THEN
		Time_percentage = 1
	  ELSE
		Time_percentage = 10
	END IF      
!
!	Test point distance (if <30, set it to 100 m):
	IF (PD .LT. 3.0D-2) PD = 1.0D-1
!
!	*****************************************************************
!	*																*
!	*				Conditional calculation parameters				*
!	*																*
!	*****************************************************************
!
!	Default values for Perm_FS, CBR_D, ERP_ref_Tx:
	CBR_D      = -999.9
	ERP_ref_Tx = -999.9
	Perm_FS    = -999.9
	Info(4) = .TRUE.
!
!	Determination of permissible interference field strength
!	"Perm_FS", the max. range of harmful interference "CBR_D" and
!	the E.R.P. of reference transmitter "ERP_ref_Tx":
	IF ((Tx_frequency .GE. 29.7) .AND. (Tx_frequency .LE. 47.0)) THEN
	  Perm_FS = 0.0
	  CBR_D = 100.0
	  ERP_ref_Tx = 3.0
	  Info(4) = .FALSE.
	ELSEIF ((Tx_frequency .GE. 68.0) .AND. (Tx_frequency .LE. 74.8)) THEN
	  Perm_FS = 6.0
	  CBR_D = 100.0
	  ERP_ref_Tx = 9.0
	  Info(4) = .FALSE.
	ELSEIF ((Tx_frequency .GE. 75.2) .AND. (Tx_frequency .LE. 87.5)) THEN
	  Perm_FS = 6.0
	  CBR_D = 100.0
	  ERP_ref_Tx = 9.0
	  Info(4) = .FALSE.
	ELSEIF ((Tx_frequency .GE. 146.0) .AND. (Tx_frequency .LE. 149.9)) THEN
	  Perm_FS = 12.0
	  CBR_D = 80.0
	  ERP_ref_Tx =12.0
	  Info(4) = .FALSE.
	ELSEIF ((Tx_frequency .GE. 150.05) .AND. (Tx_frequency .LE. 174.0)) THEN
	  Perm_FS = 12.0
	  CBR_D = 80.0
	  ERP_ref_Tx = 12.0
	  Info(4) = .FALSE.
	ELSEIF ((Tx_frequency .GE. 380.0) .AND. (Tx_frequency .LE. 385.0) .AND. &
	   ((C_mode .EQ. 8) .OR. (C_mode .EQ. -7))) THEN 
	  Perm_FS = 18.0
	  CBR_D = 50.0
	  ERP_ref_Tx = 14.0
	  Info(4) = .FALSE.
	ELSEIF ((Tx_frequency .GE. 390.0) .AND. (Tx_frequency .LE. 395.0) .AND. &
	   ((C_mode .EQ. 8) .OR. (C_mode .EQ. -7))) THEN
	  Perm_FS = 18.0
	  CBR_D = 50.0
	  ERP_ref_Tx = 14.0
	  Info(4) = .FALSE.
	ELSEIF ((Tx_frequency .GE. 406.1) .AND. (Tx_frequency .LE. 430.0)) THEN
	  Perm_FS = 20.0
	  CBR_D = 50.0
	  ERP_ref_Tx = 16.0
	  Info(4) = .FALSE.
	ELSEIF ((Tx_frequency .GE. 440.0) .AND. (Tx_frequency .LE. 470.0)) THEN
	  Perm_FS = 20.0
	  CBR_D = 50.0
	  ERP_ref_Tx = 16.0
	  Info(4) = .FALSE.
	ELSEIF ((Tx_frequency .GE. 862.0) .AND. (Tx_frequency .LE. 960.0)) THEN
	  Perm_FS = 26.0
	  CBR_D = 30.0
	  ERP_ref_Tx = 13.0
	  Info(4) = .FALSE.
	ELSEIF ((Tx_frequency .GE. 1710.0) .AND. (Tx_frequency .LE. 1785.0)) THEN
	  Perm_FS = 38.0
	  CBR_D = 15.0
	  ERP_ref_Tx = 13.0
	  Info(4) = .FALSE.
	ELSEIF ((Tx_frequency .GE. 1805.0) .AND. (Tx_frequency .LE. 1880.0)) THEN
	  Perm_FS = 42.0
	  CBR_D = 15.0
	  ERP_ref_Tx = 13.0
	  Info(4) = .FALSE.
	ELSEIF ((Tx_frequency .GE. 1885.0) .AND. (Tx_frequency .LE. 2025.0)) THEN
	  Perm_FS = 21.0
	  Info(4) = .FALSE.
	ELSEIF ((Tx_frequency .GE. 2110.0) .AND. (Tx_frequency .LE. 2200.0)) THEN
	  Perm_FS = 21.0
	  Info(4) = .FALSE.
	END IF
!
!	set C_mode depending parameters
	SELECT CASE (C_mode)
!	99 = line as P2P
		CASE (99)
!			nothing
!	9 = UMTS / IMT2000 point to point calculations.
		CASE (9)
!			nothing
!	8 = 380 - 400 MHz emergency / security services calcl.
		CASE (8)
!			nothing
!	7 = normal Vienna Agreement coverage calcl. (50% time)
		CASE (7)
			Time_percentage = 50
!	6 = DCS - DCS ML (10 %, EP = 42 dBuV/m)
		CASE (6)
			Perm_FS = 42.0
			Time_percentage = 10
!	5 = DCS - DCS FB (10 %, EP = 38 dBuV/m)
		CASE (5)
			Perm_FS = 38.0
			Time_percentage = 10
!	4 = ERMES - ERMES (10 %, EP = 32 dBuV/m)
		CASE (4)
			Perm_FS = 32.0
			Time_percentage = 10
!	3 = GSM - NMT (10 %, EP = 32 dBuV/m)
		CASE (3)
			Perm_FS = 32.0
			Time_percentage = 10
!	2 = GSM - TACS (10 %, EP = 32 dBuV/m)
		CASE (2)
			Perm_FS = 32.0
			Time_percentage = 10
!	1 = GSM - GSM (10 %, EP = 32 dBuV/m)
		CASE (1)
			Perm_FS = 32.0
			Time_percentage = 10
!	0 = normal Vienna Agreement
		CASE (0)
!			nothing
!	-1 = Coordination line calculation (h2 = 10m)
		CASE (-1)
			H_AntRx = 10
!	-2 = Coordination line calcul. GSM (h2 = 3m)
		CASE (-2)
			Perm_FS = 19.0
			Time_percentage = 10
			H_AntRx = 3
!	-3 = Coordination line calcul. ERMES (h2 = 3m), EP = 12 dBuV/m, 10 %
		CASE (-3)
			Perm_FS = 12.0
			Time_percentage = 10
			H_AntRx = 3
!	-4 = Coordination line calcul. ERMES (h2 = 3m), EP = 32 dBuV/m, 10 %
		CASE (-4)
			Perm_FS = 32.0
			Time_percentage = 10
			H_AntRx = 3
!	-5 = Coordination line calcul. ERMES (h2 = 3m), EP = 52 dBuV/m, 50 %
		CASE (-5)
			Perm_FS = 52.0
			Time_percentage = 50
			H_AntRx = 3
!	-6 = Coordination line calcul. DCS (h2 = 3m), EP = 25 dBuV/m, 10 %
		CASE (-6)
			Perm_FS = 25.0
			Time_percentage = 10
			H_AntRx = 3
!	-7 = 380 - 400 MHz emergency / security services line calcl.
		CASE (-7)
!			nothing
!	-8 = UMTS / IMT2000 line calcl.
		CASE (-8)
			Time_percentage = 10
			H_AntRx = 3
!	C_mode is out of range
		CASE DEFAULT
			HCM_error = 1025
			RETURN
!
	END SELECT
!
	Perm_FS_from_table = Perm_FS
!
!	Look, if there is an input of permissible field strength:
	IF (Perm_FS_input .NE. '     ') THEN
		Info(5) = .TRUE.
!		Input value of permissible field strength is used
		READ (Perm_FS_input, '(F5.1)', IOSTAT=IOS) Perm_FS
		IF (IOS .NE. 0) THEN
			HCM_error = 1026
!			Error in input value of permissible field strength
		END IF
	ELSE
		IF (INFO(4)) THEN
		  HCM_error = 1050
		  RETURN
		END IF
	END IF                        
!
!	Input value MARAIN ?
	IF (Max_CBR_D_input .NE. '   ') THEN
		READ (Max_CBR_D_input, '(I3)', IOSTAT = IOS) IMR
		IF (IOS .NE. 0) THEN  
		  HCM_error = 1027
!		  Error in input value of maximum cross border range
		  RETURN
		END IF      
!		Input value of maximum cross border range is used
		CBR_D = REAL(IMR)
		Info(6) = .TRUE.
	  ELSE
!		No Agreement frequency and CBR is missing
		IF ((INFO(4)) .AND. (C_mode .LT. 0) .AND. (D_to_border .LT. 0)) THEN
		  HCM_error = 1050
		  RETURN
		END IF
	END IF  
!
!	Sea temparatur:
	IF (Sea_temperature .NE. 'W') Sea_temperature = 'C'
!
!	50% time?
	INQUIRE (FILE='HCM_T50',EXIST=time50)
	IF (time50) Time_percentage = 50
!
!	**************************************************************
!
	IF (C_mode .LT. 0) THEN
!		(Border-) line calculation:   
		CALL Line_calculation ( LongTx, LatTx, LongRx, LatRx)
	  ELSE
!		Point to point calculation:
		CALL P_to_P_calculation ( LongTx, LatTx, LongRx, LatRx)
		CALL Permissble_FS_calculation ()
	END IF
!
	Prot_margin = Perm_FS - Calculated_FS
!	
	RETURN
!
!	**************************************************************
!
!	Output values:
!
!	Version				Version number of HCM_MS_V7 subroutine
!	HCM_error			Error value, see list
!	Info				Information (field) (see list)
!	LongTx				Longitude of Tx
!	LatTx				Latitude of Tx
!	H_AntTx				Antenna height of Tx
!	Tx_frequency		Tx frequency in MHz
!	Tx_serv_area		Radius of Tx service area
!	H_Datab_Tx			Height of Tx site from terrain database
!	H_Tx				Height of Tx site used during all calculations
!						(from input value or from terrain database)
!	LongRx				Longitude of Rx
!	LatRx				Latitude of Rx
!	H_AntRx				Antenna height of Rx
!	Rx_frequency		Rx frequency in MHz
!	Rx_serv_area		Radius of Rx service area
!	H_Datab_Rx			Height of Rx site from terrain database
!	H_Rx				Height of Rx site used during all calculations
!						(from input value or from terrain database)
!	Time_percentage		1, 10 or 50 % time percentage of the curves
!	CBR_D				Maximum cross border range
!	ERP_ref_Tx			E.R.P. of reference transmitter
!	T_Prof				Terrain height profile
!	M_Prof				Morphological profile
!	PN					Number of profile points
!	Heff				Total effective antenna height
!	Heff_Tx				Effective antenna height of Tx
!	Heff_Rx				Effective antenna height of Rx
!	Dh					Terrain irregularity
!	Dh_corr				Correction factor according to terrain irregularity
!	Perm_FS				Permissible field strength (for calculations)
!	Perm_FS_from_table	Permissible field strength from table in Annex 1
!	Calculated_FS		Calculated field strength
!	Distance			Distance between transmitting position and reception position 
!	Prot_margin			Protection margin 
!						(permissible field strength - calculated field strenght)
!	Tx_TCA				Transmitter terrain clearance angle
!	Rx_TCA				Receiver terrain clearance angle
!	Tx_TCA_corr			Correctin factor according to Tx terrain clearance angle 
!	Rx_TCA_corr			Correctin factor according to Rx terrain clearance angle 
!	Land_FS				Land field strength
!	Sea_FS				Sea field strength
!	Tx_ant_corr			Correction according to Tx antennatype (horizontal and vertical)
!	Rx_ant_corr			Correction according to Rx antennatype (horizontal and vertical)
!	Tx_ant_type_corr	Correction according to Tx antennatype (E or I)
!	Rx_ant_type_corr	Correction according to Rx antennatype (E or I)
!	Corr_delta_f		Correction factor according to frequency difference
!	Channel_sp_Tx		Channel spacing of Tx (kHz)
!	Channel_sp_Rx		Channel spacing of Rx (kHz)
!	Power_to_Rx			Radiated power in direction of Rx
!	Free_space_FS		Free space field strength
!	D_sea_calculated	Calculated distance over sea
!	Dir_Tx_Rx			Direction Tx to Rx	
!	Dir_Rx_Tx			Direction Rx to Tx
!	V_angle_Tx_Rx		Vertical angle Tx to Rx	
!	V_angle_Rx_Tx		Vertical angle Rx to Tx
!	H_diff_angle_Tx_Rx	Horizontal difference angle Tx to Rx	
!	H_diff_angle_Rx_Tx	Horizontal difference angle Rx to Tx
!	V_diff_angle_Tx_Rx	Vertical difference angle Tx to Rx
!	V_diff_angle_Rx_Tx	Vertical difference angle Rx to Tx
!	Delta_frequency		Frequency difference in kHz
!	Coo_Tx_new			Calculated Tx co-ordinates
!	Coo_Rx_new			Calculated Rx co-ordinates
!
!
!	HCM_error values:
!
!	0		no error
!	36		error opening terrain- or morphological data file (data not available)
!	200		error in longitude (in 'Point_height' or 'Point_type' subroutine) 
!	210		error in latitude (in 'Point_height' or 'Point_type' subroutine)
!	220		error reading record (in 'Point_height' or 'Point_type' subroutine)
!	300		latitude is not in range of 0.0 - 90.0
!			(in 'Point_height' or 'Point_type' subroutine)
!	400		height is missing (-9999) (in 'Point_height' subroutine)
!	1000	Distance between Tx and Rx = 0. Calculations not possible
!	1001	Error in geographical coordinates (Tx longitude, degrees)
!	1002	Error in geographical coordinates (Tx longitude, minutes)
!	1003	Error in geographical coordinates (Tx longitude, seconds)
!	1004	Error in geographical coordinates (Tx longitude, E/W)
!	1005	Error in geographical coordinates (Tx latitude, degrees)
!	1006	Error in geographical coordinates (Tx latitude, minutes)
!	1007	Error in geographical coordinates (Tx latitude, seconds)
!	1008	Error in geographical coordinates (Tx latitude, N/S)
!	1009	Error in Tx antenna height
!	1010	Error in transmitting frequency value
!	1011	Error in transmitting frequency unit
!	1012	Error in radius of service area of Tx
!	1013	Error in input value height of Tx site above sea level
!	1014	Error in geographical coordinates (Rx longitude, degrees)
!	1015	Error in geographical coordinates (Rx longitude, minutes)
!	1016	Error in geographical coordinates (Rx longitude, seconds)
!	1017	Error in geographical coordinates (Rx longitude, E/W)
!	1018	Error in geographical coordinates (Rx latitude, degrees)
!	1019	Error in geographical coordinates (Rx latitude, minutes)
!	1020	Error in geographical coordinates (Rx latitude, seconds)
!	1021	Error in geographical coordinates (Rx latitude, N/S)
!	1022	Error in Rx antenna height
!	1023	Error in reception frequency value
!	1024	Error in reception frequency unit
!	1025	C_mode is out of range
!	1026	Error in input value of permissible field strength
!	1027	Error in input value of maximum cross border range
!	1028	The distance is greater than 1000 km. Calculations not possible
!	1029	Error in radius of Rx service area
!	1030	Error in input value Rx site height above sea level
!	1031	Error in Tx elevation
!	1032	Error in Tx azimuth
!	1033	Error in typ of Tx antenna (E/I)
!	1034	Error in power
!	1035	Error in input value of distance over sea
!	1036	The 'xxx.ALL' borderline file for Tx is missing
!	1037	The 'xxx.ALL' borderline file for Rx is missing
!	1038	Error in type of antenna (TR20-08)
!	1039	Error in input data of correction factor according frequency difference
!	1040	Channel spacing outside definition range (Rx)
!	1041	Channel spacing outside definition range (Tx)
!	1042	Error in Rx elevation
!	1043	Error in Rx azimuth
!	1044	Error in Rx type of antenna ("E" or "I")
!	1045	Error in gain of Rx antenna
!	1046	Error in input data of depolarization loss
!	1047	Distance to borderline is too long
!	1048	Selected line data not available
!	1049	Error in line data
!	1050	No Agreement frequency and CBR input is missing
!
!	2000	wrong Figure_frequency (from Get_figure_FS_value)
!	2001	wrong Time_percentage  (from Get_figure_FS_value)
!	2002	wrong Sea_temperature  (from Get_figure_FS_value)
!	2003	wrong Figure_Heff      (from Get_figure_FS_value)
!	2004	wrong Figure_distance  (from Get_figure_FS_value)
!
!	Info values:
!
!	Info(1)		No height of Tx site is given,
!				height is taken from the terrain database
!	Info(2)		Height of Tx site differs from height of terrain database
!	Info(3)		Height of Tx site differs more than 10%,
!				calculated values may be (extremely) wrong!
!	Info(4)		Frequency out of range of table in Annex 1
!	Info(5)		Input value of permissible field strength is used
!	Info(6)		Input value of maximum cross border range is used
!	Info(7)		Distance between Tx and Rx is less than both service area radius;
!				field strength is set to 999.9
!	Info(8)		No height of Rx site is given, height is from the terrain database
!	Info(9)		Height of Rx site differs from height of terrain data
!	Info(10)	Rx site height differs more than 10%,
!				calculated values may be (extremely) wrong!
!	Info(11)	Free space field strength used because distance < 1km
!	Info(12)	Free space field strength is used, because 1st Fresnel zone is free
!	Info(13)	Distance over sea is greater than total distance.
!				Distance between Tx and Rx is used!
!	Info(14)	Input value of correction factor according frequency difference is used
!	Info(15)	Frequency difference outside definition range; 82 dB is used
!	Info(16)	Calculated distance over sea is set to 0 because of missing
!				morphological data. !
!	Info(17)	Channel spacing outside curve range, broadband formula is used!
!	Info(18)	Correction factors for the band 380 - 400 MHz are used.
!
!
	END SUBROUTINE HCM_MS_V7
!