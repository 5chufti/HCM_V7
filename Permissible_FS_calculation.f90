!
!	Permissible_FS_calculation.f90						P. Benner		17.10.2005
!														G.H.			18.03.2015
!
!	Subroutine to calculate the permissible field strength.
!
!	Input values
!
!	Tx_frequency		Tx frequency [MHz]
!	Rx_frequency		Rx frequency [MHz]
!	Desig_of_Rx_emmis	Designation of Rx emission
!	Desig_of_Tx_emmis	Designation of Tx emission
!	Ant_typ_H_Rx		Type of antenna horizontal
!	Ant_typ_V_Rx		Type of antenna vertical
!	Distance			Distance between Tx and Rx points
!	Dir_Rx_Tx			Direction Rx to Tx
!	Azi_Rx_input		Azimuth of maximum radiation
!	Ele_Rx_input		Elevation angle of main radiation
!	Depol_loss			Depolarization loss [format 99.9 (positiv)]
!	Type_of_Rx_ant		Type of Rx antenna [E or I]
!	Cor_fact_frequ_diff	Input value of correction factor according
!						frequency difference [dB] [format 99.9]
!	Rx_ant_gain			Gain of Rx-antenna
!	C_Mode				Mode of calculation
!    
!
!	Output values
!
!	HCM_Error			Error value
!	Perm_FS				Permissible field strength
!	V_angle_Rx_Tx		Vertical angle Rx to Tx
!	V_diff_angle_Rx_Tx	Difference angle vertical
!	H_diff_angle_Rx_Tx	Difference angle horizontal
!	Rx_Azimuth			Rx azimuth
!	Rx_Elevation		Rx elevation
!	Rx_ant_corr			Correction according antenna
!	Rx_ant_type_corr	Correction according antennatype [E or I]
!	Delta_frequency		Delta frequency
!	Corr_delta_f		Correction factor according delta frequency
!	Channel_sp_Rx		Channel spacing of Rx (0 for wideband)
!	Channel_sp_Tx		Channel spacing of Tx (0 for wideband)
!
!
!
!	HCM_Error values 
!
!	1039	Error in input data of correction factor according frequency difference.
!	1040	Channel spacing outside definition range (Rx)! 
!	1041	Channel spacing outside definition range (Tx)! 
!	1042	Error in Rx elevation
!	1043	Error in Rx azimuth
!	1044	Error in Rx type of antenna ("E" or "I")
!	1045	Error in gain of Rx antenna
!	1046	Error in input data of depolarization loss
!
!
!	Info values
!
!	Info(14)	Input value of correction factor according frequency difference is used
!	Info(15)	Frequency difference outside definition range!
!	Info(17)	Channel spacing outside curve range, broadband formula is used!
!	Info(18)	Correction factors for the band 380 - 400 MHz are used.
!
!
!
!	Called subroutines: TACSNMT, Antenna_correction
!
!	**************************************************************
!
	SUBROUTINE Permissble_FS_calculation ( )
!
	IMPLICIT		NONE
!
!	Include definitions:
	INCLUDE			'HCM_MS_V7_definitions.F90'
!
	INTEGER*4		IOS, I, CSXR, CSXT
!
	LOGICAL			TX_TETRA, RX_TETRA, TX_DIG, RX_DIG
!
	REAL			acorrB1, FACTOR, OMEGA, acorrsinus
	REAL			GANT, DPN, B1, B2, X1, CDFN, CDFW
	CHARACTER*4		DRX, DTX
!
	V_angle_Rx_Tx = 0.0
	Rx_ant_corr  = 0.0
	H_diff_angle_Rx_Tx = 0.0
	V_diff_angle_Rx_Tx = 0.0
	Channel_sp_Tx = 0
	Channel_sp_Rx = 0
	Corr_delta_f = 0.0
	Rx_ant_type_corr = 0.0
!
!	**************************************************************
!
!	Delta frequency in Hz:
	Delta_frequency = DINT(Tx_frequency*1D6 - Rx_frequency*1D6)
	Delta_frequency = IABS(Delta_frequency) 
!
!	Input value for correction factor according delta frequency ?
	IF (Cor_fact_frequ_diff .NE. '    ') THEN
	  Info(14) = .TRUE.
!	  Input value of correction factor according frequency difference is used
	  READ (Cor_fact_frequ_diff, *, IOSTAT=IOS) Corr_delta_f
	  IF (IOS .NE. 0) THEN
		HCM_Error = 1039
!		Error in input data of correction factor according frequency
!		difference.
		RETURN
	  END IF
	  GOTO 300
	END IF
!
	TX_DIG = (INDEX('1279',Desig_of_Tx_emis(6:6)) .GT. 0)
	RX_DIG = (INDEX('1279',Desig_of_Rx_emis(6:6)) .GT. 0)
	TX_TETRA = (Desig_of_Tx_emis(1:7) .EQ. '25K0G7W')
	RX_TETRA = (Desig_of_Rx_emis(1:7) .EQ. '25K0G7W')
!
!	Bandwidth of TX:   
	IF ((TX_TETRA) .AND. (.NOT. RX_TETRA)) THEN
		CSXT = 16000
	ELSE
		DTX = Desig_of_Tx_emis(1:4)
		I = INDEX(DTX,'K')
		IF (I .EQ. 0) THEN
			I = INDEX(DTX,'M')
			IF (I .EQ. 0) THEN
				HCM_Error = 1041
!			Channel spacing outside definition range (Tx)! 
				RETURN
			ELSE 
				FACTOR = 1000000.0
			END IF
		ELSE
			FACTOR = 1000.0
		END IF
!	Replace 'K' or 'M' with '.':
		DTX(I:I) = '.'   
		READ (DTX, *, IOSTAT=IOS) X1
		IF (IOS .NE. 0) THEN
			HCM_Error = 1041
!		Channel spacing outside definition range (Tx)!
			RETURN
		ELSE 
			CSXT = NINT(X1 * FACTOR)
		END IF
	END IF
!
!	bail out for line calculations
	IF ((C_Mode .EQ. 99) .OR. (C_Mode .LT. 0)) THEN
		Delta_frequency = 0
!	WB/NB correction
		IF (TX_DIG .AND. (CSXT .GT. 16000) .AND. (Tx_frequency .LE. 470.0) .AND. & 
			(Perm_FS_input .EQ. '     ')) Perm_FS = Perm_FS + 6*LOG10(Real(CSXT)/25000.0)
		RETURN
	END IF
!
!	shortcut for TETRA <> TETRA	
	IF (TX_TETRA .AND. RX_TETRA) THEN
!	Annex3.4.3
		SELECT CASE (delta_frequency)
			CASE (:24999)
				Corr_delta_f = 0.0
			CASE (25000:50000)
				Corr_delta_f = 45.0
			CASE (50001:)
				Corr_delta_f = 70.0
		END SELECT
		GOTO 300
	END IF
!
!	Bandwidth of Rx:   
	IF ((RX_TETRA) .AND. (.NOT. TX_TETRA)) THEN
		CSXR = 16000
	ELSE
		DRX = Desig_of_Rx_emis(1:4)
		I = INDEX(DRX,'K')
		IF (I .EQ. 0) THEN
			I = INDEX(DRX,'M')
			IF (I .EQ. 0) THEN
				HCM_Error = 1040
!			Channel spacing outside definition range (Rx)! 
				RETURN
			ELSE 
				FACTOR = 1000000.0
			END IF
		ELSE
			FACTOR = 1000.0
		END IF
!	Replace 'K' or 'M' with '.':
		DRX(I:I) = '.'   
		READ (DRX, *, IOSTAT=IOS) X1
		IF (IOS .NE. 0) THEN
			HCM_Error = 1040
!		Channel spacing outside definition range (Rx)!
			RETURN
		ELSE 
			CSXR = NINT(X1 * FACTOR)
		END IF
	END IF
!
!	*****************************************************************
!	*								
!	*	new Annex3 							
!	*								
!	*****************************************************************
!
!	Annex3.2
	IF (CSXT .GT. CSXR) THEN
		B1 = REAL(CSXT)
		B2 = REAL(CSXR)
	ELSE
		B2 = REAL(CSXT)
		B1 = REAL(CSXR)
	END IF
!
	OMEGA = REAL(Delta_frequency) / B1
!
!	Annex3.4.1	TETRA Tx vs. narrowband Rx
	IF (TX_TETRA .AND. (CSXR .LE. 25000)) THEN
!	acorrB1:
		IF (OMEGA .LT. 0.5) THEN
			acorrB1 = 0.0
		ELSEIF (OMEGA .LE. 1.0) THEN
			acorrB1 = 32.0 * OMEGA - 16.0
		ELSEIF (OMEGA .LE. 1.4) THEN
			acorrB1 = 112.0 * OMEGA - 96.0
		ELSE
			acorrB1 = 41.0 * OMEGA
		END IF
!
!	acorrsinus:
		IF (OMEGA .LT. 0.4) THEN
			acorrsinus = 0.0
		ELSEIF (OMEGA .LE. 0.7) THEN
			acorrsinus = 50.0 * OMEGA - 21.0
		ELSEIF(OMEGA .LE. 1.0) THEN
			acorrsinus = 225.0 * OMEGA - 145.0
		ELSE
			acorrsinus = -20.0 * OMEGA + 100.0
		END IF
		Corr_delta_f = MIN(acorrsinus - (acorrsinus - acorrB1) * (B2 / B1), 70.0)
		GOTO 300
	END IF 
!
!	Annex3.4.2	narrowband Tx vs. TETRA Rx
	IF (RX_TETRA .AND. (CSXT .LE. 25000)) THEN
!	acorrB1:
		IF (OMEGA .LT. 0.45) THEN
			acorrB1 = 0.0
		ELSEIF (OMEGA .LE. 0.63) THEN
			acorrB1 = 55.0 * OMEGA - 23.0
		ELSEIF (OMEGA .LE. 0.93) THEN
			acorrB1 = 180.0 * OMEGA - 100.0
		ELSE
			acorrB1 = 12.5 * OMEGA + 57.0
		END IF
!
!	acorrsinus:
		IF (OMEGA .LT. 0.45) THEN
			acorrsinus = 0.0
		ELSEIF (OMEGA .LE. 0.7) THEN
			acorrsinus = 225.0 * OMEGA - 101.0
		ELSE
			acorrsinus = 13.0 * OMEGA + 58.0
		END IF
		Corr_delta_f = MIN(acorrsinus - (acorrsinus - acorrB1) * (B2 / B1), 70.0)
		GOTO 300
	END IF 
!
!	Annex3.3	narrowband w/o TETRA
!	acorrB1:
	IF (OMEGA .LT. 0.5) THEN
		acorrB1 = 0.0
	ELSEIF (OMEGA .LE. 1.0) THEN
		acorrB1 = 47.0 * OMEGA - 24.0
	ELSEIF (OMEGA .LE. 1.3) THEN
		acorrB1 = 80.0 * OMEGA - 55.0
	ELSE
		acorrB1 = 38.0 * OMEGA
	END IF
!
!	acorrsinus:
	IF (OMEGA .LT. 0.5) THEN
		acorrsinus = 0.0
	ELSEIF (OMEGA .LE. 1.3) THEN
		acorrsinus = 88.0 * OMEGA - 44.0
	ELSE
		acorrsinus = 12.0 * OMEGA + 55.0
	END IF
	Corr_delta_f = MIN(acorrsinus - (acorrsinus - acorrB1) * (B2 / B1), 70.0)
	IF ((CSXT .LE. 25000) .AND. (CSXR .LE. 25000)) GOTO 300
	CDFN = Corr_delta_f
!
!	Annex3.5	wideband
!	acorrB1:
	IF (OMEGA .LT. 0.5) THEN
		acorrB1 = 0.0
	ELSEIF (OMEGA .LE. 2.0) THEN
		acorrB1 = 33.3 * OMEGA - 16.7
	ELSE
		acorrB1 = 10.0 * OMEGA + 30.0
	END IF
!
!	acorrsinus:
	IF (OMEGA .LT. 0.5) THEN
		acorrsinus = 0.0
	ELSEIF (OMEGA .LE. 1.25) THEN
		acorrsinus = 66.7 * OMEGA - 33.3
	ELSEIF (OMEGA .LE. 1.75) THEN
		acorrsinus = 20.0 * OMEGA + 25.0
	ELSE
		acorrsinus = 4.8 * OMEGA + 51.6
	END IF
	Corr_delta_f = MIN(acorrsinus - (acorrsinus - acorrB1) * (B2 / B1), 70.0)
	IF ((CSXR .GE. 200000) .OR. (CSXT .GE. 200000)) GOTO 300
	CDFW = Corr_delta_f
!
!	Annex3.6
	Corr_delta_f = CDFN + ((CDFW - CDFN) / 175.0) * (B1/1000.0 - 25.0)
!
! common end for p2p
300	Perm_FS = Perm_FS + Corr_delta_f
!
!	Calculation of antenna correction factors "Rx_ant_corr" and "Rx_ant_type_corr":
!
	V_angle_Rx_Tx = ATAN2D (dfloat(H_Tx + H_AntTx - (H_Rx + H_AntRx)),(1D3 * Distance))
	IF ((Ant_typ_V_Rx .NE. '000ND00') .OR. (Ant_typ_H_Rx .NE. '000ND00')) THEN
		READ (Ele_Rx_input, *, IOSTAT=IOS) Rx_Elevation
		IF ((IOS .NE. 0) .AND. (Ant_typ_V_Rx .NE. '000ND00')) THEN
		  HCM_Error = 1042
!		  Error in Rx elevation
		  RETURN
		END IF
		READ (Azi_Rx_input, *, IOSTAT=IOS) Rx_Azimuth
		IF ((IOS .NE. 0) .AND. (Ant_typ_H_Rx .NE. '000ND00')) THEN
		  HCM_Error = 1043
!		  Error in Rx azimuth
		  RETURN
		END IF
		CALL Antenna_correction (Dir_Rx_Tx,Rx_Azimuth,V_angle_Rx_Tx,Rx_Elevation,H_diff_angle_Rx_Tx, &
				V_diff_angle_Rx_Tx, Ant_typ_H_Rx, Ant_typ_V_Rx, Rx_ant_corr, HCM_Error)
		IF (HCM_Error .NE. 0) RETURN
	END IF
!
	Perm_FS = Perm_FS + Rx_ant_corr
!
	IF (Type_of_Rx_ant .EQ. 'E') THEN
		Rx_ant_type_corr = 0.0
	ELSEIF (Type_of_Rx_ant .EQ. 'I') THEN
		Rx_ant_type_corr = 2.1
	ELSE
		HCM_Error = 1044
!		Error in typ of Rx antenna (E/I)
		RETURN
	END IF
!
	Perm_FS = Perm_FS + Rx_ant_type_corr
!
!	Gain of Rx-antenna:
!
	READ (Rx_ant_gain , *, IOSTAT=IOS) GANT
	IF (IOS .NE. 0) THEN
	  HCM_Error = 1045
!	  Error in gain of Rx antenna
	  RETURN
	END IF
!
	Perm_FS = Perm_FS - GANT
!
!	Depolarisation loss:
!
	READ (Depol_loss, *, IOSTAT=IOS) DPN
	IF (IOS .NE. 0) THEN
	  HCM_Error = 1046
!	  Error in input data of depolarization loss
	  RETURN
	END IF
!	  automatic DPN calculation
	IF (Depol_loss .EQ. '-9.9') THEN
		X1 = Free_space_FS - Calculated_FS
		IF ((X1 .LT. 50.0) .AND. (H_AntRx .GT. 10) .AND. (H_AntTx .GT. 10) .AND. &
			(Tx_serv_area + Rx_serv_area .EQ. 0.0) .AND. &
			((Rx_ant_corr .LE. 10.0) .OR. (Tx_ant_corr .LE. 10.0)) ) THEN
			DPN = 25.0 - 0.5*(X1)
		ELSE
			DPN = 0.0
		END IF
		WRITE (Depol_loss, '(F4.1)') DPN	
	END IF
!
	Perm_FS = Perm_FS + DPN
!
	RETURN
!
	END SUBROUTINE Permissble_FS_calculation

