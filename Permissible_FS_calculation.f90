!
!	Permissible_FS_calculation.f90						P. Benner		25.02.2004
!														G.H.			22.09.2005
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
!	Delta_frequency		Delta frequency in kHz
!	Corr_delta_f		Correction factor according delta frequency
!	Channel_sp_Rx		Channel spacing of Rx
!	Channel_sp_Tx		Channel spacing of Tx
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
!	Info(17)	Tx channel spacing outside definition range, 25 kHz is used!
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
	INTEGER			IOS, I
!
	LOGICAL			TX_TETRA, RX_TETRA, TX_DIG, RX_DIG, TXGSM
!
	REAL			B1, B2, CSXR, CSXT, GANT, DPN
	REAL			CS1(7), CS2(7), CS3(7), CS4(7), CS5(7)
	REAL			CS6(7), CS7(7), CS8(7), CS9(7), CS10(7)
	REAL			CS11(7), CS12(7), CS13(7), CS14(7), CS15(7) 
	REAL			CS16(7), CS17(7), CS18(7), CS19(7), CS20(7)
	REAL			CS21(7), CS22(7), CS23(7), CS24(7), CS25(7)
	REAL			CS26(7), CS27(7), CS28(7), CSXX(7), X1
	CHARACTER*4		DRX, DTX
!
!
!	**************************************************************
!
!							Tables:
!
!	**************************************************************
!
!	Channel spacing Rx = 12.5, Tx = 5.0 (from 0.0 to 25.0 kHz, steps
!	are 0.0, 5.0, 6.25, 10.0, 12.5, 20.0 and 25.0 kHz)
	DATA CS1  /0.0, 2.7, 7.1, 41.3, 57.8, 73.0, 76.6/
!
!	Channel spacing Rx = 12.5, Tx = 6.25 (from 0.0 to 25.0 kHz, steps
!	are 0.0, 5.0, 6.25, 10.0, 12.5, 20.0 and 25.0 kHz)
	DATA CS2  /0.0, 2.7, 6.5, 37.7, 55.6, 72.7, 76.7/
!
!	Channel spacing Rx = 12.5, Tx = 10.0 (from 0.0 to 25.0 kHz, steps
!	are 0.0, 5.0, 6.25, 10.0, 12.5, 20.0 and 25.0 kHz)
	DATA CS3  /0.0, 2.9, 5.4, 29.2, 48.9, 71.5, 76.8/
!
!	Channel spacing Rx = 12.5, Tx = 12.5 (from 0.0 to 25.0 kHz, steps
!	are 0.0, 5.0, 6.25, 10.0, 12.5, 20.0 and 25.0 kHz)
	DATA CS4  /0.0, 2.6, 4.4, 24.1, 43.2, 69.9, 76.2/
!
!	Channel spacing Rx = 12.5, Tx = 20.0 (from 0.0 to 25.0 kHz, steps
!	are 0.0, 5.0, 6.25, 10.0, 12.5, 20.0 and 25.0 kHz)
	DATA CS5  /0.0, 1.7, 2.7, 13.3, 28.0, 64.7, 72.9/
!
!	Channel spacing Rx = 12.5, Tx = 25.0 (from 0.0 to 25.0 kHz, steps
!	are 0.0, 5.0, 6.25, 10.0, 12.5, 20.0 and 25.0 kHz)
	DATA CS6  /0.0, 1.5, 2.3, 9.4, 20.6, 60.7, 70.2/
!
!	Channel spacing Rx = 20.0, Tx = 5.0 (from 0.0 to 25.0 kHz, steps
!	are 0.0, 5.0, 6.25, 10.0, 12.5, 20.0 and 25.0 kHz)
	DATA CS7  /0.0, 1.5, 7.2, 14.0, 33.0, 74.6, 81.3/
!
!	Channel spacing Rx = 20.0, Tx = 6.25 (from 0.0 to 25.0 kHz, steps
!	are 0.0, 5.0, 6.25, 10.0, 12.5, 20.0 and 25.0 kHz)
	DATA CS8  /0.0, 1.5, 7.0, 13.1, 30.5, 73.1, 80.5/
!
!	Channel spacing Rx = 20.0, Tx = 10.0 (from 0.0 to 25.0 kHz, steps
!	are 0.0, 5.0, 6.25, 10.0, 12.5, 20.0 and 25.0 kHz)
	DATA CS9  /0.0, 1.5, 6.0, 10.9, 24.9, 68.9, 78.2/
!
!	Channel spacing Rx = 20.0, Tx = 12.5 (from 0.0 to 25.0 kHz, steps
!	are 0.0, 5.0, 6.25, 10.0, 12.5, 20.0 and 25.0 kHz)
	DATA CS10 /0.0, 1.5, 5.1, 10.0, 22.0, 66.4, 76.7/
!
!	Channel spacing Rx = 20.0, Tx = 20.0 (from 0.0 to 25.0 kHz, steps
!	are 0.0, 5.0, 6.25, 10.0, 12.5, 20.0 and 25.0 kHz)
	DATA CS11 /0.0, 1.7, 3.8, 8.0, 15.4, 59.1, 72.1/
!
!	Channel spacing Rx = 20.0, Tx = 25.0 (from 0.0 to 25.0 kHz, steps
!	are 0.0, 5.0, 6.25, 10.0, 12.5, 20.0 and 25.0 kHz)
	DATA CS12 /0.0, 1.9, 3.4, 7.2, 12.3, 53.5, 69.1/
!
!	Channel spacing Rx = 25.0, Tx = 5.0 (from 0.0 to 25.0 kHz, steps
!	are 0.0, 5.0, 6.25, 10.0, 12.5, 20.0 and 25.0 kHz)
	DATA CS13 /0.0, 0.7, 1.2, 14.5, 40.6, 72.0, 79.4/
!
!	Channel spacing Rx = 25.0, Tx = 6.25 (from 0.0 to 25.0 kHz, steps
!	are 0.0, 5.0, 6.25, 10.0, 12.5, 20.0 and 25.0 kHz)
	DATA CS14 /0.0, 0.6, 1.0, 12.0, 36.9, 70.3, 78.3/
!
!	Channel spacing Rx = 25.0, Tx = 10.0 (from 0.0 to 25.0 kHz, steps
!	are 0.0, 5.0, 6.25, 10.0, 12.5, 20.0 and 25.0 kHz)
	DATA CS15 /0.0, 0.6, 0.7, 7.3, 27.1, 66.2, 75.5/
!
!	Channel spacing Rx = 25.0, Tx = 12.5 (from 0.0 to 25.0 kHz, steps
!	are 0.0, 5.0, 6.25, 10.0, 12.5, 20.0 and 25.0 kHz)
	DATA CS16 /0.0, 0.4, 0.5, 5.4, 22.4, 63.2, 73.9/
!
!	Channel spacing Rx = 25.0, Tx = 20.0 (from 0.0 to 25.0 kHz, steps
!	are 0.0, 5.0, 6.25, 10.0, 12.5, 20.0 and 25.0 kHz)
	DATA CS17 /0.0, 0.0, 0.0, 2.6, 11.7, 56.1, 69.1/
!
!	Channel spacing Rx = 25.0, Tx = 25.0 (from 0.0 to 25.0 kHz, steps
!	are 0.0, 5.0, 6.25, 10.0, 12.5, 20.0 and 25.0 kHz)
	DATA CS18 /0.0, -0.3, -0.2, 1.9, 7.7, 50.0, 65.9/
!
!	Channel spacing Rx = 10.0, Tx = TETRA (from 0.0 to 25.0 kHz, steps
!	are 0.0, 5.0, 6.25, 10.0, 12.5, 20.0 and 25.0 kHz)
	DATA CS19 /0.0, 0.3, 0.6, 4.0, 8.0, 58.5, 62.3/
!
!	Channel spacing Rx = 12.5, Tx = TETRA (from 0.0 to 25.0 kHz, steps
!	are 0.0, 5.0, 6.25, 10.0, 12.5, 20.0 and 25.0 kHz)
	DATA CS20 /0.0, 0.6, 1.0, 3.4, 6.6, 53.7, 61.8/
!
!	Channel spacing Rx = 20.0, Tx = TETRA (from 0.0 to 25.0 kHz, steps
!	are 0.0, 5.0, 6.25, 10.0, 12.5, 20.0 and 25.0 kHz)
	DATA CS21 /0.0, 0.9, 1.4, 3.1, 4.7, 25.2, 59.7/
!
!	Channel spacing Rx = 25.0, Tx = TETRA (from 0.0 to 25.0 kHz, steps
!	are 0.0, 5.0, 6.25, 10.0, 12.5, 20.0 and 25.0 kHz)
	DATA CS22 /0.0, 1.4, 2.0, 3.0, 4.3, 17.6, 43.4/
!
!	Channel spacing Rx = TETRA, Tx = 5.0 (from 0.0 to 25.0 kHz, steps
!	are 0.0, 5.0, 6.25, 10.0, 12.5, 20.0 and 25.0 kHz)
	DATA CS23 /0.0, 0.1, 0.0, 8.5, 34.8, 69.9, 73.0/
!
!	Channel spacing Rx = TETRA, Tx = 6.25 (from 0.0 to 25.0 kHz, steps
!	are 0.0, 5.0, 6.25, 10.0, 12.5, 20.0 and 25.0 kHz)
	DATA CS24 /0.0, 0.1, 0.2, 8.4, 32.0, 69.9, 73.0/
!	
!	Channel spacing Rx = TETRA, Tx = 10.0 (from 0.0 to 25.0 kHz, steps
!	are 0.0, 5.0, 6.25, 10.0, 12.5, 20.0 and 25.0 kHz)
	DATA CS25 /0.0, 0.1, 0.2, 7.4, 25.8, 69.8, 72.9/
!
!	Channel spacing Rx = TETRA, Tx = 12.5 (from 0.0 to 25.0 kHz, steps
!	are 0.0, 5.0, 6.25, 10.0, 12.5, 20.0 and 25.0 kHz)
	DATA CS26 /0.0, 0.1, 0.2, 6.3, 22.6, 70.0, 73.0/
!
!	Channel spacing Rx = TETRA, Tx = 20.0 (from 0.0 to 25.0 kHz, steps
!	are 0.0, 5.0, 6.25, 10.0, 12.5, 20.0 and 25.0 kHz)
	DATA CS27 /0.0, 0.4, 0.8, 4.7, 15.6, 69.5, 72.9/
!
!	Channel spacing Rx = TETRA, Tx = 25.0 (from 0.0 to 25.0 kHz, steps
!	are 0.0, 5.0, 6.25, 10.0, 12.5, 20.0 and 25.0 kHz)
	DATA CS28 /0.0, 0.1, 0.7, 4.2, 12.1, 69.2, 72.4/
!
!	**************************************************************
!
!						End of tables
!
!	**************************************************************
!
!
!	Delta frequency in kHz:
!	Delta_frequency = DABS(Tx_frequency  - Rx_frequency) * 1D3
	Delta_frequency = DABS((DINT(Tx_frequency*1D6) - DINT(Rx_frequency*1D6))/1D3)
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
	IF (Delta_frequency .EQ. 0.0D0) THEN
	  Corr_delta_f = 0.0
	  GOTO 300
	END IF
!
!	Module UMTS / IMT2000
	IF (C_Mode .EQ. 9) THEN
	  IF (Delta_frequency .LT. 5000.0) Corr_delta_f = 0.0
	  IF ((Delta_frequency .GE. 5000.0) .AND. (Delta_frequency .LT. 10000.0)) &
				Corr_delta_f = 24.0
	  IF (Delta_frequency .GE. 10000.0) Corr_delta_f = 82.0
	  GOTO 300
	END IF
!
!	Bandwidth of Rx:   
	I = INDEX(Desig_of_Rx_emis(1:4),'K')
	IF (I .LT. 2) THEN
	  HCM_Error = 1040
!	  Channel spacing outside definition range (Rx)! 
	  RETURN
	END IF
	DRX = Desig_of_Rx_emis(1:4)
!
!	Replace 'K' with '.':
	DRX(I:I) = '.'   
	READ (DRX, *, IOSTAT=IOS) CSXR
	IF (IOS .NE. 0) THEN
	  HCM_Error = 1040
!	  Channel spacing outside definition range (Rx)!
	  RETURN
	END IF
!
!	Bandwidth of TX:   
	I = INDEX(Desig_of_Tx_emis(1:4),'K')
	IF (I .LT. 2) THEN
	  HCM_Error = 1041
!	  Channel spacing outside definition range (Tx)! 
	  RETURN
	END IF
	DTX = Desig_of_Tx_emis(1:4)
!      
!	Replace 'K' with '.':
	DTX(I:I) = '.'   
	READ (DTX, *, IOSTAT= IOS) CSXT
	IF (IOS .NE. 0) THEN
	  HCM_Error = 1041
!	  Channel spacing outside definition range (Tx)!
	  RETURN
	END IF
!
	TX_TETRA = .FALSE.
	RX_TETRA = .FALSE.
	TX_DIG = .FALSE.
	RX_DIG = .FALSE.
	IF (Desig_of_Tx_emis(1:7) .EQ. '25K0G7W') TX_TETRA = .TRUE. 
	IF (Desig_of_Rx_emis(1:7) .EQ. '25K0G7W') RX_TETRA = .TRUE.
	IF (Desig_of_Tx_emis(6:6) .EQ. '1') TX_DIG = .TRUE. 
	IF (Desig_of_Rx_emis(6:6) .EQ. '1') RX_DIG = .TRUE.
	IF (Desig_of_Tx_emis(6:6) .EQ. '2') TX_DIG = .TRUE. 
	IF (Desig_of_Rx_emis(6:6) .EQ. '2') RX_DIG = .TRUE.
	IF (Desig_of_Tx_emis(6:6) .EQ. '7') TX_DIG = .TRUE. 
	IF (Desig_of_Rx_emis(6:6) .EQ. '7') RX_DIG = .TRUE.
	IF (Desig_of_Tx_emis(6:6) .EQ. '9') TX_DIG = .TRUE. 
	IF (Desig_of_Rx_emis(6:6) .EQ. '9') RX_DIG = .TRUE.
!
!	Module normal Berlin:
	IF ((C_Mode .EQ. 0) .OR. (C_Mode .EQ. 4) .OR. (C_Mode .EQ. 7)) GOTO 100
!
!	Module GSM 900:
	IF ((C_Mode .EQ. 1) .OR. (C_Mode .EQ. 2) .OR. (C_Mode .EQ. 3)) GOTO 20
!
!	Module GSM 1800:
	IF ((C_Mode .EQ. 5) .OR. (C_Mode .EQ. 6)) GOTO 60
!
!	Module 380-400:
	IF (C_Mode .EQ. 8) THEN
	  IF ((TX_DIG) .AND. (RX_DIG)) THEN
		  GOTO 70
		ELSE
		  GOTO 100
	  END IF
	END IF
!
	RETURN
!
!	*****************************************************************
!	*								
!	*						Module GSM 900	
!	*								
!	*****************************************************************
!     
!	Tx - bandwidth > 100 kHz ?
!
20	IF (CSXT .GT. 100.0) THEN
!		Tx = GSM  
		TXGSM = .TRUE.
	  ELSE        
!		Rx = GSM
		TXGSM = .FALSE.
	END IF
!
!	Determine Corr_delta_f:
!
	IF ((C_Mode .EQ. 1) .AND. (.NOT. TXGSM)) THEN
	  HCM_Error = 1041
!	  Channel spacing outside definition range (Tx)!
	  RETURN
	END IF
!
	IF (C_Mode .EQ. 1) THEN 
!	  Determine Corr_delta_f
	  IF (NINT(Delta_frequency) .EQ.   0) THEN
		Corr_delta_f = 0.0
		GOTO 300
	  END IF
	  IF (NINT(Delta_frequency) .EQ. 200) THEN
		Corr_delta_f =  18.0
		GOTO 300
	  END IF
	  IF (NINT(Delta_frequency) .EQ. 400) THEN
		Corr_delta_f = 50.0
		GOTO 300
	  END IF
	  Info(15) = .TRUE.
!	  Frequency difference outside definition range!
	  Corr_delta_f = 82
	  GOTO 300
	END IF
!
	IF (Delta_frequency .GT. 400.0) THEN
	  Info(15) = .TRUE.
!	  Frequency difference outside definition range!
	  Corr_delta_f = 82
	  GOTO 300
	END IF
!
	IF (C_Mode .EQ. 2) THEN
!	  GSM <-> TACS
	  IF (TXGSM) THEN
!		  GSM interference to TACS
		  IF (Delta_frequency .LE. 100.0) Corr_delta_f = -11.0
		  IF ((Delta_frequency .GT. 100.0) .AND. (Delta_frequency .LE. 200.0)) THEN
			Corr_delta_f = -11.0 + (Delta_frequency - 100.0) * 0.3
		  END IF
		  IF ((Delta_frequency .GT. 200.0) .AND. (Delta_frequency .LE. 250.0)) THEN
			Corr_delta_f = 19.0 + (Delta_frequency - 200.0) * 3.0 / 50.0
		  END IF
		  IF (Delta_frequency .GT. 250.0) THEN
			Corr_delta_f = 22.0 + (Delta_frequency - 250.0) * 27.0 / 150.0
		  END IF
		  GOTO 300
		ELSE
!		  TACS interference to GSM
		  CALL TACSNMT (Delta_frequency, Corr_delta_f)
		  IF (Delta_frequency .GT. 275.0) Corr_delta_f = 51.0
		  GOTO 300
	  END IF
	END IF              
!
	IF (C_Mode .EQ. 3) THEN
!	  GSM <-> NMT
	  IF (TXGSM) THEN
!		  GSM interference to NMT
		  IF (Delta_frequency .LE. 100.0) Corr_delta_f = -10.0
		  IF ((Delta_frequency .GT. 100.0) .AND. (Delta_frequency .LE. 200.0)) THEN
			Corr_delta_f = -10.0 + (Delta_frequency - 100.0) * 0.3
		  END IF
		  IF ((Delta_frequency .GT. 200.0) .AND. (Delta_frequency .LE. 250.0)) THEN
			Corr_delta_f = 20.0 + (Delta_frequency - 200.0) * 3.0 / 50.0
		  END IF
		  IF (Delta_frequency .GT. 250.0) THEN
			Corr_delta_f = 23.0 + (Delta_frequency - 250.0) * 27.0 / 150.0
		  END IF
		  GOTO 300
		ELSE
!		  NMT interference to GSM
		  CALL TACSNMT (Delta_frequency, Corr_delta_f)
		  GOTO 300
	  END IF
	END IF              
!
	GOTO 300          
!
!
!
!	*****************************************************************
!	*								
!	*						Module GSM 1800	
!	*								
!	*****************************************************************
!
60    IF (Delta_frequency .GT. 600.0) THEN
		Info(15) = .TRUE.
!		Frequency difference outside definition range!
		Corr_delta_f = 82
		GOTO 300
      END IF       
!
	IF (Delta_frequency .LE. 100.0) THEN
	  Corr_delta_f = 0.0 
	  GOTO 300
	END IF
!
	IF ((Delta_frequency .GT. 100.0) .AND. (Delta_frequency .LE. 200.0)) THEN
	  Corr_delta_f = (Delta_frequency -100.0) * 0.18
	  GOTO 300
	END IF
!
	IF ((Delta_frequency .GT. 200.0) .AND. (Delta_frequency .LE. 400.0)) THEN
	  Corr_delta_f = 18.0 + (Delta_frequency -200.0) * 0.16
	  GOTO 300
	END IF
!
!	Delta_frequency > 400 kHz:
	Corr_delta_f = 50.0 + (Delta_frequency -400.0) * 0.04
!
	GOTO 300          
!	
!	
!
!	*****************************************************************
!	*								
!	*			Module 380 - 400 MHz			
!	*	(or Tx and Rx are digital systems)			
!	*								
!	*****************************************************************
!
!	If at least one station is not digital modulation, use normal
!	Vienna calculation.
!
70	IF (CSXT .GT. CSXR) THEN
		B1 = CSXT
		B2 = CSXR
	  ELSE
		B2 = CSXT
		B1 = CSXR
	END IF
!
	IF (Delta_frequency .LT. ((B1 + B2) / 2.0)) Corr_delta_f = 0.0
	IF ((Delta_frequency .LE. ((B1 + 2.0 * B2) / 2.0)) .AND. & 
		(Delta_frequency .GE. ((B1 + B2) / 2.0))) Corr_delta_f = 45.0
	IF (Delta_frequency .GT. ((B1 + 2.0 * B2) / 2.0)) THEN
	  Info(15) = .TRUE.
!	  Frequency difference outside definition range!
	  Corr_delta_f = 82
	  GOTO 300
	END IF  
!
	GOTO 300
!
!	*****************************************************************
!	*								
!	*					Module normal Agreement	
!	*								
!	*	Determination of correction factor according to delta f	
!	*								
!	*****************************************************************
!
!
100	IF (CSXR .LE. 11.0) Channel_sp_Rx = 12.5
!
	IF ((TX_TETRA) .AND. (.NOT. RX_TETRA)) THEN
	  IF (CSXR .LE. 8.8) Channel_sp_Rx = 10.0
	END IF
!
	IF ((CSXR .GT. 11.0) .AND. (CSXR .LE. 14.0)) Channel_sp_Rx = 20.0
	IF (CSXR .GT. 14.0) Channel_sp_Rx = 25.0
!	IF ((CSXR .GT. 14.0) .AND. (CSXR .LE. 16.0)) Channel_sp_Rx = 25.0
!	IF (CSXR .GT. 16.0) THEN
!	  HCM_Error = 1040
!!	  Channel spacing of Rx is outside definition range
!	  RETURN
!	END IF
!
	IF (Channel_sp_Tx .LE. 4.4) Channel_sp_Tx = 5.0
	IF ((CSXT .GT. 4.4) .AND. (CSXT .LE. 5.5)) Channel_sp_Tx = 6.25
	IF ((CSXT .GT. 5.5) .AND. (CSXT .LE. 8.8)) Channel_sp_Tx = 10.0
	IF ((CSXT .GT. 8.8) .AND. (CSXT .LE. 11.0)) Channel_sp_Tx = 12.5
	IF ((CSXT .GT. 11.0) .AND. (CSXT .LE. 14.0)) Channel_sp_Tx = 20.0
	IF ((CSXT .GT. 14.0) .AND. (CSXT .LE. 16.0)) Channel_sp_Tx = 25.0
	IF (CSXT .GT. 16.0) THEN
!	  HCM_Error = 1041
!	  Channel spacing of Tx is outside definition range
!	  RETURN
	  Info(17) = .TRUE.
!	  Tx channel spacing outside definition range, 25 kHz is used!
	  Channel_sp_Tx = 25.0
	END IF
!
	IF (Channel_sp_Rx .EQ. 10.0) THEN
	  DO I = 1, 7
		CSXX(I) = CS19(I)
	  END DO
	END IF
	IF (Channel_sp_Rx .EQ. 12.5) THEN
	  IF (Channel_sp_Tx .EQ. 5.0) THEN
		DO I = 1, 7
		  CSXX(I) = CS1(I)
		END DO
	  END IF
	  IF (Channel_sp_Tx .EQ. 6.25) THEN
		DO I = 1, 7
		  CSXX(I) = CS2(I)
		END DO
	  END IF
	  IF (Channel_sp_Tx .EQ. 10.0) THEN
		DO I = 1, 7
		  CSXX(I) = CS3(I)
		END DO
	  END IF
	  IF (Channel_sp_Tx .EQ. 12.5) THEN
		DO I = 1, 7
		  CSXX(I) = CS4(I)
		END DO
	  END IF
	  IF (Channel_sp_Tx .EQ. 20.0) THEN
		DO I = 1, 7
		  CSXX(I) = CS5(I)
		END DO
	   END IF
	  IF (Channel_sp_Tx .EQ. 25.0) THEN
		DO I = 1, 7
		  IF (.NOT. TX_TETRA) CSXX(I) = CS6(I)
		  IF (TX_TETRA) CSXX(I) = CS20(I)
		END DO
	  END IF
	END IF
!
	IF (Channel_sp_Rx .EQ. 20.0) THEN
	  IF (Channel_sp_Tx .EQ. 5.0) THEN
		DO I = 1, 7
		  CSXX(I) = CS7(I)
		END DO
	  END IF
	  IF (Channel_sp_Tx .EQ. 6.25) THEN
		DO I = 1, 7
		  CSXX(I) = CS8(I)
		END DO
	  END IF
	  IF (Channel_sp_Tx .EQ. 10.0) THEN
		DO I = 1, 7
		  CSXX(I) = CS9(I)
		END DO
	  END IF
	  IF (Channel_sp_Tx .EQ. 12.5) THEN
		DO I = 1, 7
		  CSXX(I) = CS10(I)
		END DO
	  END IF
	  IF (Channel_sp_Tx .EQ. 20.0) THEN
		DO I = 1, 7
		  CSXX(I) = CS11(I)
		END DO
	  END IF
	  IF (Channel_sp_Tx .EQ. 25.0) THEN
		DO I = 1, 7
		  IF (.NOT. TX_TETRA) CSXX(I) = CS12(I)
		  IF (TX_TETRA) CSXX(I) = CS21(I)
		END DO
	  END IF
	END IF
!
	IF (Channel_sp_Rx .EQ. 25.0) THEN
	  IF (Channel_sp_Tx .EQ. 5.0) THEN
		DO I = 1, 7
		  IF (.NOT. RX_TETRA) CSXX(I) = CS13(I)
		  IF (RX_TETRA) CSXX(I) = CS23(I)
		END DO
	  END IF
	  IF (Channel_sp_Tx .EQ. 6.25) THEN
		DO I = 1, 7
		  IF (.NOT. RX_TETRA) CSXX(I) = CS14(I)
		  IF (RX_TETRA) CSXX(I) = CS24(I)
		END DO
	  END IF
	  IF (Channel_sp_Tx .EQ. 10.0) THEN
		DO I = 1, 7
		  IF (.NOT. RX_TETRA) CSXX(I) = CS15(I)
		  IF (RX_TETRA) CSXX(I) = CS25(I)
		END DO
	  END IF
	  IF (Channel_sp_Tx .EQ. 12.5) THEN
		DO I = 1, 7
		  IF (.NOT. RX_TETRA) CSXX(I) = CS16(I)
		  IF (RX_TETRA) CSXX(I) = CS26(I)
		END DO
	  END IF
	  IF (Channel_sp_Tx .EQ. 20.0) THEN
		DO I = 1, 7
		  IF (.NOT. RX_TETRA) CSXX(I) = CS17(I)
		  IF (RX_TETRA) CSXX(I) = CS27(I)
		END DO
	  END IF
	  IF (Channel_sp_Tx .EQ. 25.0) THEN
		IF ((TX_TETRA) .AND. (RX_TETRA)) THEN
		  Info(18) = .TRUE.
!		  Correction factors for the band 380 - 400 MHz are used.
		  GOTO 70
		END IF
		DO I = 1, 7
		  IF ((.NOT. TX_TETRA) .AND. (.NOT. RX_TETRA)) CSXX(I) = CS18(I)
		  IF ((TX_TETRA) .AND. (.NOT. RX_TETRA)) THEN
			CSXX(I) = CS22(I)
		  END IF
		  IF ((.NOT. TX_TETRA) .AND. (RX_TETRA)) CSXX(I) = CS28(I)
		END DO
	  END IF
	END IF
!
!	Curve selected
!
!	Calculate Corr_delta_f:
!
	IF (Delta_frequency .EQ. 0.0) THEN
		Corr_delta_f = CSXX(1)
	  ELSE
		IF (Delta_frequency .GT. 25.0) THEN
			Info(15) = .TRUE.
!			Frequency difference outside definition range!
			Corr_delta_f = 82
		  ELSE
			IF (Delta_frequency .EQ. 5.0)  Corr_delta_f = CSXX(2)
			IF (Delta_frequency .EQ. 6.25) Corr_delta_f = CSXX(3)
			IF (Delta_frequency .EQ. 10.0) Corr_delta_f = CSXX(4)
			IF (Delta_frequency .EQ. 12.5) Corr_delta_f = CSXX(5)
			IF (Delta_frequency .EQ. 20.0) Corr_delta_f = CSXX(6)
			IF (Delta_frequency .EQ. 25.0) Corr_delta_f = CSXX(7)
			IF ((Delta_frequency .LT. 5.0) .AND. (Delta_frequency .GT. 0.0)) THEN
			  Corr_delta_f = CSXX(2) * Delta_frequency / 5.0
			END IF
			IF ((Delta_frequency .LT. 6.25) .AND. (Delta_frequency .GT. 5.0)) THEN
			  X1  = (Delta_frequency - 5.0) / (6.25 - 5.0)
			  Corr_delta_f = CSXX(2) + X1 * (CSXX(3)-CSXX(2))
			END IF
			IF ((Delta_frequency .LT. 10.0) .AND. (Delta_frequency .GT. 6.25)) THEN
			  X1  = (Delta_frequency - 6.25) / (10.0 - 6.25)
			  Corr_delta_f = CSXX(3) + X1 * (CSXX(4)-CSXX(3))
			END IF
			IF ((Delta_frequency .LT. 12.5) .AND. (Delta_frequency .GT. 10.0)) THEN
			  X1  = (Delta_frequency - 10.0) / (12.5 - 10.0)
			  Corr_delta_f = CSXX(4) + X1 * (CSXX(5)-CSXX(4))
			END IF
			IF ((Delta_frequency .LT. 20.0) .AND. (Delta_frequency .GT. 12.5)) THEN
			  X1  = (Delta_frequency - 12.5) / (20.0 - 12.5)
			  Corr_delta_f = CSXX(5) + X1 * (CSXX(6)-CSXX(5))
			END IF
			IF ((Delta_frequency .LT. 25.0) .AND. (Delta_frequency .GT. 20.0)) THEN
			  X1  = (Delta_frequency - 20.0) / (25.0 - 20.0)
			  Corr_delta_f = CSXX(6) + X1 * (CSXX(7)-CSXX(6))
			END IF
		END IF
	END IF
!
300	Perm_FS = Perm_FS + Corr_delta_f
!
!	Calculation of antenna correction factors "Rx_ant_corr" and "Rx_ant_type_corr":
!
	IF ((C_mode .EQ. 99) .OR. &
		((Ant_typ_V_Rx .EQ. '000ND00') .AND. (Ant_typ_H_Rx .EQ. '000ND00'))) THEN
		Rx_ant_corr  = 0.0
	ELSE
		V_angle_Rx_Tx = ATAND ((H_Tx + H_AntTx - H_Rx + H_AntRx) / (1E3 * Distance))
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
		CALL Ctransf (Dir_Rx_Tx,Rx_Azimuth,V_angle_Rx_Tx,Rx_Elevation,H_diff_angle_Rx_Tx,V_diff_angle_Rx_Tx)
		CALL Antenna_correction (H_diff_angle_Rx_Tx, V_diff_angle_Rx_Tx, Ant_typ_H_Rx, Ant_typ_V_Rx, Rx_ant_corr, HCM_Error)
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
!
	Perm_FS = Perm_FS + DPN
!
	RETURN
!
	END SUBROUTINE Permissble_FS_calculation
!
!
!	****************************************************************
!
	SUBROUTINE TACSNMT (DF, CDF)
!
	DOUBLE PRECISION	DF
	REAL				CDF, DD, FTAB1(7)
	INTEGER				I
!
!	From 30 kHz to 90 kHz in 10 kHz steps:      
	DATA	FTAB1 /-9.0,-8.5,-8.0,-7.0,-6.0,-4.0,-1.0/
!
	IF (DF .LE. 30.0) THEN
	  CDF = -9.0
	  RETURN
	END IF       
!
	IF ((DF .GT. 30.0) .AND. (DF .LT. 90.0)) THEN
	  I = INT((DF-30.0)/10.0) + 1
	  DD = DF - (I-1) * 10.0 - 30.0 
	  CDF = FTAB1(I) - ((FTAB1(I)-FTAB1(I+1)) * DD/10.0)
	  RETURN
	END IF       
!
	IF ((DF .GE. 90.0) .AND. (DF .LT. 150.0)) THEN
	  CDF = -1.0 + (DF-90.0) * 21.0/60.0
	  RETURN
	END IF       
!
	IF ((DF .GE. 150.0) .AND. (DF .LT. 200.0)) THEN
	  CDF = 20.0 + (DF-150.0) * 13.0/50.0
	  RETURN
	END IF       
!
	IF ((DF .GE. 200.0) .AND. (DF .LT. 330.0)) THEN
	  CDF = 33.0 + (DF-200.0) * 28.0/130.0
	  RETURN
	END IF       
!
!	DF >= 330 kHz:
	CDF = 61.0      
	RETURN                       
!      
	END SUBROUTINE TACSNMT
!