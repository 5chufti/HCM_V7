!
!	Antenna_correction.f90
!													05.07.2005
!
!	Subroutine to calculate the corrected H_diff_angle, V_diff_angle
!
	SUBROUTINE Ctransf (azi,aziM,ele,eleM,hda,vda)
!
	IMPLICIT	NONE
!
!	Include the interface definitions:
	INCLUDE				'HCM_MS_V7_definitions.F90'
!
	DOUBLE PRECISION	azi, aziM, ele, eleM
	REAL				hda, vda
!
	IF ((Ant_typ_H_Tx .EQ. '000ND00') .AND. (Ant_typ_V_Tx .EQ. '000ND00')) THEN
		Tx_ant_corr   = 0.0
	  ELSE
		IF (Ant_typ_V_Tx .EQ. '000ND00') THEN
			V_diff_angle_Tx_Rx = 0.0
		  ELSE
			V_diff_angle_Tx_Rx = eleM - ele
			IF (V_diff_angle_Tx_Rx .LT. 0.0) V_diff_angle_Tx_Rx = 360.0 + V_diff_angle_Tx_Rx
		END IF
		IF (Ant_typ_H_Tx .EQ. '000ND00') THEN
			H_diff_angle_Tx_Rx = 0.0
		  ELSE
			H_diff_angle_Tx_Rx = azi - aziM
			IF (H_diff_angle_Tx_Rx .LT.   0.0) H_diff_angle_Tx_Rx = 360.0 + &
							H_diff_angle_Tx_Rx
			IF (H_diff_angle_Tx_Rx .GT. 360.0) H_diff_angle_Tx_Rx = &
							H_diff_angle_Tx_Rx - 360.0
		END IF
	end if
		hda = H_diff_angle_Tx_Rx
		vda = V_diff_angle_Tx_Rx
	RETURN
!
	END SUBROUTINE Ctransf
!
!	Subroutine to calculate the total antenna attenuation Attenuation
!
	SUBROUTINE Antenna_correction (H_diff_angle, V_diff_angle, Ant_typ_H, &
									Ant_typ_V, Attenuation, Error)
!
	IMPLICIT	NONE
!
	CHARACTER*7	Ant_typ_H, Ant_typ_V
	INTEGER		Error
	REAL		H_diff_angle, V_diff_angle, Attenuation, TDA, Correction_H, TDx
	REAL		Correction_V, H_diff_angle_x, V_diff_angle_x
!
!	Calculate the total difference angle TDA:
	TDA = ACOSD(COSD(V_diff_angle) * COSD(H_diff_angle))
	H_diff_angle_x = H_diff_angle
	V_diff_angle_x = V_diff_angle
!
!	Ensure, that H_diff_angle_x and V_diff_angle_x is in the range 0 - 360 degrees:
	DO WHILE (H_diff_angle_x .LT. 0.0)
	  H_diff_angle_x = 360.0 + H_diff_angle_x
	END DO
	DO WHILE (V_diff_angle_x .LT. 0.0)
	  V_diff_angle_x = 360.0 + V_diff_angle_x
	END DO
	DO WHILE (H_diff_angle_x .GE. 360.0)
	  H_diff_angle_x = H_diff_angle_x - 360.0
	END DO
	DO WHILE (V_diff_angle_x .GE. 360.0)
	  V_diff_angle_x = V_diff_angle_x - 360.0
	END DO
!
!	Calculate the attenuation of the horizontal antenna Correction_H:
	TDx  = TDA
	IF (H_diff_angle_x .GT. 180.0) THEN
	  TDx  = 360.0 - TDA
	  H_diff_angle_x = 360.0 - H_diff_angle_x
	END IF
	CALL Antenna (Ant_typ_H, TDx, Correction_H, Error)
	IF (Error .NE. 0) RETURN
	Correction_H = -1.0 * Correction_H
!
!	Calculate the attenuation of the vertical antenna Correction_V:
	TDx  = TDA
	IF (V_diff_angle_x .GT. 180.0) THEN
	  TDx  = 360.0 - TDA
	  V_diff_angle_x = 360.0 - V_diff_angle_x
	END IF
	CALL Antenna (Ant_typ_V, TDx, Correction_V, Error)
	IF (Error .NE. 0) RETURN
!
	Correction_V = -1.0 * Correction_V
!	Calculate the total antenna attenuation Attenuation:
	IF (ABS(Correction_H-Correction_V) .LT. 0.0001) THEN
		Attenuation = Correction_H
	  ELSE
		IF (Correction_H .GT. Correction_V) THEN
			Attenuation = Correction_V+(Correction_H-Correction_V)*ABS(H_diff_angle_x)/ &
					(ABS(H_diff_angle_x)+ABS(V_diff_angle_x))
		  ELSE
			Attenuation = Correction_H+(Correction_V-Correction_H)*ABS(V_diff_angle_x)/ &
					(ABS(H_diff_angle_x)+ABS(V_diff_angle_x))
		END IF
	END IF
	IF (Attenuation .GT. 40.0) Attenuation = 40.0
!
	RETURN
!
	END SUBROUTINE Antenna_correction
!