!
!	TCA_correction_calculation.f90						P. Benner		29.10.2003
!														G.H.			28.04.2005
!
!	Subroutine to calculate the Terrain Clearance Angle (TCA) correction.
!
!
!	Input values:
!				TCA			Terran Clearance Angle in degrees
!				Frequency	Transmitter frequency in MHz
!
!
!	Output values:
!				TCA_corr	Correction according TCA in dB
!
!
!
	SUBROUTINE TCA_correction_calculation (TCAI, Frequency, TCAI_Corr)
!
	IMPLICIT			NONE
!
	INCLUDE				'HCM_MS_V7_definitions.F90'
!
	INTEGER*4			f_inf, f_sup
	REAL				TCAI, TCAI_corr, TCA_c_100, TCA_c_600, TCA_c_2000
	REAL				v100, v600, v2000
	DOUBLE PRECISION	Frequency
!
	v100  = 0.6492624 * TCAI	! = 37.2 * TCA (in rad) = 37.2 * PI / 180 * TCA (in degrees) 
	v600  = 1.5917403 * TCAI	! = 91.2 * TCA (in rad) = 91.2 * PI / 180 * TCA (in degrees)
	v2000 =	2.9146999 * TCAI	! = 167  * TCA (in rad) = 167  * PI / 180 * TCA (in degrees)
!
	TCA_c_100  =  9.1 - (6.9 + 20.0 * LOG10(SQRT((v100  - 0.1)**2 + 1.0) + v100  - 0.1))
	TCA_c_600  = 13.1 - (6.9 + 20.0 * LOG10(SQRT((v600  - 0.1)**2 + 1.0) + v600  - 0.1))
	TCA_c_2000 = 17.3 - (6.9 + 20.0 * LOG10(SQRT((v2000 - 0.1)**2 + 1.0) + v2000 - 0.1))
!
!	Limits:
	IF (TCA_c_100  .GT.   7.5) TCA_c_100  =   7.5
	IF (TCA_c_100  .LT. -32.0) TCA_c_100  = -32.0
	IF (TCA_c_600  .GT.  16.0) TCA_c_600  =  16.0
	IF (TCA_c_600  .LT. -35.0) TCA_c_600  = -35.0
	IF (TCA_c_2000 .GT.  24.0) TCA_c_2000 =  24.0
	IF (TCA_c_2000 .LT. -36.0) TCA_c_2000 = -36.0
!
!	Set f_inf and f_sup:
	IF (Frequency .LE. 6.0D2) THEN
		f_inf = 100
		f_sup = 600
	  ELSE
		f_inf = 600
		f_sup = 2000
	END IF
!
	IF ((f_inf .EQ. 100) .AND. (f_sup .EQ. 600)) THEN
	  TCAI_corr = TCA_c_100 + (TCA_c_600 - TCA_c_100) * LOG10(Frequency/100.0) * &
				 LOG10(600.0/100.0)
	END IF
!
	IF ((f_inf .EQ. 600) .AND. (f_sup .EQ. 2000)) THEN
	  TCAI_corr = TCA_c_600 + (TCA_c_2000 - TCA_c_600) * LOG10(Frequency/600.0) * &
				 LOG10(2000.0/600.0)
	END IF
!	Corrections for distances < 16km
	IF (Distance .LT. 1.6D1) TCAI_corr = TCAI_corr * Distance / 16.0
!	limit to neg. correction 
	IF (TCAI_corr .GE. 0.0) TCAI_corr = 0.0
	RETURN
!
	END SUBROUTINE TCA_correction_calculation
!
