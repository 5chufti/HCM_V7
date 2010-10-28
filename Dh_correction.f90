!
!	Dh_Correction.f90									P. Benner		10.10.2003
!														
!
!	Subroutine to calculate the correction factor according terrain
!	irregularity 'CDH'.
!
!
!	Input values:
!				Dh			Delta-h (terrain irregularity) in m
!				Distance    Distance in km
!				Frequency	Transmitter frequency in MHz
!
!
!	Output values:
!				Dh_corr		Correction factor according terrain irregularity
!
!
!
	SUBROUTINE Dh_Correction (Dh, Distance, Frequency, Dh_corr)
!
	IMPLICIT		 NONE
!
	DOUBLE PRECISION Distance, Frequency
	REAL			 Dh, Dh_corr
!
	INTEGER*4		 f_sup, f_inf
	REAL			 A, A1_100, A2_100, A1_600, A2_600 
	REAL			 C100, C600, C2000, A1_2000, A2_2000 
!
	IF ((Distance .LT. 1.0D1) .OR. (Dh .EQ. 50.0)) THEN
	  Dh_corr = 0.0
	  RETURN
	END IF
!
	A = Dh
	IF (Dh .LE. 10.0 ) A =  10.0
	IF (Dh .GE. 500.0) A = 500.0
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
	A1_100	= 0.0
	A1_600	= 0.0
	A1_2000	= 0.0
	A2_100	= 0.0
	A2_600	= 0.0
	A2_2000	= 0.0
!
	IF (f_inf .EQ. 100) THEN
!	  use FIGURE 1
	  IF (Distance .LT. 200.0) THEN
!		A1_100 = correction factor for distances 50 - 100 km:        
		IF (A .LE. 20.0) A1_100 = (A - 10.0) * 0.3 - 7.0
		IF ((A .GT. 20.0) .AND. (A .LE. 30.0)) THEN
		  A1_100 = (A - 30.0) * 0.15 - 2.5
		END IF
		IF ((A .GT. 30.0) .AND. (A .LE. 50.0)) THEN
		  A1_100 = (A - 50.0) * 0.125
		END IF
		IF ((A .GT. 50.0) .AND. (A .LE. 80.0)) THEN
		  A1_100 = (A - 80.0) * 0.1 + 3.0
		END IF
		IF ((A .GT. 80.0) .AND. (A .LE. 100.0)) THEN
		  A1_100 = (A - 100.0) * 0.1 + 5.0
		END IF
		IF ((A .GT. 100.0) .AND. (A .LE. 150.0)) THEN
		  A1_100 = (A - 150.0) * 0.06 + 8.0
		END IF
		IF ((A .GT. 150.0) .AND. (A .LE. 300.0)) THEN
		  A1_100 = (A - 300.0) * 0.04 + 14.0
		END IF
		IF (A .GT. 300.0) A1_100 = (A - 500.0) * 0.025 + 19.0
	  END IF
	  IF (Distance .GT. 100.0) THEN
!		A2_100 = correction factor for distances > 200 km:        
		IF (A .LE. 20.0) A2_100 = (A - 20.0) * 0.1 - 2.0
		IF ((A .GT. 20.0) .AND. (A .LE. 30.0)) THEN
		  A2_100 = (A - 30.0) * 0.05 - 1.5
		END IF
		IF ((A .GT. 30.0) .AND. (A .LE. 50.0)) THEN
		  A2_100 = (A - 50.0) * 0.075
		END IF
		IF ((A .GT. 50.0) .AND. (A .LE. 80.0)) THEN
		  A2_100 = (A - 80.0) * 0.067 + 2.0
		END IF
		IF ((A .GT. 80.0) .AND. (A .LE. 100.0)) THEN
		  A2_100 = (A - 100.0) * 0.05 + 3.0
		END IF
		IF ((A .GT. 100.0) .AND. (A .LE. 150.0)) THEN
		  A2_100 = (A - 150.0) * 0.03 + 4.5
		END IF
		IF ((A .GT. 150.0) .AND. (A .LE. 300.0)) THEN
		  A2_100 = (A - 300.0) * 0.0167 + 7.0
		END IF
		IF (A .GT. 300.0) A2_100 = (A - 500.0) * 0.0125 + 9.5
	  END IF
	END IF
!
	IF ((f_inf .EQ. 600) .OR. (f_sup .EQ. 600)) THEN
!	  use FIGURE 2
	  IF (Distance .LT. 200.0) THEN  
!		A1_600 = correction factor for distances 50 - 100 km:        
		IF (A .LE. 20.0) A1_600 = (A - 10.0) * 0.4 - 10.0
		IF ((A .GT. 20.0) .AND. (A .LE. 30.0)) THEN
		  A1_600 = (A - 30.0) * 0.3 - 3.0
		END IF
		IF ((A .GT. 30.0) .AND. (A .LE. 50.0)) THEN
		  A1_600 = (A - 50.0) * 0.15
		END IF
		IF ((A .GT. 50.0) .AND. (A .LE. 80.0)) THEN
		  A1_600 = (A - 80.0) * 0.133 + 4.0
		END IF
		IF ((A .GT. 80.0) .AND. (A .LE. 100.0)) THEN
		  A1_600 = (A - 100.0) * 0.15 + 7.0
		END IF
		IF ((A .GT. 100.0) .AND. (A .LE. 150.0)) THEN
		  A1_600 = (A - 150.0) * 0.06 + 10.0
		END IF
		IF ((A .GT. 150.0) .AND. (A .LE. 300.0)) THEN
		  A1_600 = (A - 300.0) * 0.067 + 20.0
		END IF
		IF (A .GT. 300.0) A1_600 = (A - 500.0) * 0.04 + 28.0
	  END IF
	  IF (Distance .GT. 100.0) THEN
!		A2_600 = correction factor for distances > 200 km:        
		IF (A .LE. 20.0) A2_600 = (A - 20.0) * 0.2 - 3.0
		IF ((A .GT. 20.0) .AND. (A .LE. 30.0)) THEN
		  A2_600 = (A - 30.0) * 0.1 - 2.0
		END IF
		IF ((A .GT. 30.0) .AND. (A .LE. 50.0)) THEN
		  A2_600 = (A - 50.0) * 0.1
		END IF
		IF ((A .GT. 50.0) .AND. (A .LE. 80.0)) THEN
		  A2_600 = (A - 80.0) * 0.067 + 2.0
		END IF
		IF ((A .GT. 80.0) .AND. (A .LE. 100.0)) THEN
		  A2_600 = (A - 100.0) * 0.075 + 3.5
		END IF
		IF ((A .GT. 100.0) .AND. (A .LE. 150.0)) THEN
		  A2_600 = (A - 150.0) * 0.03 + 5.0
		END IF
		IF ((A .GT. 150.0) .AND. (A .LE. 300.0)) THEN
		  A2_600 = (A - 300.0) * 0.033 + 10.0
		END IF
		IF (A .GT. 300.0) A2_600 = (A - 500.0) * 0.015 + 13.0
	  END IF
	END IF
!
	IF (f_sup .EQ. 2000) THEN
!	  use FIGURE 3
	  IF (Distance .LT. 200.0) THEN
!		A1_2000 = correction factor for distances 50 - 100 km:        
		IF (A .LE. 20.0) A1_2000 = (A - 10.0) * 0.4 - 10.0
		IF ((A .GT. 20.0) .AND. (A .LE. 30.0)) THEN
		  A1_2000 = (A - 30.0) * 0.3 - 3.0
		END IF
		IF ((A .GT. 30.0) .AND. (A .LE. 50.0)) THEN
		  A1_2000 = (A - 50.0) * 0.15
		END IF
		IF ((A .GT. 50.0) .AND. (A .LE. 80.0)) THEN
		  A1_2000 = (A - 80.0) * 0.1667 + 5.0
		END IF
		IF ((A .GT. 80.0) .AND. (A .LE. 100.0)) THEN
		  A1_2000 = (A - 100.0) * 0.185 + 8.7
		END IF
		IF ((A .GT. 100.0) .AND. (A .LE. 150.0)) THEN
		  A1_2000 = (A - 150.0) * 0.074 + 12.4
		END IF
		IF ((A .GT. 150.0) .AND. (A .LE. 300.0)) THEN
		  A1_2000 = (A - 300.0) * 0.082667 + 24.8
		END IF
		IF (A .GT. 300.0) A1_2000 = (A - 500.0) * 0.0495 + 34.7
	  END IF
	  IF (Distance .GT. 100.0) THEN
!		A2_2000 = correction factor for distances > 200 km:        
		IF (A .LE. 20.0) A2_2000 = (A - 20.0) * 0.2 - 3.0
		IF ((A .GT. 20.0) .AND. (A .LE. 30.0)) THEN
		  A2_2000 = (A - 30.0) * 0.1 - 2.0
		END IF
		IF ((A .GT. 30.0) .AND. (A .LE. 50.0)) THEN
		  A2_2000 = (A - 50.0) * 0.1
		END IF
		IF ((A .GT. 50.0) .AND. (A .LE. 80.0)) THEN
		  A2_2000 = (A - 80.0) * 0.08333 + 2.5
		END IF
		IF ((A .GT. 80.0) .AND. (A .LE. 100.0)) THEN
		  A2_2000 = (A - 100.0) * 0.09 + 4.3
		END IF
		IF ((A .GT. 100.0) .AND. (A .LE. 150.0)) THEN
		  A2_2000 = (A - 150.0) * 0.038 + 6.2
		END IF
		IF ((A .GT. 150.0) .AND. (A .LE. 300.0)) THEN
		  A2_2000 = (A - 300.0) * 0.041333 + 12.4
		END IF
		IF (A .GT. 300.0) A2_2000 = (A - 500.0) * 0.0185 + 16.1
	  END IF
	END IF
!
	IF (Distance .LE. 50.0) THEN
	  C100  = A1_100  * (Distance - 10.0) / 40.0 
	  C600  = A1_600  * (Distance - 10.0) / 40.0 
	  C2000 = A1_2000 * (Distance - 10.0) / 40.0 
	END IF
	IF ((Distance .GT. 50.0) .AND. (Distance .LE. 100.0)) THEN
	  C100  = A1_100 
	  C600  = A1_600
	  C2000 = A1_2000
	END IF
	IF ((Distance .GT. 100.0) .AND. (Distance .LE. 200.0)) THEN
	  C100  = A2_100  - (Distance - 200.0) * (A1_100 - A2_100) / 100.0
	  C600  = A2_600  - (Distance - 200.0) * (A1_600 - A2_600) / 100.0
	  C2000 = A2_2000 - (Distance - 200.0) * (A1_2000 - A2_2000) / 100.0
	END IF
	IF (Distance .GT. 200.0) THEN
	  C100  = A2_100 
	  C600  = A2_600
	  C2000 = A2_2000
	END IF
!
	IF ((f_inf .EQ. 100) .AND. (f_sup .EQ. 600)) THEN
	  Dh_corr = C100 + (C600 - C100) * LOG10(Frequency/100.0) * LOG10(600.0/100.0)
	END IF
!
	IF ((f_inf .EQ. 600) .AND. (f_sup .EQ. 2000)) THEN
	  Dh_corr = C600 + (C2000 - C600) * LOG10(Frequency/600.0) * LOG10(2000.0/600.0)
	END IF
!  
	RETURN
!
	END SUBROUTINE Dh_correction
!