!
!	Dh_calculation.f90									P. Benner
!														20.11.2003
!
!	Subroutine to calculate the terrain irregularity 'Dh'.
!
!
!	Input values:
!				Distance    Distance in km
!				T_Prof(i)	Height of the profile point(i) [first point (i = 1) is
!							the height of the transmitter site, last point = height
!							of the receiver site] from subroutine 'PROFILE'
!				PD			Point distance (gridsize) of profile in km
!				PN			Number of profile points from subroutine 'PROFILE'
!				
!
!
!	Output values:
!				Dh			Terrain irregularity in meter
!
!
!
!	
	SUBROUTINE Dh_calculation ()
!
	IMPLICIT	NONE
!
!
	INCLUDE		'HCM_MS_V7_definitions.f90'
!
	INTEGER				HI(1670), PStart, PStop
	INTEGER				I, J, K, L, N, S1, S2, S3
	DOUBLE PRECISION	XX
!
!	*******************************************************************
!
!	If distance is less than 10 km, Dh is set to 50 m (no calculation is required). 
	IF (Distance .LT. 1.0D1) THEN
!	  No DH calculations if the distance is less than 10 km.
	  Dh = 50.0
	  RETURN
	END IF
!
	IF (Distance .LE. 5.0D1) THEN
!		Between 10 km and 50 km: Normal calculation mimus two times
!		4.5 km (transmitter and receiver).
!	
!		Starting number of profile point 'PStart' and ending number
!		'PStop':
		XX = 4.5D0 / PD
		PStart = INT(XX) + 1
		XX = (Distance- 4.5D0) / PD
		PStop  = INT(XX) + 1
		N = 0
		DO J = PStart, PStop
		  N = N + 1
		  HI(N) = T_Prof(J)
		END DO
	  ELSE	! Distance > 50 km
!		More than 50 km: Calculation of two parts: 4.5 to 25 km and
!		distance - 25 km  to distance - 4.5 km.
!
!		1st part:
!		Starting number of profil point 'PStart' and ending number
!		'PStop':
		XX = 4.5D0 / PD
		PStart = INT(XX) + 1
		XX = 2.5D1 / PD
		PStop  = INT(XX) + 1
		N = 0
		DO J = PStart, PStop
		  N = N + 1
		  HI(N) = T_Prof(J)
		END DO
!		2nd part:
		XX = (Distance-2.5D1) / PD
		PStart = INT(XX) + 1
		XX = (Distance-4.5D0) / PD
		PStop  = INT (XX) + 1 
		DO J = PStart, PStop
		  N = N + 1
		  HI(N) = T_Prof(J)
		END DO
	END IF
!
!	Sort all heights:
    S1 = N / 2
	DO WHILE (S1 .NE. 0)
      S2 = N - S1
      DO S3 = 1, S2
		DO K = S3, 1, -S1
          L = K + S1
          IF (HI(K) .LT. HI(L)) EXIT
          I = HI(K)
          HI(K) = HI(L)
          HI(L) = I
		END DO
	  END DO
      S1 = S1 / 2
    END DO
!
!	Substract 10% at the top and at the bottom:
!	Remaining difference between the lowest and the highest value
!	is the value of 'Dh':
!
	IF (N .LT. 5) THEN
		Dh = FLOAT(HI(N-NINT(FLOAT(N)/10.0)) - HI(1))
	  ELSE
		Dh = FLOAT(HI(N-NINT(FLOAT(N)/10.0)) - HI(NINT(FLOAT(N)/10.0)))
	END IF
!
	RETURN
!
	END SUBROUTINE Dh_calculation
!