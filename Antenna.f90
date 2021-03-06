!
!	Antenna.f90											P. Benner		14.03.2000
!														G.H.			22.01.2010
!						                                
!
!	Subroutine to calculate the gain (loss) of an antenna.
!
!
!	Input values 
!
!	A_typ	Type of antenna (TR25-08)
!	Angle	Angle, where loss is calculated [0..360 or -180..+180 degrees]
!
!
!	Output values
!
!	RHO		Loss [linear] (1.0 = no loss;  max. = 0.01)
!	Error	Error-value (0 = no error; 1038 = error) 
!
!	**************************************************************
!
	SUBROUTINE Antenna (A_typ, Angle, RHO, Error)
!
	IMPLICIT		NONE
!
	CHARACTER*7		A_typ
	REAL			Angle, RHO
	INTEGER*4		Error
!
	CHARACTER*2		TPE
	REAL			LEAD, COLEA, TRAIL, TERM1, Alpha
	REAL			TERM2, B, COAL, E, K1, K2, K3, K4, K5
	REAL			R0, R1, R2, COAX, P0, X, PI
	INTEGER*4		IOS, A, M, N, R, P
!
	PI    = 3.14159265
	TPE	  = A_typ(4:5)
	RHO = 1.0
!
	IF (TPE .EQ. 'ND') RETURN
!
	IF (Angle .LT. 180.0) THEN
		Alpha = Angle
	ELSE
		Alpha = Angle - 360.0
	ENDIF
!
	READ(A_typ(1:3),'(F3.0)', IOSTAT=IOS) LEAD
	IF (IOS .NE. 0) THEN
	  Error = 1038
	  RETURN
	END IF
	READ(A_typ(6:7),'(F2.0)', IOSTAT=IOS) TRAIL
	IF (IOS .NE. 0) THEN
	  Error = 1038
	  RETURN
	END IF
	TRAIL = TRAIL / 100.0
	COLEA = COSD (LEAD)
	TERM1 = 1.0 - COLEA ** 2.0
	COAL  = COSD (Alpha)
!
	IF (TPE .EQ. 'EA') THEN
	  IF ((LEAD .GT. 65.0) .OR. (LEAD .EQ. 0.0)) THEN
		  Error = 1038
		  RETURN
		ELSE
		  TERM2 = 1.0 - (SQRT (2.0) * COLEA - 1.0) ** 2.0
		  B = 0.5 * TERM1 / TERM2
		  RHO = 4.0 * B * COAL / ((4.0 * B - 1.0) * COAL**2.0 + 1.0)
	  END IF
!
	ELSEIF (TPE .EQ. 'EB') THEN
	  IF ((LEAD .GT. 79.0) .OR. (LEAD .EQ. 0.0)) THEN
		  Error = 1038
		  RETURN
		ELSE
		  TERM2 = 1.44 - (SQRT (2.0) * COLEA - 0.8) ** 2.0
		  B = 0.72 * TERM1 / TERM2
		  X = B*(B-0.2)*COAL**2.0+0.2*B
		  IF (X .LT. 0.0) X = 0.0
		  RHO = (1.6*B*COAL+2.4*SQRT(X))/((4.0*B -1.44)*COAL**2.0+1.44)
	  END IF
!
	ELSEIF (TPE .EQ. 'EC') THEN
	  IF ((LEAD .GT. 96.0) .OR. (LEAD .EQ. 0.0)) THEN
		  Error = 1038
		  RETURN
		ELSE
		  TERM2 = 1.96 - (SQRT (2.0) * COLEA - 0.6) ** 2.0
		  B = 0.98 * TERM1 / TERM2
		  X = B*(B-0.4)*COAL**2.0+0.4*B
		  IF (X .LT. 0.0) X = 0.0
		  RHO = (1.2*B*COAL+2.8*SQRT(X))/((4.0*B-1.96)*COAL**2.0+1.96)
	  END IF
!
	ELSEIF (TPE .EQ. 'DE') THEN
	  IF ((LEAD .GT. 65.0) .OR. (LEAD .EQ. 0.0)) THEN
		  Error = 1038
		  RETURN
		ELSE
		  TERM2 = 2.0 - (2.0 * COLEA - SQRT (2.0)) ** 2.0
		  B = TERM1 / TERM2
		  RHO = ABS (4.0*B*COAL/((4.0*B-1.0)*COAL**2.0+1.0))
	  END IF
!
	ELSEIF (TPE .EQ. 'LA') THEN
	  IF ((LEAD .GT. 120.) .OR. (LEAD .EQ. 0.0)) THEN
		  Error = 1038
		  RETURN
		ELSE
		  B = (Alpha * PI / 180.0)
		  IF (ABS(B) .LE. 3.0 * (LEAD * PI / 180.0) / 2.0) THEN
			  TERM2 = COS (PI / 3.0 * B / (LEAD * PI / 180.0))
			  RHO = COS ((1.0 - TERM2) * PI / 2.0 )
			ELSE
			  RHO = TRAIL
		  END IF
	  END IF  
!
	ELSEIF (TPE .EQ. 'KA') THEN
	  IF (LEAD .GT. 100.0) THEN
		  Error = 1038
		  RETURN
		ELSE
		  B = LEAD / 100.0
		  X = (1.0 - B) ** 2.0 * COAL ** 2.0 + 4.0 * B
		  IF (X .LT. 0.0) X = 0.0
		  TERM2 = SQRT (X)
		  RHO   = ((1.0 - B) * COAL + TERM2) / 2.0
	  END IF
!
	ELSEIF (TPE .EQ. 'CA') THEN
	  IF (LEAD .GT. 100.0) THEN
		  Error = 1038
		  RETURN
		ELSE
		  B = LEAD / 100.0
		  X = (1.0-B**2.0)**2.0*(COS(Alpha*PI/90.0))**2.0+4.0*B**2.0
		  IF (X .LT. 0.0) X = 0.0
		  TERM2 = SQRT(X)
		  X = ((1.0-B**2.0) * COS(Alpha*PI/90.0)+TERM2) / 2.0
		  IF (X .LT. 0.0) X = 0.0
		  RHO   = SQRT(X)
	  END IF
!
	ELSEIF (TPE .EQ. 'CB') THEN
	  IF (LEAD .GT. 100.0) THEN
		  Error = 1038
		  RETURN
		ELSE
		  B = LEAD / 100.0
		  X = (1.0-B**2.0)**2.0*(COS(Alpha*PI/60.0))**2.0+4.0*B**2.0
		  IF (X .LT. 0.0) X = 0.0
		  TERM2 = SQRT(X)
		  X = ((1.0-B**2.0)*COS(Alpha*PI/60.0)+TERM2) / 2.0
		  IF (X .LT. 0.0) X = 0.0
		  RHO   = SQRT(X)
	  END IF
!
	ELSEIF (TPE .EQ. 'CC') THEN
	  IF (LEAD .GT. 100.0) THEN
		  Error = 1038
		  RETURN
		ELSE
		  B = LEAD / 100.0
		  X = (1.0-B**2.0)**2.0*(COS(Alpha*PI/45.0))**2.0+4.0*B**2.0
		  IF (X .LT. 0.0) X = 0.0
		  TERM2 = SQRT(X)
		  X =  ((1.0-B**2) * COS(Alpha*PI/45.0)+TERM2)/ 2.0 
		  IF (X .LT. 0.0) X = 0.0
		  RHO   = SQRT(X)
	  END IF
!
	ELSEIF ((TPE .EQ. 'TA') .OR. (TPE(1:1) .EQ. 'P')) THEN
	 IF ((LEAD .LT. 1.0) .OR. (LEAD .GT. 890.0)) THEN
		Error = 1038
		RETURN
	 ELSE
		IF (COAL .GT. 0.0) THEN
			X = -0.1505 / LOG10(COSD(LEAD/10.0))
			RHO = COAL ** X
		ELSE
			RHO=0.01
		END IF
	END IF
!
	ELSEIF (A_typ(4:4) .EQ. 'V' .OR. A_typ(4:4) .EQ. 'W') THEN
	  READ(A_typ(1:1),'(I1)', IOSTAT=IOS) M
	  IF (IOS .NE. 0) THEN
		Error = 1038
		RETURN
	  END IF
	  READ(A_typ(2:3),'(I2)', IOSTAT=IOS) N
	  IF (IOS .NE. 0) THEN
		Error = 1038
		RETURN
	  END IF
	  A = M * 5 + 15
	  E = 1.0
	  IF (A_typ(5:5) .EQ. 'A') E = 1E-5
	  IF (A_typ(5:5) .EQ. 'B') E = 0.05
	  IF (A_typ(5:5) .EQ. 'C') E = 0.10
	  IF (A_typ(5:5) .EQ. 'D') E = 0.15
	  IF (A_typ(5:5) .EQ. 'E') E = 0.20
	  IF (A_typ(5:5) .EQ. 'F') E = 0.25
	  IF (A_typ(5:5) .EQ. 'G') E = 0.30
	  IF (A_typ(5:5) .EQ. 'H') E = 0.35
	  IF (A_typ(5:5) .EQ. 'I') E = 0.40
	  IF (E .EQ. 1.0) THEN
		Error = 1038
		RETURN
	  END IF
	  K5 = ((1.0 + E)/2.0)**2.0
	  B  = K5/2.0*(1.0-COSD(FLOAT(A))**2)/(K5-(COSD(FLOAT(A))/SQRT(2.0)-(1.0-E)/2.0)**2.0)
	  K4 = B - K5
	  K3 = B * E * K5
	  K2 = B**2 * K5 - K3
	  K1 = B * (1.0 - E)/2.0
	  R1 = (K1 * COAL + SQRT(K2 * COAL**2.0 + K3))/(K4 * COAL**2.0 + K5)
	  COAX  = COSD((Alpha-2.0*FLOAT(N)))
	  R2 = (K1 * COAX + SQRT(K2 * COAX**2.0 + K3))/(K4 * COAX**2.0 + K5)
	  RHO = R1
	  IF (RHO .LT. R2) RHO = R2
!		2nd lobe for W-type
	  IF (A_typ(4:4) .EQ. 'W') THEN
	    READ(A_typ(6:6),'(I1)', IOSTAT=IOS) R
	    IF (IOS .NE. 0) THEN
		  Error = 1038
		  RETURN
	    END IF
	    READ(A_typ(7:7),'(I1)', IOSTAT=IOS) P
	    IF (IOS .NE. 0) THEN
		  Error = 1038
		  RETURN
	    END IF
	    R0 = FLOAT(R)/20.0
	    P0 = FLOAT(P)/20.0 + 0.35
	    IF (Alpha .GE. 0.0 .AND. Alpha .LE. 2.0*FLOAT(N)) THEN
		  IF (RHO .LT. P0) RHO = P0
		ELSE
		  IF (RHO .LT. R0) RHO = R0
	    END IF
!	no trail for W-type, override
		TRAIL = 0.01
	  ENDIF	
!	no defined type
	ELSE
		Error = 1038
		RETURN
	ENDIF
!
	IF(RHO .LT. TRAIL) RHO = TRAIL
	IF (RHO .LT. 0.01) RHO = 0.01
	IF (RHO .GT. 1.0)  RHO = 1.0

	RETURN
!
	END SUBROUTINE Antenna
!
