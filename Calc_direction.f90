!
!	Calc_direction.f90
!
!	Subroutine to calculate (horizontal) direction.
!
	SUBROUTINE Calc_direction (LongA, LatA, LongB, LatB, Azi)
!
	IMPLICIT			NONE
!
	DOUBLE PRECISION	LongA, LatA, LongB, LatB, Azi, D, T
!
	IF (LongA .EQ. LongB) THEN
		IF (LatA .GE. LatB) THEN
			Azi = 1.8D2
		  ELSE
			Azi = 0.0D0
		END IF
		RETURN
	  ELSE
		! Determine angle
		T = DSIND(LatA) * DSIND(LatB) + DCOSD(LatA) * DCOSD(LatB) * DCOSD(LongB - LongA)
		IF (T .GT.  1.0D0) T =  1.0D0
		IF (T .LT. -1.0D0) T = -1.0D0
		D = DACOSD(T)
		T = (DSIND(LatB)-DSIND(LatA)*DCOSD(D))/(DCOSD(LatA)*DSIND(D))
		IF (T .GT.  1.0D0) T =  1.0D0
		IF (T .LT. -1.0D0) T = -1.0D0
		Azi = DACOSD(T)
		! Is difference between longitudes < 180 degrees?
		IF (DABS(LongA - LongB) .LT. 1.8D2) THEN
			IF (LongB .LT. LongA) THEN
			  Azi = 3.6D2 - Azi
			END IF
		  ELSE
			IF (LongB .GE. LongA) THEN
				! Azi = Azi
			  ELSE
				Azi = 3.6D2 - Azi
			END IF
		END IF
		RETURN
	END IF
!
	END SUBROUTINE Calc_direction
!