!
!	Calc_distance.f90
!
!	This subroutine calculates the distance  between two points.
!
	SUBROUTINE Calc_distance (LongA, LatA, LongB, LatB, D)
!
	IMPLICIT			NONE
!
	DOUBLE PRECISION	LongA, LatA, LongB, LatB, D, T
!
	IF ((LongA .EQ. LongB) .AND. (LatA .EQ. LatB)) THEN
	  D = 0.0D0
	  RETURN
	END IF
!
	T = DSIND(LatA) * DSIND(LatB) + DCOSD(LatA) * DCOSD(LatB) * DCOSD(LongB-LongA)
	IF (T .GT.  1.0D0) T =  1.0D0
	IF (T .LT. -1.0D0) T = -1.0D0
	D = 111.2 * DACOSD(T)
!
	RETURN
!
	END SUBROUTINE Calc_distance
!