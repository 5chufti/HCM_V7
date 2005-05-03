!
!	Calc_direction.f90									G.H.			13.04.2005
!
!	Subroutine to calculate (horizontal) direction.
!
	SUBROUTINE Calc_direction (LonA, LatA, LonB, LatB, Azi)
!
	IMPLICIT			NONE
!
!	lon, lat, azi in degrees 
	DOUBLE PRECISION	LonA, LatA, LonB, LatB, Azi
!
	Azi = DATAN2D(DSIND(LonB-LonA) * DCOSD(LatB), & 
			DCOSD(LatA) * DSIND(LatB) - DSIND(LatA) * DCOSD(LatB) * DCOSD(LonB - LonA))
	IF (Azi .LT. 0D0) Azi = Azi + 360D0
	RETURN
!
	END SUBROUTINE Calc_direction
!