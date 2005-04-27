!
!	Calc_distance.f90									G.H.			13.04.2005
!
!	This subroutine calculates the distance  between two points.
!
	SUBROUTINE Calc_distance (LonA, LatA, LonB, LatB, D)
!
	IMPLICIT			NONE
!
!	Lat, Lon in degrees, D, R in km
	DOUBLE PRECISION	LonA, LatA, LonB, LatB, D, R, T
!
!	averaging R for latitude
	R = (6.378137D3 - 2.1385D1 * DSIND((LatA + LatB) / 2D0))
!
	T = DSIND((LatB - LatA) / 2D0)**2D0 + DCOSD(LatA) * DCOSD(LatB) &
					* DSIND((LonB - LonA) / 2D0)**2D0
	D = 2D0 * R * DATAN2(DSQRT(T), DSQRT(1D0 - T)) 
!
	RETURN
!
	END SUBROUTINE Calc_distance
!