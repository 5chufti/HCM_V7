!
!	Calc_distance.f90									G.H.			10.05.2005
!
!	This subroutine calculates the distance  between two points.
!
	SUBROUTINE Calc_distance (LonA, LatA, LonB, LatB, D)
!
	IMPLICIT			NONE
!
!	Lat, Lon in degrees, D, R in km
	DOUBLE PRECISION	LonA, LatA, LonB, LatB, D, T
!
!	calculate avg. earthradius at given mean latitude  360/2*Pi = 57,295779513082321
!	R = (6.378137D3 - 2.1385D1 * DSIND((LatA + LatB) / 2D0))
!	R = 6.378137D3 * (1D0 - DSIND((LatA + LatB) / 2D0)**2D0 / 2.9825722356D2)
!	R = (6.378137D3 - 2.1385D1 * DSIND((LAT + N_LAT) / 2D0)) / 5.7295779513082321D1
!
	T = DSIND((LatB - LatA) / 2D0)**2D0 + DCOSD(LatA) * DCOSD(LatB) &
					* DSIND((LonB - LonA) / 2D0)**2D0
	D = 2D0 * 6.37129D3 * DATAN2(DSQRT(T), DSQRT(1D0 - T)) 
!
	RETURN
!
	END SUBROUTINE Calc_distance
!