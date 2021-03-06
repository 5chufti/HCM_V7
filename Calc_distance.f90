!
!	Calc_distance.f90				(c) Gottfried Harasek '04 - '17		10.05.2005
!	This file is part of HCM.
!
!	Calc_distance.f90 is free software: you can redistribute it and/or modify
!	it as long as this copyright notice is kept intact, the sourcecode is
!	distributed with the final distributed product, mentioning the copyright.
!
!	Calc_distance.f90 is distributed in the hope that it will be useful,
!	but WITHOUT ANY WARRANTY; without even the implied warranty of
!	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.!
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
!
!	calculating between two antipodes will raise a runtime error because T=1
!
	D = 2D0 * 6.37129D3 * DATAN2(DSQRT(T), DSQRT(1D0 - T)) 
!
	RETURN
!
	END SUBROUTINE Calc_distance
!