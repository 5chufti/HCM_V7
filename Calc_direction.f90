!
!	Calc_direction.f90			(c) Gottfried Harasek '04 - '17		13.04.2005
!	This file is part of HCM.
!
!	Calc_direction.f90 is free software: you can redistribute it and/or modify
!	it as long as this copyright notice is kept intact, the sourcecode is
!	distributed with the final distributed product, mentioning the copyright.
!
!	Calc_direction.f90 is distributed in the hope that it will be useful,
!	but WITHOUT ANY WARRANTY; without even the implied warranty of
!	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
!
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