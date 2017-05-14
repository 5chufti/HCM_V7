!
!	Pofile.f90						(c) Gottfried Harasek '04 - '17		14.05.2017
!	This file is part of HCM.
!
!	Profile.f90 is free software: you can redistribute it and/or modify
!	it as long as this copyright notice is kept intact, the sourcecode is
!	distributed with the final distributed product, mentioning the copyright.
!
!	Profile.f90 is distributed in the hope that it will be useful,
!	but WITHOUT ANY WARRANTY; without even the implied warranty of
!	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
!
!
!	This subroutine constructs a terrain- and morphological profile from point A to
!	point B in steps of 100 m. The heights or morphological information are stored
!	in '[T|M]_Prof(i)'. The total number of points is in 'PN'. The first profile point
!	'Prof(1)' is the height or morphological type of point A, the last profile point
!	'Prof(PN)' is the height or morphological type of point B.
!
!
!	Input values:
!		LongA		DOUBLE PRECISION	longitude of point A (starting point)
!		LatA		DOUBLE PRECISION	latitude of point A
!		LongB		DOUBLE PRECISION	longitude of point B (ending point)
!		LatB		DOUBLE PRECISION	latitude of point B
!		PD			DOUBLE PRECISION	point distance (grid size) of the profile
!		Topo_path	CHARACTER*63		path of topo database, e.g. 'D:\TOPO'
!		Morpho_path	CHARACTER*63		path of morpho database, e.g. 'D:\MORPHO'
!
!
!	Output values:
!		Prof(i)		INTEGER*2(10002)	field with heights or morpho information
!		PN			INTEGER*2			number of profile points 
!		Error		INTEGER*4			error value
!
!	Possible error values
!		no own, only from Point_Info
!		36		error opening terrain- or morphological data file (data not available)
!		220		error reading record (in 'Point_height' or 'Point_type' subroutine)
!		400		height is missing (-9999) (in 'Point_height' subroutine)
!
!	Called subroutines : Point_Info
!
!	**********************************************************************************
!
	SUBROUTINE PROFILE (LongA, LatA, LongB, LatB)
!
	IMPLICIT			NONE
!
	INCLUDE				'HCM_MS_V7_definitions.F90'
!
	DOUBLE PRECISION	LongA, LatA, LongB, LatB
!
!
	INTEGER(4)			PC, EP
	DOUBLE PRECISION	LAY, LOY, DD, PDa, K, SLA, CLA, SDIR, CDIR, SIDD, CODD, o_Tx, o_Rx
	LOGICAL				slant
	EQUIVALENCE			(p2p,slant)
!
!	**********************************************************************************
!
!	Max. distance = 1000km; distance between two points = 100m
!	-> number of points are 10.000 + 2 for Tx site and Rx site
!
!	number of points in profile
	PN = IDINT(Distance / PD)
!	calc PDa
	PDa = (Distance/PN)/6.37129D3
!	set END Marker
	T_Prof(PN+2)=-9999
!	slant = .FALSE.
	IF (slant) THEN
!	prepare for sloped profile (C_Mode pos)
		o_Tx = DBLE(H_Tx)
		o_Rx = DBLE(H_Rx)
		K = DBLE(H_Rx - H_Tx) / DBLE(PN)
		T_Prof(PN+1)=0
		T_Prof(1)=0
		EP=PN
	ELSE
!	line calculation (C_Mode neg)
		T_Prof(1)=H_Tx
		EP=PN+1
	END IF
	PN = PN+1
!	Direction Tx to Rx 
!	Prepare often used values
	SLA = DSIND(LatA)
	CLA = DCOSD(LatA)
	SDIR = DSIND(Dir_Tx_Rx)
	CDIR = DCOSD(Dir_Tx_Rx)
!	Loop for waypoints Tx to center
	DO PC = 2, NINT(PN/2.0), 1
	  DD=DBLE(PC-1)*PDa
	  SIDD = DSIN(DD)
	  CODD = DCOS(DD)
	  DD = SLA * CODD + CLA * SIDD * CDIR
	  LAY = DASIND(DD)
	  LOY = LongA + DATAN2D(SDIR * SIDD * CLA, CODD - SLA * DD)
!	  get Information of new point:
		CALL Point_info (LOY, LAY, T_Prof(PC), M_Prof(PC))
		IF (slant) T_Prof(PC) = T_Prof(PC) - IIDNNT(o_Tx + K * DBLE(PC-1))
	  IF (HCM_Error .NE. 0) RETURN
	END DO
!
!	Direction Rx to Tx 
!	Prepare often used values
	SLA = DSIND(LatB)
	CLA = DCOSD(LatB)
	SDIR = DSIND(Dir_Rx_Tx)
	CDIR = DCOSD(Dir_Rx_Tx)
!	Loop for waypoints Rx to center
	DO PC = PC, EP, 1
	  DD=DBLE(PN-PC)*PDa
	  SIDD = DSIN(DD)
	  CODD = DCOS(DD)
	  DD = SLA * CODD + CLA * SIDD * CDIR
	  LAY = DASIND(DD)
	  LOY = LongB + DATAN2D(SDIR * SIDD * CLA, CODD - SLA * DD)
!	  get Information of new point:
		CALL Point_info (LOY, LAY, T_Prof(PC), M_Prof(PC))
		IF (slant) T_Prof(PC) = T_Prof(PC) - IIDNNT(o_Rx - K * DBLE(PN-PC))
	  IF (HCM_Error .NE. 0) RETURN
	END DO
	IF (.NOT. slant) THEN
	    H_Datab_Rx = T_Prof(PN)
	END IF
!
	RETURN
!
	END SUBROUTINE PROFILE
!
!	**********************************************************************************
!