!
!	Antenna_correction.f90
!														G.H.			5.10.2005
!
	SUBROUTINE Ctransf (azi,aziM,ele,eleM,hda,vda)
!
!	Subroutine to calculate H_diff_angle, V_diff_angle
!	as spherical coordinate transformation 
!	all distances and angles are in radians and normalized to the sphere
!
!	azi  ... azi from Tx to Rx (  0..360 in degrees)
!	aziM ... azi of Tx antenna (  0..360 in degrees)
!	ele  ... ele from Tx to Rx (-90.. 90 in degrees)
!	eleM ... ele of Tx antenna (-90.. 90 in degrees)
!	hda  ... resulting horiz angle Tx antenna to Rx (-180..180 in degrees)
!	vda	 ... resulting vert angle TX antenna to Rx (-90..+90 in degrees)
!
	IMPLICIT	NONE
!
	DOUBLE PRECISION	azi, aziM, ele, eleM
	REAL				hda, vda
!
	DOUBLE PRECISION	a, b, d
!
!	calc dist
	a = DSIND((ele - eleM) / 2D0)**2D0 + DCOSD(eleM) * DCOSD(ele) & 
		* DSIND((azi - aziM) / 2D0)**2D0
	d = 2D0 * DATAN2D(DSQRT(a), DSQRT(1D0 - a))
!	calc bearing
	b = DATAN2D(DSIND(azi - aziM) * DCOSD(ele), &
		DCOSD(eleM) * DSIND(ele) - DSIND(eleM) * DCOSD(ele) * DCOSD(azi - aziM))
!
! new point [long2,lat2] from [long1,lat1] with [dist,bearing] (all in radians)
!
!    sin(lat2) = cos(dir)*cos(lat1)*sin(dist) + sin(lat1)*cos(dist)
!
!                            Cos (dist) - Sin(lat1) * Sin(lat2)
!    cos(long2-long1) =   ----------------------------------
!                                  Cos (lat1) * Cos(lat2)
!
!
!    lat2 = asin(Sin(lat1) * Cos(dist) + Cos(lat1) * Sin(dist) * Cos(dir))
!    dlon=atan2(sin(tc)*sin(d)*cos(lat1),cos(d)-sin(lat1)*sin(lat))
!    lon=mod( lon1-dlon +pi,2*pi )-pi
!
!   because p1=(0/0) lot of terms drop: sin(0)=0,cos(0)=1
!
	vda = DASIND(DSIND(d) * DCOSD(b))
	hda = DATAN2D(DSIND(d) * DSIND(b), DCOSD(d))
	RETURN
!
	END SUBROUTINE Ctransf
!
!
	SUBROUTINE Antenna_correction (azi,aziM,ele,eleM,hda,vda,hCod,vCod,a,Error)
!
!	Subroutine to calculate the total antenna attenuation
!
!	hda  ... resulting horiz angle Tx antenna to Rx (  0..360 in degrees)
!	vda	 ... resulting vert angle TX antenna to Rx (-90..+90 in degrees)
!	hCod ... horizontal antennacode
!	vCod ... vertical antennacode
!	a    ... resulting attenuation
!	Error... Errorvalue 
!
	IMPLICIT	NONE
!
	CHARACTER*7			hCod, vCod
	INTEGER*4			Error
	REAL				hda, vda, a
	DOUBLE PRECISION	azi, aziM, ele, eleM
!
	REAL				vfe, vbe, hb, vb, h, k
	REAL				delv, w, w1, w2, vae, va0, ra
!
!	electrical or mechanical tilting needs different 'total' angle
!
	IF (vCod(4:5) .EQ. 'TA') THEN
		hda = azi - aziM
		vda = ele - eleM
	ELSE
		CALL Ctransf (azi,aziM,ele,eleM,hda,vda)
	ENDIF
!
!	prepare needed values for further calculations
!
!	check if only horizontal diagram relevant
!
	IF (vda .EQ. 0.0 .OR. vCod(4:5) .EQ. 'ND') THEN
		vfe = 1.0
		vbe = 1.0
		vb = 1.0
	ELSE
		CALL Antenna (vCod, -vda, vfe, Error)
		CALL Antenna (vCod, (180.0 + vda), vbe, Error)
		CALL Antenna (vCod, 180.0, vb, Error)
	ENDIF
!
!	check if only vertical diagram relevant
!
	IF (hda .EQ. 0.0 .OR. hCod(4:5) .EQ. 'ND') THEN
		h = 1.0
		hb = 1.0
	ELSE
		CALL Antenna (hCod, hda, h, Error)
		CALL Antenna (hCod, 180.0, hb, Error)
	ENDIF
!
	IF (Error .NE. 0) RETURN
!
	IF (vCod(4:5) .EQ. 'TA') THEN
!
!	no matching etc. needed for el. tilted antenna
!
		a = h * vfe
	ELSE
!
!	match H and V backlobe
!
	  IF (vb .GT. hb) THEN
		k = hb / vb
		vbe = vbe * SQRT(SIND(vda)**2.0 + (k * COSD(vda))**2.0)
		IF (vbe .LT. 0.01) vbe = 0.01
	  ELSEIF (vb .LT. hb) THEN
		delv = vfe - vbe
		IF (delv .GT. 0.0) THEN
			k = (hb - vb) / (1.0 - vb)
			vbe = vbe + k * delv
			IF (vbe .GT. 1.0) vbe = 1.0
		ENDIF
	  ENDIF
!
!	calculate weighing factors
!
	  IF (hb .EQ. 1.0) THEN
		w = ABS(hda) / 180.0
	  ELSEIF (hb .LT. 0.9) THEN
		w = (1.0 - h) / (1.0 - hb)
		IF (w .GT. 1.0) w = 1.0
	  ELSE
		w1 = ABS(hda) / 180.0
		w2 = (1.0 - h) / (1.0 - hb)
		IF (w2 .GT. 1.0) w2 = 1.0
		k = (1.0 - hb) * 10.0
		IF (k .GT. 1.0) k = 1.0
		w = (1.0 -k) * w1 + k * w2
	  ENDIF
!
!	interp. from vertical diagram
!
	  vae = w * vbe + (1 - w) * vfe
	  IF (vae .GT. 1.0) THEN
		vae = 1.0
	  ELSEIF (vae .LT. 0.01) THEN
		vae = 0.01
	  ENDIF
!
	  va0 = w * hb + ( 1.0 - w)
	  IF (va0 .GT. 1.0) THEN
		va0 = 1.0
	  ELSEIF (va0 .LT. 0.01) THEN
		va0 = 0.01
	  ENDIF
!	
	  ra = h / va0
	  a = vae * SQRT(SIND(vda)**2.0 + (ra * COSD(vda))**2.0)
!
	ENDIF
!
10	IF (a .LT. 0.01) a=0.01
	IF (a .GT. 1.0) a=1.0
	a = -20.0 * LOG10(a)
	RETURN
!
	END SUBROUTINE Antenna_correction
!