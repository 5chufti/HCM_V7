!
!	Antenna_correction.f90
!														G.H.			07.07.2005
!
!	Subroutine to calculate the corrected H_diff_angle, V_diff_angle
!
	SUBROUTINE Ctransf (azi,aziM,ele,eleM,hda,vda)
!
	IMPLICIT	NONE
!
	DOUBLE PRECISION	azi, aziM, ele, eleM
	REAL				hda, vda
	DOUBLE PRECISION	a, b, d
!
!	calc distance
	a = DSIND((ele - eleM) / 2D0)**2D0 + DCOSD(eleM) * DCOSD(ele) & 
		* DSIND((azi - aziM) / 2D0)**2D0
	d = 2D0 * DATAN2D(DSQRT(a), DSQRT(1D0 - a))
!	calc bearing
	b = DATAN2D(DSIND(azi - aziM) * DCOSD(ele), &
		DCOSD(eleM) * DSIND(ele) - DSIND(eleM) * DCOSD(ele) * DCOSD(azi - aziM))
!	new point    
!		sin(lat2) = cos(hdng1)*cos(lat1)*sin(alpha) + sin(lat1)*cos(alpha)
!
!		                     Cos (alpha) - Sin(lat1) * Sin(lat2)
!		cos(long2-long1) =   ----------------------------------
!		                           Cos (lat1) * Cos(lat2)
!
!   because p1=(0/0) lot of terms drop: sin(0)=0,cos(0)=1
!
	vda = SNGL(DASIND(DSIND(d) * DCOSD(b)))
	hda = SNGL(DSIGN(d,b))
	RETURN
!
	END SUBROUTINE Ctransf
!
!
!
!	Subroutine to calculate the total antenna attenuation
!
	SUBROUTINE Antenna_correction (hda, vda, hCod,vCod, a, Error)
!
	IMPLICIT	NONE
!
	CHARACTER*7 hCod, vCod
	INTEGER		Error
	REAL		hda, vda, a, vdc, vfe, vbe, hb, vb, h, k
	REAL		delv, w, w1, w2, vae, va0, ra
!
	vdc = -1.0 * vda
!
!	simple case, only horizontal diagram relevant
!
	IF (VDA .EQ. 0.0) THEN
		CALL Antenna (hCod, hda, a, Error)
		GOTO	10
	ENDIF
!
!	other simple case, only vertical diagram relevant
!
	IF (HDA .EQ. 0.0) THEN
		CALL Antenna (vCod, vdc, a, Error)
		GOTO	10
	ENDIF
!
!	prepare needed values for further calculations
!
	CALL Antenna (vCod, vdc, vfe, Error)
	CALL Antenna (vCod, (180.0 - vdc), vbe, Error)
	CALL Antenna (vCod, 180.0, vb, Error)
!
	IF (Error .NE. 0) RETURN
!
	CALL Antenna (hCod, hda, h, Error)
	CALL Antenna (hCod, 180.0, hb, Error)
!
	IF (Error .NE. 0) RETURN
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
	IF ( ra .EQ. 1.0) THEN
		a = vae
	ELSE
		a = vae * SQRT(SIND(vda)**2.0 + (ra * COSD(vda))**2.0)
		IF (a .GT. 1.0) THEN
			a = 1.0
		ELSEIF (a .LT. 0.01) THEN
			a = 0.01
		ENDIF
	ENDIF
!
10	a = -20.0 * LOG10(a)
!
	IF (a .GT. 40.0) a = 40.0
!
	RETURN
!
	END SUBROUTINE Antenna_correction
!