!
!	CooConv.f90											P. Benner		15.12.2003
!														G.Harasek       10.11.2008
!
!	This subroutine converts co-ordinates from numbers
!	(longitude, latitude) to a string.
!
	SUBROUTINE CooConv (Long, Lat, Coo)
!
	IMPLICIT			NONE
!
	DOUBLE PRECISION	Long, Lat
	CHARACTER*15		Coo
!
	INTEGER				I, J, K
	DOUBLE PRECISION	Lo, La
!
!
	Lo = Long
	La = Lat
	Coo = '               '
	IF (Lo .GE. 0D0) THEN
		Coo(4:4) = 'E'
	  ELSE
		Coo(4:4) = 'W'
		Lo = -1.0D0 * Lo
	END IF               
	IF (La .GE. 0.0D0) THEN
		Coo(11:11) = 'N'
	  ELSE
		Coo(11:11) = 'S'
		La = -1.0D0 * La
	END IF                                            
!      
	I = INT(Lo)
	Lo = Lo - I
	Lo = Lo * 6.0D1
	J = INT(Lo)
	Lo = Lo - J
	Lo = Lo * 6.0D1
	K = NINT(Lo)
	IF (K .GE. 60) THEN
	  K = K - 60
	  J = J + 1
	  IF (J .GE. 60) THEN
		J = J - 60
		I = I + 1
	  END IF       
	END IF       
!      
	WRITE (Coo(1:3), '(I3.3)') I
	WRITE (Coo(5:6), '(I2.2)') J
	WRITE (Coo(7:8), '(I2.2)') K
!              
	I = INT(La)
	La = La - I
	La = La * 6.0D1
	J = INT(La)
	La = La - J
	La = La * 6.0D1
	K = NINT(La)
	IF (K .GE. 60) THEN
	  K = K - 60
	  J = J + 1
	  IF (J .GE. 60) THEN
		J = J - 60
		I = I + 1
	  END IF       
	END IF       
!      
	WRITE (Coo(9:10), '(I2.2)') I
	WRITE (Coo(12:13), '(I2.2)') J
	WRITE (Coo(14:15), '(I2.2)') K
!
	RETURN
!
	END SUBROUTINE CooConv
!