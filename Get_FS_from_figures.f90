!
!	Get_FS_from_figures.f90								P. Benner		06.10.2003
!												
!
!	SUBROUTINE Get_FS_from_figures ( Land_FS_1kW, Sea_FS_1kW )
!
!	Subroutine to get the Land- and Sea field strength from the figures.
!
!
!	Input values:
!
!	Tx_frequency	in MHz	DOUBLE PRECISION	in COMMON NDATA (in HCM_MS_V7_definitions.f90)
!	Distance		in km	DOUBLE PRECISION	in COMMON NDATA (in HCM_MS_V7_definitions.f90)
!	Heff			in m	REAL				in COMMON NDATA (in HCM_MS_V7_definitions.f90)
!	Time_percentage	in %	INTEGER*4			in COMMON NDATA (in HCM_MS_V7_definitions.f90)
!	Sea_temperature			CHARACTER*1			in COMMON CDATA (in HCM_MS_V7_definitions.f90)
!
!	Note: If Heff is less than 0 m or greater than 3000 m, Heff will be limited by this
!		  subroutine to the range 0 m - 3000 m.
!
!
!	Output values:
!
!	HCM_error				INTEGER*4			in COMMON NDATA (in HCM_MS_V7_definitions.f90)
!	Land_FS_1kW				REAL				in COMMON NDATA (in HCM_MS_V7_definitions.f90)
!	Sea_FS_1kW				REAL				in COMMON NDATA (in HCM_MS_V7_definitions.f90)
!
!
!	Possible HCM_error values:
!							   0	no error
!							1028	Distance is > 1000 km, calculations not possible
!							1000	Distance is <=   0 km, calculations not possible
!							2000	wrong Figure_frequency (from Get_figure_FS_value)
!							2001	wrong Time_percentage  (from Get_figure_FS_value)
!							2002	wrong Sea_temperature  (from Get_figure_FS_value)
!							2003	wrong Figure_Heff      (from Get_figure_FS_value)
!							2004	wrong Figure_distance  (from Get_figure_FS_value)
!
!
!	Called subroutine(s):	Get_figure_FS_value
!
!	*****************************************************************************************
!
	SUBROUTINE Get_FS_from_figures ( Land_FS_1kW, Sea_FS_1kW )
!
	IMPLICIT			NONE
!
	INCLUDE				'HCM_MS_V7_definitions.f90'
!
	REAL				Land_FS_1kW, Sea_FS_1kW
!
	INTEGER*4			Error
	INTEGER*4			Distances(78), d_inf, d_sup, f_inf, f_sup, I
	REAL				dh1, dh10
	REAL				Heff_values(8), h_inf, h_sup
	REAL				L_Eiii, L_Eiis, L_Eisi, L_Eiss, L_Esii, L_Esis, L_Essi, L_Esss
	REAL				S_Eiii, S_Eiis, S_Eisi, S_Eiss, S_Esii, S_Esis, S_Essi, S_Esss
	REAL				L_E_ii, S_E_ii, L_E_is, S_E_is, L_E_si, S_E_si, L_E_ss, S_E_ss
	REAL				L_E__i, L_E__s, S_E__i, S_E__s, L_E10dh10, S_E10dh10, E_free_1kW
	REAL				L_E10dh10_i, S_E10dh10_i, L_E10dh10_s, S_E10dh10_s
	REAL				L_E10dh1_i, S_E10dh1_i, L_E10dh1_s, S_E10dh1_s, h10
	REAL				L_E10d_i, S_E10d_i, L_E10d_s, S_E10d_s, L_E10d, S_E10d
	REAL				L_E10dh1, S_E10dh1, L_E10dhx, S_E10dhx, dhx
	REAL				L_E10dhx_i, S_E10dhx_i, L_E10dhx_s, S_E10dhx_s
!
	DATA Heff_values	/10, 20, 37.5, 75, 150, 300, 600, 1200/
	DATA Distances		/1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, &
						 16, 17, 18, 19, 20, 25, 30, 35, 40, 45, 50, 55, 60, &
						 65, 70, 75, 80, 85, 90, 95, 100, 110, 120, 130, 140, &
						 150, 160, 170, 180, 190, 200, 225, 250, 275, 300, &
						 325, 350, 375, 400, 425, 450, 475, 500, 525, 550, &
						 575, 600, 625, 650, 675, 700, 725, 750, 775, 800, &
						 825, 850, 875, 900, 925, 950, 975, 1000/
!
!	*****************************************************************************************
!
!	Check Distance:
	IF (Distance .LE. 0.0D0) THEN
	  HCM_error = 1000	! Distance between Tx and Rx = 0; calculations not possible.
	  RETURN
	END IF
	IF (Distance .GT. 1.0D3) THEN
	  HCM_error = 1028	! Distance is greater than 1000 km; calculations not possible
	  RETURN
	END IF
!
!	Calculate free space field strength for 1 kW:
	E_free_1kW = 107.0 - 20.0 * LOG10 (Distance)
!
!	If distance < 1 km, use free space field strength:
	IF (Distance .LT. 1.0D0) THEN
	  Land_FS_1kW = E_free_1kW
	  Sea_FS_1kW  = E_free_1kW
	  RETURN
	END IF
!
!	Set d_inf and d_sup:
	DO I = 1, 77
	  IF (Distance .GE. DBLE(Distances(I))) THEN
	    d_inf = Distances(I)
		d_sup = Distances(I+1)
	  END IF
	END DO
!
!	Set f_inf and f_sup:
	IF (Tx_frequency .LE. 6.0D2) THEN
		f_inf = 100
		f_sup = 600
	  ELSE
		f_inf = 600
		f_sup = 2000
	END IF
!
!	Limit Heff to range 0 - 3000 m:
	IF (Heff .LT. 0.0)    Heff = 0.0
	IF (Heff .GT. 3000.0) Heff = 3000.0
!
!	Set h_inf and h_sup:
	IF (Heff .LT. 10) THEN
		h_inf = 10.0
		h_sup = 10.0
	  ELSE
		DO I = 1, 7
		  IF (Heff .GE. Heff_values(I)) THEN
		    h_inf = Heff_values(I)
			h_sup = Heff_values(I+1)
		  END IF
		END DO
	END IF
!
!	Inter- or extrapolation of field strength according to h1:
!	Case 10m < h1 < 3000 m:
	IF (Heff .GE. 10.0) THEN
!		Case Heff =  10 - 3000 m
!
		CALL Get_figure_FS_value (f_inf, Time_percentage, Sea_temperature, &
								  h_inf, d_inf, L_Eiii, S_Eiii, Error)
		IF (Error .NE. 0) THEN
		  HCM_error = Error
		  RETURN
		END IF
		CALL Get_figure_FS_value (f_inf, Time_percentage, Sea_temperature, &
								  h_inf, d_sup, L_Eiis, S_Eiis, Error)
		IF (Error .NE. 0) THEN
		  HCM_error = Error
		  RETURN
		END IF
		CALL Get_figure_FS_value (f_inf, Time_percentage, Sea_temperature, &
								  h_sup, d_inf, L_Eisi, S_Eisi, Error)
		IF (Error .NE. 0) THEN
		  HCM_error = Error
		  RETURN
		END IF
		CALL Get_figure_FS_value (f_inf, Time_percentage, Sea_temperature, &
								  h_sup, d_sup, L_Eiss, S_Eiss, Error)
		IF (Error .NE. 0) THEN
		  HCM_error = Error
		  RETURN
		END IF
		CALL Get_figure_FS_value (f_sup, Time_percentage, Sea_temperature, &
								  h_inf, d_inf, L_Esii, S_Esii, Error)
		IF (Error .NE. 0) THEN
		  HCM_error = Error
		  RETURN
		END IF
		CALL Get_figure_FS_value (f_sup, Time_percentage, Sea_temperature, &
								  h_inf, d_sup, L_Esis, S_Esis, Error)
		IF (Error .NE. 0) THEN
		  HCM_error = Error
		  RETURN
		END IF
		CALL Get_figure_FS_value (f_sup, Time_percentage, Sea_temperature, &
								  h_sup, d_inf, L_Essi, S_Essi, Error)
		IF (Error .NE. 0) THEN
		  HCM_error = Error
		  RETURN
		END IF
		CALL Get_figure_FS_value (f_sup, Time_percentage, Sea_temperature, &
								  h_sup, d_sup, L_Esss, S_Esss, Error)
		IF (Error .NE. 0) THEN
		  HCM_error = Error
		  RETURN
		END IF
		L_E_ii = L_Eiii + (L_Eisi - L_Eiii) * LOG10(Heff/h_inf) / LOG10(h_sup/h_inf)
		S_E_ii = S_Eiii + (S_Eisi - S_Eiii) * LOG10(Heff/h_inf) / LOG10(h_sup/h_inf)
		L_E_is = L_Eiis + (L_Eiss - L_Eiis) * LOG10(Heff/h_inf) / LOG10(h_sup/h_inf)
		S_E_is = S_Eiis + (S_Eiss - S_Eiis) * LOG10(Heff/h_inf) / LOG10(h_sup/h_inf)
		L_E_si = L_Esii + (L_Essi - L_Esii) * LOG10(Heff/h_inf) / LOG10(h_sup/h_inf)
		S_E_si = S_Esii + (S_Essi - S_Esii) * LOG10(Heff/h_inf) / LOG10(h_sup/h_inf)
		L_E_ss = L_Esis + (L_Esss - L_Esis) * LOG10(Heff/h_inf) / LOG10(h_sup/h_inf)
		S_E_ss = S_Esis + (S_Esss - S_Esis) * LOG10(Heff/h_inf) / LOG10(h_sup/h_inf)
!
!		Interpolation of field strength as a function of the distance:
		IF (d_sup .EQ. d_inf) THEN
			L_E__i = L_E_ii
			S_E__i = S_E_ii
			L_E__s = L_E_si
			S_E__s = S_E_si
		  ELSE
			L_E__i = L_E_ii + (L_E_is - L_E_ii) * LOG10(Distance/FLOAT(d_inf)) / &
				 LOG10(FLOAT(d_sup)/FLOAT(d_inf))
			S_E__i = S_E_ii + (S_E_is - S_E_ii) * LOG10(Distance/FLOAT(d_inf)) / &
				 LOG10(FLOAT(d_sup)/FLOAT(d_inf))
			L_E__s = L_E_si + (L_E_ss - L_E_si) * LOG10(Distance/FLOAT(d_inf)) / &
				 LOG10(FLOAT(d_sup)/FLOAT(d_inf))
			S_E__s = S_E_si + (S_E_ss - S_E_si) * LOG10(Distance/FLOAT(d_inf)) / &
				 LOG10(FLOAT(d_sup)/FLOAT(d_inf))
		END IF
!
!		Inter- or extrapolation of field strength as a function of the frequency:
		IF (f_sup .EQ. f_inf) THEN
			Land_FS_1kW = L_E__i
			Sea_FS_1kW  = S_E__i
		  ELSE
			Land_FS_1kW = L_E__i + (L_E__s - L_E__i) * LOG10(Tx_frequency/FLOAT(f_inf)) / &
				 LOG10(FLOAT(f_sup)/FLOAT(f_inf))
			Sea_FS_1kW  = S_E__i + (S_E__s - S_E__i) * LOG10(Tx_frequency/FLOAT(f_inf)) / &
				 LOG10(FLOAT(f_sup)/FLOAT(f_inf))
		END IF
	  ELSE
!		Case Heff =  0m - 10 m
!
		dh1  = 4.1 * SQRT(Heff)
		dh10 = 12.96534	! = 4.1 * SQRT(10.0)
		h10 = 10.0
!
		IF (Distance .LT. dh1) THEN
!			Case Heff =  0m - 10 m
!			Case Distance < dh1
!
!			Calculate E10d:
!			Get field strength for f_inf:
			CALL Get_figure_FS_value (f_inf, Time_percentage, Sea_temperature, &
									  h10, d_inf, L_E_ii, S_E_ii, Error)
			IF (Error .NE. 0) THEN
			  HCM_error = Error
			  RETURN
			END IF
			CALL Get_figure_FS_value (f_inf, Time_percentage, Sea_temperature, &
									  h10, d_sup, L_E_is, S_E_is, Error)
			IF (Error .NE. 0) THEN
			  HCM_error = Error
			  RETURN
			END IF
!			Get field strength for f_sup:
			CALL Get_figure_FS_value (f_sup, Time_percentage, Sea_temperature, &
									  h10, d_inf, L_E_si, S_E_si, Error)
			IF (Error .NE. 0) THEN
			  HCM_error = Error
			  RETURN
			END IF
			CALL Get_figure_FS_value (f_sup, Time_percentage, Sea_temperature, &
									  h10, d_sup, L_E_ss, S_E_ss, Error)
			IF (Error .NE. 0) THEN
			  HCM_error = Error
			  RETURN
			END IF
!			Interpolation of field strength as a function of the distance:
			IF (d_sup .EQ. d_inf) THEN
				L_E10d_i = L_E_ii
				S_E10d_i = S_E_ii
				L_E10d_s = L_E_si
				S_E10d_s = S_E_si
			  ELSE
				L_E10d_i = L_E_ii + (L_E_is - L_E_ii) * LOG10(Distance/FLOAT(d_inf)) / &
					   LOG10(FLOAT(d_sup)/FLOAT(d_inf)) 
				S_E10d_i = S_E_ii + (S_E_is - S_E_ii) * LOG10(Distance/FLOAT(d_inf)) / &
				 	   LOG10(FLOAT(d_sup)/FLOAT(d_inf)) 
				L_E10d_s = L_E_si + (L_E_ss - L_E_si) * LOG10(Distance/FLOAT(d_inf)) / &
					   LOG10(FLOAT(d_sup)/FLOAT(d_inf)) 
				S_E10d_s = S_E_si + (S_E_ss - S_E_si) * LOG10(Distance/FLOAT(d_inf)) / &
					   LOG10(FLOAT(d_sup)/FLOAT(d_inf)) 
			END IF
!
!			Inter- or extrapolation of field strength as a function of the frequency:
			IF (f_sup .EQ. f_inf) THEN
				L_E10d = L_E10d_i
				S_E10d = S_E10d_i
			  ELSE
				L_E10d = L_E10d_i + (L_E10d_s - L_E10d_i) * &
						LOG10(Tx_frequency/FLOAT(f_inf)) / &
						LOG10(FLOAT(f_sup)/FLOAT(f_inf))
				S_E10d = S_E10d_i + (S_E10d_s - S_E10d_i) * &
						LOG10(Tx_frequency/FLOAT(f_inf)) / &
						LOG10(FLOAT(f_sup)/FLOAT(f_inf))
			END IF
!
!			Calculate E10dh10:
!			Set d_inf and d_sup for dh10:
			d_inf = 12
			d_sup = 13
!			Get field strength for f_inf:
			CALL Get_figure_FS_value (f_inf, Time_percentage, Sea_temperature, &
									  h10, d_inf, L_E_ii, S_E_ii, Error)
			IF (Error .NE. 0) THEN
			  HCM_error = Error
			  RETURN
			END IF
			CALL Get_figure_FS_value (f_inf, Time_percentage, Sea_temperature, &
									  h10, d_sup, L_E_is, S_E_is, Error)
			IF (Error .NE. 0) THEN
			  HCM_error = Error
			  RETURN
			END IF
!			Get field strength for f_sup:
			CALL Get_figure_FS_value (f_sup, Time_percentage, Sea_temperature, &
									  h10, d_inf, L_E_si, S_E_si, Error)
			IF (Error .NE. 0) THEN
			  HCM_error = Error
			  RETURN
			END IF
			CALL Get_figure_FS_value (f_sup, Time_percentage, Sea_temperature, &
									  h10, d_sup, L_E_ss, S_E_ss, Error)
			IF (Error .NE. 0) THEN
			  HCM_error = Error
			  RETURN
			END IF
!			Interpolation of field strength as a function of the distance:
			IF (d_sup .EQ. d_inf) THEN
				L_E10dh10_i = L_E_ii
				S_E10dh10_i = S_E_ii
				L_E10dh10_s = L_E_si
				S_E10dh10_s = S_E_si
			  ELSE
				L_E10dh10_i = L_E_ii + (L_E_is - L_E_ii) * LOG10(dh10/FLOAT(d_inf)) / &
						  LOG10(FLOAT(d_sup)/FLOAT(d_inf)) 
				S_E10dh10_i = S_E_ii + (S_E_is - S_E_ii) * LOG10(dh10/FLOAT(d_inf)) / &
						  LOG10(FLOAT(d_sup)/FLOAT(d_inf)) 
				L_E10dh10_s = L_E_si + (L_E_ss - L_E_si) * LOG10(dh10/FLOAT(d_inf)) / &
						  LOG10(FLOAT(d_sup)/FLOAT(d_inf)) 
				S_E10dh10_s = S_E_si + (S_E_ss - S_E_si) * LOG10(dh10/FLOAT(d_inf)) / &
						  LOG10(FLOAT(d_sup)/FLOAT(d_inf))
			END IF 
!			Inter- or extrapolation of field strength as a function of the frequency:
			IF (f_sup .EQ. f_inf) THEN
				L_E10dh10 = L_E10dh10_i
				S_E10dh10 = S_E10dh10_i
			  ELSE
				L_E10dh10 = L_E10dh10_i + (L_E10dh10_s - L_E10dh10_i) * &
						LOG10(Tx_frequency/FLOAT(f_inf)) / &
						LOG10(FLOAT(f_sup)/FLOAT(f_inf))
				S_E10dh10 = S_E10dh10_i + (S_E10dh10_s - S_E10dh10_i) * &
						LOG10(Tx_frequency/FLOAT(f_inf)) / &
						LOG10(FLOAT(f_sup)/FLOAT(f_inf))
			END IF
!
!
!			Calculate E10dh1:
			IF (dh1 .LT. 1.0) THEN
!				Use free space field strength:
				L_E10dh1 = E_free_1kW
				S_E10dh1 = E_free_1kW
			  ELSE
!				Set d_inf and d_sup for dh1:
				DO I = 1, 77
				  IF (dh1 .GE. FLOAT(Distances(I))) THEN
				    d_inf = Distances(I)
					d_sup = Distances(I+1)
				  END IF
				END DO
!				Get field strength for f_inf:
				CALL Get_figure_FS_value (f_inf, Time_percentage, Sea_temperature, &
										  h10, d_inf, L_E_ii, S_E_ii, Error)
				IF (Error .NE. 0) THEN
				  HCM_error = Error
				  RETURN
				END IF
				CALL Get_figure_FS_value (f_inf, Time_percentage, Sea_temperature, &
										  h10, d_sup, L_E_is, S_E_is, Error)
				IF (Error .NE. 0) THEN
				  HCM_error = Error
				  RETURN
				END IF
!				Get field strength for f_sup:
				CALL Get_figure_FS_value (f_sup, Time_percentage, Sea_temperature, &
										  h10, d_inf, L_E_si, S_E_si, Error)
				IF (Error .NE. 0) THEN
				  HCM_error = Error
				  RETURN
				END IF
				CALL Get_figure_FS_value (f_sup, Time_percentage, Sea_temperature, &
										  h10, d_sup, L_E_ss, S_E_ss, Error)
				IF (Error .NE. 0) THEN
				  HCM_error = Error
				  RETURN
				END IF
!				Interpolation of field strength as a function of the distance:
				IF (d_sup .EQ. d_inf) THEN
					L_E10dh1_i = L_E_ii
					S_E10dh1_i = S_E_ii
					L_E10dh1_s = L_E_si
					S_E10dh1_s = S_E_si
				  ELSE
					L_E10dh1_i = L_E_ii + (L_E_is - L_E_ii) * LOG10(dh10/FLOAT(d_inf)) / &
							  LOG10(FLOAT(d_sup)/FLOAT(d_inf)) 
					S_E10dh1_i = S_E_ii + (S_E_is - S_E_ii) * LOG10(dh10/FLOAT(d_inf)) / &
							  LOG10(FLOAT(d_sup)/FLOAT(d_inf)) 
					L_E10dh1_s = L_E_si + (L_E_ss - L_E_si) * LOG10(dh10/FLOAT(d_inf)) / &
							  LOG10(FLOAT(d_sup)/FLOAT(d_inf)) 
					S_E10dh1_s = S_E_si + (S_E_ss - S_E_si) * LOG10(dh10/FLOAT(d_inf)) / &
							  LOG10(FLOAT(d_sup)/FLOAT(d_inf)) 
				END IF
!				Inter- or extrapolation of field strength as a function of the frequency:
				IF (f_sup .EQ. f_inf) THEN
					L_E10dh1 = L_E10dh1_i
					S_E10dh1 = S_E10dh1_i
				  ELSE
					L_E10dh1 = L_E10dh1_i + (L_E10dh1_s - L_E10dh1_i) * &
							LOG10(Tx_frequency/FLOAT(f_inf)) / &
							LOG10(FLOAT(f_sup)/FLOAT(f_inf))
					S_E10dh1 = S_E10dh1_i + (S_E10dh1_s - S_E10dh1_i) * &
							LOG10(Tx_frequency/FLOAT(f_inf)) / &
							LOG10(FLOAT(f_sup)/FLOAT(f_inf))
				END IF
			END IF
			Land_FS_1kW = L_E10dh10 + L_E10d - L_E10dh1
			Sea_FS_1kW  = S_E10dh10 + S_E10d - S_E10dh1
		  ELSE
!			Case Heff =  0m - 10 m
!			Case Distance >= dh1
!
!			Calculate E10dhx:
			dhx = dh10 + Distance - dh1
			IF (dhx .LT. 1.0) THEN
!				Use free space field strength:
				L_E10dhx = E_free_1kW
				S_E10dhx = E_free_1kW
			  ELSE
!				Set d_inf and d_sup for dhx:
				DO I = 1, 77
				  IF (dhx .GE. FLOAT(Distances(I))) THEN
				    d_inf = Distances(I)
					d_sup = Distances(I+1)
				  END IF
				END DO
!				Get field strength for f_inf:
				CALL Get_figure_FS_value (f_inf, Time_percentage, Sea_temperature, &
										  h10, d_inf, L_E_ii, S_E_ii, Error)
				IF (Error .NE. 0) THEN
				  HCM_error = Error
				  RETURN
				END IF
				CALL Get_figure_FS_value (f_inf, Time_percentage, Sea_temperature, &
										  h10, d_sup, L_E_is, S_E_is, Error)
				IF (Error .NE. 0) THEN
				  HCM_error = Error
				  RETURN
				END IF
!				Get field strength for f_sup:
				CALL Get_figure_FS_value (f_sup, Time_percentage, Sea_temperature, &
										  h10, d_inf, L_E_si, S_E_si, Error)
				IF (Error .NE. 0) THEN
				  HCM_error = Error
				  RETURN
				END IF
				CALL Get_figure_FS_value (f_sup, Time_percentage, Sea_temperature, &
										  h10, d_sup, L_E_ss, S_E_ss, Error)
				IF (Error .NE. 0) THEN
				  HCM_error = Error
				  RETURN
				END IF
!				Interpolation of field strength as a function of the distance:
				IF (d_sup .EQ. d_inf) THEN
					L_E10dhx_i = L_E_ii
					S_E10dhx_i = S_E_ii
					L_E10dhx_s = L_E_si
					S_E10dhx_s = S_E_si
				  ELSE
					L_E10dhx_i = L_E_ii + (L_E_is - L_E_ii) * LOG10(dhx/FLOAT(d_inf)) / &
							  LOG10(FLOAT(d_sup)/FLOAT(d_inf)) 
					S_E10dhx_i = S_E_ii + (S_E_is - S_E_ii) * LOG10(dhx/FLOAT(d_inf)) / &
							  LOG10(FLOAT(d_sup)/FLOAT(d_inf)) 
					L_E10dhx_s = L_E_si + (L_E_ss - L_E_si) * LOG10(dhx/FLOAT(d_inf)) / &
							  LOG10(FLOAT(d_sup)/FLOAT(d_inf)) 
					S_E10dhx_s = S_E_si + (S_E_ss - S_E_si) * LOG10(dhx/FLOAT(d_inf)) / &
							  LOG10(FLOAT(d_sup)/FLOAT(d_inf)) 
				END IF
!				Inter- or extrapolation of field strength as a function of the frequency:
				IF (f_sup .EQ. f_inf) THEN
					Land_FS_1kW = L_E10dhx_i
					Sea_FS_1kW = S_E10dhx_i
				  ELSE
					Land_FS_1kW = L_E10dhx_i + (L_E10dhx_s - L_E10dhx_i) * &
							LOG10(Tx_frequency/FLOAT(f_inf)) / &
							LOG10(FLOAT(f_sup)/FLOAT(f_inf))
					Sea_FS_1kW = S_E10dhx_i + (S_E10dhx_s - S_E10dhx_i) * &
							LOG10(Tx_frequency/FLOAT(f_inf)) / &
							LOG10(FLOAT(f_sup)/FLOAT(f_inf))
				END IF
!
			END IF	! IF (dhx .LT. 1.0) THEN
!
		END IF		! IF (Distance .LT. dh1) THEN
!
	END IF			! IF (Heff .GE. 10.0) THEN	
!
	RETURN
!
	END SUBROUTINE Get_FS_from_figures
!
!	*****************************************************************************************
!