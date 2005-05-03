!
!	HCM_MS_V7_definitions.f90
!
	INTEGER*2			T_Prof(10002), M_Prof(10002), PN, H_Datab_Tx, H_Datab_Rx
	INTEGER*2			T_L, M_L, B_L
	INTEGER*4			HCM_error, Time_percentage, C_mode, D_to_border
	INTEGER*4			H_AntTx, H_AntRx, H_Tx, H_Rx
	REAL				Heff, TCA, TCA_corr
	REAL				Dh, Dh_corr, Calculated_FS, Perm_FS, Prot_margin
	REAL				ERP_ref_Tx, CBR_D, Tx_TCA, Rx_TCA, Tx_TCA_corr, Rx_TCA_corr
	REAL				Heff_Tx, Heff_Rx, Land_FS, Sea_FS
	REAL				Tx_ant_corr, Rx_ant_corr, Tx_ant_type_corr, Rx_ant_type_corr
	REAL				Perm_FS_from_table, Corr_delta_f, Channel_sp_Rx, Channel_sp_Tx
	REAL				Power_to_Rx, Free_space_FS, Tx_serv_area, Rx_serv_area
	REAL				Tx_Azimuth, Tx_Elevation, Rx_Azimuth, Rx_Elevation, MaxPow
	DOUBLE PRECISION	Tx_frequency, Distance, PD
	DOUBLE PRECISION	D_sea_calculated, Dir_Tx_Rx, Dir_Rx_Tx, Rx_frequency
	DOUBLE PRECISION	V_angle_Tx_Rx, V_angle_Rx_Tx, H_diff_angle_Tx_Rx, V_diff_angle_Tx_Rx
	DOUBLE PRECISION	H_diff_angle_Rx_Tx, V_diff_angle_Rx_Tx, Delta_frequency
	CHARACTER*1			Sea_temperature, Type_of_Tx_ant, Type_of_Rx_ant, Chan_occup
	CHARACTER*3			Land_from, Land_to, Max_CBR_D_input
	CHARACTER*4			H_Tx_input, H_Rx_input, H_Tx_ant, H_Rx_ant, Rx_ant_gain
	CHARACTER*4			Depol_loss, Cor_fact_frequ_diff
	CHARACTER*5			Azi_Tx_input, Ele_Tx_input, Azi_Rx_input, Ele_Rx_input
	CHARACTER*5			D_sea_input, Rad_of_Tx_serv_area, Rad_of_Rx_serv_area
	CHARACTER*5			Perm_FS_input, Version
	CHARACTER*6			Max_power
	CHARACTER*7			Ant_typ_H_Tx, Ant_typ_V_Tx, Ant_typ_H_Rx, Ant_typ_V_Rx
	CHARACTER*9			Desig_of_Tx_emis, Desig_of_Rx_emis
	CHARACTER*12		Tx_frequ, Rx_frequ
	CHARACTER*15		Coo_Tx, Coo_Rx, Coo_Tx_new, Coo_Rx_new
	CHARACTER*63		Topo_path, Morpho_path, Border_path
	LOGICAL*4			Info(20)
!
!						First all 8 byte variables (DOUBLE PRECISION)
	COMMON	/NDATA/		Tx_frequency, Distance, PD, &
						Dir_Tx_Rx, Dir_Rx_Tx, D_sea_calculated, V_angle_Tx_Rx, &
						V_angle_Rx_Tx, H_diff_angle_Tx_Rx, V_diff_angle_Tx_Rx, &
						H_diff_angle_Rx_Tx, V_diff_angle_Rx_Tx, Delta_frequency, &
						Rx_frequency, &
!
!						Second all 4 byte variables (REAL, INTEGER, LOGICAL)
						D_to_border, HCM_error, Time_percentage,  &
						TCA, TCA_corr, Dh, Dh_corr, C_mode, H_AntTx, &
						Calculated_FS, Perm_FS, Prot_margin, ERP_ref_Tx, CBR_D, &
						Tx_TCA, Rx_TCA, Tx_TCA_corr, Rx_TCA_corr, Heff_Tx, &
						Heff_Rx, Land_FS, Sea_FS, Info, Tx_ant_corr, Rx_ant_corr, &
						Tx_ant_type_corr, Rx_ant_type_corr, Perm_FS_from_table, &
						Corr_delta_f, Channel_sp_Rx, Channel_sp_Tx, Power_to_Rx, &
						Free_space_FS, H_AntRx, H_Tx, H_Rx, Tx_serv_area, Heff, &
						Rx_serv_area, Tx_Azimuth, Tx_Elevation, Rx_Azimuth, &
						Rx_Elevation, MaxPow, &
!
!						Third all 2 byte variables (INTEGER*2)
						T_Prof, M_Prof, PN, H_Datab_Tx, H_Datab_Rx, T_L, M_L, B_L
!
!						All CHARACTER variables
	COMMON	/CDATA/		Topo_path, Morpho_path, Border_path, Sea_temperature, &
						Coo_Tx, Coo_Rx, H_Tx_input, H_Rx_input, Ant_typ_H_Tx, &
						Ant_typ_V_Tx, Ant_typ_H_Rx, Ant_typ_V_Rx, Azi_Tx_input, &
						Ele_Tx_input, Azi_Rx_input, Ele_Rx_input, H_Tx_ant, &
						H_Rx_ant, Max_power, Type_of_Tx_ant, Type_of_Rx_ant, &
						Tx_frequ, Rx_frequ, Chan_occup, D_sea_input, Land_to, &
						Rad_of_Tx_serv_area, Rad_of_Rx_serv_area, Land_from, &
						Perm_FS_input, Desig_of_Tx_emis, Desig_of_Rx_emis, &
						Rx_ant_gain, Depol_loss, Cor_fact_frequ_diff, &
						Max_CBR_D_input, Version, Coo_Tx_new, Coo_Rx_new
!
