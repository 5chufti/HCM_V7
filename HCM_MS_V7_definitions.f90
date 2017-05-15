!
!	HCM_MS_V7_definitions.f90							G.H. 13.05.2017
!
	INTEGER*2			H_I(10201), M_I(10201)
	INTEGER*2			T_Prof(10002), M_Prof(10002), H_Datab_Tx, H_Datab_Rx, H_Tx, H_Rx
	INTEGER*4			PN, OLD_R, O_LOD, O_LAD, RESH
	INTEGER*4			HCM_error, Time_percentage, C_mode, D_to_border
	INTEGER*4			H_AntTx, H_AntRx, Channel_sp_Rx, Channel_sp_Tx
	INTEGER*8			Delta_frequency
!
	REAL				Heff, TCA, TCA_corr, aLatM
	REAL				Dh, Dh_corr, Calculated_FS, Perm_FS, Prot_margin
	REAL				ERP_ref_Tx, CBR_D, Tx_TCA, Rx_TCA, Tx_TCA_corr, Rx_TCA_corr
	REAL				Heff_Tx, Heff_Rx, Land_FS, Sea_FS
	REAL				Tx_ant_corr, Rx_ant_corr, Tx_ant_type_corr, Rx_ant_type_corr
	REAL				Perm_FS_from_table, Corr_delta_f
	REAL				Power_to_Rx, Free_space_FS, Tx_serv_area, Rx_serv_area, MaxPow
	REAL				H_diff_angle_Rx_Tx, V_diff_angle_Rx_Tx, H_diff_angle_Tx_Rx, V_diff_angle_Tx_Rx
!
	DOUBLE PRECISION	Tx_frequency, Distance, PD
	DOUBLE PRECISION	Dir_Tx_Rx, Dir_Rx_Tx, D_sea_calculated, V_angle_Tx_Rx
	DOUBLE PRECISION	V_angle_Rx_Tx
	DOUBLE PRECISION	Rx_frequency, Tx_Azimuth, Tx_Elevation, Rx_Azimuth
	DOUBLE PRECISION	Rx_Elevation
!
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
!
	LOGICAL*4			Info(20), p2p, with_morpho, flp
!
!						First all 8 byte variables (DOUBLE PRECISION, LONG INTEGER)
	COMMON	/NDATA/		Tx_frequency, Distance, PD, &
						Dir_Tx_Rx, Dir_Rx_Tx, D_sea_calculated, V_angle_Tx_Rx, &
						V_angle_Rx_Tx, &
						Rx_frequency, Tx_Azimuth, Tx_Elevation, Rx_Azimuth, &
						Rx_Elevation, Delta_frequency, &
!
!						Second all 4 byte variables (REAL, INTEGER, LOGICAL)
						Heff, TCA, TCA_corr, aLatM, &
						Dh, Dh_corr, Calculated_FS, Perm_FS, Prot_margin, &
						ERP_ref_Tx, CBR_D, Tx_TCA, Rx_TCA, Tx_TCA_corr, Rx_TCA_corr, &
						Heff_Tx, Heff_Rx, Land_FS, Sea_FS, &
						Tx_ant_corr, Rx_ant_corr, Tx_ant_type_corr, Rx_ant_type_corr, &
						Perm_FS_from_table, Corr_delta_f, &
						Power_to_Rx, Free_space_FS, Tx_serv_area, Rx_serv_area, MaxPow, &
						H_diff_angle_Rx_Tx, V_diff_angle_Rx_Tx, H_diff_angle_Tx_Rx, V_diff_angle_Tx_Rx, &
!
						HCM_error, Time_percentage, C_mode, D_to_border, &
						OLD_R, O_LOD, O_LAD, PN,  RESH, &
						H_AntTx, H_AntRx,  Channel_sp_Rx, Channel_sp_Tx, &
!
						Info, p2p, with_morpho, flp, &
!
!						Third all 2 byte variables (INTEGER*2)
						T_Prof, M_Prof, H_Datab_Tx, H_Datab_Rx, H_Tx, H_Rx, &
						H_I, M_I
!
!						All CHARACTER variables
	COMMON	/CDATA/		Sea_temperature, Type_of_Tx_ant, Type_of_Rx_ant, Chan_occup, &
						Land_from, Land_to, Max_CBR_D_input, &
						H_Tx_input, H_Rx_input, H_Tx_ant, H_Rx_ant, Rx_ant_gain, &
						Depol_loss, Cor_fact_frequ_diff, &
						Azi_Tx_input, Ele_Tx_input, Azi_Rx_input, Ele_Rx_input, &
						D_sea_input, Rad_of_Tx_serv_area, Rad_of_Rx_serv_area, &
						Perm_FS_input, Version, &
						Max_power, &
						Ant_typ_H_Tx, Ant_typ_V_Tx, Ant_typ_H_Rx, Ant_typ_V_Rx, &
						Desig_of_Tx_emis, Desig_of_Rx_emis, &
						Tx_frequ, Rx_frequ, &
						Coo_Tx, Coo_Rx, Coo_Tx_new, Coo_Rx_new, &
						Topo_path, Morpho_path, Border_path
