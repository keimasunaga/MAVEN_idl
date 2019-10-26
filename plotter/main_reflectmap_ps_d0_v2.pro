pro main_reflectmap_ps_d0_v2
    
    env = init_env()
    SAVE_LOC = env.SAVE_LOC
    
;    ;;all data
;    popen, SAVE_LOC+'/maven/ps/random/map_gt5keV_out_d0_rmv_plm.ps'
;    plot_reflect_ion_map10_v2,'gt5keV',vdf_type='shock_normal',surf_type='shock_surface',/make_notes,journal_file='map_gt5keV_out_d0_rmv_plm'
;    pclose
;
;    popen, SAVE_LOC+'/maven/ps/random/map_gt5keV_in_d0_rmv_plm.ps'
;    plot_reflect_ion_map10_v2,'gt5keV_in',vdf_type='shock_normal',surf_type='shock_surface',/make_notes,journal_file='map_gt5keV_in_d0_rmv_plm'
;    pclose
;
    popen, SAVE_LOC+'/maven/ps/random/map_gt10keV_out_d0_rmv_plm.ps'
    plot_reflect_ion_map10_v2,'gt10keV',vdf_type='shock_normal',surf_type='shock_surface',/make_notes,journal_file='map_gt10keV_out_d0_rmv_plm'
    pclose

    popen, SAVE_LOC+'/maven/ps/random/map_gt10keV_in_d0_rmv_plm.ps'
    plot_reflect_ion_map10_v2,'gt10keV_in',vdf_type='shock_normal',surf_type='shock_surface',/make_notes,journal_file='map_gt10keV_in_d0_rmv_plm'
    pclose
;
;    popen, SAVE_LOC+'/maven/ps/random/map_20keV_out_d0_rmv_plm.ps'
;    plot_reflect_ion_map10_v2,'20keV',vdf_type='shock_normal',surf_type='shock_surface',/make_notes,journal_file='map_20keV_out_d0_rmv_plm'
;    pclose
;
;    popen, SAVE_LOC+'/maven/ps/random/map_20keV_in_d0_rmv_plm.ps'
;    plot_reflect_ion_map10_v2,'20keV_in',vdf_type='shock_normal',surf_type='shock_surface',/make_notes,journal_file='map_20keV_in_d0_rmv_plm'
;    pclose
;    
;    
;    
;    
;    ;;large gyro radii
;    popen, SAVE_LOC+'/maven/ps/random/map_gt5keV_large_rg_out_d0_rmv_plm.ps'
;    plot_reflect_ion_map10_v2,'gt5keV',vdf_type='shock_normal',surf_type='shock_surface',/rg_mean,/large,/make_notes,journal_file='map_gt5keV_large_rg_out_d0_rmv_plm'
;    pclose
;
;    popen, SAVE_LOC+'/maven/ps/random/map_gt5keV_large_rg_in_d0_rmv_plm.ps'
;    plot_reflect_ion_map10_v2,'gt5keV_in',vdf_type='shock_normal',surf_type='shock_surface',/rg_mean,/large,/make_notes,journal_file='map_gt5keV_large_rg_in_d0_rmv_plm'
;    pclose
;    
;    popen, SAVE_LOC+'/maven/ps/random/map_gt10keV_large_rg_out_d0_rmv_plm.ps'
;    plot_reflect_ion_map10_v2,'gt10keV',vdf_type='shock_normal',surf_type='shock_surface',/rg_mean,/large,/make_notes,journal_file='map_gt10keV_large_rg_out_d0_rmv_plm'
;    pclose
;    
;    popen, SAVE_LOC+'/maven/ps/random/map_gt10keV_large_rg_in_d0_rmv_plm.ps'
;    plot_reflect_ion_map10_v2,'gt10keV_in',vdf_type='shock_normal',surf_type='shock_surface',/rg_mean,/large,/make_notes,journal_file='map_gt10keV_large_rg_in_d0_rmv_plm'
;    pclose
;    
;    popen, SAVE_LOC+'/maven/ps/random/map_20keV_large_rg_out_d0_rmv_plm.ps'
;    plot_reflect_ion_map10_v2,'20keV',vdf_type='shock_normal',surf_type='shock_surface',/rg_mean,/large,/make_notes,journal_file='map_20keV_large_rg_out_d0_rmv_plm'
;    pclose
;
;    popen, SAVE_LOC+'/maven/ps/random/map_20keV_large_rg_in_d0_rmv_plm.ps'
;    plot_reflect_ion_map10_v2,'20keV_in',vdf_type='shock_normal',surf_type='shock_surface',/rg_mean,/large,/make_notes,journal_file='map_20keV_large_rg_in_d0_rmv_plm'
;    pclose
;    
;    
;    ;;small gyro radii
;    popen, SAVE_LOC+'/maven/ps/random/map_gt5keV_small_rg_out_d0_rmv_plm.ps'
;    plot_reflect_ion_map10_v2,'gt5keV',vdf_type='shock_normal',surf_type='shock_surface',/rg_mean,/small,/make_notes,journal_file='map_gt5keV_small_rg_out_d0_rmv_plm'
;    pclose
;
;    popen, SAVE_LOC+'/maven/ps/random/map_gt5keV_small_rg_in_d0_rmv_plm.ps'
;    plot_reflect_ion_map10_v2,'gt5keV_in',vdf_type='shock_normal',surf_type='shock_surface',/rg_mean,/small,/make_notes,journal_file='map_gt5keV_small_rg_in_d0_rmv_plm'
;    pclose
;
;    popen, SAVE_LOC+'/maven/ps/random/map_gt10keV_small_rg_out_d0_rmv_plm.ps'
;    plot_reflect_ion_map10_v2,'gt10keV',vdf_type='shock_normal',surf_type='shock_surface',/rg_mean,/small,/make_notes,journal_file='map_gt10keV_small_rg_out_d0_rmv_plm'
;    pclose
;
;    popen, SAVE_LOC+'/maven/ps/random/map_gt10keV_small_rg_in_d0_rmv_plm.ps'
;    plot_reflect_ion_map10_v2,'gt10keV_in',vdf_type='shock_normal',surf_type='shock_surface',/rg_mean,/small,/make_notes,journal_file='map_gt10keV_small_rg_in_d0_rmv_plm'
;    pclose
;
;    popen, SAVE_LOC+'/maven/ps/random/map_20keV_small_rg_out_d0_rmv_plm.ps'
;    plot_reflect_ion_map10_v2,'20keV',vdf_type='shock_normal',surf_type='shock_surface',/rg_mean,/small,/make_notes,journal_file='map_20keV_small_rg_out_d0_rmv_plm'
;    pclose
;
;    popen, SAVE_LOC+'/maven/ps/random/map_20keV_small_rg_in_d0_rmv_plm.ps'
;    plot_reflect_ion_map10_v2,'20keV_in',vdf_type='shock_normal',surf_type='shock_surface',/rg_mean,/small,/make_notes,journal_file='map_20keV_small_rg_in_d0_rmv_plm'
;    pclose



;;large vperp
;popen, SAVE_LOC+'/maven/ps/random/map_gt5keV_large_vperp_out_d0_rmv_plm.ps'
;plot_reflect_ion_map10_v2,'gt5keV',vdf_type='shock_normal',surf_type='shock_surface',/vmso_perp_mean,/large,/make_notes,journal_file='map_gt5keV_large_vperp_out_d0_rmv_plm'
;pclose

;popen, SAVE_LOC+'/maven/ps/random/map_gt5keV_large_vperp_in_d0_rmv_plm.ps'
;plot_reflect_ion_map10_v2,'gt5keV_in',vdf_type='shock_normal',surf_type='shock_surface',/vmso_perp_mean,/large,/make_notes,journal_file='map_gt5keV_large_vperp_in_d0_rmv_plm'
;pclose

;popen, SAVE_LOC+'/maven/ps/random/map_gt10keV_large_vperp_out_d0_rmv_plm.ps'
;plot_reflect_ion_map10_v2,'gt10keV',vdf_type='shock_normal',surf_type='shock_surface',/vmso_perp_mean,/large,/make_notes,journal_file='map_gt10keV_large_vperp_out_d0_rmv_plm'
;pclose
;
;popen, SAVE_LOC+'/maven/ps/random/map_gt10keV_large_vperp_in_d0_rmv_plm.ps'
;plot_reflect_ion_map10_v2,'gt10keV_in',vdf_type='shock_normal',surf_type='shock_surface',/vmso_perp_mean,/large,/make_notes,journal_file='map_gt10keV_large_vperp_in_d0_rmv_plm'
;pclose
;
;popen, SAVE_LOC+'/maven/ps/random/map_20keV_large_vperp_out_d0_rmv_plm.ps'
;plot_reflect_ion_map10_v2,'20keV',vdf_type='shock_normal',surf_type='shock_surface',/vmso_perp_mean,/large,/make_notes,journal_file='map_20keV_large_vperp_out_d0_rmv_plm'
;pclose
;
;popen, SAVE_LOC+'/maven/ps/random/map_20keV_large_vperp_in_d0_rmv_plm.ps'
;plot_reflect_ion_map10_v2,'20keV_in',vdf_type='shock_normal',surf_type='shock_surface',/vmso_perp_mean,/large,/make_notes,journal_file='map_20keV_large_vperp_in_d0_rmv_plm'
;pclose
;
;
;;;small vperp
;popen, SAVE_LOC+'/maven/ps/random/map_gt5keV_small_vperp_out_d0_rmv_plm.ps'
;plot_reflect_ion_map10_v2,'gt5keV',vdf_type='shock_normal',surf_type='shock_surface',/vmso_perp_mean,/small,/make_notes,journal_file='map_gt5keV_small_vperp_out_d0_rmv_plm'
;pclose
;
;popen, SAVE_LOC+'/maven/ps/random/map_gt5keV_small_vperp_in_d0_rmv_plm.ps'
;plot_reflect_ion_map10_v2,'gt5keV_in',vdf_type='shock_normal',surf_type='shock_surface',/vmso_perp_mean,/small,/make_notes,journal_file='map_gt5keV_small_vperp_in_d0_rmv_plm'
;pclose
;
;popen, SAVE_LOC+'/maven/ps/random/map_gt10keV_small_vperp_out_d0_rmv_plm.ps'
;plot_reflect_ion_map10_v2,'gt10keV',vdf_type='shock_normal',surf_type='shock_surface',/vmso_perp_mean,/small,/make_notes,journal_file='map_gt10keV_small_vperp_out_d0_rmv_plm'
;pclose
;
;popen, SAVE_LOC+'/maven/ps/random/map_gt10keV_small_vperp_in_d0_rmv_plm.ps'
;plot_reflect_ion_map10_v2,'gt10keV_in',vdf_type='shock_normal',surf_type='shock_surface',/vmso_perp_mean,/small,/make_notes,journal_file='map_gt10keV_small_vperp_in_d0_rmv_plm'
;pclose
;
;popen, SAVE_LOC+'/maven/ps/random/map_20keV_small_vperp_out_d0_rmv_plm.ps'
;plot_reflect_ion_map10_v2,'20keV',vdf_type='shock_normal',surf_type='shock_surface',/vmso_perp_mean,/small,/make_notes,journal_file='map_20keV_small_vperp_out_d0_rmv_plm'
;pclose
;
;popen, SAVE_LOC+'/maven/ps/random/map_20keV_small_vperp_in_d0_rmv_plm.ps'
;plot_reflect_ion_map10_v2,'20keV_in',vdf_type='shock_normal',surf_type='shock_surface',/vmso_perp_mean,/small,/make_notes,journal_file='map_20keV_small_vperp_in_d0_rmv_plm'
;pclose
;











;;;large Bt
;popen, SAVE_LOC+'/maven/ps/random/map_gt5keV_large_Bt_out_d0_rmv_plm.ps'
;plot_reflect_ion_map10_v2,'gt5keV',vdf_type='shock_normal',surf_type='shock_surface',/Bt_mean,/large,/make_notes,journal_file='map_gt5keV_large_Bt_out_d0_rmv_plm'
;pclose
;
;popen, SAVE_LOC+'/maven/ps/random/map_gt5keV_large_Bt_in_d0_rmv_plm.ps'
;plot_reflect_ion_map10_v2,'gt5keV_in',vdf_type='shock_normal',surf_type='shock_surface',/Bt_mean,/large,/make_notes,journal_file='map_gt5keV_large_Bt_in_d0_rmv_plm'
;pclose
;
;popen, SAVE_LOC+'/maven/ps/random/map_gt10keV_large_Bt_out_d0_rmv_plm.ps'
;plot_reflect_ion_map10_v2,'gt10keV',vdf_type='shock_normal',surf_type='shock_surface',/Bt_mean,/large,/make_notes,journal_file='map_gt10keV_large_Bt_out_d0_rmv_plm'
;pclose
;
;popen, SAVE_LOC+'/maven/ps/random/map_gt10keV_large_Bt_in_d0_rmv_plm.ps'
;plot_reflect_ion_map10_v2,'gt10keV_in',vdf_type='shock_normal',surf_type='shock_surface',/Bt_mean,/large,/make_notes,journal_file='map_gt10keV_large_Bt_in_d0_rmv_plm'
;pclose
;
;popen, SAVE_LOC+'/maven/ps/random/map_20keV_large_Bt_out_d0_rmv_plm.ps'
;plot_reflect_ion_map10_v2,'20keV',vdf_type='shock_normal',surf_type='shock_surface',/Bt_mean,/large,/make_notes,journal_file='map_20keV_large_Bt_out_d0_rmv_plm'
;pclose
;
;popen, SAVE_LOC+'/maven/ps/random/map_20keV_large_Bt_in_d0_rmv_plm.ps'
;plot_reflect_ion_map10_v2,'20keV_in',vdf_type='shock_normal',surf_type='shock_surface',/Bt_mean,/large,/make_notes,journal_file='map_20keV_large_Bt_in_d0_rmv_plm'
;pclose
;
;
;;;small Bt
;popen, SAVE_LOC+'/maven/ps/random/map_gt5keV_small_Bt_out_d0_rmv_plm.ps'
;plot_reflect_ion_map10_v2,'gt5keV',vdf_type='shock_normal',surf_type='shock_surface',/Bt_mean,/small,/make_notes,journal_file='map_gt5keV_small_Bt_out_d0_rmv_plm'
;pclose
;
;popen, SAVE_LOC+'/maven/ps/random/map_gt5keV_small_Bt_in_d0_rmv_plm.ps'
;plot_reflect_ion_map10_v2,'gt5keV_in',vdf_type='shock_normal',surf_type='shock_surface',/Bt_mean,/small,/make_notes,journal_file='map_gt5keV_small_Bt_in_d0_rmv_plm'
;pclose
;
;popen, SAVE_LOC+'/maven/ps/random/map_gt10keV_small_Bt_out_d0_rmv_plm.ps'
;plot_reflect_ion_map10_v2,'gt10keV',vdf_type='shock_normal',surf_type='shock_surface',/Bt_mean,/small,/make_notes,journal_file='map_gt10keV_small_Bt_out_d0_rmv_plm'
;pclose
;
;popen, SAVE_LOC+'/maven/ps/random/map_gt10keV_small_Bt_in_d0_rmv_plm.ps'
;plot_reflect_ion_map10_v2,'gt10keV_in',vdf_type='shock_normal',surf_type='shock_surface',/Bt_mean,/small,/make_notes,journal_file='map_gt10keV_small_Bt_in_d0_rmv_plm'
;pclose
;
;popen, SAVE_LOC+'/maven/ps/random/map_20keV_small_Bt_out_d0_rmv_plm.ps'
;plot_reflect_ion_map10_v2,'20keV',vdf_type='shock_normal',surf_type='shock_surface',/Bt_mean,/small,/make_notes,journal_file='map_20keV_small_Bt_out_d0_rmv_plm'
;pclose
;
;popen, SAVE_LOC+'/maven/ps/random/map_20keV_small_Bt_in_d0_rmv_plm.ps'
;plot_reflect_ion_map10_v2,'20keV_in',vdf_type='shock_normal',surf_type='shock_surface',/Bt_mean,/small,/make_notes,journal_file='map_20keV_small_Bt_in_d0_rmv_plm'
;pclose
;




;;;large Pdyn
;popen, SAVE_LOC+'/maven/ps/random/map_gt5keV_large_Pdyn_out_d0_rmv_plm.ps'
;plot_reflect_ion_map10_v2,'gt5keV',vdf_type='shock_normal',surf_type='shock_surface',/Pdyn_mean,/large,/make_notes,journal_file='map_gt5keV_large_Pdyn_out_d0_rmv_plm'
;pclose
;
;popen, SAVE_LOC+'/maven/ps/random/map_gt5keV_large_Pdyn_in_d0_rmv_plm.ps'
;plot_reflect_ion_map10_v2,'gt5keV_in',vdf_type='shock_normal',surf_type='shock_surface',/Pdyn_mean,/large,/make_notes,journal_file='map_gt5keV_large_Pdyn_in_d0_rmv_plm'
;pclose
;
;popen, SAVE_LOC+'/maven/ps/random/map_gt10keV_large_Pdyn_out_d0_rmv_plm.ps'
;plot_reflect_ion_map10_v2,'gt10keV',vdf_type='shock_normal',surf_type='shock_surface',/Pdyn_mean,/large,/make_notes,journal_file='map_gt10keV_large_Pdyn_out_d0_rmv_plm'
;pclose
;
;popen, SAVE_LOC+'/maven/ps/random/map_gt10keV_large_Pdyn_in_d0_rmv_plm.ps'
;plot_reflect_ion_map10_v2,'gt10keV_in',vdf_type='shock_normal',surf_type='shock_surface',/Pdyn_mean,/large,/make_notes,journal_file='map_gt10keV_large_Pdyn_in_d0_rmv_plm'
;pclose
;
;popen, SAVE_LOC+'/maven/ps/random/map_20keV_large_Pdyn_out_d0_rmv_plm.ps'
;plot_reflect_ion_map10_v2,'20keV',vdf_type='shock_normal',surf_type='shock_surface',/Pdyn_mean,/large,/make_notes,journal_file='map_20keV_large_Pdyn_out_d0_rmv_plm'
;pclose
;
;popen, SAVE_LOC+'/maven/ps/random/map_20keV_large_Pdyn_in_d0_rmv_plm.ps'
;plot_reflect_ion_map10_v2,'20keV_in',vdf_type='shock_normal',surf_type='shock_surface',/Pdyn_mean,/large,/make_notes,journal_file='map_20keV_large_Pdyn_in_d0_rmv_plm'
;pclose
;
;
;;;small Pdyn
;popen, SAVE_LOC+'/maven/ps/random/map_gt5keV_small_Pdyn_out_d0_rmv_plm.ps'
;plot_reflect_ion_map10_v2,'gt5keV',vdf_type='shock_normal',surf_type='shock_surface',/Pdyn_mean,/small,/make_notes,journal_file='map_gt5keV_small_Pdyn_out_d0_rmv_plm'
;pclose
;
;popen, SAVE_LOC+'/maven/ps/random/map_gt5keV_small_Pdyn_in_d0_rmv_plm.ps'
;plot_reflect_ion_map10_v2,'gt5keV_in',vdf_type='shock_normal',surf_type='shock_surface',/Pdyn_mean,/small,/make_notes,journal_file='map_gt5keV_small_Pdyn_in_d0_rmv_plm'
;pclose
;
;popen, SAVE_LOC+'/maven/ps/random/map_gt10keV_small_Pdyn_out_d0_rmv_plm.ps'
;plot_reflect_ion_map10_v2,'gt10keV',vdf_type='shock_normal',surf_type='shock_surface',/Pdyn_mean,/small,/make_notes,journal_file='map_gt10keV_small_Pdyn_out_d0_rmv_plm'
;pclose
;
;popen, SAVE_LOC+'/maven/ps/random/map_gt10keV_small_Pdyn_in_d0_rmv_plm.ps'
;plot_reflect_ion_map10_v2,'gt10keV_in',vdf_type='shock_normal',surf_type='shock_surface',/Pdyn_mean,/small,/make_notes,journal_file='map_gt10keV_small_Pdyn_in_d0_rmv_plm'
;pclose
;
;popen, SAVE_LOC+'/maven/ps/random/map_20keV_small_Pdyn_out_d0_rmv_plm.ps'
;plot_reflect_ion_map10_v2,'20keV',vdf_type='shock_normal',surf_type='shock_surface',/Pdyn_mean,/small,/make_notes,journal_file='map_20keV_small_Pdyn_out_d0_rmv_plm'
;pclose
;
;popen, SAVE_LOC+'/maven/ps/random/map_20keV_small_Pdyn_in_d0_rmv_plm.ps'
;plot_reflect_ion_map10_v2,'20keV_in',vdf_type='shock_normal',surf_type='shock_surface',/Pdyn_mean,/small,/make_notes,journal_file='map_20keV_small_Pdyn_in_d0_rmv_plm'
;pclose


end