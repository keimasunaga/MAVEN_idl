pro main_reflectmap_ps_d0
    
    env = init_env()
    SAVE_LOC = env.SAVE_LOC
    
;    ;;all data
;    popen, SAVE_LOC+'/maven/ps/random/map_gt5keV_out_d0_med_v2.ps'
;    plot_reflect_ion_map10,'gt5keV',vdf_type='shock_normal',surf_type='shock_surface',/make_notes,journal_file='map_gt5keV_out_d0_med_v2'
;    pclose
;
;    popen, SAVE_LOC+'/maven/ps/random/map_gt5keV_in_d0_med_v2.ps'
;    plot_reflect_ion_map10,'gt5keV_in',vdf_type='shock_normal',surf_type='shock_surface',/make_notes,journal_file='map_gt5keV_in_d0_med_v2'
;    pclose
;
;    popen, SAVE_LOC+'/maven/ps/random/map_gt10keV_out_d0_med_v2.ps'
;    plot_reflect_ion_map10,'gt10keV',vdf_type='shock_normal',surf_type='shock_surface',/make_notes,journal_file='map_gt10keV_out_d0_med_v2'
;    pclose
;
;    popen, SAVE_LOC+'/maven/ps/random/map_gt10keV_in_d0_med_v2.ps'
;    plot_reflect_ion_map10,'gt10keV_in',vdf_type='shock_normal',surf_type='shock_surface',/make_notes,journal_file='map_gt10keV_in_d0_med_v2'
;    pclose
;
;    popen, SAVE_LOC+'/maven/ps/random/map_20keV_out_d0_med_v2.ps'
;    plot_reflect_ion_map10,'20keV',vdf_type='shock_normal',surf_type='shock_surface',/make_notes,journal_file='map_20keV_out_d0_med_v2'
;    pclose
;
;    popen, SAVE_LOC+'/maven/ps/random/map_20keV_in_d0_med_v2.ps'
;    plot_reflect_ion_map10,'20keV_in',vdf_type='shock_normal',surf_type='shock_surface',/make_notes,journal_file='map_20keV_in_d0_med_v2'
;    pclose
;    
;    
;    
;    
;    ;;large gyro radii
;    popen, SAVE_LOC+'/maven/ps/random/map_gt5keV_large_rg_out_d0_med_v2.ps'
;    plot_reflect_ion_map10,'gt5keV',vdf_type='shock_normal',surf_type='shock_surface',/rg_mean,/large,/make_notes,journal_file='map_gt5keV_large_rg_out_d0_med_v2'
;    pclose
;
;    popen, SAVE_LOC+'/maven/ps/random/map_gt5keV_large_rg_in_d0_med_v2.ps'
;    plot_reflect_ion_map10,'gt5keV_in',vdf_type='shock_normal',surf_type='shock_surface',/rg_mean,/large,/make_notes,journal_file='map_gt5keV_large_rg_in_d0_med_v2'
;    pclose
;    
;    popen, SAVE_LOC+'/maven/ps/random/map_gt10keV_large_rg_out_d0_med_v2.ps'
;    plot_reflect_ion_map10,'gt10keV',vdf_type='shock_normal',surf_type='shock_surface',/rg_mean,/large,/make_notes,journal_file='map_gt10keV_large_rg_out_d0_med_v2'
;    pclose
;    
;    popen, SAVE_LOC+'/maven/ps/random/map_gt10keV_large_rg_in_d0_med_v2.ps'
;    plot_reflect_ion_map10,'gt10keV_in',vdf_type='shock_normal',surf_type='shock_surface',/rg_mean,/large,/make_notes,journal_file='map_gt10keV_large_rg_in_d0_med_v2'
;    pclose
;    
;    popen, SAVE_LOC+'/maven/ps/random/map_20keV_large_rg_out_d0_med_v2.ps'
;    plot_reflect_ion_map10,'20keV',vdf_type='shock_normal',surf_type='shock_surface',/rg_mean,/large,/make_notes,journal_file='map_20keV_large_rg_out_d0_med_v2'
;    pclose
;
;    popen, SAVE_LOC+'/maven/ps/random/map_20keV_large_rg_in_d0_med_v2.ps'
;    plot_reflect_ion_map10,'20keV_in',vdf_type='shock_normal',surf_type='shock_surface',/rg_mean,/large,/make_notes,journal_file='map_20keV_large_rg_in_d0_med_v2'
;    pclose
;    
;    
;    ;;small gyro radii
;    popen, SAVE_LOC+'/maven/ps/random/map_gt5keV_small_rg_out_d0_med_v2.ps'
;    plot_reflect_ion_map10,'gt5keV',vdf_type='shock_normal',surf_type='shock_surface',/rg_mean,/small,/make_notes,journal_file='map_gt5keV_small_rg_out_d0_med_v2'
;    pclose
;
;    popen, SAVE_LOC+'/maven/ps/random/map_gt5keV_small_rg_in_d0_med_v2.ps'
;    plot_reflect_ion_map10,'gt5keV_in',vdf_type='shock_normal',surf_type='shock_surface',/rg_mean,/small,/make_notes,journal_file='map_gt5keV_small_rg_in_d0_med_v2'
;    pclose
;
;    popen, SAVE_LOC+'/maven/ps/random/map_gt10keV_small_rg_out_d0_med_v2.ps'
;    plot_reflect_ion_map10,'gt10keV',vdf_type='shock_normal',surf_type='shock_surface',/rg_mean,/small,/make_notes,journal_file='map_gt10keV_small_rg_out_d0_med_v2'
;    pclose
;
;    popen, SAVE_LOC+'/maven/ps/random/map_gt10keV_small_rg_in_d0_med_v2.ps'
;    plot_reflect_ion_map10,'gt10keV_in',vdf_type='shock_normal',surf_type='shock_surface',/rg_mean,/small,/make_notes,journal_file='map_gt10keV_small_rg_in_d0_med_v2'
;    pclose
;
;    popen, SAVE_LOC+'/maven/ps/random/map_20keV_small_rg_out_d0_med_v2.ps'
;    plot_reflect_ion_map10,'20keV',vdf_type='shock_normal',surf_type='shock_surface',/rg_mean,/small,/make_notes,journal_file='map_20keV_small_rg_out_d0_med_v2'
;    pclose
;
;    popen, SAVE_LOC+'/maven/ps/random/map_20keV_small_rg_in_d0_med_v2.ps'
;    plot_reflect_ion_map10,'20keV_in',vdf_type='shock_normal',surf_type='shock_surface',/rg_mean,/small,/make_notes,journal_file='map_20keV_small_rg_in_d0_med_v2'
;    pclose



;;large vperp
;popen, SAVE_LOC+'/maven/ps/random/map_gt5keV_large_vperp_out_d0_med_v2.ps'
;plot_reflect_ion_map10,'gt5keV',vdf_type='shock_normal',surf_type='shock_surface',/vmso_perp_mean,/large,/make_notes,journal_file='map_gt5keV_large_vperp_out_d0_med_v2'
;pclose

;popen, SAVE_LOC+'/maven/ps/random/map_gt5keV_large_vperp_in_d0_med_v2.ps'
;plot_reflect_ion_map10,'gt5keV_in',vdf_type='shock_normal',surf_type='shock_surface',/vmso_perp_mean,/large,/make_notes,journal_file='map_gt5keV_large_vperp_in_d0_med_v2'
;pclose

;popen, SAVE_LOC+'/maven/ps/random/map_gt10keV_large_vperp_out_d0_med_v2.ps'
;plot_reflect_ion_map10,'gt10keV',vdf_type='shock_normal',surf_type='shock_surface',/vmso_perp_mean,/large,/make_notes,journal_file='map_gt10keV_large_vperp_out_d0_med_v2'
;pclose
;
;popen, SAVE_LOC+'/maven/ps/random/map_gt10keV_large_vperp_in_d0_med_v2.ps'
;plot_reflect_ion_map10,'gt10keV_in',vdf_type='shock_normal',surf_type='shock_surface',/vmso_perp_mean,/large,/make_notes,journal_file='map_gt10keV_large_vperp_in_d0_med_v2'
;pclose
;
;popen, SAVE_LOC+'/maven/ps/random/map_20keV_large_vperp_out_d0_med_v2.ps'
;plot_reflect_ion_map10,'20keV',vdf_type='shock_normal',surf_type='shock_surface',/vmso_perp_mean,/large,/make_notes,journal_file='map_20keV_large_vperp_out_d0_med_v2'
;pclose
;
;popen, SAVE_LOC+'/maven/ps/random/map_20keV_large_vperp_in_d0_med_v2.ps'
;plot_reflect_ion_map10,'20keV_in',vdf_type='shock_normal',surf_type='shock_surface',/vmso_perp_mean,/large,/make_notes,journal_file='map_20keV_large_vperp_in_d0_med_v2'
;pclose
;
;
;;;small vperp
;popen, SAVE_LOC+'/maven/ps/random/map_gt5keV_small_vperp_out_d0_med_v2.ps'
;plot_reflect_ion_map10,'gt5keV',vdf_type='shock_normal',surf_type='shock_surface',/vmso_perp_mean,/small,/make_notes,journal_file='map_gt5keV_small_vperp_out_d0_med_v2'
;pclose
;
;popen, SAVE_LOC+'/maven/ps/random/map_gt5keV_small_vperp_in_d0_med_v2.ps'
;plot_reflect_ion_map10,'gt5keV_in',vdf_type='shock_normal',surf_type='shock_surface',/vmso_perp_mean,/small,/make_notes,journal_file='map_gt5keV_small_vperp_in_d0_med_v2'
;pclose
;
;popen, SAVE_LOC+'/maven/ps/random/map_gt10keV_small_vperp_out_d0_med_v2.ps'
;plot_reflect_ion_map10,'gt10keV',vdf_type='shock_normal',surf_type='shock_surface',/vmso_perp_mean,/small,/make_notes,journal_file='map_gt10keV_small_vperp_out_d0_med_v2'
;pclose
;
;popen, SAVE_LOC+'/maven/ps/random/map_gt10keV_small_vperp_in_d0_med_v2.ps'
;plot_reflect_ion_map10,'gt10keV_in',vdf_type='shock_normal',surf_type='shock_surface',/vmso_perp_mean,/small,/make_notes,journal_file='map_gt10keV_small_vperp_in_d0_med_v2'
;pclose
;
;popen, SAVE_LOC+'/maven/ps/random/map_20keV_small_vperp_out_d0_med_v2.ps'
;plot_reflect_ion_map10,'20keV',vdf_type='shock_normal',surf_type='shock_surface',/vmso_perp_mean,/small,/make_notes,journal_file='map_20keV_small_vperp_out_d0_med_v2'
;pclose
;
;popen, SAVE_LOC+'/maven/ps/random/map_20keV_small_vperp_in_d0_med_v2.ps'
;plot_reflect_ion_map10,'20keV_in',vdf_type='shock_normal',surf_type='shock_surface',/vmso_perp_mean,/small,/make_notes,journal_file='map_20keV_small_vperp_in_d0_med_v2'
;pclose
;











;;;large Bt
;popen, SAVE_LOC+'/maven/ps/random/map_gt5keV_large_Bt_out_d0_med_v2.ps'
;plot_reflect_ion_map10,'gt5keV',vdf_type='shock_normal',surf_type='shock_surface',/Bt_mean,/large,/make_notes,journal_file='map_gt5keV_large_Bt_out_d0_med_v2'
;pclose
;
;popen, SAVE_LOC+'/maven/ps/random/map_gt5keV_large_Bt_in_d0_med_v2.ps'
;plot_reflect_ion_map10,'gt5keV_in',vdf_type='shock_normal',surf_type='shock_surface',/Bt_mean,/large,/make_notes,journal_file='map_gt5keV_large_Bt_in_d0_med_v2'
;pclose
;
;popen, SAVE_LOC+'/maven/ps/random/map_gt10keV_large_Bt_out_d0_med_v2.ps'
;plot_reflect_ion_map10,'gt10keV',vdf_type='shock_normal',surf_type='shock_surface',/Bt_mean,/large,/make_notes,journal_file='map_gt10keV_large_Bt_out_d0_med_v2'
;pclose
;
;popen, SAVE_LOC+'/maven/ps/random/map_gt10keV_large_Bt_in_d0_med_v2.ps'
;plot_reflect_ion_map10,'gt10keV_in',vdf_type='shock_normal',surf_type='shock_surface',/Bt_mean,/large,/make_notes,journal_file='map_gt10keV_large_Bt_in_d0_med_v2'
;pclose
;
;popen, SAVE_LOC+'/maven/ps/random/map_20keV_large_Bt_out_d0_med_v2.ps'
;plot_reflect_ion_map10,'20keV',vdf_type='shock_normal',surf_type='shock_surface',/Bt_mean,/large,/make_notes,journal_file='map_20keV_large_Bt_out_d0_med_v2'
;pclose
;
;popen, SAVE_LOC+'/maven/ps/random/map_20keV_large_Bt_in_d0_med_v2.ps'
;plot_reflect_ion_map10,'20keV_in',vdf_type='shock_normal',surf_type='shock_surface',/Bt_mean,/large,/make_notes,journal_file='map_20keV_large_Bt_in_d0_med_v2'
;pclose
;
;
;;;small Bt
;popen, SAVE_LOC+'/maven/ps/random/map_gt5keV_small_Bt_out_d0_med_v2.ps'
;plot_reflect_ion_map10,'gt5keV',vdf_type='shock_normal',surf_type='shock_surface',/Bt_mean,/small,/make_notes,journal_file='map_gt5keV_small_Bt_out_d0_med_v2'
;pclose
;
;popen, SAVE_LOC+'/maven/ps/random/map_gt5keV_small_Bt_in_d0_med_v2.ps'
;plot_reflect_ion_map10,'gt5keV_in',vdf_type='shock_normal',surf_type='shock_surface',/Bt_mean,/small,/make_notes,journal_file='map_gt5keV_small_Bt_in_d0_med_v2'
;pclose
;
;popen, SAVE_LOC+'/maven/ps/random/map_gt10keV_small_Bt_out_d0_med_v2.ps'
;plot_reflect_ion_map10,'gt10keV',vdf_type='shock_normal',surf_type='shock_surface',/Bt_mean,/small,/make_notes,journal_file='map_gt10keV_small_Bt_out_d0_med_v2'
;pclose
;
;popen, SAVE_LOC+'/maven/ps/random/map_gt10keV_small_Bt_in_d0_med_v2.ps'
;plot_reflect_ion_map10,'gt10keV_in',vdf_type='shock_normal',surf_type='shock_surface',/Bt_mean,/small,/make_notes,journal_file='map_gt10keV_small_Bt_in_d0_med_v2'
;pclose
;
;popen, SAVE_LOC+'/maven/ps/random/map_20keV_small_Bt_out_d0_med_v2.ps'
;plot_reflect_ion_map10,'20keV',vdf_type='shock_normal',surf_type='shock_surface',/Bt_mean,/small,/make_notes,journal_file='map_20keV_small_Bt_out_d0_med_v2'
;pclose
;
;popen, SAVE_LOC+'/maven/ps/random/map_20keV_small_Bt_in_d0_med_v2.ps'
;plot_reflect_ion_map10,'20keV_in',vdf_type='shock_normal',surf_type='shock_surface',/Bt_mean,/small,/make_notes,journal_file='map_20keV_small_Bt_in_d0_med_v2'
;pclose
;




;;large Pdyn
popen, SAVE_LOC+'/maven/ps/random/map_gt5keV_large_Pdyn_out_d0_med_v2.ps'
plot_reflect_ion_map10,'gt5keV',vdf_type='shock_normal',surf_type='shock_surface',/Pdyn_mean,/large,/make_notes,journal_file='map_gt5keV_large_Pdyn_out_d0_med_v2'
pclose

popen, SAVE_LOC+'/maven/ps/random/map_gt5keV_large_Pdyn_in_d0_med_v2.ps'
plot_reflect_ion_map10,'gt5keV_in',vdf_type='shock_normal',surf_type='shock_surface',/Pdyn_mean,/large,/make_notes,journal_file='map_gt5keV_large_Pdyn_in_d0_med_v2'
pclose

popen, SAVE_LOC+'/maven/ps/random/map_gt10keV_large_Pdyn_out_d0_med_v2.ps'
plot_reflect_ion_map10,'gt10keV',vdf_type='shock_normal',surf_type='shock_surface',/Pdyn_mean,/large,/make_notes,journal_file='map_gt10keV_large_Pdyn_out_d0_med_v2'
pclose

popen, SAVE_LOC+'/maven/ps/random/map_gt10keV_large_Pdyn_in_d0_med_v2.ps'
plot_reflect_ion_map10,'gt10keV_in',vdf_type='shock_normal',surf_type='shock_surface',/Pdyn_mean,/large,/make_notes,journal_file='map_gt10keV_large_Pdyn_in_d0_med_v2'
pclose

popen, SAVE_LOC+'/maven/ps/random/map_20keV_large_Pdyn_out_d0_med_v2.ps'
plot_reflect_ion_map10,'20keV',vdf_type='shock_normal',surf_type='shock_surface',/Pdyn_mean,/large,/make_notes,journal_file='map_20keV_large_Pdyn_out_d0_med_v2'
pclose

popen, SAVE_LOC+'/maven/ps/random/map_20keV_large_Pdyn_in_d0_med_v2.ps'
plot_reflect_ion_map10,'20keV_in',vdf_type='shock_normal',surf_type='shock_surface',/Pdyn_mean,/large,/make_notes,journal_file='map_20keV_large_Pdyn_in_d0_med_v2'
pclose


;;small Pdyn
popen, SAVE_LOC+'/maven/ps/random/map_gt5keV_small_Pdyn_out_d0_med_v2.ps'
plot_reflect_ion_map10,'gt5keV',vdf_type='shock_normal',surf_type='shock_surface',/Pdyn_mean,/small,/make_notes,journal_file='map_gt5keV_small_Pdyn_out_d0_med_v2'
pclose

popen, SAVE_LOC+'/maven/ps/random/map_gt5keV_small_Pdyn_in_d0_med_v2.ps'
plot_reflect_ion_map10,'gt5keV_in',vdf_type='shock_normal',surf_type='shock_surface',/Pdyn_mean,/small,/make_notes,journal_file='map_gt5keV_small_Pdyn_in_d0_med_v2'
pclose

popen, SAVE_LOC+'/maven/ps/random/map_gt10keV_small_Pdyn_out_d0_med_v2.ps'
plot_reflect_ion_map10,'gt10keV',vdf_type='shock_normal',surf_type='shock_surface',/Pdyn_mean,/small,/make_notes,journal_file='map_gt10keV_small_Pdyn_out_d0_med_v2'
pclose

popen, SAVE_LOC+'/maven/ps/random/map_gt10keV_small_Pdyn_in_d0_med_v2.ps'
plot_reflect_ion_map10,'gt10keV_in',vdf_type='shock_normal',surf_type='shock_surface',/Pdyn_mean,/small,/make_notes,journal_file='map_gt10keV_small_Pdyn_in_d0_med_v2'
pclose

popen, SAVE_LOC+'/maven/ps/random/map_20keV_small_Pdyn_out_d0_med_v2.ps'
plot_reflect_ion_map10,'20keV',vdf_type='shock_normal',surf_type='shock_surface',/Pdyn_mean,/small,/make_notes,journal_file='map_20keV_small_Pdyn_out_d0_med_v2'
pclose

popen, SAVE_LOC+'/maven/ps/random/map_20keV_small_Pdyn_in_d0_med_v2.ps'
plot_reflect_ion_map10,'20keV_in',vdf_type='shock_normal',surf_type='shock_surface',/Pdyn_mean,/small,/make_notes,journal_file='map_20keV_small_Pdyn_in_d0_med_v2'
pclose


end