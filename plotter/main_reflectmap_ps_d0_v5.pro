pro main_reflectmap_ps_d0_v5,rmv_ring_percentage=rmv_ring_percentage
    
    env = init_env()
    SAVE_LOC = env.SAVE_LOC
    percent = strcompress( string( rmv_ring_percentage ), /remove )
    
    
;    ;;all data
;    plot_reflect_ion_map10_v5,rmv_ring_percentage=rmv_ring_percentage,/ps,fileps='map_d0_rmv_plm'+percent+'.ps',/make_notes,journal_file='map_d0_rmv_plm'+percent
;    
;    ;;large gyro radii
;    plot_reflect_ion_map10_v5,rmv_ring_percentage=rmv_ring_percentage,/ps,fileps='map_rg_large_d0_rmv_plm'+percent+'.ps',/make_notes,journal_file='map_rg_large_d0_rmv_plm'+percent,$
;                              /sw_dpnd,/Rg,/large
;    ;;small gyro radii
;    plot_reflect_ion_map10_v5,rmv_ring_percentage=rmv_ring_percentage,/ps,fileps='map_rg_small_d0_rmv_plm'+percent+'.ps',/make_notes,journal_file='map_rg_small_d0_rmv_plm'+percent,$
;                              /sw_dpnd,/Rg,/small
;   
;    ;;large Vperp
;    plot_reflect_ion_map10_v5,rmv_ring_percentage=rmv_ring_percentage,/ps,fileps='map_vperp_large_d0_rmv_plm'+percent+'.ps',/make_notes,journal_file='map_vperp_large_d0_rmv_plm'+percent,$
;      /sw_dpnd,/vperp,/large
;    ;;large Vperp
;    plot_reflect_ion_map10_v5,rmv_ring_percentage=rmv_ring_percentage,/ps,fileps='map_vperp_small_d0_rmv_plm'+percent+'.ps',/make_notes,journal_file='map_vperp_small_d0_rmv_plm'+percent,$
;      /sw_dpnd,/vperp,/small
;   
;    ;;large Bt
;    plot_reflect_ion_map10_v5,rmv_ring_percentage=rmv_ring_percentage,/ps,fileps='map_Bt_large_d0_rmv_plm'+percent+'.ps',/make_notes,journal_file='map_Bt_large_d0_rmv_plm'+percent,$
;      /sw_dpnd,/Bt,/large
;    ;;large Bt
;    plot_reflect_ion_map10_v5,rmv_ring_percentage=rmv_ring_percentage,/ps,fileps='map_Bt_small_d0_rmv_plm'+percent+'.ps',/make_notes,journal_file='map_Bt_small_d0_rmv_plm'+percent,$
;      /sw_dpnd,/Bt,/small
;
;    ;;large Pdy
;    plot_reflect_ion_map10_v5,rmv_ring_percentage=rmv_ring_percentage,/ps,fileps='map_Pdy_large_d0_rmv_plm'+percent+'.ps',/make_notes,journal_file='map_Pdy_large_d0_rmv_plm'+percent,$
;      /sw_dpnd,/Pdy,/large
;    ;;large Pdy
;    plot_reflect_ion_map10_v5,rmv_ring_percentage=rmv_ring_percentage,/ps,fileps='map_Pdy_small_d0_rmv_plm'+percent+'.ps',/make_notes,journal_file='map_Pdy_small_d0_rmv_plm'+percent,$
;      /sw_dpnd,/Pdy,/small





;;all data
;plot_reflect_ion_map10_v5,rmv_ring_percentage=rmv_ring_percentage,fileps='map_d0_rmv_plm3_'+percent+'.ps',/make_notes,journal_file='map_d0_rmv_plm3_'+percent,/ps

;  ;;large Nsw
;  plot_reflect_ion_map10_v5,rmv_ring_percentage=rmv_ring_percentage,fileps='map_Nsw_large_d0_rmv_plm3_'+percent+'.ps',/make_notes,journal_file='map_Nsw_large_d0_rmv_plm3_'+percent,$
;    /sw_dpnd,/dens,/large,/ps
;  ;;small Nsw
;  plot_reflect_ion_map10_v5,rmv_ring_percentage=rmv_ring_percentage,fileps='map_Nsw_small_d0_rmv_plm3_'+percent+'.ps',/make_notes,journal_file='map_Nsw_small_d0_rmv_plm3_'+percent,$
;    /sw_dpnd,/dens,/small,/ps
;
;  ;;large Vsw
;  plot_reflect_ion_map10_v5,rmv_ring_percentage=rmv_ring_percentage,fileps='map_Vsw_large_d0_rmv_plm3_'+percent+'.ps',/make_notes,journal_file='map_Vsw_large_d0_rmv_plm3_'+percent,$
;    /sw_dpnd,/Vt,/large,/ps
;  ;;small Vsw
;  plot_reflect_ion_map10_v5,rmv_ring_percentage=rmv_ring_percentage,fileps='map_Vsw_small_d0_rmv_plm3_'+percent+'.ps',/make_notes,journal_file='map_Vsw_small_d0_rmv_plm3_'+percent,$
;    /sw_dpnd,/Vt,/small,/ps

;  ;;large Bt
;  plot_reflect_ion_map10_v5,rmv_ring_percentage=rmv_ring_percentage,fileps='map_Bt_large_d0_rmv_plm3_'+percent+'.ps',/make_notes,journal_file='map_Bt_large_d0_rmv_plm3_'+percent,$
;    /sw_dpnd,/Bt,/large,/ps
;  ;;small Bt
;  plot_reflect_ion_map10_v5,rmv_ring_percentage=rmv_ring_percentage,fileps='map_Bt_small_d0_rmv_plm3_'+percent+'.ps',/make_notes,journal_file='map_Bt_small_d0_rmv_plm3_'+percent,$
;    /sw_dpnd,/Bt,/small,/ps
;
;  ;;Perpendicular cone agl
;  plot_reflect_ion_map10_v5,rmv_ring_percentage=rmv_ring_percentage,fileps='map_cone_perp_d0_rmv_plm3_'+percent+'.ps',/make_notes,journal_file='map_cone_perp_d0_rmv_plm3_'+percent,$
;    /sw_dpnd,/cone,/perp,/ps
;  
;  ;;Parallel cone agl
;  plot_reflect_ion_map10_v5,rmv_ring_percentage=rmv_ring_percentage,fileps='map_cone_para_d0_rmv_plm3_'+percent+'.ps',/make_notes,journal_file='map_cone_para_d0_rmv_plm3_'+percent,$
;    /sw_dpnd,/cone,/para,/ps
;
;
;
;
;  ;;large Pdy
;  plot_reflect_ion_map10_v5,rmv_ring_percentage=rmv_ring_percentage,fileps='map_Pdy_large_d0_rmv_plm3_'+percent+'.ps',/make_notes,journal_file='map_Pdy_large_d0_rmv_plm3_'+percent,$
;    /sw_dpnd,/Pdy,/large,/ps
;  ;;small Pdy
;  plot_reflect_ion_map10_v5,rmv_ring_percentage=rmv_ring_percentage,fileps='map_Pdy_small_d0_rmv_plm3_'+percent+'.ps',/make_notes,journal_file='map_Pdy_small_d0_rmv_plm3_'+percent,$
;    /sw_dpnd,/Pdy,/small,/ps
;
;  ;;large gyro radii
;  plot_reflect_ion_map10_v5,rmv_ring_percentage=rmv_ring_percentage,fileps='map_rg_large_d0_rmv_plm3_'+percent+'.ps',/make_notes,journal_file='map_rg_large_d0_rmv_plm3_'+percent,$
;    /sw_dpnd,/Rg,/large,/ps
;  ;;small gyro radii
;  plot_reflect_ion_map10_v5,rmv_ring_percentage=rmv_ring_percentage,fileps='map_rg_small_d0_rmv_plm3_'+percent+'.ps',/make_notes,journal_file='map_rg_small_d0_rmv_plm3_'+percent,$
;    /sw_dpnd,/Rg,/small,/ps
;
;;  ;;large Vperp
;;  plot_reflect_ion_map10_v5,rmv_ring_percentage=rmv_ring_percentage,fileps='map_vperp_large_d0_rmv_plm3_'+percent+'.ps',/make_notes,journal_file='map_vperp_large_d0_rmv_plm3_'+percent,$
;;    /sw_dpnd,/vperp,/large,/ps
;;  ;;small Vperp
;;  plot_reflect_ion_map10_v5,rmv_ring_percentage=rmv_ring_percentage,fileps='map_vperp_small_d0_rmv_plm3_'+percent+'.ps',/make_notes,journal_file='map_vperp_small_d0_rmv_plm3_'+percent,$
;;    /sw_dpnd,/vperp,/small,/ps
;
;;;large Mach number
;plot_reflect_ion_map10_v5,rmv_ring_percentage=rmv_ring_percentage,fileps='map_Mms_large_d0_rmv_plm3_'+percent+'.ps',/make_notes,journal_file='map_Mms_large_d0_rmv_plm3_'+percent,$
;  /sw_dpnd,/Mach,/large,/ps
;;;small Mach number
;plot_reflect_ion_map10_v5,rmv_ring_percentage=rmv_ring_percentage,fileps='map_Mms_small_d0_rmv_plm3_'+percent+'.ps',/make_notes,journal_file='map_Mms_small_d0_rmv_plm3_'+percent,$
;  /sw_dpnd,/Mach,/small,/ps
;
;;;large sheath field
;  plot_reflect_ion_map10_v5,rmv_ring_percentage=rmv_ring_percentage,fileps='map_Bsheath2_large_d0_rmv_plm3_'+percent+'.ps',/make_notes,journal_file='map_Bsheath2_large_d0_rmv_plm3_'+percent,$
;    /sw_dpnd,/Bsheath,/large,/ps
;;;large sheath field
;  plot_reflect_ion_map10_v5,rmv_ring_percentage=rmv_ring_percentage,fileps='map_Bsheath2_small_d0_rmv_plm3_'+percent+'.ps',/make_notes,journal_file='map_Bsheath2_small_d0_rmv_plm3_'+percent,$
;    /sw_dpnd,/Bsheath,/small,/ps
;    
;
;
  ;;large EUV
  plot_reflect_ion_map10_v5,rmv_ring_percentage=rmv_ring_percentage,fileps='map_euv_large_d0_rmv_plm3_'+percent+'_v2.ps',/make_notes,journal_file='map_euv_large_d0_rmv_plm3_'+percent+'_v2',$
    /sw_dpnd,/euv,/large,/ps
  ;;small EUV
  plot_reflect_ion_map10_v5,rmv_ring_percentage=rmv_ring_percentage,fileps='map_euv_small_d0_rmv_plm3_'+percent+'_v2.ps',/make_notes,journal_file='map_euv_small_d0_rmv_plm3_'+percent+'_v2',$
    /sw_dpnd,/euv,/small,/ps

;  ;;Day crustal
;  plot_reflect_ion_map10_v5,rmv_ring_percentage=rmv_ring_percentage,fileps='map_Bcrst_Day_d0_rmv_plm3_'+percent+'.ps',/make_notes,journal_file='map_Bcrst_Day_d0_rmv_plm3_'+percent,$
;    /sw_dpnd,/Bcrst,/day,/ps
;  ;;Night crustal
;  plot_reflect_ion_map10_v5,rmv_ring_percentage=rmv_ring_percentage,fileps='map_Bcrst_Night_d0_rmv_plm3_'+percent+'.ps',/make_notes,journal_file='map_Bcrst_Night_d0_rmv_plm3_'+percent,$
;    /sw_dpnd,/Bcrst,/night,/ps





;    popen, SAVE_LOC+'/maven/ps/random/map_gt5keV_large_rg_in_d0_rmv_plm.ps'
;    plot_reflect_ion_map10_v5,'gt5keV_in',vdf_type='shock_normal',surf_type='shock_surface',/rg_mean,/large,/make_notes,journal_file='map_gt5keV_large_rg_in_d0_rmv_plm'
;    pclose
;    
;    popen, SAVE_LOC+'/maven/ps/random/map_gt10keV_large_rg_out_d0_rmv_plm.ps'
;    plot_reflect_ion_map10_v5,'gt10keV',vdf_type='shock_normal',surf_type='shock_surface',/rg_mean,/large,/make_notes,journal_file='map_gt10keV_large_rg_out_d0_rmv_plm'
;    pclose
;    
;    popen, SAVE_LOC+'/maven/ps/random/map_gt10keV_large_rg_in_d0_rmv_plm.ps'
;    plot_reflect_ion_map10_v5,'gt10keV_in',vdf_type='shock_normal',surf_type='shock_surface',/rg_mean,/large,/make_notes,journal_file='map_gt10keV_large_rg_in_d0_rmv_plm'
;    pclose
;    
;    popen, SAVE_LOC+'/maven/ps/random/map_20keV_large_rg_out_d0_rmv_plm.ps'
;    plot_reflect_ion_map10_v5,'20keV',vdf_type='shock_normal',surf_type='shock_surface',/rg_mean,/large,/make_notes,journal_file='map_20keV_large_rg_out_d0_rmv_plm'
;    pclose
;
;    popen, SAVE_LOC+'/maven/ps/random/map_20keV_large_rg_in_d0_rmv_plm.ps'
;    plot_reflect_ion_map10_v5,'20keV_in',vdf_type='shock_normal',surf_type='shock_surface',/rg_mean,/large,/make_notes,journal_file='map_20keV_large_rg_in_d0_rmv_plm'
;    pclose
;    
;    
;    ;;small gyro radii
;    popen, SAVE_LOC+'/maven/ps/random/map_gt5keV_small_rg_out_d0_rmv_plm.ps'
;    plot_reflect_ion_map10_v5,'gt5keV',vdf_type='shock_normal',surf_type='shock_surface',/rg_mean,/small,/make_notes,journal_file='map_gt5keV_small_rg_out_d0_rmv_plm'
;    pclose
;
;    popen, SAVE_LOC+'/maven/ps/random/map_gt5keV_small_rg_in_d0_rmv_plm.ps'
;    plot_reflect_ion_map10_v5,'gt5keV_in',vdf_type='shock_normal',surf_type='shock_surface',/rg_mean,/small,/make_notes,journal_file='map_gt5keV_small_rg_in_d0_rmv_plm'
;    pclose
;
;    popen, SAVE_LOC+'/maven/ps/random/map_gt10keV_small_rg_out_d0_rmv_plm.ps'
;    plot_reflect_ion_map10_v5,'gt10keV',vdf_type='shock_normal',surf_type='shock_surface',/rg_mean,/small,/make_notes,journal_file='map_gt10keV_small_rg_out_d0_rmv_plm'
;    pclose
;
;    popen, SAVE_LOC+'/maven/ps/random/map_gt10keV_small_rg_in_d0_rmv_plm.ps'
;    plot_reflect_ion_map10_v5,'gt10keV_in',vdf_type='shock_normal',surf_type='shock_surface',/rg_mean,/small,/make_notes,journal_file='map_gt10keV_small_rg_in_d0_rmv_plm'
;    pclose
;
;    popen, SAVE_LOC+'/maven/ps/random/map_20keV_small_rg_out_d0_rmv_plm.ps'
;    plot_reflect_ion_map10_v5,'20keV',vdf_type='shock_normal',surf_type='shock_surface',/rg_mean,/small,/make_notes,journal_file='map_20keV_small_rg_out_d0_rmv_plm'
;    pclose
;
;    popen, SAVE_LOC+'/maven/ps/random/map_20keV_small_rg_in_d0_rmv_plm.ps'
;    plot_reflect_ion_map10_v5,'20keV_in',vdf_type='shock_normal',surf_type='shock_surface',/rg_mean,/small,/make_notes,journal_file='map_20keV_small_rg_in_d0_rmv_plm'
;    pclose



;;large vperp
;popen, SAVE_LOC+'/maven/ps/random/map_gt5keV_large_vperp_out_d0_rmv_plm.ps'
;plot_reflect_ion_map10_v5,'gt5keV',vdf_type='shock_normal',surf_type='shock_surface',/vmso_perp_mean,/large,/make_notes,journal_file='map_gt5keV_large_vperp_out_d0_rmv_plm'
;pclose

;popen, SAVE_LOC+'/maven/ps/random/map_gt5keV_large_vperp_in_d0_rmv_plm.ps'
;plot_reflect_ion_map10_v5,'gt5keV_in',vdf_type='shock_normal',surf_type='shock_surface',/vmso_perp_mean,/large,/make_notes,journal_file='map_gt5keV_large_vperp_in_d0_rmv_plm'
;pclose

;popen, SAVE_LOC+'/maven/ps/random/map_gt10keV_large_vperp_out_d0_rmv_plm.ps'
;plot_reflect_ion_map10_v5,'gt10keV',vdf_type='shock_normal',surf_type='shock_surface',/vmso_perp_mean,/large,/make_notes,journal_file='map_gt10keV_large_vperp_out_d0_rmv_plm'
;pclose
;
;popen, SAVE_LOC+'/maven/ps/random/map_gt10keV_large_vperp_in_d0_rmv_plm.ps'
;plot_reflect_ion_map10_v5,'gt10keV_in',vdf_type='shock_normal',surf_type='shock_surface',/vmso_perp_mean,/large,/make_notes,journal_file='map_gt10keV_large_vperp_in_d0_rmv_plm'
;pclose
;
;popen, SAVE_LOC+'/maven/ps/random/map_20keV_large_vperp_out_d0_rmv_plm.ps'
;plot_reflect_ion_map10_v5,'20keV',vdf_type='shock_normal',surf_type='shock_surface',/vmso_perp_mean,/large,/make_notes,journal_file='map_20keV_large_vperp_out_d0_rmv_plm'
;pclose
;
;popen, SAVE_LOC+'/maven/ps/random/map_20keV_large_vperp_in_d0_rmv_plm.ps'
;plot_reflect_ion_map10_v5,'20keV_in',vdf_type='shock_normal',surf_type='shock_surface',/vmso_perp_mean,/large,/make_notes,journal_file='map_20keV_large_vperp_in_d0_rmv_plm'
;pclose
;
;
;;;small vperp
;popen, SAVE_LOC+'/maven/ps/random/map_gt5keV_small_vperp_out_d0_rmv_plm.ps'
;plot_reflect_ion_map10_v5,'gt5keV',vdf_type='shock_normal',surf_type='shock_surface',/vmso_perp_mean,/small,/make_notes,journal_file='map_gt5keV_small_vperp_out_d0_rmv_plm'
;pclose
;
;popen, SAVE_LOC+'/maven/ps/random/map_gt5keV_small_vperp_in_d0_rmv_plm.ps'
;plot_reflect_ion_map10_v5,'gt5keV_in',vdf_type='shock_normal',surf_type='shock_surface',/vmso_perp_mean,/small,/make_notes,journal_file='map_gt5keV_small_vperp_in_d0_rmv_plm'
;pclose
;
;popen, SAVE_LOC+'/maven/ps/random/map_gt10keV_small_vperp_out_d0_rmv_plm.ps'
;plot_reflect_ion_map10_v5,'gt10keV',vdf_type='shock_normal',surf_type='shock_surface',/vmso_perp_mean,/small,/make_notes,journal_file='map_gt10keV_small_vperp_out_d0_rmv_plm'
;pclose
;
;popen, SAVE_LOC+'/maven/ps/random/map_gt10keV_small_vperp_in_d0_rmv_plm.ps'
;plot_reflect_ion_map10_v5,'gt10keV_in',vdf_type='shock_normal',surf_type='shock_surface',/vmso_perp_mean,/small,/make_notes,journal_file='map_gt10keV_small_vperp_in_d0_rmv_plm'
;pclose
;
;popen, SAVE_LOC+'/maven/ps/random/map_20keV_small_vperp_out_d0_rmv_plm.ps'
;plot_reflect_ion_map10_v5,'20keV',vdf_type='shock_normal',surf_type='shock_surface',/vmso_perp_mean,/small,/make_notes,journal_file='map_20keV_small_vperp_out_d0_rmv_plm'
;pclose
;
;popen, SAVE_LOC+'/maven/ps/random/map_20keV_small_vperp_in_d0_rmv_plm.ps'
;plot_reflect_ion_map10_v5,'20keV_in',vdf_type='shock_normal',surf_type='shock_surface',/vmso_perp_mean,/small,/make_notes,journal_file='map_20keV_small_vperp_in_d0_rmv_plm'
;pclose
;











;;;large Bt
;popen, SAVE_LOC+'/maven/ps/random/map_gt5keV_large_Bt_out_d0_rmv_plm.ps'
;plot_reflect_ion_map10_v5,'gt5keV',vdf_type='shock_normal',surf_type='shock_surface',/Bt_mean,/large,/make_notes,journal_file='map_gt5keV_large_Bt_out_d0_rmv_plm'
;pclose
;
;popen, SAVE_LOC+'/maven/ps/random/map_gt5keV_large_Bt_in_d0_rmv_plm.ps'
;plot_reflect_ion_map10_v5,'gt5keV_in',vdf_type='shock_normal',surf_type='shock_surface',/Bt_mean,/large,/make_notes,journal_file='map_gt5keV_large_Bt_in_d0_rmv_plm'
;pclose
;
;popen, SAVE_LOC+'/maven/ps/random/map_gt10keV_large_Bt_out_d0_rmv_plm.ps'
;plot_reflect_ion_map10_v5,'gt10keV',vdf_type='shock_normal',surf_type='shock_surface',/Bt_mean,/large,/make_notes,journal_file='map_gt10keV_large_Bt_out_d0_rmv_plm'
;pclose
;
;popen, SAVE_LOC+'/maven/ps/random/map_gt10keV_large_Bt_in_d0_rmv_plm.ps'
;plot_reflect_ion_map10_v5,'gt10keV_in',vdf_type='shock_normal',surf_type='shock_surface',/Bt_mean,/large,/make_notes,journal_file='map_gt10keV_large_Bt_in_d0_rmv_plm'
;pclose
;
;popen, SAVE_LOC+'/maven/ps/random/map_20keV_large_Bt_out_d0_rmv_plm.ps'
;plot_reflect_ion_map10_v5,'20keV',vdf_type='shock_normal',surf_type='shock_surface',/Bt_mean,/large,/make_notes,journal_file='map_20keV_large_Bt_out_d0_rmv_plm'
;pclose
;
;popen, SAVE_LOC+'/maven/ps/random/map_20keV_large_Bt_in_d0_rmv_plm.ps'
;plot_reflect_ion_map10_v5,'20keV_in',vdf_type='shock_normal',surf_type='shock_surface',/Bt_mean,/large,/make_notes,journal_file='map_20keV_large_Bt_in_d0_rmv_plm'
;pclose
;
;
;;;small Bt
;popen, SAVE_LOC+'/maven/ps/random/map_gt5keV_small_Bt_out_d0_rmv_plm.ps'
;plot_reflect_ion_map10_v5,'gt5keV',vdf_type='shock_normal',surf_type='shock_surface',/Bt_mean,/small,/make_notes,journal_file='map_gt5keV_small_Bt_out_d0_rmv_plm'
;pclose
;
;popen, SAVE_LOC+'/maven/ps/random/map_gt5keV_small_Bt_in_d0_rmv_plm.ps'
;plot_reflect_ion_map10_v5,'gt5keV_in',vdf_type='shock_normal',surf_type='shock_surface',/Bt_mean,/small,/make_notes,journal_file='map_gt5keV_small_Bt_in_d0_rmv_plm'
;pclose
;
;popen, SAVE_LOC+'/maven/ps/random/map_gt10keV_small_Bt_out_d0_rmv_plm.ps'
;plot_reflect_ion_map10_v5,'gt10keV',vdf_type='shock_normal',surf_type='shock_surface',/Bt_mean,/small,/make_notes,journal_file='map_gt10keV_small_Bt_out_d0_rmv_plm'
;pclose
;
;popen, SAVE_LOC+'/maven/ps/random/map_gt10keV_small_Bt_in_d0_rmv_plm.ps'
;plot_reflect_ion_map10_v5,'gt10keV_in',vdf_type='shock_normal',surf_type='shock_surface',/Bt_mean,/small,/make_notes,journal_file='map_gt10keV_small_Bt_in_d0_rmv_plm'
;pclose
;
;popen, SAVE_LOC+'/maven/ps/random/map_20keV_small_Bt_out_d0_rmv_plm.ps'
;plot_reflect_ion_map10_v5,'20keV',vdf_type='shock_normal',surf_type='shock_surface',/Bt_mean,/small,/make_notes,journal_file='map_20keV_small_Bt_out_d0_rmv_plm'
;pclose
;
;popen, SAVE_LOC+'/maven/ps/random/map_20keV_small_Bt_in_d0_rmv_plm.ps'
;plot_reflect_ion_map10_v5,'20keV_in',vdf_type='shock_normal',surf_type='shock_surface',/Bt_mean,/small,/make_notes,journal_file='map_20keV_small_Bt_in_d0_rmv_plm'
;pclose
;




;;;large Pdyn
;popen, SAVE_LOC+'/maven/ps/random/map_gt5keV_large_Pdyn_out_d0_rmv_plm.ps'
;plot_reflect_ion_map10_v5,'gt5keV',vdf_type='shock_normal',surf_type='shock_surface',/Pdyn_mean,/large,/make_notes,journal_file='map_gt5keV_large_Pdyn_out_d0_rmv_plm'
;pclose
;
;popen, SAVE_LOC+'/maven/ps/random/map_gt5keV_large_Pdyn_in_d0_rmv_plm.ps'
;plot_reflect_ion_map10_v5,'gt5keV_in',vdf_type='shock_normal',surf_type='shock_surface',/Pdyn_mean,/large,/make_notes,journal_file='map_gt5keV_large_Pdyn_in_d0_rmv_plm'
;pclose
;
;popen, SAVE_LOC+'/maven/ps/random/map_gt10keV_large_Pdyn_out_d0_rmv_plm.ps'
;plot_reflect_ion_map10_v5,'gt10keV',vdf_type='shock_normal',surf_type='shock_surface',/Pdyn_mean,/large,/make_notes,journal_file='map_gt10keV_large_Pdyn_out_d0_rmv_plm'
;pclose
;
;popen, SAVE_LOC+'/maven/ps/random/map_gt10keV_large_Pdyn_in_d0_rmv_plm.ps'
;plot_reflect_ion_map10_v5,'gt10keV_in',vdf_type='shock_normal',surf_type='shock_surface',/Pdyn_mean,/large,/make_notes,journal_file='map_gt10keV_large_Pdyn_in_d0_rmv_plm'
;pclose
;
;popen, SAVE_LOC+'/maven/ps/random/map_20keV_large_Pdyn_out_d0_rmv_plm.ps'
;plot_reflect_ion_map10_v5,'20keV',vdf_type='shock_normal',surf_type='shock_surface',/Pdyn_mean,/large,/make_notes,journal_file='map_20keV_large_Pdyn_out_d0_rmv_plm'
;pclose
;
;popen, SAVE_LOC+'/maven/ps/random/map_20keV_large_Pdyn_in_d0_rmv_plm.ps'
;plot_reflect_ion_map10_v5,'20keV_in',vdf_type='shock_normal',surf_type='shock_surface',/Pdyn_mean,/large,/make_notes,journal_file='map_20keV_large_Pdyn_in_d0_rmv_plm'
;pclose
;
;
;;;small Pdyn
;popen, SAVE_LOC+'/maven/ps/random/map_gt5keV_small_Pdyn_out_d0_rmv_plm.ps'
;plot_reflect_ion_map10_v5,'gt5keV',vdf_type='shock_normal',surf_type='shock_surface',/Pdyn_mean,/small,/make_notes,journal_file='map_gt5keV_small_Pdyn_out_d0_rmv_plm'
;pclose
;
;popen, SAVE_LOC+'/maven/ps/random/map_gt5keV_small_Pdyn_in_d0_rmv_plm.ps'
;plot_reflect_ion_map10_v5,'gt5keV_in',vdf_type='shock_normal',surf_type='shock_surface',/Pdyn_mean,/small,/make_notes,journal_file='map_gt5keV_small_Pdyn_in_d0_rmv_plm'
;pclose
;
;popen, SAVE_LOC+'/maven/ps/random/map_gt10keV_small_Pdyn_out_d0_rmv_plm.ps'
;plot_reflect_ion_map10_v5,'gt10keV',vdf_type='shock_normal',surf_type='shock_surface',/Pdyn_mean,/small,/make_notes,journal_file='map_gt10keV_small_Pdyn_out_d0_rmv_plm'
;pclose
;
;popen, SAVE_LOC+'/maven/ps/random/map_gt10keV_small_Pdyn_in_d0_rmv_plm.ps'
;plot_reflect_ion_map10_v5,'gt10keV_in',vdf_type='shock_normal',surf_type='shock_surface',/Pdyn_mean,/small,/make_notes,journal_file='map_gt10keV_small_Pdyn_in_d0_rmv_plm'
;pclose
;
;popen, SAVE_LOC+'/maven/ps/random/map_20keV_small_Pdyn_out_d0_rmv_plm.ps'
;plot_reflect_ion_map10_v5,'20keV',vdf_type='shock_normal',surf_type='shock_surface',/Pdyn_mean,/small,/make_notes,journal_file='map_20keV_small_Pdyn_out_d0_rmv_plm'
;pclose
;
;popen, SAVE_LOC+'/maven/ps/random/map_20keV_small_Pdyn_in_d0_rmv_plm.ps'
;plot_reflect_ion_map10_v5,'20keV_in',vdf_type='shock_normal',surf_type='shock_surface',/Pdyn_mean,/small,/make_notes,journal_file='map_20keV_small_Pdyn_in_d0_rmv_plm'
;pclose


end