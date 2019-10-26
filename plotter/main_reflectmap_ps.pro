pro main_reflectmap_ps
    
    env = init_env()
    SAVE_LOC = env.SAVE_LOC
    
    
;    popen, SAVE_LOC+'/maven/ps/random/map_gt5keV_out.ps'
;    plot_reflect_ion_map8,'gt5keV',vdf_type='shock_normal',surf_type='shock_surface',/make_notes,journal_file='map_gt5keV_out'
;    pclose

    popen, SAVE_LOC+'/maven/ps/random/map_gt5keV_in.ps'
    plot_reflect_ion_map8,'gt5keV_in',vdf_type='shock_normal',surf_type='shock_surface',/make_notes,journal_file='map_gt5keV_in'
    pclose

    popen, SAVE_LOC+'/maven/ps/random/map_gt10keV_out.ps'
    plot_reflect_ion_map8,'gt10keV',vdf_type='shock_normal',surf_type='shock_surface',/make_notes,journal_file='map_gt10keV_out'
    pclose

    popen, SAVE_LOC+'/maven/ps/random/map_gt10keV_in.ps'
    plot_reflect_ion_map8,'gt10keV_in',vdf_type='shock_normal',surf_type='shock_surface',/make_notes,journal_file='map_gt10keV_in'
    pclose

    popen, SAVE_LOC+'/maven/ps/random/map_20keV_out.ps'
    plot_reflect_ion_map8,'20keV',vdf_type='shock_normal',surf_type='shock_surface',/make_notes,journal_file='map_20keV_out'
    pclose

    popen, SAVE_LOC+'/maven/ps/random/map_20keV_in.ps'
    plot_reflect_ion_map8,'20keV_in',vdf_type='shock_normal',surf_type='shock_surface',/make_notes,journal_file='map_20keV_in'
    pclose
    
    
    
    
    
;    popen, SAVE_LOC+'/maven/ps/random/map_gt5keV_large_rg_out.ps'
;    plot_reflect_ion_map8,'gt5keV',vdf_type='shock_normal',surf_type='shock_surface',/rg_mean,/large,/make_notes,journal_file='map_gt5keV_large_rg_out'
;    pclose
;
;    popen, SAVE_LOC+'/maven/ps/random/map_gt5keV_large_rg_in.ps'
;    plot_reflect_ion_map8,'gt5keV_in',vdf_type='shock_normal',surf_type='shock_surface',/rg_mean,/large,/make_notes,journal_file='map_gt5keV_large_rg_in'
;    pclose
;    
;    popen, SAVE_LOC+'/maven/ps/random/map_gt10keV_large_rg_out.ps'
;    plot_reflect_ion_map8,'gt10keV',vdf_type='shock_normal',surf_type='shock_surface',/rg_mean,/large,/make_notes,journal_file='map_gt10keV_large_rg_out'
;    pclose
;    
;    popen, SAVE_LOC+'/maven/ps/random/map_gt10keV_large_rg_in.ps'
;    plot_reflect_ion_map8,'gt10keV_in',vdf_type='shock_normal',surf_type='shock_surface',/rg_mean,/large,/make_notes,journal_file='map_gt10keV_large_rg_in'
;    pclose
;    
;    popen, SAVE_LOC+'/maven/ps/random/map_20keV_large_rg_out.ps'
;    plot_reflect_ion_map8,'20keV',vdf_type='shock_normal',surf_type='shock_surface',/rg_mean,/large,/make_notes,journal_file='map_20keV_large_rg_out'
;    pclose
;
;    popen, SAVE_LOC+'/maven/ps/random/map_20keV_large_rg_in.ps'
;    plot_reflect_ion_map8,'20keV_in',vdf_type='shock_normal',surf_type='shock_surface',/rg_mean,/large,/make_notes,journal_file='map_20keV_large_rg_in'
;    pclose
;    
;    
;    
;    popen, SAVE_LOC+'/maven/ps/random/map_gt5keV_small_rg_out.ps'
;    plot_reflect_ion_map8,'gt5keV',vdf_type='shock_normal',surf_type='shock_surface',/rg_mean,/small,/make_notes,journal_file='map_gt5keV_small_rg_out'
;    pclose
;
;    popen, SAVE_LOC+'/maven/ps/random/map_gt5keV_small_rg_in.ps'
;    plot_reflect_ion_map8,'gt5keV_in',vdf_type='shock_normal',surf_type='shock_surface',/rg_mean,/small,/make_notes,journal_file='map_gt5keV_small_rg_in'
;    pclose
;
;    popen, SAVE_LOC+'/maven/ps/random/map_gt10keV_small_rg_out.ps'
;    plot_reflect_ion_map8,'gt10keV',vdf_type='shock_normal',surf_type='shock_surface',/rg_mean,/small,/make_notes,journal_file='map_gt10keV_small_rg_out'
;    pclose
;
;    popen, SAVE_LOC+'/maven/ps/random/map_gt10keV_small_rg_in.ps'
;    plot_reflect_ion_map8,'gt10keV_in',vdf_type='shock_normal',surf_type='shock_surface',/rg_mean,/small,/make_notes,journal_file='map_gt10keV_small_rg_in'
;    pclose
;
;    popen, SAVE_LOC+'/maven/ps/random/map_20keV_small_rg_out.ps'
;    plot_reflect_ion_map8,'20keV',vdf_type='shock_normal',surf_type='shock_surface',/rg_mean,/small,/make_notes,journal_file='map_20keV_small_rg_out'
;    pclose
;
;    popen, SAVE_LOC+'/maven/ps/random/map_20keV_small_rg_in.ps'
;    plot_reflect_ion_map8,'20keV_in',vdf_type='shock_normal',surf_type='shock_surface',/rg_mean,/small,/make_notes,journal_file='map_20keV_small_rg_in'
;    pclose
;    
end