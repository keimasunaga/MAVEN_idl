pro tplot_sta_2dir,apid=apid,load=load
    
    if not keyword_set(apid) then apid = ['c0','d1']
    if keyword_set(load) then begin
      mvn_spice_load_kei
      mvn_sta_l2_load,sta_apid=apid
      mvn_sta_l2_tplot
    endif
  
    mvn_sta_etspec_dir_kei,'d1',/rmv;,thld_theta=0.
    
    zlim,['mvn_sta_d1_en_eflux_mso_px','mvn_sta_d1_en_eflux_mso_mx','mvn_sta_d1_en_eflux_mso_pz','mvn_sta_d1_en_eflux_mso_mz'],1e3,1e7
    ylim,['mvn_sta_d1_en_eflux_mso_px','mvn_sta_d1_en_eflux_mso_mx'],0.1,40000.
    tplot_options,'var_label',['orbnum','lat','lon','Zmso','Ymso','Xmso']
    tplot,['mvn_sta_c0_E','mvn_sta_d1_E','mvn_sta_d1_en_eflux_mso_px','mvn_sta_d1_en_eflux_mso_mx','mvn_sta_d1_en_eflux_mso_py','mvn_sta_d1_en_eflux_mso_my','mvn_sta_d1_en_eflux_mso_pz','mvn_sta_d1_en_eflux_mso_mz','mvn_swica_velocity_mso_all','mvn_B_1sec_MAVEN_MSO_all','alt2','sza']


end