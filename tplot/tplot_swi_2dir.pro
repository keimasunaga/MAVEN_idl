pro tplot_swi_2dir,load=load
    
    if not keyword_set(apid) then apid = ['c0','d1']
    if keyword_set(load) then begin
      mvn_spice_load_kei
      mvn_swia_load_l2_data,/loadcoarse,/tplot
      
    endif
  
    mvn_swia_diret,frame='MAVEN_MSO'
    
    ;zlim,['mvn_sta_d1_en_eflux_mso_px','mvn_sta_d1_en_eflux_mso_mx','mvn_sta_d1_en_eflux_mso_pz','mvn_sta_d1_en_eflux_mso_mz'],1e3,1e7
    ;ylim,['mvn_sta_d1_en_eflux_mso_px','mvn_sta_d1_en_eflux_mso_mx'],0.1,40000.
    tplot,['mvn_swics_en_flux','mvn_swics_en_eflux_MAVEN_MSO_pX','mvn_swics_en_eflux_MAVEN_MSO_mX','mvn_swics_en_eflux_MAVEN_MSO_pY','mvn_swics_en_eflux_MAVEN_MSO_mY','mvn_swics_en_eflux_MAVEN_MSO_pZ','mvn_swics_en_eflux_MAVEN_MSO_mZ','mvn_swics_velocity_mso_all','mvn_B_1sec_MAVEN_MSO_all','alt2','sza']


end