pro tplot_mvn_bs_png,orbit

    env = init_env()
    SAVE_LOC = env.SAVE_LOC
    file_sw = SAVE_LOC + '/maven/sav/solar_wind_info/15min/sw_info_'+string(orbit,format='(i05)')+'.sav'
    file_png = SAVE_LOC + '/maven/png/bow_shock/bow_shock_'+string(orbit,format='(i05)')+'.sav'
    ft_sw = file_test(file_sw)
    
    if ft_sw eq 0 then goto, nodata
    restore,file_sw
    t_bs = dat_sw.time
    
    tplot_saved_mvn_pfp,orbit=orbit,/noplot,swi_flg_out=swi_flg
    wi,0,xsize=1200,ysize=1000
    tplot,['mvn_swe_et_spec_svy','mvn_sta_c0_E','mvn_sta_c0_H_E','mvn_sta_c6_M','mvn_'+swi_flg+'_en_eflux','mvn_'+swi_flg+'_density','mvn_'+swi_flg+'_velocity_mso_all','mvn_'+swi_flg+'_pdy','mvn_B_1sec_MAVEN_MSO','mvn_B_1sec_MAVEN_MSO_tot','alt2','sza']
    timebar,t_bs,color=230
    makepng,file_png   

    nodata:
    
end