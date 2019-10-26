;+
; :Description:
;    This procedure creates tplot variabes of the solar wind density and velocity over a maltiple orbits.
;
; ${parameters}
; sorb: start orbit
; 
; ${keywords}
; norb: number of orbits you want to concatenate
; noplot: do not plot but create tplot variables
; 
; ${Return values}
;
; ${Related routines}
; tplot_saved_mvn_pfp
;
; $Author: Kei Masunaga (@IRF Kiruna)
;
; $Last modified Aug 14, 2017
;-

pro tplot_mvn_sw_long,sorb,norb=norb,noplot=noplot
    
    env = init_env()
    save_loc = env.SAVE_LOC
    
    time_Nsw = []
    time_Vsw = []
    Nsw_arr = []
    Vsw_arr = []
    
    for iorb=sorb,sorb+norb-1 do begin
        
        test_swi_flg = file_test(SAVE_LOC + '/maven/sav/swia_flg/orbit/swia_flg_'+string(iorb,format='(i05)')+'.sav')
        if test_swi_flg eq 0 then goto, nodata
        tplot_saved_mvn_pfp,orbit=iorb,swi_flg_out=swi_flg_out,/noplot
        print,swi_flg_out
        get_data,'mvn_'+swi_flg_out+'_density', data=dat_Nsw
        get_data,'mvn_'+swi_flg_out+'_velocity_mso',data=dat_Vsw       
        ;get_data,'mvn_B_1sec_MAVEN_MSO_all',data=dat_Bsw
        
        filename = save_loc + '/maven/sav/solar_wind_info/15min/sw_info_'+string(iorb,format='(i05)')+'.sav'
        filename_m = save_loc + '/maven/sav/solar_wind_info/15min/sw_info_'+string(iorb-1,format='(i05)')+'.sav'
        filename_p = save_loc + '/maven/sav/solar_wind_info/15min/sw_info_'+string(iorb+1,format='(i05)')+'.sav'
        if file_test(filename) eq 1 and file_test(filename_m) eq 1 and file_test(filename_p) eq 1 then begin
          restore,filename_m
          dat_sw_m = dat_sw
          t_bs_m = dat_sw_m.time
          restore,filename_p
          dat_sw_p = dat_sw
          t_bs_p = dat_sw_p.time
          restore,filename
          t_bs = dat_sw.time
          idx_Nsw = where( (dat_Nsw.x gt t_bs_m[1] and dat_Nsw.x lt t_bs[0]) or (dat_Nsw.x gt t_bs[1] and dat_Nsw.x lt t_bs_p[0]) )
          idx_Vsw = where( (dat_Vsw.x gt t_bs_m[1] and dat_Vsw.x lt t_bs[0]) or (dat_Vsw.x gt t_bs[1] and dat_Vsw.x lt t_bs_p[0]) )
          print,idx_Nsw,idx_Vsw
          if idx_Nsw[0] eq -1 or idx_Vsw[0] eq -1 then stop
          time_Nsw = [time_Nsw, dat_Nsw.x[idx_Nsw]]        
          time_Vsw = [time_Vsw, dat_Vsw.x[idx_Vsw]]
          Nsw_arr = [Nsw_arr, dat_Nsw.y[idx_Nsw]]
          Vsw_arr = [Vsw_arr, dat_Vsw.y[idx_Vsw,*]]   
        endif
        nodata:
    endfor
    
    Vsw_tot_arr = sqrt(Vsw_arr[*,0]^2+Vsw_arr[*,1]^2+Vsw_arr[*,2]^2)
    store_data,'mvn_density_long',data={x:time_Nsw, y:Nsw_arr}
    store_data,'mvn_velocity_mso_long',data={x:time_Vsw, y:Vsw_arr},dlim={colors:[80,120,230],labels:['Vx','Vy','Vz']}
    store_data,'mvn_velocity_tot_long',data={x:time_Vsw, y:Vsw_tot_arr}
    
    if not keyword_set(noplot) then tplot, ['mvn_density_long', 'mvn_velocity_mso_long','mvn_velocity_tot_long']
      
end