pro save_mvn_sw_temp,orbit=orbit,date=date,month=month,daysinmonth=daysinmonth
     
    env = init_env()
    SAVE_LOC = env.SAVE_LOC
;    if keyword_set(orbit) then begin
;     tplot_saved_mvn_pfp,orbit=orbit
;     ;;load SWEA data and get electron temperatures
;     mvn_swe_load_l2,/all,orbit=orbit
;     mvn_swe_shape_par, pans=pans
;     mvn_swe_sc_pot
;     mvn_swe_n1d, mom=mom, pans=more_pans
;     get_data,'mvn_swe_spec_dens',data=swe_dens
;     get_data,'mvn_swe_spec_temp',data=swe_temp
;     
;     get_timespan,tspan
;     st_str = time_struct(tspan[0])
;     et_str = time_struct(tspan[1])
;     if st_str.date eq et_str.date then trange_swi = tspan else trange_swi = [tspan[0]-3600d*12,tspan[1]+3600d*12]
;     mvn_swia_load_l2_data,/loadfine,/tplot,trange=trange_swi
;     mvn_swia_protonalphamoms
;     get_data,'nproton',data=nproton
;     get_data,'tproton',data=tproton
;     get_data,'vproton',data=vproton
;     get_data,'nalpha',data=nalpha
;     get_data,'talpha',data=talpha
;     get_data,'valpha',data=valpha
;     idx = nn(tproton.x,tspan)
;;     k = 1.38e-23
;;     e = 1.602e-19
;;     Te = 2.*swe_temp.y*e/k/3.
;;     store_data,'Te',data={x:swe_temp.x, y:Te}
;;     idx = nn(tproton.x,tspan)
;;     Ti = 2.*(tproton.y[*,0]+tproton.y[*,1]+tproton.y[*,2])*e/k/3.
;;     store_data,'Ti',data={x:tproton.x[idx[0]:idx[1]], y:Ti[idx[0]:idx[1],*]}
;     
;     nproton2 = { x:nproton.x[idx[0]:idx[1]], y:nproton.y[idx[0]:idx[1]] }
;     tproton2 = { x:tproton.x[idx[0]:idx[1]], y:tproton.y[idx[0]:idx[1],*] }
;     vproton2 = { x:vproton.x[idx[0]:idx[1]], y:vproton.y[idx[0]:idx[1],*] }
;     
;     nalpha2 = { x:nalpha.x[idx[0]:idx[1]], y:nalpha.y[idx[0]:idx[1]] }
;     talpha2 = { x:talpha.x[idx[0]:idx[1]], y:talpha.y[idx[0]:idx[1],*] }
;     valpha2 = { x:valpha.x[idx[0]:idx[1]], y:valpha.y[idx[0]:idx[1],*] }
;     
;     mom = {swe_dens:swe_dens, swe_temp:swe_temp,$
;            nproton:nproton2, tproton:tproton2, vproton:vproton2,$
;            nalpha:nalpha2, talpha:talpha2, valpha:valpha2}
;     save,mom, file=SAVE_LOC + '/maven/sav/moment/orbit/mom_'+string(orbit,format='(i05)')+'.sav'   
;    endif
    
    if keyword_set(date) then begin
      if strlen(date) gt 8 then begin
        message, 'Set date to be yyyymmdd'
      endif
      timespan,date
      mvn_swe_load_l2,/all,orbit=orbit
      mvn_swe_shape_par, pans=pans
      mvn_swe_sc_pot
      mvn_swe_n1d, mom=mom, pans=more_pans
      get_data,'mvn_swe_spec_dens',data=swe_dens
      get_data,'mvn_swe_spec_temp',data=swe_temp

;      get_timespan,tspan
;      st_str = time_struct(tspan[0])
;      et_str = time_struct(tspan[1])
;      if st_str.date eq et_str.date then trange_swi = tspan else trange_swi = [tspan[0]-3600d*12,tspan[1]+3600d*12]
      mvn_swia_load_l2_data,/loadfine,/tplot,trange=trange_swi
      mvn_swia_protonalphamoms
      get_data,'nproton',data=nproton
      get_data,'tproton',data=tproton
      get_data,'vproton',data=vproton
      get_data,'nalpha',data=nalpha
      get_data,'talpha',data=talpha
      get_data,'valpha',data=valpha
      
      mom = {swe_dens:swe_dens, swe_temp:swe_temp,$
        nproton:nproton, tproton:tproton, vproton:vproton,$
        nalpha:nalpha, talpha:talpha, valpha:valpha}
        stop
      save,mom, file=SAVE_LOC + '/maven/sav/moment/daily/mom_'+date+'.sav'     
      
    endif
     

;    if keyword_set(month) then begin
;      if strlen(month) gt 6 then begin
;        message, 'Set date to be yyyymm'
;      endif
;      timespan,month+'01',daysinmonth,/days
;      mvn_swe_load_l2,/all,orbit=orbit
;      mvn_swe_shape_par, pans=pans
;      mvn_swe_sc_pot
;      mvn_swe_n1d, mom=mom, pans=more_pans
;      get_data,'mvn_swe_spec_dens',data=swe_dens
;      get_data,'mvn_swe_spec_temp',data=swe_temp
;
;      ;      get_timespan,tspan
;      ;      st_str = time_struct(tspan[0])
;      ;      et_str = time_struct(tspan[1])
;      ;      if st_str.date eq et_str.date then trange_swi = tspan else trange_swi = [tspan[0]-3600d*12,tspan[1]+3600d*12]
;      mvn_swia_load_l2_data,/loadfine,/tplot,trange=trange_swi
;      mvn_swia_protonalphamoms
;      get_data,'nproton',data=nproton
;      get_data,'tproton',data=tproton
;      get_data,'vproton',data=vproton
;      get_data,'nalpha',data=nalpha
;      get_data,'talpha',data=talpha
;      get_data,'valpha',data=valpha
;
;      mom = {swe_dens:swe_dens, swe_temp:swe_temp,$
;        nproton:nproton, tproton:tproton, vproton:vproton,$
;        nalpha:nalpha, talpha:talpha, valpha:valpha}
;      save,mom, file=SAVE_LOC + '/maven/sav/moment/month/mom_'+month+'.sav'
;
;    endif

end