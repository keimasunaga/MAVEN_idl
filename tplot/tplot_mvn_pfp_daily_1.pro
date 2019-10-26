pro tplot_mvn_pfp_daily_1,sd,days=days,png=png,save=save,notplot=notplot

   
   if not keyword_set(sd) then dprint, 'Set start date!'
   st0 = sd
   env = init_env()
   SAVE_LOC = env.SAVE_LOC
   
   
   if not keyword_set(days) then days=1
     for i=0,days-1 do begin
       st = time_double(st0)
       st = st + 3600d*24d*i
       et = st + 3600d*24d
       date = strmid(time_string(st),0,4)+strmid(time_string(st),5,2)+strmid(time_string(st),8,2)
       print,'ST:',time_string(st),'    ET:',time_string(et)
       timespan,time_string([st,et])
 
     ;; ephemeris load
       mvn_spice_load,trange=[st,et]
       get_data,'alt2',data=alt2,index=i
       if (i eq 0) then maven_orbit_tplot, /current, /loadonly
  
     ;; static load
       mvn_sta_l2_load,sta_apid=['c0','c6']   
       mvn_sta_l2_tplot
       
     ;; swia load
       mvn_swia_load_l2_data,/loadall,/tplot,/eflux
       tplot_vec_tot,'mvn_swim_velocity_mso'
       store_data,'mvn_swim_velocity_mso_all',data=['mvn_swim_velocity_mso','mvn_swim_velocity_mso_tot']
       calc,'"mvn_swim_pdy"="mvn_swim_density"*"mvn_swim_velocity_mso_tot"^2*1.6726*1e-6'
        
     ;; swea load
       mvn_swe_load_l2,/spec
       ;mvn_swe_sumplot,/eph,/orb,/loadonly
       mvn_swe_etspec,/silent,unit='eflux'
       get_data,'mvn_swe_et_spec_svy',index=i
       if i eq 0 then store_data,'mvn_swe_et_spec_svy',data={x:[st,et], y:[!values.F_NaN, !values.F_NaN]}
      
     ;; sep load
       load_mvn_sep,trange=[st,et]
     
     ;; mag load  
       mvn_mag_load,spice_frame='MAVEN_MSO'
       tplot_vec_tot,'mvn_B_1sec_MAVEN_MSO'
       store_data,'mvn_B_1sec_MAVEN_MSO_all',data=['mvn_B_1sec_MAVEN_MSO','mvn_B_1sec_MAVEN_MSO_tot']
           
     ;; lpw load
    
   
   
       
     ;; options    
       options,'mvn_B_1sec_MAVEN_MSO',colors=[80,120,230]
       options,'mvn_swim_velocity_mso',colors=[80,120,230]
       options,'mvn_swim_velocity_mso_tot',colors=0,label='Vt'
      
       get_timespan,tspn
       npts = round((tspn[1] - tspn[0])/60D) + 1L
       t = tspn[0] + 60D*dindgen(npts)
       store_data,'orbnum',data={x:t, y:mvn_orbit_num(time=t)}
       tplot_options,'var_label',['orbnum','lat','lon']
       
       
     ;; tplot and make png (if needed)        
       if keyword_set(png) then wi,0,xsize=1000,ysize=800 
       tplot_options,'tickinterval',3600.d0*4.d0 
       tplot,['mvn_sep1_B-O_Eflux_Energy', 'mvn_sep2_B-O_Eflux_Energy','mvn_swe_et_spec_svy','mvn_sta_c0_E','mvn_sta_c6_M','mvn_swics_en_eflux','mvn_swim_density','mvn_swim_velocity_mso_all','mvn_swim_pdy','mvn_B_1sec_MAVEN_MSO_all','alt2','sza']
       ;if ~ keyword_set(notplot) then tplot,['mvn_sep1_B-O_Eflux_Energy', 'mvn_sep2_B-O_Eflux_Energy','swe_a4','mvn_sta_c0_E','mvn_sta_c6_M','mvn_swis_en_eflux','mvn_swim_density','mvn_swim_velocity_mso_all','mvn_swim_pdy','mvn_B_1sec_all','alt2','sza']
       
       if keyword_set(png) then begin
         filename = SAVE_LOC + '/maven/png/tplot_daily/tplot_mvn_pfp_daily_'+date   
         makepng,filename
       endif
       
     ;; save data  
       if keyword_set(save) then begin
           path = SAVE_LOC + '/maven/tplot_save/daily/'  
           tplot_save,['mvn_sep1_B-O_Eflux_Energy', 'mvn_sep2_B-O_Eflux_Energy','mvn_swe_et_spec_svy','mvn_sta_c0_E','mvn_sta_c6_M','mvn_swis_en_eflux','mvn_swim_density','mvn_swim_velocity_mso_all','mvn_swim_pdy','mvn_B_1sec_MAVEN_MSO_all','alt2','sza','lat','lon','orbnum'],filename=path+'tplot_pfp_daily_'+date
       endif
       
       ;tplot_options,'tickinterval',3600.d0
     endfor

end