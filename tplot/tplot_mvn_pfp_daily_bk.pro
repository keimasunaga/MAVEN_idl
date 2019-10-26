pro tplot_mvn_pfp_daily_bk,sd,ndays=ndays,png=png,save=save,notplot=notplot

   del_data,'*'
   if not keyword_set(sd) then dprint, 'Set start date!'
   st0 = sd
   env = init_env()
   SAVE_LOC = env.SAVE_LOC
   
     if not keyword_set(ndays) then ndays=1
     for ii=0.,ndays-1 do begin
       st = time_double(st0)
       st = st + 3600d*24d*ii
       et = st + 3600d*24d
       tspan = [st,et]
       date = strmid(time_string(st),0,4)+strmid(time_string(st),5,2)+strmid(time_string(st),8,2)
       print,'ST:',time_string(st),'    ET:',time_string(et)
       timespan,time_string([st,et])
       
       
     ;; ephemeris load
       mvn_spice_load_kei,trange=[st,et]
       split_vec,'MAVEN_POS_(Mars-MSO)'       
       t_rename,'MAVEN_POS_(Mars-MSO)_x','Xmso'
       t_rename,'MAVEN_POS_(Mars-MSO)_y','Ymso'
       t_rename,'MAVEN_POS_(Mars-MSO)_z','Zmso'

       get_data,'alt2',data=alt2,index=i
       if (i eq 0) then maven_orbit_tplot, /current, /loadonly     
  
     ;; static load
       mvn_sta_l2_load,sta_apid=['c0','c6']   
       mvn_sta_l2_tplot
       
     ;; swia load
       mvn_swia_load_l2_data,/loadcoarse,/tplot
       idx_ca = tplot_exist('mvn_swica_en_counts') 
       if idx_ca eq 1 then begin 
        mvn_swia_part_moments,type='ca'
        tplot_vec_tot,'mvn_swica_velocity'
        options,'mvn_swica_velocity',spice_master_frame='MAVEN_SPACECRAFT',spice_frame='MAVEN_SWIA'
        spice_vector_rotate_tplot,'mvn_swica_velocity','MAVEN_MSO',trange=trange,suffix='_mso'
       endif
       
       idx_cs = tplot_exist('mvn_swics_en_counts')
       if idx_ca eq 1 then begin
         mvn_swia_part_moments,type='ca'
         tplot_vec_tot,'mvn_swica_velocity'
         options,'mvn_swica_velocity',spice_master_frame='MAVEN_SPACECRAFT',spice_frame='MAVEN_SWIA'
         spice_vector_rotate_tplot,'mvn_swica_velocity','MAVEN_MSO',trange=trange,suffix='_mso'
       endif
     
       
       tplot_vec_tot,'mvn_swica_velocity_mso'
       
       store_data,'mvn_swim_velocity_mso_all',data=['mvn_swica_velocity','mvn_swica_velocity_tot']
       calc,'"mvn_swim_pdy"="mvn_swim_density"*"mvn_swim_velocity_mso_tot"^2*1.6726*1e-6'
        
     ;; swea load
       mvn_swe_load_l2,/spec
       ;mvn_swe_sumplot,/eph,/orb,/loadonly
       mvn_swe_etspec,trange=[st,et],/silent,unit='eflux'
       get_data,'mvn_swe_et_spec_svy',index=i
       if i eq 0 then store_data,'mvn_swe_et_spec_svy',data={x:tspan, y:[!values.F_NaN, !values.F_NaN]}
      
     ;; sep load
       load_mvn_sep,trange=[st,et]  ;; this is not maven_sw
     
     ;; mag load
       mvn_mag_load,spice_frame='MAVEN_MSO'
       tplot_vec_tot,'mvn_B_1sec_MAVEN_MSO'
       store_data,'mvn_B_1sec_MAVEN_MSO_all',data=['mvn_B_1sec_MAVEN_MSO','mvn_B_1sec_MAVEN_MSO_tot']
           
     ;; lpw load
    

       
     ;; options    
       options,'mvn_B_1sec_MAVEN_MSO',colors=[80,120,230]
       options,'mvn_swim_velocity_mso',colors=[80,120,230]
       options,'mvn_swim_velocity_mso_tot',colors=0,label='Vt'
       
;       get_timespan,tspn
;       npts = round((tspn[1] - tspn[0])/60D) + 1L
;       t = tspn[0] + 60D*dindgen(npts)
;       store_data,'orbnum',data={x:t, y:mvn_orbit_num(time=t)}
       tplot_options,'var_label',['orbnum','lat','lon','Zmso','Ymso','Xmso']
       tplot_options,'tickinterval',3600.d0*4 
            
       
       if ~keyword_set(png) then tplot,['mvn_sep1_B-O_Eflux_Energy', 'mvn_sep2_B-O_Eflux_Energy','mvn_swe_et_spec_svy','mvn_sta_c0_E','mvn_sta_c6_M','mvn_swics_en_eflux','mvn_swim_density','mvn_swim_velocity_mso_all','mvn_swim_pdy','mvn_B_1sec_MAVEN_MSO_all','alt2','sza']
       ;if ~ keyword_set(notplot) then tplot,['mvn_sep1_B-O_Eflux_Energy', 'mvn_sep2_B-O_Eflux_Energy','swe_a4','mvn_sta_c0_E','mvn_sta_c6_M','mvn_swis_en_eflux','mvn_swim_density','mvn_swim_velocity_mso_all','mvn_swim_pdy','mvn_B_1sec_all','alt2','sza']
       
       if keyword_set(png) then begin
         wi,0,xsize=1200,ysize=1000
         filename = SAVE_LOC + '/maven/png/tplot_daily/tplot_mvn_pfp_daily_'+date
         if !version.os eq 'darwin' then begin
           tplot,['mvn_sep1_B-O_Eflux_Energy', 'mvn_sep2_B-O_Eflux_Energy','mvn_swe_et_spec_svy','mvn_sta_c0_E','mvn_sta_c6_M','mvn_swics_en_eflux','mvn_swim_density','mvn_swim_velocity_mso_all','mvn_swim_pdy','mvn_B_1sec_MAVEN_MSO_all','alt2','sza']
           makepng,filename
         endif else begin
           tplot_options,'tickinterval',3600d
           popen,xsize=30,ysize=24,unit='cm',filename+'.ps'
           tplot,['mvn_sep1_B-O_Eflux_Energy', 'mvn_sep2_B-O_Eflux_Energy','mvn_swe_et_spec_svy','mvn_sta_c0_E','mvn_sta_c6_M','mvn_swics_en_eflux','mvn_swim_density','mvn_swim_velocity_mso_all','mvn_swim_pdy','mvn_B_1sec_MAVEN_MSO_all','alt2','sza']
           pclose
           spawn,'convert '+filename+'.ps '+filename+'.png'
           ;spawn,'rm '+filename+'.ps'
         endelse
         
         
       endif
       
       if keyword_set(save) then begin
           path = SAVE_LOC + '/maven/tplot_save/daily/'  
           tplot_save,['mvn_sep1_B-O_Eflux_Energy', 'mvn_sep2_B-O_Eflux_Energy','mvn_swe_et_spec_svy','mvn_sta_c0_E','mvn_sta_c6_M','mvn_swics_en_eflux','mvn_swim_density','mvn_swim_velocity_mso_all','mvn_swim_pdy','mvn_B_1sec_all','alt2','sza','lat','lon','orbnum'],filename=path+'test_tplot'+date
       endif
       
   
     endfor

end