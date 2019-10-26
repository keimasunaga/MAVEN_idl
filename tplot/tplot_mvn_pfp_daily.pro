pro tplot_mvn_pfp_daily,sd,ndays=ndays,png=png,save=save,notplot=notplot,load_spice=load_spice

   del_data,'*'
   
   if not keyword_set(sd) then dprint, 'Set start date!'
   st0 = sd
   env = init_env()
   SAVE_LOC = env.SAVE_LOC
   tplot_restore,file=SAVE_LOC+'/maven/tplot_save/mvn_ephemeris/mvn_ephemeris.tplot'
   
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
       del_data,['Xmso','Ymso','Zmso']
       if ~keyword_set(load_spice) then mvn_spice_load_kei,trange=[st,et] else mvn_spice_load,trange=[st,et]
       split_vec,'MAVEN_POS_(Mars-MSO)'
       split_vec,'MAVEN_POS_(Mars-MSO)'
       store_data,'MAVEN_POS_(Mars-MSO)_x',newname='Xmso' & options,'Xmso',dlimit={ytitle:'Xmso'} & calc,'"Xsc" = "Xmso"*1000./3389.9' & del_data,'Xmso' & store_data,'Xsc',newname='Xmso'
       store_data,'MAVEN_POS_(Mars-MSO)_y',newname='Ymso' & options,'Ymso',dlimit={ytitle:'Ymso'} & calc,'"Ysc" = "Ymso"*1000./3389.9' & del_data,'Ymso' & store_data,'Ysc',newname='Ymso'
       store_data,'MAVEN_POS_(Mars-MSO)_z',newname='Zmso' & options,'Zmso',dlimit={ytitle:'Zmso'} & calc,'"Zsc" = "Zmso"*1000./3389.9' & del_data,'Zmso' & store_data,'Zsc',newname='Zmso'   
;       t_rename,'MAVEN_POS_(Mars-MSO)_x','Xmso'
;       t_rename,'MAVEN_POS_(Mars-MSO)_y','Ymso'
;       t_rename,'MAVEN_POS_(Mars-MSO)_z','Zmso'
       store_data,'orbnum',newname='orbnum_whole' & get_data,'orbnum_whole',data=orbnum
       idx_orbnum = nn('orbnum_whole',[st,et])
       ;tinterpol_mxn,'orbnum_whole','Xmso'
       store_data,'orbnum',data={x:orbnum.x[idx_orbnum[0]:idx_orbnum[1]],y:orbnum.y[idx_orbnum[0]:idx_orbnum[1]]}
       get_data,'alt2',data=alt2,index=i
       
       if (ii eq 0) then begin
        ;maven_orbit_tplot, /current,/loadonly;,timecrop=time_string([st,et+10d*24d*3600d])
        store_data,'alt',newname='alt_whole' & get_data,'alt_whole',data=alt
        store_data,'sza',newname='sza_whole' & get_data,'sza_whole',data=sza
        store_data,'sheath',newname='sheath_whole' & get_data,'sheath_whole',data=sheath
        store_data,'pileup',newname='pileup_whole' & get_data,'pileup_whole',data=pileup
        store_data,'wake',newname='wake_whole' & get_data,'wake_whole',data=wake
        store_data,'wind',newname='wind_whole' & get_data,'wind_whole',data=wind
        store_data,'lon',newname='lon_whole' & get_data,'lon_whole',data=lon
        store_data,'lat',newname='lat_whole' & get_data,'lat_whole',data=lat
        
       endif

       idx_daily = nn('alt_whole',[st,et])
       store_data,'alt',data={x:alt.x[idx_daily[0]:idx_daily[1]],y:alt.y[idx_daily[0]:idx_daily[1]]}
       store_data,'sza',data={x:sza.x[idx_daily[0]:idx_daily[1]],y:sza.y[idx_daily[0]:idx_daily[1]]}
       store_data,'sheath',data={x:sheath.x[idx_daily[0]:idx_daily[1]],y:sheath.y[idx_daily[0]:idx_daily[1]]} & options,'sheath',colors=4
       store_data,'pileup',data={x:pileup.x[idx_daily[0]:idx_daily[1]],y:pileup.y[idx_daily[0]:idx_daily[1]]} & options,'pileup',colors=5
       store_data,'wake',data={x:wake.x[idx_daily[0]:idx_daily[1]],y:wake.y[idx_daily[0]:idx_daily[1]]} & options,'wake',colors=2
       store_data,'wind',data={x:wind.x[idx_daily[0]:idx_daily[1]],y:wind.y[idx_daily[0]:idx_daily[1]]} 
       store_data,'lon',data={x:lon.x[idx_daily[0]:idx_daily[1]],y:lon.y[idx_daily[0]:idx_daily[1]]}
       store_data,'lat',data={x:lat.x[idx_daily[0]:idx_daily[1]],y:lat.y[idx_daily[0]:idx_daily[1]]}
  
     ;; static load
       mvn_sta_l2_load,sta_apid=['c0','c6']   
       mvn_sta_l2_tplot
       
     ;; swia load
       del_data,'mvn_swi*'
       mvn_swia_load_l2_data,/loadcoarse,/tplot
       idx_ca = tplot_exist('mvn_swica_en_counts') 
       if idx_ca eq 1 then begin 
         mvn_swia_part_moments,type='ca'
         options,'mvn_swica_velocity',spice_master_frame='MAVEN_SPACECRAFT',spice_frame='MAVEN_SWIA'
         spice_vector_rotate_tplot,'mvn_swica_velocity','MAVEN_MSO',trange=trange,suffix='_mso'
         tplot_vec_tot,'mvn_swica_velocity_mso'
         options,'mvn_swica_velocity_mso',colors=[80,120,230]
         store_data,'mvn_swica_velocity_mso_all',data=['mvn_swica_velocity_mso','mvn_swica_velocity_mso_tot']
         calc,'"mvn_swica_pdy"="mvn_swica_density"*"mvn_swica_velocity_mso_tot"^2*1.6726*1e-6'
       endif
       idx_cs = tplot_exist('mvn_swics_en_counts')
       if idx_cs eq 1 then begin
         mvn_swia_part_moments,type='cs'
         options,'mvn_swics_velocity',spice_master_frame='MAVEN_SPACECRAFT',spice_frame='MAVEN_SWIA'
         spice_vector_rotate_tplot,'mvn_swics_velocity','MAVEN_MSO',trange=trange,suffix='_mso'
         tplot_vec_tot,'mvn_swics_velocity_mso'
         options,'mvn_swics_velocity_mso',colors=[80,120,230]
         store_data,'mvn_swics_velocity_mso_all',data=['mvn_swics_velocity_mso','mvn_swics_velocity_mso_tot']
         calc,'"mvn_swics_pdy"="mvn_swics_density"*"mvn_swica_velocity_mso_tot"^2*1.6726*1e-6'
       endif
       stop
;       tplot_vec_tot,'mvn_swica_velocity_mso'      
;       store_data,'mvn_swim_velocity_mso_all',data=['mvn_swica_velocity','mvn_swica_velocity_tot']

        
     ;; swea load
       mvn_swe_load_l2,/spec
       ;mvn_swe_sumplot,/eph,/orb,/loadonly
       mvn_swe_etspec,trange=[st,et],/silent,unit='eflux'
       get_data,'mvn_swe_et_spec_svy',index=i
       ;if i eq 0 then store_data,'mvn_swe_et_spec_svy',data={x:tspan, y:[!values.F_NaN, !values.F_NaN]}
      
     ;; sep load
       load_mvn_sep,trange=[st,et]  ;; this is not maven_sw
     
     ;; mag load
       mvn_mag_load,spice_frame='MAVEN_MSO'
       tplot_vec_tot,'mvn_B_1sec_MAVEN_MSO'
       store_data,'mvn_B_1sec_MAVEN_MSO_all',data=['mvn_B_1sec_MAVEN_MSO','mvn_B_1sec_MAVEN_MSO_tot']
           
     ;; lpw load
    
      ; mvn_lpw_cdf_read,st,vars=['wspecact','wspeccas','wn']
       
     ;; options    
       options,'mvn_B_1sec_MAVEN_MSO',colors=[80,120,230],labflg=1
       options,'mvn_swica_velocity_mso',colors=[80,120,230],labflg=1
       options,'mvn_swica_velocity_mso_tot',colors=0,label='Vt'
       
;       get_timespan,tspn
;       npts = round((tspn[1] - tspn[0])/60D) + 1L
;       t = tspn[0] + 60D*dindgen(npts)
;       store_data,'orbnum',data={x:t, y:mvn_orbit_num(time=t)}
       tplot_options,'var_label',['orbnum','lat','lon','Zmso','Ymso','Xmso']
       tplot_options,'tickinterval',3600.d0*4 
       
       
       if ~keyword_set(png) then begin
          if idx_ca eq 1 then tplot,['mvn_sep1_B-O_Eflux_Energy', 'mvn_sep2_B-O_Eflux_Energy','mvn_swe_et_spec_svy','mvn_sta_c0_E','mvn_sta_c0_H_E','mvn_sta_c6_M','mvn_swica_en_eflux','mvn_swica_density','mvn_swica_velocity_mso_all','mvn_swica_pdy','mvn_B_1sec_MAVEN_MSO_all','alt2','sza'] $
          else tplot,['mvn_sep1_B-O_Eflux_Energy', 'mvn_sep2_B-O_Eflux_Energy','mvn_swe_et_spec_svy','mvn_sta_c0_E','mvn_sta_c0_H_E','mvn_sta_c6_M','mvn_swics_en_eflux','mvn_swics_density','mvn_swics_velocity_mso_all','mvn_swics_pdy','mvn_B_1sec_MAVEN_MSO_all','alt2','sza'] 
       
       endif
       ;if ~ keyword_set(notplot) then tplot,['mvn_sep1_B-O_Eflux_Energy', 'mvn_sep2_B-O_Eflux_Energy','swe_a4','mvn_sta_c0_E','mvn_sta_c6_M','mvn_swis_en_eflux','mvn_swim_density','mvn_swim_velocity_mso_all','mvn_swim_pdy','mvn_B_1sec_all','alt2','sza']
       
       if keyword_set(png) then begin
         wi,0,xsize=1200,ysize=1000
         filename = SAVE_LOC + '/maven/png/tplot_daily/tplot_mvn_pfp_daily_'+date
         if !version.os eq 'darwin' then begin
           ;tplot,['mvn_sep1_B-O_Eflux_Energy', 'mvn_sep2_B-O_Eflux_Energy','mvn_swe_et_spec_svy','mvn_sta_c0_E','mvn_sta_c0_H_E','mvn_sta_c6_M','mvn_swica_en_eflux','mvn_swica_density','mvn_swica_velocity_mso_all','mvn_swica_pdy','mvn_B_1sec_MAVEN_MSO_all','alt2','sza']
           if idx_ca eq 1 then tplot,['mvn_sep1_B-O_Eflux_Energy', 'mvn_sep2_B-O_Eflux_Energy','mvn_swe_et_spec_svy','mvn_sta_c0_E','mvn_sta_c0_H_E','mvn_sta_c6_M','mvn_swica_en_eflux','mvn_swica_density','mvn_swica_velocity_mso_all','mvn_swica_pdy','mvn_B_1sec_MAVEN_MSO_all','alt2','sza'] $
           else tplot,['mvn_sep1_B-O_Eflux_Energy', 'mvn_sep2_B-O_Eflux_Energy','mvn_swe_et_spec_svy','mvn_sta_c0_E','mvn_sta_c0_H_E','mvn_sta_c6_M','mvn_swics_en_eflux','mvn_swics_density','mvn_swics_velocity_mso_all','mvn_swics_pdy','mvn_B_1sec_MAVEN_MSO_all','alt2','sza']
           makepng,filename
         endif else begin
           tplot_options,'tickinterval',3600d
           popen,xsize=30,ysize=24,unit='cm',filename+'.ps'
           ;tplot,['mvn_sep1_B-O_Eflux_Energy', 'mvn_sep2_B-O_Eflux_Energy','mvn_swe_et_spec_svy','mvn_sta_c0_E','mvn_sta_c0_H_E','mvn_sta_c6_M','mvn_swics_en_eflux','mvn_swim_density','mvn_swim_velocity_mso_all','mvn_swim_pdy','mvn_B_1sec_MAVEN_MSO_all','alt2','sza']
           if idx_ca eq 1 then tplot,['mvn_sep1_B-O_Eflux_Energy', 'mvn_sep2_B-O_Eflux_Energy','mvn_swe_et_spec_svy','mvn_sta_c0_E','mvn_sta_c0_H_E','mvn_sta_c6_M','mvn_swica_en_eflux','mvn_swica_density','mvn_swica_velocity_mso_all','mvn_swica_pdy','mvn_B_1sec_MAVEN_MSO_all','alt2','sza'] $
           else tplot,['mvn_sep1_B-O_Eflux_Energy', 'mvn_sep2_B-O_Eflux_Energy','mvn_swe_et_spec_svy','mvn_sta_c0_E','mvn_sta_c0_H_E','mvn_sta_c6_M','mvn_swics_en_eflux','mvn_swics_density','mvn_swics_velocity_mso_all','mvn_swics_pdy','mvn_B_1sec_MAVEN_MSO_all','alt2','sza']
           pclose
           spawn,'convert '+filename+'.ps '+filename+'.png'
           ;spawn,'rm '+filename+'.ps'
         endelse
         
         
       endif
       
       if keyword_set(save) then begin
           path = SAVE_LOC + '/maven/tplot_save/daily/'  
           ;tplot_save,['mvn_sep1_B-O_Eflux_Energy', 'mvn_sep2_B-O_Eflux_Energy','mvn_swe_et_spec_svy','mvn_sta_c0_E','mvn_sta_c6_M','mvn_swics_en_eflux','mvn_swim_density','mvn_swim_velocity_mso_all','mvn_swim_pdy','mvn_B_1sec_all','alt2','sza','lat','lon','orbnum'],filename=path+'tplot_pfp_'+date
           tplot_save,['mvn_sep1_B-O_Eflux_Energy', 'mvn_sep2_B-O_Eflux_Energy',$
                       'mvn_swe_et_spec_svy',$
                       'mvn_sta_c0_E','mvn_sta_c0_H_E','mvn_sta_c6_M',$
                       'mvn_swica_en_eflux','mvn_swica_density','mvn_swica_velocity_mso','mvn_swica_pdy','mvn_swica_temperature','mvn_swica_pressure',$
                       'mvn_swics_en_eflux','mvn_swics_density','mvn_swics_velocity_mso','mvn_swics_pdy','mvn_swics_temperature','mvn_swics_pressure',$
                       'mvn_B_1sec_MAVEN_MSO',$
                       'alt','sza','sheath','pileup','wake','wind','lon','lat',$
                       'Xmso','Ymso','Zmso','orbnum'],$
                       filename=path+'tplot_pfp_'+date
            swiflg = {flg_ca:idx_ca, flg_cs:idx_cs}
            save,swiflg,file=SAVE_LOC+'/maven/sav/swia_flg/daily/swia_flg_'+date+'.sav'
                       
       endif
       
   
     endfor

end