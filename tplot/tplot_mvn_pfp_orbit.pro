;+
; :Description:
;    Purpose of this routine is to display (and save) a time series of pfp (plasma and field package) of single orbit
;
; ${parameters}
;  sorbit: start orbit number
;
; ${keywords}
;  norb:  number of orbits. if set, time sereis will be drawn times of this number in a row
;  png: save figure in a png file
;  tsave: save tvars in a tplot save file.
;  noplot: do not show tplot (produce tplot variables only)
;  
; ${Related routines}
;  tplot_saved_mvn_pfp_orbit: restore and tplot
;  tplot_mvn_pfp_daily: daily version
;
; :Author: Kei Masunaga (@STEL, Nagoya Univ.)
;
; $Last modified May 13, 2014
;-


pro tplot_mvn_pfp_orbit,sorbit,norb=norb,png=png,tsave=tsave,notplot=notplot,trange=trange
   
   del_data,'*'
   if not keyword_set(sorbit) then dprint, 'Set start orbit!'
   env = init_env()
   SAVE_LOC = env.SAVE_LOC
   ;if !version.os eq 'darwin' then SAVE_LOC_HEAVY = env.SAVE_LOC_HEAVY
   
     if not keyword_set(norb) then norb=1
     for ii=0.,norb-1 do begin
       del_data,'*'
       orbit = sorbit + ii
       print,'ORBIT: ',orbit
       tspan = mvn_orbit_num(orbnum=[orbit-0.5,orbit+0.5])
       if keyword_set(trange) then tspan = time_double(trange)
       st = tspan[0]
       et = tspan[1]
       date = strmid(time_string(st),0,4)+strmid(time_string(st),5,2)+strmid(time_string(st),8,2)
       print,'ST:',time_string(st),'    ET:',time_string(et)
       timespan,time_string([st,et])
       
     print,time_string([st,et])
     ;; ephemeris load
       mvn_spice_load_kei,trange=[st,et]
       split_vec,'MAVEN_POS_(Mars-MSO)'       
       store_data,'MAVEN_POS_(Mars-MSO)_x',newname='Xmso' & options,'Xmso',dlimit={ytitle:'Xmso'} & calc,'"Xsc" = "Xmso"*1000./3389.9' & del_data,'Xmso' & store_data,'Xsc',newname='Xmso'
       store_data,'MAVEN_POS_(Mars-MSO)_y',newname='Ymso' & options,'Ymso',dlimit={ytitle:'Ymso'} & calc,'"Ysc" = "Ymso"*1000./3389.9' & del_data,'Ymso' & store_data,'Ysc',newname='Ymso'
       store_data,'MAVEN_POS_(Mars-MSO)_z',newname='Zmso' & options,'Zmso',dlimit={ytitle:'Zmso'} & calc,'"Zsc" = "Zmso"*1000./3389.9' & del_data,'Zmso' & store_data,'Zsc',newname='Zmso'

       get_data,'alt2',data=alt2,index=i
       if (i eq 0) then maven_orbit_tplot, /current, /loadonly     
       
     ;; static load
       mvn_sta_l2_load,sta_apid=['c0','c6']   
       mvn_sta_l2_tplot
       
     ;; swia load
       mvn_swia_load_l2_data,/loadcoarse,/tplot,/eflux,trange=[st,et]
       tplot_vec_tot,'mvn_swim_velocity_mso'
       store_data,'mvn_swim_velocity_mso_all',data=['mvn_swim_velocity_mso','mvn_swim_velocity_mso_tot']
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
       
       tplot_options,'var_label',['orbnum','lat','lon','Zmso','Ymso','Xmso']
       tplot_options,'tickinterval',1800.d0 
            
       
       if ~keyword_set(png) then tplot,['mvn_sep1_B-O_Eflux_Energy', 'mvn_sep2_B-O_Eflux_Energy','mvn_swe_et_spec_svy','mvn_sta_c0_E','mvn_sta_c6_M','mvn_swics_en_eflux','mvn_swim_density','mvn_swim_velocity_mso_all','mvn_swim_pdy','mvn_B_1sec_MAVEN_MSO_all','alt2','sza']
       ;if ~ keyword_set(notplot) then tplot,['mvn_sep1_B-O_Eflux_Energy', 'mvn_sep2_B-O_Eflux_Energy','swe_a4','mvn_sta_c0_E','mvn_sta_c6_M','mvn_swis_en_eflux','mvn_swim_density','mvn_swim_velocity_mso_all','mvn_swim_pdy','mvn_B_1sec_all','alt2','sza']
       
       if keyword_set(png) then begin
         wi,0,xsize=1200,ysize=1000
         filename = SAVE_LOC + '/maven/png/tplot_orbit/tplot_mvn_pfp_orbit_'+string(orbit,format='(I05)')
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
     
       ;; produce tplot save data
       if keyword_set(tsave) then begin
        
           get_timespan,t
           idx_eph = nn('alt',t)
           get_data,'alt',data=alt & store_data,'alt_',data={ x:alt.x[idx_eph[0]:idx_eph[1]], y:alt.y[idx_eph[0]:idx_eph[1]] }
           get_data,'sza',data=sza & store_data,'sza_',data={ x:sza.x[idx_eph[0]:idx_eph[1]], y:sza.y[idx_eph[0]:idx_eph[1]] }
           get_data,'sheath',data=sheath & store_data,'sheath_',data={ x:sheath.x[idx_eph[0]:idx_eph[1]], y:sheath.y[idx_eph[0]:idx_eph[1]] }
           get_data,'pileup',data=pileup & store_data,'pileup_',data={ x:pileup.x[idx_eph[0]:idx_eph[1]], y:pileup.y[idx_eph[0]:idx_eph[1]] }                      
           get_data,'wake',data=wake & store_data,'wake_',data={ x:wake.x[idx_eph[0]:idx_eph[1]], y:wake.y[idx_eph[0]:idx_eph[1]] }           
           get_data,'wind',data=wind & store_data,'wind_',data={ x:wind.x[idx_eph[0]:idx_eph[1]], y:wind.y[idx_eph[0]:idx_eph[1]] }
           ;get_data,'iono',data=iono & if n_elements(iono.x) eq n_elements(p store_data,'iono',data={ x:iono.x[idx_eph[0]:idx_eph[1]], y:iono.y[idx_eph[0]:idx_eph[1]] }
           get_data,'lat',data=lat & store_data,'lat_',data={ x:lat.x[idx_eph[0]:idx_eph[1]], y:lat.y[idx_eph[0]:idx_eph[1]] }
           get_data,'lon',data=lon & store_data,'lon_',data={ x:lon.x[idx_eph[0]:idx_eph[1]], y:lon.y[idx_eph[0]:idx_eph[1]] }
           
         
           get_data,'mvn_sep1_B-O_Eflux_Energy',data=sep1
           get_data,'mvn_sep2_B-O_Eflux_Energy',data=sep2
           if n_elements(sep1.x) eq 1 then if sep1.x eq 0 then goto, skip_sep
           exist_sep = tplot_exist('mvn_sep1_B-O_Eflux_Energy')
           idx_sep = nn('mvn_sep1_B-O_Eflux_Energy',t)
           store_data,'mvn_sep1_B-O_Eflux_Energy',data={ x:sep1.x[idx_sep[0]:idx_sep[1]], y:sep1.y[idx_sep[0]:idx_sep[1],*], v:sep1.v}
           store_data,'mvn_sep2_B-O_Eflux_Energy',data={ x:sep2.x[idx_sep[0]:idx_sep[1]], y:sep2.y[idx_sep[0]:idx_sep[1],*], v:sep2.v}
            
           skip_sep:
           
           get_data,'mvn_swics_en_eflux',data=swia
           get_data,'mvn_swim_density',data=density
           if size(swia,/type) ne 8 then goto, skip_swia                    
           idx_swia = nn('mvn_swics_en_eflux',t) & idx_swim = nn('mvn_swim_velocity_mso',t) 
           store_data,'mvn_swics_en_eflux',data={ x:swia.x[idx_swia[0]:idx_swia[1]], y:swia.y[idx_swia[0]:idx_swia[1],*], v:swia.v[idx_swia[0]:idx_swia[1],*], ylog:swia.ylog, zlog:swia.zlog, spec:swia.spec, no_interp:swia.no_interp, yrange:swia.yrange, ystyle:swia.ystyle, zrange:swia.zrange, ytitle:swia.ytitle, ztitle:swia.ztitle}
           store_data,'mvn_swim_density',data={ x:density.x[idx_swim[0]:idx_swim[1]], y:density.y[idx_swim[0]:idx_swim[1]] }
           get_data,'mvn_swim_velocity_mso',data=vel & store_data,'mvn_swim_velocity_mso',data={ x:vel.x[idx_swim[0]:idx_swim[1]], y:vel.y[idx_swim[0]:idx_swim[1],*] }
           get_data,'mvn_swim_temperature_mso',data=temp & store_data,'mvn_swim_temperature_mso',data={ x:temp.x[idx_swim[0]:idx_swim[1]], y:temp.y[idx_swim[0]:idx_swim[1],*] }
           get_data,'mvn_swim_pressure',data=pres & store_data,'mvn_swim_pressure',data={ x:pres.x[idx_swim[0]:idx_swim[1]], y:pres.y[idx_swim[0]:idx_swim[1],*] }           
           get_data,'mvn_swim_pdy',data=pdy & store_data,'mvn_swim_pdy',data={ x:pdy.x[idx_swim[0]:idx_swim[1]], y:pdy.y[idx_swim[0]:idx_swim[1]] }
           skip_swia:
           
           get_data,'mvn_B_1sec_MAVEN_MSO',data=mag
           if size(mag,/type) ne 8 then goto, skip_mag  
           idx_mag = nn('mvn_B_1sec_MAVEN_MSO',t)
           store_data,'mvn_B_1sec_MAVEN_MSO',data={x:mag.x[idx_mag[0]:idx_mag[1]], y:mag.y[idx_mag[0]:idx_mag[1],*]}
           skip_mag:
            
           get_data,'orbnum',data=orbnum
           idx_orb = nn('orbnum',(t[0]+t[1])/2d)
           store_data,'orbnum_',data={x:orbnum.x[idx_orb-1:idx_orb+1],y:orbnum.y[idx_orb-1:idx_orb+1]}
                    
           path = SAVE_LOC + '/maven/tplot_save/orbit/'  
           tplot_save,['mvn_sep1_B-O_Eflux_Energy', 'mvn_sep2_B-O_Eflux_Energy','mvn_swe_et_spec_svy','mvn_sta_c0_E','mvn_sta_c6_M','mvn_swics_en_eflux','mvn_swim_density','mvn_swim_velocity_mso','mvn_swim_pdy','mvn_swim_temperature_mso','mvn_swim_pressure','mvn_B_1sec_MAVEN_MSO',$
                       'alt_','sza_','sheath_','pileup_','wake_','wind_','lat_','lon_','Xmso','Ymso','Zmso','orbnum_'],$
                        filename=path+'tplot_mvn_pfp_orbit_'+string(orbit,format='(I05)')
           
       endif
       
   
     endfor

end