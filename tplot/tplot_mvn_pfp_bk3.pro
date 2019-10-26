pro tplot_mvn_pfp_bk3,sd=sd,ndays=ndays,sorb=sorb,norb=norb,png=png,save=save,notplot=notplot

   del_data,'*'
   env = init_env()
   SAVE_LOC = env.SAVE_LOC
   tplot_restore,file=SAVE_LOC+'/maven/tplot_save/mvn_ephemeris/mvn_ephemeris.tplot' ;;use tsave_maven_orbit_tplot.pro to save this file
   
   if ~keyword_set(ndays) and ~keyword_set(norb) then ndat = 1.
   
   if keyword_set(ndays) then ndat = ndays
   if keyword_set(norb) then ndat = norb
   
     for ii=0.,ndat-1 do begin
       if keyword_set(sd) then begin
       st0 = sd
       st = time_double(st0)
       st = st + 3600d*24d*ii
       et = st + 3600d*24d -1d
       tspan = [st,et]
       date = strmid(time_string(st),0,4)+strmid(time_string(st),5,2)+strmid(time_string(st),8,2)
       print,'ST:',time_string(st),'    ET:',time_string(et)
       timespan,time_string([st,et])
       endif
       
       if keyword_set(sorb) then begin
       orbit = sorb+ii
       tspan = mvn_orbit_num(orbnum=[orbit-0.5,orbit+0.5])
       st = tspan[0]
       et = tspan[1]
       print,'ST:',time_string(st),'    ET:',time_string(et)
       timespan,time_string([st,et])
       endif
       
     ;; ephemeris load
       del_data,['Xmso','Ymso','Zmso']
       ;mvn_spice_load_kei,trange=[st,et]
       mvn_spice_load,trange=[st,et]
       
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
       ;get_data,'alt2',data=alt2,index=i
       
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
       sta_c6_bgrm, eflux, counts, /overwrite 
       mvn_sta_l2_tplot
       sta_c6_bgrm_kei, eflux, counts, energy, time_c6 ,mass_range=[12,20], /overwrite 
       mvn_sta_l2_tplot_c6_kei
       
     ;; swia load
       sdate = strmid(time_string(tspan[0]),0,10) & edate = strmid(time_string(tspan[1]),0,10)
       print,sdate,edate
     
       if sdate eq edate then begin  ;;start and end in the same day
        
          mvn_swia_load_l2_data,/loadcoarse,/tplot,trange=tspan
       
          idx_ca = tplot_exist('mvn_swica_en_counts') 
         
          if idx_ca eq 1 then begin
            get_data,'mvn_swica_en_counts',data=dswica
            stop
            mvn_swia_part_moments,type='ca'
            options,'mvn_swica_velocity',spice_master_frame='MAVEN_SPACECRAFT',spice_frame='MAVEN_SWIA'
            spice_vector_rotate_tplot,'mvn_swica_velocity','MAVEN_MSO',trange=trange,suffix='_mso'
            tplot_vec_tot,'mvn_swica_velocity_mso'
            options,'mvn_swica_velocity_mso',colors=[80,120,230]
            store_data,'mvn_swica_velocity_mso_all',data=['mvn_swica_velocity_mso','mvn_swica_velocity_mso_tot']
            calc,'"mvn_swica_pdy"="mvn_swica_density"*"mvn_swica_velocity_mso_tot"^2*1.6726*1e-6'
            idx_time_ca = nn('mvn_swica_velocity',tspan)
            if idx_time_ca[0] eq idx_time_ca[1] then idx_ca = 0             
            get_data,'mvn_swica_en_eflux',data=eflux_ca & store_data,'mvn_swica_en_eflux',data={x:eflux_ca.x[idx_time_ca[0]:idx_time_ca[1]],y:eflux_ca.y[idx_time_ca[0]:idx_time_ca[1],*], v:eflux_ca.v[idx_time_ca[0]:idx_time_ca[1],*], ylog:eflux_ca.ylog, zlog:eflux_ca.zlog, spec:eflux_ca.spec, no_interp:eflux_ca.no_interp, yrange:eflux_ca.yrange, ystyle:eflux_ca.ystyle, zrange:eflux_ca.zrange, ytitle:eflux_ca.ytitle, ztitle:eflux_ca.ztitle}
            get_data,'mvn_swica_velocity_mso',data=vel_ca & store_data,'mvn_swica_velocity_mso',data={x:vel_ca.x[idx_time_ca[0]:idx_time_ca[1]],y:vel_ca.y[idx_time_ca[0]:idx_time_ca[1],*]}
            get_data,'mvn_swica_density',data=dens_ca & store_data,'mvn_swica_density',data={x:dens_ca.x[idx_time_ca[0]:idx_time_ca[1]],y:dens_ca.y[idx_time_ca[0]:idx_time_ca[1],*]}
            get_data,'mvn_swica_temperature',data=temp_ca & store_data,'mvn_swica_temperature',data={x:temp_ca.x[idx_time_ca[0]:idx_time_ca[1]],y:temp_ca.y[idx_time_ca[0]:idx_time_ca[1],*]}
            get_data,'mvn_swica_pressure',data=press_ca & store_data,'mvn_swica_pressure',data={x:press_ca.x[idx_time_ca[0]:idx_time_ca[1]],y:press_ca.y[idx_time_ca[0]:idx_time_ca[1],*]}
            get_data,'mvn_swica_pdy',data=pdy_ca & store_data,'mvn_swica_pdy',data={x:pdy_ca.x[idx_time_ca[0]:idx_time_ca[1]],y:pdy_ca.y[idx_time_ca[0]:idx_time_ca[1],*]}           
          endif
          
          idx_cs = tplot_exist('mvn_swics_en_counts')
          if idx_cs eq 1 then begin
            mvn_swia_part_moments,type='cs'
            options,'mvn_swics_velocity',spice_master_frame='MAVEN_SPACECRAFT',spice_frame='MAVEN_SWIA'
            spice_vector_rotate_tplot,'mvn_swics_velocity','MAVEN_MSO',trange=trange,suffix='_mso'
            tplot_vec_tot,'mvn_swics_velocity_mso'
            options,'mvn_swics_velocity_mso',colors=[80,120,230]
            store_data,'mvn_swics_velocity_mso_all',data=['mvn_swics_velocity_mso','mvn_swics_velocity_mso_tot']
            calc,'"mvn_swics_pdy"="mvn_swics_density"*"mvn_swics_velocity_mso_tot"^2*1.6726*1e-6'
            idx_time_cs = nn('mvn_swics_velocity',tspan)
            get_data,'mvn_swics_en_eflux',data=eflux_cs & store_data,'mvn_swics_en_eflux',data={x:eflux_cs.x[idx_time_cs[0]:idx_time_cs[1]],y:eflux_cs.y[idx_time_cs[0]:idx_time_cs[1],*], v:eflux_cs.v[idx_time_cs[0]:idx_time_cs[1],*], ylog:eflux_cs.ylog, zlog:eflux_cs.zlog, spec:eflux_cs.spec, no_interp:eflux_cs.no_interp, yrange:eflux_cs.yrange, ystyle:eflux_cs.ystyle, zrange:eflux_cs.zrange, ytitle:eflux_cs.ytitle, ztitle:eflux_cs.ztitle}
            get_data,'mvn_swics_velocity_mso',data=vel_cs & store_data,'mvn_swics_velocity_mso',data={x:vel_cs.x[idx_time_cs[0]:idx_time_cs[1]],y:vel_cs.y[idx_time_cs[0]:idx_time_cs[1],*]}
            get_data,'mvn_swics_density',data=dens_cs & store_data,'mvn_swics_density',data={x:dens_cs.x[idx_time_cs[0]:idx_time_cs[1]],y:dens_cs.y[idx_time_cs[0]:idx_time_cs[1],*]}
            get_data,'mvn_swics_temperature',data=temp_cs & store_data,'mvn_swics_temperature',data={x:temp_cs.x[idx_time_cs[0]:idx_time_cs[1]],y:temp_cs.y[idx_time_cs[0]:idx_time_cs[1],*]}
            get_data,'mvn_swics_pressure',data=press_cs & store_data,'mvn_swics_pressure',data={x:press_cs.x[idx_time_cs[0]:idx_time_cs[1]],y:press_cs.y[idx_time_cs[0]:idx_time_cs[1],*]}
            get_data,'mvn_swics_pdy',data=pdy_cs & store_data,'mvn_swics_pdy',data={x:pdy_cs.x[idx_time_cs[0]:idx_time_cs[1]],y:pdy_cs.y[idx_time_cs[0]:idx_time_cs[1],*]}
          endif
       endif else begin  ;;start and end in the different day
         del_data,'mvn_*_1'
         del_data,'mvn_*_2'
         mvn_swia_load_l2_data,/loadcoarse,/tplot,trange=tspan
         
         idx_ca_1 = tplot_exist('mvn_swica_en_counts') & idx_cs_1 = tplot_exist('mvn_swics_en_counts')
        
          if idx_ca_1 eq 1 then begin 
            mvn_swia_part_moments,type='ca'
            options,'mvn_swica_velocity',spice_master_frame='MAVEN_SPACECRAFT',spice_frame='MAVEN_SWIA'
            spice_vector_rotate_tplot,'mvn_swica_velocity','MAVEN_MSO',trange=trange,suffix='_mso'
            tplot_vec_tot,'mvn_swica_velocity_mso'
            options,'mvn_swica_velocity_mso',colors=[80,120,230]
            store_data,'mvn_swica_velocity_mso_all',data=['mvn_swica_velocity_mso','mvn_swica_velocity_mso_tot']
            calc,'"mvn_swica_pdy"="mvn_swica_density"*"mvn_swica_velocity_mso_tot"^2*1.6726*1e-6'
            idx_time_ca = nn('mvn_swica_velocity',tspan)
            get_data,'mvn_swica_en_eflux',data=eflux_ca & store_data,'mvn_swica_en_eflux',data={x:eflux_ca.x[idx_time_ca[0]:idx_time_ca[1]],y:eflux_ca.y[idx_time_ca[0]:idx_time_ca[1],*], v:eflux_ca.v[idx_time_ca[0]:idx_time_ca[1],*], $
                                                                                          ylog:eflux_ca.ylog, zlog:eflux_ca.zlog, spec:eflux_ca.spec, no_interp:eflux_ca.no_interp, yrange:eflux_ca.yrange, ystyle:eflux_ca.ystyle, zrange:eflux_ca.zrange, ytitle:eflux_ca.ytitle, ztitle:eflux_ca.ztitle} & store_data,'mvn_swica_en_eflux',newname='mvn_swica_en_eflux_1'
            get_data,'mvn_swica_velocity_mso',data=vel_ca & store_data,'mvn_swica_velocity_mso',data={x:vel_ca.x[idx_time_ca[0]:idx_time_ca[1]],y:vel_ca.y[idx_time_ca[0]:idx_time_ca[1],*]} & store_data,'mvn_swica_velocity_mso',newname='mvn_swica_velocity_mso_1'
            get_data,'mvn_swica_density',data=dens_ca & store_data,'mvn_swica_density',data={x:dens_ca.x[idx_time_ca[0]:idx_time_ca[1]],y:dens_ca.y[idx_time_ca[0]:idx_time_ca[1],*]} & store_data,'mvn_swica_density',newname='mvn_swica_density_1'
            get_data,'mvn_swica_temperature',data=temp_ca & store_data,'mvn_swica_temperature',data={x:temp_ca.x[idx_time_ca[0]:idx_time_ca[1]],y:temp_ca.y[idx_time_ca[0]:idx_time_ca[1],*]} & store_data,'mvn_swica_temperature',newname='mvn_swica_temperature_1'
            get_data,'mvn_swica_pressure',data=press_ca & store_data,'mvn_swica_pressure',data={x:press_ca.x[idx_time_ca[0]:idx_time_ca[1]],y:press_ca.y[idx_time_ca[0]:idx_time_ca[1],*]} & store_data,'mvn_swica_pressure',newname='mvn_swica_pressure_1'
            get_data,'mvn_swica_pdy',data=pdy_ca & store_data,'mvn_swica_pdy',data={x:pdy_ca.x[idx_time_ca[0]:idx_time_ca[1]],y:pdy_ca.y[idx_time_ca[0]:idx_time_ca[1],*]} & store_data,'mvn_swica_pdy',newname='mvn_swica_pdy_1'
           
          endif
        
          if idx_cs_1 eq 1 then begin
            mvn_swia_part_moments,type='cs'
            options,'mvn_swics_velocity',spice_master_frame='MAVEN_SPACECRAFT',spice_frame='MAVEN_SWIA'
            spice_vector_rotate_tplot,'mvn_swics_velocity','MAVEN_MSO',trange=trange,suffix='_mso'
            tplot_vec_tot,'mvn_swics_velocity_mso'
            options,'mvn_swics_velocity_mso',colors=[80,120,230]
            store_data,'mvn_swics_velocity_mso_all',data=['mvn_swics_velocity_mso','mvn_swics_velocity_mso_tot']
            calc,'"mvn_swics_pdy"="mvn_swics_density"*"mvn_swics_velocity_mso_tot"^2*1.6726*1e-6'
            idx_time_cs = nn('mvn_swics_velocity',tspan)
            get_data,'mvn_swics_en_eflux',data=eflux_cs & store_data,'mvn_swics_en_eflux',data={x:eflux_cs.x[idx_time_cs[0]:idx_time_cs[1]],y:eflux_cs.y[idx_time_cs[0]:idx_time_cs[1],*], v:eflux_cs.v[idx_time_cs[0]:idx_time_cs[1],*], $
              ylog:eflux_cs.ylog, zlog:eflux_cs.zlog, spec:eflux_cs.spec, no_interp:eflux_cs.no_interp, yrange:eflux_cs.yrange, ystyle:eflux_cs.ystyle, zrange:eflux_cs.zrange, ytitle:eflux_cs.ytitle, ztitle:eflux_cs.ztitle} & store_data,'mvn_swics_en_eflux',newname='mvn_swics_en_eflux_1'
            get_data,'mvn_swics_velocity_mso',data=vel_cs & store_data,'mvn_swics_velocity_mso',data={x:vel_cs.x[idx_time_cs[0]:idx_time_cs[1]],y:vel_cs.y[idx_time_cs[0]:idx_time_cs[1],*]} & store_data,'mvn_swics_velocity_mso',newname='mvn_swics_velocity_mso_1'
            get_data,'mvn_swics_density',data=dens_cs & store_data,'mvn_swics_density',data={x:dens_cs.x[idx_time_cs[0]:idx_time_cs[1]],y:dens_cs.y[idx_time_cs[0]:idx_time_cs[1],*]} & store_data,'mvn_swics_density',newname='mvn_swics_density_1'
            get_data,'mvn_swics_temperature',data=temp_cs & store_data,'mvn_swics_temperature',data={x:temp_cs.x[idx_time_cs[0]:idx_time_cs[1]],y:temp_cs.y[idx_time_cs[0]:idx_time_cs[1],*]} & store_data,'mvn_swics_temperature',newname='mvn_swics_temperature_1'
            get_data,'mvn_swics_pressure',data=press_cs & store_data,'mvn_swics_pressure',data={x:press_cs.x[idx_time_cs[0]:idx_time_cs[1]],y:press_cs.y[idx_time_cs[0]:idx_time_cs[1],*]} & store_data,'mvn_swics_pressure',newname='mvn_swics_pressure_1'
            get_data,'mvn_swics_pdy',data=pdy_cs & store_data,'mvn_swics_pdy',data={x:pdy_cs.x[idx_time_cs[0]:idx_time_cs[1]],y:pdy_cs.y[idx_time_cs[0]:idx_time_cs[1],*]} & store_data,'mvn_swics_pdy',newname='mvn_swics_pdy_1'

          endif
        
          del_data,['mvn_swica_en_counts','mvn_swics_en_counts']
             
          mvn_swia_load_l2_data,/loadcoarse,/tplot,trange=tspan+3600d*24

          idx_ca_2 = tplot_exist('mvn_swica_en_counts') & idx_cs_2 = tplot_exist('mvn_swics_en_counts')
          if idx_ca_2 eq 1 then begin
            mvn_swia_part_moments,type='ca'
            options,'mvn_swica_velocity',spice_master_frame='MAVEN_SPACECRAFT',spice_frame='MAVEN_SWIA'
            spice_vector_rotate_tplot,'mvn_swica_velocity','MAVEN_MSO',trange=trange,suffix='_mso'
            tplot_vec_tot,'mvn_swica_velocity_mso'
            options,'mvn_swica_velocity_mso',colors=[80,120,230]
            store_data,'mvn_swica_velocity_mso_all',data=['mvn_swica_velocity_mso','mvn_swica_velocity_mso_tot']
            calc,'"mvn_swica_pdy"="mvn_swica_density"*"mvn_swica_velocity_mso_tot"^2*1.6726*1e-6'
            idx_time_ca = nn('mvn_swica_velocity',tspan)
            get_data,'mvn_swica_en_eflux',data=eflux_ca & store_data,'mvn_swica_en_eflux',data={x:eflux_ca.x[idx_time_ca[0]:idx_time_ca[1]],y:eflux_ca.y[idx_time_ca[0]:idx_time_ca[1],*], v:eflux_ca.v[idx_time_ca[0]:idx_time_ca[1],*], $
                                                                                          ylog:eflux_ca.ylog, zlog:eflux_ca.zlog, spec:eflux_ca.spec, no_interp:eflux_ca.no_interp, yrange:eflux_ca.yrange, ystyle:eflux_ca.ystyle, zrange:eflux_ca.zrange, ytitle:eflux_ca.ytitle, ztitle:eflux_ca.ztitle} & store_data,'mvn_swica_en_eflux',newname='mvn_swica_en_eflux_2'
            get_data,'mvn_swica_velocity_mso',data=vel_ca & store_data,'mvn_swica_velocity_mso',data={x:vel_ca.x[idx_time_ca[0]:idx_time_ca[1]],y:vel_ca.y[idx_time_ca[0]:idx_time_ca[1],*]} & store_data,'mvn_swica_velocity_mso',newname='mvn_swica_velocity_mso_2'
            get_data,'mvn_swica_density',data=dens_ca & store_data,'mvn_swica_density',data={x:dens_ca.x[idx_time_ca[0]:idx_time_ca[1]],y:dens_ca.y[idx_time_ca[0]:idx_time_ca[1],*]} & store_data,'mvn_swica_density',newname='mvn_swica_density_2'
            get_data,'mvn_swica_temperature',data=temp_ca & store_data,'mvn_swica_temperature',data={x:temp_ca.x[idx_time_ca[0]:idx_time_ca[1]],y:temp_ca.y[idx_time_ca[0]:idx_time_ca[1],*]} & store_data,'mvn_swica_temperature',newname='mvn_swica_temperature_2'
            get_data,'mvn_swica_pressure',data=press_ca & store_data,'mvn_swica_pressure',data={x:press_ca.x[idx_time_ca[0]:idx_time_ca[1]],y:press_ca.y[idx_time_ca[0]:idx_time_ca[1],*]} & store_data,'mvn_swica_pressure',newname='mvn_swica_pressure_2'
            get_data,'mvn_swica_pdy',data=pdy_ca & store_data,'mvn_swica_pdy',data={x:pdy_ca.x[idx_time_ca[0]:idx_time_ca[1]],y:pdy_ca.y[idx_time_ca[0]:idx_time_ca[1],*]} & store_data,'mvn_swica_pdy',newname='mvn_swica_pdy_2'

          endif
             
          if idx_cs_2 eq 1 then begin
            mvn_swia_part_moments,type='cs'
            options,'mvn_swics_velocity',spice_master_frame='MAVEN_SPACECRAFT',spice_frame='MAVEN_SWIA'
            spice_vector_rotate_tplot,'mvn_swics_velocity','MAVEN_MSO',trange=trange,suffix='_mso'
            tplot_vec_tot,'mvn_swics_velocity_mso'
            options,'mvn_swics_velocity_mso',colors=[80,120,230]
            store_data,'mvn_swics_velocity_mso_all',data=['mvn_swics_velocity_mso','mvn_swics_velocity_mso_tot']
            calc,'"mvn_swics_pdy"="mvn_swics_density"*"mvn_swics_velocity_mso_tot"^2*1.6726*1e-6'
            idx_time_cs = nn('mvn_swics_velocity',tspan)
            get_data,'mvn_swics_en_eflux',data=eflux_cs & store_data,'mvn_swics_en_eflux',data={x:eflux_cs.x[idx_time_cs[0]:idx_time_cs[1]],y:eflux_cs.y[idx_time_cs[0]:idx_time_cs[1],*], v:eflux_cs.v[idx_time_cs[0]:idx_time_cs[1],*], $
            ylog:eflux_cs.ylog, zlog:eflux_cs.zlog, spec:eflux_cs.spec, no_interp:eflux_cs.no_interp, yrange:eflux_cs.yrange, ystyle:eflux_cs.ystyle, zrange:eflux_cs.zrange, ytitle:eflux_cs.ytitle, ztitle:eflux_cs.ztitle} & store_data,'mvn_swics_en_eflux',newname='mvn_swics_en_eflux_2'
            get_data,'mvn_swics_velocity_mso',data=vel_cs & store_data,'mvn_swics_velocity_mso',data={x:vel_cs.x[idx_time_cs[0]:idx_time_cs[1]],y:vel_cs.y[idx_time_cs[0]:idx_time_cs[1],*]} & store_data,'mvn_swics_velocity_mso',newname='mvn_swics_velocity_mso_2'
            get_data,'mvn_swics_density',data=dens_cs & store_data,'mvn_swics_density',data={x:dens_cs.x[idx_time_cs[0]:idx_time_cs[1]],y:dens_cs.y[idx_time_cs[0]:idx_time_cs[1],*]} & store_data,'mvn_swics_density',newname='mvn_swics_density_2'
            get_data,'mvn_swics_temperature',data=temp_cs & store_data,'mvn_swics_temperature',data={x:temp_cs.x[idx_time_cs[0]:idx_time_cs[1]],y:temp_cs.y[idx_time_cs[0]:idx_time_cs[1],*]} & store_data,'mvn_swics_temperature',newname='mvn_swics_temperature_2'
            get_data,'mvn_swics_pressure',data=press_cs & store_data,'mvn_swics_pressure',data={x:press_cs.x[idx_time_cs[0]:idx_time_cs[1]],y:press_cs.y[idx_time_cs[0]:idx_time_cs[1],*]} & store_data,'mvn_swics_pressure',newname='mvn_swics_pressure_2'
            get_data,'mvn_swics_pdy',data=pdy_cs & store_data,'mvn_swics_pdy',data={x:pdy_cs.x[idx_time_cs[0]:idx_time_cs[1]],y:pdy_cs.y[idx_time_cs[0]:idx_time_cs[1],*]} & store_data,'mvn_swics_pdy',newname='mvn_swics_pdy_2'

          endif
          
          idx_ca = [idx_ca_1,idx_ca_2]
          idx_cs = [idx_cs_1,idx_cs_2]
          
          ;;combine each 2 tplot
          if idx_ca_1 eq 1 and idx_ca_2 eq 1 then begin
            connect_tvar,'mvn_swica_en_eflux' & get_data,'mvn_swica_en_eflux',data=eflux
            eflux = create_struct(eflux, 'ylog',eflux_ca.ylog, 'zlog',eflux_ca.zlog, 'spec',eflux_ca.spec, 'no_interp',eflux_ca.no_interp, 'yrange',eflux_ca.yrange,  'ystyle',eflux_ca.ystyle, 'zrange',eflux_ca.zrange, 'ytitle',eflux_ca.ytitle,'ztitle',eflux_ca.ztitle)
            store_data,'mvn_swica_en_eflux',data=eflux
            connect_tvar,'mvn_swica_velocity_mso' & tplot_vec_tot,'mvn_swica_velocity_mso'
            connect_tvar,'mvn_swica_density'
            connect_tvar,'mvn_swica_temperature'
            connect_tvar,'mvn_swica_pressure'
            connect_tvar,'mvn_swica_pdy'           
          endif else begin
            
            if idx_ca_1 eq 1 then begin
              store_data,'mvn_swica_en_eflux_1',newname='mvn_swica_en_eflux'
              store_data,'mvn_swica_velocity_mso_1',newname='mvn_swica_velocity_mso'
              store_data,'mvn_swica_density_1',newname='mvn_swica_density'
              store_data,'mvn_swica_temperature_1',newname='mvn_swica_temperature'
              store_data,'mvn_swica_pressure_1',newname='mvn_swica_pressure'
              store_data,'mvn_swica_pdy_1',newname='mvn_swica_pdy'        
            endif
            
            if idx_ca_2 eq 1 then begin
              store_data,'mvn_swica_en_eflux_2',newname='mvn_swica_en_eflux'
              store_data,'mvn_swica_velocity_mso_2',newname='mvn_swica_velocity_mso'
              store_data,'mvn_swica_density_2',newname='mvn_swica_density'
              store_data,'mvn_swica_temperature_2',newname='mvn_swica_temperature'
              store_data,'mvn_swica_pressure_2',newname='mvn_swica_pressure'
              store_data,'mvn_swica_pdy_2',newname='mvn_swica_pdy'
            endif
            
          endelse

          ;;combine each 2 tplot
          if idx_cs_1 eq 1 and idx_cs_2 eq 1 then begin
            connect_tvar,'mvn_swics_en_eflux' & get_data,'mvn_swics_en_eflux',data=eflux
            eflux = create_struct(eflux, 'ylog',eflux_cs.ylog, 'zlog',eflux_cs.zlog, 'spec',eflux_cs.spec, 'no_interp',eflux_cs.no_interp, 'yrange',eflux_cs.yrange,  'ystyle',eflux_cs.ystyle, 'zrange',eflux_cs.zrange, 'ytitle',eflux_cs.ytitle,'ztitle',eflux_cs.ztitle)
            store_data,'mvn_swics_en_eflux',data=eflux
            connect_tvar,'mvn_swics_velocity_mso' & tplot_vec_tot,'mvn_swica_velocity_mso'
            connect_tvar,'mvn_swics_density'
            connect_tvar,'mvn_swics_temperature'
            connect_tvar,'mvn_swics_pressure'
            connect_tvar,'mvn_swics_pdy'
          endif else begin
            
            if idx_cs_1 eq 1 then begin
              store_data,'mvn_swics_en_eflux_1',newname='mvn_swics_en_eflux'
              store_data,'mvn_swics_velocity_mso_1',newname='mvn_swics_velocity_mso'
              store_data,'mvn_swics_density_1',newname='mvn_swics_density'
              store_data,'mvn_swics_temperature_1',newname='mvn_swics_temperature'
              store_data,'mvn_swics_pressure_1',newname='mvn_swics_pressure'
              store_data,'mvn_swics_pdy_1',newname='mvn_swics_pdy'
            endif

            if idx_cs_2 eq 1 then begin
              store_data,'mvn_swics_en_eflux_2',newname='mvn_swics_en_eflux'
              store_data,'mvn_swics_velocity_mso_2',newname='mvn_swics_velocity_mso'
              store_data,'mvn_swics_density_2',newname='mvn_swics_density'
              store_data,'mvn_swics_temperature_2',newname='mvn_swics_temperature'
              store_data,'mvn_swics_pressure_2',newname='mvn_swics_pressure'
              store_data,'mvn_swics_pdy_2',newname='mvn_swics_pdy'
            endif
                       
          endelse  
          
       endelse

        
     ;; swea load
       mvn_swe_load_l2,/spec
       ;mvn_swe_sumplot,/eph,/orb,/loadonly
       mvn_swe_etspec,trange=[st,et],/silent,unit='eflux'
       get_data,'mvn_swe_et_spec_svy',index=i
       if i eq 0 then store_data,'mvn_swe_et_spec_svy',data={x:tspan, y:[!values.F_NaN, !values.F_NaN]}
      
     ;; sep load
       load_mvn_sep,trange=[st,et]  ;; this is not maven_sw
       get_data,'mvn_SEP1F_ion_eflux',data=sep1F_i,dlim=dl_1F_i,lim=lim_1F_i
       get_data,'mvn_SEP1R_ion_eflux',data=sep1R_i,dlim=dl_1R_i,lim=lim_1R_i 
       get_data,'mvn_SEP2F_ion_eflux',data=sep2F_i,dlim=dl_2F_i,lim=lim_2F_i
       get_data,'mvn_SEP2R_ion_eflux',data=sep2R_i,dlim=dl_2R_i,lim=lim_2R_i
       get_data,'mvn_SEP1F_elec_eflux',data=sep1F_e,dlim=dl_1F_e,lim=lim_1F_e
       get_data,'mvn_SEP1R_elec_eflux',data=sep1R_e,dlim=dl_1R_e,lim=lim_1R_e
       get_data,'mvn_SEP2F_elec_eflux',data=sep2F_e,dlim=dl_2F_e,lim=lim_2F_e
       get_data,'mvn_SEP2R_elec_eflux',data=sep2R_e,dlim=dl_2R_e,lim=lim_2R_e      
       if n_tags(sep1F_i) eq 0 then goto,no_sep
       
       ;; find index of NaN in the time
       idx_real_1F_i = where(finite(sep1F_i.x) ne 0)
       idx_real_1R_i = where(finite(sep1R_i.x) ne 0)
       idx_real_2F_i = where(finite(sep2F_i.x) ne 0)
       idx_real_2R_i = where(finite(sep2R_i.x) ne 0)
       idx_real_1F_e = where(finite(sep1F_e.x) ne 0)
       idx_real_1R_e = where(finite(sep1R_e.x) ne 0)
       idx_real_2F_e = where(finite(sep2F_e.x) ne 0)
       idx_real_2R_e = where(finite(sep2R_e.x) ne 0)
           
       
       if idx_real_1F_i[0] ne -1 then store_data,'mvn_SEP1F_ion_eflux',data={x:sep1F_i.x[idx_real_1F_i], y:sep1F_i.y[idx_real_1F_i,*], v:sep1F_i.v[idx_real_1F_i,*]},dlim=dl_1F_i,lim=lim_1F_i
       if idx_real_1R_i[0] ne -1 then store_data,'mvn_SEP1R_ion_eflux',data={x:sep1R_i.x[idx_real_1R_i], y:sep1R_i.y[idx_real_1R_i,*], v:sep1R_i.v[idx_real_1R_i,*]},dlim=dl_1R_i,lim=lim_1R_i
       if idx_real_2F_i[0] ne -1 then store_data,'mvn_SEP2F_ion_eflux',data={x:sep2F_i.x[idx_real_2F_i], y:sep2F_i.y[idx_real_2F_i,*], v:sep2F_i.v[idx_real_2F_i,*]},dlim=dl_2F_i,lim=lim_2F_i
       if idx_real_2R_i[0] ne -1 then store_data,'mvn_SEP2R_ion_eflux',data={x:sep2R_i.x[idx_real_2R_i], y:sep2R_i.y[idx_real_2R_i,*], v:sep2R_i.v[idx_real_2R_i,*]},dlim=dl_2R_i,lim=lim_2R_i
       if idx_real_1F_i[0] ne -1 then store_data,'mvn_SEP1F_elec_eflux',data={x:sep1F_e.x[idx_real_1F_e], y:sep1F_e.y[idx_real_1F_e,*], v:sep1F_e.v[idx_real_1F_e,*]},dlim=dl_1F_e,lim=lim_1F_e
       if idx_real_1R_e[0] ne -1 then store_data,'mvn_SEP1R_elec_eflux',data={x:sep1R_e.x[idx_real_1R_e], y:sep1R_e.y[idx_real_1R_e,*], v:sep1R_e.v[idx_real_1R_e,*]},dlim=dl_1R_e,lim=lim_1R_e
       if idx_real_2F_e[0] ne -1 then store_data,'mvn_SEP2F_elec_eflux',data={x:sep2F_e.x[idx_real_2F_e], y:sep2F_e.y[idx_real_2F_e,*], v:sep2F_e.v[idx_real_2F_e,*]},dlim=dl_2F_e,lim=lim_2F_e
       if idx_real_2R_e[0] ne -1 then store_data,'mvn_SEP2R_elec_eflux',data={x:sep2R_e.x[idx_real_2R_e], y:sep2R_e.y[idx_real_2R_e,*], v:sep2R_e.v[idx_real_2R_e,*]},dlim=dl_2R_e,lim=lim_2R_e
      
 
       replace_tvar_trange,'mvn_SEP1F_ion_eflux',[st,et]
       replace_tvar_trange,'mvn_SEP2F_ion_eflux',[st,et]
       replace_tvar_trange,'mvn_SEP1R_ion_eflux',[st,et]
       replace_tvar_trange,'mvn_SEP2R_ion_eflux',[st,et]
       replace_tvar_trange,'mvn_SEP1F_elec_eflux',[st,et]
       replace_tvar_trange,'mvn_SEP2F_elec_eflux',[st,et]
       replace_tvar_trange,'mvn_SEP1R_elec_eflux',[st,et]
       replace_tvar_trange,'mvn_SEP2R_elec_eflux',[st,et]
       no_sep:
       
     ;; mag load
       mvn_mag_load,'L2_1sec',spice_frame='MAVEN_MSO'
       tplot_vec_tot,'mvn_B_1sec_MAVEN_MSO'
       replace_tvar_trange,'mvn_B_1sec_MAVEN_MSO',[st,et]
       replace_tvar_trange,'mvn_B_1sec_MAVEN_MSO_tot',[st,et]
       store_data,'mvn_B_1sec_MAVEN_MSO_all',data=['mvn_B_1sec_MAVEN_MSO','mvn_B_1sec_MAVEN_MSO_tot']
    
     ;; lpw load
    
      ; mvn_lpw_cdf_read,st,vars=['wspecact','wspeccas','wn']
       
     ;; options    
       options,'mvn_B_1sec_MAVEN_MSO',colors=[80,120,230],labels=['Bx','By','Bz'],labflag=1
       options,'mvn_swica_velocity_mso',colors=[80,120,230],labels=['Vx','Vy','Vz'],labflag=1
       options,'mvn_swica_velocity_mso_tot',colors=0,label='Vt'
       options,'mvn_swics_velocity_mso',colors=[80,120,230],labels=['Vx','Vy','Vz'],labflag=1
       options,'mvn_swics_velocity_mso_tot',colors=0,label='Vt'
       
;       get_timespan,tspn
;       npts = round((tspn[1] - tspn[0])/60D) + 1L
;       t = tspn[0] + 60D*dindgen(npts)
;       store_data,'orbnum',data={x:t, y:mvn_orbit_num(time=t)}
       tplot_options,'var_label',['orbnum','lat','lon','Zmso','Ymso','Xmso']
       if keyword_set(sd) then tplot_options,'tickinterval',3600.d0*4 
       if keyword_set(sorb) then tplot_options,'tickinterval',3600.d0*0.5 
       
       if ~keyword_set(png) then begin
          if n_elements(idx_ca) eq 2 then if idx_ca[0] and idx_ca[1] eq 1 then tplot,['mvn_SEP1F_ion_eflux', 'mvn_SEP1R_ion_eflux','mvn_SEP2F_ion_eflux','mvn_SEP2R_ion_eflux','mvn_swe_et_spec_svy','mvn_sta_c0_E','mvn_sta_c0_H_E','mvn_sta_c6_M','mvn_swica_en_eflux','mvn_swica_density','mvn_swica_velocity_mso_all','mvn_swica_pdy','mvn_B_1sec_MAVEN_MSO_all','alt2','sza'] $
          else tplot,['mvn_SEP1F_ion_eflux', 'mvn_SEP1R_ion_eflux','mvn_SEP2F_ion_eflux','mvn_SEP2R_ion_eflux','mvn_swe_et_spec_svy','mvn_sta_c0_E','mvn_sta_c0_H_E','mvn_sta_c6_M','mvn_swics_en_eflux','mvn_swics_density','mvn_swics_velocity_mso_all','mvn_swics_pdy','mvn_B_1sec_MAVEN_MSO_all','alt2','sza'] 
          if n_elements(idx_ca) eq 1 then if idx_ca eq 1 then tplot,['mvn_SEP1F_ion_eflux', 'mvn_SEP1R_ion_eflux','mvn_SEP2F_ion_eflux','mvn_SEP2R_ion_eflux','mvn_swe_et_spec_svy','mvn_sta_c0_E','mvn_sta_c0_H_E','mvn_sta_c6_M','mvn_swica_en_eflux','mvn_swica_density','mvn_swica_velocity_mso_all','mvn_swica_pdy','mvn_B_1sec_MAVEN_MSO_all','alt2','sza'] $
          else tplot,['mvn_SEP1F_ion_eflux', 'mvn_SEP1R_ion_eflux','mvn_SEP2F_ion_eflux','mvn_SEP2R_ion_eflux','mvn_swe_et_spec_svy','mvn_sta_c0_E','mvn_sta_c0_H_E','mvn_sta_c6_M','mvn_swics_en_eflux','mvn_swics_density','mvn_swics_velocity_mso_all','mvn_swics_pdy','mvn_B_1sec_MAVEN_MSO_all','alt2','sza']

       endif
       ;if ~ keyword_set(notplot) then tplot,['mvn_sep1_B-O_Eflux_Energy', 'mvn_sep2_B-O_Eflux_Energy','swe_a4','mvn_sta_c0_E','mvn_sta_c6_M','mvn_swis_en_eflux','mvn_swim_density','mvn_swim_velocity_mso_all','mvn_swim_pdy','mvn_B_1sec_all','alt2','sza']
       
       if keyword_set(png) then begin
         wi,0,xsize=1200,ysize=1000
         
         if keyword_set(sd) then begin
         filename = SAVE_LOC + '/maven/png/tplot_daily/tplot_mvn_pfp_daily_'+date
         if !version.os eq 'darwin' then begin
           ;tplot,['mvn_sep1_B-O_Eflux_Energy', 'mvn_sep2_B-O_Eflux_Energy','mvn_swe_et_spec_svy','mvn_sta_c0_E','mvn_sta_c0_H_E','mvn_sta_c6_M','mvn_swica_en_eflux','mvn_swica_density','mvn_swica_velocity_mso_all','mvn_swica_pdy','mvn_B_1sec_MAVEN_MSO_all','alt2','sza']
           if idx_ca eq 1 then tplot,['mvn_SEP1F_ion_eflux', 'mvn_SEP1R_ion_eflux','mvn_SEP2F_ion_eflux','mvn_SEP2R_ion_eflux','mvn_swe_et_spec_svy','mvn_sta_c0_E','mvn_sta_c0_H_E','mvn_sta_c6_M','mvn_swica_en_eflux','mvn_swica_density','mvn_swica_velocity_mso_all','mvn_swica_pdy','mvn_B_1sec_MAVEN_MSO_all','alt2','sza'] $
           else tplot,['mvn_SEP1F_ion_eflux', 'mvn_SEP1R_ion_eflux','mvn_SEP2F_ion_eflux','mvn_SEP2R_ion_eflux','mvn_swe_et_spec_svy','mvn_sta_c0_E','mvn_sta_c0_H_E','mvn_sta_c6_M','mvn_swics_en_eflux','mvn_swics_density','mvn_swics_velocity_mso_all','mvn_swics_pdy','mvn_B_1sec_MAVEN_MSO_all','alt2','sza']
           makepng,filename
         endif else begin
           tplot_options,'tickinterval',3600d
           popen,xsize=30,ysize=24,unit='cm',filename+'.ps'
           ;tplot,['mvn_sep1_B-O_Eflux_Energy', 'mvn_sep2_B-O_Eflux_Energy','mvn_swe_et_spec_svy','mvn_sta_c0_E','mvn_sta_c0_H_E','mvn_sta_c6_M','mvn_swics_en_eflux','mvn_swim_density','mvn_swim_velocity_mso_all','mvn_swim_pdy','mvn_B_1sec_MAVEN_MSO_all','alt2','sza']
           if idx_ca eq 1 then tplot,['mvn_SEP1F_ion_eflux', 'mvn_SEP1R_ion_eflux','mvn_SEP2F_ion_eflux','mvn_SEP2R_ion_eflux','mvn_swe_et_spec_svy','mvn_sta_c0_E','mvn_sta_c0_H_E','mvn_sta_c6_M','mvn_swica_en_eflux','mvn_swica_density','mvn_swica_velocity_mso_all','mvn_swica_pdy','mvn_B_1sec_MAVEN_MSO_all','alt2','sza'] $
           else tplot,['mvn_SEP1F_ion_eflux', 'mvn_SEP1R_ion_eflux','mvn_SEP2F_ion_eflux','mvn_SEP2R_ion_eflux','mvn_swe_et_spec_svy','mvn_sta_c0_E','mvn_sta_c0_H_E','mvn_sta_c6_M','mvn_swics_en_eflux','mvn_swics_density','mvn_swics_velocity_mso_all','mvn_swics_pdy','mvn_B_1sec_MAVEN_MSO_all','alt2','sza']
           pclose
           spawn,'convert '+filename+'.ps '+filename+'.png'
           ;spawn,'rm '+filename+'.ps'
         endelse
         endif
         
         if keyword_set(sorb) then begin
           filename = SAVE_LOC + '/maven/png/tplot_orbit/tplot_mvn_pfp_orbit_'+string(orbit,format='(i05)')
           if !version.os eq 'darwin' then begin
             ;tplot,['mvn_sep1_B-O_Eflux_Energy', 'mvn_sep2_B-O_Eflux_Energy','mvn_swe_et_spec_svy','mvn_sta_c0_E','mvn_sta_c0_H_E','mvn_sta_c6_M','mvn_swica_en_eflux','mvn_swica_density','mvn_swica_velocity_mso_all','mvn_swica_pdy','mvn_B_1sec_MAVEN_MSO_all','alt2','sza']           
             if sdate eq edate then begin
              if idx_ca eq 1 then tplot,['mvn_SEP1F_ion_eflux', 'mvn_SEP1R_ion_eflux','mvn_SEP2F_ion_eflux','mvn_SEP2R_ion_eflux','mvn_swe_et_spec_svy','mvn_sta_c0_E','mvn_sta_c0_H_E','mvn_sta_c6_M','mvn_swica_en_eflux','mvn_swica_density','mvn_swica_velocity_mso_all','mvn_swica_pdy','mvn_B_1sec_MAVEN_MSO_all','alt2','sza'] $
              else tplot,['mvn_SEP1F_ion_eflux', 'mvn_SEP1R_ion_eflux','mvn_SEP2F_ion_eflux','mvn_SEP2R_ion_eflux','mvn_swe_et_spec_svy','mvn_sta_c0_E','mvn_sta_c0_H_E','mvn_sta_c6_M','mvn_swics_en_eflux','mvn_swics_density','mvn_swics_velocity_mso_all','mvn_swics_pdy','mvn_B_1sec_MAVEN_MSO_all','alt2','sza']
             endif else begin
               if idx_ca_1 eq 1 and idx_ca_2 eq 1 then tplot,['mvn_SEP1F_ion_eflux', 'mvn_SEP1R_ion_eflux','mvn_SEP2F_ion_eflux','mvn_SEP2R_ion_eflux','mvn_swe_et_spec_svy','mvn_sta_c0_E','mvn_sta_c0_H_E','mvn_sta_c6_M','mvn_swica_en_eflux','mvn_swica_density','mvn_swica_velocity_mso_all','mvn_swica_pdy','mvn_B_1sec_MAVEN_MSO_all','alt2','sza'] $
               else tplot,['mvn_SEP1F_ion_eflux', 'mvn_SEP1R_ion_eflux','mvn_SEP2F_ion_eflux','mvn_SEP2R_ion_eflux','mvn_swe_et_spec_svy','mvn_sta_c0_E','mvn_sta_c0_H_E','mvn_sta_c6_M','mvn_swics_en_eflux','mvn_swics_density','mvn_swics_velocity_mso_all','mvn_swics_pdy','mvn_B_1sec_MAVEN_MSO_all','alt2','sza']     
             endelse 
             makepng,filename
           endif else begin
             tplot_options,'tickinterval',3600d
             popen,xsize=30,ysize=24,unit='cm',filename+'.ps'
             if sdate eq edate then begin
              if idx_ca eq 1 then tplot,['mvn_SEP1F_ion_eflux', 'mvn_SEP1R_ion_eflux','mvn_SEP2F_ion_eflux','mvn_SEP2R_ion_eflux','mvn_swe_et_spec_svy','mvn_sta_c0_E','mvn_sta_c0_H_E','mvn_sta_c6_M','mvn_swica_en_eflux','mvn_swica_density','mvn_swica_velocity_mso_all','mvn_swica_pdy','mvn_B_1sec_MAVEN_MSO_all','alt2','sza'] $
              else tplot,['mvn_SEP1F_ion_eflux', 'mvn_SEP1R_ion_eflux','mvn_SEP2F_ion_eflux','mvn_SEP2R_ion_eflux','mvn_swe_et_spec_svy','mvn_sta_c0_E','mvn_sta_c0_H_E','mvn_sta_c6_M','mvn_swics_en_eflux','mvn_swics_density','mvn_swics_velocity_mso_all','mvn_swics_pdy','mvn_B_1sec_MAVEN_MSO_all','alt2','sza']
             endif else begin
               if idx_ca_1 eq 1 and idx_ca_2 eq 1 then tplot,['mvn_SEP1F_ion_eflux', 'mvn_SEP1R_ion_eflux','mvn_SEP2F_ion_eflux','mvn_SEP2R_ion_eflux','mvn_swe_et_spec_svy','mvn_sta_c0_E','mvn_sta_c0_H_E','mvn_sta_c6_M','mvn_swica_en_eflux','mvn_swica_density','mvn_swica_velocity_mso_all','mvn_swica_pdy','mvn_B_1sec_MAVEN_MSO_all','alt2','sza'] $
               else tplot,['mvn_SEP1F_ion_eflux', 'mvn_SEP1R_ion_eflux','mvn_SEP2F_ion_eflux','mvn_SEP2R_ion_eflux','mvn_swe_et_spec_svy','mvn_sta_c0_E','mvn_sta_c0_H_E','mvn_sta_c6_M','mvn_swics_en_eflux','mvn_swics_density','mvn_swics_velocity_mso_all','mvn_swics_pdy','mvn_B_1sec_MAVEN_MSO_all','alt2','sza']     
             endelse 
             pclose
             spawn,'convert '+filename+'.ps '+filename+'.png'
             ;spawn,'rm '+filename+'.ps'
           endelse
         endif
         
       endif
       
       if keyword_set(save) then begin
         if keyword_set(sd) then begin
           path = SAVE_LOC + '/maven/tplot_save/daily/'  
           ;tplot_save,['mvn_sep1_B-O_Eflux_Energy', 'mvn_sep2_B-O_Eflux_Energy','mvn_swe_et_spec_svy','mvn_sta_c0_E','mvn_sta_c6_M','mvn_swics_en_eflux','mvn_swim_density','mvn_swim_velocity_mso_all','mvn_swim_pdy','mvn_B_1sec_all','alt2','sza','lat','lon','orbnum'],filename=path+'tplot_pfp_'+date
           tplot_save,['mvn_SEP1F_ion_eflux', 'mvn_SEP1R_ion_eflux','mvn_SEP2F_ion_eflux','mvn_SEP2R_ion_eflux',$
                       'mvn_SEP1F_elec_eflux', 'mvn_SEP1R_elec_eflux','mvn_SEP2F_elec_eflux','mvn_SEP2R_elec_eflux',$
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
          
         if keyword_set(sorb) then begin
           path = SAVE_LOC + '/maven/tplot_save/orbit/'
           ;tplot_save,['mvn_sep1_B-O_Eflux_Energy', 'mvn_sep2_B-O_Eflux_Energy','mvn_swe_et_spec_svy','mvn_sta_c0_E','mvn_sta_c6_M','mvn_swics_en_eflux','mvn_swim_density','mvn_swim_velocity_mso_all','mvn_swim_pdy','mvn_B_1sec_all','alt2','sza','lat','lon','orbnum'],filename=path+'tplot_pfp_'+date
           tplot_save,['mvn_SEP1F_ion_eflux', 'mvn_SEP1R_ion_eflux','mvn_SEP2F_ion_eflux','mvn_SEP2R_ion_eflux',$
             'mvn_SEP1F_elec_eflux', 'mvn_SEP1R_elec_eflux','mvn_SEP2F_elec_eflux','mvn_SEP2R_elec_eflux',$
             'mvn_swe_et_spec_svy',$
             'mvn_sta_c0_E','mvn_sta_c0_H_E','mvn_sta_c6_M',$
             'mvn_swica_en_eflux','mvn_swica_density','mvn_swica_velocity_mso','mvn_swica_pdy','mvn_swica_temperature','mvn_swica_pressure',$
             'mvn_swics_en_eflux','mvn_swics_density','mvn_swics_velocity_mso','mvn_swics_pdy','mvn_swics_temperature','mvn_swics_pressure',$
             'mvn_B_1sec_MAVEN_MSO',$
             'alt','sza','sheath','pileup','wake','wind','lon','lat',$
             'Xmso','Ymso','Zmso','orbnum'],$
             filename=path+'tplot_pfp_'+string(orbit,format='(i05)')
           swiflg = {flg_ca:idx_ca, flg_cs:idx_cs}
           save,swiflg,file=SAVE_LOC+'/maven/sav/swia_flg/orbit/swia_flg_'+string(orbit,format='(i05)')+'.sav'          
         endif  
       endif
       
     endfor

end