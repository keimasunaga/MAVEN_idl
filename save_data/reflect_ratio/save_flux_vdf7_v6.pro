

pro save_flux_vdf7_v6,orbit,trange=trange,png=png,ps2pdf=ps2pdf,ps2png=ps2png,save=save,event=event,highres=highres,_extra=extra,get_apid=get_apid,$
                   plot_vdf=plot_vdf,spice_load=spice_load,dat_sav=dat_sav

  env = init_env()
  SAVE_LOC = env.SAVE_LOC
  SAVE_LOC_HEAVY = env.SAVE_LOC_HEAVY
  
  del_data,'*'
  tplot_saved_mvn_pfp,orbit=orbit
  
  file_sw = SAVE_LOC + '/maven/sav/solar_wind_info/15min/sw_info_'+string(orbit,format='(i05)')+'.sav'
  file_sw_m1 = SAVE_LOC + '/maven/sav/solar_wind_info/15min/sw_info_'+string(orbit-1,format='(i05)')+'.sav'
  file_sw_p1 = SAVE_LOC + '/maven/sav/solar_wind_info/15min/sw_info_'+string(orbit+1,format='(i05)')+'.sav'
  ft_sw = file_test(file_sw)
  ft_sw_m1 = file_test(file_sw_m1)
  ft_sw_p1 = file_test(file_sw_p1)
  if ft_sw eq 0 or ft_sw_m1 eq 0 or ft_sw_p1 eq 0 then goto, no_save

  restore,file_sw
  bs_time = dat_sw.time
  restore,file_sw_m1
  bs_time_m1 = dat_sw.time
  restore,file_sw_p1
  bs_time_p1 = dat_sw.time
  
  
  if not keyword_set(trange) then get_timespan,trange
  ;if keyword_set(spice_load) then mvn_spice_load,trange=trange
  mvn_sta_l2_load,sta_apid=['c6','d0','d1'],trange=trange
  mvn_sta_l2_tplot
  
  if get_apid eq 'd0' then begin
     common mvn_d0,mvn_d0_ind,mvn_d0_dat
     mtime = (mvn_d0_dat.time + mvn_d0_dat.end_time)/2d
     tres = mvn_d0_dat.end_time-mvn_d0_dat.time;time[1] - time[0]
  endif 
  
  if get_apid eq 'd1' then begin
    common mvn_d1,mvn_d1_ind,mvn_d1_dat
    mtime = (mvn_d1_dat.time + mvn_d1_dat.end_time)/2d
    tres = mvn_d1_dat.end_time-mvn_d1_dat.time;time[1] - time[0]
  endif

  ;get_data,'mvn_sta_d0_M',data=d
  ;if keyword_set(highres) then get_data,'mvn_sta_d1_E',data=d
  ;sz = size(d,/type)
  sz = size(mvn_d0_dat,/type)
  if sz ne 8 then begin
     print,'No data, returning.'
     data_ok = 0
     return
  endif 
  
;  if keyword_set(event) then begin
;    ;if not keyword_set(highres) then idx = nn('mvn_sta_d0_E',trange) else idx = nn('mvn_sta_d1_E',trange)
;    idx = nn(mtime,trange)
;    time = d.x[idx[0]:idx[1]]
;  endif else begin
    time = mtime;d.x
;  endelse
  
  
  
   ;;load position (not ploting)
  maven_state_mso,pos_mvn,vel_mvn,time_pos,trange=trange
  R_m = 3389.9D
  pos_mvn2 = pos_mvn/R_m  
  
  ;;find nerest number (Hara-kun method)
  w = value_locate(time_pos,time)
  w1 = (w>0) < (n_elements(time_pos)-1)
  w2 = ((w1+1)>0) < (n_elements(time_pos)-1)
  dt1 = abs(time-time_pos[w1])
  dt2 = abs(time-time_pos[w2])
  w = [[w1],[w2]]
  dt = [[dt1],[dt2]]
  mndt = min(dt,dim=2,imin)
  n2 = w[imin]
  
  ;;difine strcture array and path to save
  nsav = 0
  dat_sav_arr = 0.
  if keyword_set(highres) then begin
    if keyword_set(event) then save_path = SAVE_LOC+'/maven/sav/flux/deflect_ion_rmv_ring/d1' $
    else save_path = SAVE_LOC+'/maven/sav/flux/deflect_ion_rmv_ring/d1
  endif else begin
    if keyword_set(event) then save_path = SAVE_LOC+'/maven/sav/flux/deflect_ion_rmv_ring/d0' $
    else save_path = SAVE_LOC+'/maven/sav/flux/deflect_ion_rmv_ring/d0
  endelse
  
  
  for it = 0,n_elements(time)-1 do begin
    date_s = strmid(time_string(time[it]),0,4)+strmid(time_string(time[it]),5,2)+strmid(time_string(time[it]),8,2)+ $
             strmid(time_string(time[it]),11,2)+strmid(time_string(time[it]),14,2)+strmid(time_string(time[it]),17,2)   
    tspan = [time[it]-tres[it]/2d,time[it]+tres[it]/2d] 
    nvb = get_swimag_nvb(tspan)                            
    field = get_local_field_dir(tspan)
    rot = get_rot_angle(field.V, field.B)
    pos = [pos_mvn2[n2[it],0],pos_mvn2[n2[it],1],pos_mvn2[n2[it],2]]
    pos_mse = mso2mse(pos[0],pos[1],pos[2],rot)  
    if (time[it] lt bs_time_m1[1]) or (time[it] gt bs_time[0] and time[it] lt bs_time[1]) or (time[it] gt bs_time_p1[0]) then goto,no_data
    if field.E[0] eq 0 and field.E[1] eq 0 and field.E[2] eq 0 then begin
      dprint,'No local field data, skip this time range.'
      goto, nofield
    endif

    wi,1;,wsize=[1800,1200]
        if keyword_set(ps2pdf) or keyword_set(ps2png) then begin
          if not keyword_set(highres) then ps_path = SAVE_LOC+'/maven/ps/vdf_reflect_rmv_ring/normal/oxy/'+string(orbit,format='(i05)') $
                                      else ps_path = SAVE_LOC+'/maven/ps/vdf_reflect_rmv_ring/highres/oxy/'+string(orbit,format='(i05)')
          if file_test(ps_path) eq 0 then file_mkdir,ps_path
          popen,ps_path+'/vdf_reflect_'+date_s+'.eps',xsize=40,ysize=40,unit='cm',/enc
        endif
    !p.multi=[0,4,5]
   
    if keyword_set(plot_vdf) then begin      
      plot_mvn_orbit,orbit=orbit,/xy,_extra=extra
      plot_mvn_orbit,tspan,/xy,/oplot,/seconds,Evec=field.E,Bvec=field.B,Vvec=field.V
      plot_mvn_orbit,orbit=orbit,/xz,_extra=extra
      plot_mvn_orbit,tspan,/xz,/oplot,/seconds,Evec=field.E,Bvec=field.B,Vvec=field.V
      plot_mvn_orbit,orbit=orbit,/yz,_extra=extra
      plot_mvn_orbit,tspan,/yz,/oplot,/seconds,Evec=field.E,Bvec=field.B,Vvec=field.V
      plot_mvn_orbit,orbit=orbit,/cyl,_extra=extra
      plot_mvn_orbit,tspan,/cyl,/oplot,/seconds,/show_sza,/show_alt
      
      plot_mvn_orbit,orbit=orbit,/xy,_extra=extra,frame='MSE',angle=rot
      plot_mvn_orbit,tspan,/xy,/oplot,/seconds,Evec=field.E,Bvec=field.B,Vvec=field.V,frame='MSE',angle=rot
      plot_mvn_orbit,orbit=orbit,/xz,_extra=extra,frame='MSE',angle=rot
      plot_mvn_orbit,tspan,/xz,/oplot,/seconds,Evec=field.E,Bvec=field.B,Vvec=field.V,frame='MSE',angle=rot
      plot_mvn_orbit,orbit=orbit,/yz,_extra=extra,frame='MSE',angle=rot
      plot_mvn_orbit,tspan,/yz,/oplot,/seconds,Evec=field.E,Bvec=field.B,Vvec=field.V,frame='MSE',angle=rot
      plot_mvn_orbit,orbit=orbit,/cyl,_extra=extra
      plot_mvn_orbit,tspan,/cyl,/oplot,/seconds,/show_sza,/show_alt
    endif 
    
    if not keyword_set(get_apid) then get_apid = 'd0'
    result = execute('dat = mvn_sta_get(get_apid,tt=tspan)')
    if get_apid eq 'd1' then if result eq 0 then result = execute('dat = mvn_sta_get("d0",tt=tspan)')
    sta_getdat_bgrm,dat,dat_new,ratio=0.5
    ;if dat_new.end_time - dat_new.time gt 1.2*tres then stop
    
    if keyword_set(plot_vdf) then $
    flux_vdf7_v6,dat_new,nvb,pos,$
                 sta_apid=get_apid,mass_assmpt=16.,mass_range=[12,20],angle=[-45,45],units='df',$
                 plot_vdf=plot_vdf,mult='4,5',panel_mode=3,_extra={charsize:1.5},$
                 nbin=800,vres=2.,/show_ring,/surf

;;NO REMOVAL from ring dist.                 
    flux_vdf7_v6,dat_new,nvb,pos,$
                 flux_sav=flux_sav_out_sh_0,$
                 sta_apid=get_apid,mass_assmpt=16.,mass_range=[12,20],angle=[-45,45],units='df',$
                 mult='4,5',panel_mode=1,_extra={charsize:1.5},$
                 nbin=800,vres=2.,/show_ring,/surf,$
                 vsh_dir=1

    flux_vdf7_v6,dat_new,nvb,pos,$
                 flux_sav=flux_sav_in_sh_0,$
                 sta_apid=get_apid,mass_assmpt=16.,mass_range=[12,20],angle=[-45,45],units='df',$
                 mult='4,5',panel_mode=2,_extra={charsize:1.5},$
                 nbin=800,vres=2.,/show_ring,/surf,$
                 vsh_dir=-1

;;REMOVAL from 25% wide ring dist.
               flux_vdf7_v6,dat_new,nvb,pos,$
                 flux_sav=flux_sav_out_sh_25,$
                 sta_apid=get_apid,mass_assmpt=16.,mass_range=[12,20],angle=[-45,45],units='df',$
                 plot_vdf=plot_vdf,mult='4,5',panel_mode=1,_extra={charsize:1.5},$
                 nbin=800,vres=2.,/show_ring,/surf,$
                 vsh_dir=1,/rmv_ring,ring_range=[0.75,1.25]

               flux_vdf7_v6,dat_new,nvb,pos,$
                 flux_sav=flux_sav_in_sh_25,$
                 sta_apid=get_apid,mass_assmpt=16.,mass_range=[12,20],angle=[-45,45],units='df',$
                 plot_vdf=plot_vdf,mult='4,5',panel_mode=2,_extra={charsize:1.5},$
                 nbin=800,vres=2.,/show_ring,/surf,$
                 vsh_dir=-1,/rmv_ring,ring_range=[0.75,1.25]

;;REMOVAL from 50% wide ring dist.
    flux_vdf7_v6,dat_new,nvb,pos,$
                 flux_sav=flux_sav_out_sh_50,$
                 sta_apid=get_apid,mass_assmpt=16.,mass_range=[12,20],angle=[-45,45],units='df',$
                 plot_vdf=plot_vdf,mult='4,5',panel_mode=1,_extra={charsize:1.5},$
                 nbin=800,vres=2.,/show_ring,/surf,$
                 vsh_dir=1,/rmv_ring,ring_range=[0.5,1.5]

    flux_vdf7_v6,dat_new,nvb,pos,$
                 flux_sav=flux_sav_in_sh_50,$
                 sta_apid=get_apid,mass_assmpt=16.,mass_range=[12,20],angle=[-45,45],units='df',$
                 plot_vdf=plot_vdf,mult='4,5',panel_mode=2,_extra={charsize:1.5},$
                 nbin=800,vres=2.,/show_ring,/surf,$
                 vsh_dir=-1,/rmv_ring,ring_range=[0.5,1.5]

;;REMOVAL from 75% wide ring dist.
    flux_vdf7_v6,dat_new,nvb,pos,$
                 flux_sav=flux_sav_out_sh_75,$
                 sta_apid=get_apid,mass_assmpt=16.,mass_range=[12,20],angle=[-45,45],units='df',$
                 mult='4,5',panel_mode=1,_extra={charsize:1.5},$
                 nbin=800,vres=2.,/show_ring,/surf,$
                 vsh_dir=1,/rmv_ring,ring_range=[0.25,1.75]

    flux_vdf7_v6,dat_new,nvb,pos,$
                 flux_sav=flux_sav_in_sh_75,$
                 sta_apid=get_apid,mass_assmpt=16.,mass_range=[12,20],angle=[-45,45],units='df',$
                 mult='4,5',panel_mode=2,_extra={charsize:1.5},$
                 nbin=800,vres=2.,/show_ring,/surf,$
                 vsh_dir=-1,/rmv_ring,ring_range=[0.25,1.75]


;;REMOVAL from 100% wide ring dist.
    flux_vdf7_v6,dat_new,nvb,pos,$
                 flux_sav=flux_sav_out_sh_100,$
                 sta_apid=get_apid,mass_assmpt=16.,mass_range=[12,20],angle=[-45,45],units='df',$
                 mult='4,5',panel_mode=1,_extra={charsize:1.5},$
                 nbin=800,vres=2.,/show_ring,/surf,$
                 vsh_dir=1,/rmv_ring,ring_range=[0.,2.]

    flux_vdf7_v6,dat_new,nvb,pos,$
                 flux_sav=flux_sav_in_sh_100,$
                 sta_apid=get_apid,mass_assmpt=16.,mass_range=[12,20],angle=[-45,45],units='df',$
                 mult='4,5',panel_mode=2,_extra={charsize:1.5},$
                 nbin=800,vres=2.,/show_ring,/surf,$
                 vsh_dir=-1,/rmv_ring,ring_range=[0.,2.]
                                                                
    
    if keyword_set(ps2pdf) then begin
      pclose
      if keyword_set(highres) then begin
        if keyword_set(event) then begin
          spawn,'pstopdf '+SAVE_LOC+'/maven/ps/vdf_reflect_rmv_ring/highres/event/'+string(orbit,format='(i05)')+'/vdf_reflect_'+date_s+'.eps'
          spawn,'rm ' + SAVE_LOC+'/maven/ps/vdf_reflect_rmv_ring/higres/event/'+string(orbit,format='(i05)')+'/vdf_reflect_'+date_s+'.eps'
        endif else begin
          spawn,'pstopdf '+SAVE_LOC+'/maven/ps/vdf_reflect_rmv_ring/highres/oxy/'+string(orbit,format='(i05)')+'/vdf_reflect_'+date_s+'.eps'
          spawn,'rm ' + SAVE_LOC+'/maven/ps/vdf_reflect_rmv_ring/higres/oxy/'+string(orbit,format='(i05)')+'/vdf_reflect_'+date_s+'.eps'
        endelse
      endif else begin
        if keyword_set(event) then begin
          spawn,'pstopdf '+SAVE_LOC+'/maven/ps/vdf_reflect_rmv_ring/normal/event/'+string(orbit,format='(i05)')+'/vdf_reflect_'+date_s+'.eps'
          spawn,'rm ' + SAVE_LOC+'/maven/ps/vdf_reflect_rmv_ring/normal/event/'+string(orbit,format='(i05)')+'/vdf_reflect_'+date_s+'.eps'
        endif else begin
          spawn,'pstopdf '+SAVE_LOC+'/maven/ps/vdf_reflect_rmv_ring/normal/oxy/'+string(orbit,format='(i05)')+'/vdf_reflect_'+date_s+'.eps'
          spawn,'rm ' + SAVE_LOC+'/maven/ps/vdf_reflect_rmv_ring/normal/oxy/'+string(orbit,format='(i05)')+'/vdf_reflect_'+date_s+'.eps'
        endelse
      endelse
    endif
    
    if keyword_set(ps2png) then begin
      pclose
      if keyword_set(highres) then begin
        if keyword_set(event) then png_path = SAVE_LOC+'/maven/png/vdf_reflect_rmv_ring/highres/event/'+string(orbit,format='(i05)') $
        else png_path = SAVE_LOC+'/maven/png/vdf_reflect_rmv_ring/highres/oxy/'+string(orbit,format='(i05)')
      endif else begin
        if keyword_set(event) then png_path = SAVE_LOC+'/maven/png/vdf_reflect_rmv_ring/normal/event/'+string(orbit,format='(i05)') $
        else png_path = SAVE_LOC+'/maven/png/vdf_reflect_rmv_ring/normal/oxy/'+string(orbit,format='(i05)')
      endelse
      if file_test(png_path) eq 0 then file_mkdir,png_path
        cd,ps_path       
        spawn,'convert vdf_reflect_'+date_s+'.eps vdf_reflect_'+date_s+'.png'
        spawn,'rm ' + 'vdf_reflect_'+date_s+'.eps'
        cd,current_dir
    endif
    ;if pos[0] gt 0 then stop
   
    if size(flux_sav_out_sh_0,/type) ne 8 then goto, no_data 
 
     dat_sav = {Stime:flux_sav_out_sh_0.time[0], Etime: flux_sav_out_sh_0.time[1], Pos_mso:pos, Pos_mse:transpose(flux_sav_out_sh_0.pos_mse), $
                theta_bs:flux_sav_out_sh_0.theta_bs, phi_bs:flux_sav_out_sh_0.phi_bs, bin_theta_phi:flux_sav_out_sh_0.bin_theta_phi,$
                
                Odens_100eV_out_0:flux_sav_out_sh_0.Odens_100eV, Ovel_mso_100eV_out_0:flux_sav_out_sh_0.Ovel_mso_100eV, Ovel_mse_100eV_out_0:flux_sav_out_sh_0.Ovel_mse_100eV, Ovel_sh_0_100eV_out_0:flux_sav_out_sh_0.Ovel_sh_100eV,$
                Odens_1keV_out_0:flux_sav_out_sh_0.Odens_1keV, Ovel_mso_1keV_out_0:flux_sav_out_sh_0.Ovel_mso_1keV, Ovel_mse_1keV_out_0:flux_sav_out_sh_0.Ovel_mse_1keV, Ovel_sh_0_1keV_out_0:flux_sav_out_sh_0.Ovel_sh_1keV,$
                Odens_5keV_out_0:flux_sav_out_sh_0.Odens_5keV, Ovel_mso_5keV_out_0:flux_sav_out_sh_0.Ovel_mso_5keV, Ovel_mse_5keV_out_0:flux_sav_out_sh_0.Ovel_mse_5keV, Ovel_sh_0_5keV_out_0:flux_sav_out_sh_0.Ovel_sh_5keV,$
                Odens_10keV_out_0:flux_sav_out_sh_0.Odens_10keV, Ovel_mso_10keV_out_0:flux_sav_out_sh_0.Ovel_mso_10keV, Ovel_mse_10keV_out_0:flux_sav_out_sh_0.Ovel_mse_10keV, Ovel_sh_0_10keV_out_0:flux_sav_out_sh_0.Ovel_sh_10keV,$
                Odens_20keV_out_0:flux_sav_out_sh_0.Odens_20keV, Ovel_mso_20keV_out_0:flux_sav_out_sh_0.Ovel_mso_20keV, Ovel_mse_20keV_out_0:flux_sav_out_sh_0.Ovel_mse_20keV, Ovel_sh_0_20keV_out_0:flux_sav_out_sh_0.Ovel_sh_20keV,$
                Odens_gt1keV_out_0:flux_sav_out_sh_0.Odens_gt1keV, Ovel_mso_gt1keV_out_0:flux_sav_out_sh_0.Ovel_mso_gt1keV, Ovel_mse_gt1keV_out_0:flux_sav_out_sh_0.Ovel_mse_gt1keV, Ovel_sh_0_gt1keV_out_0:flux_sav_out_sh_0.Ovel_sh_gt1keV,$
                Odens_gt5keV_out_0:flux_sav_out_sh_0.Odens_gt5keV, Ovel_mso_gt5keV_out_0:flux_sav_out_sh_0.Ovel_mso_gt5keV, Ovel_mse_gt5keV_out_0:flux_sav_out_sh_0.Ovel_mse_gt5keV, Ovel_sh_0_gt5keV_out_0:flux_sav_out_sh_0.Ovel_sh_gt5keV,$
                Odens_gt10keV_out_0:flux_sav_out_sh_0.Odens_gt10keV, Ovel_mso_gt10keV_out_0:flux_sav_out_sh_0.Ovel_mso_gt10keV, Ovel_mse_gt10keV_out_0:flux_sav_out_sh_0.Ovel_mse_gt10keV, Ovel_sh_0_gt10keV_out_0:flux_sav_out_sh_0.Ovel_sh_gt10keV,$
                Odens_all_out_0:flux_sav_out_sh_0.Odens_all, Ovel_mso_all_out_0:flux_sav_out_sh_0.Ovel_mso_all, Ovel_mse_all_out_0:flux_sav_out_sh_0.Ovel_mse_all, Ovel_sh_0_all_out_0:flux_sav_out_sh_0.Ovel_sh_all,$
                
                Odens_100eV_in_0:flux_sav_in_sh_0.Odens_100eV, Ovel_mso_100eV_in_0:flux_sav_in_sh_0.Ovel_mso_100eV, Ovel_mse_100eV_in_0:flux_sav_in_sh_0.Ovel_mse_100eV, Ovel_sh_0_100eV_in_0:flux_sav_in_sh_0.Ovel_sh_100eV,$
                Odens_1keV_in_0:flux_sav_in_sh_0.Odens_1keV, Ovel_mso_1keV_in_0:flux_sav_in_sh_0.Ovel_mso_1keV, Ovel_mse_1keV_in_0:flux_sav_in_sh_0.Ovel_mse_1keV, Ovel_sh_0_1keV_in_0:flux_sav_in_sh_0.Ovel_sh_1keV,$
                Odens_5keV_in_0:flux_sav_in_sh_0.Odens_5keV, Ovel_mso_5keV_in_0:flux_sav_in_sh_0.Ovel_mso_5keV, Ovel_mse_5keV_in_0:flux_sav_in_sh_0.Ovel_mse_5keV, Ovel_sh_0_5keV_in_0:flux_sav_in_sh_0.Ovel_sh_5keV,$
                Odens_10keV_in_0:flux_sav_in_sh_0.Odens_10keV, Ovel_mso_10keV_in_0:flux_sav_in_sh_0.Ovel_mso_10keV, Ovel_mse_10keV_in_0:flux_sav_in_sh_0.Ovel_mse_10keV, Ovel_sh_0_10keV_in_0:flux_sav_in_sh_0.Ovel_sh_10keV,$
                Odens_20keV_in_0:flux_sav_in_sh_0.Odens_20keV, Ovel_mso_20keV_in_0:flux_sav_in_sh_0.Ovel_mso_20keV, Ovel_mse_20keV_in_0:flux_sav_in_sh_0.Ovel_mse_20keV, Ovel_sh_0_20keV_in_0:flux_sav_in_sh_0.Ovel_sh_20keV,$
                Odens_gt1keV_in_0:flux_sav_in_sh_0.Odens_gt1keV, Ovel_mso_gt1keV_in_0:flux_sav_in_sh_0.Ovel_mso_gt1keV, Ovel_mse_gt1keV_in_0:flux_sav_in_sh_0.Ovel_mse_gt1keV, Ovel_sh_0_gt1keV_in_0:flux_sav_in_sh_0.Ovel_sh_gt1keV,$
                Odens_gt5keV_in_0:flux_sav_in_sh_0.Odens_gt5keV, Ovel_mso_gt5keV_in_0:flux_sav_in_sh_0.Ovel_mso_gt5keV, Ovel_mse_gt5keV_in_0:flux_sav_in_sh_0.Ovel_mse_gt5keV, Ovel_sh_0_gt5keV_in_0:flux_sav_in_sh_0.Ovel_sh_gt5keV,$
                Odens_gt10keV_in_0:flux_sav_in_sh_0.Odens_gt10keV, Ovel_mso_gt10keV_in_0:flux_sav_in_sh_0.Ovel_mso_gt10keV, Ovel_mse_gt10keV_in_0:flux_sav_in_sh_0.Ovel_mse_gt10keV, Ovel_sh_0_gt10keV_in_0:flux_sav_in_sh_0.Ovel_sh_gt10keV,$
                Odens_all_in_0:flux_sav_in_sh_0.Odens_all, Ovel_mso_all_in_0:flux_sav_in_sh_0.Ovel_mso_all, Ovel_mse_all_in_0:flux_sav_in_sh_0.Ovel_mse_all, Ovel_sh_0_all_in_0:flux_sav_in_sh_0.Ovel_sh_all,$
                
                
                Odens_100eV_out_25:flux_sav_out_sh_25.Odens_100eV, Ovel_mso_100eV_out_25:flux_sav_out_sh_25.Ovel_mso_100eV, Ovel_mse_100eV_out_25:flux_sav_out_sh_25.Ovel_mse_100eV, Ovel_sh_25_100eV_out_25:flux_sav_out_sh_25.Ovel_sh_100eV,$
                Odens_1keV_out_25:flux_sav_out_sh_25.Odens_1keV, Ovel_mso_1keV_out_25:flux_sav_out_sh_25.Ovel_mso_1keV, Ovel_mse_1keV_out_25:flux_sav_out_sh_25.Ovel_mse_1keV, Ovel_sh_25_1keV_out_25:flux_sav_out_sh_25.Ovel_sh_1keV,$
                Odens_5keV_out_25:flux_sav_out_sh_25.Odens_5keV, Ovel_mso_5keV_out_25:flux_sav_out_sh_25.Ovel_mso_5keV, Ovel_mse_5keV_out_25:flux_sav_out_sh_25.Ovel_mse_5keV, Ovel_sh_25_5keV_out_25:flux_sav_out_sh_25.Ovel_sh_5keV,$
                Odens_10keV_out_25:flux_sav_out_sh_25.Odens_10keV, Ovel_mso_10keV_out_25:flux_sav_out_sh_25.Ovel_mso_10keV, Ovel_mse_10keV_out_25:flux_sav_out_sh_25.Ovel_mse_10keV, Ovel_sh_25_10keV_out_25:flux_sav_out_sh_25.Ovel_sh_10keV,$
                Odens_20keV_out_25:flux_sav_out_sh_25.Odens_20keV, Ovel_mso_20keV_out_25:flux_sav_out_sh_25.Ovel_mso_20keV, Ovel_mse_20keV_out_25:flux_sav_out_sh_25.Ovel_mse_20keV, Ovel_sh_25_20keV_out_25:flux_sav_out_sh_25.Ovel_sh_20keV,$
                Odens_gt1keV_out_25:flux_sav_out_sh_25.Odens_gt1keV, Ovel_mso_gt1keV_out_25:flux_sav_out_sh_25.Ovel_mso_gt1keV, Ovel_mse_gt1keV_out_25:flux_sav_out_sh_25.Ovel_mse_gt1keV, Ovel_sh_25_gt1keV_out_25:flux_sav_out_sh_25.Ovel_sh_gt1keV,$
                Odens_gt5keV_out_25:flux_sav_out_sh_25.Odens_gt5keV, Ovel_mso_gt5keV_out_25:flux_sav_out_sh_25.Ovel_mso_gt5keV, Ovel_mse_gt5keV_out_25:flux_sav_out_sh_25.Ovel_mse_gt5keV, Ovel_sh_25_gt5keV_out_25:flux_sav_out_sh_25.Ovel_sh_gt5keV,$
                Odens_gt10keV_out_25:flux_sav_out_sh_25.Odens_gt10keV, Ovel_mso_gt10keV_out_25:flux_sav_out_sh_25.Ovel_mso_gt10keV, Ovel_mse_gt10keV_out_25:flux_sav_out_sh_25.Ovel_mse_gt10keV, Ovel_sh_25_gt10keV_out_25:flux_sav_out_sh_25.Ovel_sh_gt10keV,$
                Odens_all_out_25:flux_sav_out_sh_25.Odens_all, Ovel_mso_all_out_25:flux_sav_out_sh_25.Ovel_mso_all, Ovel_mse_all_out_25:flux_sav_out_sh_25.Ovel_mse_all, Ovel_sh_25_all_out_25:flux_sav_out_sh_25.Ovel_sh_all,$

                Odens_100eV_in_25:flux_sav_in_sh_25.Odens_100eV, Ovel_mso_100eV_in_25:flux_sav_in_sh_25.Ovel_mso_100eV, Ovel_mse_100eV_in_25:flux_sav_in_sh_25.Ovel_mse_100eV, Ovel_sh_25_100eV_in_25:flux_sav_in_sh_25.Ovel_sh_100eV,$
                Odens_1keV_in_25:flux_sav_in_sh_25.Odens_1keV, Ovel_mso_1keV_in_25:flux_sav_in_sh_25.Ovel_mso_1keV, Ovel_mse_1keV_in_25:flux_sav_in_sh_25.Ovel_mse_1keV, Ovel_sh_25_1keV_in_25:flux_sav_in_sh_25.Ovel_sh_1keV,$
                Odens_5keV_in_25:flux_sav_in_sh_25.Odens_5keV, Ovel_mso_5keV_in_25:flux_sav_in_sh_25.Ovel_mso_5keV, Ovel_mse_5keV_in_25:flux_sav_in_sh_25.Ovel_mse_5keV, Ovel_sh_25_5keV_in_25:flux_sav_in_sh_25.Ovel_sh_5keV,$
                Odens_10keV_in_25:flux_sav_in_sh_25.Odens_10keV, Ovel_mso_10keV_in_25:flux_sav_in_sh_25.Ovel_mso_10keV, Ovel_mse_10keV_in_25:flux_sav_in_sh_25.Ovel_mse_10keV, Ovel_sh_25_10keV_in_25:flux_sav_in_sh_25.Ovel_sh_10keV,$
                Odens_20keV_in_25:flux_sav_in_sh_25.Odens_20keV, Ovel_mso_20keV_in_25:flux_sav_in_sh_25.Ovel_mso_20keV, Ovel_mse_20keV_in_25:flux_sav_in_sh_25.Ovel_mse_20keV, Ovel_sh_25_20keV_in_25:flux_sav_in_sh_25.Ovel_sh_20keV,$
                Odens_gt1keV_in_25:flux_sav_in_sh_25.Odens_gt1keV, Ovel_mso_gt1keV_in_25:flux_sav_in_sh_25.Ovel_mso_gt1keV, Ovel_mse_gt1keV_in_25:flux_sav_in_sh_25.Ovel_mse_gt1keV, Ovel_sh_25_gt1keV_in_25:flux_sav_in_sh_25.Ovel_sh_gt1keV,$
                Odens_gt5keV_in_25:flux_sav_in_sh_25.Odens_gt5keV, Ovel_mso_gt5keV_in_25:flux_sav_in_sh_25.Ovel_mso_gt5keV, Ovel_mse_gt5keV_in_25:flux_sav_in_sh_25.Ovel_mse_gt5keV, Ovel_sh_25_gt5keV_in_25:flux_sav_in_sh_25.Ovel_sh_gt5keV,$
                Odens_gt10keV_in_25:flux_sav_in_sh_25.Odens_gt10keV, Ovel_mso_gt10keV_in_25:flux_sav_in_sh_25.Ovel_mso_gt10keV, Ovel_mse_gt10keV_in_25:flux_sav_in_sh_25.Ovel_mse_gt10keV, Ovel_sh_25_gt10keV_in_25:flux_sav_in_sh_25.Ovel_sh_gt10keV,$
                Odens_all_in_25:flux_sav_in_sh_25.Odens_all, Ovel_mso_all_in_25:flux_sav_in_sh_25.Ovel_mso_all, Ovel_mse_all_in_25:flux_sav_in_sh_25.Ovel_mse_all, Ovel_sh_25_all_in_25:flux_sav_in_sh_25.Ovel_sh_all,$
                
                
                Odens_100eV_out_50:flux_sav_out_sh_50.Odens_100eV, Ovel_mso_100eV_out_50:flux_sav_out_sh_50.Ovel_mso_100eV, Ovel_mse_100eV_out_50:flux_sav_out_sh_50.Ovel_mse_100eV, Ovel_sh_50_100eV_out_50:flux_sav_out_sh_50.Ovel_sh_100eV,$
                Odens_1keV_out_50:flux_sav_out_sh_50.Odens_1keV, Ovel_mso_1keV_out_50:flux_sav_out_sh_50.Ovel_mso_1keV, Ovel_mse_1keV_out_50:flux_sav_out_sh_50.Ovel_mse_1keV, Ovel_sh_50_1keV_out_50:flux_sav_out_sh_50.Ovel_sh_1keV,$
                Odens_5keV_out_50:flux_sav_out_sh_50.Odens_5keV, Ovel_mso_5keV_out_50:flux_sav_out_sh_50.Ovel_mso_5keV, Ovel_mse_5keV_out_50:flux_sav_out_sh_50.Ovel_mse_5keV, Ovel_sh_50_5keV_out_50:flux_sav_out_sh_50.Ovel_sh_5keV,$
                Odens_10keV_out_50:flux_sav_out_sh_50.Odens_10keV, Ovel_mso_10keV_out_50:flux_sav_out_sh_50.Ovel_mso_10keV, Ovel_mse_10keV_out_50:flux_sav_out_sh_50.Ovel_mse_10keV, Ovel_sh_50_10keV_out_50:flux_sav_out_sh_50.Ovel_sh_10keV,$
                Odens_20keV_out_50:flux_sav_out_sh_50.Odens_20keV, Ovel_mso_20keV_out_50:flux_sav_out_sh_50.Ovel_mso_20keV, Ovel_mse_20keV_out_50:flux_sav_out_sh_50.Ovel_mse_20keV, Ovel_sh_50_20keV_out_50:flux_sav_out_sh_50.Ovel_sh_20keV,$
                Odens_gt1keV_out_50:flux_sav_out_sh_50.Odens_gt1keV, Ovel_mso_gt1keV_out_50:flux_sav_out_sh_50.Ovel_mso_gt1keV, Ovel_mse_gt1keV_out_50:flux_sav_out_sh_50.Ovel_mse_gt1keV, Ovel_sh_50_gt1keV_out_50:flux_sav_out_sh_50.Ovel_sh_gt1keV,$
                Odens_gt5keV_out_50:flux_sav_out_sh_50.Odens_gt5keV, Ovel_mso_gt5keV_out_50:flux_sav_out_sh_50.Ovel_mso_gt5keV, Ovel_mse_gt5keV_out_50:flux_sav_out_sh_50.Ovel_mse_gt5keV, Ovel_sh_50_gt5keV_out_50:flux_sav_out_sh_50.Ovel_sh_gt5keV,$
                Odens_gt10keV_out_50:flux_sav_out_sh_50.Odens_gt10keV, Ovel_mso_gt10keV_out_50:flux_sav_out_sh_50.Ovel_mso_gt10keV, Ovel_mse_gt10keV_out_50:flux_sav_out_sh_50.Ovel_mse_gt10keV, Ovel_sh_50_gt10keV_out_50:flux_sav_out_sh_50.Ovel_sh_gt10keV,$
                Odens_all_out_50:flux_sav_out_sh_50.Odens_all, Ovel_mso_all_out_50:flux_sav_out_sh_50.Ovel_mso_all, Ovel_mse_all_out_50:flux_sav_out_sh_50.Ovel_mse_all, Ovel_sh_50_all_out_50:flux_sav_out_sh_50.Ovel_sh_all,$

                Odens_100eV_in_50:flux_sav_in_sh_50.Odens_100eV, Ovel_mso_100eV_in_50:flux_sav_in_sh_50.Ovel_mso_100eV, Ovel_mse_100eV_in_50:flux_sav_in_sh_50.Ovel_mse_100eV, Ovel_sh_50_100eV_in_50:flux_sav_in_sh_50.Ovel_sh_100eV,$
                Odens_1keV_in_50:flux_sav_in_sh_50.Odens_1keV, Ovel_mso_1keV_in_50:flux_sav_in_sh_50.Ovel_mso_1keV, Ovel_mse_1keV_in_50:flux_sav_in_sh_50.Ovel_mse_1keV, Ovel_sh_50_1keV_in_50:flux_sav_in_sh_50.Ovel_sh_1keV,$
                Odens_5keV_in_50:flux_sav_in_sh_50.Odens_5keV, Ovel_mso_5keV_in_50:flux_sav_in_sh_50.Ovel_mso_5keV, Ovel_mse_5keV_in_50:flux_sav_in_sh_50.Ovel_mse_5keV, Ovel_sh_50_5keV_in_50:flux_sav_in_sh_50.Ovel_sh_5keV,$
                Odens_10keV_in_50:flux_sav_in_sh_50.Odens_10keV, Ovel_mso_10keV_in_50:flux_sav_in_sh_50.Ovel_mso_10keV, Ovel_mse_10keV_in_50:flux_sav_in_sh_50.Ovel_mse_10keV, Ovel_sh_50_10keV_in_50:flux_sav_in_sh_50.Ovel_sh_10keV,$
                Odens_20keV_in_50:flux_sav_in_sh_50.Odens_20keV, Ovel_mso_20keV_in_50:flux_sav_in_sh_50.Ovel_mso_20keV, Ovel_mse_20keV_in_50:flux_sav_in_sh_50.Ovel_mse_20keV, Ovel_sh_50_20keV_in_50:flux_sav_in_sh_50.Ovel_sh_20keV,$
                Odens_gt1keV_in_50:flux_sav_in_sh_50.Odens_gt1keV, Ovel_mso_gt1keV_in_50:flux_sav_in_sh_50.Ovel_mso_gt1keV, Ovel_mse_gt1keV_in_50:flux_sav_in_sh_50.Ovel_mse_gt1keV, Ovel_sh_50_gt1keV_in_50:flux_sav_in_sh_50.Ovel_sh_gt1keV,$
                Odens_gt5keV_in_50:flux_sav_in_sh_50.Odens_gt5keV, Ovel_mso_gt5keV_in_50:flux_sav_in_sh_50.Ovel_mso_gt5keV, Ovel_mse_gt5keV_in_50:flux_sav_in_sh_50.Ovel_mse_gt5keV, Ovel_sh_50_gt5keV_in_50:flux_sav_in_sh_50.Ovel_sh_gt5keV,$
                Odens_gt10keV_in_50:flux_sav_in_sh_50.Odens_gt10keV, Ovel_mso_gt10keV_in_50:flux_sav_in_sh_50.Ovel_mso_gt10keV, Ovel_mse_gt10keV_in_50:flux_sav_in_sh_50.Ovel_mse_gt10keV, Ovel_sh_50_gt10keV_in_50:flux_sav_in_sh_50.Ovel_sh_gt10keV,$
                Odens_all_in_50:flux_sav_in_sh_50.Odens_all, Ovel_mso_all_in_50:flux_sav_in_sh_50.Ovel_mso_all, Ovel_mse_all_in_50:flux_sav_in_sh_50.Ovel_mse_all, Ovel_sh_50_all_in_50:flux_sav_in_sh_50.Ovel_sh_all,$                
                
                
                Odens_100eV_out_75:flux_sav_out_sh_75.Odens_100eV, Ovel_mso_100eV_out_75:flux_sav_out_sh_75.Ovel_mso_100eV, Ovel_mse_100eV_out_75:flux_sav_out_sh_75.Ovel_mse_100eV, Ovel_sh_75_100eV_out_75:flux_sav_out_sh_75.Ovel_sh_100eV,$
                Odens_1keV_out_75:flux_sav_out_sh_75.Odens_1keV, Ovel_mso_1keV_out_75:flux_sav_out_sh_75.Ovel_mso_1keV, Ovel_mse_1keV_out_75:flux_sav_out_sh_75.Ovel_mse_1keV, Ovel_sh_75_1keV_out_75:flux_sav_out_sh_75.Ovel_sh_1keV,$
                Odens_5keV_out_75:flux_sav_out_sh_75.Odens_5keV, Ovel_mso_5keV_out_75:flux_sav_out_sh_75.Ovel_mso_5keV, Ovel_mse_5keV_out_75:flux_sav_out_sh_75.Ovel_mse_5keV, Ovel_sh_75_5keV_out_75:flux_sav_out_sh_75.Ovel_sh_5keV,$
                Odens_10keV_out_75:flux_sav_out_sh_75.Odens_10keV, Ovel_mso_10keV_out_75:flux_sav_out_sh_75.Ovel_mso_10keV, Ovel_mse_10keV_out_75:flux_sav_out_sh_75.Ovel_mse_10keV, Ovel_sh_75_10keV_out_75:flux_sav_out_sh_75.Ovel_sh_10keV,$
                Odens_20keV_out_75:flux_sav_out_sh_75.Odens_20keV, Ovel_mso_20keV_out_75:flux_sav_out_sh_75.Ovel_mso_20keV, Ovel_mse_20keV_out_75:flux_sav_out_sh_75.Ovel_mse_20keV, Ovel_sh_75_20keV_out_75:flux_sav_out_sh_75.Ovel_sh_20keV,$
                Odens_gt1keV_out_75:flux_sav_out_sh_75.Odens_gt1keV, Ovel_mso_gt1keV_out_75:flux_sav_out_sh_75.Ovel_mso_gt1keV, Ovel_mse_gt1keV_out_75:flux_sav_out_sh_75.Ovel_mse_gt1keV, Ovel_sh_75_gt1keV_out_75:flux_sav_out_sh_75.Ovel_sh_gt1keV,$
                Odens_gt5keV_out_75:flux_sav_out_sh_75.Odens_gt5keV, Ovel_mso_gt5keV_out_75:flux_sav_out_sh_75.Ovel_mso_gt5keV, Ovel_mse_gt5keV_out_75:flux_sav_out_sh_75.Ovel_mse_gt5keV, Ovel_sh_75_gt5keV_out_75:flux_sav_out_sh_75.Ovel_sh_gt5keV,$
                Odens_gt10keV_out_75:flux_sav_out_sh_75.Odens_gt10keV, Ovel_mso_gt10keV_out_75:flux_sav_out_sh_75.Ovel_mso_gt10keV, Ovel_mse_gt10keV_out_75:flux_sav_out_sh_75.Ovel_mse_gt10keV, Ovel_sh_75_gt10keV_out_75:flux_sav_out_sh_75.Ovel_sh_gt10keV,$
                Odens_all_out_75:flux_sav_out_sh_75.Odens_all, Ovel_mso_all_out_75:flux_sav_out_sh_75.Ovel_mso_all, Ovel_mse_all_out_75:flux_sav_out_sh_75.Ovel_mse_all, Ovel_sh_75_all_out_75:flux_sav_out_sh_75.Ovel_sh_all,$

                Odens_100eV_in_75:flux_sav_in_sh_75.Odens_100eV, Ovel_mso_100eV_in_75:flux_sav_in_sh_75.Ovel_mso_100eV, Ovel_mse_100eV_in_75:flux_sav_in_sh_75.Ovel_mse_100eV, Ovel_sh_75_100eV_in_75:flux_sav_in_sh_75.Ovel_sh_100eV,$
                Odens_1keV_in_75:flux_sav_in_sh_75.Odens_1keV, Ovel_mso_1keV_in_75:flux_sav_in_sh_75.Ovel_mso_1keV, Ovel_mse_1keV_in_75:flux_sav_in_sh_75.Ovel_mse_1keV, Ovel_sh_75_1keV_in_75:flux_sav_in_sh_75.Ovel_sh_1keV,$
                Odens_5keV_in_75:flux_sav_in_sh_75.Odens_5keV, Ovel_mso_5keV_in_75:flux_sav_in_sh_75.Ovel_mso_5keV, Ovel_mse_5keV_in_75:flux_sav_in_sh_75.Ovel_mse_5keV, Ovel_sh_75_5keV_in_75:flux_sav_in_sh_75.Ovel_sh_5keV,$
                Odens_10keV_in_75:flux_sav_in_sh_75.Odens_10keV, Ovel_mso_10keV_in_75:flux_sav_in_sh_75.Ovel_mso_10keV, Ovel_mse_10keV_in_75:flux_sav_in_sh_75.Ovel_mse_10keV, Ovel_sh_75_10keV_in_75:flux_sav_in_sh_75.Ovel_sh_10keV,$
                Odens_20keV_in_75:flux_sav_in_sh_75.Odens_20keV, Ovel_mso_20keV_in_75:flux_sav_in_sh_75.Ovel_mso_20keV, Ovel_mse_20keV_in_75:flux_sav_in_sh_75.Ovel_mse_20keV, Ovel_sh_75_20keV_in_75:flux_sav_in_sh_75.Ovel_sh_20keV,$
                Odens_gt1keV_in_75:flux_sav_in_sh_75.Odens_gt1keV, Ovel_mso_gt1keV_in_75:flux_sav_in_sh_75.Ovel_mso_gt1keV, Ovel_mse_gt1keV_in_75:flux_sav_in_sh_75.Ovel_mse_gt1keV, Ovel_sh_75_gt1keV_in_75:flux_sav_in_sh_75.Ovel_sh_gt1keV,$
                Odens_gt5keV_in_75:flux_sav_in_sh_75.Odens_gt5keV, Ovel_mso_gt5keV_in_75:flux_sav_in_sh_75.Ovel_mso_gt5keV, Ovel_mse_gt5keV_in_75:flux_sav_in_sh_75.Ovel_mse_gt5keV, Ovel_sh_75_gt5keV_in_75:flux_sav_in_sh_75.Ovel_sh_gt5keV,$
                Odens_gt10keV_in_75:flux_sav_in_sh_75.Odens_gt10keV, Ovel_mso_gt10keV_in_75:flux_sav_in_sh_75.Ovel_mso_gt10keV, Ovel_mse_gt10keV_in_75:flux_sav_in_sh_75.Ovel_mse_gt10keV, Ovel_sh_75_gt10keV_in_75:flux_sav_in_sh_75.Ovel_sh_gt10keV,$
                Odens_all_in_75:flux_sav_in_sh_75.Odens_all, Ovel_mso_all_in_75:flux_sav_in_sh_75.Ovel_mso_all, Ovel_mse_all_in_75:flux_sav_in_sh_75.Ovel_mse_all, Ovel_sh_75_all_in_75:flux_sav_in_sh_75.Ovel_sh_all,$
                
                
                Odens_100eV_out_100:flux_sav_out_sh_100.Odens_100eV, Ovel_mso_100eV_out_100:flux_sav_out_sh_100.Ovel_mso_100eV, Ovel_mse_100eV_out_100:flux_sav_out_sh_100.Ovel_mse_100eV, Ovel_sh_100_100eV_out_100:flux_sav_out_sh_100.Ovel_sh_100eV,$
                Odens_1keV_out_100:flux_sav_out_sh_100.Odens_1keV, Ovel_mso_1keV_out_100:flux_sav_out_sh_100.Ovel_mso_1keV, Ovel_mse_1keV_out_100:flux_sav_out_sh_100.Ovel_mse_1keV, Ovel_sh_100_1keV_out_100:flux_sav_out_sh_100.Ovel_sh_1keV,$
                Odens_5keV_out_100:flux_sav_out_sh_100.Odens_5keV, Ovel_mso_5keV_out_100:flux_sav_out_sh_100.Ovel_mso_5keV, Ovel_mse_5keV_out_100:flux_sav_out_sh_100.Ovel_mse_5keV, Ovel_sh_100_5keV_out_100:flux_sav_out_sh_100.Ovel_sh_5keV,$
                Odens_10keV_out_100:flux_sav_out_sh_100.Odens_10keV, Ovel_mso_10keV_out_100:flux_sav_out_sh_100.Ovel_mso_10keV, Ovel_mse_10keV_out_100:flux_sav_out_sh_100.Ovel_mse_10keV, Ovel_sh_100_10keV_out_100:flux_sav_out_sh_100.Ovel_sh_10keV,$
                Odens_20keV_out_100:flux_sav_out_sh_100.Odens_20keV, Ovel_mso_20keV_out_100:flux_sav_out_sh_100.Ovel_mso_20keV, Ovel_mse_20keV_out_100:flux_sav_out_sh_100.Ovel_mse_20keV, Ovel_sh_100_20keV_out_100:flux_sav_out_sh_100.Ovel_sh_20keV,$
                Odens_gt1keV_out_100:flux_sav_out_sh_100.Odens_gt1keV, Ovel_mso_gt1keV_out_100:flux_sav_out_sh_100.Ovel_mso_gt1keV, Ovel_mse_gt1keV_out_100:flux_sav_out_sh_100.Ovel_mse_gt1keV, Ovel_sh_100_gt1keV_out_100:flux_sav_out_sh_100.Ovel_sh_gt1keV,$
                Odens_gt5keV_out_100:flux_sav_out_sh_100.Odens_gt5keV, Ovel_mso_gt5keV_out_100:flux_sav_out_sh_100.Ovel_mso_gt5keV, Ovel_mse_gt5keV_out_100:flux_sav_out_sh_100.Ovel_mse_gt5keV, Ovel_sh_100_gt5keV_out_100:flux_sav_out_sh_100.Ovel_sh_gt5keV,$
                Odens_gt10keV_out_100:flux_sav_out_sh_100.Odens_gt10keV, Ovel_mso_gt10keV_out_100:flux_sav_out_sh_100.Ovel_mso_gt10keV, Ovel_mse_gt10keV_out_100:flux_sav_out_sh_100.Ovel_mse_gt10keV, Ovel_sh_100_gt10keV_out_100:flux_sav_out_sh_100.Ovel_sh_gt10keV,$
                Odens_all_out_100:flux_sav_out_sh_100.Odens_all, Ovel_mso_all_out_100:flux_sav_out_sh_100.Ovel_mso_all, Ovel_mse_all_out_100:flux_sav_out_sh_100.Ovel_mse_all, Ovel_sh_100_all_out_100:flux_sav_out_sh_100.Ovel_sh_all,$

                Odens_100eV_in_100:flux_sav_in_sh_100.Odens_100eV, Ovel_mso_100eV_in_100:flux_sav_in_sh_100.Ovel_mso_100eV, Ovel_mse_100eV_in_100:flux_sav_in_sh_100.Ovel_mse_100eV, Ovel_sh_100_100eV_in_100:flux_sav_in_sh_100.Ovel_sh_100eV,$
                Odens_1keV_in_100:flux_sav_in_sh_100.Odens_1keV, Ovel_mso_1keV_in_100:flux_sav_in_sh_100.Ovel_mso_1keV, Ovel_mse_1keV_in_100:flux_sav_in_sh_100.Ovel_mse_1keV, Ovel_sh_100_1keV_in_100:flux_sav_in_sh_100.Ovel_sh_1keV,$
                Odens_5keV_in_100:flux_sav_in_sh_100.Odens_5keV, Ovel_mso_5keV_in_100:flux_sav_in_sh_100.Ovel_mso_5keV, Ovel_mse_5keV_in_100:flux_sav_in_sh_100.Ovel_mse_5keV, Ovel_sh_100_5keV_in_100:flux_sav_in_sh_100.Ovel_sh_5keV,$
                Odens_10keV_in_100:flux_sav_in_sh_100.Odens_10keV, Ovel_mso_10keV_in_100:flux_sav_in_sh_100.Ovel_mso_10keV, Ovel_mse_10keV_in_100:flux_sav_in_sh_100.Ovel_mse_10keV, Ovel_sh_100_10keV_in_100:flux_sav_in_sh_100.Ovel_sh_10keV,$
                Odens_20keV_in_100:flux_sav_in_sh_100.Odens_20keV, Ovel_mso_20keV_in_100:flux_sav_in_sh_100.Ovel_mso_20keV, Ovel_mse_20keV_in_100:flux_sav_in_sh_100.Ovel_mse_20keV, Ovel_sh_100_20keV_in_100:flux_sav_in_sh_100.Ovel_sh_20keV,$
                Odens_gt1keV_in_100:flux_sav_in_sh_100.Odens_gt1keV, Ovel_mso_gt1keV_in_100:flux_sav_in_sh_100.Ovel_mso_gt1keV, Ovel_mse_gt1keV_in_100:flux_sav_in_sh_100.Ovel_mse_gt1keV, Ovel_sh_100_gt1keV_in_100:flux_sav_in_sh_100.Ovel_sh_gt1keV,$
                Odens_gt5keV_in_100:flux_sav_in_sh_100.Odens_gt5keV, Ovel_mso_gt5keV_in_100:flux_sav_in_sh_100.Ovel_mso_gt5keV, Ovel_mse_gt5keV_in_100:flux_sav_in_sh_100.Ovel_mse_gt5keV, Ovel_sh_100_gt5keV_in_100:flux_sav_in_sh_100.Ovel_sh_gt5keV,$
                Odens_gt10keV_in_100:flux_sav_in_sh_100.Odens_gt10keV, Ovel_mso_gt10keV_in_100:flux_sav_in_sh_100.Ovel_mso_gt10keV, Ovel_mse_gt10keV_in_100:flux_sav_in_sh_100.Ovel_mse_gt10keV, Ovel_sh_100_gt10keV_in_100:flux_sav_in_sh_100.Ovel_sh_gt10keV,$
                Odens_all_in_100:flux_sav_in_sh_100.Odens_all, Ovel_mso_all_in_100:flux_sav_in_sh_100.Ovel_mso_all, Ovel_mse_all_in_100:flux_sav_in_sh_100.Ovel_mse_all, Ovel_sh_100_all_in_100:flux_sav_in_sh_100.Ovel_sh_all,$
                
                
                DENS:nvb.dens, DENS_STD:nvb.dens_std, $
                VMSO:nvb.vmso, VMSO_STD:nvb.vmso_std, $
                BMSO:nvb.bmso, BMSO_STD:nvb.bmso_std, $
                
                THETA_V:nvb.theta_v, THETA_V_STD:nvb.theta_v_std,$
                PHI_V:nvb.phi_v,$
                R_V:nvb.r_v, $
                ZTEST_V:nvb.ztest_v,$
                
                THETA_B:nvb.theta_b, THETA_B_STD:nvb.theta_b_std, $
                PHI_B:nvb.phi_b,$
                R_B:nvb.r_b, $
                ZTEST_B:nvb.ztest_b,$
                
                nflg:nvb.nflg, vflg:nvb.vflg, bflg:nvb.bflg}
               
                if size(dat_sav_arr,/type) ne 4 then nsav = nsav + 1
                if size(dat_sav_arr,/type) eq 4 then dat_sav_arr = replicate(dat_sav,n_elements(time))   
                for nfld=0,n_tags(dat_sav)-1 do dat_sav_arr[nsav].(nfld) = dat_sav.(nfld)
               ; if size(dat_sav,/type) ne 8 then 
               ; replicate(dat_sav, 
    
    no_data:
    nofield:
  endfor 
  
  dat_sav_arr = dat_sav_arr[0:nsav]
  if not keyword_set(plot_vdf) then if size(dat_sav_arr,/type) eq 8 then save,dat_sav_arr,file=save_path+'/flux_vdf_'+string(orbit,format='(i05)')+'.sav'
  no_save:

end