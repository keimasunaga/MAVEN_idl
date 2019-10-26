;+
; :Description:
;    This routine draws ion velocity distribution obtained from STATIC.
;    It also overplots ring distribution and its width.
;    Outward and inward VDFs as well as the original VDF are plotted.
;    example:
;    main_plot_sta_vdf_rmv_ring,451,get_apid='d0',ring_percentage=50,trange=['2014-12-23/01:55:07','2014-12-23/01:57:31'],time_upstrm=['2014-12-23/02:56:59','2014-12-23/02:59:23']
;
; ${parameters}
; orbit: orbit_mumber
;
; ${keywords}
; trange: time range of data to be output (VDF plot). During this range, a VDF is plotted in each time step.
; event: 
; highres: if you want to plot d1 data, set this keyword
; time_upstrm: Set time period to plot another ring distribution (i.e., upstream solar wind ring or downstrem magnetosheath ring)
; get_apid: STATIC apid, set d0 or d1
; ring_percentage: Width of the ring distribution. Set 25, 50, 75, or 100. Set 25 if width = +/-  0.25 * Vsw
; png: save png files
;
; ${Return values}
;
; ${Related routines}
;  plot_sta_vdf_rmv_ring.pro
;
; $Author: Kei Masunaga (@EPS, Univ. Tokyo)
;
; $Last modified Jan 31, 2017
;-

pro main_plot_sta_vdf_rmv_ring,orbit,trange=trange,event=event,highres=highres,time_upstrm=time_upstrm,$
                               get_apid=get_apid,ring_percentage=ring_percentage,$
                               png=png,ps2pdf=ps2pdf,ps2png=ps2png,_extra=extra
                               
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
  
  if keyword_set(ring_percentage) then ring_range = [1.-ring_percentage/100., 1.+ring_percentage/100.] else ring_range = [0.5, 1.5]
  if not keyword_set(trange) then get_timespan,trange  
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
    
    if keyword_set(time_upstrm) then begin
      tspan_upstrm = time_double(time_upstrm)
      nvb_upstrm = get_swimag_nvb(tspan_upstrm)
      field_upstrm = get_local_field_dir(tspan_upstrm)
    endif

    field = get_local_field_dir(tspan)
    rot = get_rot_angle(field.V, field.B)
    pos = [pos_mvn2[n2[it],0],pos_mvn2[n2[it],1],pos_mvn2[n2[it],2]]
    pos_mse = mso2mse(pos[0],pos[1],pos[2],rot)  
    if (time[it] lt bs_time_m1[1]) or (time[it] gt bs_time[0] and time[it] lt bs_time[1]) or (time[it] gt bs_time_p1[0]) then begin
      print,time_string(time), 'Nodata'
      goto,no_data
    endif
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
    
    if keyword_set(time_upstrm) then begin  
      plot_mvn_orbit_upstrm,orbit=orbit,/xy,_extra=extra
      plot_mvn_orbit_upstrm,tspan,/xy,/oplot,/seconds,Evec=field.E,Bvec=field.B,Vvec=field.V,time_upstrm=tspan_upstrm,Eusvec=field_upstrm.E,Busvec=field_upstrm.B,Vusvec=field_upstrm.V
      plot_mvn_orbit_upstrm,orbit=orbit,/xz,_extra=extra
      plot_mvn_orbit_upstrm,tspan,/xz,/oplot,/seconds,Evec=field.E,Bvec=field.B,Vvec=field.V,time_upstrm=tspan_upstrm,Eusvec=field_upstrm.E,Busvec=field_upstrm.B,Vusvec=field_upstrm.V
      plot_mvn_orbit_upstrm,orbit=orbit,/yz,_extra=extra
      plot_mvn_orbit_upstrm,tspan,/yz,/oplot,/seconds,Evec=field.E,Bvec=field.B,Vvec=field.V,time_upstrm=tspan_upstrm,Eusvec=field_upstrm.E,Busvec=field_upstrm.B,Vusvec=field_upstrm.V
      plot_mvn_orbit_upstrm,orbit=orbit,/cyl,_extra=extra
      plot_mvn_orbit_upstrm,tspan,/cyl,/oplot,/seconds,/show_sza,/show_alt
       
      plot_mvn_orbit_upstrm,orbit=orbit,/xy,_extra=extra,frame='MSE',angle=rot
      plot_mvn_orbit_upstrm,tspan,/xy,/oplot,/seconds,Evec=field.E,Bvec=field.B,Vvec=field.V,frame='MSE',angle=rot,time_upstrm=tspan_upstrm,Eusvec=field_upstrm.E,Busvec=field_upstrm.B,Vusvec=field_upstrm.V
      plot_mvn_orbit_upstrm,orbit=orbit,/xz,_extra=extra,frame='MSE',angle=rot
      plot_mvn_orbit_upstrm,tspan,/xz,/oplot,/seconds,Evec=field.E,Bvec=field.B,Vvec=field.V,frame='MSE',angle=rot,time_upstrm=tspan_upstrm,Eusvec=field_upstrm.E,Busvec=field_upstrm.B,Vusvec=field_upstrm.V
      plot_mvn_orbit_upstrm,orbit=orbit,/yz,_extra=extra,frame='MSE',angle=rot
      plot_mvn_orbit_upstrm,tspan,/yz,/oplot,/seconds,Evec=field.E,Bvec=field.B,Vvec=field.V,frame='MSE',angle=rot,time_upstrm=tspan_upstrm,Eusvec=field_upstrm.E,Busvec=field_upstrm.B,Vusvec=field_upstrm.V
      plot_mvn_orbit_upstrm,orbit=orbit,/cyl,_extra=extra
      plot_mvn_orbit_upstrm,tspan,/cyl,/oplot,/seconds,/show_sza,/show_alt
    endif else begin
      plot_mvn_orbit_upstrm,orbit=orbit,/xy,_extra=extra
      plot_mvn_orbit_upstrm,tspan,/xy,/oplot,/seconds,Evec=field.E,Bvec=field.B,Vvec=field.V
      plot_mvn_orbit_upstrm,orbit=orbit,/xz,_extra=extra
      plot_mvn_orbit_upstrm,tspan,/xz,/oplot,/seconds,Evec=field.E,Bvec=field.B,Vvec=field.V
      plot_mvn_orbit_upstrm,orbit=orbit,/yz,_extra=extra
      plot_mvn_orbit_upstrm,tspan,/yz,/oplot,/seconds,Evec=field.E,Bvec=field.B,Vvec=field.V
      plot_mvn_orbit_upstrm,orbit=orbit,/cyl,_extra=extra
      plot_mvn_orbit_upstrm,tspan,/cyl,/oplot,/seconds,/show_sza,/show_alt

      plot_mvn_orbit_upstrm,orbit=orbit,/xy,_extra=extra,frame='MSE',angle=rot
      plot_mvn_orbit_upstrm,tspan,/xy,/oplot,/seconds,Evec=field.E,Bvec=field.B,Vvec=field.V,frame='MSE',angle=rot
      plot_mvn_orbit_upstrm,orbit=orbit,/xz,_extra=extra,frame='MSE',angle=rot
      plot_mvn_orbit_upstrm,tspan,/xz,/oplot,/seconds,Evec=field.E,Bvec=field.B,Vvec=field.V,frame='MSE',angle=rot
      plot_mvn_orbit_upstrm,orbit=orbit,/yz,_extra=extra,frame='MSE',angle=rot
      plot_mvn_orbit_upstrm,tspan,/yz,/oplot,/seconds,Evec=field.E,Bvec=field.B,Vvec=field.V,frame='MSE',angle=rot
      plot_mvn_orbit_upstrm,orbit=orbit,/cyl,_extra=extra
      plot_mvn_orbit_upstrm,tspan,/cyl,/oplot,/seconds,/show_sza,/show_alt     
    endelse
    
    if not keyword_set(get_apid) then get_apid = 'd0'
    result = execute('dat = mvn_sta_get(get_apid,tt=tspan)')
    if get_apid eq 'd1' then if result eq 0 then result = execute('dat = mvn_sta_get("d0",tt=tspan)')
    sta_getdat_bgrm,dat,dat_new,ratio=0.5
    ;if dat_new.end_time - dat_new.time gt 1.2*tres then stop

    plot_sta_vdf_rmv_ring,dat_new,nvb,pos,nvb_upstrm=nvb_upstrm,$
                          sta_apid=get_apid,mass_assmpt=16.,mass_range=[12,20],angle=[-45,45],units='df',$
                          mult='4,5',panel_mode=3,_extra={charsize:1.5},$
                          nbin=800,vres=2.,/show_ring,/surf                 

    ;;REMOVAL from XX % wide ring dist.
    plot_sta_vdf_rmv_ring,dat_new,nvb,pos,nvb_upstrm=nvb_upstrm,$
                          sta_apid=get_apid,mass_assmpt=16.,mass_range=[12,20],angle=[-45,45],units='df',$
                          mult='4,5',panel_mode=1,_extra={charsize:1.5},$
                          nbin=800,vres=2.,/show_ring,/surf,$
                          vsh_dir=1,/rmv_ring,ring_range=ring_range

    plot_sta_vdf_rmv_ring,dat_new,nvb,pos,nvb_upstrm=nvb_upstrm,$
                          sta_apid=get_apid,mass_assmpt=16.,mass_range=[12,20],angle=[-45,45],units='df',$
                          mult='4,5',panel_mode=2,_extra={charsize:1.5},$
                          nbin=800,vres=2.,/show_ring,/surf,$
                          vsh_dir=-1,/rmv_ring,ring_range=ring_range
                                                                
    
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
    
    no_data:
    
    nofield:
  endfor 
  
  no_save:

end