function get_local_position,tspan


  get_data,'Xmso',data=Xmso
  get_data,'Ymso',data=Ymso
  get_data,'Zmso',data=Zmso

  if size(Xmso,/type) ne 8 or size(d_v,/type) ne 8 then pos = [0,0,0]

  idx_pos = nn('Xmso',tspan)
  time_idx_pos = Xmso.x[idx_pos]
  if time_idx_pos[1] - time_idx_pos[0] lt 100d then pos = [0,0,0]
  
  pos = [total(Xmso.y[idx_pos[0]:idx_pos[1],*],1)/(idx_pos[1]-idx_pos[0]+1.),$
         total(Ymso.y[idx_pos[0]:idx_pos[1],*],1)/(idx_pos[1]-idx_pos[0]+1.),$
         total(Zmso.y[idx_pos[0]:idx_pos[1],*],1)/(idx_pos[1]-idx_pos[0]+1.)]

  return,pos
  
end  

function get_local_field,tspan,orbit=orbit,highres=highres
  env = init_env()
  SAVE_LOC = env.SAVE_LOC

  ;  orbit = round(mvn_orbit_num(time=tspan))
  fn_swi = SAVE_LOC + '/maven/sav/swia_flg/orbit/swia_flg_'+string(round(orbit),format='(i05)')+'.sav'

  restore,fn_swi
  flg_ca = swiflg.flg_ca
  flg_cs = swiflg.flg_cs
  if n_elements(flg_ca) eq 1 then if flg_ca eq 1 then swiname = 'swica' else swiname = 'swics' 
  if n_elements(flg_ca) eq 2 then if flg_ca[0] eq 1 and flg_ca[1] eq 1 then swiname = 'swica' else swiname = 'swics' 
  
  get_data,'mvn_B_1sec_MAVEN_MSO',data=d_b
  get_data,'mvn_'+swiname+'_velocity_mso',data=d_v
  
  if size(d_b,/type) ne 8 or size(d_v,/type) ne 8 then return, {B:[0,0,0],V:[0,0,0],E:[0,0,0]}
  
  idx_b = nn('mvn_B_1sec_MAVEN_MSO',tspan)
  idx_v = nn('mvn_'+swiname+'_velocity_mso',tspan)
  
  time_idx_b = d_b.x[idx_b]
  time_idx_v = d_v.x[idx_v]
  
  print,'TIMESPAN',time_string(tspan)
  print,'MAG_TIME',time_string(time_idx_b)
  print,'SWI_TIME',time_string(time_idx_v)
  
  if not keyword_set(highres) then if time_idx_b[1] - time_idx_b[0] lt 100d or time_idx_v[1] - time_idx_v[0] lt 100d then return,{B:[0,0,0],V:[0,0,0],E:[0,0,0]}
  if keyword_set(highres) then if time_idx_b[1] - time_idx_b[0] lt 10d or time_idx_v[1] - time_idx_v[0] lt 10d then return,{B:[0,0,0],V:[0,0,0],E:[0,0,0]}

  Vvec = total(d_v.y[idx_v[0]:idx_v[1],*],1)/(idx_v[1]-idx_v[0]+1.)
  Bvec = total(d_b.y[idx_b[0]:idx_b[1],*],1)/(idx_b[1]-idx_b[0]+1.)
  Evec = crossp(Bvec,Vvec)
  Vvec_e = Vvec/total(Vvec^2)^.5
  Bvec_e = Bvec/total(Bvec^2)^.5
  Evec_e = Evec/total(Evec^2)^.5


  return,{B:Bvec_e, V:Vvec_e, E:Evec_e}

end


pro save_flux_vdf3,orbit,trange=trange,png=png,ps2pdf=ps2pdf,ps2png=ps2png,save=save,event=event,highres=highres,_extra=extra,get_apid=get_apid,$
                   plot_vdf=plot_vdf

  env = init_env()
  SAVE_LOC = env.SAVE_LOC
  SAVE_LOC_HEAVY = env.SAVE_LOC_HEAVY
  
  del_data,'*'
  if not keyword_set(sta_apid) then l2_load_apid = ['c6','d0','d1']
  if not keyword_set(get_apid) then get_apid = 'd0'
  tplot_saved_mvn_pfp,orbit=orbit
  if not keyword_set(trange) then get_timespan,trange
  mvn_spice_load_kei,trange=trange
  mvn_sta_l2_load,sta_apid=l2_load_apid
  mvn_sta_l2_tplot
  get_data,'mvn_sta_d0_M',data=d
  if keyword_set(highres) then get_data,'mvn_sta_d1_E',data=d
  
  sz = size(d,/type)
  if sz ne 8 then begin
     print,'No data, returning.'
     data_ok = 0
     return
  endif 
  
  if keyword_set(event) then begin
    if not keyword_set(highres) then idx = nn('mvn_sta_d0_M',trange) else idx = nn('mvn_sta_d1_E',trange)
    time = d.x[idx[0]:idx[1]]
  endif else begin
    time = d.x    
  endelse
 
  for it = 0,n_elements(time)-1 do begin
    
    date_s = strmid(time_string(time[it]),0,4)+strmid(time_string(time[it]),5,2)+strmid(time_string(time[it]),8,2)+ $
             strmid(time_string(time[it]),11,2)+strmid(time_string(time[it]),14,2)+strmid(time_string(time[it]),17,2)
    wi,1;,wsize=[1800,1200]
    if keyword_set(ps2pdf) or keyword_set(ps2png) then begin
      if not keyword_set(highres) then ps_path = SAVE_LOC+'/maven/ps/vdf_reflect/normal/oxy/'+string(orbit,format='(i05)') $
                                  else ps_path = SAVE_LOC+'/maven/ps/vdf_reflect/highres/oxy/'+string(orbit,format='(i05)')
      if file_test(ps_path) eq 0 then file_mkdir,ps_path
      popen,ps_path+'/vdf_reflect_'+date_s+'.eps',xsize=40,ysize=40,unit='cm',/enc
    endif
    !p.multi=[0,4,5]
    if not keyword_set(highres) then tspan = [time[it]-64d,time[it]+64d] $
                                else tspan = [time[it]-8d,time[it]+8d]
    field = get_local_field(tspan,orbit=orbit,highres=highres)
    rot = get_rot_angle(field.V, field.B)
    pos = get_local_position(tspan)
    pos_mse = mso2mse(pos[0],pos[1],pos[2],rot)
    
    if field.E[0] eq 0 and field.E[1] eq 0 and field.E[2] eq 0 then begin
      dprint,'No local field data, skip this time range.'
      goto, nofield
    endif
   
    if keyword_set(plot_vdf) then begin
      plot_mvn_orbit,orbit=orbit,/xy,_extra=extra,frame='MSE',angle=rot
      plot_mvn_orbit,tspan,/xy,/oplot,/seconds,Evec=field.E,Bvec=field.B,Vvec=field.V,frame='MSE',angle=rot
      plot_mvn_orbit,orbit=orbit,/xz,_extra=extra,frame='MSE',angle=rot
      plot_mvn_orbit,tspan,/xz,/oplot,/seconds,Evec=field.E,Bvec=field.B,Vvec=field.V,frame='MSE',angle=rot
      plot_mvn_orbit,orbit=orbit,/yz,_extra=extra,frame='MSE',angle=rot
      plot_mvn_orbit,tspan,/yz,/oplot,/seconds,Evec=field.E,Bvec=field.B,Vvec=field.V,frame='MSE',angle=rot
      plot_mvn_orbit,orbit=orbit,/cyl,_extra=extra
      plot_mvn_orbit,tspan,/cyl,/oplot,/seconds,/show_sza,/show_alt
    endif 
    
    result = execute('dat = mvn_sta_get(get_apid,tt=tspan)')
    if get_apid eq 'd1' then if result eq 0 then result = execute('dat = mvn_sta_get("d0",tt=tspan)')
    sta_getdat_bgrm,dat,dat_new,ratio=0.5
    if keyword_set(plot_vdf) then flux_vdf3,dat_new,orbit=orbit,/local,/show,units='df',sta_apid=get_apid,mult='4,5',panel_mode=2,nbin=800,vres=2.,$
                  /remove,_extra=extra,mass_assmpt=16.,mass_range=[12,20],angle=[-45,45],/surf,/plot_vdf
    flux_vdf3,dat_new,orbit=orbit,/local,/show,units='df',sta_apid=get_apid,mult='4,5',panel_mode=1,nbin=800,vres=2.,$
                 /remove,vdf_sav=vdf_sav_out,_extra=extra,mass_assmpt=16.,mass_range=[12,20],angle=[-45,45],/surf,$
                 rmv_vsh=-1,plot_vdf=plot_vdf
    flux_vdf3,dat_new,orbit=orbit,/local,/show,units='df',sta_apid=get_apid,mult='4,5',panel_mode=1,nbin=800,vres=2.,$
                 /remove,vdf_sav=vdf_sav_in,_extra=extra,mass_assmpt=16.,mass_range=[12,20],angle=[-45,45],/surf,$
                 rmv_vsh=1
    
       
    if not keyword_set(highres) then begin
      if keyword_set(event) then png_path = SAVE_LOC+'/maven/png/vdf_reflect/normal/event/'+string(orbit,format='(i05)') $
                            else png_path = SAVE_LOC+'/maven/png/vdf_reflect/normal/oxy/'+string(orbit,format='(i05)')
    endif else begin
      if keyword_set(event) then png_path = SAVE_LOC+'/maven/png/vdf_reflect/highres/event/'+string(orbit,format='(i05)') $
      else png_path = SAVE_LOC+'/maven/png/vdf_reflect/highres/oxy/'+string(orbit,format='(i05)')
    endelse
    
                            
    
    if keyword_set(ps2pdf) then begin
      pclose
      if not keyword_set(highres) then begin
        spawn,'pstopdf '+SAVE_LOC+'/maven/ps/vdf_reflect/normal/event/'+string(orbit,format='(i05)')+'.eps'
        spawn,'rm ' + SAVE_LOC+'/maven/ps/vdf_reflect/normal/event/'+string(orbit,format='(i05)')+'.eps'
      endif else begin
        spawn,'pstopdf '+SAVE_LOC+'/maven/ps/vdf_reflect/highres/event/'+string(orbit,format='(i05)')+'.eps'
        spawn,'rm ' + SAVE_LOC+'/maven/ps/vdf_reflect/higres/event/'+string(orbit,format='(i05)')+'.eps'
      endelse
    endif
    
    if keyword_set(ps2png) then begin
      pclose
      if file_test(png_path) eq 0 then file_mkdir,png_path
      if not keyword_set(highres) then begin
        ;spawn,'convert '+filename+'.ps '+filename+'.png'
        cd,current=current_dir
        cd,ps_path
       
        spawn,'convert vdf_reflect_'+date_s+'.eps vdf_reflect_'+date_s+'.png'
        spawn,'rm ' + 'vdf_reflect_'+date_s+'.eps'
        cd,current_dir
       
      endif else begin
        spawn,'pstopdf '+SAVE_LOC+'/maven/ps/vdf_reflect/highres/event/'+string(orbit,format='(i05)')+'.eps'
        spawn,'rm ' + SAVE_LOC+'/maven/ps/vdf_reflect/higres/event/'+string(orbit,format='(i05)')+'.eps'
      endelse
    endif
    
    
    if (size(vdf_sav_out))[2] ne 8 then goto, no_save 
    
    if keyword_set(save) then begin
     if not keyword_set(highres) then begin
       if keyword_set(event) then save_path = SAVE_LOC+'/maven/sav/flux/deflect_ion/d0/'+string(orbit,format='(i05)') $
                             else save_path = SAVE_LOC+'/maven/sav/flux/deflect_ion/d0/'+string(orbit,format='(i05)')
     endif else begin
       if keyword_set(event) then save_path = SAVE_LOC+'/maven/sav/flux/deflect_ion/d1/'+string(orbit,format='(i05)') $
                             else save_path = SAVE_LOC+'/maven/sav/flux/deflect_ion/d1/'+string(orbit,format='(i05)')
      
     endelse
     
     if file_test(save_path) eq 0 then file_mkdir,save_path
     dat_sav = {time:vdf_sav_out.time, $
                Odens_100ev:vdf_sav_out.Odens_100ev, Ovel_mso_100ev:vdf_sav_out.Ovel_mso_100ev,$
                Odens_1kev:vdf_sav_out.Odens_1kev, Ovel_mso_1kev:vdf_sav_out.Ovel_mso_1kev,$
                Odens_5kev:vdf_sav_out.Odens_5kev, Ovel_mso_5kev:vdf_sav_out.Ovel_mso_5kev,$
                Odens_10kev:vdf_sav_out.Odens_10kev, Ovel_mso_10kev:vdf_sav_out.Ovel_mso_10kev,$
                Odens_20kev:vdf_sav_out.Odens_20kev, Ovel_mso_20kev:vdf_sav_out.Ovel_mso_20kev,$
                Odens_gt1kev:vdf_sav_out.Odens_gt1kev, Ovel_mso_gt1kev:vdf_sav_out.Ovel_mso_gt1kev,$
                Odens_gt5kev:vdf_sav_out.Odens_gt5kev, Ovel_mso_gt5kev:vdf_sav_out.Ovel_mso_gt5kev,$
                Odens_gt10kev:vdf_sav_out.Odens_gt10kev, Ovel_mso_gt10kev:vdf_sav_out.Ovel_mso_gt10kev,$
                Odens_all:vdf_sav_out.Odens_all, Ovel_mso_all:vdf_sav_out.Ovel_mso_all,$
                
                Odens_100ev_in:vdf_sav_in.Odens_100ev, Ovel_mso_100ev_in:vdf_sav_in.Ovel_mso_100ev,$
                Odens_1kev_in:vdf_sav_in.Odens_1kev, Ovel_mso_1kev_in:vdf_sav_in.Ovel_mso_1kev,$
                Odens_5kev_in:vdf_sav_in.Odens_5kev, Ovel_mso_5kev_in:vdf_sav_in.Ovel_mso_5kev,$
                Odens_10kev_in:vdf_sav_in.Odens_10kev, Ovel_mso_10kev_in:vdf_sav_in.Ovel_mso_10kev,$
                Odens_20kev_in:vdf_sav_in.Odens_20kev, Ovel_mso_20kev_in:vdf_sav_in.Ovel_mso_20kev,$
                Odens_gt1kev_in:vdf_sav_in.Odens_gt1kev, Ovel_mso_gt1kev_in:vdf_sav_in.Ovel_mso_gt1kev,$
                Odens_gt5kev_in:vdf_sav_in.Odens_gt5kev, Ovel_mso_gt5kev_in:vdf_sav_in.Ovel_mso_gt5kev,$
                Odens_gt10kev_in:vdf_sav_in.Odens_gt10kev, Ovel_mso_gt10kev_in:vdf_sav_in.Ovel_mso_gt10kev,$
                Odens_all_in:vdf_sav_in.Odens_all, Ovel_mso_all_in:vdf_sav_in.Ovel_mso_all,$
                
                Vmso:vdf_sav_out.Vmso, Vmso_std:vdf_sav_out.Vmso_std, Bmso:vdf_sav_out.Bmso, Bmso_std:vdf_sav_out.Bmso_std, Dens:vdf_sav_out.Dens,Dens_std:vdf_sav_out.Dens_std,$
                Theta_b:vdf_sav_out.Theta_b, Theta_b_std:vdf_sav_out.Theta_b_std, Phi_b:vdf_sav_out.Phi_b, R_b:vdf_sav_out.r_b, ztest_b:vdf_sav_out.Ztest_b,$
                Theta_v:vdf_sav_out.Theta_v, Theta_v_std:vdf_sav_out.Theta_v_std, Phi_v:vdf_sav_out.Phi_v, R_v:vdf_sav_out.r_v, ztest_v:vdf_sav_out.Ztest_v,$
                Pos_mso:vdf_sav_out.pos_mso,$
                Pos_mse:vdf_sav_out.pos_mse} 
         
     save,dat_sav,file=save_path+'/flux_vdf_'+date_s+'.sav'   
    
    endif
    
    no_save:
    nofield:
  endfor 
   
   
    

end