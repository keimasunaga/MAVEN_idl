pro vbvec,trange,vb,solar_wind=solar_wind,local=local,mva=mva
   
   env = init_env()
  SAVE_LOC = env.SAVE_LOC
  mtime = (trange[0]+trange[1])/2d
  orbit = mvn_orbit_num(time=mtime)
  
  fn_swi = SAVE_LOC + '/maven/sav/swia_flg/orbit/swia_flg_'+string(round(orbit),format='(i05)')+'.sav'
  restore,fn_swi
  flg_ca = swiflg.flg_ca
  flg_cs = swiflg.flg_cs
  if n_elements(flg_ca) eq 1 then if flg_ca eq 1 then swiname = 'swica' else swiname = 'swics' 
  if n_elements(flg_ca) eq 2 then if flg_ca[0] eq 1 and flg_ca[0] eq 1 then swiname = 'swica' else swiname = 'swics'

  
  
  ;; load solar wind information and calculate convection electric field (Note: Kei needs to add Vmso_std and Bmso_std for the solar wind info later)
  if keyword_set(solar_wind) then begin
    orbit = mvn_orbit_num(time=mtime)
    filename = SAVE_LOC + '/maven/sav/solar_wind_info/15min/sw_info_'+string(round(orbit),format='(i05)')+'.sav' ;00426.sav'
    if file_test(filename) eq 0 then begin
      dprint,'No solar wind info, returning.'
      return
    endif
    restore,filename
    dens = dat_sw.dens
    Vmso_avg = total(dat_sw.VEL,2)/2.  ;;average velocity for the inbound and outbound
    Vmso_tot = sqrt(Vmso_avg[0]^2+Vmso_avg[1]^2+Vmso_avg[2]^2)
    Vmso_e = Vmso_avg/Vmso_tot
    Bmso_avg = total(dat_sw.MAG_MSO,2)/2. ;; average magnetic field for the inbound and outbound
    Bmso_e = Bmso_avg/sqrt(Bmso_avg[0]^2+Bmso_avg[1]^2+Bmso_avg[2]^2)
    vproton_para_tot = inpro(Vmso_avg,Bmso_e)
    vproton_perp_tot = sqrt(Vmso_tot^2 - vproton_para_tot^2)
  endif

  ;; get local velocity vector and magnetic field vector and calculate local convection electric field
  if keyword_set(local) then begin
    
    idx_dens = nn('mvn_'+swiname+'_density',[trange[0],trange[1]])
    get_data,'mvn_'+swiname+'_density',t_dens,y_dens
    dens = total(y_dens[idx_dens[0]:idx_dens[1],*],1)/(idx_dens[1]-idx_dens[0]+1.)    
    dens_std = stddev(y_dens[idx_dens[0]:idx_dens[1]])
    
    idx_vel = nn('mvn_'+swiname+'_velocity_mso',[trange[0],trange[1]])
    get_data,'mvn_'+swiname+'_velocity_mso',t_vel,y_vel
    Vmso = y_vel[idx_vel[0]:idx_vel[1],*]
    Vmso_avg = total(y_vel[idx_vel[0]:idx_vel[1],*],1)/(idx_vel[1]-idx_vel[0]+1.)
    Vmso_tot = sqrt(Vmso_avg[0]^2+Vmso_avg[1]^2+Vmso_avg[2]^2)
    Vmso_e = Vmso_avg/Vmso_tot
    Vmso_std = stddev(y_vel[idx_vel[0]:idx_vel[1],*],dimension=1)
    
    idx_mag = nn('mvn_B_1sec_MAVEN_MSO',[trange[0],trange[1]])
    get_data,'mvn_B_1sec_MAVEN_MSO',t_mag,y_mag
    Bmso = y_mag[idx_mag[0]:idx_mag[1],*]
    Bmso_avg = total(y_mag[idx_mag[0]:idx_mag[1],*],1)/(idx_mag[1]-idx_mag[0]+1.)
    Bmso_std = stddev(y_mag[idx_mag[0]:idx_mag[1],*],dimension=1)
    Bmso_e = Bmso_avg/sqrt(Bmso_avg[0]^2+Bmso_avg[1]^2+Bmso_avg[2]^2)
    vproton_para_tot = inpro(Vmso_avg,Bmso_e)
    vproton_perp_tot = sqrt(Vmso_tot^2 - vproton_para_tot^2)
   
    vec_theta_phi,Bmso,th_b,th_b_std,ph_b,r_b,z_b
    vec_theta_phi,Vmso,th_v,th_v_std,ph_v,r_v,z_v


  endif

  ;; use mva method and get axes of the plasma sheet
  if keyword_set(mva) then begin
    stop
    idx_dens = nn('mvn_'+swiname+'_density',[trange[0],trange[1]])
    get_data,'mvn_'+swiname+'_density',t_dens,y_dens
    dens = total(y_dens[idx_dens[0]:idx_dens[1],*],1)/(idx_dens[1]-idx_dens[0]+1.)
    
    Bevec = get_mag_eigenvec_mva('mvn_B_1sec_MAVEN_MSO',tcenter=trange[0],trange=[trange[0],trange[1]])
    Bi = Bevec[*,0] & Bj = Bevec[*,1] & Bk = Bevec[*,2]
    Bmso_avg = Bj
    Bmso_e = Bj   ;;Bj (medium variance is the direction of the magnetic field)
    Vmso_avg = Bi
    Vmso_e = Bi
   ; idx_vel = nn('mvn_swim_velocity_mso',[trange[0],trange[1]])
   ; get_data,'mvn_swim_velocity_mso',t_vel,y_vel
   ; Vmso = total(y_vel[idx_vel[0]:idx_vel[1],*],1)/(idx_vel[1]-idx_vel[0]+1.)
   Vmso_tot = sqrt(Vmso_avg[0]^2+Vmso_avg[1]^2+Vmso_avg[2]^2)
   ; Vmso_e = Vmso/Vmso_tot
    vproton_para_tot = inpro(Vmso_avg,Bmso_e)
    vproton_perp_tot = sqrt(Vmso_tot^2 - vproton_para_tot^2)
    Vmso_std = [0,0,0]
    Bmso_std = [0,0,0]
    
  endif
  ;;

  vb = {Dens:dens,Dens_std:Dens_std, Vmso:Vmso_avg, Vmso_std:Vmso_std, Bmso:Bmso_avg, Bmso_std:Bmso_std,vperp:vproton_perp_tot, vpara:vproton_para_tot,$
        theta_b:th_b, theta_b_std:th_b_std, phi_b:ph_b, r_b:r_b, ztest_b:z_b,$
        theta_v:th_v, theta_v_std:th_v_std, phi_v:ph_v, r_v:r_v, ztest_v:z_v}


end




function sta_mass_select,counts,mass_arr,mass_range

   idx_out_of_mrange = where(mass_arr lt mass_range[0] or mass_arr gt mass_range[1])
   counts[idx_out_of_mrange] = 0. 
   return,counts

end


function get_uvec_mso,V,theta,phi

  ux = V*cos(theta*!DTOR)*cos(phi*!DTOR)
  uy = V*cos(theta*!DTOR)*sin(phi*!DTOR)
  uz = V*sin(theta*!DTOR)
  dat = {ux:ux, uy:uy, uz:uz}

  return,dat

end


function trans_vec,vx,vy,vz,q

  t2 =   q[0]*q[1]              ;- cf. quaternion_rotation.pro
  t3 =   q[0]*q[2]
  t4 =   q[0]*q[3]
  t5 =  -q[1]*q[1]
  t6 =   q[1]*q[2]
  t7 =   q[1]*q[3]
  t8 =  -q[2]*q[2]
  t9 =   q[2]*q[3]
  t10 = -q[3]*q[3]

  vxn = 2*( (t8 + t10)*vx + (t6 -  t4)*vy + (t3 + t7)*vz ) + vx
  vyn = 2*( (t4 +  t6)*vx + (t5 + t10)*vy + (t9 - t2)*vz ) + vy
  vzn = 2*( (t7 -  t3)*vx + (t2 +  t9)*vy + (t5 + t8)*vz ) + vz

  thetan = 90. - ACOS(vzn)*!RADEG
  phin = ATAN(vyn, vxn) * !RADEG
  
  dat = {theta: thetan, phi: phin}
  
  return,dat

end



pro flux_vdf,trange=trange,load=load,sta_apid=sta_apid,dat=dat,$
                 mass_range=mass_range,mass_assmpt=mass_assmpt,erange=erange,angle_range=angle_range,units=units,$
                 emplot=emplot,show_ring=show_ring,backscat=backscat,surf=surf,remove_proton=remove_proton,$
                 solar_wind=solar_wind,local=local,mva=mva,mso=mso,$
                 mult=mult,panel_mode=panel_mode,vdf_sav=vdf_sav,charsize=charsize,$
                 nbin=nbin,vres=vres,vrange=vrange
   
                      
    if not keyword_set(angle_range) then angle_range = [-22.5, 22.5]
    if not keyword_set(units) then units='eflux'
    if not keyword_set(sta_apid) then sta_apid = 'd1'
   
    ;; difine bin size and vresolution for vdf
    
    if sta_apid eq 'd1' or sta_apid eq 'd0' then begin
      if not keyword_set(nbin) then  nbin = 800.
      if not keyword_set(vres) then vres = 2.
      if not keyword_set(vrange) then vrange = [-nbin/2.*vres,nbin/2.*vres]
    endif 
    
    if sta_apid eq 'cf' or sta_apid eq 'ce' then begin
      if not keyword_set(nbin) then  nbin = 200.
      if not keyword_set(vres) then vres = 1.
      if not keyword_set(vrange) then vrange = [-nbin/2.*vres,nbin/2.*vres]
    endif
    
    im_xy = fltarr(nbin+1,nbin+1) & im_xy2 = fltarr(nbin+1,nbin+1)
    N_im_xy = fltarr(nbin+1,nbin+1) & N_im_xy2 = fltarr(nbin+1,nbin+1)
    im_yz = fltarr(nbin+1,nbin+1) & im_yz2 = fltarr(nbin+1,nbin+1)
    N_im_yz = fltarr(nbin+1,nbin+1) & N_im_yz2 = fltarr(nbin+1,nbin+1)
    im_xz = fltarr(nbin+1,nbin+1) & im_xz2 = fltarr(nbin+1,nbin+1)
    N_im_xz = fltarr(nbin+1,nbin+1) & N_im_xz2 = fltarr(nbin+1,nbin+1)
    im_paraperp = fltarr(nbin+1,nbin/2.+1)  & im_paraperp2 = fltarr(nbin+1,nbin/2.+1)
    N_im_paraperp = fltarr(nbin+1,nbin/2.+1) & N_im_paraperp2 = fltarr(nbin+1,nbin/2.+1)
    ;im_th_ph_mso = fltarr(360,180)
   
    
    ;; get a data structure and reform as a form of [32e, 4p, 16a, 8m] for d1
    if keyword_set(load) then mvn_sta_l2_load,sta_apid=sta_apid
   
    if ~keyword_set(dat) then begin
      if keyword_set(trange) then begin
        dat = mvn_sta_get(sta_apid,tt=trange)
      endif else begin
        dat = mvn_sta_get(sta_apid)
      endelse
      
      if keyword_set(remove_proton) then sta_getdat_bgrm,dat,dat,ratio=0.5
    endif    
   
   
    ;; display emplot
    if keyword_set(emplot) then begin
      wi,1
      dat2 = mvn_sta_get('c6')
      zscale = max(dat2.data)/1.e5 > 1
      contour4d,dat2,/points,/label,/fill,/mass,zrange=zscale*[.1,1.e5],/twt,units='eflux'
      stop
    endif
   
    ;; get velocity vector and magnetic field vector of solar wind or local flow.
    ;; a keyword of mva denotes the minimum variance analysis, which is useful to determine magentic field direction in the plasma sheet.
    vbvec,[dat.time,dat.end_time],vb,solar_wind=solar_wind,local=local,mva=mva
    dens = vb.dens & dens_std = vb.dens_std
    Vmso = vb.Vmso & Vmso_e = Vmso/total(Vmso^2)^.5 & Vmso_std = vb.Vmso_std
    th_v = vb.theta_v & th_v_std = vb.theta_v_std & ph_v = vb.phi_v & r_v = vb.r_v & z_v = vb.ztest_v
    Bmso = vb.Bmso & Bmso_e = Bmso/total(Bmso^2)^.5 & Bmso_std = vb.Bmso_std
    th_b = vb.theta_b & th_b_std = vb.theta_b_std & ph_b = vb.phi_b & r_b = vb.r_b & z_b = vb.ztest_b
    
    vproton_perp_tot = vb.vperp & vproton_para_tot = vb.vpara
    
    
    ;; let's put mag filter here to remove fluctuating field in the future!!
    
    
    
    ;; start processing static data
    mvn_sta_convert_units,dat,units ;'df'
    mvn_pfp_cotrans,dat,from='MAVEN_STATIC',to='MAVEN_MSO',theta=theta_mso,phi=phi_mso

 

    env = init_env()
    SAVE_LOC = env.SAVE_LOC
    time = (dat.time + dat.end_time)/2.d0
    counts = reform(dat.data,32,4,16,8)
    theta_c = reform(dat.theta,32,4,16,8) & dtheta = reform(dat.dtheta,32,4,16,8)
    phi_c = reform(dat.phi,32,4,16,8) & dphi = reform(dat.dphi,32,4,16,8)
    E_c = reform(dat.energy,32,4,16,8) & dE = reform(dat.denergy,32,4,16,8)
    mass_arr = reform(dat.mass_arr,32,4,16,8)
    if not keyword_set(mass_assmpt) then mass_assmpt = 16.
    if not keyword_set(mass_range) then mass_range = [12.,20.]
    counts2 = sta_mass_select(counts,mass_arr,mass_range)
    vel_c = mvn_ev2km(E_c,mass_assmpt) & dvel =  mvn_ev2km(dE,mass_assmpt)
   
    ;; +/- width from the center value 
    theta_m = theta_c - dtheta/2.
    theta_p = theta_c + dtheta/2.
    phi_m = phi_c - dphi/2.
    phi_p = phi_c + dphi/2.
    E_m = E_c - dE/2.
    E_p = E_c + dE/2.
    vel_m = mvn_ev2km(E_m,mass_assmpt);vel_c - dvel/2.
    vel_p = mvn_ev2km(E_p,mass_assmpt);vel_c + dvel/2.
 
    ;; convert theta, phi (sphere frame) to cartesian in the sc frame
    sphere_to_cart, 1.d0, theta_m, phi_m, vx_mm, vy_mm, vz_mm
    sphere_to_cart, 1.d0, theta_m, phi_p, vx_mp, vy_mp, vz_mp
    sphere_to_cart, 1.d0, theta_p, phi_m, vx_pm, vy_pm, vz_pm
    sphere_to_cart, 1.d0, theta_p, phi_p, vx_pp, vy_pp, vz_pp
    sphere_to_cart, 1.d0, theta_c, phi_c, vx_c, vy_c, vz_c

    ; load quarntenion
    q = spice_body_att('MAVEN_STATIC', 'MAVEN_MSO', time, $
      /quaternion, verbose=verbose)

    ; rotate 4 vectors of the top of a bin and the center of bin in the static frame into the MSO frame
    d_mm = trans_vec(vx_mm, vy_mm, vz_mm, q)
    d_mp = trans_vec(vx_mp, vy_mp, vz_mp, q)
    d_pm = trans_vec(vx_pm, vy_pm, vz_pm, q)    
    d_pp = trans_vec(vx_pp, vy_pp, vz_pp, q)
    d_c = trans_vec(vx_c, vy_c, vz_c, q)
    
    theta_mm = d_mm.theta
    phi_mm = d_mm.phi
    theta_mp = d_mp.theta
    phi_mp = d_mp.phi
    theta_pm = d_pm.theta
    phi_pm = d_pm.phi
    theta_pp = d_pp.theta
    phi_pp = d_pp.phi
    theta_c_mso = d_c.theta
    phi_c_mso = d_c.phi
    
    ;; get_vel_mso corrdinates
    u_mmm = get_uvec_mso(vel_m,theta_mm,phi_mm) & u_pmm = get_uvec_mso(vel_p,theta_mm,phi_mm)
    u_mmp = get_uvec_mso(vel_m,theta_mp,phi_mp) & u_pmp = get_uvec_mso(vel_p,theta_mp,phi_mp)
    u_mpm = get_uvec_mso(vel_m,theta_pm,phi_pm) & u_ppm = get_uvec_mso(vel_p,theta_pm,phi_pm)
    u_mpp = get_uvec_mso(vel_m,theta_pp,phi_pp) & u_ppp = get_uvec_mso(vel_p,theta_pp,phi_pp)
    u_c = get_uvec_mso(vel_c,theta_c_mso,phi_c_mso)
    stop
    idx_minus = where(u_c.ux lt 0)
    counts2[idx_minus] = 0.
    counts3 = reform(counts2,32,64,8)
    dat.data = counts3
    erange_10kev = [10000,35000]
    Odens_10kev = n_4d(dat,energy=erange_10kev,mass=mass_range,m_int=mass_assmpt)
    Ovel_10kev = v_4d(dat,energy=erange_10kev,mass=mass_range,m_int=mass_assmpt)
    Oflux_10kev = j_4d(dat,energy=erange_10kev,mass=mass_range,m_int=mass_assmpt)
    ;vunit_10kev = Ovel_10kev/total(Ovel_10kev^2)^.5
    ;Ovel_mso_10kev = total(Ovel_10kev^2)^.5*spice_vector_rotate(vunit_10kev,time,'MAVEN_STATIC','MAVEN_MSO',check_objects=check_objects,verbose=verbose,qrot=q)
    if total(Ovel_10kev^2)^.5 ne 0. then begin
      vunit_10kev = Ovel_10kev/total(Ovel_10kev^2)^.5
      Ovel_mso_10kev = total(Ovel_10kev^2)^.5*spice_vector_rotate(vunit_10kev,time,'MAVEN_STATIC','MAVEN_MSO',check_objects=check_objects,verbose=verbose,qrot=q)
    endif else begin
      Ovel_mso_10kev = [0.,0.,0.]
    endelse
    
    
    erange_1kev = [1000,9999.99d]
    Odens_1kev = n_4d(dat,energy=erange_1kev,mass=mass_range,m_int=mass_assmpt)
    Ovel_1kev = v_4d(dat,energy=erange_1kev,mass=mass_range,m_int=mass_assmpt)
    Oflux_1kev = j_4d(dat,energy=erange_1kev,mass=mass_range,m_int=mass_assmpt)
    ;vunit_1kev = Ovel_1kev/total(Ovel_1kev^2)^.5
    ;Ovel_mso_1kev = total(Ovel_1kev^2)^.5*spice_vector_rotate(vunit_1kev,time,'MAVEN_STATIC','MAVEN_MSO',check_objects=check_objects,verbose=verbose,qrot=q)
    if total(Ovel_1kev^2)^.5 ne 0. then begin
      vunit_1kev = Ovel_1kev/total(Ovel_1kev^2)^.5
      Ovel_mso_1kev = total(Ovel_1kev^2)^.5*spice_vector_rotate(vunit_1kev,time,'MAVEN_STATIC','MAVEN_MSO',check_objects=check_objects,verbose=verbose,qrot=q)
    endif else begin
      Ovel_mso_1kev = [0.,0.,0.]
    endelse
    
    
    erange_100ev = [100,999.99d]
    Odens_100ev = n_4d(dat,energy=erange_100ev,mass=mass_range,m_int=mass_assmpt)
    Ovel_100ev = v_4d(dat,energy=erange_100ev,mass=mass_range,m_int=mass_assmpt)
    Oflux_100ev = j_4d(dat,energy=erange_100ev,mass=mass_range,m_int=mass_assmpt)
    if total(Ovel_100ev^2)^.5 ne 0. then begin
      vunit_100ev = Ovel_100ev/total(Ovel_100ev^2)^.5
      Ovel_mso_100ev = total(Ovel_100ev^2)^.5*spice_vector_rotate(vunit_100ev,time,'MAVEN_STATIC','MAVEN_MSO',check_objects=check_objects,verbose=verbose,qrot=q)
    endif else begin
      Ovel_mso_100ev = [0.,0.,0.]
    endelse


    erange_all = [100,35000]
    Odens_all = n_4d(dat,energy=erange_all,mass=mass_range,m_int=mass_assmpt)
    Ovel_all = v_4d(dat,energy=erange_all,mass=mass_range,m_int=mass_assmpt)
    Oflux_all = j_4d(dat,energy=erange_all,mass=mass_range,m_int=mass_assmpt)   
    if total(Ovel_all^2)^.5 ne 0. then begin 
      vunit_all = Ovel_all/total(Ovel_all^2)^.5 
      Ovel_mso_all = total(Ovel_all^2)^.5*spice_vector_rotate(vunit_all,time,'MAVEN_STATIC','MAVEN_MSO',check_objects=check_objects,verbose=verbose,qrot=q)
    endif else begin       
      Ovel_mso_all = [0.,0.,0.]
    endelse
      
;   print,time_string(time)
;   print,ovel_10kev
;   print,odens_10kev
;   print,oflux_10kev
;   stop
    ;;;;;;;;;;;;;;
    
    
    ;;set energy range, default is energetic ions for [50, 30000] eV. 
    ;Note that you need higher velocity resolution to see ion distrubutions less than 50 eV so that polyfillv works 
    if not keyword_set(erange) then begin
      if sta_apid eq 'd1' or sta_apid eq 'd0' then erange = [100.,30000.]
      if sta_apid eq 'cf' or sta_apid eq 'ce' then erange = [1.,500.]
      idx_erange = where(E_c[*,0,0,0] ge erange[0] and E_c[*,0,0,0] le erange[1])
    endif
    
    for ie=0,31 do begin
       idx_ie = where(idx_erange eq ie) 
       if idx_ie[0] eq -1 then goto, out_of_energy
       for ip=0,3 do begin
        
         for ia=0,15 do begin
            
            ;; calculate angle between a velocity vector of each sector and each plane
            vec_c = [u_c.ux[ie,ip,ia,6],u_c.uy[ie,ip,ia,6],u_c.uz[ie,ip,ia,6]]/vel_c[ie,ip,ia,6] ;;velocity vector of each bin
            if keyword_set(mso) then begin
              nx = [1.,0.,0.] & ny = [0.,1.,0.] & nz = [0.,0.,1.]  ;;normal vectors of yz plane, xz plane and xy plane
              agl_vcnx = acos(inpro(vec_c,nx)) * !radeg
              agl_vcny = acos(inpro(vec_c,ny)) * !radeg
              agl_vcnz = acos(inpro(vec_c,nz)) * !radeg
              
            endif else begin
              Vpara_mso = inpro(Vmso,Bmso_e)*Bmso_e & Vperp_mso = Vmso - Vpara_mso
              nx = Bmso_e & ny = Vperp_mso/total(Vperp_mso^2)^.5 & nz = crossp(nx,ny)/total(crossp(nx,ny)^2)^.5  ;;normal vectors of ve plane, eb plane and vb plane
              agl_vcnx = acos(inpro(vec_c,nx)) * !radeg  
              agl_vcny = acos(inpro(vec_c,ny)) * !radeg
              agl_vcnz = acos(inpro(vec_c,nz)) * !radeg
            endelse 
            agl_yz = 90d - agl_vcnx  ;; angle between the velocity vector and the yz plane (-90 to 90 degree)
            agl_xz = 90d - agl_vcny  ;; angle between the velocity vector and the xz plane (-90 to 90 degree)
            agl_xy = 90d - agl_vcnz  ;; angle between the velocity vector and the xy plane (-90 to 90 degree)
                       
            ;8 point of a polygon in the mso coordnate
            u_mmm_x = u_mmm.ux[ie,ip,ia,6] & u_mmm_y = u_mmm.uy[ie,ip,ia,6] & u_mmm_z = u_mmm.uz[ie,ip,ia,6]
            u_mmp_x = u_mmp.ux[ie,ip,ia,6] & u_mmp_y = u_mmp.uy[ie,ip,ia,6] & u_mmp_z = u_mmp.uz[ie,ip,ia,6]
            u_mpm_x = u_mpm.ux[ie,ip,ia,6] & u_mpm_y = u_mpm.uy[ie,ip,ia,6] & u_mpm_z = u_mpm.uz[ie,ip,ia,6]
            u_mpp_x = u_mpp.ux[ie,ip,ia,6] & u_mpp_y = u_mpp.uy[ie,ip,ia,6] & u_mpp_z = u_mpp.uz[ie,ip,ia,6]
            u_pmm_x = u_pmm.ux[ie,ip,ia,6] & u_pmm_y = u_pmm.uy[ie,ip,ia,6] & u_pmm_z = u_pmm.uz[ie,ip,ia,6]
            u_pmp_x = u_pmp.ux[ie,ip,ia,6] & u_pmp_y = u_pmp.uy[ie,ip,ia,6] & u_pmp_z = u_pmp.uz[ie,ip,ia,6]
            u_ppm_x = u_ppm.ux[ie,ip,ia,6] & u_ppm_y = u_ppm.uy[ie,ip,ia,6] & u_ppm_z = u_ppm.uz[ie,ip,ia,6]
            u_ppp_x = u_ppp.ux[ie,ip,ia,6] & u_ppp_y = u_ppp.uy[ie,ip,ia,6] & u_ppp_z = u_ppp.uz[ie,ip,ia,6]
            
            if keyword_set(mso) then begin
              ;; 8 points of a polygon in the mso coordinate
              vx = [u_mmm_x,u_pmm_x,u_mmp_x,u_pmp_x,u_mpm_x,u_ppm_x,u_mpp_x,u_ppp_x]
              vy = [u_mmm_y,u_pmm_y,u_mmp_y,u_pmp_y,u_mpm_y,u_ppm_y,u_mpp_y,u_ppp_y]
              vz = [u_mmm_z,u_pmm_z,u_mmp_z,u_pmp_z,u_mpm_z,u_ppm_z,u_mpp_z,u_ppp_z]
            endif
            
            ;; convert 8 points into the bve coordinate system 
             if not keyword_set(mso) then begin
              v_mmm = mso2bve([[u_mmm_x],[u_mmm_y],[u_mmm_z]], Vmso, Bmso)
              v_mmp = mso2bve([[u_mmp_x],[u_mmp_y],[u_mmp_z]], Vmso, Bmso)
              v_mpm = mso2bve([[u_mpm_x],[u_mpm_y],[u_mpm_z]], Vmso, Bmso)
              v_mpp = mso2bve([[u_mpp_x],[u_mpp_y],[u_mpp_z]], Vmso, Bmso)
              v_pmm = mso2bve([[u_pmm_x],[u_pmm_y],[u_pmm_z]], Vmso, Bmso)
              v_pmp = mso2bve([[u_pmp_x],[u_pmp_y],[u_pmp_z]], Vmso, Bmso)
              v_ppm = mso2bve([[u_ppm_x],[u_ppm_y],[u_ppm_z]], Vmso, Bmso)
              v_ppp = mso2bve([[u_ppp_x],[u_ppp_y],[u_ppp_z]], Vmso, Bmso)
            
              vx = [v_mmm[0],v_mmp[0],v_mpm[0],v_mpp[0],v_pmm[0],v_pmp[0],v_ppm[0],v_ppp[0]]
              vy = [v_mmm[1],v_mmp[1],v_mpm[1],v_mpp[1],v_pmm[1],v_pmp[1],v_ppm[1],v_ppp[1]]
              vz = [v_mmm[2],v_mmp[2],v_mpm[2],v_mpp[2],v_pmm[2],v_pmp[2],v_ppm[2],v_ppp[2]]
              vpara = vx
              vperp = sqrt(vy^2 + vz^2)
             endif           
            
            

            if agl_xy ge angle_range[0] and agl_xy le angle_range[1] then begin
              idx_xy_roi = find_vdist_roi(vx,vy)
              inside_xy = polyfillv(round(Vx[idx_xy_roi]/vres)+nbin/2.,round(Vy[idx_xy_roi]/vres)+nbin/2.,nbin+1,nbin+1)
              if inside_xy[0] eq -1 then stop
              Nxy = fltarr(nbin+1,nbin+1) & Cts_xy = fltarr(nbin+1,nbin+1)
              Nxy[inside_xy] = 1 & Cts_xy[inside_xy] = total(counts2[ie,ip,ia,*])
              N_im_xy = N_im_xy + Nxy & im_xy = im_xy + Cts_xy
            endif
            
            if agl_yz ge angle_range[0] and agl_yz le angle_range[1] then begin
              idx_yz_roi = find_vdist_roi(vy,vz)
              
              inside_yz = polyfillv(round(Vy[idx_yz_roi]/vres)+nbin/2.,round(Vz[idx_yz_roi]/vres)+nbin/2.,nbin+1,nbin+1)
              Nyz = fltarr(nbin+1,nbin+1) & Cts_yz = fltarr(nbin+1,nbin+1)
              Nyz[inside_yz] = 1 & Cts_yz[inside_yz] = total(counts2[ie,ip,ia,*])
              N_im_yz = N_im_yz + Nyz & im_yz = im_yz + Cts_yz
              
            endif
            
            if agl_xz ge angle_range[0] and agl_xz le angle_range[1] then begin
              idx_xz_roi = find_vdist_roi(vx,vz)
              inside_xz = polyfillv(round(Vx[idx_xz_roi]/vres)+nbin/2.,round(Vz[idx_xz_roi]/vres)+nbin/2.,nbin+1,nbin+1)
              Nxz = fltarr(nbin+1,nbin+1) & Cts_xz = fltarr(nbin+1,nbin+1)
              Nxz[inside_xz] = 1 & Cts_xz[inside_xz] = total(counts2[ie,ip,ia,*])
              N_im_xz = N_im_xz + Nxz & im_xz = im_xz + Cts_xz        
            endif
            
            if not keyword_set(mso) then begin
              idx_paraperp_roi = find_vdist_roi(vpara,vperp)
              inside_paraperp = polyfillv(round(Vpara[idx_paraperp_roi]/vres)+nbin/2.,round(Vperp[idx_paraperp_roi]/vres),nbin+1,nbin+1)
              Nparaperp = fltarr(nbin+1,nbin/2.+1) & Cts_paraperp = fltarr(nbin+1,nbin/2.+1)
              Nparaperp[inside_paraperp] = 1 & Cts_paraperp[inside_paraperp] = total(counts2[ie,ip,ia,*])
              N_im_paraperp = N_im_paraperp + Nparaperp & im_paraperp = im_paraperp + Cts_paraperp
            endif        
                
         endfor       
       endfor
       
       out_of_energy:
    endfor
    
    wi,1
    if keyword_set(mso) then begin
       vx_tit = 'Vx' & vy_tit='Vy' & vz_tit='Vz'
    endif else begin
       vx_tit = 'Vb' & vy_tit = 'Vv' & vz_tit = 'Ve'    
    endelse
    
    if keyword_set(panel_mode) then begin
      nhrz = long(strmid(mult,0,1))
      nvrt = long(strmid(mult,2,1))
      
      if panel_mode eq 1 then begin
         mpanel_imxy = '0,'+strcompress(string(nvrt-2),/remove)
         mpanel_Nxy = '1,'+strcompress(string(nvrt-2),/remove)
         mpanel_imyz = '2,'+strcompress(string(nvrt-2),/remove)
         mpanel_Nyz = '3,'+strcompress(string(nvrt-2),/remove)
         mpanel_imxz = '0,'+strcompress(string(nvrt-1),/remove)
         mpanel_Nxz = '1,'+strcompress(string(nvrt-1),/remove)
         mpanel_imparaperp = '2,'+strcompress(string(nvrt-1),/remove)
         mpanel_Nparaperp = '3,'+strcompress(string(nvrt-1),/remove)
      endif
    endif
    
    if units eq 'df' then zrange = [1e-13,1e-6]
    if units eq 'eflux' then zrange = [1e3,1e7]
    if units eq 'flux' then zrange = [1e-4,1e2]
    
    
    
    ;;calculate surface angle of IMB
    get_data,'Xmso',data=Xmso
    get_data,'Ymso',data=Ymso
    get_data,'Zmso',data=Zmso
    idx = nn('Xmso',time)
    pos = [Xmso.y[idx],Ymso.y[idx],Zmso.y[idx]]
    if keyword_set(surf) then begin
      surf_angle_imb = get_imb_surf_angle(pos,Vmso,Bmso,/imb,foot_point=foot_point_imb)
      idx_nan = where(finite(surf_angle_imb) eq 1, imb_ok)
      dist_sc_imb_z = abs(pos[2] - foot_point_imb[2,*])
      idx = where(dist_sc_imb_z eq min(dist_sc_imb_z))
      zsurf_imb = [surf_angle_imb[idx]*vrange[0],surf_angle_imb[idx]*vrange[1]]
      
      surf_angle_bs = get_imb_surf_angle(pos,Vmso,Bmso,/bs,foot_point=foot_point_bs)
      idx_nan = where(finite(surf_angle_bs) eq 1, bs_ok)
      dist_sc_bs_z = abs(pos[2] - foot_point_bs[2,*])
      idx = where(dist_sc_bs_z eq min(dist_sc_bs_z))
      zsurf_bs = [surf_angle_bs[idx]*vrange[0],surf_angle_bs[idx]*vrange[1]]
    endif
    
    
    ;;calculate the sun direction in the bve frame
    sundir = [1.,0.,0.];spice_body_pos('SUN','MAVEN',utc=time_string(time),frame='MAVEN_MSO');,check='MAVEN_SPACECRAFT');;[1.,0.,0.]

    sundir_bve = mso2bve(sundir,Vmso,Bmso)
    sundir_bv = [sundir_bve[0],sundir_bve[1]]/(sundir_bve[0]^2+sundir_bve[1]^2)^.5
    sundir_ve = [sundir_bve[1],sundir_bve[2]]/(sundir_bve[1]^2+sundir_bve[2]^2)^.5
    sundir_be = [sundir_bve[0],sundir_bve[2]]/(sundir_bve[0]^2+sundir_bve[2]^2)^.5


    marsdir = -pos;spice_body_pos('MARS','MAVEN',utc=time_string(time),frame='MAVEN_MSO',check='MAVEN_SPACECRAFT');;[1.,0.,0.]
    marsdir_bve = mso2bve(marsdir,Vmso,Bmso)
    marsdir_bv = [marsdir_bve[0],marsdir_bve[1]]/(marsdir_bve[0]^2+marsdir_bve[1]^2)^.5
    marsdir_ve = [marsdir_bve[1],marsdir_bve[2]]/(marsdir_bve[1]^2+marsdir_bve[2]^2)^.5
    marsdir_be = [marsdir_bve[0],marsdir_bve[2]]/(marsdir_bve[0]^2+marsdir_bve[2]^2)^.5

    
    ;;;;;;;;;;;;;;;;
    ;; Start plot ;;
    ;;;;;;;;;;;;;;;;
    
    ;;plot xy plane (bv plane)
    if not keyword_set(panel_mode) then mult='2,4'
    idx_xy = where(N_im_xy gt 0)
    im_xy2[idx_xy] = im_xy[idx_xy]/N_im_xy[idx_xy]
    idx_zero_xy = where(N_im_xy eq 0)
    im_xy2[idx_zero_xy] = !values.F_NaN
    N_im_xy2[idx_xy] = N_im_xy[idx_xy]
    N_im_xy2[idx_zero_xy] = !values.F_NaN

    plotxyz,(findgen(nbin+1)-nbin/2.)*vres,(findgen(nbin+1)-nbin/2.)*vres,im_xy2,xrange=vrange,yrange=vrange,zrange=zrange,/zlog,mult=mult,mpanel=mpanel_imxy,xtit=Vx_tit+' [km/s]',ytit=Vy_tit+' [km/s]',tit=time_string(dat.time)+'-'+strmid(time_string(dat.end_time),11,8),charsize=charsize
    loadct2,0
    plots,circle_v2(0,0,mvn_ev2km(10.,mass_assmpt)),color=180
    plots,circle_v2(0,0,mvn_ev2km(100.,mass_assmpt)),color=180
    plots,circle_v2(0,0,mvn_ev2km(1000.,mass_assmpt)),color=180
    plots,circle_v2(0,0,mvn_ev2km(10000.,mass_assmpt)),color=180
    loadct2,39
    arrow,sundir_bv[0]*650,sundir_bv[1]*650,sundir_bv[0]*750,sundir_bv[1]*750,color=210,/data,hsize=3
    arrow,marsdir_bv[0]*650,marsdir_bv[1]*650,marsdir_bv[0]*750,marsdir_bv[1]*750,color=230,/data,hsize=3
    plotxyz,(findgen(nbin+1)-nbin/2.)*vres,(findgen(nbin+1)-nbin/2.)*vres,N_im_xy2,xrange=vrange,yrange=vrange,zrange=[0,10],/add,mpanel=mpanel_Nxy,xtit=Vx_tit+' [km/s]',ytit=Vy_tit+' [km/s]',tit=time_string(dat.time)+'-'+strmid(time_string(dat.end_time),11,8),charsize=charsize
    loadct2,0
    plots,circle_v2(0,0,mvn_ev2km(10.,mass_assmpt)),color=180
    plots,circle_v2(0,0,mvn_ev2km(100.,mass_assmpt)),color=180
    plots,circle_v2(0,0,mvn_ev2km(1000.,mass_assmpt)),color=180
    plots,circle_v2(0,0,mvn_ev2km(10000.,mass_assmpt)),color=180
    loadct2,39
    
    
    
    ;;plot yz plane (ve plane)
    idx_yz = where(N_im_yz gt 0)
    idx_zero_yz = where(N_im_yz eq 0)
    im_yz2[idx_yz] = im_yz[idx_yz]/N_im_yz[idx_yz]
    im_yz2[idx_zero_yz] = !values.F_NaN
    N_im_yz2[idx_yz] = N_im_yz[idx_yz]
    N_im_yz2[idx_zero_yz] = !values.F_NaN
    
    plotxyz,(findgen(nbin+1)-nbin/2.)*vres,(findgen(nbin+1)-nbin/2.)*vres,im_yz2,xrange=vrange,yrange=vrange,zrange=zrange,/zlog,/add,mpanel=mpanel_imyz,xtit=Vy_tit+' [km/s]',ytit=Vz_tit+' [km/s]',tit=time_string(dat.time)+'-'+strmid(time_string(dat.end_time),11,8),charsize=charsize
    loadct2,0
    plots,circle_v2(0,0,mvn_ev2km(10.,mass_assmpt)),color=180
    plots,circle_v2(0,0,mvn_ev2km(100.,mass_assmpt)),color=180
    if sta_apid eq 'd1' or sta_apid eq 'd0' then plots,circle_v2(0,0,mvn_ev2km(1000.,mass_assmpt)),color=180
    if sta_apid eq 'd1' or sta_apid eq 'd0' then plots,circle_v2(0,0,mvn_ev2km(10000.,mass_assmpt)),color=180   
    loadct2,39
    arrow,sundir_ve[0]*650,sundir_ve[1]*650,sundir_ve[0]*750,sundir_ve[1]*750,color=210,/data,hsize=3
    arrow,marsdir_ve[0]*650,marsdir_ve[1]*650,marsdir_ve[0]*750,marsdir_ve[1]*750,color=230,/data,hsize=3
    if keyword_set(show_ring) then begin
      plots,circle_v2(vproton_perp_tot,0,vproton_perp_tot),color=230,line=2
    endif
    
    if keyword_set(backscat) then begin
      plots,circle_v2(vproton_perp_tot,0,2.*vproton_perp_tot),color=180,line=2
    endif
    if keyword_set(surf) then begin
      if imb_ok ne 0 then oplot,vrange,zsurf_imb,color=80,line=2
      if bs_ok ne 0 then oplot,vrange,zsurf_bs,color=230,line=2
    
    endif
    
    
    
    plotxyz,(findgen(nbin+1)-nbin/2.)*vres,(findgen(nbin+1)-nbin/2.)*vres,N_im_yz2,xrange=vrange,yrange=vrange,zrange=[0,10],/add,mpanel=mpanel_Nyz,xtit=Vy_tit+' [km/s]',ytit=Vz_tit+' [km/s]',tit=time_string(dat.time)+'-'+strmid(time_string(dat.end_time),11,8),charsize=charsize
    loadct2,0
    plots,circle_v2(0,0,mvn_ev2km(10.,mass_assmpt)),color=180
    plots,circle_v2(0,0,mvn_ev2km(100.,mass_assmpt)),color=180
    if sta_apid eq 'd1' or sta_apid eq 'd0' then plots,circle_v2(0,0,mvn_ev2km(1000.,mass_assmpt)),color=180
    if sta_apid eq 'd1' or sta_apid eq 'd0' then plots,circle_v2(0,0,mvn_ev2km(10000.,mass_assmpt)),color=180  
    loadct2,39
     if keyword_set(show_ring) then begin
      plots,circle_v2(vproton_perp_tot,0,vproton_perp_tot),color=230,line=2
    endif
    
    if keyword_set(backscat) then begin
      plots,circle_v2(vproton_perp_tot,0,2.*vproton_perp_tot),color=180,line=2
    endif
    
    
    ;;plot xz plane  (be plane)
    idx_xz = where(N_im_xz gt 0)
    idx_zero_xz = where(N_im_xz eq 0)
    im_xz2[idx_xz] = im_xz[idx_xz]/N_im_xz[idx_xz]
    im_xz2[idx_zero_xz] = !values.F_NaN
    N_im_xz2[idx_xz] = N_im_xz[idx_xz]
    N_im_xz2[idx_zero_xz] = !values.F_NaN
    
    plotxyz,(findgen(nbin+1)-nbin/2.)*vres,(findgen(nbin+1)-nbin/2.)*vres,im_xz2,xrange=vrange,yrange=vrange,zrange=zrange,/zlog,/add,mpanel=mpanel_imxz,xtit=Vx_tit+' [km/s]',ytit=Vz_tit+' [km/s]',tit=time_string(dat.time)+'-'+strmid(time_string(dat.end_time),11,8),charsize=charsize
    loadct2,0
    plots,circle_v2(0,0,mvn_ev2km(10.,mass_assmpt)),color=180
    plots,circle_v2(0,0,mvn_ev2km(100.,mass_assmpt)),color=180
    plots,circle_v2(0,0,mvn_ev2km(1000.,mass_assmpt)),color=180
    plots,circle_v2(0,0,mvn_ev2km(10000.,mass_assmpt)),color=180
    loadct2,39
    arrow,sundir_be[0]*650,sundir_be[1]*650,sundir_be[0]*750,sundir_be[1]*750,color=210,/data,hsize=3
    arrow,marsdir_be[0]*650,marsdir_be[1]*650,marsdir_be[0]*750,marsdir_be[1]*750,color=230,/data,hsize=3
    plotxyz,(findgen(nbin+1)-nbin/2.)*vres,(findgen(nbin+1)-nbin/2.)*vres,N_im_xz2,xrange=vrange,yrange=vrange,zrange=[0,10],/add,mpanel=mpanel_Nxz,xtit=Vx_tit+' [km/s]',ytit=Vz_tit+' [km/s]',tit=time_string(dat.time)+'-'+strmid(time_string(dat.end_time),11,8),charsize=charsize
    loadct2,0
    plots,circle_v2(0,0,mvn_ev2km(10.,mass_assmpt)),color=180
    plots,circle_v2(0,0,mvn_ev2km(100.,mass_assmpt)),color=180
    plots,circle_v2(0,0,mvn_ev2km(1000.,mass_assmpt)),color=180
    plots,circle_v2(0,0,mvn_ev2km(10000.,mass_assmpt)),color=180
    loadct2,39

    ;;plot para-perp plane
    if not keyword_set(mso) then begin
    idx_paraperp = where(N_im_paraperp gt 0)
    idx_zero_paraperp = where(N_im_paraperp eq 0)
    im_paraperp2[idx_paraperp] = im_paraperp[idx_paraperp]/N_im_paraperp[idx_paraperp]
    im_paraperp2[idx_zero_paraperp] = !values.F_NaN
    N_im_paraperp2[idx_paraperp] = N_im_paraperp[idx_paraperp]
    N_im_paraperp2[idx_zero_paraperp] = !values.F_NaN    
    
    plotxyz,(findgen(nbin+1)-nbin/2.)*vres,findgen(nbin/2.+1)*vres,im_paraperp2,xrange=vrange,yrange=[0,vrange[1]],zrange=zrange,/zlog,/add,mpanel=mpanel_imparaperp,xtit='Vpara [km/s]',ytit='Vperp [km/s]',tit=time_string(dat.time)+'-'+strmid(time_string(dat.end_time),11,8),charsize=charsize
    plots,vproton_para_tot,vproton_perp_tot,psym=4,thick=2,color=230
    plots,vproton_para_tot/sqrt(mass_assmpt),vproton_perp_tot/sqrt(mass_assmpt),psym=4,thick=2,color=200
    oplot,[0,0],[0,2*vproton_perp_tot],line=2,color=230
    loadct2,0
    oplot,mvn_ev2km(10.,mass_assmpt)*cos(findgen(180)*!DTOR),mvn_ev2km(10.,mass_assmpt)*sin(findgen(180)*!DTOR),color=180
    oplot,mvn_ev2km(100.,mass_assmpt)*cos(findgen(180)*!DTOR),mvn_ev2km(100.,mass_assmpt)*sin(findgen(180)*!DTOR),color=180
    oplot,mvn_ev2km(1000.,mass_assmpt)*cos(findgen(180)*!DTOR),mvn_ev2km(1000.,mass_assmpt)*sin(findgen(180)*!DTOR),color=180
    oplot,mvn_ev2km(10000.,mass_assmpt)*cos(findgen(180)*!DTOR),mvn_ev2km(10000.,mass_assmpt)*sin(findgen(180)*!DTOR),color=180
    loadct2,39
    plotxyz,(findgen(nbin+1)-nbin/2.)*vres,findgen(nbin/2.+1)*vres,N_im_paraperp2,xrange=vrange,yrange=[0,vrange[1]],zrange=[0,100],/add,mpanel=mpanel_Nparaperp,xtit='Vpara [km/s]',ytit='Vperp [km/s]',tit=time_string(dat.time)+'-'+strmid(time_string(dat.end_time),11,8),charsize=charsize
    plots,vproton_para_tot,vproton_perp_tot,psym=4,thick=2,color=230
    plots,vproton_para_tot/sqrt(mass_assmpt),vproton_perp_tot/sqrt(mass_assmpt),psym=4,thick=2,color=200
    oplot,[0,0],[0,2*vproton_perp_tot],line=2,color=230    
    loadct2,0
    oplot,mvn_ev2km(10.,mass_assmpt)*cos(findgen(180)*!DTOR),mvn_ev2km(10.,mass_assmpt)*sin(findgen(180)*!DTOR),color=180
    oplot,mvn_ev2km(100.,mass_assmpt)*cos(findgen(180)*!DTOR),mvn_ev2km(100.,mass_assmpt)*sin(findgen(180)*!DTOR),color=180
    oplot,mvn_ev2km(1000.,mass_assmpt)*cos(findgen(180)*!DTOR),mvn_ev2km(1000.,mass_assmpt)*sin(findgen(180)*!DTOR),color=180
    oplot,mvn_ev2km(10000.,mass_assmpt)*cos(findgen(180)*!DTOR),mvn_ev2km(10000.,mass_assmpt)*sin(findgen(180)*!DTOR),color=180
    xyouts,-vrange+100,vrange-100,'B: '+string(total(Bmso^2)^.5,format='(f6.2)')+' nT',/data
    loadct2,39
    endif
    stop
    wi,0
    !p.charsize=1.

    vdf_sav = {time:[dat.time, dat.end_time], $
               Odens_100ev:Odens_100ev, Ovel_mso_100ev:Ovel_mso_100ev,$
               Odens_1kev:Odens_1kev, Ovel_mso_1kev:Ovel_mso_1kev,$
               Odens_10kev:Odens_10kev, Ovel_mso_10kev:Ovel_mso_10kev,$
               Odens_all:Odens_all, Ovel_mso_all:Ovel_mso_all,$
               dens:dens, dens_std:dens_std, Vmso:Vmso, Vmso_std:Vmso_std, Bmso:Bmso, Bmso_std:Bmso_std,$
               Theta_b:th_b, Theta_b_std:th_b_std, Phi_b:ph_b, R_b:r_b, ztest_b:z_b,$
               Theta_v:th_v, Theta_v_std:th_v_std, Phi_v:ph_v, R_v:r_v, ztest_v:z_v}

end