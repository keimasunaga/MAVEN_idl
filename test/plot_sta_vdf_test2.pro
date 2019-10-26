function sta_mass_select,counts,mass_arr,mass_range

   idx_out_of_mrange = where(mass_arr lt mass_range[0] or mass_arr gt mass_range[1])
   counts[idx_out_of_mrange] = 0. 
   return,counts

end


function get_uvec_sta,V,theta,phi

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



pro plot_sta_vdf_test2,load=load,show_ring=show_ring,mass_assmpt=mass_assmpt,solar_wind=solar_wind,local=local,mva=mva,mso=mso




   
    wi,1
    ;dat2 = mvn_sta_get('c6')
   ; zscale = max(dat2.data)/1.e5 > 1
   ; contour4d,dat2,/points,/label,/fill,/mass,zrange=zscale*[.1,1.e5],/twt,units='eflux'
    ;stop
    ;dat2 = total( total(dat.data,1), 2 )
    ;dat2d_sc = fltarr(16,4)
    ;for i=0,63s do dat2d_sc[fix(i/4),i mod 4] = dat2[i]
    ;plotxyz,findgen(16)*22.5-157.5-11.25, findgen(4)*22.5-37.5,dat2d_sc,xtit='PHI',ytit='THETA'


    
    ;; get a data structure and reform as a form of [32e, 4p, 16a, 8m]
    if keyword_set(load) then mvn_sta_l2_load,sta_apid=['d0','d1']
    ;dat = mvn_sta_get_d1()
    dat = mvn_sta_get('d1')
    mvn_sta_convert_units,dat,'df'
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
    if not keyword_set(mass_range) then mass_range = [16.,50.]
    counts2 = sta_mass_select(counts,mass_arr,mass_range)
    vel_c = mvn_ev2km(E_c,mass_assmpt) & dvel =  mvn_ev2km(dE,mass_assmpt)
        
    ;; +/- width from the center value 
    theta_m = theta_c - dtheta/2.
    theta_p = theta_c + dtheta/2.
    phi_m = phi_c - dphi/2.
    phi_p = phi_c + dphi/2.
    vel_m = vel_c ;- dvel
    vel_p = vel_c + dvel
    
    ;; convert theta, phi (sphere frame) to cartesian in the sc frame
    sphere_to_cart, 1.d0, theta_m, phi_m, vx_mm, vy_mm, vz_mm
    sphere_to_cart, 1.d0, theta_m, phi_p, vx_mp, vy_mp, vz_mp
    sphere_to_cart, 1.d0, theta_p, phi_m, vx_pm, vy_pm, vz_pm
    sphere_to_cart, 1.d0, theta_p, phi_p, vx_pp, vy_pp, vz_pp
   

    ; load quarntenion
    q = spice_body_att('MAVEN_STATIC', 'MAVEN_MSO', time, $
      /quaternion, verbose=verbose)

    ; rotate 4 vectors at the top of each bin in the static frame into the MSO frame
    d_mm = trans_vec(vx_mm, vy_mm, vz_mm, q)
    d_mp = trans_vec(vx_mp, vy_mp, vz_mp, q)
    d_pm = trans_vec(vx_pm, vy_pm, vz_pm, q)    
    d_pp = trans_vec(vx_pp, vy_pp, vz_pp, q)

    
    theta_mm = d_mm.theta
    phi_mm = d_mm.phi
    theta_mp = d_mp.theta
    phi_mp = d_mp.phi
    theta_pm = d_pm.theta
    phi_pm = d_pm.phi
    theta_pp = d_pp.theta
    phi_pp = d_pp.phi
    
    
    ;; get_vel_mso corrdinates
    u_mmm = get_uvec_sta(vel_m,theta_mm,phi_mm) & u_pmm = get_uvec_sta(vel_p,theta_mm,phi_mm)
    u_mmp = get_uvec_sta(vel_m,theta_mp,phi_mp) & u_pmp = get_uvec_sta(vel_p,theta_mp,phi_mp)
    u_mpm = get_uvec_sta(vel_m,theta_pm,phi_pm) & u_ppm = get_uvec_sta(vel_p,theta_pm,phi_pm)
    u_mpp = get_uvec_sta(vel_m,theta_pp,phi_pp) & u_ppp = get_uvec_sta(vel_p,theta_pp,phi_pp)
    
    u_c = get_uvec_sta(vel_c,theta_c,phi_c)
    
    ux = u_c.ux[sort(u_c.ux)]
    uy = u_c.uy[sort(u_c.ux)]
    uz = u_c.uz[sort(u_c.ux)]
    counts3 = counts2[sort(u_c.ux)]
    stop
    ;; load solar wind information and calculate convection electric field
    if keyword_set(solar_wind) then begin
      orbit = 426
      filename = SAVE_LOC + '/maven/sav/solar_wind_info/15min/sw_info_'+string(orbit,format='(i05)')+'.sav' ;00426.sav'
      restore,filename
      Vmso = total(dat_sw.VEL,2)/2.  ;;average velocity for the inbound and outbound
      Vmso_tot = sqrt(Vmso[0]^2+Vmso[1]^2+Vmso[2]^2)
      Bmso = total(dat_sw.MAG_MSO,2)/2. ;; average magnetic field for the inbound and outbound
      Bmso_e = Bmso/sqrt(Bmso[0]^2+Bmso[1]^2+Bmso[2]^2)
      vproton_perp_tot = sqrt(Vmso_tot^2 - inpro(Vmso,Bmso_e)^2)
    endif
    
    ;; get local velocity vector and magnetic field vector and calculate local convection electric field
    if keyword_set(local) then begin
      idx_vel = nn('mvn_swim_velocity_mso',[dat.time,dat.end_time])
      get_data,'mvn_swim_velocity_mso',t_vel,y_vel
      Vmso = total(y_vel[idx_vel[0]:idx_vel[1],*],1)/(idx_vel[1]-idx_vel[0]+1.)
      Vmso_tot = sqrt(Vmso[0]^2+Vmso[1]^2+Vmso[2]^2)
      
      idx_mag = nn('mvn_B_1sec_MAVEN_MSO',[dat.time,dat.end_time])
      get_data,'mvn_B_1sec_MAVEN_MSO',t_mag,y_mag
      Bmso = total(y_mag[idx_mag[0]:idx_mag[1],*],1)/(idx_mag[1]-idx_mag[0]+1.)
      Bmso_e = Bmso/sqrt(Bmso[0]^2+Bmso[1]^2+Bmso[2]^2)
      vproton_perp_tot = sqrt(Vmso_tot^2 - inpro(Vmso,Bmso_e)^2)
    endif
    
      ;; use mva method and get axes of the plasma sheet
     if keyword_set(mva) then begin  
      Bevec = get_mag_eigenvec_mva('mvn_B_1sec_MAVEN_MSO',tcenter=dat.time,trange=[dat.time,dat.end_time])
      Bi = Bevec[*,0] & Bj = Bevec[*,1] & Bk = Bevec[*,2]
      Bmso = Bj
      Bmso_e = Bj   ;;Bj (medium variance is the direction of the magnetic field)
      
      idx_vel = nn('mvn_swim_velocity_mso',[dat.time,dat.end_time])
      get_data,'mvn_swim_velocity_mso',t_vel,y_vel
      Vmso = total(y_vel[idx_vel[0]:idx_vel[1],*],1)/(idx_vel[1]-idx_vel[0]+1.)
      Vmso_tot = sqrt(Vmso[0]^2+Vmso[1]^2+Vmso[2]^2)
      vproton_perp_tot = sqrt(Vmso_tot^2 - inpro(Vmso,Bmso_e)^2)
      stop
     endif
    ;; 
    stop
    dat_v = mso2bve([[ux],[uy],[uz]],Vmso,Bmso)
    stop
 ;   u_mmm = mso2bve([[u_mmm.ux],[u_mmm.uy],[u_mmm.uz]], Vmso, Bmso)
 ;   u_mmp = mso2bve([u_mmp.ux,u_mmp.uy,u_mmp.uz], Vmso, Bmso)
 ;   u_mpm = mso2bve([u_mpm.ux,u_mpm.uy,u_mpm.uz], Vmso, Bmso)
 ;   u_mpp = mso2bve([u_mpp.ux,u_mpp.uy,u_mpp.uz], Vmso, Bmso)
    
    
    ;;;;;;;;;;;;;;
    
    if not keyword_set(nbin) then nbin = 80
    if not keyword_set(vres) then vres = 20.
    im_xy = fltarr(nbin+1,nbin+1)
    N_im_xy = fltarr(nbin+1,nbin+1)
    im_yz = fltarr(nbin+1,nbin+1)
    N_im_yz = fltarr(nbin+1,nbin+1)
    im_xz = fltarr(nbin+1,nbin+1)
    N_im_xz = fltarr(nbin+1,nbin+1)
       
    im_th_ph_mso = fltarr(360,180)
    
    for ie=0,31 do begin
       for ia=0,15 do begin
         for ip=0,3 do begin
                    
            ;8 point of a polygon in the mso coordnate
            u_mmm_x = u_mmm.ux[ie,ip,ia,0] & u_mmm_y = u_mmm.uy[ie,ip,ia,6] & u_mmm_z = u_mmm.uz[ie,ip,ia,6]
            u_mmp_x = u_mmp.ux[ie,ip,ia,0] & u_mmp_y = u_mmp.uy[ie,ip,ia,6] & u_mmp_z = u_mmp.uz[ie,ip,ia,6]
            u_mpm_x = u_mpm.ux[ie,ip,ia,0] & u_mpm_y = u_mpm.uy[ie,ip,ia,6] & u_mpm_z = u_mpm.uz[ie,ip,ia,6]
            u_mpp_x = u_mpp.ux[ie,ip,ia,0] & u_mpp_y = u_mpp.uy[ie,ip,ia,6] & u_mpp_z = u_mpp.uz[ie,ip,ia,6]
            u_pmm_x = u_pmm.ux[ie,ip,ia,0] & u_pmm_y = u_pmm.uy[ie,ip,ia,6] & u_pmm_z = u_pmm.uz[ie,ip,ia,6]
            u_pmp_x = u_pmp.ux[ie,ip,ia,0] & u_pmp_y = u_pmp.uy[ie,ip,ia,6] & u_pmp_z = u_pmp.uz[ie,ip,ia,6]
            u_ppm_x = u_ppm.ux[ie,ip,ia,0] & u_ppm_y = u_ppm.uy[ie,ip,ia,6] & u_ppm_z = u_ppm.uz[ie,ip,ia,6]
            u_ppp_x = u_ppp.ux[ie,ip,ia,0] & u_ppp_y = u_ppp.uy[ie,ip,ia,6] & u_ppp_z = u_ppp.uz[ie,ip,ia,6]
            
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
            
              vx = [v_mmm[0],v_pmm[0],v_mmp[0],v_pmp[0],v_mpm[0],v_ppm[0],v_mpp[0],v_ppp[0]]
              vy = [v_mmm[1],v_pmm[1],v_mmp[1],v_pmp[1],v_mpm[1],v_ppm[1],v_mpp[1],v_ppp[1]]
              vz = [v_mmm[2],v_pmm[2],v_mmp[2],v_pmp[2],v_mpm[2],v_ppm[2],v_mpp[2],v_ppp[2]]
             endif           
            
            
             

            
            idx_xy_roi = find_vdist_roi(vx,vy)
            inside_xy = polyfillv(round(Vx[idx_xy_roi]/vres)+nbin/2.,round(Vy[idx_xy_roi]/vres)+nbin/2.,nbin+1,nbin+1)
            Nxy = fltarr(nbin+1,nbin+1) & Cts_xy = fltarr(nbin+1,nbin+1)
            Nxy[inside_xy] = 1 & Cts_xy[inside_xy] = total(counts2[ie,ip,ia,*])
            N_im_xy = N_im_xy + Nxy & im_xy = im_xy + Cts_xy
            
            idx_yz_roi = find_vdist_roi(vy,vz)
            inside_yz = polyfillv(round(Vy[idx_yz_roi]/vres)+nbin/2.,round(Vz[idx_yz_roi]/vres)+nbin/2.,nbin+1,nbin+1)
            Nyz = fltarr(nbin+1,nbin+1) & Cts_yz = fltarr(nbin+1,nbin+1)
            Nyz[inside_yz] = 1 & Cts_yz[inside_yz] = total(counts2[ie,ip,ia,*])
            N_im_yz = N_im_yz + Nyz & im_yz = im_yz + Cts_yz
            
            idx_xz_roi = find_vdist_roi(vx,vz)
            inside_xz = polyfillv(round(Vx[idx_xz_roi]/vres)+nbin/2.,round(Vz[idx_xz_roi]/vres)+nbin/2.,nbin+1,nbin+1)
            Nxz = fltarr(nbin+1,nbin+1) & Cts_xz = fltarr(nbin+1,nbin+1)
            Nxz[inside_xz] = 1 & Cts_xz[inside_xz] = total(counts2[ie,ip,ia,*])
            N_im_xz = N_im_xz + Nxz & im_xz = im_xz + Cts_xz         
                
         endfor       
       endfor
       
       
    endfor
    
    wi,1
    
    idx_xy = where(N_im_xy gt 0)
    im_xy[idx_xy] = im_xy[idx_xy]/N_im_xy[idx_xy] 
    plotxyz,(findgen(nbin+1)-nbin/2.)*20,(findgen(nbin+1)-nbin/2.)*20,im_xy,xrange=[-nbin/2.*vres,nbin/2.*vres],yrange=[-nbin/2.*vres,nbin/2.*vres],/zlog,mult='2,3',xtit='Vx [km/s]',ytit='Vy [km/s]',tit=time_string(dat.time)+'-'+strmid(time_string(dat.end_time),11,8)
    plots,circle_v2(0,0,mvn_ev2km(10.,mass_assmpt)),color=255
    plots,circle_v2(0,0,mvn_ev2km(100.,mass_assmpt)),color=255
    plots,circle_v2(0,0,mvn_ev2km(1000.,mass_assmpt)),color=255
    plots,circle_v2(0,0,mvn_ev2km(10000.,mass_assmpt)),color=255
    plotxyz,(findgen(nbin+1)-nbin/2.)*20,(findgen(nbin+1)-nbin/2.)*20,N_im_xy,xrange=[-nbin/2.*vres,nbin/2.*vres],yrange=[-nbin/2.*vres,nbin/2.*vres],zrange=[1,100],/zlog,/add,xtit='Vx [km/s]',ytit='Vy [km/s]'

    idx_yz = where(N_im_yz gt 0)
    im_yz[idx_yz] = im_yz[idx_yz]/N_im_yz[idx_yz]
    plotxyz,(findgen(nbin+1)-nbin/2.)*20,(findgen(nbin+1)-nbin/2.)*20,im_yz,xrange=[-nbin/2.*vres,nbin/2.*vres],yrange=[-nbin/2.*vres,nbin/2.*vres],/zlog,/add,xtit='Vy [km/s]',ytit='Vz [km/s]'
    plots,circle_v2(0,0,mvn_ev2km(10.,mass_assmpt)),color=255
    plots,circle_v2(0,0,mvn_ev2km(100.,mass_assmpt)),color=255
    plots,circle_v2(0,0,mvn_ev2km(1000.,mass_assmpt)),color=255
    plots,circle_v2(0,0,mvn_ev2km(10000.,mass_assmpt)),color=255
    
    if keyword_set(show_ring) then begin
   
       plots,circle_v2(vproton_perp_tot,0,vproton_perp_tot),color=230,line=2
      
      
    endif
    
    
    plotxyz,(findgen(nbin+1)-nbin/2.)*20,(findgen(nbin+1)-nbin/2.)*20,N_im_yz,xrange=[-nbin/2.*vres,nbin/2.*vres],yrange=[-nbin/2.*vres,nbin/2.*vres],zrange=[1,100],/zlog,/add,xtit='Vy [km/s]',ytit='Vz [km/s]'
    
    idx_xz = where(N_im_xz gt 0)
    im_xz[idx_xz] = im_xz[idx_xz]/N_im_xz[idx_xz]
    plotxyz,(findgen(nbin+1)-nbin/2.)*20,(findgen(nbin+1)-nbin/2.)*20,im_xz,xrange=[-nbin/2.*vres,nbin/2.*vres],yrange=[-nbin/2.*vres,nbin/2.*vres],/zlog,/add,xtit='Vx [km/s]',ytit='Vz [km/s]'
    plots,circle_v2(0,0,mvn_ev2km(10.,mass_assmpt)),color=255
    plots,circle_v2(0,0,mvn_ev2km(100.,mass_assmpt)),color=255
    plots,circle_v2(0,0,mvn_ev2km(1000.,mass_assmpt)),color=255
    plots,circle_v2(0,0,mvn_ev2km(10000.,mass_assmpt)),color=255
    plotxyz,(findgen(nbin+1)-nbin/2.)*20,(findgen(nbin+1)-nbin/2.)*20,N_im_xz,xrange=[-nbin/2.*vres,nbin/2.*vres],yrange=[-nbin/2.*vres,nbin/2.*vres],zrange=[1,100],/zlog,/add,xtit='Vx [km/s]',ytit='Vz [km/s]'
    
    
    
    wi,0

stop
end