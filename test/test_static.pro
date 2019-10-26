pro test_static
   
   trange=['2015-06-25/01:00:00','2015-06-25/01:30:00']
   orbit = 1430
   ;orbit = mean(mvn_orbit_num(time=trange) + 0.5)
   tplot_saved_mvn_pfp,orbit=orbit
   mvn_spice_load
   mvn_sta_l2_load,sta_apid='d0',trange=trange
   dat = mvn_sta_get('d0',tt=trange)
   
   ;; select mass
   counts = reform(dat.cnts,32,4,16,8)
   mass_arr = reform(dat.mass_arr,32,4,16,8)
   if not keyword_set(mass_assmpt) then mass_assmpt = 16.
   if not keyword_set(mass_range) then mass_range = [12.,20.]
   counts2 = sta_mass_select(counts,mass_arr,mass_range)
   dat.cnts = reform(counts2,32,64,8)
   ;sta_getdat_bgrm,dat,dat_new,ratio=0.5
   
   ;; convert counts to physical value
   mvn_sta_convert_units_kei_v2,dat,'df'
   ;; STATIC frame to mso frame
   mvn_pfp_cotrans,dat,from='MAVEN_STATIC',to='MAVEN_MSO',theta=theta_mso,phi=phi_mso
   
   stop
   time = (dat.time + dat.end_time)/2.d0
   counts = reform(dat.cnts,32,4,16,8)
   df = reform(dat.data,32,4,16,8)
   theta_mso_c = reform(theta_mso,32,4,16,8) & dtheta = reform(dat.dtheta,32,4,16,8)
   phi_mso_c = reform(phi_mso,32,4,16,8) & dphi = reform(dat.dphi,32,4,16,8)
   E_c = reform(dat.energy,32,4,16,8) & dE = reform(dat.denergy,32,4,16,8)
   vel_c = mvn_ev2km(E_c,mass_assmpt) & dvel =  mvn_ev2km(dE,mass_assmpt)
   
   stop
   dtheta_arr = dtheta[*,*,*,0]/3.
   dphi_arr = dphi[*,*,*,0]/3.
   
   for idth = 0,2 do begin
    for idph = 0,2 do begin
      
      ;; convert sphere frame to cartesian in the mso frame
      theta_mso = theta_mso_c + dtheta_arr * (idth - 1.)
      phi_mso = phi_mso_c + dphi_arr + (idph - 1.)
      sphere_to_cart, vel_c, theta_mso, phi_mso, vx_mso, vy_mso, vz_mso
      
      ;; get local field vectors
      tspan = [dat.time, dat.end_time]
      field = get_local_field(tspan,orbit=orbit)
      
      vel_c_b = dblarr(32,4,16)
      vel_c_v = dblarr(32,4,16)
      vel_c_e = dblarr(32,4,16)
   
      for ie=0,31 do begin
       for ith=0,3 do begin
        for iph=0,15 do begin
     
          vel_c_bve = mso2bve([Vx_mso[ie,ith,iph,0],Vy_mso[ie,ith,iph,0],Vz_mso[ie,ith,iph,0]],field.V, field.B)
          vel_c_b[ie,ith,iph] = vel_c_bve[0]
          vel_c_v[ie,ith,iph] = vel_c_bve[1]
          vel_c_e[ie,ith,iph] = vel_c_bve[2]
      
        endfor
       endfor
      endfor
   
    endfor
   endfor
   stop
end