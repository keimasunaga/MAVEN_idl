;+
; :Description:
;    This routine draws ion velocity distribution obtained from STATIC.
;    It also overplots ring distribution and its width.
;    Outward and inward VDFs as well as the original VDF are plotted.    
;
; ${parameters}
; dat: d0 or d1 (or cf, ce) Structure 
; nvb: structure including density, velocity, and magnetic field. Use function get_swimag_nvb,trange
; pos: MAVEN mso position. Use pro maven_state_mso,pos_mvn,vel_mvn,time_pos,trange=trange
; 
; ${keywords}
; sta_apid: STATIC apid (default is d0)
; mass_range: Ion mass range to integrate, default is [12,20]
; mass_assmpt: Ion mass assumed
; erange:
; angle_range:
; units: Unit of VDF, default is eflux. If you want to plot in the unit of phase space density, set this keyword as 'df'
; mult:
; panel_mode:
; charsize: Characer size of the VDF plot
; nbin:
; vres: 
; vrange: Velocity range for VDF.
; show_ring: Overplot ring distribution
; surf=surf: Overplot shock surface
; 
; ${Return values}
;
; ${Related routines}
;  main_plot_sta_vdf_rmv_ring.pro
;
; $Author: Kei Masunaga (@EPS, Univ. Tokyo)
;
; $Last modified Jan 31, 2017
;-




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



pro plot_sta_vdf_rmv_ring,dat,nvb,pos,nvb_upstrm=nvb_upstrm,$      ;;input
                          ;flux_sav=flux_sav,$ ;;output
                          sta_apid=sta_apid,mass_range=mass_range,mass_assmpt=mass_assmpt,erange=erange,angle_range=angle_range,units=units,$                 
                          mult=mult,panel_mode=panel_mode,charsize=charsize,$
                          nbin=nbin,vres=vres,vrange=vrange,show_ring=show_ring,surf=surf,$
                          vsh_dir=vsh_dir,rmv_ring=rmv_ring,ring_range=ring_range
                 

                      
    ;; difine bin size and vresolution for vdf
    if not keyword_set(angle_range) then angle_range = [-22.5, 22.5]
    if not keyword_set(units) then units='eflux'
    if not keyword_set(sta_apid) then sta_apid = 'd0'  
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
       
    
    ;; get velocity vector and magnetic field vector of solar wind or local flow.
    ;nvb = get_swimag_nvb([dat.time,dat.end_time]);,vb,solar_wind=solar_wind,local=local,mva=mva
    dens = nvb.dens & dens_std = nvb.dens_std
    Vmso = nvb.Vmso & Vmso_e = Vmso/total(Vmso^2)^.5 & Vmso_std = nvb.Vmso_std
    th_v = nvb.theta_v & th_v_std = nvb.theta_v_std & ph_v = nvb.phi_v & r_v = nvb.r_v & z_v = nvb.ztest_v
    Bmso = nvb.Bmso & Bmso_e = Bmso/total(Bmso^2)^.5 & Bmso_std = nvb.Bmso_std
    th_b = nvb.theta_b & th_b_std = nvb.theta_b_std & ph_b = nvb.phi_b & r_b = nvb.r_b & z_b = nvb.ztest_b
    vproton_perp_tot = nvb.vperp & vproton_para_tot = nvb.vpara
    Emso = -crossp(Vmso,Bmso) & Emso_e = -crossp(Vmso_e,Bmso_e)
    
    if keyword_set(nvb_upstrm) then begin
      Vmso_us = nvb_upstrm.Vmso & Vmso_us_e = Vmso_us/total(Vmso_us^2)^.5
      Bmso_us = nvb_upstrm.Bmso & Bmso_us_e = Bmso_us/total(Bmso_us^2)^.5
      
      ;;calculate a coordinate conversion matrix
      Vbve_us = mso2bve(Vmso_us,Vmso_e,Bmso_e)
      Vbve_us_e = mso2bve(Vmso_us_e,Vmso_e,Bmso_e) 
      Bbve_us_e = mso2bve(Bmso_us_e,Vmso_e,Bmso_e)
      Ebve_us_e = -crossp(Vbve_us_e,Bbve_us_e) 
      Vbve_us_perp = nvb_upstrm.vperp      
      mtx = invert([ [Bbve_us_e], [Vbve_us_e], [Ebve_us_e] ])
;      ;;Testing code below
;      Ebve_us2 = -crossp(Vbve_us_e,[1.,0.,0.])
;      Ebve_us_e2 = Ebve_us2/sqrt(Ebve_us2[0]^2 + Ebve_us2[1]^2 + Ebve_us2[2]^2)
;      Vbve_us_e2 = crossp(Ebve_us_e2,[1.,0.,0.])
;      mtx = invert([ [1.,0.,0.], [Vbve_us_e2], [Ebve_us_e2] ])
;       ;;
      
      ;;ring distribution in the upstream (or downstream) bve coordinate
      tau = findgen(360)*!DTOR      
      vus_ring = fltarr(3,360)
      vus_ring[1,*] = Vbve_us_perp * (1.+ cos(tau))
      vus_ring[2,*] = Vbve_us_perp * sin(tau)
      ;;Convert the upstream (or downstream) ring distribution to the original bve coordinate
      for itau = 0,n_elements(tau)-1 do begin
        vus_ring[*,itau] = mtx ## vus_ring[*,itau]
      endfor    
      vus_ring_v = vus_ring[1,*];mtx[1,1]*Vbve_us_perp*(1.+cos(tau)) + mtx[2,1]*Vbve_us_perp*sin(tau)
      vus_ring_e = vus_ring[2,*];mtx[1,2]*Vbve_us_perp*(1.+cos(tau)) + mtx[2,2]*Vbve_us_perp*sin(tau)
      
      ;;Reconvert
      mtxinv = invert(mtx)
      vus_ring2 = fltarr(3,360)
      for itau = 0,n_elements(tau)-1 do begin
        vus_ring2[*,itau] = mtxinv ## vus_ring[*,itau]
      endfor
      
      vus_ring_v2 = vus_ring2[1,*];mtxinv[1,1]*vus_ring_v*(1.+cos(tau)) + mtxinv[2,1]*vus_ring_e*sin(tau)
      vus_ring_e2 = vus_ring2[2,*];mtxinv[1,2]*vus_ring_v*(1.+cos(tau)) + mtxinv[2,2]*vus_ring_e*sin(tau)
    endif
 
    rot = get_rot_angle(vmso,bmso)
    if finite(rot) eq 0 then begin 
      print,'vmso or bmso is missing, no plot.'
      goto, no_output
    endif    ;; let's put mag filter here to remove fluctuating field in the future!!
    
    ;; start processing static data    
;    mvn_pfp_cotrans,dat,from='MAVEN_STATIC',to='MAVEN_MSO',theta=theta_mso,phi=phi_mso
    env = init_env()
    SAVE_LOC = env.SAVE_LOC
    time = (dat.time + dat.end_time)/2.d0
    counts = reform(dat.cnts,32,4,16,8)    
    theta_c = reform(dat.theta,32,4,16,8) & dtheta = reform(dat.dtheta,32,4,16,8)
    phi_c = reform(dat.phi,32,4,16,8) & dphi = reform(dat.dphi,32,4,16,8)
    E_c = reform(dat.energy,32,4,16,8) & dE = reform(dat.denergy,32,4,16,8)
    mass_arr = reform(dat.mass_arr,32,4,16,8)
    if not keyword_set(mass_assmpt) then mass_assmpt = 16.
    if not keyword_set(mass_range) then mass_range = [12.,20.]
    counts2 = sta_mass_select(counts,mass_arr,mass_range)
    dat.cnts = counts2
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
    q = spice_body_att('MAVEN_STATIC', 'MAVEN_MSO', time, /quaternion, verbose=verbose)
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
    
    ;; get_vel_mso and mse corrdinates
    u_mmm = get_uvec_mso(vel_m,theta_mm,phi_mm) & u_pmm = get_uvec_mso(vel_p,theta_mm,phi_mm)
    u_mmp = get_uvec_mso(vel_m,theta_mp,phi_mp) & u_pmp = get_uvec_mso(vel_p,theta_mp,phi_mp)
    u_mpm = get_uvec_mso(vel_m,theta_pm,phi_pm) & u_ppm = get_uvec_mso(vel_p,theta_pm,phi_pm)
    u_mpp = get_uvec_mso(vel_m,theta_pp,phi_pp) & u_ppp = get_uvec_mso(vel_p,theta_pp,phi_pp)
    u_c = get_uvec_mso(vel_c,theta_c_mso,phi_c_mso)   
    u_c_mse = mso2mse_mlt(u_c.ux,u_c.uy,u_c.uz,rot)
    vmse = mso2mse(vmso[0],vmso[1],vmso[2],rot)
    bmse = mso2mse(bmso[0],bmso[1],bmso[2],rot)
       
    ;;Derive fluxes in a side of the shock normal direction from VDF
    pos_mse = mso2mse(pos[0],pos[1],pos[2],rot)
    pos_bve = mso2bve(pos,vmso,bmso)
    vec_norm_sh = get_norm_shock3(pos_mse,theta=theta_bs_arr,phi=phi_bs_arr,ind=ind_norm) ;;shock normal vector in the mse frame
    theta_bs = theta_bs_arr[ind_norm[0]]
    phi_bs = phi_bs_arr[ind_norm[1]]
    vec_norm_sh_bve = mso2bve(vec_norm_sh,vmse,bmse) ;;shock normal vector in the bve frame
    agl_slope_bv = atan(-vec_norm_sh_bve[0]/vec_norm_sh_bve[1]) & agl_slope_ve = atan(-vec_norm_sh_bve[1]/vec_norm_sh_bve[2]) & agl_slope_be = atan(-vec_norm_sh_bve[0]/vec_norm_sh_bve[2])
    slope_bv = -vec_norm_sh_bve[0]/vec_norm_sh_bve[1] & slope_ve = -vec_norm_sh_bve[1]/vec_norm_sh_bve[2] & slope_be = -vec_norm_sh_bve[0]/vec_norm_sh_bve[2]    
    if pos[0] lt 0 then goto,no_output
    if ind_norm[0] ge 18 then goto, no_output
    if vec_norm_sh[0] lt 0 then stop
    if vec_norm_sh[0] lt 0 then vec_norm_sh = -vec_norm_sh
    dat_out = dat
    idx_u_sh = intarr(32,4,16,8)
    
    ;;check angel between the shock normal vector and each velocity in the MSE frame
    for ss=0,n_elements(u_c_mse.x)-1 do begin      
       umse = [u_c_mse.x[ss], u_c_mse.y[ss], u_c_mse.z[ss]]
       agl_u_sh = acos(inpro(vec_norm_sh,umse)/norm(vec_norm_sh)/norm(umse))*!RADEG
       if agl_u_sh lt 90. then idx_u_sh[ss] = 1 else idx_u_sh[ss] = -1        
    endfor
    
    ;;remove counts parallel/anti-parallel to the shock normal
     ;; Get +Vsh counts
    if keyword_set(vsh_dir) then begin
     if vsh_dir eq 1 then begin
       idx_p = where(idx_u_sh eq -1)
       counts2[idx_p] = 0.
       counts3 = reform(counts2,32,64,8)
       dat_out.cnts = counts3
     endif
    ;; Get -Vsh counts
     if vsh_dir eq -1 then begin      
      idx_p = where(idx_u_sh eq 1)
      counts2[idx_p] = 0.
      counts3 = reform(counts2,32,64,8)
      dat_out.cnts = counts3
     endif
    endif
    
    mvn_sta_convert_units_kei_v2,dat_out,'df'
    df = dat_out.data
    df2 = reform(df,32,4,16,8)
    
    
    ;;set energy range, default is energetic ions for [100, 30000] eV. 
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
            u_c_x = u_c.ux[ie,ip,ia,6] & u_c_y = u_c.uy[ie,ip,ia,6] & u_c_z = u_c.uz[ie,ip,ia,6]
         
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
              v_c = mso2bve([[u_c_x],[u_c_y],[u_c_z]], Vmso, Bmso)
                           
              vx = [v_mmm[0],v_mmp[0],v_mpm[0],v_mpp[0],v_pmm[0],v_pmp[0],v_ppm[0],v_ppp[0]]
              vy = [v_mmm[1],v_mmp[1],v_mpm[1],v_mpp[1],v_pmm[1],v_pmp[1],v_ppm[1],v_ppp[1]]
              vz = [v_mmm[2],v_mmp[2],v_mpm[2],v_mpp[2],v_pmm[2],v_pmp[2],v_ppm[2],v_ppp[2]]
              vpara = vx
              vperp = sqrt(vy^2 + vz^2)
              vdist_from_ring = sqrt((V_c[1]-vproton_perp_tot)^2 + V_c[2]^2)
              pagl_c =  acos(inpro(v_c,[1.,0.,0.])/total(v_c^2)^.5) * !radeg   
              
              ;;remove plume (only in the +E hemisphere)
              if keyword_set(rmv_ring) then begin
                  if not keyword_set(ring_range) then ring_range = [0.5, 1.5]
                  if pos_mse[2] ge 0 then begin
                    if vdist_from_ring le vproton_perp_tot*ring_range[1] and vdist_from_ring ge vproton_perp_tot*ring_range[0] $
                    and v_c[1] le vproton_perp_tot and v_c[2] gt 0 $
                    and (pagl_c ge 45 and pagl_c le 135) $
                    then begin
                      counts2[ie,ip,ia,*]=0.
                      df2[ie,ip,ia,*]=0.
                    endif
                  endif  
              endif
             endif           
                
            if agl_xy ge angle_range[0] and agl_xy le angle_range[1] then begin
              idx_xy_roi = find_vdist_roi(vx,vy)
              inside_xy = polyfillv(round(Vx[idx_xy_roi]/vres)+nbin/2.,round(Vy[idx_xy_roi]/vres)+nbin/2.,nbin+1,nbin+1)
              if inside_xy[0] eq -1 then stop
              Nxy = fltarr(nbin+1,nbin+1) & Cts_xy = fltarr(nbin+1,nbin+1)
              Nxy[inside_xy] = 1 & Cts_xy[inside_xy] = total(df2[ie,ip,ia,*])
              N_im_xy = N_im_xy + Nxy & im_xy = im_xy + Cts_xy
            endif
            
            if agl_yz ge angle_range[0] and agl_yz le angle_range[1] then begin
              idx_yz_roi = find_vdist_roi(vy,vz)           
              inside_yz = polyfillv(round(Vy[idx_yz_roi]/vres)+nbin/2.,round(Vz[idx_yz_roi]/vres)+nbin/2.,nbin+1,nbin+1)
              Nyz = fltarr(nbin+1,nbin+1) & Cts_yz = fltarr(nbin+1,nbin+1)
              Nyz[inside_yz] = 1 & Cts_yz[inside_yz] = total(df2[ie,ip,ia,*])
              N_im_yz = N_im_yz + Nyz & im_yz = im_yz + Cts_yz 
            endif
            
            if agl_xz ge angle_range[0] and agl_xz le angle_range[1] then begin
              idx_xz_roi = find_vdist_roi(vx,vz)
              inside_xz = polyfillv(round(Vx[idx_xz_roi]/vres)+nbin/2.,round(Vz[idx_xz_roi]/vres)+nbin/2.,nbin+1,nbin+1)
              Nxz = fltarr(nbin+1,nbin+1) & Cts_xz = fltarr(nbin+1,nbin+1)
              Nxz[inside_xz] = 1 & Cts_xz[inside_xz] = total(df2[ie,ip,ia,*])
              N_im_xz = N_im_xz + Nxz & im_xz = im_xz + Cts_xz        
            endif
            
            if not keyword_set(mso) then begin
              idx_paraperp_roi = find_vdist_roi(vpara,vperp)
              inside_paraperp = polyfillv(round(Vpara[idx_paraperp_roi]/vres)+nbin/2.,round(Vperp[idx_paraperp_roi]/vres),nbin+1,nbin+1)
              Nparaperp = fltarr(nbin+1,nbin/2.+1) & Cts_paraperp = fltarr(nbin+1,nbin/2.+1)
              Nparaperp[inside_paraperp] = 1 & Cts_paraperp[inside_paraperp] = total(df2[ie,ip,ia,*])
              N_im_paraperp = N_im_paraperp + Nparaperp & im_paraperp = im_paraperp + Cts_paraperp
            endif                  
              
         endfor       
       endfor
       
       out_of_energy:
    endfor
    
   if keyword_set(rmv_ring) then begin 
    dat_out.cnts = reform(counts2,32,64,8)
    dat_out.data = reform(df2,32,64,8)
   endif  
        
    ;; process data to plot vdf
    wi,1
    if keyword_set(mso) then begin
       vx_tit = 'Vx' & vy_tit='Vy' & vz_tit='Vz'
    endif else begin
       vx_tit = 'Vb' & vy_tit = 'Vv' & vz_tit = 'Ve'    
    endelse
    
    if keyword_set(panel_mode) then begin
      nhrz = long(strmid(mult,0,1))
      nvrt = long(strmid(mult,2,1))
      
      if panel_mode eq 3 then begin
        mpanel_imxy = '0,'+strcompress(string(nvrt-3),/remove)
        mpanel_imyz = '1,'+strcompress(string(nvrt-3),/remove)
        mpanel_imxz = '2,'+strcompress(string(nvrt-3),/remove) 
      endif
      
      if panel_mode eq 1 then begin
         mpanel_imxy = '0,'+strcompress(string(nvrt-2),/remove)
         mpanel_imyz = '1,'+strcompress(string(nvrt-2),/remove)
         mpanel_imxz = '2,'+strcompress(string(nvrt-2),/remove)
      endif
      
      if panel_mode eq 2 then begin
        mpanel_imxy = '0,'+strcompress(string(nvrt-1),/remove)
        mpanel_imyz = '1,'+strcompress(string(nvrt-1),/remove)
        mpanel_imxz = '2,'+strcompress(string(nvrt-1),/remove)
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
    
    if units eq 'df' then zrange = [1e-16,1e-9]
    if units eq 'eflux' then zrange = [1e3,1e7]
    if units eq 'flux' then zrange = [1e-4,1e2]
    
    ;;plot xy plane (bv plane)
    if not keyword_set(panel_mode) then mult='2,4'
    idx_xy = where(N_im_xy gt 0)
    im_xy2[idx_xy] = im_xy[idx_xy]/N_im_xy[idx_xy]
    idx_zero_xy = where(N_im_xy eq 0)
    im_xy2[idx_zero_xy] = !values.F_NaN
    N_im_xy2[idx_xy] = N_im_xy[idx_xy]
    N_im_xy2[idx_zero_xy] = !values.F_NaN

    plotxyz,(findgen(nbin+1)-nbin/2.)*vres,(findgen(nbin+1)-nbin/2.)*vres,im_xy2,xrange=vrange,yrange=vrange,zrange=zrange,/zlog,mult=mult,mpanel=mpanel_imxy,xtit=Vx_tit+' [km/s]',ytit=Vy_tit+' [km/s]',tit=time_string(dat_out.time)+'-'+strmid(time_string(dat_out.end_time),11,8),charsize=charsize
    loadct2,0
    plots,circle_v2(0,0,mvn_ev2km(10.,mass_assmpt)),color=180
    plots,circle_v2(0,0,mvn_ev2km(100.,mass_assmpt)),color=180
    plots,circle_v2(0,0,mvn_ev2km(1000.,mass_assmpt)),color=180
    plots,circle_v2(0,0,mvn_ev2km(10000.,mass_assmpt)),color=180
    loadct2,39
    arrow,sundir_bv[0]*650,sundir_bv[1]*650,sundir_bv[0]*750,sundir_bv[1]*750,color=210,/data,hsize=3
    arrow,marsdir_bv[0]*650,marsdir_bv[1]*650,marsdir_bv[0]*750,marsdir_bv[1]*750,color=230,/data,hsize=3
    if keyword_set(surf) then begin
      oplot,vrange,slope_bv*vrange,color=230,line=3
    endif   

    ;;plot yz plane (ve plane)
    idx_yz = where(N_im_yz gt 0)
    idx_zero_yz = where(N_im_yz eq 0)
    im_yz2[idx_yz] = im_yz[idx_yz]/N_im_yz[idx_yz]
    im_yz2[idx_zero_yz] = !values.F_NaN
    N_im_yz2[idx_yz] = N_im_yz[idx_yz]
    N_im_yz2[idx_zero_yz] = !values.F_NaN
   
    plotxyz,(findgen(nbin+1)-nbin/2.)*vres,(findgen(nbin+1)-nbin/2.)*vres,im_yz2,xrange=vrange,yrange=vrange,zrange=zrange,/zlog,/add,mpanel=mpanel_imyz,xtit=Vy_tit+' [km/s]',ytit=Vz_tit+' [km/s]',tit=time_string(dat_out.time)+'-'+strmid(time_string(dat_out.end_time),11,8),charsize=charsize
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
      if keyword_set(nvb_upstrm) then begin
        oplot,vus_ring_v,vus_ring_e,color=200,line=2   
        oplot,vus_ring_v2,vus_ring_e2,color=140,line=2       
      endif
      if keyword_set(rmv_ring) then begin       
         plots,circle_v2(vproton_perp_tot,0,vproton_perp_tot*ring_range[1]),color=230,line=2
         plots,circle_v2(vproton_perp_tot,0,vproton_perp_tot*ring_range[0]),color=230,line=2
         t = (!pi/2./100.)*(findgen(101)+100.)
         vv_quater_outer = ring_range[1]*vproton_perp_tot*cos(t) + vproton_perp_tot
         ve_quater_outer = ring_range[1]*vproton_perp_tot*sin(t)
         vv_quater_inner = ring_range[0]*vproton_perp_tot*cos(t) + vproton_perp_tot
         ve_quater_inner = ring_range[0]*vproton_perp_tot*sin(t)
         oplot,vv_quater_outer,ve_quater_outer,color=80,thick=3
         oplot,vv_quater_inner,ve_quater_inner,color=80,thick=3
         oplot,[vproton_perp_tot,vproton_perp_tot],[vproton_perp_tot*ring_range[0],vproton_perp_tot*ring_range[1]],color=80,thick=3
         oplot,[-vproton_perp_tot*(1.-ring_range[1]),vproton_perp_tot*(1.-ring_range[1])],[0,0],color=80,thick=3
      endif
    endif
    if keyword_set(surf) then begin
       oplot,vrange,slope_ve*vrange,color=230,line=3
    endif
    
    ;;plot xz plane  (be plane)
    idx_xz = where(N_im_xz gt 0)
    idx_zero_xz = where(N_im_xz eq 0)
    im_xz2[idx_xz] = im_xz[idx_xz]/N_im_xz[idx_xz]
    im_xz2[idx_zero_xz] = !values.F_NaN
    N_im_xz2[idx_xz] = N_im_xz[idx_xz]
    N_im_xz2[idx_zero_xz] = !values.F_NaN
    
    plotxyz,(findgen(nbin+1)-nbin/2.)*vres,(findgen(nbin+1)-nbin/2.)*vres,im_xz2,xrange=vrange,yrange=vrange,zrange=zrange,/zlog,/add,mpanel=mpanel_imxz,xtit=Vx_tit+' [km/s]',ytit=Vz_tit+' [km/s]',tit=time_string(dat_out.time)+'-'+strmid(time_string(dat_out.end_time),11,8),charsize=charsize
    loadct2,0
    plots,circle_v2(0,0,mvn_ev2km(10.,mass_assmpt)),color=180
    plots,circle_v2(0,0,mvn_ev2km(100.,mass_assmpt)),color=180
    plots,circle_v2(0,0,mvn_ev2km(1000.,mass_assmpt)),color=180
    plots,circle_v2(0,0,mvn_ev2km(10000.,mass_assmpt)),color=180
    loadct2,39
    arrow,sundir_be[0]*650,sundir_be[1]*650,sundir_be[0]*750,sundir_be[1]*750,color=210,/data,hsize=3
    arrow,marsdir_be[0]*650,marsdir_be[1]*650,marsdir_be[0]*750,marsdir_be[1]*750,color=230,/data,hsize=3
    if keyword_set(surf) then begin
      oplot,vrange,slope_be*vrange,color=230,line=3
    endif    
    noplot:
  endif 



  erange_20kev = [20000,35000.]
  calc_flux_sh,dat_out,$
    Odens=Odens_20keV,Ovel_mso=Ovel_mso_20keV,Ovel_mse=Ovel_mse_20keV,Ovel_sh=Ovel_sh_20keV,$
    erange=erange_20keV,mass_range=mass_range,mass_assmpt=mass_assmpt,rot=rot,q=q,vsh_dir=vsh_dir,vec_norm_sh=vec_norm_sh

  erange_10kev = [10000.,20000.]
  calc_flux_sh,dat_out,$
    Odens=Odens_10keV,Ovel_mso=Ovel_mso_10keV,Ovel_mse=Ovel_mse_10keV,Ovel_sh=Ovel_sh_10keV,$
    erange=erange_10keV,mass_range=mass_range,mass_assmpt=mass_assmpt,rot=rot,q=q,vsh_dir=vsh_dir,vec_norm_sh=vec_norm_sh

  erange_5kev = [5000.,10000.]
  calc_flux_sh,dat_out,$
    Odens=Odens_5keV,Ovel_mso=Ovel_mso_5keV,Ovel_mse=Ovel_mse_5keV,Ovel_sh=Ovel_sh_5keV,$
    erange=erange_5keV,mass_range=mass_range,mass_assmpt=mass_assmpt,rot=rot,q=q,vsh_dir=vsh_dir,vec_norm_sh=vec_norm_sh

  erange_1kev = [1000.,5000.]
  calc_flux_sh,dat_out,$
    Odens=Odens_1keV,Ovel_mso=Ovel_mso_1keV,Ovel_mse=Ovel_mse_1keV,Ovel_sh=Ovel_sh_1keV,$
    erange=erange_1keV,mass_range=mass_range,mass_assmpt=mass_assmpt,rot=rot,q=q,vsh_dir=vsh_dir,vec_norm_sh=vec_norm_sh

  erange_100ev = [100.,1000.]
  calc_flux_sh,dat_out,$
    Odens=Odens_100eV,Ovel_mso=Ovel_mso_100eV,Ovel_mse=Ovel_mse_100eV,Ovel_sh=Ovel_sh_100eV,$
    erange=erange_100eV,mass_range=mass_range,mass_assmpt=mass_assmpt,rot=rot,q=q,vsh_dir=vsh_dir,vec_norm_sh=vec_norm_sh

  erange_gt10kev = [10000.,35000.]
  calc_flux_sh,dat_out,$
    Odens=Odens_gt10keV,Ovel_mso=Ovel_mso_gt10keV,Ovel_mse=Ovel_mse_gt10keV,Ovel_sh=Ovel_sh_gt10keV,$
    erange=erange_gt10keV,mass_range=mass_range,mass_assmpt=mass_assmpt,rot=rot,q=q,vsh_dir=vsh_dir,vec_norm_sh=vec_norm_sh

  erange_gt5kev = [5000.,35000.]
  calc_flux_sh,dat_out,$
    Odens=Odens_gt5keV,Ovel_mso=Ovel_mso_gt5keV,Ovel_mse=Ovel_mse_gt5keV,Ovel_sh=Ovel_sh_gt5keV,$
    erange=erange_gt5keV,mass_range=mass_range,mass_assmpt=mass_assmpt,rot=rot,q=q,vsh_dir=vsh_dir,vec_norm_sh=vec_norm_sh

  erange_gt1keV = [1000.,35000.]
  calc_flux_sh,dat_out,$
    Odens=Odens_gt1keV,Ovel_mso=Ovel_mso_gt1keV,Ovel_mse=Ovel_mse_gt1keV,Ovel_sh=Ovel_sh_gt1keV,$
    erange=erange_gt1keV,mass_range=mass_range,mass_assmpt=mass_assmpt,rot=rot,q=q,vsh_dir=vsh_dir,vec_norm_sh=vec_norm_sh

  erange_all = [100.,35000.]
  calc_flux_sh,dat_out,$
    Odens=Odens_all,Ovel_mso=Ovel_mso_all,Ovel_mse=Ovel_mse_all,Ovel_sh=Ovel_sh_all,$
    erange=erange_all,mass_range=mass_range,mass_assmpt=mass_assmpt,rot=rot,q=q,vsh_dir=vsh_dir,vec_norm_sh=vec_norm_sh

    
    
    xyouts,1.8*vrange[1],0.9*vrange[1],'N_gt10kev: '+string(Odens_gt10kev[0],format='(f12.7)')+'/cm3',/data
    xyouts,1.8*vrange[1],0.8*vrange[1],'N_gt1kev: '+string(Odens_gt1kev[0],format='(f12.7)')+'/cm3',/data
    xyouts,1.8*vrange[1],0.7*vrange[1],'N_100ev: '+string(Odens_100ev[0],format='(f12.7)')+'/cm3',/data
    xyouts,1.8*vrange[1],0.6*vrange[1],'V_gt10kev: '+string(Ovel_mso_gt10kev[0],format='(f7.2)')+','+string(Ovel_mso_gt10kev[1],format='(f7.2)')+','+string(Ovel_mso_gt10kev[2],format='(f7.2)')+' km/s',/data
    xyouts,1.8*vrange[1],0.5*vrange[1],'V_gt1kev: '+string(Ovel_mso_gt1kev[0],format='(f7.2)')+','+string(Ovel_mso_gt1kev[1],format='(f7.2)')+','+string(Ovel_mso_gt1kev[2],format='(f7.2)')+' km/s',/data
    xyouts,1.8*vrange[1],0.4*vrange[1],'V_100ev: '+string(Ovel_mso_100ev[0],format='(f7.2)')+','+string(Ovel_mso_100ev[1],format='(f7.2)')+','+string(Ovel_mso_100ev[2],format='(f7.2)')+' km/s',/data
    xyouts,1.8*vrange[1],0.3*vrange[1],'B: '+string((Bmso[0]^2+Bmso[1]^2+Bmso[2]^2)^.5,format='(f7.2)')+' nT',/data
    wi,0
    !p.charsize=1.
    
   
   print,'PLOT VDF succeeded!!'
;  flux_sav = {time:[dat_out.time, dat_out.end_time], $
;             pos_mse:pos_mse,theta_bs:theta_bs,phi_bs:phi_bs,bin_theta_phi:ind_norm,$
;             Odens_100ev:Odens_100ev, Ovel_mso_100ev:Ovel_mso_100ev, Ovel_mse_100ev:Ovel_mse_100ev, Ovel_sh_100ev:Ovel_sh_100ev,$
;             Odens_1kev:Odens_1kev, Ovel_mso_1kev:Ovel_mso_1kev, Ovel_mse_1kev:Ovel_mse_1kev, Ovel_sh_1kev:Ovel_sh_1kev,$
;             Odens_5kev:Odens_5kev, Ovel_mso_5kev:Ovel_mso_5kev, Ovel_mse_5kev:Ovel_mse_5kev, Ovel_sh_5kev:Ovel_sh_1kev,$
;             Odens_10kev:Odens_10kev, Ovel_mso_10kev:Ovel_mso_10kev, Ovel_mse_10kev:Ovel_mse_10kev, Ovel_sh_10kev:Ovel_sh_10kev,$
;             Odens_20kev:Odens_20kev, Ovel_mso_20kev:Ovel_mso_20kev, Ovel_mse_20kev:Ovel_mse_20kev, Ovel_sh_20kev:Ovel_sh_20kev,$
;             Odens_gt1kev:Odens_gt1kev, Ovel_mso_gt1kev:Ovel_mso_gt1kev, Ovel_mse_gt1kev:Ovel_mse_gt1kev, Ovel_sh_gt1kev:Ovel_sh_gt1kev,$
;             Odens_gt5kev:Odens_gt1kev, Ovel_mso_gt5kev:Ovel_mso_gt5kev, Ovel_mse_gt5kev:Ovel_mse_gt5kev, Ovel_sh_gt5kev:Ovel_sh_gt5kev,$
;             Odens_gt10kev:Odens_gt10kev, Ovel_mso_gt10kev:Ovel_mso_gt10kev,Ovel_mse_gt10kev:Ovel_mse_gt10kev, Ovel_sh_gt10kev:Ovel_sh_gt10kev,$
;             Odens_all:Odens_all, Ovel_mso_all:Ovel_mso_all, Ovel_mse_all:Ovel_mse_all, Ovel_sh_all:Ovel_sh_all}
       
   no_output:


end