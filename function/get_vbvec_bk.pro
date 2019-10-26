function get_vbvec_bk,trange,solar_wind=solar_wind,local=local,mva=mva

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
      return,0
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

  return,vb
end
