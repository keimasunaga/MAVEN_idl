;+
; :Description:
;    This function returns local proton density, proton velocity and magnetic field, their standard deviation and ztest
;
; ${parameters}
;  trange: time range in unix time [st, et]
;
; ${keywords}
;  structure containing local proton density, proton velocity and magnetic field, their standard deviation and ztest
;  nflg, vflg, and bflg indicate whether they have enough data points to average
;  
; ${Return values}
;
; ${Related routines}
;
; $Author: Kei Masunaga (@EPS, Univ. Tokyo)
;
; $Last modified July 16, 2016

function get_swimag_nvb,trange

  env = init_env()
  SAVE_LOC = env.SAVE_LOC
  mtime = (trange[0]+trange[1])/2d
  orbit = mvn_orbit_num(time=mtime)
  
  fn_swi = SAVE_LOC + '/maven/sav/swia_flg/orbit/swia_flg_'+string(round(orbit),format='(i05)')+'.sav'
  restore,fn_swi
  flg_ca = swiflg.flg_ca
  flg_cs = swiflg.flg_cs
  if n_elements(flg_ca) eq 1 then if flg_ca eq 1 then swiname = 'swica' else swiname = 'swics'
  if n_elements(flg_ca) eq 2 then if flg_ca[0] eq 1 and flg_ca[1] eq 1 then swiname = 'swica' else swiname = 'swics'

  ;; get local velocity vector and magnetic field vector and calculate local convection electric field
    idx_dens = nn('mvn_'+swiname+'_density',[trange[0],trange[1]])
    get_data,'mvn_'+swiname+'_density',t_dens,y_dens
    t_idx_dens = t_dens[idx_dens]
    if t_idx_dens[1] - t_idx_dens[0] lt (trange[1]-trange[0])/2d then nflg = 0 else nflg = 1
    dens = mean(y_dens[idx_dens[0]:idx_dens[1],*])
    dens_std = stddev(y_dens[idx_dens[0]:idx_dens[1]])

    idx_vel = nn('mvn_'+swiname+'_velocity_mso',[trange[0],trange[1]])
    get_data,'mvn_'+swiname+'_velocity_mso',t_vel,y_vel    
    t_idx_vel = t_vel[idx_vel] 
    if t_idx_vel[1] - t_idx_vel[0] lt (trange[1]-trange[0])/2d then vflg = 0 else vflg = 1  
    Vmso = y_vel[idx_vel[0]:idx_vel[1],*]
    Vmso_avg = mean(y_vel[idx_vel[0]:idx_vel[1],*],dimension=1)
    Vmso_tot = sqrt(Vmso_avg[0]^2+Vmso_avg[1]^2+Vmso_avg[2]^2)
    Vmso_e = Vmso_avg/Vmso_tot
    Vmso_std = stddev(y_vel[idx_vel[0]:idx_vel[1],*],dimension=1)
    vec_theta_phi,Vmso,th_v,th_v_std,ph_v,r_v,z_v

    idx_mag = nn('mvn_B_1sec_MAVEN_MSO',[trange[0],trange[1]])
    get_data,'mvn_B_1sec_MAVEN_MSO',t_mag,y_mag
    t_idx_mag = t_mag[idx_mag]
    if t_idx_mag[1] - t_idx_mag[0] lt (trange[1]-trange[0])/2d then bflg = 0 else bflg = 1  
    Bmso = y_mag[idx_mag[0]:idx_mag[1],*]
    Bmso_avg = mean(y_mag[idx_mag[0]:idx_mag[1],*],dimension=1)
    Bmso_std = stddev(y_mag[idx_mag[0]:idx_mag[1],*],dimension=1)
    Bmso_e = Bmso_avg/sqrt(Bmso_avg[0]^2+Bmso_avg[1]^2+Bmso_avg[2]^2)
    vproton_para_tot = inpro(Vmso_avg,Bmso_e)
    vproton_perp_tot = sqrt(Vmso_tot^2 - vproton_para_tot^2)
    vec_theta_phi,Bmso,th_b,th_b_std,ph_b,r_b,z_b   

    nvb = {Dens:dens,Dens_std:Dens_std, Vmso:Vmso_avg, Vmso_std:Vmso_std, Bmso:Bmso_avg, Bmso_std:Bmso_std,vperp:vproton_perp_tot, vpara:vproton_para_tot,$
          theta_b:th_b, theta_b_std:th_b_std, phi_b:ph_b, r_b:r_b, ztest_b:z_b,$
          theta_v:th_v, theta_v_std:th_v_std, phi_v:ph_v, r_v:r_v, ztest_v:z_v, $
          nflg:nflg, vflg:vflg, bflg:bflg}

   return,nvb
end
