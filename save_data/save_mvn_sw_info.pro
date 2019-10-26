;+
; :Description:
;    This routine saves time of the bow shock crossings and
;    correspoinding solar wind parameters averaged for 15 minutes from the shock boundaries
;
; ${parameters}
;
; ${keywords}
; orbit: orbit number
; apend_tvar: set when plotting two continuous orbits
; swi_flg_manual: set when manually difine swia flgs. Swia flgs are values to judge if both swica and swics data or either of them exists
;
; ${Return values}
;
; ${Related routines}
; main_save_mvn_sw_info
;
; $Author: Kei Masunaga (@EPS, Univ. Tokyo)
;
; $Last modified May 15, 2016
;-

pro save_mvn_sw_info,orbit=orbit,append_tvar=append_tvar,swi_flg_manual=swi_flg_manual


   env = init_env()
   SAVE_LOC = env.SAVE_LOC
   ;restore,SAVE_LOC + ; restore bowshock crossing time
   restore,SAVE_LOC + '/maven/sav/swia_flg/orbit/swia_flg_' + string(orbit,format='(i05)') + '.sav'
   flg_ca = swiflg.flg_ca & flg_cs = swiflg.flg_cs
   if n_elements(flg_ca) eq 2 then begin
    if flg_ca[0] eq 1 and flg_ca[1] eq 1 then swi_flg = 'swica' else swi_flg = 'swics'
   endif else begin
    if flg_ca eq 1 then swi_flg = 'swica' else swi_flg = 'swics'
   endelse
   if keyword_set(swi_flg_manual) then swi_flg = swi_flg_manual
   
   if not keyword_set(append_tvar) then begin
   
     if not keyword_set(time_avg) then time_avg  = 15d
     tspan_avg = time_avg*60d ;15 minutes
     get_data,'mvn_'+swi_flg+'_velocity_mso',data=vel
     get_data,'mvn_'+swi_flg+'_density',data=dens
     get_data,'mvn_'+swi_flg+'_temperature',data=temp
     get_data,'mvn_'+swi_flg+'_pressure',data=pres
     get_data,'mvn_'+swi_flg+'_pdy',data=pdy         
     get_data,'mvn_B_1sec_MAVEN_MSO',data=mag
 ;    get_data,'mvn_B_1sec',data=mag_sc
     get_data,'orbnum',data=orbnum
   
     ;; get time of inbound and outbound crossings and set indice for each parameter at the time
     ctime,t
     
     idx_orb = nn('orbnum',t)
     trange_in = [t[0]-tspan_avg, t[0]]
     trange_out = [t[1], t[1]+tspan_avg]
     idx_vel_in = nn('mvn_'+swi_flg+'_velocity_mso',trange_in) & idx_vel_out = nn('mvn_'+swi_flg+'_velocity_mso',trange_out)
     idx_dens_in = nn('mvn_'+swi_flg+'_density',trange_in) & idx_dens_out = nn('mvn_'+swi_flg+'_density',trange_out)
     idx_temp_in = nn('mvn_'+swi_flg+'_temperature',trange_in) & idx_temp_out = nn('mvn_'+swi_flg+'_temperature',trange_out)
     idx_pres_in = nn('mvn_'+swi_flg+'_pressure',trange_in) & idx_press_out = nn('mvn_'+swi_flg+'_pressure',trange_out)
     idx_pdy_in = nn('mvn_'+swi_flg+'_pdy',trange_in) & idx_pdy_out = nn('mvn_'+swi_flg+'_pdy',trange_out)
     idx_mag_in = nn('mvn_B_1sec_MAVEN_MSO',trange_in) & idx_mag_out = nn('mvn_B_1sec_MAVEN_MSO',trange_out)
     ;idx_mag_sc_in = nn('mvn_B_1sec',trange_in) & idx_mag_sc_out = nn('mvn_B_1sec',trange_out)
     ;orbnum_s = string(fix(avg(orbnum.y[idx_orb])),format='(i05)')
     orbnum_s = string(orbit,format='(i05)')
     
    dt_v_in = vel.x[idx_vel_in[1]]- vel.x[idx_vel_in[0]] 
    dt_v_out = vel.x[idx_vel_out[1]]- vel.x[idx_vel_out[0]] 
    dt_b_in = mag.x[idx_mag_in[1]]- mag.x[idx_mag_in[0]] 
    dt_b_out = mag.x[idx_mag_out[1]]- mag.x[idx_mag_out[0]]
    
    ;calculate average of parameters for inbound and outbound
    if swi_flg eq 'swica' then begin
     if dt_v_in ge time_avg * 60d -20d then vel_in = avg(vel.y[idx_vel_in[0]:idx_vel_in[1],*],0) else vel_in = [!values.F_NaN,!values.F_NaN,!values.F_NaN]
     if dt_v_out ge time_avg * 60d -20d then vel_out = avg(vel.y[idx_vel_out[0]:idx_vel_out[1],*],0) else vel_out = [!values.F_NaN,!values.F_NaN,!values.F_NaN]
     if dt_v_in ge time_avg * 60d -20d then dens_in = avg(dens.y[idx_dens_in[0]:idx_dens_in[1]]) else dens_in = !values.F_NaN 
     if dt_v_out ge time_avg * 60d -20d then dens_out = avg(dens.y[idx_dens_out[0]:idx_dens_out[1]]) else dens_out = !values.F_NaN 
     if dt_v_in ge time_avg * 60d -20d then temp_in = avg(temp.y[idx_temp_in[0]:idx_temp_in[1],*],0) else temp_in = [!values.F_NaN,!values.F_NaN,!values.F_NaN,!values.F_NaN]
     if dt_v_out ge time_avg * 60d -20d then temp_out = avg(temp.y[idx_vel_out[0]:idx_vel_out[1],*],0) else temp_out = [!values.F_NaN,!values.F_NaN,!values.F_NaN,!values.F_NaN]
     if dt_v_in ge time_avg * 60d -20d then pres_in = avg(pres.y[idx_pres_in[0]:idx_pres_in[1],*],0) else pres_in = [!values.F_NaN,!values.F_NaN,!values.F_NaN,!values.F_NaN,!values.F_NaN,!values.F_NaN]
     if dt_v_out ge time_avg * 60d -20d then pres_out = avg(pres.y[idx_vel_out[0]:idx_vel_out[1],*],0) else pres_out = [!values.F_NaN,!values.F_NaN,!values.F_NaN,!values.F_NaN,!values.F_NaN,!values.F_NaN]
     if dt_v_in ge time_avg * 60d -20d then pdy_in = avg(pdy.y[idx_pdy_in[0]:idx_pdy_in[1],*],0) else pdy_in = !values.F_NaN
     if dt_v_in ge time_avg * 60d -20d then pdy_out = avg(pdy.y[idx_vel_out[0]:idx_vel_out[1],*],0) else pdy_out = !values.F_NaN
    endif else begin
      if dt_v_in ge time_avg * 60d -60d then vel_in = avg(vel.y[idx_vel_in[0]:idx_vel_in[1],*],0) else vel_in = [!values.F_NaN,!values.F_NaN,!values.F_NaN]
      if dt_v_out ge time_avg * 60d -60d then vel_out = avg(vel.y[idx_vel_out[0]:idx_vel_out[1],*],0) else vel_out = [!values.F_NaN,!values.F_NaN,!values.F_NaN]
      if dt_v_in ge time_avg * 60d -60d then dens_in = avg(dens.y[idx_dens_in[0]:idx_dens_in[1]]) else dens_in = !values.F_NaN
      if dt_v_out ge time_avg * 60d -60d then dens_out = avg(dens.y[idx_dens_out[0]:idx_dens_out[1]]) else dens_out = !values.F_NaN
      if dt_v_in ge time_avg * 60d -60d then temp_in = avg(temp.y[idx_temp_in[0]:idx_temp_in[1],*],0) else temp_in = [!values.F_NaN,!values.F_NaN,!values.F_NaN,!values.F_NaN]
      if dt_v_out ge time_avg * 60d -60d then temp_out = avg(temp.y[idx_vel_out[0]:idx_vel_out[1],*],0) else temp_out = [!values.F_NaN,!values.F_NaN,!values.F_NaN,!values.F_NaN]
      if dt_v_in ge time_avg * 60d -60d then pres_in = avg(pres.y[idx_pres_in[0]:idx_pres_in[1],*],0) else pres_in = [!values.F_NaN,!values.F_NaN,!values.F_NaN,!values.F_NaN,!values.F_NaN,!values.F_NaN]
      if dt_v_out ge time_avg * 60d -60d then pres_out = avg(pres.y[idx_vel_out[0]:idx_vel_out[1],*],0) else pres_out = [!values.F_NaN,!values.F_NaN,!values.F_NaN,!values.F_NaN,!values.F_NaN,!values.F_NaN]
      if dt_v_in ge time_avg * 60d -60d then pdy_in = avg(pdy.y[idx_pdy_in[0]:idx_pdy_in[1],*],0) else pdy_in = !values.F_NaN
      if dt_v_in ge time_avg * 60d -60d then pdy_out = avg(pdy.y[idx_vel_out[0]:idx_vel_out[1],*],0) else pdy_out = !values.F_NaN   
    endelse
    
     if dt_b_in ne 0 then mag_in = avg(mag.y[idx_mag_in[0]:idx_mag_in[1],*],0,/NaN) else mag_in = !values.F_NaN
     if dt_b_out ne 0 then mag_out = avg(mag.y[idx_mag_out[0]:idx_mag_out[1],*],0,/NaN) else mag_out = !values.F_NaN
    ; mag_sc_in = avg(mag.y[idx_mag_sc_in[0]:idx_mag_in[1],*],0) & mag_sc_out = avg(mag.y[idx_mag_sc_out[0]:idx_mag_sc_out[1],*],0)
    
     ;;calculate clock angle
     if mag_in[0] ne !values.F_NaN then calc_mag_angles,mag.y[idx_mag_in[0]:idx_mag_in[1],*],angle=angles_in else angles_in = !values.F_NaN
     if mag_out[0] ne !values.F_NaN then calc_mag_angles,mag.y[idx_mag_out[0]:idx_mag_out[1],*],angle=angles_out else angles_out = !values.F_NaN
     print,'vel_in:',vel_in,'vel_out',vel_out
     print,'mag_in:',mag_in,'mag_out',mag_out
     
     
    
     dat_sw = {Time:t, $
               VEL:[ [vel_in],[vel_out] ],$
               DENS:[ [dens_in], [dens_out] ],$
               TEMP:[ [temp_in], [temp_out] ],$
               PRES:[ [pres_in], [pres_out] ],$
               PDY:[ [pdy_in], [pdy_out] ],$
               MAG_MSO:[ [mag_in],[mag_out] ],$
             
               ANGLES_in:angles_in,$
               ANGLES_out:angles_out}

     save,dat_sw,file=SAVE_LOC+'/maven/sav/solar_wind_info/'+string(time_avg,format='(i02)')+'min/sw_info_'+orbnum_s+'.sav'

   endif
   
     if keyword_set(append_tvar) then begin
   
     if not keyword_set(time_avg) then time_avg  =15d
     tspan_avg = time_avg*60d ;15 minutes
     get_data,'mvn_'+swi_flg+'_velocity_mso_1',data=vel1 & get_data,'mvn_'+swi_flg+'_velocity_mso_2',data=vel2
     get_data,'mvn_'+swi_flg+'_density_1',data=dens1 & get_data,'mvn_'+swi_flg+'_density_1',data=dens2
     get_data,'mvn_'+swi_flg+'_temperature_mso_1',data=temp1 & get_data,'mvn_'+swi_flg+'_temperature_mso_2',data=temp2
     get_data,'mvn_'+swi_flg+'_pressure_1',data=pres1 & get_data,'mvn_'+swi_flg+'_pressure_2',data=pres2
     get_data,'mvn_'+swi_flg+'_pdy_1',data=pdy1 & get_data,'mvn_'+swi_flg+'_pdy_2',data=pdy2 
     get_data,'mvn_B_1sec_MAVEN_MSO_1',data=mag1 & get_data,'mvn_B_1sec_MAVEN_MSO_2',data=mag2
 ;    get_data,'mvn_B_1sec_1',data=mag_sc1 &  get_data,'mvn_B_1sec_2',data=mag_sc
     get_data,'orbnum_',data=orbnum
     
     store_data,'mvn_'+swi_flg+'_velocity_mso',data={x:[vel1.x,vel2.x],y:[vel1.y,vel2.y]}
     store_data,'mvn_'+swi_flg+'_density',data={x:[dens1.x,dens2.x],y:[dens1.y,dens2.y]}
     store_data,'mvn_'+swi_flg+'_temperature_mso',data={x:[temp1.x,temp2.x],y:[temp1.y,temp2.y]}
     store_data,'mvn_'+swi_flg+'_pressure',data={x:[pres1.x,pres2.x],y:[pres1.y,pres2.y]}
     store_data,'mvn_'+swi_flg+'_pdy',data={x:[pdy1.x,pdy2.x],y:[pdy1.y,pdy2.y]}
     store_data,'mvn_B_1sec_MAVEN_MSO',data={x:[mag1.x,mag2.x],y:[mag1.y,mag2.y]}
     ;store_data,'orbnum',data={x:[orbnum1.x,orbnum2.x],y:[orbnum1.y.y,orbnum2.y]}    

    ; append_tvar, ['mvn_swim_velocity_mso', 'mvn_swim_density','mvn_swim_temperature_mso','mvn_swim_pressure','mvn_swim_pdy','mvn_B_1sec_MAVEN_MSO','orbnum']

     ;; get time of inbound and outbound crossings and set indice for each parameter at the time
     ctime,t
     idx_orb = nn('orbnum_',t)
     trange_in = [t[0]-tspan_avg, t[0]]
     trange_out = [t[1], t[1]+tspan_avg]
     idx_vel_in = nn('mvn_'+swi_flg+'_velocity_mso',trange_in) & idx_vel_out = nn('mvn_'+swi_flg+'_velocity_mso',trange_out)
     idx_dens_in = nn('mvn_'+swi_flg+'_density',trange_in) & idx_dens_out = nn('mvn_'+swi_flg+'_density',trange_out)
     idx_temp_in = nn('mvn_'+swi_flg+'_temperature_mso',trange_in) & idx_temp_out = nn('mvn_'+swi_flg+'_temperature_mso',trange_out)
     idx_pres_in = nn('mvn_'+swi_flg+'_pressure',trange_in) & idx_press_out = nn('mvn_'+swi_flg+'_pressure',trange_out)
     idx_pdy_in = nn('mvn_'+swi_flg+'_pdy',trange_in) & idx_pdy_out = nn('mvn_'+swi_flg+'_pdy',trange_out)
     idx_mag_in = nn('mvn_B_1sec_MAVEN_MSO',trange_in) & idx_mag_out = nn('mvn_B_1sec_MAVEN_MSO',trange_out)
     ;idx_mag_sc_in = nn('mvn_B_1sec',trange_in) & idx_mag_sc_out = nn('mvn_B_1sec',trange_out)    
     ;orbnum_s = string(fix(round(avg(orbnum.y[idx_orb]))),format='(i05)')
     orbnum_s = string(orbit,format='(i05)')
     
     get_data,'mvn_'+swi_flg+'_velocity_mso',data=vel
     get_data,'mvn_'+swi_flg+'_density',data=dens
     get_data,'mvn_'+swi_flg+'_temperature_mso',data=temp
     get_data,'mvn_'+swi_flg+'_pressure',data=pres
     get_data,'mvn_'+swi_flg+'_pdy',data=pdy
     get_data,'mvn_B_1sec_MAVEN_MSO',data=mag
     ;    get_data,'mvn_B_1sec',data=mag_sc
   
   
     dt_v_in = vel.x[idx_vel_in[1]]- vel.x[idx_vel_in[0]] > time_avg * 60d -5d
     dt_v_out = vel.x[idx_vel_out[1]]- vel.x[idx_vel_out[0]] > time_avg * 60d -5d
     dt_b_in = mag.x[idx_mag_in[1]]- mag.x[idx_mag_in[0]] > time_avg * 60d -5d
     dt_b_out = mag.x[idx_mag_out[1]]- mag.x[idx_mag_out[0]] > time_avg * 60d -5d
   
   
     ;calculate average of parameters for inbound and outbound
;     vel_in = avg(vel.y[idx_vel_in[0]:idx_vel_in[1],*],0) & vel_out = avg(vel.y[idx_vel_out[0]:idx_vel_out[1],*],0)
;     dens_in = avg(dens.y[idx_dens_in[0]:idx_dens_in[1]]) & dens_out = avg(dens.y[idx_dens_out[0]:idx_dens_out[1]])
;     temp_in = avg(temp.y[idx_temp_in[0]:idx_temp_in[1],*],0) & temp_out = avg(temp.y[idx_vel_out[0]:idx_vel_out[1],*],0)
;     pres_in = avg(pres.y[idx_pres_in[0]:idx_pres_in[1],*],0) & pres_out = avg(pres.y[idx_vel_out[0]:idx_vel_out[1],*],0)
;     pdy_in = avg(pdy.y[idx_pdy_in[0]:idx_pdy_in[1],*],0) & pdy_out = avg(pdy.y[idx_vel_out[0]:idx_vel_out[1],*],0)
;     mag_in = avg(mag.y[idx_mag_in[0]:idx_mag_in[1],*],0) & mag_out = avg(mag.y[idx_mag_out[0]:idx_mag_out[1],*],0)
;     ; mag_sc_in = avg(mag.y[idx_mag_sc_in[0]:idx_mag_in[1],*],0) & mag_sc_out = avg(mag.y[idx_mag_sc_out[0]:idx_mag_sc_out[1],*],0)
     if dt_v_in ge time_avg * 60d -20d then vel_in = avg(vel.y[idx_vel_in[0]:idx_vel_in[1],*],0) else vel_in = [!values.F_NaN,!values.F_NaN,!values.F_NaN]
     if dt_v_out ge time_avg * 60d -20d then vel_out = avg(vel.y[idx_vel_out[0]:idx_vel_out[1],*],0) else vel_out = [!values.F_NaN,!values.F_NaN,!values.F_NaN]
     if dt_v_in ge time_avg * 60d -20d then dens_in = avg(dens.y[idx_dens_in[0]:idx_dens_in[1]]) else dens_in = !values.F_NaN 
     if dt_v_out ge time_avg * 60d -20d then dens_out = avg(dens.y[idx_dens_out[0]:idx_dens_out[1]]) else dens_out = !values.F_NaN 
     if dt_v_in ge time_avg * 60d -20d then temp_in = avg(temp.y[idx_temp_in[0]:idx_temp_in[1],*],0) else temp_in = [!values.F_NaN,!values.F_NaN,!values.F_NaN]
     if dt_v_out ge time_avg * 60d -20d then temp_out = avg(temp.y[idx_vel_out[0]:idx_vel_out[1],*],0) else temp_out = [!values.F_NaN,!values.F_NaN,!values.F_NaN]
     if dt_v_in ge time_avg * 60d -20d then pres_in = avg(pres.y[idx_pres_in[0]:idx_pres_in[1],*],0) else pres_in = !values.F_NaN
     if dt_v_out ge time_avg * 60d -20d then pres_out = avg(pres.y[idx_vel_out[0]:idx_vel_out[1],*],0) else pres_out = [!values.F_NaN,!values.F_NaN,!values.F_NaN,!values.F_NaN,!values.F_NaN,!values.F_NaN]
     if dt_v_in ge time_avg * 60d -20d then pdy_in = avg(pdy.y[idx_pdy_in[0]:idx_pdy_in[1],*],0) else pdy_in = !values.F_NaN
     if dt_v_in ge time_avg * 60d -20d then pdy_out = avg(pdy.y[idx_vel_out[0]:idx_vel_out[1],*],0) else pdy_out = !values.F_NaN
     if dt_b_in ne 0 then mag_in = avg(mag.y[idx_mag_in[0]:idx_mag_in[1],*],0) else mag_in = !values.F_NaN
     if dt_b_out ne 0 then mag_out = avg(mag.y[idx_mag_out[0]:idx_mag_out[1],*],0) else mag_out = !values.F_NaN
     
     ;;calculate clock angle
;     calc_mag_angles,mag.y[idx_mag_in[0]:idx_mag_in[1],*],angle=angles_in
;     calc_mag_angles,mag.y[idx_mag_out[0]:idx_mag_out[1],*],angle=angles_out
     if mag_in[0] ne !values.F_NaN then calc_mag_angles,mag.y[idx_mag_in[0]:idx_mag_in[1],*],angle=angles_in else angles_in = !values.F_NaN
     if mag_out[0] ne !values.F_NaN then calc_mag_angles,mag.y[idx_mag_out[0]:idx_mag_out[1],*],angle=angles_out else angles_out = !values.F_NaN


     dat_sw = {Time:t, $
       VEL:[ [vel_in],[vel_out] ],$
       DENS:[ [dens_in], [dens_out] ],$
       TEMP:[ [temp_in], [temp_out] ],$
       PRES:[ [pres_in], [pres_out] ],$
       PDY:[ [pdy_in], [pdy_out] ],$
       MAG_MSO:[ [mag_in],[mag_out] ],$

       ANGLES_in:angles_in,$
       ANGLES_out:angles_out}   
     
     save,dat_sw,file=SAVE_LOC+'/maven/sav/solar_wind_info/'+string(time_avg,format='(i02)')+'min/sw_info_'+orbnum_s+'.sav'

     
   endif

end