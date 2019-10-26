;+
; :Description:
;    This function calculate and returns local field vectors: magnetic field, velocity, and convection electric field
;
; ${parameters}
;  
;  V: a velocity vector in the MSO frame
;  B: a magnetic field vector in the MSO frame
;
; ${keywords}
;  orbit: orbit number (NOTE: it's not official orbit number!)
;  highres: 
;  
; ${Related routines}
;
; :Author: Kei Masunaga (@STEL, Nagoya Univ.)
;
; $Last modified May 20, 2015
;-


function get_local_fields,tspan,orbit=orbit,highres=highres
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