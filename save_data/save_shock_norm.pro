;+
; :Description:
;   This function saves shock normal vectors at Mars.
;   The shock empherical model of Edberg et al. [2008] is used. 
;
; ${parameters}
;
; ${keywords}
;
; ${Related routines}
;  get_norm_shock3.pro
;  
; :Author: Kei Masunaga (@STEL, Nagoya Univ.)
;
; $Last modified Nov 20, 2015
;-

pro save_shock_norm

  env = init_env()
  SAVE_LOC = env.SAVE_LOC
  
  ;;Define variables
  Rm = 3389.9D * 1e3 * 1e2 ;[cm]
  L = 2.1
  e = 1.05
  X0 = 0.55
  thres = 5 * !DTOR  ;;Resolutin of Theta' [Degree]
  phres = 5 * !DTOR  ;;Resolution of Phi' [Degree]

  theta_arr = findgen(163)*!DTOR   ;; Angle from the X axis. Note that in this coordinate system the origin is a focus of the hyperboric curve
  r_arr = L/(1.+e*cos(theta_arr))  ;; Distance to the shock surface from the focus of the hyperboric curve
  X = r_arr * cos(theta_arr) 
  Y = r_arr * sin(theta_arr) 
  phi_dash_bin = ((findgen(72)*5. + 2.5) - 180.)*!DTOR
  phi_dash_bin2 = replicate(1.,32) # phi_dash_bin
  phi_dash_bin_edge = ((findgen(73)*5.) - 180.)*!DTOR
  phi_dash_bin_edge2 = replicate(1.,33) # phi_dash_bin_edge
  theta_dash_bin = (findgen(32)*5 + 2.5)*!DTOR  ;; Angle from the X axis with a even gap of 5 degree. Note that in this coordinate system the origin is a center of Mars
  theta_dash_bin_edge = findgen(33)*5.*!DTOR    ;; Array of edge of the theta' array with a even gap of 5 degree.  
     
  ;;calculate r' (Distance to the shock surface from the center of Mars)
  theta_dash_arr = acos((X0+X)/sqrt((X0+X)^2+Y^2)) ;; Angle from the X axis (data points correspnd to those of theta_arr)
  r_dash = (X0 + r_arr*cos(theta_arr))/cos(theta_dash_arr) ;;r'      ;(X0 + r_arr*cos(theta_arr))/cos(theta_dash_arr)

  ;;r' has to be interpolated by data points of theta_dash_bin because data points of r' are correspoinding to those of theta_dash_arr
  r_dash_bin = interpol(r_dash,theta_dash_arr,theta_dash_bin)
  r_dash_bin_edge = interpol(r_dash,theta_dash_arr,theta_dash_bin_edge)
  rho_dash_bin = r_dash_bin*sin(theta_dash_bin)
  rho_dash_bin2 = rho_dash_bin # replicate(1.,72)
  rho_dash_bin_edge = r_dash_bin_edge*sin(theta_dash_bin_edge)
  rho_dash_bin_edge2 = rho_dash_bin_edge # replicate(1.,73)
  x_dash_bin = r_dash_bin*cos(theta_dash_bin)
  x_dash_bin2 = x_dash_bin # replicate(1.,72)
  x_dash_bin_edge = r_dash_bin_edge*cos(theta_dash_bin_edge)
  x_dash_bin_edge2 = x_dash_bin_edge # replicate(1.,73)
  y_dash_bin2 = rho_dash_bin2 * cos(phi_dash_bin2)
  y_dash_bin_edge2 = rho_dash_bin_edge2 * cos(phi_dash_bin_edge2)
  z_dash_bin2 = rho_dash_bin2 * sin(phi_dash_bin2)
  z_dash_bin_edge2 = rho_dash_bin_edge2 * sin(phi_dash_bin_edge2) 
  
  ;;Define shock normal vectors and distance to the normal vectors from a certain position
  x_norm_cnt = fltarr(32,72)
  y_norm_cnt = fltarr(32,72)
  z_norm_cnt = fltarr(32,72)
  Dst = fltarr(32,72)

  ;;calculate shock normal vectors (For a squre ABCD, the normal vector = AC x BD)
  for i=0,31 do begin
    for j=0,71 do begin
      vec12 = [x_dash_bin_edge2[i+1,j]-x_dash_bin_edge2[i,j], y_dash_bin_edge2[i+1,j]-y_dash_bin_edge2[i,j], z_dash_bin_edge2[i+1,j]-z_dash_bin_edge2[i,j]]
      vec14 = [x_dash_bin_edge2[i,j+1]-x_dash_bin_edge2[i,j], y_dash_bin_edge2[i,j+1]-y_dash_bin_edge2[i,j], z_dash_bin_edge2[i,j+1]-z_dash_bin_edge2[i,j]]  
      vec13 = [x_dash_bin_edge2[i+1,j+1]-x_dash_bin_edge2[i,j], y_dash_bin_edge2[i+1,j+1]-y_dash_bin_edge2[i,j], z_dash_bin_edge2[i+1,j+1]-z_dash_bin_edge2[i,j]]
      vec24 = [x_dash_bin_edge2[i,j+1]-x_dash_bin_edge2[i+1,j], y_dash_bin_edge2[i,j+1]-y_dash_bin_edge2[i+1,j], z_dash_bin_edge2[i,j+1]-z_dash_bin_edge2[i+1,j]]
      if i eq 0 then norm_cnt = crossp(vec12,vec13)/total(crossp(vec12,vec13)^2)^.5   ;;When theta=0, thera are only 3 data points thus no squre exists  
      if i ne 0 then norm_cnt = crossp(vec13,vec24)/total(crossp(vec13,vec24)^2)^.5
      x_norm_cnt[i,j] = norm_cnt[0]
      y_norm_cnt[i,j] = norm_cnt[1]
      z_norm_cnt[i,j] = norm_cnt[2]
;      L0 = [x_dash_bin2[i,j],y_dash_bin2[i,j],z_dash_bin2[i,j]]
;      L1 = L0 + [norm_cnt[0], norm_cnt[0], norm_cnt[0]]
;      Dst[i,j] = pnt_line(pos,L0,L1)      
    endfor    
  endfor
  norm_shock = {x_norm_cnt:x_norm_cnt, y_norm_cnt:y_norm_cnt, z_norm_cnt:z_norm_cnt}
  save, norm_shock, file=SAVE_LOC + '/maven/sav/shock_normal/shock_normal.sav'
 
end