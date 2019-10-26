function get_norm_shock,pos_mse

  ;;Define variables
  Rm = 3389.9D * 1e3 * 1e2 ;[cm]
  L = 2.1
  e = 1.05
  X0 = 0.55
  theta_arr = findgen(103)*!DTOR   ;; Angle from the X axis. Note that in this coordinate system the origin is a focus of the hyperboric curve
  r_arr = L/(1.+e*cos(theta_arr))  ;; Distance to the shock surface from the focus of the hyperboric curve
  X = r_arr * cos(theta_arr) ;L*cos(theta_arr)/(1.+e*cos(theta_arr))
  Y = r_arr * sin(theta_arr) ;L*sin(theta_arr)/(1.+e*cos(theta_arr))
  rth_bin = fltarr(18) & rph_bin = fltarr(18)
  phi_dash_bin = ((findgen(72)*5. + 2.5) - 180.)*!DTOR
  phi_dash_bin_edge = ((findgen(73)*5.) - 180.)*!DTOR
  theta_dash_bin = (findgen(18)*5 + 2.5)*!DTOR  ;; Angle from the X axis with a even gap of 5 degree. Note that in this coordinate system the origin is a center of Mars
  theta_dash_bin_edge = findgen(19)*5.*!DTOR    ;; Array of edge of the theta' array with a even gap of 5 degree.  
  thres = 5 * !DTOR  ;;Resolutin of Theta' [Degree]
  phres = 5 * !DTOR  ;;Resolution of Phi' [Degree]
  nr = 10. ;; scale number of the resolution bin of the r direction
  
  
   
  ;;calculate r' (Distance to the shock surface from the center of Mars)
  theta_dash_arr = acos((X0+X)/sqrt((X0+X)^2+Y^2)) ;; Angle from the X axis (data points correspnd to those of theta_arr)
  r_dash = (X0 + r_arr*cos(theta_arr))/cos(theta_dash_arr) ;;r'      ;(X0 + r_arr*cos(theta_arr))/cos(theta_dash_arr)

  ;;r' has to be interpolated by data points of theta_dash_bin because data points of r' are correspoinding to those of theta_dash_arr
  r_dash_bin = interpol(r_dash,theta_dash_arr,theta_dash_bin)
  r_dash_bin_edge = interpol(r_dash,theta_dash_arr,theta_dash_bin_edge)
  rres = r_dash_bin / nr
  rres_edge = r_dash_bin_edge / nr
  
  ;for i=0,n_elements(r_dash_bin)-1 do rth_bin[i] = sqrt(r_dash_bin_edge[i]^2 + r_dash_bin_edge[i+1]^2 - 2.*r_dash_bin_edge[i]*r_dash_bin_edge[i+1]*cos(5.*!DTOR))
  ;for i=0,n_elements(r_dash_bin)-1 do rph_bin[i] = r_dash_bin_edge[i]*sin(theta_dash_bin[i])*(5.*!DTOR)


  ;rres2 = rres * r_dash_bin
  nth = !pi/2./thres
  nph = 2.*!pi/thres
  f_rtp = fltarr(2*nr,nth,nph) & f_tp = fltarr(nth,nph) & f_tp_avg = fltarr(nth,nph)
  n_rtp = fltarr(2*nr,nth,nph) & n_tp = fltarr(nth,nph)
  

  pos_r = total(pos_mse^2)^.5
  pos_th = acos(pos_mse[0]/pos_r)
  if pos_mse[2] ge 0 then pos_ph = acos(pos_mse[1]/pos_r/sin(pos_th)) $
  else pos_ph = -acos(pos_mse[1]/pos_r/sin(pos_th))


  if pos_th*!radeg gt 90 then goto,nightside
  
  ith = fix(pos_th/thres)
  iph = fix(pos_ph/phres) + 35.5
  ir = fix(pos_r/rres[ith])

  th_m = theta_dash_bin_edge[ith] 
  th_p = theta_dash_bin_edge[ith+1] 
  ph_m = phi_dash_bin[iph] 
  ph_p = phi_dash_bin[iph+1]
  r_m = r_dash_bin_edge[ith] * ir/nr
  r_p = r_dash_bin_edge[ith+1] * ir/nr

  ;;calculate 4 points sorrounding a center of the bin 
  x1 = r_m * cos(th_m)
  y1 = r_m * sin(th_m) * cos(ph_m)
  z1 = r_m * sin(th_m) * sin(ph_m)

  x2 = r_p * cos(th_p)
  y2 = r_p * sin(th_p) * cos(ph_m)
  z2 = r_p * sin(th_p) * sin(ph_m)

  x3 = r_p * cos(th_p)
  y3 = r_p * sin(th_p) * cos(ph_p)
  z3 = r_p * sin(th_p) * sin(ph_p)

  x4 = r_m * cos(th_m)
  y4 = r_m * sin(th_m) * cos(ph_p)
  z4 = r_m * sin(th_m) * sin(ph_p)

  ;; calculate a center of the bin 
  Xg = (x1+x2+x3+x4)/4.
  Yg = (y1+y2+y3+y4)/4.
  Zg = (z1+z2+z3+z4)/4.

  ;;Define variables to calculate normal vector of the bin
  a1 = X1-Xg
  b1 = Y1-Yg
  c1 = Z1-Zg
  a2 = X2-Xg
  b2 = Y2-Yg
  c2 = Z2-Zg
  a3 = X3-Xg
  b3 = Y3-Yg
  c3 = Z3-Zg
  a4 = X4-Xg
  b4 = Y4-Yg
  c4 = Z4-Zg
  vec_12 = [a2-a1, b2-b1, c2-c1]
  vec_14 = [a4-a1, b4-b1, c4-c1]
  vec_13 = [a3-a1, b3-b1, c3-c1]

  ;;calculate normal a vector. Normal vector of a square ABCD can be calculated by crossp(AB,AD)
  vec_sh_norm = crossp(vec_12,vec_14)
  vec_sh_norm2 = vec_sh_norm/total(vec_sh_norm^2)^.5
  
  if x1 eq x4 and y1 eq y4 and z1 eq z4 then begin
    vec_sh_norm = crossp(vec_12,vec_13)
    vec_sh_norm2 = vec_sh_norm/total(vec_sh_norm^2)^.5
  endif
  return,vec_sh_norm2

  nightside:
  return,0

end