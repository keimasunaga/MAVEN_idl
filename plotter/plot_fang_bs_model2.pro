pro plot_fang_bs_model2

    env = init_env()
    SAVE_LOC = env.SAVE_LOC
    
    filename = SAVE_LOC + '/maven/ascii/model/fang/BS_dayside.dat'
    nline = file_lines(filename)
    sline = ''
    close,1
    openr,1,filename
    bs_pos = fltarr(3,nline-9)
      
    for i=0,8 do readf,1,sline
    readf,1,bs_pos   
    close,1
    !p.multi=[0,3,1]
    plot,bs_pos[0,*],bs_pos[1,*],psym=1,/iso;,xrange=[1.0,1.8],yrange=[-0.4,0.4]
    plot,bs_pos[0,*],bs_pos[2,*],psym=1,/iso;,xrange=[1.0,1.8],yrange=[-0.4,0.4]
    plot,bs_pos[1,*],bs_pos[2,*],psym=1,/iso;,xrange=[-1,1],yrange=[-1,1]
    !p.multi=[0,1,1]
    
    bs_pos_r = sqrt(bs_pos[0,*]^2+bs_pos[1,*]^2+bs_pos[2,*]^2)
    ;theta_arr = acos(bs_pos[0,*]/bs_pos_r)
    ;phi_arr = acos( bs_pos[1,*]/bs_pos_r/sin(theta_arr) )               
    bs_pos_unitx = bs_pos[0,*]/bs_pos_r;total(bs_pos^2,1)^.5
    bs_pos_unity = bs_pos[1,*]/bs_pos_r;total(bs_pos^2,1)^.5
    bs_pos_unitz = bs_pos[2,*]/bs_pos_r;total(bs_pos^2,1)^.5
   
  
    
    theta_arr = acos(bs_pos_unitx)
    j = bs_pos_unity/sin(theta_arr)
    idx_a = where(j gt 1)
    idx_b = where(j lt -1)
    phi_arr = acos( j )
    phi_arr[idx_a] = acos(1.0)
    phi_arr[idx_b] = acos(-1.0)
   
    idx_m = where(bs_pos[2,*] lt 0)
    phi_arr[idx_m] = 2*!pi-phi_arr[idx_m]  
  
    phi_points = findgen(360./5.)*5.
    theta_points = findgen(90./5.) *5.
    
    idx_th_ph = dblarr(90./5)
    stop
    for i=0,360/5.-1 do begin
      print,i
      idx_bounds = where(phi_arr*!radeg ge phi_points[i]-2.5 and phi_arr*!radeg le phi_points[i]+2.5)
      if i eq 0 or i eq 1 then idx_bounds = idx_bounds[0:17]
      idx_th_ph = [[idx_th_ph],[idx_bounds]]
           
    endfor
    
    idx_th_ph = idx_th_ph[*,1:*]
    
    th_0 = theta_arr[idx_th_ph[0,*]]
    phi_0 = phi_arr[idx_th_ph[*,0]]
    

    ;;calculate incident ion velocity
    ;;note: incident angle can be calculated by solving
    ;; Vx = Vperp*(1+cos(theta)), Vz = Vperp*sin(theta)
    ;;tan(Vz/Vx) = tan(45 deg) = 1.
    ;; For incident angle = 30deg, theta = 60 deg
    ;; For incident angle = 45deg, theta = 90 deg

    Vsw = 400.
    agl_parker = 56.*!DTOR
    Vperp_bulk = Vsw * cos(90.*!DTOR-agl_parker)
    theta = 90.*!DTOR   ;;read note above
    Vperp_oxy = ( (Vperp_bulk * (1.+cos(theta)))^2 + (Vperp_bulk * sin(theta))^2 )^.5
    Einc = 0.5*16.*1.67e-27*(Vperp_oxy*1e3)^2/1.6e-19
    vvec_inc = [ -Vperp_bulk*(1.*cos(theta))*sin(agl_parker) , -Vperp_bulk*(1.+cos(theta))*cos(agl_parker), Vperp_bulk*sin(theta) ]
    vvec_inc_tot = total(vvec_inc^2)^.5
    vvec_inc_unit = vvec_inc/total(vvec_inc^2)^.5
    
    
    vvec_arr = fltarr(3)   
    shock_norm = fltarr(3)
    bs_pos2 = fltarr(3)
    
    for it=0,17-1 do begin
       for ip=0,71-1 do begin
    
    
    
    ;;initial velocity is shock normal direction
    theta1 = theta_arr[idx_th_ph[it,ip]];40d*!DTOR
    theta2 = theta_arr[idx_th_ph[it+1,ip]];50d*!DTOR
    theta3 = theta_arr[idx_th_ph[it+1,ip+1]]
    theta4 = theta_arr[idx_th_ph[it,ip+1]]
    phi1 = phi_arr[idx_th_ph[it,ip]];40d*!DTOR
    phi2 = phi_arr[idx_th_ph[it+1,ip]];50d *!DTOR
    phi3 = phi_arr[idx_th_ph[it+1,ip+1]];40d*!DTOR
    phi4 = phi_arr[idx_th_ph[it,ip+1]];50d *!DTOR
    r1 = total(bs_pos[*,idx_th_ph[it,ip]]^2)^.5
    r2 = total(bs_pos[*,idx_th_ph[it+1,ip]]^2)^.5
    r3 = total(bs_pos[*,idx_th_ph[it+1,ip+1]]^2)^.5
    r4 = total(bs_pos[*,idx_th_ph[it+1,ip]]^2)^.5
    r = (r1+r2+r3+r4)/4.
    
    ;print,theta1*!radeg,theta2*!radeg,phi1*!radeg,phi2*!radeg
    x1 = r1*cos(theta1)
    y1 = r1*sin(theta1)*cos(phi1)
    z1 = r2*sin(theta1)*sin(phi1)
    x2 = r2*cos(theta2)
    y2 = r2*sin(theta2)*cos(phi2)
    z2 = r2*sin(theta2)*sin(phi2)
    x3 = r3*cos(theta3)
    y3 = r3*sin(theta3)*cos(phi3)
    z3 = r3*sin(theta3)*sin(phi3)
    x4 = r4*cos(theta4)
    y4 = r4*sin(theta4)*cos(phi4)
    z4 = r4*sin(theta4)*sin(phi4)

    Xg = (x1+x2+x3+x4)/4.
    Yg = (y1+y2+y3+y4)/4.    
    Zg = (z1+z2+z3+z4)/4.
    
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
    
    vec_norm = crossp(vec_12,vec_14)
    vec_norm2 = vec_norm/total(vec_norm^2)^.5
    shock_norm = [ [shock_norm],[vec_norm2] ]
    bs_pos2 = [[bs_pos2], [xg,yg,zg]]
    
    if zg ge 0 then vvec_arr = [[vvec_arr],[vvec_inc_unit[0],vvec_inc_unit[1],-vvec_inc_unit[2]]]
    if zg lt 0 then vvec_arr = [[vvec_arr],[vvec_inc_unit[0],vvec_inc_unit[1],vvec_inc_unit[2]]]
    
    
    endfor
    endfor
    bs_pos2 = bs_pos2[*,1:*]
    vvec_arr = vvec_arr[*,1:*]
    shock_norm = shock_norm[*,1:*]
    sz = size(shock_norm)
    E0 = replicate(Einc,sz[2])
    
    
    sav = [[transpose(bs_pos2[0,*])],[transpose(bs_pos2[1,*])],[transpose(bs_pos2[2,*])],[E0],[transpose(vvec_arr[0,*])],[transpose(vvec_arr[1,*])],[transpose(vvec_arr[2,*])]]
stop   
    filename = SAVE_LOC + '/maven/ascii/vdfparam/incident_ions_30deg'+'.dat'
    format = '7(f17.10)'
    write_ascii,sav,filename,format
    
    
    stop



end