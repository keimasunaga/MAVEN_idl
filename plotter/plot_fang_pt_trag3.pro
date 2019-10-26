pro plot_fang_pt_trag3

    env = init_env()
    SAVE_LOC = env.SAVE_LOC
    
    filename = SAVE_LOC + '/maven/ascii/model/fang/BS_dayside.dat'
    nline = file_lines(filename)
    sline = ''
    close,1
    openr,1,filename
    bs_pos_day = fltarr(3,nline-9)
  
       
    for i=0,8 do readf,1,sline
    readf,1,bs_pos_day   
    close,1
    
    
    filename = SAVE_LOC + '/maven/ascii/model/fang/BS_nightside.dat'
    nline = file_lines(filename)
    sline = ''
    close,1
    openr,1,filename
    bs_pos_night = fltarr(3,nline-9)
    
    for i=0,8 do readf,1,sline
    readf,1,bs_pos_night
    close,1
    
    bs_pos = [[bs_pos_day],[bs_pos_night]]

    idx_y = where(bs_pos[1,*] eq 0)
    idx_z = where(bs_pos[2,*] eq 0)
    ;bs_x_y0 = bs_pos[0,idx_y]
    ;bs_z_y0 = bs_pos[2,idx_y]
  
    bs_x_z0 = bs_pos[0,idx_z]
    bs_y_z0 = bs_pos[1,idx_z]
    bs_z_z0 = bs_pos[2,idx_z]
    idx_y_z0 = sort(bs_y_z0)
    bs_y_z0 = bs_y_z0[idx_y_z0]
    bs_y_z0 = [bs_y_z0[0:48],bs_y_z0[49],bs_y_z0[122+indgen(49)*2]]
    bs_x_z0 = bs_x_z0[idx_y_z0]
    bs_x_z0 = [bs_x_z0[0:48],bs_x_z0[49],bs_x_z0[122+indgen(49)*2]]
    theta_bs_xy = acos(bs_x_z0/sqrt(bs_y_z0^2+bs_x_z0^2))
    idx_theta_xy_m = where(bs_y_z0 lt 0)
    theta_bs_xy[idx_theta_xy_m] = -theta_bs_xy[idx_theta_xy_m]    
    stop
    bs_x_y0 = bs_pos[0,idx_y]
    bs_y_y0 = bs_pos[1,idx_y]
    bs_z_y0 = bs_pos[2,idx_y]
    idx_z_y0 = sort(bs_z_y0)
    bs_z_y0 = bs_z_y0[idx_z_y0]
    bs_z_y0 = [bs_z_y0[0:48],bs_z_y0[49],bs_z_y0[122+indgen(49)]]
    bs_x_y0 = bs_x_y0[idx_z_y0]
    bs_x_y0 = [bs_x_y0[0:48],bs_x_y0[49],bs_x_y0[122+indgen(49)]]
    theta_bs_xz = acos(bs_x_y0/sqrt(bs_z_y0^2+bs_x_y0^2))
    idx_theta_xz_m = where(bs_z_y0 lt 0)
    theta_bs_xz[idx_theta_xz_m] = -theta_bs_xz[idx_theta_xz_m]
    
    stop
    !p.multi=[0,3,1]
    ;plot,bs_pos[0,*],bs_pos[1,*],psym=1,/iso;,xrange=[1.0,1.8],yrange=[-0.4,0.4]
    plot,bs_pos[0,*],bs_pos[2,*],psym=1,/iso,xrange=[-3,3],yrange=[-5,5],xtit='X',ytit='Z',charsize=2
    ;plot,bs_pos[1,*],bs_pos[2,*],psym=1,/iso;,xrange=[-1,1],yrange=[-1,1]
    
    
    ln_f = file_lines(SAVE_LOC+'/maven/ascii/model/fang/incident_ions_plane/all/a_reflect_list/reflect.dat')
    fn_arr = strarr(ln_f)
    openr,1, SAVE_LOC+'/maven/ascii/model/fang/incident_ions_plane/all/a_reflect_list/reflect.dat'
    readf,1,fn_arr
    close
    
    file_trag = SAVE_LOC+'/maven/ascii/model/fang/incident_ions_plane/all/' + fn_arr 
    
    ;file_trag = SAVE_LOC + '/maven/ascii/model/fang/incident_ions_plane/all/ep_*.dat'
    fs = file_search(file_trag)
    fn_out = ''
    
    for q=0,n_elements(fs)-1 do begin
      sline = ''
      ln = file_lines(fs[q])
      dat = fltarr(4,ln-9)
      close,1
      openr,1,fs[q]
      for s=0,8 do readf,1,sline
      readf,1,dat
      close,1   
      ;moxy = 16.*1.67e-27
      ;e_ele = 1.602e-19
      ;E_0 = 36729.4921875000
      ;V0 = sqrt(2.*E_0*e_ele/moxy)
      ;vvec_0 = V0* [-0.8290375471 ,-0.5591928959, 0.0000000000]
      nclmn = n_elements(dat[0,*])
      dt = 0.25
      vvec_i = (dat[0:2,findgen(nclmn-1)+1] - dat[0:2,findgen(nclmn-1)])*3389.9d3/dt
      agl = acos(inpro(vvec_i[0:2,0],vvec_i[0:2,nclmn-2])/total(vvec_i[0:2,0]^2)^.5/total(vvec_i[0:2,nclmn-2]^2)^.5)*!radeg
      if agl lt 60 then goto,skip
      oplot,dat[0,*],dat[2,*],color=230
      
      inout_arr = ''
      for ii=0,n_elements(dat[0,*])-1 do begin
        
        
           if dat[2,ii] ge 0 then theta_xz_ptcl = acos(dat[0,ii]/sqrt(dat[0,ii]^2+dat[2,ii]^2)) else theta_xz_ptcl = -acos(dat[0,ii]/sqrt(dat[0,ii]^2+dat[2,ii]^2))
          
           idx_xz = interpol(findgen(n_elements(theta_bs_xz)),theta_bs_xz,theta_xz_ptcl)
           xbs_intp = interpol(bs_x_y0,findgen(n_elements(bs_x_y0)),idx_xz)
           zbs_intp = interpol(bs_z_y0,findgen(n_elements(bs_z_y0)),idx_xz)
           
           R_bs_xz = sqrt(xbs_intp^2 + zbs_intp^2)
           R_ptcl_xz = sqrt(dat[0,ii]^2 + dat[2,ii]^2)
           if R_bs_xz gt R_ptcl_xz then inout_xz = 'in' else inout_xz = 'out'
           
                  
           if dat[1,ii] ge 0 then theta_xy_ptcl = acos(dat[0,ii]/sqrt(dat[0,ii]^2+dat[1,ii]^2)) else theta_xy_ptcl = -acos(dat[0,ii]/sqrt(dat[0,ii]^2+dat[1,ii]^2))
           
           idx_xy = interpol(findgen(n_elements(theta_bs_xy)),theta_bs_xy,theta_xy_ptcl)
           xbs_intp = interpol(bs_x_z0,findgen(n_elements(bs_x_z0)),idx_xy)
           ybs_intp = interpol(bs_y_z0,findgen(n_elements(bs_y_z0)),idx_xy)

           R_bs_xy = sqrt(xbs_intp^2 + ybs_intp^2)
           R_ptcl_xy = sqrt(dat[0,ii]^2 + dat[1,ii]^2)
           if R_bs_xy gt R_ptcl_xy then inout_xy = 'in' else inout_xy = 'out'         
           
           if inout_xz eq 'in' and inout_xy eq 'in' then inout_arr = [inout_arr,'in'] else inout_arr = [inout_arr,'out']
           ;if inout_xz eq 'in' and inout_xy eq 'in' then print,'in' else print,'out'
           ;print,inout_xz,inout_xy
           
      endfor
 
      inout_arr = inout_arr[1:*]
      if inout_arr[0] eq 'in' then goto,skip
      if inout_arr[n_elements(inout_xz_arr)-1] eq 'in' then goto, skip
      jdgin = where(inout_arr eq 'in')
      if jdgin[0] eq -1 then goto,skip
      fn_out = [fn_out,fs[q]]
      skip:
    endfor
    fn_out = fn_out[1:*]
    filename = SAVE_LOC + '/maven/ascii/model/fang/incident_ions_plane/all/a_reflect_list/reflect_gt60'+'.dat'
    format = '1(a13)'
    write_ascii,strmid(fn_out,78,13),filename,format
    stop
     plot,bs_pos[0,*],bs_pos[1,*],psym=1,/iso,xrange=[-3,3],yrange=[-5,5],xtit='X',ytit='Y',charsize=2
    
     for q=0,n_elements(fs)-1 do begin
       sline = ''
       ln = file_lines(fs[q])
       dat = fltarr(4,ln-9)
       close,1
       openr,1,fs[q]
       for s=0,8 do readf,1,sline
       readf,1,dat
       close,1
       ;if dat[2,0] lt -1.5 and dat[1,0] gt 0 then begin
         oplot,dat[0,*],dat[1,*],color=230
       ;endif
     endfor
    stop
    
    plot,bs_pos[1,*],bs_pos[2,*],psym=1,/iso,xrange=[-3,3],yrange=[-5,5],xtit='Y',ytit='Z',charsize=2

    for q=0,n_elements(fs)-1 do begin
      sline = ''
      ln = file_lines(fs[q])
      dat = fltarr(4,ln-9)
      close,1
      openr,1,fs[q]
      for s=0,8 do readf,1,sline
      readf,1,dat
      close,1
      ;if dat[2,0] lt -1.5 and dat[1,0] gt 0 then begin
        oplot,dat[1,*],dat[2,*],color=230
      ;endif
    endfor
    stop
    
   
    
    
    
    !p.multi=[0,1,1]
    plot_3dbox,reform(bs_pos[0,*]), reform(bs_pos[1,*]),reform(bs_pos[2,*])
;    p = PLOT3D(reform(bs_pos[0,*]), reform(bs_pos[1,*]),reform(bs_pos[2,*]), 'o', /SYM_FILLED, $
;      XRANGE=[-6, 6], YRANGE=[-6, 6], $
;      ZRANGE=[-1.4, 1.4],$
;      AXIS_STYLE=2, MARGIN=[0.2, 0.3, 0.1, 0], $
;      XMINOR=0, YMINOR=0, ZMINOR=0, $
;      DEPTH_CUE=[0, 2], /PERSPECTIVE, $
;      RGB_TABLE=33,  $
;      SHADOW_COLOR="deep sky blue", $
;      XY_SHADOW=1, YZ_SHADOW=1, XZ_SHADOW=1, $
;      XTITLE='x', YTITLE='y')
    
    
    stop
    
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
    
;    stop
;    iph_p0 = where(phi_arr*!radeg gt phi_points[0]-2.5 and phi_arr*!radeg le phi_points[0]+2.5)
;    iph_p1 = where(phi_arr*!radeg gt phi_points[1]-2.5 and phi_arr*!radeg le phi_points[1]+2.5)
;    iph_p2 = where(phi_arr*!radeg gt phi_points[2]-2.5 and phi_arr*!radeg le phi_points[2]+2.5)
;    iph_p3 = where(phi_arr*!radeg gt phi_points[3]-2.5 and phi_arr*!radeg le phi_points[3]+2.5)
;    iph_p4 = where(phi_arr*!radeg gt phi_points[4]-2.5 and phi_arr*!radeg le phi_points[4]+2.5)
;    iph_p5 = where(phi_arr*!radeg gt phi_points[5]-2.5 and phi_arr*!radeg le phi_points[5]+2.5)
;    iph_p6 = where(phi_arr*!radeg gt phi_points[6]-2.5 and phi_arr*!radeg le phi_points[6]+2.5)
;    iph_p7 = where(phi_arr*!radeg gt phi_points[7]-2.5 and phi_arr*!radeg le phi_points[7]+2.5)
;    iph_p8 = where(phi_arr*!radeg gt phi_points[8]-2.5 and phi_arr*!radeg le phi_points[8]+2.5)
;    iph_p9 = where(phi_arr*!radeg gt phi_points[9]-2.5 and phi_arr*!radeg le phi_points[9]+2.5)
;    iph_p10 = where(phi_arr*!radeg gt phi_points[10]-2.5 and phi_arr*!radeg le phi_points[10]+2.5)
;    iph_p11 = where(phi_arr*!radeg gt phi_points[11]-2.5 and phi_arr*!radeg le phi_points[11]+2.5)
;    iph_p12 = where(phi_arr*!radeg gt phi_points[12]-2.5 and phi_arr*!radeg le phi_points[12]+2.5)
;    iph_p13 = where(phi_arr*!radeg gt phi_points[13]-2.5 and phi_arr*!radeg le phi_points[13]+2.5)
;    iph_p14 = where(phi_arr*!radeg gt phi_points[14]-2.5 and phi_arr*!radeg le phi_points[14]+2.5)
;    iph_p15 = where(phi_arr*!radeg gt phi_points[15]-2.5 and phi_arr*!radeg le phi_points[15]+2.5)
;    iph_p16 = where(phi_arr*!radeg gt phi_points[16]-2.5 and phi_arr*!radeg le phi_points[16]+2.5)
;    iph_p17 = where(phi_arr*!radeg gt phi_points[17]-2.5 and phi_arr*!radeg le phi_points[17]+2.5)
;    stop
;    ith_0 = where(theta_arr*!radeg gt theta_points[0]-2.5 and theta_arr*!radeg le theta_points[0]+2.5)
;    ith_1 = where(theta_arr*!radeg gt theta_points[1]-2.5 and theta_arr*!radeg le theta_points[1]+2.5)
;    ith_2 = where(theta_arr*!radeg gt theta_points[2]-2.5 and theta_arr*!radeg le theta_points[2]+2.5)
;    
;    ;print,where(iph_p0 eq ith_0)
;    
;    
;    
;    
;    ;;initial velocity is radial direction
;    Vini_arr = [1.,0.,0.]
;    theta = (dindgen(9)*10d +10d)*!DTOR
;    phi = dindgen(36)*10d*!DTOR
;    for i=0,n_elements(theta)-1 do begin
;      for j=0,n_elements(phi)-1 do begin  
;        
;        Vini_x = cos(theta[i])
;        Vini_y = sin(theta[i])*cos(phi[j])
;        Vini_z = sin(theta[i])*sin(phi[j])
;        Vini_tot = sqrt(Vini_x^2+Vini_y^2+Vini_z^2)
;        Vini_arr = [[Vini_arr],[Vini_x/Vini_tot,Vini_y/Vini_tot,Vini_z/Vini_tot]]
;        
;      endfor
;    endfor
;    
; stop   
;    ele = 1.602e-19
;    mi = 16.*1.6e-27  ;[kg]
;    E0 = 1000d        ;[eV]
;    V0 = sqrt(2.*E0*ele/mi)
;    
    
    
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
    
   
    
    endfor
    endfor
    bs_pos2 = bs_pos2[*,1:*]
    shock_norm = shock_norm[*,1:*]
    sz = size(shock_norm)
    E0 = replicate(100.,sz[2])
    E1 = replicate(1000.,sz[2])
    E2 = replicate(10000.,sz[2])
    
    sav = [[transpose(bs_pos2[0,*])],[transpose(bs_pos2[1,*])],[transpose(bs_pos2[2,*])],[E0],[E1],[E2],[transpose(shock_norm[0,*])],[transpose(shock_norm[1,*])],[transpose(shock_norm[2,*])]]
stop   
    filename = SAVE_LOC + '/maven/ascii/vdfparam/theoritical_ions'+'.dat'
    format = '9(f15.8)'
    write_ascii,sav,filename,format
    
    
    stop



end